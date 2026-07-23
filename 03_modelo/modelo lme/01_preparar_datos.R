# =============================================================
# 01_preparar_datos.R
# -------------------------------------------------------------
# Carga los ensayos Santillana y los resultados SIMCE a nivel de
# colegio, limpia ambas tablas, y construye las features listas
# para modelar. Respecto a la v1, esta versión reemplaza:
#
#   - slope_logro calculado "a mano" (cov/var)  -> por la pendiente
#     de un modelo de crecimiento mixto (lme4), que ENCOGE
#     (shrinkage) la pendiente y el intercepto de cada estudiante
#     hacia el promedio del grupo según cuánta evidencia real tenga
#     (1 ensayo -> casi no se aleja del promedio; 6 ensayos -> se
#     acerca a su tendencia observada).
#   - last_logro (el puntaje del último ensayo rendido, sin más)
#     -> por pred_final_logro: el puntaje esperado en el ensayo 6
#     según la trayectoria ajustada, usando el mismo punto de
#     referencia para todos los estudiantes (hayan llegado o no a
#     rendir el ensayo 6).
#
# Y agrega una variable nueva:
#
#   - colegio_efecto_historico: efecto persistente del colegio
#     estimado con TODOS los años de SIMCE disponibles, incluidos
#     años sin ensayo (2022). Se calcula con ventana expansiva
#     (sólo años ESTRICTAMENTE anteriores al año que se va a
#     predecir) para no filtrar información del futuro.
#
# Salidas (en output/):
#   - ind_features.rds     : 1 fila por estudiante x año x grado x área
#   - school_features.rds  : 1 fila por colegio x año x grado x área
#   - school_model_data.rds: school_features + promedio_simce observado
#   - efecto_historico.rds : tabla colegio_efecto_historico por
#                             (año objetivo, grado, área, rbd_revisado)
#
# Nota: el paso 4 ajusta hasta 12 modelos mixtos de crecimiento
# (3 años x 2 grados x 2 áreas) sobre decenas de miles de filas;
# puede tardar varios minutos en correr.
# =============================================================

library(tidyverse)
library(arrow)
library(readxl)
library(lme4)

# ---- 0. Configuración --------------------------------------------
# Configurar rutas de archivos: ----
usuario <- Sys.info()[["user"]]
rutas <- config::get(config = usuario, file = "config.yml")
ruta_data_in <- rutas$ruta_data_in
ruta_data_intermedia <- rutas$ruta_data_intermedia
ruta_outputs <- rutas$ruta_outputs

dir_salidas <- ruta_outputs %>% file.path('modelo_lme')
dir_salidas %>% dir.create(showWarnings = FALSE)

# ---- 1. Cargar datos -----------------------------------------------

# Cargar datos ----
## ENSAYOS ----
ensayos_santillana0 <- ruta_data_intermedia %>% 
  file.path('ensayo_simce', 'ensayos_santillana_corregido.parquet') %>% 
  read_parquet() 

ensayos <- ensayos_santillana0 %>% 
  mutate(agno = as.numeric(agno)) |> 
  filter(!outlier_iqr, !outlier_isoforest)

## SIMCES ----
simce0_rbd <- ruta_data_intermedia %>% 
  file.path('simce', 'resultados_simce_rbd_corregido.parquet') %>% 
  read_parquet()

simce <- simce0_rbd %>% 
  filter(!outlier_iqr, !outlier_isoforest)

# ---- 2. Limpieza de ensayos -----------------------------------------
ensayos_limpio <- ensayos %>%
  mutate(
    # El número de ensayo (1 a 6) va dentro del texto de "evaluacion",
    # p.ej. "Preparación Simce 3 (2025)" -> 3.
    ensayo_num   = str_match(evaluacion, "Simce\\s+(\\d+)")[, 2] %>% as.integer(),
    rbd_revisado = as.integer(rbd_revisado),
    # ~0.01% de los registros supera 100% de logro (probablemente
    # puntaje extra/errores de origen); se acota a 100.
    porcentaje_logro = pmin(porcentaje_logro, 100)
  ) %>%
  rename(grado = nivel) %>%
  filter(!is.na(rbd_revisado), !is.na(ensayo_num))

# Un mismo estudiante puede tener 2 registros para el mismo número de
# ensayo (p.ej. versión "basal" y "extenso" del ensayo 1 en matemática
# 2023). Se colapsan promediando antes de seguir.
ensayos_dedup <- ensayos_limpio %>%
  group_by(id_usuario_curso, agno, grado, area, ensayo_num, rbd_revisado) %>%
  summarise(porcentaje_logro = mean(porcentaje_logro), .groups = "drop")

# ---- 3. Resumen simple por estudiante (promedio y n° de ensayos) -----
resumen_simple <- ensayos_dedup %>%
  group_by(id_usuario_curso, agno, grado, area, rbd_revisado) %>%
  summarise(
    n_evals    = n(),
    mean_logro = mean(porcentaje_logro),
    .groups = "drop"
  )

# ---- 4. Modelo de crecimiento por estudiante (lme4) -------------------
# Se ajusta un modelo separado por (año, grado, área) porque la
# dificultad/escala de los ensayos cambia entre esos grupos.
#
# IMPORTANTE - decisión tomada tras probar el modelo con datos reales:
# un primer intento con pendiente aleatoria POR ESTUDIANTE
# ((ensayo_num | id_usuario_curso)) resultó NO IDENTIFICABLE: ~27% de
# los estudiantes rinde un solo ensayo ese año, así que en varios grupos
# el número de parámetros aleatorios (2 por estudiante) termina superando
# al número de observaciones. lme4 lo rechaza con error, con razón: no
# hay evidencia para estimar una pendiente individual con 1 solo punto.
#
# Se resuelve subiendo la pendiente aleatoria a nivel de COLEGIO (que sí
# tiene densidad de datos de sobra: cientos de estudiantes x varios
# ensayos) y dejando sólo el INTERCEPTO como aleatorio por estudiante.
# Interpretación: la tendencia (mejora/caída durante el año) se estima
# por colegio; cada estudiante hereda la tendencia de su colegio y aporta
# su propio nivel (cuánto está por sobre/bajo esa trayectoria).
ajustar_crecimiento_grupo <- function(datos_grupo) {
  modelo <- lmer(
    porcentaje_logro ~ ensayo_num + (1 + ensayo_num | rbd_revisado) + (1 | id_usuario_curso),
    data = datos_grupo,
    control = lmerControl(optimizer = "bobyqa")
  )

  fe <- fixef(modelo)

  efecto_colegio <- ranef(modelo)$rbd_revisado %>%
    rownames_to_column("rbd_revisado") %>%
    transmute(
      rbd_revisado        = as.integer(rbd_revisado),
      colegio_intercepto  = `(Intercept)`,
      colegio_slope       = ensayo_num
    )

  ranef(modelo)$id_usuario_curso %>%
    rownames_to_column("id_usuario_curso") %>%
    transmute(
      id_usuario_curso      = as.integer(id_usuario_curso),
      estudiante_intercepto = `(Intercept)`
    ) %>%
    left_join(distinct(datos_grupo, id_usuario_curso, rbd_revisado), by = "id_usuario_curso") %>%
    left_join(efecto_colegio, by = "rbd_revisado") %>%
    mutate(
      intercepto_hat   = fe[["(Intercept)"]] + colegio_intercepto + estudiante_intercepto,
      slope_hat        = fe[["ensayo_num"]] + colegio_slope,
      # Puntaje esperado en el ensayo 6, punto de referencia común para
      # todos los estudiantes del grupo (hayan o no rendido el 6°).
      # Se acota a [0, 100] porque es una extrapolación y en casos
      # extremos (estudiante con pocos datos y pendiente de colegio muy
      # pronunciada) puede salirse del rango válido de porcentaje_logro.
      pred_final_logro = pmin(pmax(intercepto_hat + slope_hat * 6, 0), 100)
    ) %>%
    select(id_usuario_curso, slope_hat, pred_final_logro)
}

grupos_crecimiento <- ensayos_dedup %>% distinct(agno, grado, area)

crecimiento_individual <- map_dfr(seq_len(nrow(grupos_crecimiento)), function(i) {
  a  <- grupos_crecimiento$agno[i]
  g  <- grupos_crecimiento$grado[i]
  ar <- grupos_crecimiento$area[i]
  cat("Ajustando modelo de crecimiento:", a, g, ar, "...\n")

  datos_grupo <- ensayos_dedup %>% filter(agno == a, grado == g, area == ar)
  ajustar_crecimiento_grupo(datos_grupo) %>%
    mutate(agno = a, grado = g, area = ar)
})

# ---- 5. Features finales a nivel de estudiante y de colegio -----------
ind_features <- resumen_simple %>%
  inner_join(
    crecimiento_individual,
    by = c("id_usuario_curso", "agno", "grado", "area")
  ) %>%
  rename(slope_logro = slope_hat)

school_features <- ind_features %>%
  group_by(agno, grado, area, rbd_revisado) %>%
  summarise(
    n_estudiantes    = n(),
    mean_logro       = mean(mean_logro, na.rm = TRUE),
    pred_final_logro = mean(pred_final_logro, na.rm = TRUE),
    slope_logro      = mean(slope_logro, na.rm = TRUE),
    n_evals_prom     = mean(n_evals),
    sd_entre_estud   = sd(mean_logro, na.rm = TRUE),   # sólo referencial en el 1er modelo
    .groups = "drop"
  )

# ---- 6. Efecto histórico del colegio (todos los años de SIMCE) --------
# Para cada año objetivo se usa SOLO el SIMCE de años ESTRICTAMENTE
# anteriores (para no filtrar información del futuro hacia el pasado).
# Con 2+ años previos se ajusta un modelo mixto con intercepto aleatorio
# por colegio, controlando por año (para no confundir "colegio bueno"
# con "año en que el SIMCE dio más fácil/difícil a nivel país"). Con
# exactamente 1 año previo no hay forma de separar año de colegio, así
# que se usa el promedio_simce de ese único año, centrado, como
# aproximación.
simce_limpio <- simce %>%
  distinct() %>%                       # el archivo trae filas duplicadas exactas en 2024
  filter(promedio_simce > 0) %>%       # 0 = colegio sin resultado publicado ese año/área
  mutate(rbd = as.integer(rbd)) %>%
  rename(rbd_revisado = rbd)

calcular_efecto_historico <- function(simce_todo, anio_objetivo, grado_obj, area_obj) {

  previos <- simce_todo %>%
    filter(agno < anio_objetivo, grado == grado_obj, area == area_obj)

  n_anios_previos <- n_distinct(previos$agno)

  if (n_anios_previos == 0) {
    return(tibble(rbd_revisado = integer(0), colegio_efecto_historico = double(0)))
  }

  if (n_anios_previos == 1) {
    return(
      previos %>%
        mutate(colegio_efecto_historico = promedio_simce - mean(promedio_simce)) %>%
        select(rbd_revisado, colegio_efecto_historico)
    )
  }

  modelo <- lmer(promedio_simce ~ factor(agno) + (1 | rbd_revisado), data = previos)

  ranef(modelo)$rbd_revisado %>%
    rownames_to_column("rbd_revisado") %>%
    transmute(
      rbd_revisado = as.integer(rbd_revisado),
      colegio_efecto_historico = `(Intercept)`
    )
}

anios_objetivo <- school_features %>% distinct(agno, grado, area)

efecto_historico <- map_dfr(seq_len(nrow(anios_objetivo)), function(i) {
  a  <- anios_objetivo$agno[i]
  g  <- anios_objetivo$grado[i]
  ar <- anios_objetivo$area[i]
  calcular_efecto_historico(simce_limpio, a, g, ar) %>%
    mutate(agno = a, grado = g, area = ar)
})

# Colegios sin historia previa (nuevos en Santillana, o sin SIMCE
# publicado antes de ese año) reciben efecto 0 = "sin evidencia de que
# se desvíen del promedio nacional".
school_features <- school_features %>%
  left_join(efecto_historico, by = c("agno", "grado", "area", "rbd_revisado")) %>%
  mutate(colegio_efecto_historico = replace_na(colegio_efecto_historico, 0))

ind_features <- ind_features %>%
  left_join(efecto_historico, by = c("agno", "grado", "area", "rbd_revisado")) %>%
  mutate(colegio_efecto_historico = replace_na(colegio_efecto_historico, 0))

# ---- 7. Cruce final con el SIMCE del MISMO año (para entrenar) --------
school_model_data <- school_features %>%
  inner_join(
    simce_limpio %>% select(agno, grado, area, rbd_revisado, promedio_simce),
    by = c("agno", "grado", "area", "rbd_revisado")
  )

cat("\nFilas ind_features:      ", nrow(ind_features), "\n")
cat("Filas school_features:   ", nrow(school_features), "\n")
cat("Filas school_model_data: ", nrow(school_model_data), " (colegios con ensayo Y simce cruzados)\n\n")
cat("Cobertura por año/grado/área en school_model_data:\n")
print(school_model_data %>% count(agno, grado, area) %>% pivot_wider(names_from = area,
                                                                     values_from = n))

# ---- 8. Guardar --------------------------------------------------------
saveRDS(ind_features,      dir_salidas %>% file.path("ind_features.rds"))
saveRDS(school_features,   dir_salidas %>% file.path("school_features.rds"))
saveRDS(school_model_data, dir_salidas %>% file.path("school_model_data.rds"))
saveRDS(efecto_historico,  dir_salidas %>% file.path("efecto_historico.rds"))

cat("\nListo. Objetos guardados en output/*.rds\n")
