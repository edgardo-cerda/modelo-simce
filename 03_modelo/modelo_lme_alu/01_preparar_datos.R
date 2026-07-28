# =============================================================
# 01_preparar_datos.R  (v4 - SIMCE por alumno + contexto del colegio)
# -------------------------------------------------------------
# NOVEDAD v4: se incorporan las variables de contexto que trae el
# archivo por RBD y que no se estaban usando:
#
#   cod_grupo      GSE: 1 Bajo, 2 Medio bajo, 3 Medio, 4 Medio alto, 5 Alto
#   cod_depe1      1 Municipal Corporación, 2 Municipal DAEM,
#                  3 Particular subvencionado, 4 Particular pagado,
#                  5 Corporación de administración delegada,
#                  6 Servicio Local de Educación
#   cod_depe2      versión colapsada de la anterior
#   cod_rural_rbd  1 Urbano, 2 Rural
#
# CÓMO ENTRAN, Y POR QUÉ NO COMO SIMPLES PREDICTORES MÁS:
#
# Probadas como columnas adicionales de la regresión final (por
# grado x área, ~150-220 colegios) no aportan casi nada: el MAE
# out-of-time se mueve entre -0.2 y +0.3 puntos, y en un grupo
# empeora. La razón es que `colegio_efecto_historico` YA las
# contiene: el nivel histórico de un colegio es, en buena medida, su
# GSE y su dependencia (la expectativa contextual correlaciona 0.70
# con el nivel histórico). Sumadas aparte sólo gastan grados de
# libertad en una regresión que tiene pocas filas.
#
# Donde sí valen oro es donde la historia falla: sin efecto
# histórico, el contexto sube el R² out-of-time de 0.40 a 0.62-0.66.
# Son un buen SUSTITUTO de la historia, no un complemento de ella.
#
# Por eso entran como PRIOR del efecto histórico. El modelo pasa de
#
#     promedio_simce ~ factor(agno) + (1 | rbd)
# a
#     promedio_simce ~ factor(agno) + GSE + dependencia + ruralidad + (1 | rbd)
#
# y el efecto del colegio se reconstruye como
#
#     colegio_efecto_historico = expectativa_contextual + desvío_del_colegio
#
# Esto arregla tres cosas de una vez:
#   - Colegios SIN historia previa: antes recibían 0 (= "promedio
#     nacional"), mala suposición tanto para un particular pagado GSE
#     alto como para un rural GSE bajo. Ahora reciben lo que se espera
#     de un colegio de su contexto.
#   - Colegios con UN año de historia: el encogimiento de lme4 los tira
#     hacia el promedio de su CONTEXTO, no del país. Prior correcto.
#   - Colegios con historia larga: casi no cambian, que es lo que
#     corresponde — ahí manda la evidencia propia.
#
# El modelo de contexto se estima sobre el universo NACIONAL (~6.400
# colegios de 4b, ~3.000 de 2m, todos los GSE, 30% de ruralidad, las 6
# dependencias), no sobre la base Santillana, que está muy concentrada
# (57-63% particular pagado, GSE promedio ~4, 1-4% rural). Estimar el
# efecto del GSE con 180 colegios de los cuales casi ninguno es rural
# sería pedirle a los datos algo que no tienen: se estiman los
# coeficientes donde hay variación y se transfieren.
#
# Efecto lateral útil: como el contexto llega a los modelos finales
# convertido en UN número, desaparece el problema de las CATEGORÍAS
# AUSENTES. Con 150-220 colegios por grupo es perfectamente posible
# que "corporación de administración delegada" o "rural" no aparezca
# en entrenamiento y sí en el año de prueba, lo que haría fallar
# predict(). Acá no puede pasar.
#
# CONTEXTO REZAGADO: el GSE se recalcula en cada medición y cambia de
# categoría en ~26% de los colegios de un año a otro; la dependencia
# se movió 8% entre 2024 y 2025 por el paso a Servicio Local. Como
# ambas se publican JUNTO con los resultados del año, usar las del año
# objetivo sería filtrar información que no existe al momento de
# predecir. Se usa siempre el último registro ESTRICTAMENTE anterior.
#
# NO cambió la forma de la distribución interna (`forma_z`): se probó
# condicionarla por GSE dentro de cada tercil de nivel y las
# diferencias son de ≤0.06 en z (~2 puntos SIMCE). El tercil de nivel
# ya captura lo que el GSE aportaría.
#
# -------------------------------------------------------------
# (v3) Cambio anterior respecto a la v2: se carga
# `consolidado_datos_simce_alu.parquet`, que trae el SIMCE de
# CADA ESTUDIANTE (2022-2025, 4b y 2m, matemática y lectura, más
# el error estándar de medición eem_*). Se verificó que reproduce
# los promedios por colegio del archivo RBD casi exactamente
# (diferencia mediana ~0.27 pts), o sea es el mismo universo pero
# desagregado.
#
# Eso permite dos cosas que antes eran imposibles:
#
#   1) MODELAR LA DISPERSIÓN: además del promedio del colegio,
#      ahora existe `sd_simce` observada (desviación estándar de
#      los puntajes individuales dentro de cada colegio). Es el
#      target del modelo de 02b_modelo_dispersion.R.
#
#   2) MATCHING POR PERCENTIL: se conoce la FORMA real de la
#      distribución interna de puntajes de un colegio, no sólo su
#      promedio. Se resume como una función de cuantiles sobre el
#      puntaje estandarizado dentro del colegio,
#      z = (ptje_alumno - media_colegio) / sd_colegio,
#      que resulta ser muy estable entre colegios y años (por eso
#      se puede usar como plantilla). Se guarda por (grado, área)
#      y por tercil de nivel del colegio, porque los colegios de
#      bajo rendimiento tienen colas algo más asimétricas a la
#      derecha y los de alto, a la izquierda.
#
# Ambas cosas se calculan con VENTANA EXPANSIVA (sólo años
# estrictamente anteriores al año que se quiere predecir), igual
# que `colegio_efecto_historico`, para no filtrar información del
# futuro hacia el pasado.
#
# Salidas (en output/modelo_lme/):
#   - ind_features.rds      : 1 fila por estudiante x año x grado x área
#                             (ahora incluye pct_ensayo y z_ensayo:
#                             posición relativa del estudiante DENTRO
#                             de su colegio)
#   - school_features.rds   : 1 fila por colegio x año x grado x área
#                             (ahora incluye features de dispersión)
#   - school_model_data.rds : school_features + promedio_simce Y sd_simce
#   - efecto_historico.rds  : efecto persistente del colegio (nivel)
#   - simce_alumno.rds      : SIMCE individual en formato largo
#   - simce_dist.rds        : media, sd y cuantiles observados por colegio
#   - forma_z.rds           : plantilla de forma de la distribución interna
#   - cortes_tercil.rds     : cortes para asignar tercil de nivel
#   - confiabilidades.rds   : confiabilidad del ensayo y del SIMCE
#                             (acota cuánto puede correlacionar uno con
#                             el otro a nivel individual: ver README 5b)
#
# Nota: el paso 4 ajusta hasta 12 modelos mixtos de crecimiento
# sobre decenas de miles de filas; puede tardar varios minutos.
# =============================================================

library(tidyverse)
library(arrow)
library(readxl)
library(lme4)

# ---- 0. Configuración --------------------------------------------
usuario <- Sys.info()[["user"]]
rutas <- config::get(config = usuario, file = "config.yml")
ruta_data_in <- rutas$ruta_data_in
ruta_data_intermedia <- rutas$ruta_data_intermedia
ruta_outputs <- rutas$ruta_outputs

dir_salidas <- ruta_outputs %>% file.path('modelo_lme_alu')
dir_salidas %>% dir.create(showWarnings = FALSE)

# Parámetros -------------------------------------------------------
# Mínimo de alumnos con SIMCE para que la sd interna del colegio sea
# una estimación utilizable (con n<15 la sd muestral es muy ruidosa:
# su error relativo es ~1/sqrt(2(n-1))).
MIN_ALU_SD    <- 15
GRADOS_MODELO <- c("4b", "2m")   # los únicos con ensayo Santillana
# Grilla de percentiles con que se guarda la plantilla de forma.
GRILLA_P      <- seq(0.005, 0.995, by = 0.005)

# Niveles fijos de las variables de contexto. Se declaran explícitos
# para que los factores tengan los mismos niveles en entrenamiento y en
# predicción aunque alguna categoría no aparezca en un subconjunto.
NIVELES_GSE   <- as.character(1:5)
NIVELES_DEPE1 <- as.character(1:6)
NIVELES_RURAL <- as.character(1:2)

etiquetas_gse <- tibble(
  cod_grupo = 1:5,
  gse_etiqueta = c("Bajo", "Medio bajo", "Medio", "Medio alto", "Alto")
)
etiquetas_depe1 <- tibble(
  cod_depe1 = 1:6,
  depe1_etiqueta = c("Municipal Corporación", "Municipal DAEM",
                     "Particular subvencionado", "Particular pagado",
                     "Corporación de administración delegada",
                     "Servicio Local de Educación")
)
etiquetas_depe2 <- tibble(
  cod_depe2 = 1:4,
  depe2_etiqueta = c("Municipal", "Particular subvencionado",
                     "Particular pagado", "Servicio Local de Educación")
)
etiquetas_rural <- tibble(
  cod_rural_rbd = 1:2,
  rural_etiqueta = c("Urbano", "Rural")
)

preparar_factores <- function(d) {
  d %>% mutate(
    f_gse   = factor(as.character(as.integer(cod_grupo)),     levels = NIVELES_GSE),
    f_depe  = factor(as.character(as.integer(cod_depe1)),     levels = NIVELES_DEPE1),
    f_rural = factor(as.character(as.integer(cod_rural_rbd)), levels = NIVELES_RURAL)
  )
}
contexto_completo <- function(d) {
  !is.na(d$f_gse) & !is.na(d$f_depe) & !is.na(d$f_rural)
}

# ---- 1. Cargar datos -----------------------------------------------

## ENSAYOS ----
ensayos_santillana0 <- ruta_data_intermedia %>%
  file.path('ensayo_santillana', 'ensayos_santillana_corregido.parquet') %>%
  read_parquet()

ensayos <- ensayos_santillana0 %>%
  mutate(agno = as.numeric(agno)) |>
  filter(!outlier_iqr, !outlier_isoforest)

## SIMCE AGREGADO POR COLEGIO ----
simce0_rbd <- ruta_data_intermedia %>%
  file.path('simce', 'resultados_simce_rbd_corregido.parquet') %>%
  read_parquet()

simce <- simce0_rbd %>%
  filter(!outlier_iqr, !outlier_isoforest)

## SIMCE POR ALUMNO (NUEVO) ----
# Si el archivo está en otra carpeta, ajustar esta ruta.
ruta_alu <- ruta_data_intermedia %>%
  file.path('simce', 'consolidado_datos_simce_alu.parquet')
stopifnot("No se encuentra consolidado_datos_simce_alu.parquet" = file.exists(ruta_alu))
simce_alu0 <- read_parquet(ruta_alu)

# ---- 2. Limpieza de ensayos -----------------------------------------
ensayos_limpio <- ensayos %>%
  mutate(
    porcentaje_logro = pmin(porcentaje_logro, 100),
    n_evaluacion = as.integer(n_evaluacion)
  ) %>%
  filter(!is.na(rbd_revisado), !is.na(n_evaluacion), porcentaje_logro > 0)

ensayos_dedup <- ensayos_limpio %>%
  group_by(id_usuario_curso, agno, grado, area, n_evaluacion, rbd_revisado) %>%
  summarise(porcentaje_logro = mean(porcentaje_logro), .groups = "drop")

# ---- 2b. SIMCE individual a formato largo ---------------------------
# El archivo viene ancho (una columna por área). Se pasa a largo para
# que calce con la llave (agno, grado, area, rbd_revisado) del resto
# del pipeline. Se descartan puntajes ausentes (~20% de las filas: son
# alumnos matriculados que no rindieron o quedaron excluidos).
base_alu <- simce_alu0 %>%
  filter(grado %in% GRADOS_MODELO) %>%
  mutate(agno = as.numeric(agno), rbd_revisado = as.numeric(rbd))

simce_alumno <- bind_rows(
  base_alu %>% transmute(agno, grado, rbd_revisado, idalumno,
                         area = "matematica", ptje = ptje_mate, eem = eem_mate),
  base_alu %>% transmute(agno, grado, rbd_revisado, idalumno,
                         area = "lenguaje",   ptje = ptje_lect, eem = eem_lect)
) %>%
  filter(!is.na(ptje), ptje > 0)

rm(base_alu, simce_alu0); gc()

cat("SIMCE individual cargado:", nrow(simce_alumno), "puntajes alumno x área\n")

# ---- 2c. Distribución interna observada por colegio ------------------
# Esta tabla es la "verdad" contra la que se validan las predicciones
# individuales en 04_validacion_individual.R.
simce_dist <- simce_alumno %>%
  group_by(agno, grado, area, rbd_revisado) %>%
  summarise(
    n_alu_simce     = n(),
    media_simce_alu = mean(ptje),
    sd_simce        = sd(ptje),
    p10_simce       = quantile(ptje, 0.10, names = FALSE),
    p25_simce       = quantile(ptje, 0.25, names = FALSE),
    p50_simce       = quantile(ptje, 0.50, names = FALSE),
    p75_simce       = quantile(ptje, 0.75, names = FALSE),
    p90_simce       = quantile(ptje, 0.90, names = FALSE),
    eem_medio       = mean(eem, na.rm = TRUE),  # error de medición del test
    .groups = "drop"
  ) %>%
  filter(n_alu_simce >= MIN_ALU_SD, !is.na(sd_simce), sd_simce > 0)

# Rango plausible de puntajes por grado/área: se usa para acotar las
# predicciones individuales (no tiene sentido predecir 500 puntos).
limites_simce <- simce_alumno %>%
  group_by(grado, area) %>%
  summarise(
    ptje_min = quantile(ptje, 0.001, names = FALSE),
    ptje_max = quantile(ptje, 0.999, names = FALSE),
    .groups = "drop"
  )

# ---- 2d. Contexto del colegio y universo nacional (NUEVO) ------------
# `simce_limpio` se define acá (antes estaba en la sección 6) porque
# ahora también es la fuente del contexto.
simce_limpio <- simce %>%
  distinct() %>%                       # el archivo trae filas duplicadas exactas en 2024
  filter(promedio_simce > 0) %>%       # 0 = colegio sin resultado publicado ese año/área
  mutate(rbd_revisado = as.numeric(rbd)) 

contexto_crudo <- simce_limpio %>%
  select(agno, grado, area, rbd_revisado,
         cod_depe1, cod_depe2, cod_grupo, cod_rural_rbd,
         cod_com_rbd, nom_com_rbd) %>%
  distinct(agno, grado, area, rbd_revisado, .keep_all = TRUE)

# Bases nacionales: es sobre ÉSTAS que se estiman los efectos de GSE,
# dependencia y ruralidad, porque acá sí hay variación en esas
# variables (la base Santillana no la tiene).
simce_nacional <- simce_limpio %>% preparar_factores()
dist_nacional  <- simce_dist %>%
  left_join(contexto_crudo, by = c("agno", "grado", "area", "rbd_revisado")) %>%
  preparar_factores()

cat("\nColegios por GSE en el universo nacional (último año):\n")
print(
  simce_nacional %>%
    filter(agno == max(agno), grado %in% GRADOS_MODELO, area == "matematica") %>%
    count(grado, cod_grupo) %>%
    pivot_wider(names_from = cod_grupo, values_from = n, names_prefix = "GSE_")
)

# ---- 3. Resumen simple por estudiante --------------------------------
resumen_simple <- ensayos_dedup %>%
  group_by(id_usuario_curso, agno, grado, area, rbd_revisado) %>%
  summarise(
    n_evals    = n(),
    mean_logro = mean(porcentaje_logro),
    .groups = "drop"
  )

# ---- 4. Modelo de crecimiento por estudiante (lme4) -------------------
# (Sin cambios de diseño respecto a la v2: pendiente aleatoria por
# COLEGIO e intercepto aleatorio por estudiante, porque ~27% de los
# estudiantes rinde un solo ensayo y una pendiente individual no es
# identificable.)
#
# NUEVO: además de pred_final_logro (acotado a [0,100]) se devuelve
# `pred_final_logro_raw`, sin acotar, y `nivel_est` (el intercepto
# aleatorio del estudiante, ya encogido por lme4). El acotado a [0,100]
# genera EMPATES en los extremos, lo que arruinaría el ranking dentro
# del colegio: para ordenar estudiantes se usa la versión sin acotar.
ajustar_crecimiento_grupo <- function(datos_grupo) {
  modelo <- lmer(
    porcentaje_logro ~ n_evaluacion + (1 + n_evaluacion | rbd_revisado) + (1 | id_usuario_curso),
    data = datos_grupo,
    control = lmerControl(optimizer = "bobyqa")
  )

  fe <- fixef(modelo)

  efecto_colegio <- ranef(modelo)$rbd_revisado %>%
    rownames_to_column("rbd_revisado") %>%
    transmute(
      rbd_revisado        = as.numeric(rbd_revisado),
      colegio_intercepto  = `(Intercept)`,
      colegio_slope       = n_evaluacion
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
      intercepto_hat       = fe[["(Intercept)"]] + colegio_intercepto + estudiante_intercepto,
      slope_hat            = fe[["n_evaluacion"]] + colegio_slope,
      pred_final_logro_raw = intercepto_hat + slope_hat * 6,
      pred_final_logro     = pmin(pmax(pred_final_logro_raw, 0), 100),
      nivel_est            = estudiante_intercepto
    ) %>%
    select(id_usuario_curso, slope_hat, pred_final_logro, pred_final_logro_raw, nivel_est)
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

# ---- 5. Features a nivel de estudiante --------------------------------
# NUEVO: pct_ensayo y z_ensayo, la posición del estudiante DENTRO de su
# propio colegio. Son la materia prima de las dos versiones de
# predicción individual:
#   - versión A (dispersión): usa z_ensayo
#   - versión B (matching por percentil): usa pct_ensayo
# El índice sobre el que se ordena es pred_final_logro_raw, que ya viene
# encogido (shrinkage) por el modelo mixto: un estudiante con 1 solo
# ensayo queda cerca del promedio de su colegio en vez de irse a un
# extremo por ruido de una sola medición.
ind_features <- resumen_simple %>%
  inner_join(
    crecimiento_individual,
    by = c("id_usuario_curso", "agno", "grado", "area")
  ) %>%
  rename(slope_logro = slope_hat) %>%
  mutate(indice_ensayo = pred_final_logro_raw) %>%
  group_by(agno, grado, area, rbd_revisado) %>%
  mutate(
    n_est_colegio = n(),
    # percentil dentro del colegio, con corrección (rank - 0.5)/n para
    # que ningún estudiante quede en 0 o 1 exactos.
    pct_ensayo = (rank(indice_ensayo, ties.method = "average") - 0.5) / n(),
    # posición estandarizada dentro del colegio. Si el colegio tiene
    # 1-2 estudiantes o sd 0, se deja en 0 (= "en el promedio").
    z_ensayo = if (n() >= 3 && sd(indice_ensayo) > 0) {
      (indice_ensayo - mean(indice_ensayo)) / sd(indice_ensayo)
    } else 0
  ) %>%
  ungroup()

# ---- 5b. Features a nivel de colegio ----------------------------------
# NUEVO: además de las de nivel, features de DISPERSIÓN del ensayo
# (sd_entre_estud ya existía pero era sólo referencial; ahora es
# predictor del modelo de dispersión) e iqr_logro_ensayo, que resultó
# algo más robusto que la sd frente a colegios con outliers.
school_features <- ind_features %>%
  group_by(agno, grado, area, rbd_revisado) %>%
  summarise(
    n_estudiantes    = n(),
    promedio_mean_logro       = mean(mean_logro, na.rm = TRUE),
    pred_final_logro = mean(pred_final_logro, na.rm = TRUE),
    slope_logro      = mean(slope_logro, na.rm = TRUE),
    n_evals_prom     = mean(n_evals, na.rm = TRUE),
    sd_entre_estud   = sd(mean_logro, na.rm = TRUE),
    iqr_logro_ensayo = quantile(mean_logro, 0.90, names = FALSE) -
                       quantile(mean_logro, 0.10, names = FALSE),
    .groups = "drop"
  ) %>% 
  rename(mean_logro = promedio_mean_logro)

# ---- 5c. Contexto REZAGADO de los colegios Santillana (NUEVO) ---------
# Para cada año objetivo, el último registro de contexto estrictamente
# anterior de ese mismo colegio, grado y área. El GSE cambia de categoría
# en ~26% de los colegios de un año a otro y la dependencia se movió 8%
# entre 2024 y 2025 (paso a Servicio Local), así que el rezago no es una
# formalidad: es la única versión de estas variables que existe al
# momento de predecir.
anios_objetivo <- school_features %>% distinct(agno, grado, area)

contexto_rezagado <- map_dfr(sort(unique(anios_objetivo$agno)), function(y) {
  contexto_crudo %>%
    filter(agno < y) %>%
    group_by(grado, area, rbd_revisado) %>%
    slice_max(agno, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    rename(agno_contexto = agno) %>%
    mutate(agno = y)
}) %>%
  preparar_factores()

cobertura_ctx <- school_features %>%
  left_join(contexto_rezagado %>% select(agno, grado, area, rbd_revisado, cod_grupo),
            by = c("agno", "grado", "area", "rbd_revisado")) %>%
  summarise(cobertura = mean(!is.na(cod_grupo))) %>%
  pull(cobertura)
cat(sprintf("\nCobertura del contexto rezagado en colegios Santillana: %.1f%%\n",
            100 * cobertura_ctx))

# Los colegios objetivo de cada grupo, con su contexto rezagado pegado.
colegios_objetivo <- function(a, g, ar) {
  school_features %>%
    filter(agno == a, grado == g, area == ar) %>%
    select(rbd_revisado) %>%
    left_join(
      contexto_rezagado %>%
        filter(agno == a, grado == g, area == ar) %>%
        select(rbd_revisado, cod_grupo, cod_depe1, cod_rural_rbd, f_gse, f_depe, f_rural),
      by = "rbd_revisado"
    )
}

# ---- 6. Efecto histórico del colegio, con prior contextual (MODIFICADO)
# El modelo se ajusta sobre el universo NACIONAL de años estrictamente
# anteriores:
#
#   promedio_simce ~ factor(agno) + GSE + dependencia + ruralidad + (1 | rbd)
#
# y para cada colegio objetivo devuelve:
#   contexto_nivel  = lo que se espera de un colegio de ese contexto,
#                     centrado en el promedio nacional del año de
#                     referencia (0 = "colegio nacional promedio").
#   desvio_colegio  = cuánto se desvía este colegio de esa expectativa,
#                     ya encogido por lme4 según cuánta historia tenga.
#   colegio_efecto_historico = la suma. Misma escala e interpretación
#                     que en la v3 (desviación en puntos respecto del
#                     colegio nacional promedio), así que los modelos de
#                     02 y 02b no necesitan cambiar de fórmula.
#
# Un colegio sin historia queda con desvio_colegio = 0, es decir "se
# comporta como los de su contexto" — bastante mejor prior que el 0 de
# la versión anterior, que equivalía a "se comporta como el país".
estimar_efecto_historico <- function(nacional, objetivo, anio_objetivo,
                                    grado_obj, area_obj, sd_tipica = NA_real_) {

  vacio <- objetivo %>%
    transmute(rbd_revisado,
              contexto_nivel = 0, desvio_colegio = 0,
              colegio_efecto_historico = 0, n_anios_hist = 0L)

  previos <- nacional %>%
    filter(agno < anio_objetivo, grado == grado_obj, area == area_obj)
  previos <- previos[contexto_completo(previos), ]

  if (nrow(previos) < 100) return(vacio)

  anio_ref <- max(previos$agno)
  n_anios  <- n_distinct(previos$agno)

  # ¿Se puede estimar un intercepto aleatorio por colegio? Sólo si hay
  # más observaciones que colegios, es decir si al menos parte de los
  # colegios aparece en 2+ años. Con un solo año previo (caso de 2023,
  # que sólo tiene 2022 antes) hay exactamente una fila por colegio: el
  # efecto aleatorio no es identificable y lme4 aborta con
  # "number of levels of each grouping factor must be < number of
  # observations". La parte de contexto SÍ es estimable en ese caso
  # (miles de colegios reparten 5 GSE x 6 dependencias x 2 ruralidades),
  # así que se ajusta sólo la parte fija y el desvío del colegio se
  # calcula a mano como residuo encogido.
  usar_mixto <- n_distinct(previos$rbd_revisado) < nrow(previos)

  if (usar_mixto) {

    formula_hist <- if (n_anios >= 2) {
      promedio_simce ~ factor(agno) + f_gse + f_depe + f_rural + (1 | rbd_revisado)
    } else {
      promedio_simce ~ f_gse + f_depe + f_rural + (1 | rbd_revisado)
    }

    modelo <- lmer(formula_hist, data = previos,
                   control = lmerControl(optimizer = "bobyqa"))

    predecir_fijo <- function(nd) predict(modelo, newdata = nd,
                                          re.form = NA, allow.new.levels = TRUE)

    desvios <- ranef(modelo)$rbd_revisado %>%
      rownames_to_column("rbd_revisado") %>%
      transmute(rbd_revisado = as.integer(rbd_revisado),
                desvio_colegio = `(Intercept)`)

  } else {

    modelo <- lm(promedio_simce ~ f_gse + f_depe + f_rural, data = previos)
    predecir_fijo <- function(nd) predict(modelo, newdata = nd)

    # Encogimiento hecho a mano, replicando lo que haría lme4 si pudiera:
    # el residuo de un colegio mezcla su desvío real con el error de
    # muestreo de su promedio (sd interna^2 / n alumnos). La proporción
    # que es señal es lambda = var_real / (var_real + var_error), y el
    # residuo se multiplica por ella. Sin esto, un colegio de 15 alumnos
    # con un promedio alto por azar arrastraría ese azar al año
    # siguiente como si fuera una característica suya.
    residuos  <- residuals(modelo)
    var_total <- var(residuos)
    var_error <- if (!is.na(sd_tipica) && "nalu" %in% names(previos)) {
      mean(sd_tipica^2 / pmax(previos$nalu, 1), na.rm = TRUE)
    } else 0
    lambda <- if (var_total > 0) max(var_total - var_error, 0) / var_total else 0

    cat(sprintf("   (un solo año previo: efecto de colegio por residuo encogido, lambda = %.2f)\n",
                lambda))

    desvios <- tibble(
      rbd_revisado   = previos$rbd_revisado,
      desvio_colegio = lambda * residuos
    ) %>%
      group_by(rbd_revisado) %>%
      summarise(desvio_colegio = mean(desvio_colegio), .groups = "drop")
  }

  # Centrado: predicción media de la parte fija sobre el universo
  # nacional en el año de referencia.
  nd_nacional   <- previos %>% filter(agno == anio_ref)
  base_nacional <- mean(predecir_fijo(nd_nacional))

  n_hist <- previos %>% count(rbd_revisado, name = "n_anios_hist")

  obj <- objetivo %>% mutate(agno = anio_ref)
  ok  <- contexto_completo(obj)

  # Colegios sin contexto conocido: expectativa 0 = promedio nacional.
  obj$contexto_nivel <- 0
  if (any(ok)) {
    obj$contexto_nivel[ok] <- predecir_fijo(obj[ok, ]) - base_nacional
  }

  obj %>%
    select(rbd_revisado, contexto_nivel) %>%
    left_join(desvios, by = "rbd_revisado") %>%
    left_join(n_hist,  by = "rbd_revisado") %>%
    mutate(
      desvio_colegio = replace_na(desvio_colegio, 0),
      n_anios_hist   = replace_na(n_anios_hist, 0L),
      colegio_efecto_historico = contexto_nivel + desvio_colegio
    )
}

# sd interna típica de los años previos: la necesita el encogimiento
# manual de arriba para saber cuánto del residuo de un colegio es ruido.
sd_tipica_previa <- function(a, g, ar) {
  x <- simce_dist %>% filter(agno < a, grado == g, area == ar)
  if (nrow(x) == 0) NA_real_ else mean(x$sd_simce)
}

efecto_historico <- map_dfr(seq_len(nrow(anios_objetivo)), function(i) {
  a  <- anios_objetivo$agno[i]
  g  <- anios_objetivo$grado[i]
  ar <- anios_objetivo$area[i]
  cat("Efecto histórico + contexto:", a, g, ar, "...\n")
  estimar_efecto_historico(simce_nacional, colegios_objetivo(a, g, ar), a, g, ar,
                           sd_tipica = sd_tipica_previa(a, g, ar)) %>%
    mutate(agno = a, grado = g, area = ar)
})

# ---- 6b. Dispersión histórica, con prior contextual (MODIFICADO) ------
# Misma lógica para el ancho de la distribución interna:
#
#   sd_simce ~ factor(agno) + GSE + dependencia + ruralidad + (1 | rbd)
#
# Advertencia honesta sobre esta pieza: el contexto explica MUY poco de
# la dispersión (R² ~0.01 en el universo nacional). La sd interna baja
# de forma monótona pero suave con el GSE (42.2 puntos en GSE bajo a
# 37.6 en GSE alto, 4b matemática) y el particular pagado es el más
# homogéneo (37.5), pero esas diferencias son chicas comparadas con la
# variación entre colegios del mismo estrato. Acá el contexto sirve casi
# sólo como valor por defecto para colegios sin historia. Se deja porque
# es gratis y es mejor que la constante nacional que se usaba antes,
# pero no hay que esperarle mucho.
estimar_sd_historica <- function(dist_nac, objetivo, anio_objetivo, grado_obj, area_obj) {

  previos <- dist_nac %>%
    filter(agno < anio_objetivo, grado == grado_obj, area == area_obj)
  previos <- previos[contexto_completo(previos), ]

  if (nrow(previos) < 100) {
    return(objetivo %>% transmute(rbd_revisado, contexto_sd = NA_real_,
                                  sd_hist_colegio = NA_real_, n_anios_sd_hist = 0L))
  }

  anio_ref <- max(previos$agno)
  n_anios  <- n_distinct(previos$agno)

  # Mismo problema de identificabilidad que en la sección 6: con un solo
  # año previo hay una fila por colegio y el intercepto aleatorio no es
  # estimable. Acá el encogimiento manual es aún más necesario, porque la
  # sd muestral de un colegio chico es ruidosísima: su varianza de
  # muestreo es aproximadamente sd^2 / (2(n-1)).
  usar_mixto <- n_distinct(previos$rbd_revisado) < nrow(previos)

  if (usar_mixto) {

    formula_sd <- if (n_anios >= 2) {
      sd_simce ~ factor(agno) + f_gse + f_depe + f_rural + (1 | rbd_revisado)
    } else {
      sd_simce ~ f_gse + f_depe + f_rural + (1 | rbd_revisado)
    }

    modelo <- lmer(formula_sd, data = previos,
                   control = lmerControl(optimizer = "bobyqa"))

    predecir_fijo <- function(nd) predict(modelo, newdata = nd,
                                          re.form = NA, allow.new.levels = TRUE)

    desvios <- ranef(modelo)$rbd_revisado %>%
      rownames_to_column("rbd_revisado") %>%
      transmute(rbd_revisado = as.integer(rbd_revisado),
                desvio_sd = `(Intercept)`)

  } else {

    modelo <- lm(sd_simce ~ f_gse + f_depe + f_rural, data = previos)
    predecir_fijo <- function(nd) predict(modelo, newdata = nd)

    residuos  <- residuals(modelo)
    var_total <- var(residuos)
    var_error <- mean(previos$sd_simce^2 / (2 * pmax(previos$n_alu_simce - 1, 1)),
                      na.rm = TRUE)
    lambda <- if (var_total > 0) max(var_total - var_error, 0) / var_total else 0

    cat(sprintf("   (un solo año previo: desvío de sd por residuo encogido, lambda = %.2f)\n",
                lambda))

    desvios <- tibble(
      rbd_revisado = previos$rbd_revisado,
      desvio_sd    = lambda * residuos
    ) %>%
      group_by(rbd_revisado) %>%
      summarise(desvio_sd = mean(desvio_sd), .groups = "drop")
  }

  n_hist <- previos %>% count(rbd_revisado, name = "n_anios_sd_hist")

  obj <- objetivo %>% mutate(agno = anio_ref)
  ok  <- contexto_completo(obj)

  obj$contexto_sd <- mean(previos$sd_simce)   # respaldo: sd nacional promedio
  if (any(ok)) {
    obj$contexto_sd[ok] <- predecir_fijo(obj[ok, ])
  }

  obj %>%
    select(rbd_revisado, contexto_sd) %>%
    left_join(desvios, by = "rbd_revisado") %>%
    left_join(n_hist,  by = "rbd_revisado") %>%
    mutate(
      desvio_sd       = replace_na(desvio_sd, 0),
      n_anios_sd_hist = replace_na(n_anios_sd_hist, 0L),
      # sd esperada = la de su contexto + su propio desvío encogido.
      sd_hist_colegio = pmax(contexto_sd + desvio_sd, 5)
    )
}

sd_historica <- map_dfr(seq_len(nrow(anios_objetivo)), function(i) {
  a  <- anios_objetivo$agno[i]
  g  <- anios_objetivo$grado[i]
  ar <- anios_objetivo$area[i]
  cat("Dispersión histórica + contexto:", a, g, ar, "...\n")
  estimar_sd_historica(dist_nacional, colegios_objetivo(a, g, ar), a, g, ar) %>%
    mutate(agno = a, grado = g, area = ar)
})


# ---- 6c. FORMA de la distribución interna (NUEVO) ---------------------
# Plantilla de cuantiles del puntaje estandarizado dentro del colegio.
# Se calcula con años estrictamente anteriores al año objetivo, y por
# tercil de nivel del colegio: en colegios de bajo rendimiento la cola
# derecha es algo más larga y en los de alto pasa lo contrario (efecto
# piso/techo de la prueba). Se guarda además la versión "todos" como
# respaldo cuando un tercil queda con pocos datos.
#
# Esta es la pieza que reemplaza el supuesto de normalidad: en vez de
# asumir que los puntajes dentro de un colegio se distribuyen normal,
# se usa la forma empírica observada en ~6.400 colegios reales.
calcular_forma_z <- function(alu_todo, anio_objetivo, grado_obj, area_obj) {

  previos <- alu_todo %>%
    filter(agno < anio_objetivo, grado == grado_obj, area == area_obj)

  if (nrow(previos) == 0) {
    # Respaldo: forma normal estándar.
    return(tibble(tercil = "todos", p = GRILLA_P, z = qnorm(GRILLA_P)))
  }

  stats_col <- previos %>%
    group_by(agno, rbd_revisado) %>%
    summarise(n_c = n(), media_c = mean(ptje), sd_c = sd(ptje), .groups = "drop") %>%
    filter(n_c >= MIN_ALU_SD, !is.na(sd_c), sd_c > 0) %>%
    group_by(agno) %>%
    mutate(tercil = cut(media_c,
                        breaks = c(-Inf, quantile(media_c, c(1/3, 2/3), names = FALSE), Inf),
                        labels = c("bajo", "medio", "alto"))) %>%
    ungroup()

  z_todos <- previos %>%
    inner_join(stats_col, by = c("agno", "rbd_revisado")) %>%
    mutate(z = (ptje - media_c) / sd_c)

  por_tercil <- z_todos %>%
    group_by(tercil) %>%
    filter(n() >= 5000) %>%
    group_modify(~ tibble(p = GRILLA_P, z = quantile(.x$z, GRILLA_P, names = FALSE))) %>%
    ungroup() %>%
    mutate(tercil = as.character(tercil))

  bind_rows(
    por_tercil,
    tibble(tercil = "todos", p = GRILLA_P,
           z = quantile(z_todos$z, GRILLA_P, names = FALSE))
  )
}

forma_z <- map_dfr(seq_len(nrow(anios_objetivo)), function(i) {
  cat("Calculando forma de la distribución interna:",
      anios_objetivo$agno[i], anios_objetivo$grado[i], anios_objetivo$area[i], "...\n")
  calcular_forma_z(simce_alumno,
                   anios_objetivo$agno[i],
                   anios_objetivo$grado[i],
                   anios_objetivo$area[i]) %>%
    mutate(agno  = anios_objetivo$agno[i],
           grado = anios_objetivo$grado[i],
           area  = anios_objetivo$area[i])
})

# Cortes de tercil para asignar cada colegio a una plantilla de forma
# en el momento de predecir (usando su media PREDICHA, no la observada).
cortes_tercil <- map_dfr(seq_len(nrow(anios_objetivo)), function(i) {
  a <- anios_objetivo$agno[i]; g <- anios_objetivo$grado[i]; ar <- anios_objetivo$area[i]
  previos <- simce_dist %>% filter(agno < a, grado == g, area == ar)
  if (nrow(previos) == 0) {
    return(tibble(agno = a, grado = g, area = ar,
                  corte_1 = NA_real_, corte_2 = NA_real_))
  }
  q <- quantile(previos$media_simce_alu, c(1/3, 2/3), names = FALSE)
  tibble(agno = a, grado = g, area = ar, corte_1 = q[1], corte_2 = q[2])
})

# ---- 6d. Confiabilidades (NUEVO, sólo diagnóstico) --------------------
# ¿Cuánto puede, como máximo, correlacionar el ensayo con el SIMCE a
# nivel individual? La correlación observable entre dos mediciones está
# acotada por sus confiabilidades: rho <= sqrt(conf_ensayo * conf_simce).
# Este número se usa en 03 para poner un techo razonable al parámetro
# RHO_ENSAYO_SIMCE de la versión A (ver README, punto 5b).
#
#  - Confiabilidad del SIMCE dentro del colegio: 1 - eem^2 / var_interna
#    (el archivo de alumnos trae el error estándar de medición).
#  - Confiabilidad del promedio de ensayos del estudiante: ICC entre
#    estudiantes (tras descontar la dificultad de cada ensayo),
#    corregida por Spearman-Brown según cuántos ensayos rindió.
conf_simce <- simce_alumno %>%
  inner_join(simce_dist %>% select(agno, grado, area, rbd_revisado, sd_simce),
             by = c("agno", "grado", "area", "rbd_revisado")) %>%
  filter(!is.na(eem)) %>%
  group_by(grado, area) %>%
  summarise(
    var_interna  = mean(sd_simce^2),
    eem2         = mean(eem^2),
    conf_simce   = 1 - eem2 / var_interna,
    .groups = "drop"
  )

conf_ensayo <- ensayos_dedup %>%
  group_by(agno, grado, area, n_evaluacion) %>%
  mutate(x = porcentaje_logro - mean(porcentaje_logro)) %>%   # quita dificultad del ensayo
  group_by(agno, grado, area, id_usuario_curso) %>%
  filter(n() >= 2) %>%
  mutate(x_medio = mean(x)) %>%
  group_by(grado, area) %>%
  summarise(
    n_estudiantes = n_distinct(paste(agno, id_usuario_curso)),
    var_within = sum((x - x_medio)^2) / (n() - n_estudiantes),
    var_total  = var(x),
    n_prom     = n() / n_estudiantes,   # ensayos por estudiante
    icc        = pmax(var_total - var_within, 0) / pmax(var_total, 1e-9),
    conf_ensayo = n_prom * icc / (1 + (n_prom - 1) * icc),
    .groups = "drop"
  ) %>%
  select(grado, area, icc, n_prom, conf_ensayo)

confiabilidades <- conf_simce %>%
  select(grado, area, conf_simce) %>%
  inner_join(conf_ensayo, by = c("grado", "area")) %>%
  mutate(rho_maximo = sqrt(conf_simce * conf_ensayo))

cat("\nConfiabilidades estimadas (techo de la correlación ensayo-SIMCE individual):\n")
print(confiabilidades %>% mutate(across(where(is.numeric), ~round(.x, 3))))

# ---- 7. Pegar todo a nivel de colegio y de estudiante ------------------
# Tabla de contexto por colegio, con etiquetas legibles: sirve para
# reportar (comparar un colegio contra su estrato GSE, por ejemplo) y
# para auditar de dónde salió su expectativa.
contexto_colegio <- contexto_rezagado %>%
  left_join(etiquetas_gse,   by = "cod_grupo") %>%
  left_join(etiquetas_depe1, by = "cod_depe1") %>%
  left_join(etiquetas_depe2, by = "cod_depe2") %>%
  left_join(etiquetas_rural, by = "cod_rural_rbd") %>%
  left_join(efecto_historico %>% select(agno, grado, area, rbd_revisado,
                                        contexto_nivel, desvio_colegio, n_anios_hist),
            by = c("agno", "grado", "area", "rbd_revisado")) %>%
  left_join(sd_historica %>% select(agno, grado, area, rbd_revisado,
                                    contexto_sd, n_anios_sd_hist),
            by = c("agno", "grado", "area", "rbd_revisado"))

vars_contexto <- c("cod_grupo", "gse_etiqueta", "cod_depe1", "depe1_etiqueta",
                   "cod_depe2", "depe2_etiqueta", "cod_rural_rbd", "rural_etiqueta",
                   "nom_com_rbd", "agno_contexto")

school_features <- school_features %>%
  left_join(efecto_historico %>% select(agno, grado, area, rbd_revisado,
                                        contexto_nivel, desvio_colegio,
                                        colegio_efecto_historico, n_anios_hist),
            by = c("agno", "grado", "area", "rbd_revisado")) %>%
  left_join(sd_historica %>% select(agno, grado, area, rbd_revisado,
                                    contexto_sd, sd_hist_colegio, n_anios_sd_hist),
            by = c("agno", "grado", "area", "rbd_revisado")) %>%
  left_join(contexto_colegio %>% select(agno, grado, area, rbd_revisado,
                                        all_of(vars_contexto)),
            by = c("agno", "grado", "area", "rbd_revisado")) %>%
  mutate(
    across(c(contexto_nivel, desvio_colegio, colegio_efecto_historico),
           ~ replace_na(.x, 0)),
    n_anios_hist    = replace_na(n_anios_hist, 0L),
    n_anios_sd_hist = replace_na(n_anios_sd_hist, 0L),
    sin_historia    = n_anios_hist == 0,
    sin_contexto    = is.na(cod_grupo)
  ) %>%
  # Respaldo final por si un grupo entero quedó sin modelo de dispersión.
  group_by(grado, area) %>%
  mutate(
    contexto_sd     = coalesce(contexto_sd, mean(contexto_sd, na.rm = TRUE)),
    sd_hist_colegio = coalesce(sd_hist_colegio, contexto_sd)
  ) %>%
  ungroup()

ind_features <- ind_features %>%
  left_join(school_features %>% select(agno, grado, area, rbd_revisado,
                                       colegio_efecto_historico, contexto_nivel,
                                       all_of(vars_contexto)),
            by = c("agno", "grado", "area", "rbd_revisado")) %>%
  mutate(colegio_efecto_historico = replace_na(colegio_efecto_historico, 0))

cat("\nColegios sin historia previa (reciben el prior de su contexto):",
    sum(school_features$sin_historia), "de", nrow(school_features), "\n")
cat("Colegios sin contexto conocido (reciben expectativa 0):",
    sum(school_features$sin_contexto), "\n")


# ---- 8. Cruce final con el SIMCE del MISMO año (para entrenar) --------
# Ahora se traen DOS targets: el promedio (modelo de 02) y la sd interna
# (modelo de 02b). El promedio sigue viniendo del archivo oficial por
# RBD; la sd sólo puede venir del archivo de alumnos.
school_model_data <- school_features %>%
  inner_join(
    simce_limpio %>% select(agno, grado, area, rbd_revisado, promedio_simce),
    by = c("agno", "grado", "area", "rbd_revisado")
  ) %>%
  left_join(
    simce_dist %>% select(agno, grado, area, rbd_revisado,
                          n_alu_simce, media_simce_alu, sd_simce),
    by = c("agno", "grado", "area", "rbd_revisado")
  )

cat("\nFilas ind_features:      ", nrow(ind_features), "\n")
cat("Filas school_features:   ", nrow(school_features), "\n")
cat("Filas school_model_data: ", nrow(school_model_data), " (colegios con ensayo Y simce cruzados)\n")
cat("  de ellas con sd_simce observada: ", sum(!is.na(school_model_data$sd_simce)), "\n\n")
cat("Cobertura por año/grado/área en school_model_data:\n")
print(school_model_data %>% count(agno, grado, area) %>%
        pivot_wider(names_from = area, values_from = n))

# Composición de la base Santillana. Vale la pena mirarla cada vez: es
# la razón por la que los coeficientes de contexto NO se estiman acá
# sino en el universo nacional.
cat("\nComposición de la base Santillana por contexto (último año, matemática):\n")
print(
  school_features %>%
    filter(agno == max(agno), area == "matematica") %>%
    count(grado, depe2_etiqueta, gse_etiqueta) %>%
    pivot_wider(names_from = gse_etiqueta, values_from = n, values_fill = 0)
)

# ---- 8b. Descriptivos para la presentación (NUEVO) ---------------------
# Estas tablas se calculan ACÁ y no en 05_figuras_presentacion.R porque
# dependen del archivo de alumnos completo (~2,6 M filas) y de las bases
# nacionales, que en este punto ya están en memoria. Lo que se guarda es
# el resumen —unas pocas decenas de filas y una grilla de densidad de 256
# puntos por grupo—, no los datos crudos: la presentación queda liviana y
# no necesita acceso a los parquet.
desc_ensayos <- ensayos_dedup %>%
  group_by(agno, grado, area) %>%
  summarise(
    n_colegios_ensayo = n_distinct(rbd_revisado),
    n_estudiantes     = n_distinct(id_usuario_curso),
    n_ensayos         = n(),
    logro_medio       = mean(porcentaje_logro),
    logro_sd          = sd(porcentaje_logro),
    ensayos_por_est   = n() / n_distinct(id_usuario_curso),
    .groups = "drop"
  )

desc_simce_alu <- simce_alumno %>%
  group_by(agno, grado, area) %>%
  summarise(
    n_alumnos        = n(),
    n_colegios_simce = n_distinct(rbd_revisado),
    ptje_medio       = mean(ptje),
    ptje_sd          = sd(ptje),
    ptje_p10         = quantile(ptje, 0.10, names = FALSE),
    ptje_p90         = quantile(ptje, 0.90, names = FALSE),
    .groups = "drop"
  )

# Dispersión INTERNA típica (entre alumnos del mismo colegio): es la
# magnitud que el modelo de 02b tiene que acertar.
desc_sd_interna <- simce_dist %>%
  group_by(agno, grado, area) %>%
  summarise(
    n_colegios       = n(),
    sd_interna_media = mean(sd_simce),
    sd_interna_p10   = quantile(sd_simce, 0.10, names = FALSE),
    sd_interna_p90   = quantile(sd_simce, 0.90, names = FALSE),
    .groups = "drop"
  )

# Curva de densidad de los puntajes individuales del último año, ya
# evaluada en una grilla: evita mover millones de puntos a la presentación.
dens_simce_alu <- simce_alumno %>%
  filter(agno == max(agno)) %>%
  group_by(grado, area) %>%
  group_modify(~ {
    d <- density(.x$ptje, n = 256)
    tibble(x = d$x, y = d$y)
  }) %>%
  ungroup()

# Composición por GSE: colegios con ensayo vs. universo nacional. Es la
# evidencia de que la base no es representativa (limitación del punto 5).
comp_gse <- bind_rows(
  school_features %>%
    filter(agno == max(agno), area == "matematica") %>%
    distinct(grado, rbd_revisado, gse_etiqueta) %>%
    count(grado, gse_etiqueta, name = "n") %>%
    mutate(fuente = "santillana"),
  simce_nacional %>%
    filter(agno == max(agno), grado %in% GRADOS_MODELO, area == "matematica") %>%
    left_join(etiquetas_gse, by = "cod_grupo") %>%
    distinct(grado, rbd_revisado, gse_etiqueta) %>%
    count(grado, gse_etiqueta, name = "n") %>%
    mutate(fuente = "nacional")
) %>%
  filter(!is.na(gse_etiqueta)) %>%
  mutate(gse_etiqueta = factor(gse_etiqueta,
                               levels = etiquetas_gse$gse_etiqueta)) %>%
  group_by(fuente, grado) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

# Cobertura del ensayo respecto del SIMCE, por colegio: sostiene el
# argumento de que el ensayo cubre el curso completo.
cobertura_ensayo <- school_features %>%
  select(agno, grado, area, rbd_revisado, n_estudiantes) %>%
  inner_join(simce_dist %>% select(agno, grado, area, rbd_revisado, n_alu_simce),
             by = c("agno", "grado", "area", "rbd_revisado")) %>%
  mutate(cobertura = n_estudiantes / n_alu_simce) %>%
  group_by(agno, grado, area) %>%
  summarise(cobertura_mediana = median(cobertura),
            n_colegios = n(), .groups = "drop")

# --- Resumen por año de cada fuente ------------------------------------
# Tamaño de cada base, en los términos en que se describe en la
# presentación: cuántos colegios y cuántos alumnos distintos hay por año.
desc_simce_anio <- simce_alumno %>%
  group_by(agno) %>%
  summarise(
    n_colegios = n_distinct(rbd_revisado),
    n_alumnos  = n_distinct(idalumno),
    n_puntajes = n(),                      # alumno x área
    .groups = "drop"
  )

# En los ensayos, un mismo alumno rinde varias veces y en dos áreas: el
# promedio se calcula por alumno-área, que es la unidad que después usa
# el modelo de crecimiento.
desc_ensayos_anio <- ensayos_dedup %>%
  group_by(agno) %>%
  summarise(
    n_colegios          = n_distinct(rbd_revisado),
    n_alumnos           = n_distinct(id_usuario_curso),
    n_ensayos           = n(),
    ensayos_por_alumno  = n() / n_distinct(paste(id_usuario_curso, area)),
    .groups = "drop"
  )

# --- Varianza dentro y entre colegios ----------------------------------
# Descomposición exacta (suma de cuadrados) de la varianza de los puntajes
# individuales. Es el argumento numérico de por qué no basta con predecir
# el promedio del colegio: buena parte de la variación ocurre ENTRE
# estudiantes del mismo colegio, no entre colegios.
varianza_simce <- simce_alumno %>%
  filter(agno == max(agno)) %>%
  group_by(grado, area) %>%
  group_modify(function(.x, .y) {
    media_global <- mean(.x$ptje)
    por_colegio <- .x %>%
      group_by(rbd_revisado) %>%
      summarise(n = n(), media = mean(ptje),
                ss = sum((ptje - mean(ptje))^2), .groups = "drop")
    ss_entre  <- sum(por_colegio$n * (por_colegio$media - media_global)^2)
    ss_dentro <- sum(por_colegio$ss)
    ss_total  <- ss_entre + ss_dentro
    gl        <- nrow(.x) - 1
    tibble(
      n_alumnos  = nrow(.x),
      n_colegios = nrow(por_colegio),
      var_total  = ss_total / gl,
      var_entre  = ss_entre / gl,
      var_dentro = ss_dentro / gl,
      sd_total   = sqrt(ss_total / gl),
      sd_entre   = sqrt(ss_entre / gl),
      sd_dentro  = sqrt(ss_dentro / gl),
      pct_entre  = ss_entre / ss_total,
      pct_dentro = ss_dentro / ss_total
    )
  }) %>%
  ungroup()

cat("\nDescomposición de la varianza del SIMCE (último año):\n")
print(varianza_simce %>%
        transmute(grado, area, sd_total = round(sd_total, 1),
                  `% entre colegios` = round(100 * pct_entre),
                  `% dentro del colegio` = round(100 * pct_dentro)))

# --- Un colegio de ejemplo ---------------------------------------------
# Para mostrar en concreto que dentro de un solo colegio hay mucha
# variación se elige el colegio con más alumnos del grupo de referencia.
# Se exportan sus puntajes (unos cientos de valores) y la densidad
# nacional del mismo grupo, como referencia.
GRADO_EJEMPLO <- "4b"
AREA_EJEMPLO  <- "matematica"

ejemplo_grupo <- simce_alumno %>%
  filter(agno == max(agno), grado == GRADO_EJEMPLO, area == AREA_EJEMPLO)

rbd_ejemplo <- ejemplo_grupo %>%
  count(rbd_revisado) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  pull(rbd_revisado)

puntajes_ejemplo <- ejemplo_grupo %>% filter(rbd_revisado == rbd_ejemplo)

dens_ref <- density(ejemplo_grupo$ptje, n = 256)

colegio_ejemplo <- list(
  meta = tibble(
    agno            = max(simce_alumno$agno),
    grado           = GRADO_EJEMPLO,
    area            = AREA_EJEMPLO,
    rbd             = rbd_ejemplo,
    n_alumnos       = nrow(puntajes_ejemplo),
    media_colegio   = mean(puntajes_ejemplo$ptje),
    sd_colegio      = sd(puntajes_ejemplo$ptje),
    media_nacional  = mean(ejemplo_grupo$ptje),
    sd_nacional     = sd(ejemplo_grupo$ptje)
  ),
  puntajes      = puntajes_ejemplo %>% select(ptje),
  dens_nacional = tibble(x = dens_ref$x, y = dens_ref$y)
)

# --- Ensayos: forma y progresión ---------------------------------------
dens_logro_ensayo <- ind_features %>%
  filter(agno == max(agno)) %>%
  group_by(grado, area) %>%
  group_modify(~ {
    d <- density(.x$mean_logro, n = 256)
    tibble(x = d$x, y = d$y)
  }) %>%
  ungroup()

# ¿Sube el logro a medida que avanzan los ensayos del año? Es la señal
# que recoge `slope_logro`.
logro_por_evaluacion <- ensayos_dedup %>%
  group_by(agno, grado, area, n_evaluacion) %>%
  summarise(logro_medio = mean(porcentaje_logro),
            n_obs = n(), .groups = "drop") %>%
  filter(n_obs >= 30)

# --- Evolución conjunta de ambas fuentes -------------------------------
# En escalas distintas no son comparables, así que cada serie se
# estandariza dentro de su grado x área: lo que interesa es si se mueven
# juntas de un año a otro, no su nivel.
z_seguro <- function(x) if (length(x) > 1 && sd(x) > 0) (x - mean(x)) / sd(x) else 0

evolucion_fuentes <- school_model_data %>%
  group_by(agno, grado, area) %>%
  summarise(n_colegios = n(),
            simce = mean(promedio_simce, na.rm = TRUE),
            logro = mean(mean_logro, na.rm = TRUE),
            .groups = "drop") %>%
  group_by(grado, area) %>%
  mutate(z_simce = z_seguro(simce), z_logro = z_seguro(logro)) %>%
  ungroup()

# --- SIMCE promedio por GSE --------------------------------------------
# Sobre el universo nacional, que es donde hay variación en GSE.
simce_por_gse <- simce_nacional %>%
  filter(agno == max(agno), grado %in% GRADOS_MODELO) %>%
  left_join(etiquetas_gse, by = "cod_grupo") %>%
  filter(!is.na(gse_etiqueta)) %>%
  group_by(grado, area, gse_etiqueta) %>%
  summarise(n_colegios  = n(),
            simce_medio = mean(promedio_simce),
            simce_sd    = sd(promedio_simce),
            .groups = "drop") %>%
  mutate(gse_etiqueta = factor(gse_etiqueta, levels = etiquetas_gse$gse_etiqueta))

descriptivos <- list(
  desc_ensayos         = desc_ensayos,
  desc_simce_alu       = desc_simce_alu,
  desc_sd_interna      = desc_sd_interna,
  desc_simce_anio      = desc_simce_anio,
  desc_ensayos_anio    = desc_ensayos_anio,
  dens_simce_alu       = dens_simce_alu,
  dens_logro_ensayo    = dens_logro_ensayo,
  logro_por_evaluacion = logro_por_evaluacion,
  varianza_simce       = varianza_simce,
  colegio_ejemplo      = colegio_ejemplo,
  evolucion_fuentes    = evolucion_fuentes,
  simce_por_gse        = simce_por_gse,
  comp_gse             = comp_gse,
  cobertura_ensayo     = cobertura_ensayo,
  confiabilidades      = confiabilidades
)

cat("\nDescriptivos para la presentación (último año):\n")
print(desc_simce_alu %>% filter(agno == max(agno)))

# ---- 9. Guardar --------------------------------------------------------
saveRDS(descriptivos,      dir_salidas %>% file.path("descriptivos.rds"))
saveRDS(ind_features,      dir_salidas %>% file.path("ind_features.rds"))
saveRDS(school_features,   dir_salidas %>% file.path("school_features.rds"))
saveRDS(school_model_data, dir_salidas %>% file.path("school_model_data.rds"))
saveRDS(efecto_historico,  dir_salidas %>% file.path("efecto_historico.rds"))
saveRDS(contexto_colegio,  dir_salidas %>% file.path("contexto_colegio.rds"))
saveRDS(simce_alumno,      dir_salidas %>% file.path("simce_alumno.rds"))
saveRDS(simce_dist,        dir_salidas %>% file.path("simce_dist.rds"))
saveRDS(forma_z,           dir_salidas %>% file.path("forma_z.rds"))
saveRDS(cortes_tercil,     dir_salidas %>% file.path("cortes_tercil.rds"))
saveRDS(limites_simce,     dir_salidas %>% file.path("limites_simce.rds"))
saveRDS(confiabilidades,   dir_salidas %>% file.path("confiabilidades.rds"))

cat("\nListo. Objetos guardados en output/modelo_lme/*.rds\n")
