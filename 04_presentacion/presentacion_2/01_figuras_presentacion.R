# =============================================================
# 05_figuras_presentacion.R   (NUEVO)
# -------------------------------------------------------------
# Construye TODAS las tablas y gráficos que se muestran en la
# presentación (presentacion_modelo_simce.qmd) y los deja en un
# único archivo:
#
#     output/modelo_lme_alu/figuras_presentacion.rds
#
# La presentación sólo carga ese .rds y los imprime: no calcula
# nada. Así el .qmd renderiza en segundos, no necesita acceso a los
# parquet ni vuelve a ajustar modelos, y la presentación queda
# reproducible aunque se arme en otro computador.
#
# Reparto de trabajo (por qué algunas cosas NO se calculan acá):
#
#   - Lo que exige el archivo de alumnos completo (~2,6 M filas)
#     —descriptivos, densidades, composición por contexto— se
#     calcula en 01_preparar_datos.R, que ya tiene esos datos en
#     memoria, y se exporta resumido en `descriptivos.rds`.
#   - Lo que exige los modelos ajustados —observado vs. predicho del
#     año de prueba— se exporta desde 02 (`diag_nivel.rds`) y
#     02b (`diag_dispersion.rds`), donde los modelos existen.
#   - Lo que exige comparar predicciones contra puntajes reales se
#     exporta desde 04 (`validacion_por_colegio.rds`,
#     `dens_validacion.rds`).
#
# Este script sólo LEE esas salidas, les da formato y arma las
# figuras. Si falta algún insumo, esa figura queda en NULL y la
# presentación muestra en su lugar el texto de la propuesta: nunca
# falla el render.
#
# Los gráficos se guardan en dos versiones:
#   graficos            -> plotly (interactivos, para el HTML)
#   graficos_estaticos  -> ggplot (por si se exporta la presentación
#                          a PDF, donde plotly no se imprime bien)
# =============================================================

library(tidyverse)
library(plotly)

# ---- 0. Configuración --------------------------------------------
usuario <- Sys.info()[["user"]]
rutas <- config::get(config = usuario, file = "config.yml")
ruta_outputs <- rutas$ruta_outputs
dir_salidas <- ruta_outputs %>% file.path('modelo_lme_alu_v2')

# Modelo anterior (sin IRT) y regla de conversión externa ("modelo 1"):
# insumos del escenario 2 y 3 en las comparaciones de resultados. El
# escenario 1 (este script) siempre es "modelo final" = modelo_lme_alu_v2.
dir_salidas_anterior <- ruta_outputs %>% file.path('modelo_lme_alu')
ruta_consolidado_ensayos <- rutas$ruta_data_intermedia %>%
  file.path('ensayo_santillana', 'consolidado_ensayo_santillana.parquet')

# Paleta: la misma del tema de la presentación (tema_simce.scss).
COL <- c(tinta    = "#10303B",
         lago     = "#1F7A8C",
         marigold = "#E9A13B",
         coral    = "#C95D5D",
         bruma    = "#8CA3A8")

ORDEN_GRADO <- c("4b", "2m")
ETIQ_GRADO  <- c("4b" = "4° básico", "2m" = "2° medio")
ETIQ_AREA   <- c("matematica" = "Matemática", "lenguaje" = "Lenguaje")

# Alto de cada gráfico dentro de la lámina. Medido sobre la plantilla:
# con título y bajada quedan ~580 px libres, así que 420 deja aire.
ALTO_GRAFICO <- 420

# ---- 0b. Utilidades ----------------------------------------------
# Lectura tolerante: si el insumo no existe, devuelve NULL y avisa.
leer_rds <- function(nombre) {
  ruta <- dir_salidas %>% file.path(nombre)
  if (!file.exists(ruta)) {
    cat("  [falta]", nombre, "-> las figuras que dependen de él quedarán vacías\n")
    return(NULL)
  }
  readRDS(ruta)
}
leer_csv <- function(nombre) {
  ruta <- dir_salidas %>% file.path(nombre)
  if (!file.exists(ruta)) {
    cat("  [falta]", nombre, "-> las figuras que dependen de él quedarán vacías\n")
    return(NULL)
  }
  suppressMessages(read_csv(ruta, show_col_types = FALSE))
}
# Igual que leer_rds(), pero desde un directorio arbitrario: se usa para leer
# los insumos del modelo anterior (modelo_lme_alu) sin cambiar dir_salidas.
leer_rds_de <- function(dir, nombre) {
  ruta <- dir %>% file.path(nombre)
  if (!file.exists(ruta)) {
    cat("  [falta]", nombre, "en", dir, "-> las figuras que dependen de él quedarán vacías\n")
    return(NULL)
  }
  readRDS(ruta)
}

# Construye una figura sin arriesgar el script completo: si los datos
# faltan o algo falla, devuelve NULL y sigue.
construir <- function(nombre, insumos, expr) {
  if (any(map_lgl(insumos, is.null))) {
    cat("  -", nombre, ": sin insumos, se omite\n")
    return(NULL)
  }
  out <- tryCatch(expr, error = function(e) {
    cat("  -", nombre, ": error ->", conditionMessage(e), "\n")
    NULL
  })
  if (!is.null(out)) cat("  -", nombre, ": ok\n")
  out
}

# Etiquetas legibles. Tolera tablas que traigan sólo grado o sólo área.
etiquetar <- function(d) {
  if ("grado" %in% names(d)) {
    d$grado <- factor(d$grado, levels = ORDEN_GRADO,
                      labels = ETIQ_GRADO[ORDEN_GRADO])
  }
  if ("area" %in% names(d)) {
    d$area <- unname(ifelse(d$area %in% names(ETIQ_AREA),
                            ETIQ_AREA[d$area], d$area))
  }
  d
}

tema_pres <- theme_minimal(base_size = 12) +
  theme(
    text            = element_text(colour = COL[["tinta"]]),
    plot.title      = element_blank(),      # el título va en la lámina
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(colour = "#E3E8E6"),
    strip.text      = element_text(face = "bold", colour = COL[["tinta"]]),
    legend.position = "bottom",
    legend.title    = element_blank()
  )

# ggplot -> plotly, con la configuración que conviene dentro de una
# diapositiva: sin barra de herramientas, leyenda horizontal abajo y
# tooltip tomado del aes(text = ...).
interactivo <- function(p, alto = ALTO_GRAFICO, leyenda = TRUE) {
  g <- ggplotly(p, tooltip = "text", height = alto) %>%
    config(displayModeBar = FALSE, responsive = TRUE) %>%
    layout(
      margin = list(l = 55, r = 15, b = 45, t = 25),
      hoverlabel = list(bgcolor = "white", bordercolor = COL[["lago"]]),
      # Leyenda horizontal, centrada bajo el gráfico y sin título: el
      # título repite lo que ya dice el eje o el propio color.
      legend = if (leyenda) {
        list(orientation = "h", x = 0.5, xanchor = "center",
             y = -0.18, yanchor = "top",
             title = list(text = ""))
      } else list()
    )
  if (!leyenda) g <- g %>% layout(showlegend = FALSE)
  g
}

# ---- 1. Insumos ---------------------------------------------------
cat("Leyendo insumos desde", dir_salidas, "\n")

descriptivos     <- leer_rds("descriptivos.rds")          # 01
school_model     <- leer_rds("school_model_data.rds")     # 01
forma_z          <- leer_rds("forma_z.rds")               # 01
diag_nivel       <- leer_rds("diag_nivel.rds")            # 02
diag_dispersion  <- leer_rds("diag_dispersion.rds")       # 02b
met_nivel        <- leer_csv("metricas_validacion.csv")   # 02
met_dispersion   <- leer_csv("metricas_dispersion.csv")   # 02b
coherencia       <- leer_csv("chequeo_coherencia.csv")    # 03
val_dist         <- leer_csv("validacion_distribucional.csv")  # 04
val_estrato      <- leer_csv("validacion_por_estrato.csv")     # 04
val_colegio      <- leer_rds("validacion_por_colegio.rds")     # 04
dens_validacion  <- leer_rds("dens_validacion.rds")            # 04
anio_test        <- leer_rds("anio_test.rds")                  # 02
niveles_logro    <- leer_rds("niveles_logro.rds")              # 04

anio_test <- if (is.null(anio_test)) NA else anio_test

# --- Insumos del escenario "modelo anterior" (sin IRT) ---
diag_nivel_anterior <- leer_rds_de(dir_salidas_anterior, "diag_nivel.rds")
pred_ind_anterior   <- leer_rds_de(dir_salidas_anterior, "predicciones_individual.rds")

# --- Insumo del escenario "modelo 1" (regla de conversión Santillana) ---
# Predicción pegada por evaluación en el consolidado de ensayos; se agrega
# a nivel de estudiante (así un estudiante con más ensayos rendidos no pesa
# más que uno con uno solo). De acá salen tanto la predicción por colegio
# como la clasificación por nivel de aprendizaje.
modelo1_alumno <- construir("modelo1_alumno",
  list(if (file.exists(ruta_consolidado_ensayos)) TRUE else NULL, anio_test), {
  arrow::read_parquet(ruta_consolidado_ensayos) %>%
    filter(agno == anio_test, !is.na(simce_estimado_modelo1)) %>%
    mutate(rbd_revisado = as.numeric(rbd)) %>%
    group_by(rbd_revisado, grado, area, id_usuario_curso) %>%
    summarise(pred_alu = mean(simce_estimado_modelo1, na.rm = TRUE), .groups = "drop")
})

modelo1_colegio <- construir("modelo1_colegio", list(modelo1_alumno), {
  modelo1_alumno %>%
    group_by(rbd_revisado, grado, area) %>%
    summarise(pred_modelo1 = mean(pred_alu, na.rm = TRUE), .groups = "drop")
})

# --- Comparación de escenarios a nivel colegio: modelo 1 / anterior / final ---
# Universo: colegios del set de prueba del modelo final (diag_nivel) que
# además tienen predicción del modelo anterior y del modelo 1. Es la misma
# base para la tabla ampliada de validación de nivel y para la lámina
# dedicada de comparación de escenarios.
comparacion_escenarios <- construir("comparacion_escenarios",
  list(diag_nivel, diag_nivel_anterior, modelo1_colegio), {
  diag_nivel %>%
    transmute(rbd_revisado, grado, area, observado, pred_final = predicho) %>%
    inner_join(
      diag_nivel_anterior %>% transmute(rbd_revisado, grado, area, pred_anterior = predicho),
      by = c("rbd_revisado", "grado", "area")
    ) %>%
    inner_join(modelo1_colegio, by = c("rbd_revisado", "grado", "area"))
})

# 2. TABLAS ----

cat("\nTablas:\n")
tablas <- list()

# MAE/R² por escenario (modelo 1 / anterior / final), sobre el universo
# comparable de comparacion_escenarios. Formato largo -> se usa tanto para
# ampliar t_nivel como para la tabla dedicada t_comparacion_escenarios.
mae_escenarios <- construir("mae_escenarios", list(comparacion_escenarios), {
  comparacion_escenarios %>%
    pivot_longer(c(pred_modelo1, pred_anterior, pred_final),
                 names_to = "escenario", values_to = "pred") %>%
    mutate(escenario = recode(escenario,
                              pred_modelo1  = "Modelo 1",
                              pred_anterior = "Modelo anterior",
                              pred_final    = "Modelo final")) %>%
    group_by(grado, area, escenario) %>%
    summarise(
      n_colegios = n(),
      mae = mean(abs(pred - observado), na.rm = TRUE),
      r2  = 1 - sum((observado - pred)^2, na.rm = TRUE) /
                sum((observado - mean(observado, na.rm = TRUE))^2, na.rm = TRUE),
      .groups = "drop"
    )
})

## --- 2.1 Descriptivos generales de los datos (lámina "Los datos en cifras") ----
tablas$t_datos <- construir("t_datos", list(descriptivos), {
  ens <- descriptivos$desc_ensayos %>% filter(agno == max(agno))
  alu <- descriptivos$desc_simce_alu %>% filter(agno == max(agno))
  sdi <- descriptivos$desc_sd_interna %>% filter(agno == max(agno))

  ens %>%
    left_join(alu, by = c("agno", "grado", "area")) %>%
    left_join(sdi, by = c("agno", "grado", "area")) %>%
    etiquetar() %>%
    arrange(grado, area) %>%
    transmute(
      Grado = grado, Área = area,
      `Colegios` = n_colegios_ensayo,
      `Estudiantes con ensayo` = n_estudiantes,
      `Ensayos rendidos` = n_ensayos,
      `Logro medio (%)` = round(logro_medio, 1),
      `Alumnos con SIMCE` = n_alumnos,
      `SIMCE medio` = round(ptje_medio, 1),
      `sd entre alumnos` = round(ptje_sd, 1),
      `sd interna del colegio` = round(sd_interna_media, 1)
    )
})

## --- 2.2 Resultados SIMCE: tamaño de la base por año ----
tablas$t_simce_anio <- construir("t_simce_anio", list(descriptivos), {
  descriptivos$desc_simce_anio %>%
    arrange(agno) %>%
    transmute(
      `Año` = agno,
      `Colegios` = format(n_colegios, big.mark = "."),
      `Alumnos con puntaje` = format(n_alumnos, big.mark = ".")
    )
})

## --- 2.3 Ensayos Santillana: tamaño de la base por año ----
tablas$t_ensayos_anio <- construir("t_ensayos_anio", list(descriptivos), {
  descriptivos$desc_ensayos_anio %>%
    arrange(agno) %>%
    transmute(
      `Año` = agno,
      `Colegios` = format(n_colegios, big.mark = "."),
      `Alumnos` = format(n_alumnos, big.mark = "."),
      `Ensayos rendidos` = format(n_ensayos, big.mark = "."),
      `Ensayos por alumno (en cada área)` = round(ensayos_por_alumno, 1)
    )
})

## --- 2.4 Descomposición de la varianza del SIMCE ----
# Acompaña al gráfico del colegio de ejemplo: cuánta de la variación
# total ocurre entre colegios y cuánta entre alumnos del mismo colegio.
tablas$t_varianza <- construir("t_varianza", list(descriptivos), {
  abrev_area <- c("matematica" = "Matemática", "lenguaje" = "Lenguaje")
  descriptivos$varianza_simce %>%
    mutate(grado = factor(grado, levels = ORDEN_GRADO)) %>%
    arrange(grado, area) %>%
    transmute(
      Grado = grado,
      Area = abrev_area[area],
      `sd total` = round(sd_total, 1),
      `Entre colegios` = round(sd_entre, 1),
      `Dentro de colegios` = round(sd_dentro, 1),
      `% dentro de colegios` = paste0(round(100 * pct_dentro), "%")
    )
})

# --- 2.5 Validación del modelo de nivel
tablas$t_nivel <- construir("t_nivel", list(met_nivel), {
  base <- met_nivel
  # Los promedios del año de prueba los exporta 02; si se está leyendo
  # una salida anterior, se reconstruyen desde el diagnóstico.
  faltan <- !all(c("media_pred_test", "media_obs_test") %in% names(base))
  if (faltan && !is.null(diag_nivel)) {
    medias <- diag_nivel %>%
      group_by(grado, area) %>%
      summarise(media_pred_test = mean(predicho, na.rm = TRUE),
                media_obs_test  = mean(observado, na.rm = TRUE),
                .groups = "drop")
    base <- base %>% left_join(medias, by = c("grado", "area"))
  } else if (faltan) {
    base$media_pred_test <- NA_real_
    base$media_obs_test  <- NA_real_
  }

  out <- base %>%
    etiquetar() %>%
    arrange(grado, area) %>%
    transmute(
      Grado = grado, Área = area,
      `Colegios (prueba)` = n_test,
      `SIMCE predicho (promedio)` = round(media_pred_test, 1),
      `SIMCE real (promedio)` = round(media_obs_test, 1),
      `R² (prueba)` = round(r2_test, 2),
      `MAE modelo final` = round(mae, 2)
    )

  # Referencia de los otros 2 escenarios, sobre el universo comparable
  # (colegios de prueba con predicción de modelo anterior y modelo 1).
  # El MAE del modelo final queda como última columna: es la lectura de
  # cierre después de los dos escenarios de referencia.
  if (!is.null(mae_escenarios)) {
    ref <- mae_escenarios %>%
      filter(escenario %in% c("Modelo anterior", "Modelo 1")) %>%
      etiquetar() %>%
      mutate(mae = round(mae, 2)) %>%
      select(grado, area, escenario, mae) %>%
      pivot_wider(names_from = escenario, values_from = mae,
                  names_prefix = "MAE ") %>%
      rename(Grado = grado, Área = area)
    out <- out %>%
      left_join(ref, by = c("Grado", "Área")) %>%
      relocate(`MAE modelo final`, .after = last_col())
  }
  out
})

# --- 2.6 Validación del modelo de dispersión
tablas$t_dispersion <- construir("t_dispersion", list(met_dispersion), {
  base <- met_dispersion
  faltan <- !"sd_media_predicha" %in% names(base)
  if (faltan && !is.null(diag_dispersion)) {
    medias <- diag_dispersion %>%
      group_by(grado, area) %>%
      summarise(sd_media_predicha = mean(sd_predicha, na.rm = TRUE),
                .groups = "drop")
    base <- base %>% left_join(medias, by = c("grado", "area"))
  } else if (faltan) {
    base$sd_media_predicha <- NA_real_
  }

  base %>%
    etiquetar() %>%
    arrange(grado, area) %>%
    transmute(
      Grado = grado, Área = area,
      `Colegios (prueba)` = n_test,
      `sd predicha` = round(sd_media_predicha, 1),
      `sd observada` = round(sd_media_observada, 1),
      `MAE modelo` = round(mae, 2),
      `R² (prueba)` = round(r2_test, 2)
    )
})

# --- 2.7 Validación distribucional de la predicción individual
# Sólo se muestran la versión B (la que describe la presentación), el
# baseline "solo la media" y el oráculo con media y sd observadas.
tablas$t_distribucional <- construir("t_distribucional", list(val_dist), {
  base <- val_dist %>% etiquetar() %>% arrange(grado, area)
  out <- base %>%
    transmute(
      Grado = grado, Área = area,
      `Colegios` = n_colegios,
      `Error de cuantiles` = round(qmae_B, 1),
      `Baseline: solo la media` = round(qmae_solo_media, 1)
    )
  out$`sd observada`  <- round(base$sd_observada, 1)
  out$`sd predicha`   <- round(base$sd_efectiva_B, 1)
  out
})

# --- 2.7b Distribución por Niveles de Aprendizaje
# Traduce la validación distribucional al lenguaje que usan los colegios:
# cuántos estudiantes quedan en cada nivel según el SIMCE real y cuántos
# según el modelo. Los cortes son los oficiales de los Estándares de
# Aprendizaje y se fijan en 04, no se estiman.
tablas$t_niveles_logro <- construir("t_niveles_logro", list(niveles_logro), {
  abrev <- c("matematica" = "Matemática", "lenguaje" = "Lenguaje")
  NIVELES <- c("Insuficiente", "Elemental", "Adecuado")
  
  base <- niveles_logro %>%
    mutate(
      grado  = factor(grado, levels = ORDEN_GRADO,
                      labels = ETIQ_GRADO[ORDEN_GRADO]),
      area   = unname(abrev[area]),
      nivel  = as.character(nivel),
      real   = paste0(round(100 * pct_obs, 1), "%"),
      modelo = paste0(round(100 * pct_pred, 1), "%")
    ) %>%
    arrange(grado, area)
  
  # Se arma columna por columna en vez de con pivot_wider: los nombres
  # quedan controlados y no hay riesgo de que una columna se caiga en
  # silencio por un desajuste de nombres.
  tabla <- base %>% distinct(grado, area)
  for (nv in NIVELES) {
    for (val in c("real", "modelo")) {
      nombre <- paste0(nv, " (", val, ")")
      trozo <- base %>%
        filter(nivel == nv) %>%
        transmute(grado, area, !!nombre := .data[[val]])
      tabla <- left_join(tabla, trozo, by = c("grado", "area"))
    }
  }
  tabla %>% rename(Grado = grado, `Área` = area)
})


# --- 2.8 Error por estrato GSE
tablas$t_estrato <- construir("t_estrato", list(val_estrato), {
  val_estrato %>%
    etiquetar() %>%
    arrange(grado, gse_etiqueta) %>%
    transmute(
      Grado = grado,
      `GSE` = gse_etiqueta,
      `Colegios` = n_colegios,
      `Error de cuantiles` = round(qmae_B, 1),
      `Error abs. de la media` = round(error_abs_media, 1),
      `sd interna observada` = round(sd_obs, 1)
    )
})

# --- 2.9 Chequeos de coherencia (sólo versión B)
tablas$t_coherencia <- construir("t_coherencia", list(coherencia), {
  coherencia %>%
    group_by(grado, area) %>%
    summarise(
      colegios = n(),
      dif_media = mean(abs(dif_media_B), na.rm = TRUE),
      razon_sd  = mean(razon_sd_B, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    etiquetar() %>%
    arrange(grado, area) %>%
    transmute(
      Grado = grado, Área = area,
      `Colegios` = colegios,
      `|media predicha − predicción del colegio|` = round(dif_media, 2),
      `sd de las predicciones / ancho predicho` = round(razon_sd, 3)
    )
})

# --- 2.10 Comparación de los 3 escenarios de predicción (nivel colegio)
# Modelo 1 (regla de conversión Santillana) vs. modelo anterior (sin IRT)
# vs. modelo final (con IRT), sobre el mismo universo comparable de colegios.
tablas$t_comparacion_escenarios <- construir("t_comparacion_escenarios",
  list(mae_escenarios), {
  ORDEN_ESC <- c("Modelo 1", "Modelo anterior", "Modelo final")
  base <- mae_escenarios %>%
    etiquetar() %>%
    mutate(escenario = factor(escenario, levels = ORDEN_ESC)) %>%
    arrange(grado, area, escenario)

  mejora <- base %>%
    select(grado, area, escenario, mae) %>%
    pivot_wider(names_from = escenario, values_from = mae) %>%
    transmute(
      grado, area,
      `mejora_vs_modelo1`    = round(100 * (`Modelo 1` - `Modelo final`) / `Modelo 1`, 1),
      `mejora_vs_anterior`   = round(100 * (`Modelo anterior` - `Modelo final`) / `Modelo anterior`, 1)
    )

  base %>%
    transmute(grado, area, escenario,
              `Colegios` = n_colegios,
              `MAE` = round(mae, 1),
              `R²` = round(r2, 2)) %>%
    left_join(mejora, by = c("grado", "area")) %>%
    rename(Grado = grado, Área = area, Escenario = escenario,
           `Mejora modelo final vs. modelo 1 (%)` = mejora_vs_modelo1,
           `Mejora modelo final vs. modelo anterior (%)` = mejora_vs_anterior)
})

# --- 2.10b Nivel Insuficiente: real vs. los 3 escenarios
# Traduce la comparación de escenarios a la unidad que usa el colegio:
# qué porcentaje de estudiantes queda bajo el corte de Insuficiente
# según cada forma de estimar el puntaje, y cuánto se aleja del real.
#
# ¡OJO! Los cortes están duplicados de 04_validacion_individual.R, que es
# la fuente de verdad (son los oficiales de los Estándares de Aprendizaje
# y NO se estiman de los datos). Si se corrigen allá, hay que corregirlos
# acá también.
CORTES_NIVEL <- tribble(
  ~grado, ~area,        ~corte_elemental,
  "4b",   "lenguaje",   241,
  "4b",   "matematica", 245,
  "2m",   "lenguaje",   250,
  "2m",   "matematica", 252
)

# % bajo el corte de Elemental (= Insuficiente) sobre el mismo universo de
# colegios validados que usa 04, para no mezclar error del modelo con
# diferencias de composición.
pct_insuficiente <- function(datos, columna_puntaje, colegios) {
  datos %>%
    inner_join(colegios, by = c("grado", "area", "rbd_revisado")) %>%
    left_join(CORTES_NIVEL, by = c("grado", "area")) %>%
    filter(!is.na(corte_elemental), !is.na({{ columna_puntaje }})) %>%
    group_by(grado, area) %>%
    summarise(pct = mean({{ columna_puntaje }} < corte_elemental), .groups = "drop")
}

tablas$t_insuficiente <- construir("t_insuficiente",
  list(niveles_logro, val_colegio, pred_ind_anterior, modelo1_alumno, anio_test), {
  colegios_validados <- val_colegio %>% distinct(grado, area, rbd_revisado)

  # Real y modelo final ya vienen calculados en 04, sobre este universo.
  base <- niveles_logro %>%
    filter(as.character(nivel) == "Insuficiente") %>%
    select(grado, area, pct_real = pct_obs, pct_final = pct_pred)

  anterior <- pred_ind_anterior %>%
    filter(agno == anio_test) %>%
    pct_insuficiente(pred_B, colegios_validados) %>%
    rename(pct_anterior = pct)

  modelo1 <- modelo1_alumno %>%
    pct_insuficiente(pred_alu, colegios_validados) %>%
    rename(pct_modelo1 = pct)

  base %>%
    left_join(anterior, by = c("grado", "area")) %>%
    left_join(modelo1, by = c("grado", "area")) %>%
    etiquetar() %>%
    arrange(grado, area) %>%
    transmute(
      Grado = grado, Área = area,
      `Real` = sprintf("%.1f%%", 100 * pct_real),
      `Modelo 1` = sprintf("%.1f%%", 100 * pct_modelo1),
      `Modelo anterior` = sprintf("%.1f%%", 100 * pct_anterior),
      `Modelo final` = sprintf("%.1f%%", 100 * pct_final),
      `Error modelo 1 (pp)` = round(100 * (pct_modelo1 - pct_real), 1),
      `Error modelo anterior (pp)` = round(100 * (pct_anterior - pct_real), 1),
      `Error modelo final (pp)` = round(100 * (pct_final - pct_real), 1)
    )
})

# --- 2.11 Efecto de la calibración IRT sobre la señal del ensayo
# Correlación entre el logro promedio del colegio y su SIMCE promedio,
# con el logro crudo y con el logro ajustado por IRT. Es la lectura más
# directa de si la calibración mejora la señal que entra al modelo:
# mismo colegio, misma variable dependiente, sólo cambia cómo se mide el
# logro. Se calcula sobre todos los años con SIMCE observado.
tablas$t_correlacion_irt <- construir("t_correlacion_irt", list(school_model), {
  school_model %>%
    filter(!is.na(promedio_simce), !is.na(mean_logro_crudo), !is.na(mean_logro_irt)) %>%
    group_by(grado, area) %>%
    summarise(
      n_colegios = n(),
      r_crudo = cor(mean_logro_crudo, promedio_simce),
      r_irt   = cor(mean_logro_irt, promedio_simce),
      .groups = "drop"
    ) %>%
    etiquetar() %>%
    arrange(grado, area) %>%
    transmute(
      Grado = grado, Área = area,
      `Colegios` = n_colegios,
      `r con logro crudo` = round(r_crudo, 3),
      `r con logro IRT` = round(r_irt, 3),
      `Ganancia` = round(r_irt - r_crudo, 3)
    )
})

# 3. GRÁFICOS ----
cat("\nGráficos:\n")
gg <- list()   # versiones ggplot (estáticas)

# --- 3.1 Distribución de los puntajes SIMCE individuales
gg$g_densidad_simce <- construir("g_densidad_simce", list(descriptivos), {
  descriptivos$dens_simce_alu %>%
    etiquetar() %>%
    mutate(text = paste0(grado, " · ", area,
                         "<br>Puntaje: ", round(x),
                         "<br>Densidad: ", signif(y, 2))) %>%
    ggplot(aes(x, y, colour = area, group = area, text = text)) +
    geom_line(linewidth = 0.9) +
    facet_wrap(~ grado, ncol = 2) +
    scale_colour_manual(values = c("Matemática" = COL[["lago"]],
                                   "Lenguaje"   = COL[["marigold"]])) +
    labs(x = "Puntaje SIMCE", y = "Densidad") +
    tema_pres
})

# --- 3.2 SIMCE promedio por año (acompaña a la tabla de la lámina 9)
gg$g_simce_por_anio <- construir("g_simce_por_anio", list(descriptivos), {
  descriptivos$desc_simce_alu %>%
    etiquetar() %>%
    mutate(text = paste0(grado, " · ", area,
                         "<br>Año: ", agno,
                         "<br>SIMCE promedio: ", round(ptje_medio, 1),
                         "<br>Alumnos: ", format(n_alumnos, big.mark = "."))) %>%
    ggplot(aes(agno, ptje_medio, colour = area, group = interaction(grado, area),
               text = text, shape = grado,
               linetype = grado)) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 1.8) +
    # facet_wrap(~ grado) +
    scale_x_continuous(breaks = function(x) seq(floor(min(x)), ceiling(max(x)), by = 1)) +
    scale_y_continuous(limits = c(225, 300)) +
    scale_colour_manual(values = c("Matemática" = COL[["lago"]],
                                   "Lenguaje"   = COL[["marigold"]])) +
    labs(x = NULL, y = "SIMCE promedio") +
    tema_pres
})

# --- 3.3 Relación básica: logro en los ensayos vs. SIMCE del colegio
gg$g_ensayo_vs_simce <- construir("g_ensayo_vs_simce", list(school_model), {
  datos <- school_model %>%
    filter(!is.na(promedio_simce), !is.na(mean_logro)) %>%
    etiquetar()

  # La recta se calcula acá y se dibuja como geom_line, en vez de dejarla
  # en manos de geom_smooth: ggplotly no siempre traslada la capa de
  # suavizado, y así además se puede mostrar la pendiente y el R² en el
  # tooltip de la propia línea.
  tendencia <- datos %>%
    group_by(grado, area) %>%
    group_modify(function(.x, .y) {
      m  <- lm(promedio_simce ~ mean_logro, data = .x)
      xs <- seq(min(.x$mean_logro), max(.x$mean_logro), length.out = 50)
      tibble(
        mean_logro = xs,
        promedio_simce = predict(m, newdata = tibble(mean_logro = xs)),
        pendiente = coef(m)[2],
        r2 = summary(m)$r.squared
      )
    }) %>%
    ungroup() %>%
    mutate(text = paste0("Tendencia ", grado, " · ", area,
                         "<br>+1 punto de logro = ", round(pendiente, 2),
                         " puntos SIMCE",
                         "<br>R²: ", round(r2, 2)))

  ggplot(datos, aes(mean_logro, promedio_simce)) +
    geom_point(aes(text = paste0("RBD ", rbd_revisado,
                                 "<br>Año: ", agno,
                                 "<br>GSE: ", coalesce(gse_etiqueta, "sin dato"),
                                 "<br>Logro ensayo: ", round(mean_logro, 1), "%",
                                 "<br>SIMCE: ", round(promedio_simce, 1))),
               alpha = 0.4, colour = COL[["lago"]], size = 1.3) +
    geom_line(data = tendencia, aes(text = text),
              colour = COL[["coral"]], linewidth = 1.1) +
    facet_grid(area ~ grado) +
    labs(x = "Logro medio en los ensayos (%)", y = "SIMCE promedio del colegio") +
    tema_pres
})

# --- 3.4 Un colegio por dentro: dispersión intra-colegio
# El histograma es la distribución real de un colegio concreto (el de más
# alumnos del grupo); la curva es la distribución nacional del mismo
# grado y área. La distancia entre ambas medias es variación ENTRE
# colegios; el ancho del histograma es variación DENTRO del colegio.
gg$g_colegio_ejemplo <- construir("g_colegio_ejemplo", list(descriptivos), {
  ej   <- descriptivos$colegio_ejemplo
  meta <- ej$meta

  ggplot(ej$puntajes, aes(ptje)) +
    geom_histogram(aes(y = after_stat(density),
                       text = after_stat(paste0("Alumnos del colegio: ", count))),
                   bins = 22, fill = COL[["marigold"]],
                   colour = "white", linewidth = 0.2) +
    geom_line(data = ej$dens_nacional,
              aes(x, y, text = paste0("Distribución nacional<br>Puntaje: ", round(x))),
              inherit.aes = FALSE, colour = COL[["tinta"]], linewidth = 0.9) +
    geom_vline(xintercept = meta$media_colegio, colour = COL[["coral"]],
               linetype = "dashed", linewidth = 0.7) +
    geom_vline(xintercept = meta$media_nacional, colour = COL[["tinta"]],
               linetype = "dotted", linewidth = 0.7) +
    labs(x = paste0("Puntaje SIMCE (", meta$grado, " · ", meta$area, ", ", meta$agno, ")"),
         y = "Densidad") +
    tema_pres
})

# --- 3.5 Ensayos: distribución del logro
gg$g_densidad_logro <- construir("g_densidad_logro", list(descriptivos), {
  descriptivos$dens_logro_ensayo %>%
    etiquetar() %>%
    mutate(text = paste0(grado, " · ", area,
                         "<br>Logro: ", round(x, 1), "%")) %>%
    ggplot(aes(x, y, colour = area, group = area, text = text)) +
    geom_line(linewidth = 0.9) +
    facet_wrap(~ grado, ncol = 2) +
    scale_colour_manual(values = c("Matemática" = COL[["lago"]],
                                   "Lenguaje"   = COL[["marigold"]])) +
    labs(x = "Logro promedio del estudiante en los ensayos (%)",
         y = "Densidad") +
    tema_pres
})

# --- 3.6 Ensayos: logro medio según número de evaluación
gg$g_logro_por_evaluacion <- construir("g_logro_por_evaluacion", list(descriptivos), {
  descriptivos$logro_por_evaluacion %>%
    etiquetar() %>%
    mutate(agno_f = factor(agno),
           text = paste0("Año ", agno, " · ", grado, " · ", area,
                         "<br>Ensayo n° ", n_evaluacion,
                         "<br>Logro medio: ", round(logro_medio, 1), "%",
                         "<br>Observaciones: ", n_obs)) %>%
    ggplot(aes(n_evaluacion, logro_medio, colour = agno_f,
               group = agno_f, text = text)) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 1.4) +
    facet_grid(area ~ grado) +
    scale_x_continuous(breaks = 1:6) +
    labs(x = "Número de evaluación", y = "Logro medio (%)") +
    tema_pres
})

# --- 3.7 Evolución conjunta de ambas fuentes (estandarizadas)
gg$g_evolucion_estandarizada <- construir("g_evolucion_estandarizada", list(descriptivos), {
  descriptivos$evolucion_fuentes %>%
    etiquetar() %>%
    pivot_longer(c(z_simce, z_logro), names_to = "serie", values_to = "z") %>%
    mutate(
      serie = recode(serie, z_simce = "SIMCE", z_logro = "Ensayos"),
      valor  = ifelse(serie == "SIMCE", round(simce, 1), round(logro, 1)),
      unidad = ifelse(serie == "SIMCE", " puntos", "% de logro"),
      colegios = ifelse(serie == "SIMCE", n_colegios_simce, n_colegios_ensayo),
      text  = paste0(serie, " · ", grado, " · ", area,
                     "<br>Año: ", agno,
                     "<br>Valor: ", valor, unidad,
                     "<br>Estandarizado: ", round(z, 2),
                     "<br>Colegios: ", colegios)
    ) %>%
    ggplot(aes(agno, z, colour = serie, group = serie, text = text)) +
    geom_hline(yintercept = 0, colour = "#C9D3D2", linewidth = 0.4) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 1.6) +
    facet_grid(area ~ grado) +
    # los años son enteros: sin esto el eje muestra 2022.5, 2023.0, ...
    scale_x_continuous(breaks = function(x) seq(floor(min(x)), ceiling(max(x)), by = 1)) +
    scale_y_continuous(limits = c(-2, 2)) +
    scale_colour_manual(values = c("SIMCE" = COL[["tinta"]],
                                   "Ensayos" = COL[["marigold"]])) +
    labs(x = NULL, y = "Desviaciones respecto del promedio del grupo") +
    tema_pres
})

# --- 3.8 SIMCE promedio por GSE
gg$g_simce_por_gse <- construir("g_simce_por_gse", list(descriptivos), {
  descriptivos$simce_por_gse %>%
    etiquetar() %>%
    mutate(text = paste0("GSE ", gse_etiqueta, " · ", grado, " · ", area,
                         "<br>SIMCE medio: ", round(simce_medio, 1),
                         "<br>Colegios: ", n_colegios)) %>%
    ggplot(aes(gse_etiqueta, simce_medio, colour = area, group = area, text = text)) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 2.2) +
    facet_wrap(~ grado) +
    scale_colour_manual(values = c("Matemática" = COL[["lago"]],
                                   "Lenguaje"   = COL[["marigold"]])) +
    labs(x = "Grupo socioeconómico", y = "SIMCE promedio del colegio") +
    tema_pres
})

# --- 3.9 Composición de la base con ensayo vs. universo nacional
gg$g_composicion_gse <- construir("g_composicion_gse", list(descriptivos), {
  descriptivos$comp_gse %>%
    etiquetar() %>%
    mutate(
      fuente = factor(fuente,
                      levels = c("nacional", "santillana"),
                      labels = c("Universo nacional", "Colegios con ensayo")),
      text = paste0(fuente, "<br>GSE ", gse_etiqueta,
                    "<br>", round(100 * prop, 1), "% de los colegios")
    ) %>%
    ggplot(aes(gse_etiqueta, prop, fill = fuente, text = text)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7) +
    facet_wrap(~ grado) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_fill_manual(values = c("Universo nacional"   = COL[["bruma"]],
                                 "Colegios con ensayo" = COL[["marigold"]])) +
    labs(x = "Grupo socioeconómico", y = "% de colegios") +
    tema_pres
})

# --- 3.10 Plantilla de forma de la distribución interna
# Compara la forma empírica (por tercil de nivel) con la normal, que es
# lo que se asumiría sin estos datos.
gg$g_forma_z <- construir("g_forma_z", list(forma_z), {
  f <- forma_z %>%
    filter(agno == max(agno), tercil != "todos") %>%
    mutate(tercil = factor(tercil, levels = c("bajo", "medio", "alto"),
                           labels = c("Nivel bajo", "Nivel medio", "Nivel alto")))
  normal <- tibble(p = unique(f$p)) %>% mutate(z = qnorm(p), tercil = "Normal")

  bind_rows(
    f %>% select(p, z, tercil) %>% mutate(tipo = "empírica"),
    normal %>% mutate(tipo = "normal")
  ) %>%
    mutate(text = paste0(tercil, "<br>Percentil: ", round(100 * p),
                         "<br>z: ", round(z, 2))) %>%
    ggplot(aes(p, z, colour = tercil, linetype = tipo, group = tercil, text = text)) +
    geom_line(linewidth = 0.9) +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_colour_manual(values = c("Nivel bajo"  = COL[["coral"]],
                                   "Nivel medio" = COL[["lago"]],
                                   "Nivel alto"  = COL[["marigold"]],
                                   "Normal"      = COL[["bruma"]])) +
    scale_linetype_manual(values = c("empírica" = "solid", "normal" = "dashed"),
                          guide = "none") +
    labs(x = "Percentil dentro del colegio",
         y = "Puntaje estandarizado (z)") +
    tema_pres
})

# --- 3.11 Nivel: observado vs. predicho en el año de prueba
gg$g_obs_pred_nivel <- construir("g_obs_pred_nivel", list(diag_nivel), {
  d <- diag_nivel %>% etiquetar()
  # Compatibilidad con salidas de 02 anteriores, que no traían mean_logro.
  if (!"mean_logro" %in% names(d)) d$mean_logro <- NA_real_
  # v2 renombró n_anios_hist -> n_anios_nivel_hist.
  if (!"n_anios_hist" %in% names(d) && "n_anios_nivel_hist" %in% names(d)) {
    d$n_anios_hist <- d$n_anios_nivel_hist
  }

  d %>%
    mutate(text = paste0("RBD ", rbd_revisado,
                         "<br>GSE: ", coalesce(gse_etiqueta, "sin dato"),
                         "<br>Años de historia: ", n_anios_hist,
                         "<br>Logro medio en ensayos: ",
                         ifelse(is.na(mean_logro), "sin dato",
                                paste0(round(mean_logro, 1), "%")),
                         "<br>Observado: ", round(observado, 1),
                         "<br>Predicho: ", round(predicho, 1),
                         "<br>Error: ", round(predicho - observado, 1))) %>%
    ggplot(aes(observado, predicho, text = text)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                colour = COL[["coral"]]) +
    geom_point(alpha = 0.5, colour = COL[["lago"]], size = 1.3) +
    facet_grid(area ~ grado) +
    labs(x = "SIMCE observado", y = "SIMCE predicho") +
    tema_pres
})

# --- 3.12 Dispersión: sd interna observada vs. predicha
gg$g_obs_pred_sd <- construir("g_obs_pred_sd", list(diag_dispersion), {
  diag_dispersion %>%
    etiquetar() %>%
    mutate(text = paste0("RBD ", rbd_revisado,
                         "<br>Alumnos: ", n_alu_simce,
                         "<br>sd observada: ", round(sd_observada, 1),
                         "<br>sd predicha: ", round(sd_predicha, 1),
                         "<br>sd histórica: ", round(sd_historica, 1))) %>%
    ggplot(aes(sd_observada, sd_predicha, text = text)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                colour = COL[["coral"]]) +
    geom_point(alpha = 0.5, colour = COL[["marigold"]], size = 1.3) +
    facet_grid(area ~ grado) +
    labs(x = "sd interna observada", y = "sd interna predicha") +
    tema_pres
})

# --- 3.13 Distribución individual: predicha vs. observada
gg$g_densidad_validacion <- construir("g_densidad_validacion", list(dens_validacion), {
  dens_validacion %>%
    filter(fuente %in% c("Observado", "Predicción (versión B)")) %>%
    etiquetar() %>%
    mutate(text = paste0(fuente, "<br>Puntaje: ", round(x))) %>%
    ggplot(aes(x, y, colour = fuente, group = fuente, text = text)) +
    geom_line(linewidth = 0.9) +
    facet_grid(area ~ grado) +
    scale_colour_manual(values = c("Observado" = COL[["tinta"]],
                                   "Predicción (versión B)" = COL[["marigold"]])) +
    labs(x = "Puntaje SIMCE individual", y = "Densidad") +
    tema_pres
})

# --- 3.14 Error de cuantiles por colegio: distribución y estratos
gg$g_error_por_colegio <- construir("g_error_por_colegio", list(val_colegio), {
  val_colegio %>%
    etiquetar() %>%
    mutate(text = paste0("RBD ", rbd_revisado,
                         "<br>Error de cuantiles: ", round(qmae_B, 1),
                         "<br>Error de la media: ", round(error_media, 1),
                         "<br>Alumnos: ", n_obs)) %>%
    ggplot(aes(qmae_B, text = text)) +
    geom_histogram(bins = 30, fill = COL[["lago"]], colour = "white",
                   linewidth = 0.2) +
    facet_grid(area ~ grado, scales = "free_y") +
    labs(x = "Error de cuantiles del colegio (puntos SIMCE)",
         y = "N° de colegios") +
    tema_pres
})

gg$g_qmae_estrato <- construir("g_qmae_estrato", list(val_estrato), {
  val_estrato %>%
    etiquetar() %>%
    mutate(text = paste0("GSE ", gse_etiqueta,
                         "<br>Colegios: ", n_colegios,
                         "<br>Error de cuantiles: ", round(qmae_B, 1))) %>%
    ggplot(aes(gse_etiqueta, qmae_B, fill = grado, text = text)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7) +
    scale_fill_manual(values = c(COL[["lago"]], COL[["marigold"]])) +
    labs(x = "Grupo socioeconómico",
         y = "Error de cuantiles (puntos SIMCE)") +
    tema_pres
})

# --- 3.15 Comparación de escenarios: MAE de nivel-colegio por escenario
gg$g_comparacion_escenarios <- construir("g_comparacion_escenarios",
  list(mae_escenarios), {
  ORDEN_ESC <- c("Modelo 1", "Modelo anterior", "Modelo final")
  mae_escenarios %>%
    etiquetar() %>%
    mutate(escenario = factor(escenario, levels = ORDEN_ESC),
           text = paste0(escenario, " · ", grado, " · ", area,
                        "<br>MAE: ", round(mae, 1),
                        "<br>Colegios: ", n_colegios)) %>%
    ggplot(aes(escenario, mae, fill = escenario, text = text)) +
    geom_col(width = 0.7) +
    facet_grid(area ~ grado) +
    scale_fill_manual(values = c("Modelo 1" = COL[["bruma"]],
                                 "Modelo anterior" = COL[["marigold"]],
                                 "Modelo final" = COL[["lago"]])) +
    labs(x = NULL, y = "MAE de nivel-colegio (puntos SIMCE)") +
    theme(axis.text.x = element_blank()) +
    tema_pres
})

# 4. Versión interactiva de cada gráfico -------------------------
graficos <- imap(compact(gg), function(p, nombre) {
  tryCatch(
    interactivo(p, leyenda = nombre %in% c("g_densidad_simce", "g_composicion_gse",
                                           "g_forma_z", "g_densidad_validacion",
                                           "g_qmae_estrato", "g_densidad_logro",
                                           "g_logro_por_evaluacion",
                                           "g_evolucion_estandarizada",
                                           "g_simce_por_gse", "g_simce_por_anio",
                                           "g_comparacion_escenarios"),
                # el colegio de ejemplo va junto a una tabla: más bajo
                # los que comparten lámina con una tabla van más bajos
                alto = if (nombre %in% c("g_colegio_ejemplo", "g_simce_por_anio")) {
                  330
                } else ALTO_GRAFICO),
    error = function(e) {
      cat("  - ", nombre, ": no se pudo convertir a plotly (", conditionMessage(e),
          "); queda sólo la versión estática\n", sep = "")
      NULL
    }
  )
})

# ---- 5. Guardar ------------------------------------------------------
figuras <- list(
  tablas             = compact(tablas),
  graficos           = compact(graficos),
  graficos_estaticos = compact(gg),
  meta = list(
    anio_test    = anio_test,
    generado_el  = Sys.time(),
    dir_salidas  = dir_salidas
  )
)

saveRDS(figuras, dir_salidas %>% file.path("figuras_presentacion.rds"))

cat("\nListo. figuras_presentacion.rds guardado en", dir_salidas, "\n")
cat("  tablas :", paste(names(figuras$tablas), collapse = ", "), "\n")
cat("  gráficos:", paste(names(figuras$graficos), collapse = ", "), "\n")
cat("\nEn la presentación, apuntar `ruta_figuras` a ese archivo",
    "(o copiarlo junto al .qmd).\n")
