# ==========================================================================
# Limpieza de errores + marcado de outliers (IQR / Isolation Forest)
# Datasets: resultados_simce_rbd.csv, ensayos_santillana.csv
#
# Qué hace este script:
#   1. Corrige errores conocidos y deterministicos (valores imposibles,
#      duplicados, inconsistencias de llave, texto sucio) y deja un LOG
#      de cada corrección aplicada (no corrige nada en silencio).
#   2. Sobre los datos ya corregidos, agrega DOS columnas de marcado de
#      anomalías por dataset:
#         - outlier_iqr        (método IQR, por grupo)
#         - outlier_isoforest  (Isolation Forest, por grupo)
#      Estas NO se eliminan del dataset: solo se marcan, para que la
#      decisión de excluirlas o no quede en manos de quien modela.
#   3. Exporta: datos corregidos + marcados, y los logs de corrección.
# ==========================================================================


# ---- 0. Configuración --------------------------------------------
# Configurar rutas de archivos: ----
rutas <- config::get(file = "config.yml")
ruta_data_in <- rutas$ruta_data_in
ruta_data_intermedia <- rutas$ruta_data_intermedia

ruta_resultados  <- file.path(ruta_data_intermedia, 'simce', 'consolidado_datos_simce_rbd.parquet')
ruta_ensayos     <- file.path(ruta_data_intermedia, 'ensayo_simce', 'consolidado_ensayo_simce.parquet')
carpeta_salida   <- ruta_data_intermedia

umbral_isoforest <- 0.75     # score de anomalía (0-1) sobre el cual se marca
min_obs_grupo    <- 30      # mínimo de observaciones para ajustar isolation forest por grupo
factor_iqr       <- 1.75     # multiplicador estándar de Tukey para el método IQR

library(tidyverse)
library(isotree)
library(janitor)
library(arrow)

## 1. Funciones genéricas de marcado de outliers ---------------------------

#' Marca outliers univariados con el método IQR (Tukey), calculado por grupo.
#'
#' @param df data.frame de entrada
#' @param value_col nombre (string) de la columna numérica a evaluar
#' @param group_vars vector de nombres (string) de columnas de agrupación;
#'   NULL para calcular el IQR sobre todo el dataset sin agrupar
#' @param factor_iqr multiplicador del IQR (1.5 = regla estándar de Tukey)
#' @param col_name nombre de la columna lógica de salida
#' @return el mismo df con una columna lógica adicional `col_name`
flag_outliers_iqr <- function(df, value_col, group_vars = NULL,
                               factor_iqr = 1.5, col_name = "outlier_iqr") {

  agrupar <- function(data) {
    data %>%
      mutate(
        .q1  = quantile(.data[[value_col]], .25, na.rm = TRUE),
        .q3  = quantile(.data[[value_col]], .75, na.rm = TRUE),
        .iqr = .q3 - .q1,
        .lim_inf = .q1 - factor_iqr * .iqr,
        .lim_sup = .q3 + factor_iqr * .iqr,
        !!col_name := !is.na(.data[[value_col]]) &
          (.data[[value_col]] < .lim_inf | .data[[value_col]] > .lim_sup)
      ) %>%
      select(-.q1, -.q3, -.iqr, -.lim_inf, -.lim_sup)
  }

  if (is.null(group_vars)) {
    agrupar(df)
  } else {
    df %>%
      group_by(across(all_of(group_vars))) %>%
      group_modify(~ agrupar(.x)) %>%
      ungroup()
  }
}


#' Marca outliers con Isolation Forest, ajustado por grupo (opcional).
#'
#' @param df data.frame de entrada
#' @param feature_cols vector de nombres (string) de columnas numéricas a usar
#'   como variables del modelo (puede ser una sola variable)
#' @param group_vars vector de nombres (string) de columnas de agrupación;
#'   NULL para ajustar un solo modelo sobre todo el dataset
#' @param umbral score de anomalía (0-1) sobre el cual se marca como outlier
#' @param min_obs_grupo mínimo de filas completas (sin NA en feature_cols)
#'   necesarias dentro de un grupo para poder ajustar el modelo; grupos más
#'   chicos quedan sin marcar (NA) en vez de forzar un modelo inestable
#' @param col_name nombre de la columna lógica de salida
#' @param col_score nombre de la columna numérica de score que se agrega también
#' @return el mismo df con dos columnas adicionales: el score y el flag lógico
flag_outliers_isoforest <- function(df, feature_cols, group_vars = NULL,
                                     umbral = 0.6, min_obs_grupo = 30,
                                     ntrees = 100, sample_size = 256, seed = 123,
                                     col_name = "outlier_isoforest",
                                     col_score = "score_isoforest") {

  set.seed(seed)

  ajustar_grupo <- function(data) {
    completos <- complete.cases(data[feature_cols])
    scores <- rep(NA_real_, nrow(data))

    if (sum(completos) >= min_obs_grupo) {
      mat <- as.matrix(data[completos, feature_cols, drop = FALSE])
      n_muestra <- min(sample_size, nrow(mat))
      modelo <- isolation.forest(
        mat, ntrees = ntrees, sample_size = n_muestra,
        ndim = length(feature_cols), seed = seed
      )
      scores[completos] <- predict(modelo, mat, type = "score")
    }
    data[[col_score]] <- scores
    data
  }

  if (is.null(group_vars)) {
    df <- ajustar_grupo(df)
  } else {
    df <- df %>%
      group_by(across(all_of(group_vars))) %>%
      group_modify(~ ajustar_grupo(.x)) %>%
      ungroup()
  }

  df %>%
    mutate(!!col_name := !is.na(.data[[col_score]]) & .data[[col_score]] > umbral)
}


## 2. Funciones de corrección de errores -----------------------------------
## Cada función corrige errores DETERMINÍSTICOS (valores imposibles,
## duplicados, inconsistencias de llave) y devuelve tanto los datos
## corregidos como un log detallado de qué se corrigió y por qué.

#' Corrige errores conocidos en resultados_simce_rbd
corregir_resultados_simce <- function(df) {
  logs <- list()
  n_inicial <- nrow(df)

  # 2.1 promedio_simce == 0 no es un puntaje válido de la escala SIMCE.
  #     Se recodifica como NA (dato faltante), en vez de dejarlo como si
  #     fuera un puntaje real.
  idx_cero <- which(df$promedio_simce == 0)
  if (length(idx_cero) > 0) {
    logs$ceros <- df[idx_cero, ] %>%
      mutate(correccion = "promedio_simce == 0 -> recodificado a NA")
    df$promedio_simce[idx_cero] <- NA_real_
  }

  # 2.2 Duplicados en la llave (agno, grado, rbd, area).
  #     - Si todas las filas duplicadas tienen el mismo promedio_simce:
  #       se colapsan a una sola fila (duplicado exacto, sin conflicto).
  #     - Si tienen valores distintos (conflicto real): se promedian y
  #       se deja constancia en el log para revisión manual.
  dup_info <- df %>% count(agno, grado, rbd, area, name = "n_rep") %>% filter(n_rep > 1)

  if (nrow(dup_info) > 0) {
    logs$duplicados <- df %>%
      semi_join(dup_info, by = c("agno", "grado", "rbd", "area")) %>%
      mutate(correccion = "llave (agno,grado,rbd,area) duplicada -> colapsada")

    df <- df %>%
      group_by(agno, grado, rbd, area) %>%
      summarise(
        promedio_simce = if (n_distinct(promedio_simce, na.rm = TRUE) <= 1) {
          suppressWarnings(first(na.omit(promedio_simce)))
        } else {
          mean(promedio_simce, na.rm = TRUE)
        },
        .groups = "drop"
      )
  }

  list(
    datos = df,
    log_correcciones = if (length(logs) > 0) bind_rows(logs) else
      df[0, ] %>% mutate(correccion = character()),
    resumen = tibble(
      correccion = c("promedio_simce == 0 -> NA", "duplicados de llave colapsados"),
      n_filas_afectadas = c(length(idx_cero), nrow(dup_info)),
      n_filas_inicial = n_inicial
    )
  )
}


#' Corrige errores conocidos en ensayos_santillana
corregir_ensayos_santillana <- function(df) {
  logs <- list()
  n_inicial <- nrow(df)

  # 2.1 porcentaje_logro fuera de [0, 100] es matemáticamente imposible.
  #     Se recodifica como NA en vez de intentar "adivinar" el valor real
  #     (podría venir de un mal escalado, no hay forma confiable de arreglarlo).
  idx_rango <- which(df$porcentaje_logro < 0 | df$porcentaje_logro > 100)
  if (length(idx_rango) > 0) {
    logs$rango_logro <- df[idx_rango, ] %>%
      mutate(correccion = "porcentaje_logro fuera de [0,100] -> recodificado a NA")
    df$porcentaje_logro[idx_rango] <- NA_real_
  }

  # 2.2 rbd vs rbd_revisado: se usa rbd_revisado como fuente de verdad
  #     cuando está disponible, ya que representa la corrección posterior
  #     del RBD original.
  idx_mismatch <- which(!is.na(df$rbd) & !is.na(df$rbd_revisado) & df$rbd != df$rbd_revisado)
  if (length(idx_mismatch) > 0) {
    logs$rbd_corregido <- df[idx_mismatch, ] %>%
      mutate(correccion = "rbd reemplazado por rbd_revisado (no coincidían)")
  }
  df <- df %>%
    mutate(
      rbd_original = rbd,
      rbd = if_else(!is.na(rbd_revisado), rbd_revisado, rbd)
    )

  # 2.3 Duplicados exactos en (id_usuario_curso, id_evaluacion): un mismo
  #     intento de evaluación no debería tener más de un registro. Se
  #     conserva el primero y se descartan los siguientes.
  dup_info <- df %>%
    count(id_usuario_curso, id_evaluacion, name = "n_rep") %>%
    filter(n_rep > 1)

  if (nrow(dup_info) > 0) {
    logs$duplicados <- df %>%
      semi_join(dup_info, by = c("id_usuario_curso", "id_evaluacion")) %>%
      mutate(correccion = "duplicado (id_usuario_curso, id_evaluacion) -> se conserva 1er registro")
    df <- df %>% distinct(id_usuario_curso, id_evaluacion, .keep_all = TRUE)
  }

  # 2.4 Campo `curso` con texto adicional del colegio pegado
  #     (ej. "4° Básico A - COLEGIO SAN JOSÉ DE LAMPA"). Se deja solo la
  #     parte del curso propiamente tal.
  idx_curso <- which(str_detect(df$curso, " - "))
  if (length(idx_curso) > 0) {
    logs$curso_sucio <- df[idx_curso, ] %>%
      mutate(correccion = "curso: texto de colegio removido")
  }
  df <- df %>% mutate(curso = str_trim(str_remove(curso, " - .*$")))

  list(
    datos = df,
    log_correcciones = if (length(logs) > 0) bind_rows(logs) else
      df[0, ] %>% mutate(correccion = character()),
    resumen = tibble(
      correccion = c(
        "porcentaje_logro fuera de [0,100] -> NA",
        "rbd reemplazado por rbd_revisado",
        "duplicados (id_usuario_curso, id_evaluacion) eliminados",
        "curso con texto de colegio limpiado"
      ),
      n_filas_afectadas = c(length(idx_rango), length(idx_mismatch), nrow(dup_info), length(idx_curso)),
      n_filas_inicial = n_inicial
    )
  )
}


## 3. Ejecutar limpieza -----------------------------------------------------

resultados_raw <- read_parquet(ruta_resultados, show_col_types = FALSE)
ensayos_raw    <- read_parquet(ruta_ensayos, show_col_types = FALSE)

res_limpieza <- corregir_resultados_simce(resultados_raw)
ens_limpieza <- corregir_ensayos_santillana(ensayos_raw)

resultados <- res_limpieza$datos
ensayos    <- ens_limpieza$datos

cat("=== Resumen de correcciones: resultados_simce_rbd ===\n")
print(res_limpieza$resumen)
cat("Filas: ", nrow(resultados_raw), " -> ", nrow(resultados), "\n\n", sep = "")

cat("=== Resumen de correcciones: ensayos_santillana ===\n")
print(ens_limpieza$resumen)
cat("Filas: ", nrow(ensayos_raw), " -> ", nrow(ensayos), "\n\n", sep = "")


## 4. Marcar outliers: IQR + Isolation Forest -------------------------------
## Ambos métodos se aplican sobre la MISMA variable de interés de cada
## dataset, calculados por grupo (agno, grado/nivel, area), y quedan
## como dos columnas lógicas independientes: outlier_iqr y outlier_isoforest.
## No se eliminan filas; solo se marcan para revisión/decisión posterior.

resultados <- resultados %>%
  flag_outliers_iqr(
    value_col   = "promedio_simce",
    group_vars  = c("agno", "grado", "area"),
    factor_iqr  = factor_iqr,
    col_name    = "outlier_iqr"
  ) %>%
  flag_outliers_isoforest(
    feature_cols  = "promedio_simce",
    group_vars    = c("agno", "grado", "area"),
    umbral        = umbral_isoforest,
    min_obs_grupo = min_obs_grupo,
    col_name      = "outlier_isoforest",
    col_score     = "score_isoforest"
  )

ensayos <- ensayos %>%
  flag_outliers_iqr(
    value_col   = "porcentaje_logro",
    group_vars  = c("agno", "nivel", "area"),
    factor_iqr  = factor_iqr,
    col_name    = "outlier_iqr"
  ) %>%
  flag_outliers_isoforest(
    feature_cols  = "porcentaje_logro",
    group_vars    = c("agno", "nivel", "area"),
    umbral        = umbral_isoforest,
    min_obs_grupo = min_obs_grupo,
    col_name      = "outlier_isoforest",
    col_score     = "score_isoforest"
  )

cat("=== Outliers marcados: resultados_simce_rbd ===\n")
resultados %>%
  summarise(
    n_total = n(),
    n_outlier_iqr = sum(outlier_iqr, na.rm = TRUE),
    n_outlier_isoforest = sum(outlier_isoforest, na.rm = TRUE),
    n_ambos = sum(outlier_iqr & outlier_isoforest, na.rm = TRUE)
  ) %>%
  print()

cat("\n=== Outliers marcados: ensayos_santillana ===\n")
ensayos %>%
  summarise(
    n_total = n(),
    n_outlier_iqr = sum(outlier_iqr, na.rm = TRUE),
    n_outlier_isoforest = sum(outlier_isoforest, na.rm = TRUE),
    n_ambos = sum(outlier_iqr & outlier_isoforest, na.rm = TRUE)
  ) %>%
  print()


## 5. Exportar resultados ----------------------------------------------------

write_parquet(resultados, file.path(carpeta_salida, 'simce', 'resultados_simce_rbd_corregido.parquet'))
write_parquet(ensayos,    file.path(carpeta_salida, 'ensayo_simce', "ensayos_santillana_corregido.parquet"))

write_csv(res_limpieza$log_correcciones, file.path(carpeta_salida, 'simce', "log_correcciones_resultados.csv"))
write_csv(ens_limpieza$log_correcciones, file.path(carpeta_salida, 'ensayo_simce', "log_correcciones_ensayos.csv"))

cat("\nArchivos exportados en: ", normalizePath(carpeta_salida), "\n", sep = "")
cat("  - resultados_simce_rbd_corregido.csv   (datos corregidos + outlier_iqr + outlier_isoforest)\n")
cat("  - ensayos_santillana_corregido.csv     (datos corregidos + outlier_iqr + outlier_isoforest)\n")
cat("  - log_correcciones_resultados.csv      (detalle de cada corrección aplicada)\n")
cat("  - log_correcciones_ensayos.csv         (detalle de cada corrección aplicada)\n")
