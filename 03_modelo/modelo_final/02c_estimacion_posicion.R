# =============================================================
# 02c_estimacion_posicion.R
# -------------------------------------------------------------
# Tercer componente del modelo: la POSICIÓN del estudiante dentro de su
# colegio. Los otros dos son el NIVEL (02a) y la DISPERSIÓN (02b), y los
# tres se ensamblan en 03:
#
#     puntaje = NIVEL(colegio) + DISPERSIÓN(colegio) x POSICIÓN(estudiante)
#
# La posición tiene dos piezas, cada una estimada donde hay evidencia:
#
#   EL ORDENAMIENTO   quién va antes que quién dentro del colegio. Sale de
#                     los ensayos de este año, encogido por la confiabilidad
#                     con que se midió a cada estudiante (sección 2).
#   LA FORMA          a qué puntaje corresponde cada percentil. Sale del
#                     SIMCE de años anteriores y la calcula 01a; acá sólo
#                     se recoge para que 03 lea la posición completa en un
#                     solo lugar.
#
# -------------------------------------------------------------
# POR QUÉ ESTE COMPONENTE NO ENTRENA COEFICIENTES
# -------------------------------------------------------------
# 02a y 02b ajustan modelos sobre el SIMCE observado y guardan coeficientes
# que se reutilizan en rondas siguientes. Acá no hay nada equivalente: el
# ordenamiento depende de qué alumnos rindieron los ensayos de ESTE año, así
# que se recalcula en cada ronda. La única pieza "entrenada" de la posición
# es la plantilla de forma, y viene de 01a.
#
# Esa asimetría es real y conviene tenerla presente: es la razón de que la
# posición sea el componente que menos se puede validar. No existe vínculo
# alumno-a-alumno entre el ensayo y el SIMCE, así que su supuesto de fondo
# —que el ensayo ordena parecido a como ordenará el SIMCE— no es
# comprobable directamente. Ver el registro de pruebas, sección 5.
#
# CORRE EN CADA RONDA, después de 01b y antes de 03.
#
# -------------------------------------------------------------
# SALIDA: un único archivo `salida_02c_posicion.rds` con una lista de:
#
#   $posicion         1 fila por estudiante: `pct_ensayo` (el percentil
#                     dentro del colegio, lo único que consume 03),
#                     `z_ensayo`, `indice_ensayo` y `rel_indice`.
#   $forma_z          plantilla de forma, recogida de 01a
#   $cortes_tercil    cortes de nivel para elegir la plantilla, de 01a
#   $confiabilidades  confiabilidad del SIMCE y del índice (diagnóstico)
# =============================================================

library(tidyverse)

# ---- 0. Configuración --------------------------------------------
usuario <- Sys.info()[["user"]]
rutas <- config::get(config = usuario, file = "config.yml")
ruta_outputs <- rutas$ruta_outputs

dir_salidas <- ruta_outputs %>% file.path('modelo_final')

# Mínimo de estudiantes para estimar la varianza verdadera entre alumnos
# DENTRO de un colegio. Bajo este umbral se usa la del grupo completo.
MIN_EST_TAU <- 5

# Piso para tau^2 como fracción de la varianza observada: evita que un
# colegio donde el ruido explica toda la varianza quede con tau^2 = 0 y,
# por lo tanto, con todos sus estudiantes encogidos exactamente al
# promedio (lo que borraría el ordenamiento interno).
PISO_TAU2   <- 0.05

# ---- 1. Insumos ------------------------------------------------------
leer_salida <- function(archivo, de) {
  ruta <- dir_salidas %>% file.path(archivo)
  if (!file.exists(ruta)) {
    stop("Falta ", archivo, " en ", dir_salidas, ".
",
         "Lo genera ", de, ": hay que correrlo antes.")
  }
  readRDS(ruta)
}

simce  <- leer_salida("salida_01a_simce.rds",  "01a_insumos_simce.R")
ensayo <- leer_salida("salida_01b_ensayo.rds", "01b_insumos_ensayo.R")

conf_simce <- simce$conf_simce   # la del índice se calcula más abajo

# Si `ind_features` viene de una corrida anterior a la separación de este
# componente, todavía trae las columnas de posición y el join de más abajo
# las duplicaría con sufijos .x/.y. Se descartan: acá se recalculan.
ind_features <- ensayo$ind_features %>%
  select(-any_of(c("indice_ensayo", "rel_indice", "z_bruto",
                   "pct_ensayo", "z_ensayo", "n_est_colegio")))


# ---- 2. Ordenamiento dentro del colegio ------------------------------
# Es la base del ordenamiento de los estudiantes dentro de su colegio.
#
# EL PROBLEMA. No todos los estudiantes están medidos con la misma
# precisión: el 15-22% rinde UN solo ensayo. Tratar su theta como si fuera
# tan bueno como el de quien rindió seis mete ruido en el ranking interno
# del colegio, que es justamente lo que la predicción individual usa.
#
# CÓMO. Para el estudiante i del colegio c, su theta centrado en el
# promedio de su colegio se descompone en posición verdadera más error:
#
#     x_i = t_i + e_i,   Var(t) = tau^2,   Var(e_i) = se_theta_i^2
#
# donde tau^2 es la varianza VERDADERA entre estudiantes del mismo colegio
# —la observada menos el ruido de medición— y `se_theta` lo entrega la
# calibración por estudiante. De ahí su confiabilidad individual y el
# estimador encogido:
#
#     rel_i  = tau^2 / (tau^2 + se_theta_i^2)
#     indice = rel_i * (x_i - media_colegio) / tau
#
# que es el BLUP univariado: cada alumno se acerca al promedio de su
# colegio en proporción a cuánto de su medición es ruido. Un estudiante
# con seis ensayos casi no se mueve; uno con un solo ensayo sí.
#
# ESCALA. El índice queda en unidades de desviación VERDADERA dentro del
# colegio, así que su sd es sqrt(rel) < 1, no 1. Es deliberado: así el
# encogimiento de un estudiante mal medido sobrevive hasta la predicción
# final en vez de perderse al re-estandarizar.
#
# ALCANCE REAL, para no atribuirle más de lo que hace: el ordenamiento que
# produce es casi idéntico al de rankear por theta a secas (Spearman
# dentro del colegio: mediana 0.995-0.998, p10 0.966-0.990). Como la
# predicción individual consume sólo el RANGO (`pct_ensayo`), el
# encogimiento importa para interpretar el índice y para el diagnóstico de
# confiabilidad, no para mover la predicción.

cat("\nConstruyendo índice individual encogido (secc. 4)...\n")

construir_indice <- function(est_entrada) {

  # --- tau^2 verdadera entre estudiantes del mismo colegio --------------
  # Primero una versión de grupo (respaldo para colegios chicos), después
  # la propia de cada colegio. La varianza OBSERVADA entre estudiantes
  # mezcla diferencias reales con ruido de medición; se descuenta el ruido.
  tau2_grupo <- est_entrada %>%
    group_by(agno, grado, area, rbd_revisado) %>%
    filter(n() >= MIN_EST_TAU) %>%
    mutate(x_c = x_est - mean(x_est)) %>%
    group_by(agno, grado, area) %>%
    summarise(
      var_obs_g = sum(x_c^2) / pmax(n() - n_distinct(rbd_revisado), 1),
      var_err_g = mean(var_err),
      tau2_g    = pmax(var_obs_g - var_err_g, PISO_TAU2 * var_obs_g),
      .groups = "drop"
    ) %>%
    select(agno, grado, area, tau2_g)

  est_base <- est_entrada %>%
    left_join(tau2_grupo, by = c("agno", "grado", "area")) %>%
    # Último respaldo de tau^2, calculado DENTRO de cada grado x área: si un
    # grupo entero quedara sin `tau2_g` (ningún colegio con MIN_EST_TAU
    # estudiantes), se usa la varianza entre todos sus estudiantes. Es peor
    # estimación —mezcla diferencias entre colegios con diferencias dentro—
    # pero es finita y del grupo correcto. Usar una varianza global mezclaría
    # 4b lenguaje con 2m matemática, que están en escalas distintas.
    group_by(grado, area) %>%
    mutate(tau2_respaldo = pmax(var(x_est, na.rm = TRUE), 1e-6)) %>%
    group_by(agno, grado, area, rbd_revisado) %>%
    mutate(
      n_est_col = n(),
      x_c       = x_est - mean(x_est),
      var_obs_c = var(x_est),               # NA si el colegio tiene 1 estudiante
      var_err_c = mean(var_err)
    ) %>%
    ungroup() %>%
    mutate(
      tau2 = if_else(
        n_est_col >= MIN_EST_TAU & !is.na(var_obs_c),
        pmax(var_obs_c - var_err_c, PISO_TAU2 * var_obs_c),
        tau2_g
      ),
      # Nunca por debajo del piso del grupo: un colegio cuyo ruido explica
      # toda su varianza interna quedaría con tau2 = 0 y todos sus
      # estudiantes encogidos al promedio exacto, borrando el ordenamiento.
      tau2       = pmax(coalesce(tau2, tau2_g), PISO_TAU2 * coalesce(tau2_g, 0)),
      tau2       = if_else(is.na(tau2) | tau2 <= 0, tau2_respaldo, tau2),
      rel_indice = tau2 / (tau2 + var_err),
      z_bruto    = x_c / sqrt(tau2),
      # BLUP univariado: la posición bruta, encogida hacia el promedio del
      # colegio en proporción a cuánto de la medición es ruido.
      indice_ensayo = rel_indice * z_bruto
    ) %>%
    select(-tau2_respaldo)

  stopifnot(
    "tau2 no finito en la sección 4" = all(is.finite(est_base$tau2)),
    "rel_indice fuera de [0,1] en la sección 4" =
      all(est_base$rel_indice >= 0 & est_base$rel_indice <= 1)
  )

  cat("  Confiabilidad del índice individual, por número de ensayos rendidos:\n")
  print(
    est_base %>%
      filter(agno == max(agno)) %>%
      group_by(grado, area, k_ensayos) %>%
      summarise(n = n(),
                rel_indice = round(mean(rel_indice), 3),
                .groups = "drop") %>%
      filter(n >= 50)
  )

  est_base
}

# El insumo del índice: theta y su error estándar por estudiante.
# `se_theta` YA es el error de medición, así que no hace falta estimar una
# sigma^2 entre ensayos ni dividirla por k. La calibración lo entrega
# directo y además diferenciado por CUÁLES formas rindió el estudiante, no
# sólo por cuántas.
#
# Los estudiantes sin theta (grupo no calibrado) quedan fuera del índice.
# No se pierden: 01b los conserva en `ind_features` porque su colegio los
# necesita para el promedio, y más abajo se les asigna la posición central.
est_entrada <- ind_features %>%
  filter(!is.na(theta)) %>%
  select(agno, grado, area, rbd_revisado, id_usuario_curso,
         k_ensayos, theta, se_theta, logro_irt) %>%
  mutate(x_est = theta, var_err = se_theta^2)

cat("Estudiantes con theta sobre el total del ensayo:",
    sprintf("%.1f%%", 100 * nrow(est_entrada) / nrow(ind_features)), "
")

est_indice <- construir_indice(est_entrada)

# ---- 3. De índice a percentil ----------------------------------------
# `pct_ensayo` es la posición del estudiante DENTRO de su propio colegio y
# es la materia prima de la predicción individual de 03: a cada estudiante
# se le asigna el puntaje que corresponde a su percentil.
#
# `z_ensayo` es el mismo índice sin convertir a percentil. No lo consume
# la predicción, pero se conserva porque es la magnitud interpretable: dice a
# cuántas desviaciones verdaderas del promedio de su colegio está el
# estudiante, ya descontado el ruido de medición.
ind_features <- ind_features %>%
  left_join(
    est_indice %>%
      select(agno, grado, area, rbd_revisado, id_usuario_curso,
             indice_ensayo, rel_indice, z_bruto),
    by = c("agno", "grado", "area", "rbd_revisado", "id_usuario_curso")
  )

# Los estudiantes sin theta no tienen índice: quedan fuera del ordenamiento
# interno pero se conservan en la base (su colegio los necesita para el
# promedio). Se les asigna la posición central, que es el supuesto neutro.
n_sin_indice <- sum(is.na(ind_features$indice_ensayo))
if (n_sin_indice > 0) {
  cat("\nEstudiantes sin índice (sin theta), al centro de su colegio:",
      n_sin_indice, "de", nrow(ind_features), "\n")
}

ind_features <- ind_features %>%
  mutate(indice_ensayo = coalesce(indice_ensayo, 0),
         rel_indice    = coalesce(rel_indice, 0)) %>%
  group_by(agno, grado, area, rbd_revisado) %>%
  mutate(
    n_est_colegio = n(),
    # percentil dentro del colegio, con corrección (rank - 0.5)/n para
    # que ningún estudiante quede en 0 o 1 exactos.
    pct_ensayo = (rank(indice_ensayo, ties.method = "average") - 0.5) / n(),
    # Posición dentro del colegio. NO se re-estandariza a sd 1:
    # `indice_ensayo` ya viene en unidades de desviación verdadera, así que
    # su sd dentro del colegio es sqrt(rel) < 1 y el encogimiento por
    # confiabilidad sobrevive. Re-estandarizar acá lo borraría.
    # Con menos de 3 estudiantes el centrado no significa nada: 0.
    z_ensayo = if (n() >= 3) indice_ensayo else 0
  ) %>%
  ungroup()

posicion <- ind_features %>%
  select(agno, grado, area, rbd_revisado, id_usuario_curso,
         n_est_colegio, indice_ensayo, rel_indice, z_bruto,
         pct_ensayo, z_ensayo)

# ---- 3b. Confiabilidades (diagnóstico) -------------------------------
# Cuánto puede, como máximo, correlacionar el ensayo con el SIMCE a nivel
# individual: la correlación observable entre dos mediciones está acotada
# por sus confiabilidades. La del SIMCE la calculó 01a; acá se agrega la
# del índice del ensayo. Ningún script aguas abajo lee esta tabla para
# predecir.
conf_indice <- est_indice %>%
  group_by(grado, area) %>%
  summarise(
    conf_indice     = mean(rel_indice, na.rm = TRUE),
    conf_indice_p10 = quantile(rel_indice, 0.10, na.rm = TRUE, names = FALSE),
    k_medio         = mean(k_ensayos),
    .groups = "drop"
  )

confiabilidades <- conf_simce %>%
  select(grado, area, conf_simce) %>%
  left_join(conf_indice, by = c("grado", "area"))

cat("\nConfiabilidades estimadas (diagnóstico):\n")
print(confiabilidades %>% mutate(across(where(is.numeric), ~round(.x, 3))))
cat("\n  conf_indice es la confiabilidad media del índice individual;\n",
    " conf_indice_p10 la del decil peor medido. k_medio es cuántos ensayos\n",
    " rinde en promedio un estudiante.\n")


# ---- 4. Guardar ------------------------------------------------------
salida_posicion <- list(
  posicion        = posicion,          # 1 fila por estudiante
  forma_z         = simce$forma_z,     # plantilla de forma (viene de 01a)
  cortes_tercil   = simce$cortes_tercil,
  confiabilidades = confiabilidades    # diagnóstico
)

saveRDS(salida_posicion, dir_salidas %>% file.path("salida_02c_posicion.rds"))

cat("
Listo. Salida en", dir_salidas %>% file.path("salida_02c_posicion.rds"), "
")
cat("Es una lista con:", paste(names(salida_posicion), collapse = ", "), "
")
cat("Siguiente paso: 03_prediccion.R
")
