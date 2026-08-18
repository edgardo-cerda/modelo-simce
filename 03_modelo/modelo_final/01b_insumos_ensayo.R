# =============================================================
# 01b_insumos_ensayo.R
# -------------------------------------------------------------
# Segunda mitad de la preparación de insumos: todo lo que depende de los
# ensayos del año. Es el script que hay que correr en cada ronda nueva.
#
# CORRE DESPUÉS DE 00 (calibración IRT) Y DE 01a (insumos de SIMCE). Las
# salidas de 01a —el histórico del colegio, su contexto, la forma de la
# distribución interna— no cambian porque lleguen más ensayos, así que se
# leen tal cual y no hay que recalcularlas.
#
# Al no cargar el archivo de alumnos del SIMCE (~2,6 M filas) ni ajustar
# ningún modelo mixto, este script corre en menos de un minuto. Ésa es la
# razón de que la preparación esté partida en dos.
#
# -------------------------------------------------------------
# LA MEDIDA BASE DEL ENSAYO
# -------------------------------------------------------------
# Es la habilidad estimada por la calibración IRT concurrente de 00, y
# entra en los dos niveles:
#
#   INDIVIDUAL  la medida es theta y su error es `se_theta^2`, que la
#               calibración entrega por estudiante. La precisión no se
#               aproxima por CUÁNTOS ensayos rindió: se sabe directamente,
#               y depende también de CUÁLES rindió y de cuánto
#               discriminaban esos ítems.
#
#   ESCOLAR     `mean_logro` es el promedio del PUNTAJE VERDADERO del
#               estudiante — el porcentaje del banco completo de ítems del
#               año que contestaría bien dado su theta (curva
#               característica del test evaluada en theta). Está en puntos
#               de porcentaje de logro, así que 02a, 02b y 03 leen la misma
#               escala de siempre, pero ya no depende de QUÉ ensayos aplicó
#               el colegio.
#
# -------------------------------------------------------------
# EL HISTÓRICO Y EL CONTEXTO LLEGAN DESDE 01a
# -------------------------------------------------------------
# `nivel_hist_colegio` y `sd_hist_colegio` vienen calculados para TODO el
# país; acá sólo se pegan por (año, grado, área, rbd). Los colegios que no
# están en el panel nacional —sin ningún SIMCE previo, o RBD que no cruza—
# reciben los mismos valores por defecto que recibirían si se hubieran
# calculado en el momento: 0 para el nivel y la sd nacional promedio de los
# años previos para la dispersión, que es lo que guarda
# `respaldo_historico.rds`.
#
# El contexto del colegio (GSE, dependencia, ruralidad) entra como PRIOR
# del efecto histórico, no como predictor suelto: probado como columna
# adicional de la regresión final no aporta (MAE entre -0.2 y +0.3, peor en
# un grupo), porque `nivel_hist_colegio` ya lo contiene. Ver el registro de
# pruebas, sección 2.2.
#
# -------------------------------------------------------------
# SALIDA: un único archivo `salida_01b_ensayo.rds` con una lista de:
#
#   $ind_features       1 fila por estudiante x año x grado x área, con su
#                       medida del ensayo (`mean_logro`) y su theta. NO trae
#                       la posición dentro del colegio: ése es el tercer
#                       componente del modelo y lo estima 02c.
#   $school_features    1 fila por colegio x año x grado x área, con
#                       `mean_logro` y su versión encogida `mean_logro_enc`.
#   $school_model_data  school_features + promedio_simce y sd_simce, que es
#                       lo que entrenan 02a y 02b.
#   $confiabilidades    la del SIMCE; 02c le agrega la del índice.
#   $descriptivos       tablas resumidas que consume la presentación.
# =============================================================

library(tidyverse)
library(arrow)

# ---- 0. Configuración --------------------------------------------
usuario <- Sys.info()[["user"]]
rutas <- config::get(config = usuario, file = "config.yml")
ruta_data_intermedia <- rutas$ruta_data_intermedia
ruta_outputs <- rutas$ruta_outputs

dir_salidas <- ruta_outputs %>% file.path('modelo_final')
dir_salidas %>% dir.create(showWarnings = FALSE, recursive = TRUE)

# Piso para la varianza verdadera como fracción de la observada, en el
# encogimiento del nivel escolar (sección 6b). Evita que un grupo donde el
# ruido explica toda la varianza entre colegios quede con confiabilidad 0 y
# todos los colegios encogidos al promedio exacto.
PISO_TAU2 <- 0.05

# ---- 1. Insumos de otros scripts ------------------------------------
# Cada script del pipeline guarda todo lo suyo en una sola lista.
leer_salida <- function(archivo, de) {
  ruta <- dir_salidas %>% file.path(archivo)
  if (!file.exists(ruta)) {
    stop("Falta ", archivo, " en ", dir_salidas, ".\n",
         "Lo genera ", de, ": hay que correrlo antes.")
  }
  readRDS(ruta)
}

simce <- leer_salida("salida_01a_simce.rds", "01a_insumos_simce.R")

simce_dist         <- simce$simce_dist
simce_colegio      <- simce$simce_colegio
contexto_colegio   <- simce$contexto_colegio
nivel_historico    <- simce$nivel_historico
sd_historica       <- simce$sd_historica
respaldo_historico <- simce$respaldo_historico
conf_simce         <- simce$conf_simce
descriptivos_simce <- simce$descriptivos
anios_horizonte    <- simce$anios_horizonte

# Descriptivos del SIMCE que la lista final vuelve a usar tal cual.
desc_simce_alu  <- descriptivos_simce$desc_simce_alu
desc_sd_interna <- descriptivos_simce$desc_sd_interna
desc_simce_anio <- descriptivos_simce$desc_simce_anio
dens_simce_alu  <- descriptivos_simce$dens_simce_alu
varianza_simce  <- descriptivos_simce$varianza_simce
colegio_ejemplo <- descriptivos_simce$colegio_ejemplo
simce_por_gse   <- descriptivos_simce$simce_por_gse

# ---- 2. Cargar ensayos e IRT ---------------------------------------
## ENSAYOS ----
ensayos_santillana0 <- ruta_data_intermedia %>%
  file.path('ensayo_santillana', 'ensayos_santillana_corregido.parquet') %>%
  read_parquet()

ensayos <- ensayos_santillana0 %>%
  mutate(agno = as.numeric(agno)) |>
  filter(!outlier_iqr, !outlier_isoforest)

## IRT: habilidad por estudiante y parámetros de ítem ----
# Los produce 00_calibracion_irt.R. theta está en una escala común entre
# los ensayos de un mismo año x grado x área (media 0, sd 1 en la cohorte
# Santillana de ese año), NO entre años.
irt <- leer_salida("salida_00_irt.rds", "00_calibracion_irt.R")

irt_theta <- irt$theta
irt_items <- irt$items

# --- Puntaje verdadero: theta traducido a porcentaje de logro ------------
# La curva característica del test evaluada en theta: qué porcentaje del
# BANCO COMPLETO de ítems de ese año x grado x área contestaría bien un
# estudiante con esa habilidad,
#
#     logro_irt(theta) = 100 * media_j  P(correcto | theta, a_j, b_j)
#
# con P el 2PL. Es una transformación monótona de theta, así que no cambia
# ningún ordenamiento, pero devuelve la medida a la escala en que está
# escrito el resto del proyecto (0-100) y la vuelve independiente de qué
# ensayos aplicó cada colegio: todos se evalúan contra el mismo banco.
#
# Se evalúa sobre una grilla y se interpola, en vez de calcular 12.000 x 240
# probabilidades por grupo.
curva_verdadera <- function(theta_vec, a_vec, b_vec, n_grilla = 2001) {
  grilla <- seq(min(theta_vec) - 0.5, max(theta_vec) + 0.5, length.out = n_grilla)
  tcc <- vapply(grilla, function(th) mean(plogis(a_vec * (th - b_vec))), numeric(1))
  approx(x = grilla, y = 100 * tcc, xout = theta_vec, rule = 2)$y
}

irt_theta <- irt_theta %>%
  group_by(agno, grado, area) %>%
  group_modify(function(.x, .y) {
    it <- irt_items %>%
      filter(agno == .y$agno, grado == .y$grado, area == .y$area,
             is.finite(a), is.finite(b))
    .x$logro_irt <- if (nrow(it) == 0) NA_real_ else
      curva_verdadera(.x$theta, it$a, it$b)
    .x
  }) %>%
  ungroup()

cat("IRT cargado:", nrow(irt_theta), "estudiantes con theta |",
    n_distinct(paste(irt_theta$agno, irt_theta$grado, irt_theta$area)),
    "grupos calibrados\n")

stopifnot(
  "irt_theta trae theta o se_theta no finitos" =
    all(is.finite(irt_theta$theta) & is.finite(irt_theta$se_theta)),
  "la transformación a puntaje verdadero dejó NA" =
    !any(is.na(irt_theta$logro_irt))
)

# --- COMPARABILIDAD DEL BANCO ENTRE AÑOS ---------------------------------
# El riesgo más serio al predecir una ronda nueva con POCOS ensayos.
#
# `mean_logro` es el porcentaje del BANCO COMPLETO de ítems del año que un
# estudiante contestaría bien. Eso lo vuelve independiente de qué ensayos
# aplicó cada colegio —que era el punto del IRT— pero NO lo vuelve
# independiente de qué ítems componen el banco de ese año. Si un año trae
# 240 ítems y el siguiente 40, los dos "porcentajes del banco" se refieren
# a bancos distintos, y su diferencia de nivel mezcla habilidad con
# composición del banco.
#
# Importa porque el modelo de nivel usa `mean_logro_enc` EN NIVELES: un
# banco más fácil sube el logro de todos los colegios y el modelo lo lee
# como que mejoraron. Es la misma deriva entre años que 02 ya reporta,
# pero amplificada cuando el número de formas cambia mucho.
#
# El indicador es el logro esperado a habilidad media (theta = 0): qué
# porcentaje del banco de ese año contesta bien el estudiante promedio de
# ESE año.
#
# CÓMO SE LEE, con cuidado: theta se estandariza dentro de cada año, así
# que theta = 0 no es el mismo estudiante en 2024 que en 2025 — es el
# promedio de cada cohorte. Por lo tanto este número NO separa "el banco se
# hizo más fácil" de "la cohorte mejoró": mezcla las dos cosas, y sin
# ítems ancla entre años no hay forma de separarlas (ver el encabezado de
# 00). Lo que sí hace es MEDIR el tamaño del movimiento, y avisar cuando
# cambia bruscamente el número de formas, que es cuando la composición del
# banco es menos comparable y el movimiento tiene menos chance de ser real.
banco_por_anio <- irt_items %>%
  filter(is.finite(a), is.finite(b)) %>%
  group_by(agno, grado, area) %>%
  summarise(n_formas = n_distinct(forma), n_items = n(),
            b_medio = mean(b),
            logro_theta0 = 100 * mean(plogis(a * (0 - b))),
            .groups = "drop")

cat("\nComparabilidad del banco de ítems entre años:\n")
cat("(logro_theta0 = qué porcentaje del banco de ese año contesta bien el\n")
cat(" estudiante promedio de ese mismo año. Su movimiento mezcla dificultad\n")
cat(" del banco con nivel de la cohorte: no las separa, sólo las mide.)\n")
print(banco_por_anio %>%
        mutate(across(c(b_medio, logro_theta0), ~round(.x, 2))) %>%
        arrange(grado, area, agno) %>%
        as.data.frame())

# Aviso cuando el año más reciente se aparta del resto. El umbral es
# deliberadamente bajo: 3 puntos de logro sobre el banco ya son del orden
# del sesgo entre años que 02 reporta como problema abierto.
deriva_banco <- banco_por_anio %>%
  group_by(grado, area) %>%
  arrange(agno, .by_group = TRUE) %>%
  summarise(anio_nuevo = last(agno),
            n_formas_nuevo = last(n_formas),
            logro_nuevo = last(logro_theta0),
            logro_previo = if (n() > 1) mean(logro_theta0[-n()]) else NA_real_,
            .groups = "drop") %>%
  mutate(salto = logro_nuevo - logro_previo)

if (any(!is.na(deriva_banco$salto) & abs(deriva_banco$salto) > 3)) {
  problemas <- deriva_banco %>% filter(!is.na(salto), abs(salto) > 3)
  warning("El banco de ítems del año más reciente no es comparable con el ",
          "de los años previos en: ",
          paste(sprintf("%s %s (%+.1f pts de logro, %d forma(s))",
                        problemas$grado, problemas$area, problemas$salto,
                        problemas$n_formas_nuevo), collapse = "; "),
          ". El modelo de nivel usa `mean_logro_enc` EN NIVELES, así que ",
          "ese salto se traslada a la predicción como si fuera mejora real. ",
          "Ver la nota de 02 sobre la especificación centrada.")
  cat("\n*** AVISO: salto de banco entre años. Ver el warning. ***\n")
  print(deriva_banco %>% mutate(across(where(is.numeric), ~round(.x, 2))) %>%
          as.data.frame())
}

# ---- 3. Limpieza de ensayos -----------------------------------------
ensayos_limpio <- ensayos %>%
  mutate(
    porcentaje_logro = pmin(porcentaje_logro, 100),
    n_evaluacion = as.integer(n_evaluacion)
  ) %>%
  filter(!is.na(rbd_revisado), !is.na(n_evaluacion), porcentaje_logro > 0)

# `porcentaje_logro` queda como viene de la fuente. Se usa sólo como
# respaldo para los pocos estudiantes sin theta (secc. 3) y en los
# descriptivos: la medida que consumen los modelos es el puntaje verdadero
# del IRT.

# Revisar - no deben quedar valores NA - sobre 100 o bajo 0 en porcentaje de logro
stopifnot(
  ensayos_limpio |> 
    filter(is.na(porcentaje_logro)) |> 
    nrow()==0
)

stopifnot(
  ensayos_limpio |> 
    filter(porcentaje_logro<0) |> 
    nrow()==0
)

stopifnot(
  ensayos_limpio |> 
    filter(porcentaje_logro>100) |> 
    nrow()==0
)



ensayos_dedup <- ensayos_limpio %>%
  group_by(id_usuario_curso, agno, grado, area, n_evaluacion, rbd_revisado) %>%
  summarise(porcentaje_logro = mean(porcentaje_logro), .groups = "drop")


# ---- 4. Resumen simple por estudiante --------------------------------
# UNA medida por estudiante: `mean_logro`, el puntaje verdadero — qué
# porcentaje del banco COMPLETO de ítems del año contestaría bien según su
# theta. Escala 0-100, y no depende de qué ensayos le tocaron.
#
# `logro_observado` (el promedio crudo de los ensayos que rindió) se
# conserva sólo como respaldo para los pocos estudiantes sin theta y para
# los descriptivos. Depende de CUÁLES ensayos rindió: un alumno que sólo
# dio el ensayo fácil sale mejor evaluado sin ser mejor.
resumen_simple <- ensayos_dedup %>%
  group_by(id_usuario_curso, agno, grado, area, rbd_revisado) %>%
  summarise(
    k_ensayos       = n(),
    logro_observado = mean(porcentaje_logro),
    .groups = "drop"
  ) %>%
  left_join(
    irt_theta %>% select(agno, grado, area, id_usuario_curso,
                         theta, se_theta, logro_irt),
    by = c("agno", "grado", "area", "id_usuario_curso")
  ) %>%
  mutate(
    # Un estudiante sin theta (grupo no calibrado) conserva su logro
    # observado: es peor medida, pero lo deja en la base en vez de borrarlo.
    mean_logro = coalesce(logro_irt, logro_observado)
  )

cat("\nEstudiantes sin theta que conservan su logro observado:",
    sum(is.na(resumen_simple$theta)), "de", nrow(resumen_simple), "\n")

# ---- 5. Features a nivel de estudiante --------------------------------
# `ind_features` es una fila por estudiante x año x grado x área, con su
# medida del ensayo (`mean_logro`) y su theta. La POSICIÓN dentro del
# colegio —el índice encogido y su percentil— NO se calcula acá: es el
# tercer componente del modelo y vive en 02c_estimacion_posicion.R.
ind_features <- resumen_simple


# ---- 6. Features a nivel de colegio ----------------------------------
# Tres features del ensayo por colegio: su NIVEL (`mean_logro`) y dos de
# DISPERSIÓN (`sd_entre_estud` e `iqr_logro_ensayo`, este último algo más
# robusto frente a colegios con outliers). Las de dispersión son
# predictores del modelo de 02b.
#
school_features <- ind_features %>%
  group_by(agno, grado, area, rbd_revisado) %>%
  summarise(
    n_estudiantes    = n(),
    k_ensayos_prom   = mean(k_ensayos, na.rm = TRUE),

    # OJO con los nombres: summarise() evalúa en orden y con enmascaramiento
    # de datos, así que si la media se llamara `mean_logro` —igual que la
    # columna de entrada— el `sd()` de la línea siguiente recibiría el
    # ESCALAR recién calculado y no la columna, y devolvería NA en silencio.
    # Por eso la salida se nombra distinto y se renombra después.
    prom_logro         = mean(mean_logro, na.rm = TRUE),
    sd_entre_estud     = sd(mean_logro, na.rm = TRUE),
    iqr_logro_ensayo   = quantile(mean_logro, 0.90, names = FALSE) -
                         quantile(mean_logro, 0.10, names = FALSE),

    # Para el encogimiento en escala theta (secc. 6b): el promedio de
    # habilidad del colegio y la dispersión entre sus alumnos, que son los
    # análogos exactos de `mean_logro` y `sd_entre_estud` pero en
    # unidades de theta. Con estas dos, 5b-bis reusa `encoger_nivel()` tal
    # cual y el error del promedio sigue siendo el de MUESTREO
    # (sd^2 / n), que es lo que corresponde: los alumnos que rinden el ensayo
    # no son los mismos que rinden el SIMCE, así que la variabilidad de qué
    # alumnos son forma parte del error, no sólo la precisión con que se mide
    # a cada uno.
    #
    # OJO: se guarda `n_theta` —cuántos alumnos TIENEN theta— porque
    # `mean(theta, na.rm = TRUE)` excluye del promedio a los no calibrados
    # (secc. 3) y el divisor del error tiene que excluirlos también.
    n_theta            = sum(!is.na(theta)),
    mean_theta         = mean(theta, na.rm = TRUE),
    sd_entre_estud_theta = sd(theta, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(mean_logro = prom_logro) %>%
  mutate(
    # Colegios sin ningún estudiante calibrado (grupo no calibrado, o los
    # pocos que quedan fuera por outliers): mean() de un vector vacío da NaN,
    # no NA. Se homogeniza para que is.na()/coalesce() aguas abajo lo traten
    # como lo que es, un dato ausente.
    mean_theta = if_else(is.nan(mean_theta), NA_real_, mean_theta)
  )

# ---- 6b. Encogimiento de mean_logro por confiabilidad -------------
# Reemplaza a `n_evals_prom` en el modelo de 02. El razonamiento es el
# mismo del índice individual, un nivel más arriba: el `mean_logro` de un
# colegio es una ESTIMACIÓN de su nivel verdadero en el ensayo, y su
# error de muestreo es la sd entre sus estudiantes dividida por la raíz
# de cuántos son:
#
#     var_error = sd_entre_estud^2 / n_estudiantes
#
# La confiabilidad es la fracción de la varianza observada ENTRE colegios
# que es señal:
#
#     conf = var_verdadera / (var_verdadera + var_error)
#
# y el valor encogido acerca al colegio al promedio de su grupo en
# proporción a cuánto de su medición es ruido:
#
#     mean_logro_enc = mu_grupo + conf * (mean_logro - mu_grupo)
#
# Por qué esto reemplaza bien a `n_evals_prom`: el número de ensayos
# entra AUTOMÁTICAMENTE por la puerta correcta. Un colegio cuyos alumnos
# rindieron un solo ensayo tiene medidas individuales más ruidosas, eso
# infla `sd_entre_estud`, eso infla `var_error` y eso lo encoge más. No
# hace falta un término lineal aparte, que además nunca tuvo una lectura
# defendible (¿por qué el número de ensayos subiría o bajaría el SIMCE?).
#
# El encogimiento va hacia el promedio del grupo, no hacia
# `nivel_hist_colegio`: las dos variables están en escalas distintas
# (% de logro vs. puntos SIMCE). El efecto neto es el buscado igual —
# como el `mean_logro` de un colegio mal medido aporta menos, el modelo
# se apoya proporcionalmente más en el prior histórico-contextual, que
# ya está en la fórmula como término propio.
#
# EXPECTATIVA HONESTA: con 45-72 estudiantes por colegio en la mediana,
# la confiabilidad del promedio escolar es alta (~0.90-0.97, p10 entre
# 0.79 y 0.94). El encogimiento es por lo tanto suave y NO se espera que
# mejore el MAE — en la prueba out-of-time queda entre neutro y +0.25
# puntos peor. Se justifica por corrección y por reemplazar un término
# ininterpretable, no por precisión. Si se quiere volver atrás, basta
# usar `mean_logro` en 02a en vez de `mean_logro_enc`.
#
# No hay filtración temporal: todo esto se calcula con los ensayos del
# mismo año, que están disponibles al momento de predecir. No usa SIMCE.
#
# `col_n` es cuántos alumnos entran en el promedio: se pasa `n_theta`,
# porque los estudiantes sin calibrar no entran en `mean_theta` y tampoco
# deben entrar en el divisor de su error.
encoger_nivel <- function(d, col_media, col_sd, out_enc, out_conf, out_var_err,
                          col_n = "n_estudiantes") {
  media <- d[[col_media]]
  sd_e  <- d[[col_sd]]
  clave <- paste(d$agno, d$grado, d$area)

  # La sd entre estudiantes es NA en colegios con 1 alumno: se les asigna la
  # sd típica de su grupo, peor que su dato pero mejor que un NA.
  sd_aux  <- ave(sd_e, clave, FUN = function(x) coalesce(x, median(x, na.rm = TRUE)))
  var_err <- sd_aux^2 / pmax(d[[col_n]], 1)

  var_obs  <- ave(media,   clave, FUN = function(x) var(x, na.rm = TRUE))
  var_err_m <- ave(var_err, clave, FUN = function(x) mean(x, na.rm = TRUE))
  var_true <- pmax(var_obs - var_err_m, PISO_TAU2 * var_obs)
  mu_grupo <- ave(media,   clave, FUN = function(x) mean(x, na.rm = TRUE))

  conf <- var_true / (var_true + var_err)
  # Grupos con un solo colegio (var no estimable) o cualquier otro caso
  # degenerado: confiabilidad 1, o sea no encoger. Deja la media tal cual en
  # vez de convertirla en NA y romper el modelo.
  conf <- if_else(is.finite(conf), conf, 1)

  d[[out_var_err]] <- var_err
  d[[out_conf]]    <- conf
  d[[out_enc]]     <- coalesce(mu_grupo + conf * (media - mu_grupo), media)
  d
}

# --- Encogimiento en escala THETA, expresado de vuelta en logro ---------
# Por qué no se encoge directamente `mean_logro`: esa columna ya es el
# promedio de una curva —la característica del test evaluada en el theta de
# cada alumno (secc. 1)—, y encogerla hacia la media del grupo EN ESA
# ESCALA no es lo mismo que encoger la habilidad y curvar recién al final.
# La curva comprime los extremos (es una logística), así que el error de
# esa aproximación se concentra justo donde más importa: colegios con theta
# alto o bajo, que son los que un encogimiento debería mover más.
#
# La corrección: encoger `mean_theta` —que por construcción de la
# calibración concurrente tiene media 0 y sd 1 en la cohorte de cada año x
# grado x área, la escala natural para este shrinkage lineal— y sólo
# entonces traducir el resultado a puntos de logro con la misma
# `curva_verdadera()` de la sección 1. Así la salida queda en la escala
# única (0-100) que consumen 02a, 02b y 03, pero el encogimiento en sí
# ocurre donde el supuesto de linealidad es correcto.
#
# Lo que NO cambia es la definición del error. `encoger_nivel()` se aplica
# tal cual sobre (`mean_theta`, `sd_entre_estud_theta`), así que el error del
# promedio escolar sigue siendo el de MUESTREO —sd^2/n, que incluye tanto qué
# alumnos son como con cuánta precisión se mide a cada uno— y no sólo el de
# medición. Usar `se_theta^2/n` en su lugar parece más fino, porque la
# calibración entrega ese error directo, pero descarta la parte de muestreo:
# medido sobre estos datos sube la confiabilidad de ~0.91-0.97 a ~0.97-0.995
# y por lo tanto encoge bastante menos. Y la parte de muestreo acá no es
# ruido irrelevante: los alumnos que rinden el ensayo no son los mismos que
# rinden el SIMCE, que es lo que este número termina prediciendo.
#
# Reusar `encoger_nivel()` en vez de escribir la aritmética de nuevo también
# garantiza que los casos degenerados se traten igual que en la versión cruda
# (colegios de 1 alumno sin sd, grupos con un solo colegio).
school_features <- school_features %>%
  encoger_nivel("mean_theta", "sd_entre_estud_theta",
                "theta_enc", "conf_mean_logro", "var_err_logro",
                col_n = "n_theta") %>%
  # Traducir theta_enc a puntos de logro, grupo por grupo, con los
  # parámetros de ítem de ESE año x grado x área (mismo patrón que la
  # sección 1 con `irt_theta`).
  group_by(agno, grado, area) %>%
  group_modify(function(.x, .y) {
    it <- irt_items %>%
      filter(agno == .y$agno, grado == .y$grado, area == .y$area,
             is.finite(a), is.finite(b))
    .x$mean_logro_enc <- if (nrow(it) == 0) NA_real_ else
      curva_verdadera(.x$theta_enc, it$a, it$b)
    .x
  }) %>%
  ungroup() %>%
  mutate(
    # Colegios sin theta (grupo no calibrado) o sin parámetros de ítem: se
    # quedan con el nivel sin encoger, mismo respaldo que usa
    # `encoger_nivel()` en el caso degenerado.
    mean_logro_enc  = coalesce(mean_logro_enc, mean_logro),
    conf_mean_logro = coalesce(conf_mean_logro, 1)
  )

# ---- 6c. Nivel del ensayo CENTRADO DENTRO DEL AÑO -----------------
# El problema que resuelve: el logro en los ensayos sube todos los años y
# el SIMCE no lo sigue. En el panel de colegios con SIMCE observado el
# logro IRT subió de 2023 a 2025 en los cuatro grupos, mientras el SIMCE
# 2025 CAYÓ en tres de ellos. Un modelo entrenado con 2023-24, donde ambas
# series suben juntas, proyecta a 2025 una subida que no ocurrió: sobre-
# predice 5 a 6 puntos.
#
# Parte de esa deriva es composición (cada año entran colegios nuevos a
# Santillana), parte es dificultad de las formas y parte puede ser real.
# Separarlas no es posible con tres años. Lo que sí se puede es dejar de
# leerla como nivel: centrando dentro de año x grado x área, el ensayo
# aporta sólo la POSICIÓN RELATIVA del colegio entre sus pares de ese año,
# y el nivel absoluto queda a cargo de `nivel_hist_colegio`. La deriva
# común se cancela por construcción, sea cual sea su origen.
#
# Se calcula sin usar SIMCE y sólo con los ensayos del propio año, así que
# está disponible al momento de predecir una ronda nueva: 03 puede
# construirla igual (centrar los colegios del año que se predice entre sí).
#
school_features <- school_features %>%
  group_by(agno, grado, area) %>%
  mutate(
    mean_logro_enc_c = mean_logro_enc - mean(mean_logro_enc, na.rm = TRUE)
  ) %>%
  ungroup()

cat("\nDeriva del ensayo entre años (media del grupo que se descuenta al centrar):\n")
print(
  school_features %>%
    group_by(agno, grado, area) %>%
    summarise(n_colegios = n(),
              media_logro_enc = round(mean(mean_logro_enc, na.rm = TRUE), 1),
              .groups = "drop") %>%
    as.data.frame()
)

cat("\nConfiabilidad del mean_logro escolar (encogimiento del punto 2):\n")
print(
  school_features %>%
    group_by(agno, grado, area) %>%
    summarise(n_colegios = n(),
              n_est_mediana = median(n_estudiantes),
              conf_media = round(mean(conf_mean_logro), 3),
              conf_p10   = round(quantile(conf_mean_logro, 0.10), 3),
              encogim_max_pts = round(max(abs(mean_logro - mean_logro_enc)), 2),
              .groups = "drop")
)

# ---- 7. Pegar el histórico y el contexto que calculó 01a --------------
# 01a los calculó para todo el país y para un horizonte de años. Acá sólo
# se seleccionan los que corresponden a cada colegio con ensayo.
anios_con_ensayo <- sort(unique(school_features$agno))
fuera_de_horizonte <- setdiff(anios_con_ensayo, anios_horizonte)

if (length(fuera_de_horizonte) > 0) {
  stop("Hay ensayos de ", paste(fuera_de_horizonte, collapse = ", "),
       ", pero 01a sólo precalculó hasta ", max(anios_horizonte), ".\n",
       "Hay que volver a correr 01a_insumos_simce.R con un HORIZONTE_ANIOS mayor.")
}

vars_contexto <- c("cod_grupo", "gse_etiqueta", "cod_depe1", "depe1_etiqueta",
                   "cod_depe2", "depe2_etiqueta", "cod_rural_rbd", "rural_etiqueta",
                   "nom_com_rbd", "agno_contexto")

school_features <- school_features %>%
  left_join(nivel_historico %>% select(agno, grado, area, rbd_revisado,
                                        contexto_nivel, desvio_nivel,
                                        nivel_hist_colegio, n_anios_nivel_hist),
            by = c("agno", "grado", "area", "rbd_revisado")) %>%
  left_join(sd_historica %>% select(agno, grado, area, rbd_revisado,
                                    contexto_sd, sd_hist_colegio, n_anios_sd_hist),
            by = c("agno", "grado", "area", "rbd_revisado")) %>%
  left_join(contexto_colegio %>% select(agno, grado, area, rbd_revisado,
                                        all_of(vars_contexto)),
            by = c("agno", "grado", "area", "rbd_revisado")) %>%
  # Respaldo de dispersión para colegios que no están en el panel nacional
  # (sin ningún SIMCE previo, o RBD que no cruza). Reciben la sd nacional
  # promedio de los años previos, que es exactamente lo que les habría
  # tocado si el histórico se hubiera calculado sobre ellos.
  left_join(respaldo_historico, by = c("agno", "grado", "area")) %>%
  mutate(
    across(c(contexto_nivel, desvio_nivel, nivel_hist_colegio),
           ~ replace_na(.x, 0)),
    n_anios_nivel_hist = replace_na(n_anios_nivel_hist, 0L),
    n_anios_sd_hist    = replace_na(n_anios_sd_hist, 0L),
    contexto_sd        = coalesce(contexto_sd, contexto_sd_respaldo),
    sd_hist_colegio    = coalesce(sd_hist_colegio, pmax(contexto_sd, 5)),
    sin_historia       = n_anios_nivel_hist == 0,
    sin_contexto       = is.na(cod_grupo)
  ) %>%
  select(-contexto_sd_respaldo) %>%
  # Respaldo final por si un grupo entero quedó sin histórico de dispersión.
  group_by(grado, area) %>%
  mutate(
    contexto_sd     = coalesce(contexto_sd, mean(contexto_sd, na.rm = TRUE)),
    sd_hist_colegio = coalesce(sd_hist_colegio, contexto_sd)
  ) %>%
  ungroup()

ind_features <- ind_features %>%
  left_join(school_features %>% select(agno, grado, area, rbd_revisado,
                                       nivel_hist_colegio, contexto_nivel,
                                       all_of(vars_contexto)),
            by = c("agno", "grado", "area", "rbd_revisado")) %>%
  mutate(nivel_hist_colegio = replace_na(nivel_hist_colegio, 0))

cat(sprintf("\nCobertura del contexto rezagado en colegios con ensayo: %.1f%%\n",
            100 * mean(!school_features$sin_contexto)))
cat("Colegios sin historia previa (reciben el prior de su contexto):",
    sum(school_features$sin_historia), "de", nrow(school_features), "\n")
cat("Colegios sin contexto conocido (reciben expectativa 0):",
    sum(school_features$sin_contexto), "\n")

# ---- 8. Confiabilidad del SIMCE (se completa en 02c) -----------------
# La confiabilidad del índice individual la calcula 02c, que es donde se
# estima ese índice. Acá sólo viaja la del SIMCE, que viene de 01a, para
# que quede junto al resto de los insumos.
confiabilidades <- conf_simce %>% select(grado, area, conf_simce)

# ---- 9. Cruce con el SIMCE del MISMO año (para entrenar) -------------
# Dos targets: el promedio (modelo de 02) y la sd interna (modelo de 02b).
# Los años sin SIMCE —una ronda nueva— quedan fuera por el inner_join, que
# es lo correcto: no hay con qué entrenar sobre ellos.
school_model_data <- school_features %>%
  inner_join(simce_colegio,
             by = c("agno", "grado", "area", "rbd_revisado")) %>%
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

# Composición de la base Santillana. Vale la pena mirarla cada vez: es la
# razón por la que los coeficientes de contexto NO se estiman acá sino en
# el universo nacional.
cat("\nComposición de la base Santillana por contexto (último año, matemática):\n")
print(
  school_features %>%
    filter(agno == max(agno), area == "matematica") %>%
    count(grado, depe2_etiqueta, gse_etiqueta) %>%
    pivot_wider(names_from = gse_etiqueta, values_from = n, values_fill = 0)
)

# ---- 10. Descriptivos para la presentación ------------------------------
# Los que dependen del SIMCE los calculó 01a; acá se agregan los del
# ensayo y se arma la lista final que consume la presentación.
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

# En los ensayos, un mismo alumno rinde varias veces y en dos áreas: el
# promedio se calcula por alumno-área, que es la unidad del índice
# individual de la sección 4.
desc_ensayos_anio <- ensayos_dedup %>%
  group_by(agno) %>%
  summarise(
    n_colegios          = n_distinct(rbd_revisado),
    n_alumnos           = n_distinct(id_usuario_curso),
    n_ensayos           = n(),
    ensayos_por_alumno  = n() / n_distinct(paste(id_usuario_curso, area)),
    .groups = "drop"
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

# ¿Sube el logro a medida que avanzan los ensayos del año? Sube y baja
# según la dificultad de cada ensayo, no de forma monótona: es la serie que
# muestra por qué el logro OBSERVADO no es comparable entre formas y por
# qué hace falta la calibración IRT.
logro_por_evaluacion <- ensayos_dedup %>%
  group_by(agno, grado, area, n_evaluacion) %>%
  summarise(logro_medio = mean(porcentaje_logro),
            n_obs = n(), .groups = "drop") %>%
  filter(n_obs >= 30)

# --- Evolución conjunta de ambas fuentes -------------------------------
# Cada serie se calcula sobre la población que le corresponde:
#
#   - ENSAYOS: promedio de logro de todos los colegios que rindieron
#     ensayo ese año.
#   - SIMCE: promedio de los colegios que ESE AÑO están en la base de
#     ensayos (o sea, los mismos colegios de la serie anterior que además
#     tienen SIMCE publicado). Así las dos series hablan del mismo grupo
#     de establecimientos y no del universo nacional.
#
# Las escalas son distintas (puntos vs. % de logro), así que cada serie se
# estandariza dentro de su grado x área. La estandarización permite
# comparar el MOVIMIENTO, no el nivel, y no corrige que la dificultad del
# ensayo pueda cambiar de un año a otro.
z_seguro <- function(x) {
  if (sum(!is.na(x)) > 1 && sd(x, na.rm = TRUE) > 0) {
    (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
  } else 0
}

logro_anual <- school_features %>%
  group_by(agno, grado, area) %>%
  summarise(n_colegios_ensayo = n(),
            logro = mean(mean_logro, na.rm = TRUE),
            .groups = "drop")

simce_anual <- school_model_data %>%
  filter(!is.na(promedio_simce)) %>%
  group_by(agno, grado, area) %>%
  summarise(n_colegios_simce = n(),
            simce = mean(promedio_simce),
            .groups = "drop")

evolucion_fuentes <- logro_anual %>%
  full_join(simce_anual, by = c("agno", "grado", "area")) %>%
  group_by(grado, area) %>%
  mutate(z_simce = z_seguro(simce), z_logro = z_seguro(logro)) %>%
  ungroup() %>%
  arrange(grado, area, agno)

cat("\nEvolución conjunta de ambas fuentes:\n")
print(evolucion_fuentes %>%
        mutate(across(c(simce, logro), ~round(.x, 1))) %>%
        select(agno, grado, area, n_colegios_ensayo, logro, n_colegios_simce, simce))

# Composición por GSE: colegios con ensayo vs. universo nacional. Es la
# evidencia de que la base no es representativa. La mitad nacional la
# calculó 01a.
niveles_gse_etiqueta <- c("Bajo", "Medio bajo", "Medio", "Medio alto", "Alto")

comp_gse <- bind_rows(
  school_features %>%
    filter(agno == max(agno), area == "matematica") %>%
    distinct(grado, rbd_revisado, gse_etiqueta) %>%
    count(grado, gse_etiqueta, name = "n") %>%
    mutate(fuente = "santillana"),
  descriptivos_simce$comp_gse_nacional
) %>%
  filter(!is.na(gse_etiqueta)) %>%
  mutate(gse_etiqueta = factor(gse_etiqueta, levels = niveles_gse_etiqueta)) %>%
  group_by(fuente, grado) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

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

# ---- 11. Guardar --------------------------------------------------------
salida_ensayo <- list(
  ind_features      = ind_features,      # 1 fila por estudiante
  school_features   = school_features,   # 1 fila por colegio
  school_model_data = school_model_data, # + promedio_simce y sd_simce
  confiabilidades   = confiabilidades,   # diagnóstico
  descriptivos      = descriptivos       # tablas para la presentación
)

saveRDS(salida_ensayo, dir_salidas %>% file.path("salida_01b_ensayo.rds"))

cat("\nListo. Salida en", dir_salidas %>% file.path("salida_01b_ensayo.rds"), "\n")
cat("Es una lista con:", paste(names(salida_ensayo), collapse = ", "), "\n")
cat("Siguiente paso: 03_prediccion.R para predecir la ronda,\n")
cat("o 02a_estimacion_nivel.R si llegó SIMCE nuevo y hay que re-estimar.\n")
