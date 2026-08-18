# =============================================================
# 01a_insumos_simce.R
# -------------------------------------------------------------
# Primera mitad de la preparación de insumos: TODO lo que depende sólo
# del SIMCE y nada de los ensayos.
#
# CUÁNDO CORRERLO: sólo cuando llega un SIMCE nuevo. Sus salidas no
# cambian porque lleguen más ensayos, así que en una ronda de predicción
# se reusan tal cual y no hace falta volver a ejecutarlo.
#
# Es la parte cara del pipeline: carga ~2,6 millones de puntajes
# individuales y ajusta dos modelos mixtos por año objetivo x grado x
# área. Sacarla de la ruta de predicción es lo que hace que una ronda
# nueva corra en minutos en vez de en decenas de minutos.
#
# -------------------------------------------------------------
# EL AÑO OBJETIVO Y EL HORIZONTE
# -------------------------------------------------------------
# Las piezas históricas —nivel, dispersión, forma de la distribución— se
# calculan PARA UN AÑO OBJETIVO, usando sólo años estrictamente
# anteriores. Como este script no mira los ensayos, no puede saber qué
# años van a tener ronda, así que precalcula para un horizonte: todos los
# años con SIMCE más los `HORIZONTE_ANIOS` siguientes.
#
# Si 01b pide un año fuera de ese rango, avisa que hay que volver a
# correr este script con un horizonte mayor.
#
# -------------------------------------------------------------
# POR QUÉ EL HISTÓRICO SE CALCULA PARA TODO EL PAÍS
# -------------------------------------------------------------
# El efecto histórico de un colegio se estima con un modelo sobre el
# universo NACIONAL.
#
# El contexto del colegio (GSE, dependencia, ruralidad) entra como PRIOR
# del efecto histórico, no como predictor suelto: ver el encabezado de 01b
# y el registro de pruebas, sección 2.2.
#
# CONTEXTO REZAGADO: el GSE se recalcula en cada medición y cambia de
# categoría en ~26% de los colegios de un año a otro; la dependencia se
# movió 8% entre 2024 y 2025 por el paso a Servicio Local. Como ambas se
# publican JUNTO con los resultados del año, usar las del año objetivo
# sería filtrar información que no existe al momento de predecir. Se usa
# siempre el último registro ESTRICTAMENTE anterior.
#
# -------------------------------------------------------------
# SALIDA: un único archivo `salida_01a_simce.rds` con una lista de:
#
#   $simce_alumno        SIMCE individual en formato largo
#   $simce_dist          media, sd y cuantiles por colegio
#   $simce_colegio       promedio_simce por colegio (target de 02)
#   $limites_simce       rango plausible de puntajes por grupo
#   $contexto_rezagado   contexto del colegio, por año objetivo
#   $contexto_colegio    idem con etiquetas legibles e histórico
#   $nivel_historico     efecto persistente de NIVEL, todo el país
#   $sd_historica        efecto persistente de DISPERSIÓN, idem
#   $respaldo_historico  valores por defecto para colegios sin contexto
#   $forma_z             plantilla de forma de la distribución interna
#   $cortes_tercil       cortes para asignar tercil de nivel
#   $conf_simce          confiabilidad del SIMCE (diagnóstico)
#   $descriptivos        descriptivos que sólo usan SIMCE
#   $anios_horizonte     años objetivo precalculados
# =============================================================

library(tidyverse)
library(arrow)
library(lme4)

# ---- 0. Configuración --------------------------------------------
usuario <- Sys.info()[["user"]]
rutas <- config::get(config = usuario, file = "config.yml")
ruta_data_intermedia <- rutas$ruta_data_intermedia
ruta_outputs <- rutas$ruta_outputs

dir_salidas <- ruta_outputs %>% file.path('modelo_lme_alu_v2')
dir_salidas %>% dir.create(showWarnings = FALSE, recursive = TRUE)

# Parámetros -------------------------------------------------------
# Mínimo de alumnos con SIMCE para que la sd interna del colegio sea
# una estimación utilizable
MIN_ALU_SD    <- 15
GRADOS_MODELO <- c("4b", "2m")   # los únicos con ensayo Santillana
# Grilla de percentiles con que se guarda la plantilla de forma.
GRILLA_P      <- seq(0.005, 0.995, by = 0.005)

# Cuántos años hacia adelante precalcular, además de los que ya tienen
# SIMCE. Con 3 alcanza para varias rondas sin volver a correr este script.
HORIZONTE_ANIOS <- 3

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

# ---- 1. Cargar SIMCE -----------------------------------------------

## SIMCE AGREGADO POR COLEGIO ----
simce0_rbd <- ruta_data_intermedia %>%
  file.path('simce', 'resultados_simce_rbd_corregido.parquet') %>%
  read_parquet()

simce <- simce0_rbd %>%
  filter(!outlier_iqr, !outlier_isoforest)

## SIMCE POR ALUMNO ----
ruta_alu <- ruta_data_intermedia %>%
  file.path('simce', 'consolidado_datos_simce_alu.parquet')

simce_alu0 <- read_parquet(ruta_alu)

# ---- 2. SIMCE individual a formato largo -----------------------------
# El archivo viene ancho (una columna por área). Se pasa a largo para que
# calce con la llave (agno, grado, area, rbd_revisado) del resto del
# pipeline. Se descartan puntajes ausentes (~20% de las filas: son alumnos
# matriculados que no rindieron o quedaron excluidos).
simce_alumno <- simce_alu0 %>%
  filter(grado %in% GRADOS_MODELO) %>%
  mutate(agno = as.numeric(agno),
         rbd_revisado = as.numeric(rbd)) %>%
  pivot_longer(cols = c(ptje_mate, ptje_lect, eem_mate, eem_lect, eda_mate, eda_lect),
               names_sep = '_',
               names_to = c('.value', 'area')
               ) %>%
  mutate(area = ifelse(area == 'mate', 'matematica', 'lenguaje')) %>%
  filter(!is.na(ptje))

rm(simce_alu0); gc()

cat("SIMCE individual cargado:", nrow(simce_alumno), "puntajes alumno x área\n")

# ---- 3. Distribución interna observada por colegio -------------------
# Esta tabla es la "verdad" contra la que se validan las predicciones
# individuales en 04_validacion.R.
simce_dist <- simce_alumno %>%
  group_by(agno, grado, area, rbd_revisado) %>%
  summarise(
    n_alu_simce     = n(),
    media_simce_alu = mean(ptje, na.rm = TRUE),
    sd_simce        = sd(ptje, na.rm = TRUE),
    p10_simce       = quantile(ptje, 0.10, names = FALSE, na.rm = TRUE),
    p25_simce       = quantile(ptje, 0.25, names = FALSE, na.rm = TRUE),
    p50_simce       = quantile(ptje, 0.50, names = FALSE, na.rm = TRUE),
    p75_simce       = quantile(ptje, 0.75, names = FALSE, na.rm = TRUE),
    p90_simce       = quantile(ptje, 0.90, names = FALSE, na.rm = TRUE),
    eem_medio       = mean(eem, na.rm = TRUE),  # error de medición del test
    .groups = "drop"
  ) %>%
  filter(n_alu_simce >= MIN_ALU_SD, !is.na(sd_simce), sd_simce > 0)

# Rango plausible de puntajes por grado/área: se usa para acotar las
# predicciones individuales (no tiene sentido predecir 500 puntos).
limites_simce <- simce_alumno %>%
  filter(!is.na(ptje)) %>%
  group_by(grado, area) %>%
  summarise(
    ptje_min = quantile(ptje, 0.0001, names = FALSE),
    ptje_max = quantile(ptje, 0.9999, names = FALSE),
    .groups = "drop"
  )

# ---- 4. Contexto del colegio y universo nacional ---------------------
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
# dependencia y ruralidad, porque acá sí hay variación en esas variables
# (la base Santillana no la tiene).
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

# ---- 5. Años objetivo del horizonte ----------------------------------
anios_simce <- sort(unique(simce_limpio$agno))
anios_horizonte <- (min(anios_simce) + 1):(max(anios_simce) + HORIZONTE_ANIOS)

anios_objetivo <- expand_grid(
  agno  = anios_horizonte,
  grado = GRADOS_MODELO,
  area  = c("matematica", "lenguaje")
)

cat("\nAños con SIMCE:", paste(anios_simce, collapse = ", "), "\n")
cat("Años objetivo precalculados:", paste(anios_horizonte, collapse = ", "), "\n")

# ---- 6. Contexto rezagado --------------------------------------------
# Para cada año objetivo, el último registro de contexto estrictamente
# anterior de ese mismo colegio, grado y área.
contexto_rezagado <- map_dfr(anios_horizonte, function(y) {
  contexto_crudo %>%
    filter(agno < y) %>%
    group_by(grado, area, rbd_revisado) %>%
    slice_max(agno, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    rename(agno_contexto = agno) %>%
    mutate(agno = y)
}) %>%
  preparar_factores()

# Todos los colegios del país que tienen contexto para ese año objetivo.
# 01b se queda después con los que tienen ensayo.
colegios_universo <- function(a, g, ar) {
  contexto_rezagado %>%
    filter(agno == a, grado == g, area == ar) %>%
    select(rbd_revisado, cod_grupo, cod_depe1, cod_rural_rbd,
           f_gse, f_depe, f_rural)
}

# ---- 7. Efecto histórico de NIVEL, con prior contextual --------------
# El modelo se ajusta sobre el universo NACIONAL de años estrictamente
# anteriores:
#
#   promedio_simce ~ factor(agno) + GSE + dependencia + ruralidad + (1 | rbd)
#
# y para cada colegio devuelve:
#   contexto_nivel      lo que se espera de un colegio de ese contexto,
#                       centrado en el promedio nacional del año de
#                       referencia (0 = "colegio nacional promedio").
#   desvio_nivel        cuánto se desvía este colegio de esa expectativa,
#                       ya encogido por lme4 según cuánta historia tenga.
#   nivel_hist_colegio  la suma.
#
# Un colegio sin historia queda con desvio_nivel = 0, es decir "se
# comporta como los de su contexto", que es mejor prior que el promedio
# nacional.
estimar_nivel_historico <- function(nacional, objetivo, anio_objetivo,
                                    grado_obj, area_obj, sd_tipica = NA_real_) {

  vacio <- objetivo %>%
    transmute(rbd_revisado,
              contexto_nivel = 0, desvio_nivel = 0,
              nivel_hist_colegio = 0, n_anios_nivel_hist = 0L)

  previos <- nacional %>%
    filter(agno < anio_objetivo, grado == grado_obj, area == area_obj)
  previos <- previos[contexto_completo(previos), ]

  if (nrow(previos) < 100) return(vacio)

  anio_ref <- max(previos$agno)
  n_anios  <- n_distinct(previos$agno)

  # ¿Se puede estimar un intercepto aleatorio por colegio? Sólo si hay más
  # observaciones que colegios, es decir si al menos parte de los colegios
  # aparece en 2+ años. Con un solo año previo hay exactamente una fila por
  # colegio: el efecto aleatorio no es identificable y lme4 aborta. La parte
  # de contexto SÍ es estimable en ese caso (miles de colegios reparten 5
  # GSE x 6 dependencias x 2 ruralidades), así que se ajusta sólo la parte
  # fija y el desvío del colegio se calcula a mano como residuo encogido.
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
                desvio_nivel = `(Intercept)`)

  } else {

    modelo <- lm(promedio_simce ~ f_gse + f_depe + f_rural, data = previos)
    predecir_fijo <- function(nd) predict(modelo, newdata = nd)

    # Encogimiento hecho a mano, replicando lo que haría lme4 si pudiera:
    # el residuo de un colegio mezcla su desvío real con el error de
    # muestreo de su promedio (sd interna^2 / n alumnos). La proporción que
    # es señal es lambda = var_real / (var_real + var_error), y el residuo
    # se multiplica por ella. Sin esto, un colegio de 15 alumnos con un
    # promedio alto por azar arrastraría ese azar al año siguiente como si
    # fuera una característica suya.
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
      desvio_nivel = lambda * residuos
    ) %>%
      group_by(rbd_revisado) %>%
      summarise(desvio_nivel = mean(desvio_nivel), .groups = "drop")
  }

  # Centrado: predicción media de la parte fija sobre el universo nacional
  # en el año de referencia.
  nd_nacional   <- previos %>% filter(agno == anio_ref)
  base_nacional <- mean(predecir_fijo(nd_nacional))

  n_hist <- previos %>% count(rbd_revisado, name = "n_anios_nivel_hist")

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
      desvio_nivel = replace_na(desvio_nivel, 0),
      n_anios_nivel_hist   = replace_na(n_anios_nivel_hist, 0L),
      nivel_hist_colegio = contexto_nivel + desvio_nivel
    )
}

# sd interna típica de los años previos: la necesita el encogimiento
# manual de arriba para saber cuánto del residuo de un colegio es ruido.
sd_tipica_previa <- function(a, g, ar) {
  x <- simce_dist %>% filter(agno < a, grado == g, area == ar)
  if (nrow(x) == 0) NA_real_ else mean(x$sd_simce)
}

nivel_historico <- map_dfr(seq_len(nrow(anios_objetivo)), function(i) {
  a  <- anios_objetivo$agno[i]
  g  <- anios_objetivo$grado[i]
  ar <- anios_objetivo$area[i]
  cat("Efecto histórico + contexto:", a, g, ar, "...\n")
  estimar_nivel_historico(simce_nacional, colegios_universo(a, g, ar), a, g, ar,
                           sd_tipica = sd_tipica_previa(a, g, ar)) %>%
    mutate(agno = a, grado = g, area = ar)
})

# ---- 8. Dispersión histórica, con prior contextual -------------------
# Misma lógica para el ancho de la distribución interna:
#
#   sd_simce ~ factor(agno) + GSE + dependencia + ruralidad + (1 | rbd)
#
# Advertencia honesta: el contexto explica MUY poco de la dispersión
# (R² ~0.01 en el universo nacional). La sd interna baja de forma monótona
# pero suave con el GSE (42.2 puntos en GSE bajo a 37.6 en GSE alto, 4b
# matemática) y el particular pagado es el más homogéneo (37.5), pero esas
# diferencias son chicas comparadas con la variación entre colegios del
# mismo estrato. Acá el contexto sirve casi sólo como valor por defecto
# para colegios sin historia.
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

  # Mismo problema de identificabilidad que en la sección 7. Acá el
  # encogimiento manual es aún más necesario, porque la sd muestral de un
  # colegio chico es ruidosísima: su varianza de muestreo es
  # aproximadamente sd^2 / (2(n-1)).
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
  estimar_sd_historica(dist_nacional, colegios_universo(a, g, ar), a, g, ar) %>%
    mutate(agno = a, grado = g, area = ar)
})

# Respaldo para colegios que 01b encuentre en el ensayo pero que NO estén
# en el universo nacional (sin ningún SIMCE previo, o RBD que no cruza).
# Antes esos colegios entraban igual a `estimar_sd_historica` y salían con
# la sd nacional promedio de los años previos; acá ese valor se guarda
# aparte para que 01b aplique exactamente la misma regla.
respaldo_historico <- map_dfr(seq_len(nrow(anios_objetivo)), function(i) {
  a  <- anios_objetivo$agno[i]
  g  <- anios_objetivo$grado[i]
  ar <- anios_objetivo$area[i]
  previos <- dist_nacional %>% filter(agno < a, grado == g, area == ar)
  previos <- previos[contexto_completo(previos), ]
  tibble(agno = a, grado = g, area = ar,
         contexto_sd_respaldo = if (nrow(previos) < 100) NA_real_
                                else mean(previos$sd_simce))
})

# ---- 9. FORMA de la distribución interna -----------------------------
# Plantilla de cuantiles del puntaje estandarizado dentro del colegio. Se
# calcula con años estrictamente anteriores al año objetivo, y por tercil
# de nivel del colegio: en colegios de bajo rendimiento la cola derecha es
# algo más larga y en los de alto pasa lo contrario (efecto piso/techo).
# Se guarda además la versión "todos" como respaldo cuando un tercil queda
# con pocos datos.
#
# Es la pieza que reemplaza el supuesto de normalidad: en vez de asumir
# que los puntajes dentro de un colegio se distribuyen normal, se usa la
# forma empírica observada en ~6.400 colegios reales.
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

# Cortes de tercil para asignar cada colegio a una plantilla de forma en
# el momento de predecir (usando su media PREDICHA, no la observada).
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

# ---- 10. Confiabilidad del SIMCE (diagnóstico) -----------------------
# Cuánto puede, como máximo, correlacionar el ensayo con el SIMCE a nivel
# individual: la correlación observable entre dos mediciones está acotada
# por sus confiabilidades. Acá va la del SIMCE dentro del colegio,
# 1 - eem^2 / var_interna; la del índice del ensayo la calcula 01b.
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

# ---- 11. Contexto del colegio con etiquetas --------------------------
# Sirve para reportar (comparar un colegio contra su estrato GSE, por
# ejemplo) y para auditar de dónde salió su expectativa.
contexto_colegio <- contexto_rezagado %>%
  left_join(etiquetas_gse,   by = "cod_grupo") %>%
  left_join(etiquetas_depe1, by = "cod_depe1") %>%
  left_join(etiquetas_depe2, by = "cod_depe2") %>%
  left_join(etiquetas_rural, by = "cod_rural_rbd") %>%
  left_join(nivel_historico %>% select(agno, grado, area, rbd_revisado,
                                        contexto_nivel, desvio_nivel, n_anios_nivel_hist),
            by = c("agno", "grado", "area", "rbd_revisado")) %>%
  left_join(sd_historica %>% select(agno, grado, area, rbd_revisado,
                                    contexto_sd, n_anios_sd_hist),
            by = c("agno", "grado", "area", "rbd_revisado"))

# Promedio SIMCE por colegio: es el target del modelo de 02a y lo que 01b
# necesita para armar `school_model_data`.
simce_colegio <- simce_limpio %>%
  select(agno, grado, area, rbd_revisado, promedio_simce)

# ---- 12. Descriptivos que sólo dependen del SIMCE --------------------
# Se calculan acá y no en la presentación porque dependen del archivo de
# alumnos completo (~2,6 M filas). Lo que se guarda es el resumen.
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

desc_simce_anio <- simce_alumno %>%
  group_by(agno) %>%
  summarise(
    n_colegios = n_distinct(rbd_revisado),
    n_alumnos  = n_distinct(idalumno),
    n_puntajes = n(),                      # alumno x área
    .groups = "drop"
  )

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

# Un colegio de ejemplo: para mostrar en concreto que dentro de un solo
# colegio hay mucha variación se elige el de más alumnos del grupo de
# referencia.
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

# SIMCE promedio por GSE, sobre el universo nacional, que es donde hay
# variación en GSE.
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

# Composición por GSE del universo nacional. 01b le agrega la parte
# Santillana, que es la que necesita los ensayos.
comp_gse_nacional <- simce_nacional %>%
  filter(agno == max(agno), grado %in% GRADOS_MODELO, area == "matematica") %>%
  left_join(etiquetas_gse, by = "cod_grupo") %>%
  distinct(grado, rbd_revisado, gse_etiqueta) %>%
  count(grado, gse_etiqueta, name = "n") %>%
  mutate(fuente = "nacional")

descriptivos_simce <- list(
  desc_simce_alu    = desc_simce_alu,
  desc_sd_interna   = desc_sd_interna,
  desc_simce_anio   = desc_simce_anio,
  dens_simce_alu    = dens_simce_alu,
  varianza_simce    = varianza_simce,
  colegio_ejemplo   = colegio_ejemplo,
  simce_por_gse     = simce_por_gse,
  comp_gse_nacional = comp_gse_nacional
)

# ---- 13. Guardar -----------------------------------------------------
# Todo en una sola lista. Ver el encabezado para qué es cada elemento.
salida_simce <- list(
  simce_alumno       = simce_alumno,
  simce_dist         = simce_dist,
  simce_colegio      = simce_colegio,
  limites_simce      = limites_simce,
  contexto_rezagado  = contexto_rezagado,
  contexto_colegio   = contexto_colegio,
  nivel_historico    = nivel_historico,
  sd_historica       = sd_historica,
  respaldo_historico = respaldo_historico,
  forma_z            = forma_z,
  cortes_tercil      = cortes_tercil,
  conf_simce         = conf_simce,
  descriptivos       = descriptivos_simce,
  anios_horizonte    = anios_horizonte
)

saveRDS(salida_simce, dir_salidas %>% file.path("salida_01a_simce.rds"))

cat("\nListo. Salida en", dir_salidas %>% file.path("salida_01a_simce.rds"), "\n")
cat("Es una lista con:", paste(names(salida_simce), collapse = ", "), "\n")
cat("Años objetivo cubiertos:", paste(range(anios_horizonte), collapse = " a "), "\n")
cat("Siguiente paso: 01b_insumos_ensayo.R\n")
