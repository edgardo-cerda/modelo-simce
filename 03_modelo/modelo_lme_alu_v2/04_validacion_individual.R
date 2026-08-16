# =============================================================
# 04_validacion_individual.R   (NUEVO)
# -------------------------------------------------------------
# ¿Cómo se valida una predicción individual si no existe un vínculo
# alumno-a-alumno entre el ensayo Santillana y el SIMCE?
#
# No se puede evaluar estudiante por estudiante. Pero sí se puede
# evaluar algo casi tan útil, y que la v2 no podía evaluar en
# absoluto: si el CONJUNTO de predicciones de un colegio reproduce
# el conjunto de puntajes que ese colegio efectivamente obtuvo.
# Es decir, comparar distribución predicha vs. distribución
# observada, colegio por colegio.
#
# Se hace con dos métricas:
#
#   1) ERROR DE CUANTILES: se comparan los percentiles 5, 10, ..., 95
#      de las predicciones contra los mismos percentiles observados,
#      y se promedia el error absoluto. Si da 10 puntos, significa
#      que la distribución predicha está corrida ~10 puntos respecto
#      de la real en un percentil típico. Es la distancia de
#      Wasserstein-1 discretizada; captura errores de nivel, de ancho
#      y de forma en un solo número.
#
#   2) ERROR INDIVIDUAL BAJO RANKING PERFECTO: se le asigna a cada
#      estudiante el puntaje observado que corresponde a su percentil
#      dentro del colegio, y se mide el error contra su predicción.
#      OJO: esto supone que el ensayo ordena perfectamente a los
#      estudiantes, cosa que es falsa. Por lo tanto es una COTA
#      OPTIMISTA del error individual real, no una estimación de él.
#      Se reporta porque acota por abajo lo que se le puede prometer
#      a un colegio.
#
# Todo se compara contra dos referencias:
#   - v2 legado: aplicar los coeficientes del colegio al estudiante.
#   - "sólo la media": darle a todos los estudiantes del colegio el
#     mismo puntaje predicho (lo que en la práctica se podía ofrecer
#     antes de tener el archivo de alumnos).
#
# La validación es out-of-time: los modelos de 02 y 02b se entrenaron
# excluyendo este año, y la forma de la distribución y la dispersión
# histórica se calcularon en 01 con ventana expansiva.
#
# -------------------------------------------------------------
# NUEVO v10: VALIDACIÓN POR SEXO (sección 3c)
# -------------------------------------------------------------
# Las dos métricas de arriba tienen un punto ciego: son insensibles a la
# ASIGNACIÓN. En la versión por percentil, el conjunto de puntajes
# predichos de un colegio queda fijado por (media, ancho, forma), así que
# reordenar a los estudiantes entre sí no cambia ni la distribución
# predicha ni el error de cuantiles. Hasta la v9 ninguna métrica del
# proyecto podía decir si el modelo repartía bien DENTRO del colegio.
#
# El sexo rompe ese punto ciego. Es el primer atributo individual presente
# en las dos fuentes a la vez: estimado del nombre en el ensayo,
# administrativo en el SIMCE. Partiendo cada colegio en dos grupos se
# puede preguntar si la distribución predicha para cada uno reproduce la
# observada — una prueba de la asignación, y de paso una revisión de
# equidad de un producto que entrega puntajes por estudiante.
#
# QUÉ ENCONTRÓ, que es la razón de dejarla corriendo en cada ronda: el
# modelo SUBESTIMA LA VENTAJA MASCULINA EN MATEMÁTICA. La brecha observada
# entre hombres y mujeres es de 13.3 puntos en 2m y 16.4 en 4b, y el
# modelo predice 5.7 y 11.1: faltan 7.7 y 5.2 puntos. En lenguaje la
# brecha se reproduce bien (falta -0.7 en 2m, -2.5 en 4b).
#
# Y POR QUÉ EL SEXO IGUAL NO ENTRA COMO PREDICTOR: corregir esa brecha
# —desplazando cada sexo media brecha faltante, que es la cota superior de
# lo que un ajuste podría lograr— mejora el error de cuantiles en 0.00,
# 0.21, 0.03 y 0.10 puntos, sobre errores de 11 a 18. Es una a dos
# centésimas del error. La brecha ya viaja dentro del theta (se verificó
# que atraviesa el pipeline intacta), así que agregar el sexo sería
# contarla dos veces sobre la parte que el ensayo ya mide, a cambio de
# nada medible.
#
# La sección recalcula ambas cosas en cada corrida: si en algún momento la
# ganancia dejara de ser de décimas, la decisión se puede revisar con
# datos frescos en vez de con este comentario.
#
# -------------------------------------------------------------
# TAMBIÉN SE PROBÓ: PERCENTIL CONDICIONAL AL SEXO (y por qué falló)
# -------------------------------------------------------------
# El desplazamiento de arriba corrige sólo la media. La alternativa más
# general es calcular la posición del estudiante DENTRO DE SU SEXO y
# mapearla a una distribución del colegio desplazada y reescalada para ese
# sexo — así se corrigen también dispersión y forma, no sólo el nivel. Es
# una propuesta mejor y hubo que medirla aparte.
#
# Estimando los parámetros y evaluándolos EN EL MISMO AÑO parecía servir:
# ganaba 0.18, 0.44, 0.23 y 0.38 puntos, en los cuatro grupos. Dos
# controles la desarmaron:
#
#   1. OUT-OF-TIME. Estimando el desplazamiento y la escala con los años
#      anteriores y aplicándolos al año de prueba, la ganancia cae a 0.08,
#      0.73, 0.06 y -0.54. Es NEGATIVA justo en 4b matemática, que es donde
#      la brecha bruta era mayor: el parámetro no transfiere entre años.
#      Y con razón, porque la brecha faltante se mueve harto de un año a
#      otro (-2.3, +1.7, +3.1 en 4b matemática).
#
#   2. PLACEBO. El mismo procedimiento sobre un grupo ALEATORIO en vez del
#      sexo gana MÁS: entre 0.97 y 2.14 puntos en cinco sorteos, siempre
#      por encima del sexo en los cuatro grupos.
#
# El placebo revela el mecanismo: re-rankear dentro de CUALQUIER subgrupo
# hace que la sub-distribución predicha vuelva a cubrir todo el rango, igual
# que la observada. Comparar un subconjunto del rango predicho contra el
# rango completo del observado penaliza a la regla agrupada por
# construcción. O sea que la ganancia aparente era del método —partir en dos
# y re-rankear— y no de la variable. El sexo queda por debajo del azar
# porque además arrastra un parámetro estimado que no transfiere.
#
# MORALEJA, que vale más allá del sexo: cualquier regla de asignación que
# re-rankee dentro de subgrupos tiene que compararse contra un placebo
# aleatorio antes de creerle. La métrica de cuantiles por subgrupo la
# favorece mecánicamente.
# =============================================================

library(tidyverse)

# ---- 0. Configuración --------------------------------------------
usuario <- Sys.info()[["user"]]
rutas <- config::get(config = usuario, file = "config.yml")
ruta_outputs <- rutas$ruta_outputs
dir_salidas <- ruta_outputs %>% file.path('modelo_lme_alu_v2')

MIN_ALU_VALID <- 15
QS <- seq(0.05, 0.95, by = 0.05)

pred_individual <- dir_salidas %>% file.path("predicciones_individual.rds") %>% readRDS()
simce_alumno    <- dir_salidas %>% file.path("simce_alumno.rds") %>% readRDS()
simce_dist      <- dir_salidas %>% file.path("simce_dist.rds") %>% readRDS()

anio_val <- max(pred_individual$agno)
cat("Validando predicciones individuales del año:", anio_val, "\n")

obs <- simce_alumno %>% filter(agno == anio_val)
if (nrow(obs) == 0) {
  stop("No hay SIMCE individual observado para ", anio_val,
       ": la validación sólo corre sobre un año ya cerrado.")
}

# ---- 1. Cuantiles observados y predichos por colegio ----------------
cuantiles <- function(x) list(quantile(x, QS, names = FALSE, na.rm = TRUE))

obs_q <- obs %>%
  group_by(grado, area, rbd_revisado) %>%
  filter(n() >= MIN_ALU_VALID) %>%
  summarise(
    n_obs   = n(),
    media_obs = mean(ptje),
    sd_obs  = sd(ptje),
    q_obs   = cuantiles(ptje),
    .groups = "drop"
  )

pred_q <- pred_individual %>%
  group_by(grado, area, rbd_revisado) %>%
  filter(n() >= 10) %>%
  summarise(
    n_pred        = n(),
    pred_colegio  = first(pred_simce_colegio),
    sd_predicha   = first(sd_simce_pred),
    q_B           = cuantiles(pred_B),
    q_v2          = cuantiles(pred_v2_legado),
    sd_B          = sd(pred_B, na.rm = TRUE),
    sd_v2         = sd(pred_v2_legado, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!is.na(pred_colegio))

comp <- pred_q %>%
  inner_join(obs_q, by = c("grado", "area", "rbd_revisado")) %>%
  mutate(
    q_solo_media = map(pred_colegio, ~ rep(.x, length(QS))),
    qmae_B          = map2_dbl(q_B, q_obs, ~ mean(abs(.x - .y))),
    qmae_v2         = map2_dbl(q_v2, q_obs, ~ mean(abs(.x - .y))),
    qmae_solo_media = map2_dbl(q_solo_media, q_obs, ~ mean(abs(.x - .y))),
    error_sd_B      = sd_B - sd_obs,
    error_sd_v2     = sd_v2 - sd_obs,
    error_media     = pred_colegio - media_obs
  )

# ---- 1b. Oráculo: la misma versión B pero con la media y la sd
# OBSERVADAS del colegio. No es una predicción (usa información que no
# existe al momento de predecir): sirve para separar cuánto del error
# viene de predecir el colegio y cuánto de repartir hacia los estudiantes.
# Si esta columna da un error mucho más bajo, el problema está en el
# modelo de colegio, no en la plantilla de forma.
if ("z_forma" %in% names(pred_individual)) {

  q_oraculo <- pred_individual %>%
    inner_join(obs_q %>% select(grado, area, rbd_revisado, media_obs, sd_obs),
               by = c("grado", "area", "rbd_revisado")) %>%
    mutate(pred_B_oraculo = media_obs + sd_obs * z_forma) %>%
    group_by(grado, area, rbd_revisado) %>%
    summarise(q_oraculo = cuantiles(pred_B_oraculo), .groups = "drop")

  comp <- comp %>%
    left_join(q_oraculo, by = c("grado", "area", "rbd_revisado")) %>%
    mutate(qmae_B_oraculo = map2_dbl(q_oraculo, q_obs,
                                     ~ if (is.null(.x)) NA_real_ else mean(abs(.x - .y))))
} else {
  warning("pred_individual no trae z_forma: no se puede calcular el oráculo.")
  comp$qmae_B_oraculo <- NA_real_
}

cat("\nColegios validados:", nrow(comp), "\n\n")

resumen_dist <- comp %>%
  group_by(grado, area) %>%
  summarise(
    n_colegios       = n(),
    qmae_B           = mean(qmae_B),
    qmae_v2_legado   = mean(qmae_v2),
    qmae_solo_media  = mean(qmae_solo_media),
    qmae_B_oraculo   = mean(qmae_B_oraculo, na.rm = TRUE),
    sd_observada     = mean(sd_obs),
    sd_predicha      = mean(sd_predicha),
    sd_efectiva_B    = mean(sd_B),
    sd_efectiva_v2   = mean(sd_v2),
    error_abs_media  = mean(abs(error_media)),
    .groups = "drop"
  )

cat("ERROR DE CUANTILES (puntos SIMCE; más bajo es mejor):\n")
print(resumen_dist %>% mutate(across(where(is.numeric), ~round(.x, 1))))
cat("\nLectura: qmae_solo_media es lo que se lograba dándole a todos los\n",
    "estudiantes del colegio el mismo puntaje. La diferencia contra qmae_B\n",
    "es exactamente lo que aporta modelar la dispersión y la forma.\n\n")

# ---- 2. Error individual bajo ranking perfecto (cota optimista) ------
# A cada estudiante se le asigna el puntaje observado de su mismo
# percentil dentro del colegio.
emparejar_por_rango <- function(datos_colegio, puntajes_obs) {
  datos_colegio$obs_emparejado <- quantile(
    puntajes_obs, probs = datos_colegio$pct_ensayo, names = FALSE, na.rm = TRUE
  )
  datos_colegio
}

obs_por_colegio <- obs %>%
  group_by(grado, area, rbd_revisado) %>%
  filter(n() >= MIN_ALU_VALID) %>%
  summarise(puntajes = list(ptje), .groups = "drop")

emparejado <- pred_individual %>%
  inner_join(obs_por_colegio, by = c("grado", "area", "rbd_revisado")) %>%
  group_by(grado, area, rbd_revisado) %>%
  group_modify(function(.x, .y) emparejar_por_rango(.x, .x$puntajes[[1]])) %>%
  ungroup() %>%
  select(-puntajes)

resumen_ind <- emparejado %>%
  group_by(grado, area) %>%
  summarise(
    n_estudiantes   = n(),
    mae_B           = mean(abs(pred_B - obs_emparejado), na.rm = TRUE),
    mae_v2_legado   = mean(abs(pred_v2_legado - obs_emparejado), na.rm = TRUE),
    mae_solo_media  = mean(abs(pred_simce_colegio - obs_emparejado), na.rm = TRUE),
    .groups = "drop"
  )

cat("ERROR INDIVIDUAL BAJO RANKING PERFECTO (cota optimista, ver encabezado):\n")
print(resumen_ind %>% mutate(across(where(is.numeric), ~round(.x, 2))))
cat("\nRecordatorio: el emparejamiento supone que el ensayo ordena perfecto a\n",
    "los estudiantes. El error individual REAL es mayor que éste.\n\n")

# SECCIÓN 3 (sensibilidad a rho): ELIMINADA EN v9 -----------------------
# Movía rho entre 0.6 y 1.0 y reportaba MAE, cobertura y ancho del rango.
# No podía concluir nada por construcción: sus dos métricas empujan en
# direcciones opuestas y la del error emparejado favorece rho=1
# mecánicamente (supone ranking perfecto). Con la versión A fuera del
# pipeline (ver 03), rho ya no existe.

# ---- 3b. Desglose por contexto ---------------------------------------
# ¿El modelo funciona parejo entre estratos, o sólo en los que dominan
# la base Santillana (particular pagado, GSE alto)? Si el error es
# claramente peor en un estrato con pocos colegios, conviene decirlo
# antes de que el reporte llegue a ese colegio.
por_estrato <- comp %>%
  left_join(
    pred_individual %>%
      distinct(grado, area, rbd_revisado, gse_etiqueta, depe2_etiqueta),
    by = c("grado", "area", "rbd_revisado")
  ) %>%
  group_by(grado, gse_etiqueta) %>%
  filter(n() >= 5) %>%
  summarise(n_colegios = n(),
            qmae_B = mean(qmae_B),
            error_abs_media = mean(abs(error_media)),
            sd_obs = mean(sd_obs),
            .groups = "drop")

cat("\nERROR POR ESTRATO GSE (ojo con los estratos de pocos colegios):\n")
print(por_estrato %>% mutate(across(where(is.numeric), ~round(.x, 1))))

# ---- 3c. VALIDACIÓN POR SEXO (NUEVO v10) -----------------------------
# Lo que esta sección habilita, y por qué es distinta de todo lo anterior.
#
# La validación de la sección 1 compara la distribución PREDICHA de un
# colegio contra la OBSERVADA. Es insensible a quién es quién: en la
# versión por percentil el conjunto de puntajes predichos de un colegio
# queda fijado por (media, ancho, forma), así que reordenar a los
# estudiantes entre sí no cambia esa distribución ni una décima. Dicho de
# otro modo: hasta ahora NINGUNA métrica del proyecto podía decir si el
# modelo estaba asignando bien los puntajes DENTRO del colegio.
#
# El sexo cambia eso. Es el primer atributo individual presente en las dos
# fuentes a la vez —estimado del nombre en el ensayo (01), administrativo
# en el SIMCE (01_cargar_y_consolidar_simce.R)— así que permite partir cada
# colegio en dos grupos y preguntar si la distribución predicha para cada
# uno reproduce la observada. Es una prueba de la ASIGNACIÓN, no sólo del
# agregado, y de paso una revisión de equidad de un producto que entrega
# puntajes por estudiante.
#
# Los dos sexos NO están enlazados alumno a alumno (siguen siendo
# poblaciones distintas: quienes rinden el ensayo no son exactamente
# quienes rinden el SIMCE), así que esto compara distribuciones por grupo,
# no personas.
#
# CÓMO LEER LA SALIDA:
#   sesgo_H, sesgo_M   predicho menos observado en cada grupo. Arrastran el
#                      sesgo general de deriva entre años, así que no se
#                      leen solos.
#   brecha_predicha    (media H - media M) según el modelo
#   brecha_observada   (media H - media M) en el SIMCE real
#   falta              observada menos predicha. ES EL NÚMERO QUE IMPORTA:
#                      cuánto de la brecha real NO llega a la predicción.
#                      Positivo = el modelo subestima la ventaja masculina.
MIN_POR_SEXO <- 8   # mínimo de alumnos de cada sexo, en cada lado

if (!"sexo" %in% names(pred_individual) || !"sexo" %in% names(obs)) {

  warning("Falta la columna `sexo` en pred_individual o en simce_alumno: ",
          "se omite la validación por sexo. Hay que volver a correr 01 (v10).")
  val_sexo <- NULL

} else {

  obs_sexo <- obs %>%
    filter(!is.na(sexo)) %>%
    group_by(grado, area, rbd_revisado, sexo) %>%
    filter(n() >= MIN_POR_SEXO) %>%
    summarise(n_obs = n(), media_obs = mean(ptje), q_obs = cuantiles(ptje),
              .groups = "drop")

  pred_sexo <- pred_individual %>%
    filter(!is.na(sexo)) %>%
    group_by(grado, area, rbd_revisado, sexo) %>%
    filter(n() >= MIN_POR_SEXO) %>%
    summarise(n_pred = n(), media_pred = mean(pred_B), q_pred = cuantiles(pred_B),
              .groups = "drop")

  # Sólo colegios donde AMBOS sexos superan el mínimo en ambos lados: si
  # uno de los grupos entra sólo por un lado, la brecha no es comparable.
  val_sexo <- inner_join(pred_sexo, obs_sexo,
                         by = c("grado", "area", "rbd_revisado", "sexo")) %>%
    group_by(grado, area, rbd_revisado) %>%
    filter(n_distinct(sexo) == 2) %>%
    ungroup() %>%
    mutate(qmae = map2_dbl(q_pred, q_obs, ~ mean(abs(.x - .y))),
           sesgo = media_pred - media_obs)

  resumen_sexo <- val_sexo %>%
    group_by(grado, area, sexo) %>%
    summarise(n_colegios = n(), qmae = mean(qmae),
              media_pred = mean(media_pred), media_obs = mean(media_obs),
              .groups = "drop") %>%
    pivot_wider(names_from = sexo,
                values_from = c(n_colegios, qmae, media_pred, media_obs)) %>%
    transmute(
      grado, area, n_colegios = n_colegios_hombre,
      qmae_H = qmae_hombre, qmae_M = qmae_mujer,
      brecha_predicha = media_pred_hombre - media_pred_mujer,
      brecha_observada = media_obs_hombre - media_obs_mujer,
      falta = brecha_observada - brecha_predicha
    )

  cat("\n\nCALIBRACIÓN POR SEXO (puntos SIMCE; el año de validación):\n")
  print(resumen_sexo %>% mutate(across(where(is.numeric), ~round(.x, 2))) %>%
          as.data.frame())
  cat("\n`falta` positivo = el modelo subestima la ventaja masculina en ese grupo.\n")

  # --- ¿Cuánto se ganaría corrigiendo? ---------------------------------
  # Se desplaza cada sexo por media brecha faltante —lo máximo que podría
  # aportar un ajuste por sexo— y se recalcula el error de cuantiles. Es
  # una COTA SUPERIOR de la ganancia: supone que la corrección es exacta y
  # que el sexo estimado no tiene error. Se reporta en cada corrida porque
  # es el número que decide si vale la pena meter el sexo al modelo, y
  # conviene que la decisión se pueda revisar con datos frescos.
  ganancia_sexo <- val_sexo %>%
    left_join(resumen_sexo %>% select(grado, area, falta), by = c("grado", "area")) %>%
    mutate(
      desplazamiento = if_else(sexo == "hombre", falta / 2, -falta / 2),
      qmae_ajustado = map2_dbl(map2(q_pred, desplazamiento, ~ .x + .y), q_obs,
                               ~ mean(abs(.x - .y)))
    ) %>%
    group_by(grado, area) %>%
    summarise(qmae_actual = mean(qmae), qmae_con_ajuste = mean(qmae_ajustado),
              ganancia = mean(qmae) - mean(qmae_ajustado), .groups = "drop")

  cat("\nCOTA SUPERIOR de lo que aportaría ajustar por sexo:\n")
  print(ganancia_sexo %>% mutate(across(where(is.numeric), ~round(.x, 2))) %>%
          as.data.frame())
  cat("\nSi la ganancia es de décimas sobre un error de dos dígitos, el sexo\n",
      "no se justifica como predictor: la brecha ya viaja dentro del theta.\n")
}

# ---- 4. Gráficos ------------------------------------------------------
# (i) Distribución predicha vs. observada, agrupando todos los colegios.
dens_data <- bind_rows(
  emparejado %>% transmute(grado, area, valor = pred_B, fuente = "Predicción (percentil)"),
  emparejado %>% transmute(grado, area, valor = pred_v2_legado, fuente = "v2 legado"),
  obs %>% inner_join(distinct(pred_individual, grado, area, rbd_revisado),
                     by = c("grado", "area", "rbd_revisado")) %>%
    transmute(grado, area, valor = ptje, fuente = "Observado")
)

p1 <- ggplot(dens_data, aes(valor, color = fuente)) +
  geom_density(linewidth = 0.7) +
  facet_wrap(~ grado + area, scales = "free") +
  labs(title = paste("Distribución de puntajes individuales -", anio_val),
       subtitle = "Colegios con ensayo Santillana",
       x = "Puntaje SIMCE", y = "Densidad", color = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(dir_salidas %>% file.path("validacion_distribucion_individual.png"),
       p1, width = 10, height = 7)

# (ii) Dispersión predicha vs. observada por colegio.
p2 <- comp %>%
  select(grado, area, sd_obs, `Predicción` = sd_B, `v2 legado` = sd_v2) %>%
  pivot_longer(c(`Predicción`, `v2 legado`), names_to = "fuente", values_to = "sd_pred") %>%
  ggplot(aes(sd_obs, sd_pred, color = fuente)) +
  geom_point(alpha = 0.4) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  facet_wrap(~ grado + area, scales = "free") +
  labs(title = "Dispersión interna: predicha vs. observada",
       x = "sd observada entre estudiantes", y = "sd de las predicciones", color = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(dir_salidas %>% file.path("validacion_dispersion_individual.png"),
       p2, width = 10, height = 7)

# ---- 4b. Densidades resumidas para la presentación (NUEVO) -----------
# El gráfico de distribución predicha vs. observada se arma acá y no en
# 05 porque `emparejado` y `obs` tienen millones de filas. Se guarda la
# curva ya evaluada en una grilla de 256 puntos por serie: unas pocas
# miles de filas en total.
dens_validacion <- dens_data %>%
  filter(fuente %in% c("Observado", "Predicción (percentil)")) %>%
  filter(!is.na(valor)) %>%
  group_by(grado, area, fuente) %>%
  group_modify(~ {
    d <- density(.x$valor, n = 256)
    tibble(x = d$x, y = d$y)
  }) %>%
  ungroup()

# ---- 4c. Distribución por Niveles de Aprendizaje (NUEVO) -------------
# Traduce puntajes a las categorías que los colegios efectivamente usan
# (Insuficiente / Elemental / Adecuado) y compara la distribución real
# con la que predice el modelo. Es la lectura más directa de si la
# predicción sirve para la pregunta "¿cuántos de mis estudiantes
# quedarían en cada nivel?".
#
# ¡IMPORTANTE! Los puntajes de corte son fijados por la Agencia de
# Calidad de la Educación en los Estándares de Aprendizaje, NO se
# estiman de los datos. Los valores de abajo provienen de los documentos
# oficiales del Mineduc:
#
#   4° básico Lectura     Elemental >= 241, Adecuado >= 284
#   4° básico Matemática  Elemental >= 245, Adecuado >= 295
#   2° medio  Lectura     Elemental >= 250, Adecuado >= 295
#   2° medio  Matemática  Elemental >= 252, Adecuado >= 319
#
# Los tres primeros están verificados contra los Estándares publicados
# en curriculumnacional.cl; el de 2° medio Matemática viene de una copia
# del mismo documento y conviene confirmarlo antes de publicar cifras.
# Además, los Estándares de 2° medio figuran como "no vigentes" en el
# portal: si la Agencia actualizó los cortes para los años de estos
# datos, hay que reemplazarlos acá. Un corte equivocado no rompe nada,
# simplemente clasifica mal y en silencio.
CORTES_NIVEL <- tribble(
  ~grado, ~area,        ~corte_elemental, ~corte_adecuado,
  "4b",   "lenguaje",   241,              284,
  "4b",   "matematica", 245,              295,
  "2m",   "lenguaje",   250,              295,
  "2m",   "matematica", 252,              319
)

clasificar_nivel <- function(datos, columna_puntaje) {
  datos %>%
    left_join(CORTES_NIVEL, by = c("grado", "area")) %>%
    mutate(
      nivel = case_when(
        is.na(corte_elemental) | is.na({{ columna_puntaje }}) ~ NA_character_,
        {{ columna_puntaje }} >= corte_adecuado               ~ "Adecuado",
        {{ columna_puntaje }} >= corte_elemental              ~ "Elemental",
        TRUE                                                 ~ "Insuficiente"
      ),
      nivel = factor(nivel, levels = c("Insuficiente", "Elemental", "Adecuado"))
    )
}

# Se usa el MISMO universo que el resto de la validación: sólo los
# colegios que quedaron en `comp` (con predicción y con al menos
# MIN_ALU_VALID alumnos observados). Comparar la distribución observada
# de todos los colegios contra la predicha de un subconjunto mezclaría
# el error del modelo con una diferencia de composición.
colegios_validados <- comp %>% distinct(grado, area, rbd_revisado)

niveles_obs <- obs %>%
  inner_join(colegios_validados, by = c("grado", "area", "rbd_revisado")) %>%
  clasificar_nivel(ptje) %>%
  filter(!is.na(nivel)) %>%
  count(grado, area, nivel, name = "n_obs") %>%
  group_by(grado, area) %>%
  mutate(pct_obs = n_obs / sum(n_obs)) %>%
  ungroup()

niveles_pred <- pred_individual %>%
  inner_join(colegios_validados, by = c("grado", "area", "rbd_revisado")) %>%
  clasificar_nivel(pred_B) %>%
  filter(!is.na(nivel)) %>%
  count(grado, area, nivel, name = "n_pred") %>%
  group_by(grado, area) %>%
  mutate(pct_pred = n_pred / sum(n_pred)) %>%
  ungroup()

niveles_logro <- niveles_obs %>%
  full_join(niveles_pred, by = c("grado", "area", "nivel")) %>%
  mutate(across(c(n_obs, n_pred), ~ replace_na(.x, 0L)),
         across(c(pct_obs, pct_pred), ~ replace_na(.x, 0)),
         dif_pp = 100 * (pct_pred - pct_obs)) %>%
  arrange(grado, area, nivel)

cat("DISTRIBUCIÓN POR NIVEL DE APRENDIZAJE (observada vs. predicha):\n")
print(niveles_logro %>%
        transmute(grado, area, nivel,
                  `% real` = round(100 * pct_obs, 1),
                  `% modelo` = round(100 * pct_pred, 1),
                  `dif (pp)` = round(dif_pp, 1)))
cat("\nError absoluto medio de la composición, en puntos porcentuales:\n")
print(niveles_logro %>%
        group_by(grado, area) %>%
        summarise(error_pp = mean(abs(dif_pp)), .groups = "drop") %>%
        mutate(error_pp = round(error_pp, 1)))
cat("\nRecordatorio: los cortes son los oficiales de los Estándares de\n",
    "Aprendizaje, no salen de estos datos. Ver el encabezado de esta sección.\n\n")

# ---- 5. Guardar ------------------------------------------------------
saveRDS(niveles_logro,   dir_salidas %>% file.path("niveles_logro.rds"))      # lo usa 05
write_csv(niveles_logro, dir_salidas %>% file.path("validacion_niveles_logro.csv"))
saveRDS(dens_validacion, dir_salidas %>% file.path("dens_validacion.rds"))   # lo usa 05
saveRDS(comp %>% select(-starts_with("q_")),
        dir_salidas %>% file.path("validacion_por_colegio.rds"))             # lo usa 05
write_csv(comp %>% select(-starts_with("q_")),
          dir_salidas %>% file.path("validacion_por_colegio.csv"))
write_csv(resumen_dist, dir_salidas %>% file.path("validacion_distribucional.csv"))
write_csv(resumen_ind,  dir_salidas %>% file.path("validacion_individual.csv"))
write_csv(por_estrato,  dir_salidas %>% file.path("validacion_por_estrato.csv"))
if (!is.null(val_sexo)) {
  saveRDS(resumen_sexo,   dir_salidas %>% file.path("validacion_sexo.rds"))
  write_csv(resumen_sexo, dir_salidas %>% file.path("validacion_sexo.csv"))
  write_csv(ganancia_sexo, dir_salidas %>% file.path("validacion_sexo_ganancia.csv"))
}

cat("\nListo. Resultados en ", dir_salidas, ":\n",
    " - validacion_distribucional.csv / validacion_individual.csv\n",
    " - validacion_por_colegio.csv / validacion_por_estrato.csv\n",
    " - validacion_distribucion_individual.png / validacion_dispersion_individual.png\n")
