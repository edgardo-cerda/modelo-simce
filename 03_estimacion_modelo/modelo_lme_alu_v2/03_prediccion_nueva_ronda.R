# =============================================================
# 03_prediccion_nueva_ronda.R
# -------------------------------------------------------------
# Aplica los modelos de 02 (nivel) y 02b (dispersión) para predecir
# la "próxima ronda" a nivel de colegio y de estudiante.
#
# La predicción individual se construye en dos piezas separadas, cada
# una estimada donde sí hay evidencia:
#
#     puntaje_estudiante = MEDIA del colegio (modelo 02)
#                        + ANCHO del colegio (modelo 02b)
#                          x POSICIÓN del estudiante dentro de su colegio
#
# --- MATCHING POR PERCENTIL ----------------------------------------
#   pred_B = mu_hat + sd_hat * Q(percentil del estudiante)
#
#   donde Q es la forma empírica de la distribución interna de puntajes
#   observada en miles de colegios reales (calculada en 01, por grado,
#   área y tercil de nivel; no asume normalidad). A cada estudiante se
#   le asigna el puntaje que corresponde a su percentil dentro del
#   colegio.
#
#   Por construcción, el conjunto de predicciones de un colegio
#   reproduce la distribución que ese colegio debería tener: media,
#   ancho y forma, incluidas las colas. Responde las preguntas que un
#   colegio hace de verdad — "¿cómo se va a ver la distribución de mi
#   curso?", "¿cuántos estudiantes quedarían en nivel insuficiente?",
#   "¿quiénes están en riesgo?".
#
#   El supuesto que hace, y conviene decirlo: que el ensayo ordena bien
#   a los estudiantes. No los ordena perfecto, así que las predicciones
#   individuales son puntualmente menos precisas de lo que su dispersión
#   sugiere. Lo que sí reproduce bien es el CONJUNTO.
#
# No necesita ningún parámetro libre: la forma de la distribución se
# OBSERVA en ~6.400 colegios reales en vez de suponerse. Todo lo que el
# script entrega es verificable contra datos.
#
# La predicción no puede validarse estudiante por estudiante —no existe
# vínculo alumno-a-alumno entre el ensayo y el SIMCE— pero sí a nivel de
# distribución: ver 04_validacion_individual.R.
#
# -------------------------------------------------------------
# CÓMO SE CORRE UNA RONDA NUEVA
# -------------------------------------------------------------
# El pipeline es el mismo de siempre —00, 01, 02, 02b, 03, 04— y se adapta
# solo. No hay modo, flag ni parámetro que cambiar:
#
#   1. Cargar los ensayos del año nuevo y correr 00 y 01. El año nuevo
#      aparece en `school_features` pero NO en `school_model_data`, porque
#      todavía no tiene SIMCE con que cruzarse.
#   2. 02 y 02b ajustan DOS juegos de modelos: los de validación (que
#      excluyen el último año cerrado, y de los que salen las métricas) y
#      los de producción (que usan todos los años cerrados).
#   3. Este script detecta si el año objetivo tiene SIMCE y elige el juego
#      que no lo vio. Con una ronda nueva, ésos son los de producción.
#   4. 04 no encuentra SIMCE del año nuevo, avisa y termina bien. Las
#      métricas siguen siendo las del último año cerrado.
#
# UN SOLO ENSAYO EN EL AÑO NUEVO: probado de punta a punta y funciona. La
# calibración de 00 admite una forma única (no hay nada que enlazar, así
# que el chequeo de conectividad pasa trivialmente) y el índice individual
# tolera k = 1 con confiabilidad ~0.85-0.93, porque `se_theta` sale de los
# ítems efectivamente rendidos y una forma de 35-45 ítems mide razonable.
#
# LO QUE SÍ HAY QUE MIRAR en ese caso: el aviso de comparabilidad del banco
# que imprime 01. `mean_logro` es el porcentaje del banco de ítems del año,
# y con una forma ese banco es de ~40 ítems en vez de ~240. Si el nivel
# implícito se corre respecto de los años anteriores, el modelo —que usa
# `mean_logro_enc` EN NIVELES— lo lee como cambio real de los colegios. Es
# la misma deriva entre años que 02 reporta como pendiente, amplificada.
# =============================================================

library(tidyverse)

# ---- 0. Configuración --------------------------------------------
usuario <- Sys.info()[["user"]]
rutas <- config::get(config = usuario, file = "config.yml")
ruta_outputs <- rutas$ruta_outputs
dir_salidas <- ruta_outputs %>% file.path('modelo_lme_alu_v2')

# ---- 1. Insumos ---------------------------------------------------
ind_features    <- dir_salidas %>% file.path("ind_features.rds") %>% readRDS()
school_features <- dir_salidas %>% file.path("school_features.rds") %>% readRDS()
forma_z         <- dir_salidas %>% file.path("forma_z.rds") %>% readRDS()
cortes_tercil   <- dir_salidas %>% file.path("cortes_tercil.rds") %>% readRDS()
limites_simce   <- dir_salidas %>% file.path("limites_simce.rds") %>% readRDS()
anios_cerrados  <- dir_salidas %>% file.path("anios_cerrados.rds") %>% readRDS()

anio_prediccion <- max(school_features$agno)

# ---- 1b. Qué juego de modelos corresponde ---------------------------
# Una sola regla: SE USA EL MODELO QUE NO VIO EL AÑO QUE SE PREDICE.
#
#   - Año objetivo SIN SIMCE (una ronda nueva, p.ej. 2026): ningún modelo
#     pudo verlo, así que corresponde el de PRODUCCIÓN, ajustado con todos
#     los años cerrados. Desperdiciar el año más reciente ahí no tendría
#     sentido: es el más informativo y no hay nada que contaminar.
#
#   - Año objetivo CERRADO (todavía no llegan los ensayos de la ronda
#     nueva): el de producción lo tuvo en su entrenamiento, así que
#     predecirlo con él sería dentro de muestra. Corresponde el de
#     VALIDACIÓN, que lo dejó fuera.
#
# En los dos casos la predicción es fuera de muestra, que es lo que hace
# que las métricas reportadas describan de verdad lo que se le entrega al
# colegio. No hay switch: lo decide qué años traen SIMCE.
es_ronda_nueva <- !(anio_prediccion %in% anios_cerrados)

if (es_ronda_nueva) {
  modelos    <- dir_salidas %>% file.path("modelos_escolares_produccion.rds") %>% readRDS()
  modelos_sd <- dir_salidas %>% file.path("modelos_dispersion_produccion.rds") %>% readRDS()
  limites_sd <- dir_salidas %>% file.path("limites_dispersion_produccion.rds") %>% readRDS()
  modo <- "PRODUCCIÓN"
  detalle <- paste("entrenados con", paste(anios_cerrados, collapse = ", "))
} else {
  modelos    <- dir_salidas %>% file.path("modelos_escolares.rds") %>% readRDS()
  modelos_sd <- dir_salidas %>% file.path("modelos_dispersion.rds") %>% readRDS()
  limites_sd <- dir_salidas %>% file.path("limites_dispersion.rds") %>% readRDS()
  modo <- "VALIDACIÓN"
  detalle <- paste("entrenados excluyendo", anio_prediccion)
}

cat("Prediciendo ronda:", anio_prediccion, "\n")
cat("Años con SIMCE observado:", paste(anios_cerrados, collapse = ", "), "\n")
cat("Modelos usados:", modo, "(", detalle, ")\n\n")

# ---- 2. Predicción a nivel de colegio: media y ancho ---------------
# mu_hat viene del modelo de 02; sd_hat del modelo de 02b. Si para un
# grupo no hay modelo de dispersión (pocos datos), se usa como respaldo
# la dispersión histórica del propio colegio, que ya viene calculada
# con ventana expansiva en 01.
predecir_colegio <- function(datos, clave) {

  datos$pred_simce_colegio <- if (clave %in% names(modelos)) {
    predict(modelos[[clave]], newdata = datos)
  } else NA_real_

  if (clave %in% names(modelos_sd)) {
    lim <- limites_sd[[clave]]
    sd_hat <- predict(modelos_sd[[clave]], newdata = datos)
    datos$sd_simce_pred <- pmin(pmax(sd_hat, lim[1]), lim[2])
    datos$origen_sd <- "modelo"
  } else {
    datos$sd_simce_pred <- datos$sd_hist_colegio
    datos$origen_sd <- "historica"
  }
  # Respaldo final para colegios sin historia ni modelo: la dispersión
  # esperada para su contexto (GSE, dependencia, ruralidad), calculada
  # en 01. Antes acá iba una constante nacional.
  datos$sd_simce_pred <- coalesce(datos$sd_simce_pred, datos$contexto_sd)
  datos
}

pred_colegio <- school_features %>%
  filter(agno == anio_prediccion) %>%
  group_split(grado, area) %>%
  map_dfr(~ predecir_colegio(.x, paste(.x$grado[1], .x$area[1], sep = "_"))) %>%
  # Tercil de nivel, para elegir la plantilla de forma en la versión B.
  # Se asigna con la media PREDICHA (no la observada): en producción no
  # se conoce la observada.
  left_join(cortes_tercil, by = c("agno", "grado", "area")) %>%
  mutate(
    tercil = case_when(
      is.na(corte_1) | is.na(pred_simce_colegio) ~ "todos",
      pred_simce_colegio <= corte_1              ~ "bajo",
      pred_simce_colegio <= corte_2              ~ "medio",
      TRUE                                       ~ "alto"
    )
  )

cat("Colegios con predicción:", sum(!is.na(pred_colegio$pred_simce_colegio)), "\n")
print(pred_colegio %>%
        group_by(grado, area) %>%
        summarise(n = n(),
                  media_pred = mean(pred_simce_colegio, na.rm = TRUE),
                  sd_pred_media = mean(sd_simce_pred, na.rm = TRUE),
                  .groups = "drop"))

write_csv(
  pred_colegio %>%
    select(agno, grado, area, rbd_revisado, n_estudiantes,
           gse_etiqueta, depe2_etiqueta, rural_etiqueta, nom_com_rbd,
           agno_contexto, n_anios_nivel_hist,
           # Las DOS: `mean_logro` es el nivel observado del colegio en el
           # ensayo y `mean_logro_enc` el que efectivamente entra al modelo.
           # Exportar sólo el primero deja el reporte mostrando una cifra
           # distinta de la que produjo la predicción.
           mean_logro, mean_logro_enc, conf_mean_logro,
           contexto_nivel, desvio_nivel, nivel_hist_colegio,
           contexto_sd, sd_hist_colegio,
           pred_simce_colegio, sd_simce_pred, origen_sd, tercil),
  dir_salidas %>% file.path("predicciones_colegio.csv")
)

# Comparación de cada colegio contra su propio estrato: útil para
# reportar ("su colegio está X puntos sobre lo esperado para un
# particular subvencionado GSE medio alto") y para detectar predicciones
# raras antes de que salgan a un cliente.
resumen_estrato <- pred_colegio %>%
  group_by(grado, area, gse_etiqueta, depe2_etiqueta) %>%
  summarise(n = n(),
            pred_media = mean(pred_simce_colegio, na.rm = TRUE),
            sd_media   = mean(sd_simce_pred, na.rm = TRUE),
            .groups = "drop") %>%
  filter(n >= 5)
cat("\nPredicción media por estrato (contexto rezagado):\n")
print(resumen_estrato %>% mutate(across(where(is.numeric), ~round(.x, 1))))

# ---- 3. Base individual: estudiante + su colegio -------------------
ind_pred <- ind_features %>%
  filter(agno == anio_prediccion) %>%
  # Las columnas de contexto (gse_etiqueta, depe2_etiqueta, ...) ya vienen
  # dentro de ind_features desde 01: no se vuelven a pegar acá para no
  # generar sufijos .x/.y.
  inner_join(
    pred_colegio %>%
      select(agno, grado, area, rbd_revisado,
             pred_simce_colegio, sd_simce_pred, tercil),
    by = c("agno", "grado", "area", "rbd_revisado")
  ) %>%
  left_join(limites_simce, by = c("grado", "area"))

# ---- 4. Predicción individual: matching por percentil ---------------
# A cada estudiante se le asigna el cuantil de la plantilla empírica
# que corresponde a su percentil dentro del colegio, y ese cuantil se
# traslada a la escala del colegio con su media y ancho predichos.
aplicar_forma <- function(datos) {
  datos %>%
    group_by(agno, grado, area, tercil) %>%
    group_modify(function(.x, .y) {
      f <- forma_z %>%
        filter(agno == .y$agno, grado == .y$grado, area == .y$area, tercil == .y$tercil)
      if (nrow(f) == 0) {
        f <- forma_z %>%
          filter(agno == .y$agno, grado == .y$grado, area == .y$area, tercil == "todos")
      }
      .x$z_forma <- if (nrow(f) == 0) {
        qnorm(pmin(pmax(.x$pct_ensayo, 0.001), 0.999))   # respaldo: normal
      } else {
        approx(x = f$p, y = f$z, xout = .x$pct_ensayo, rule = 2)$y
      }
      .x
    }) %>%
    ungroup()
}

pred_B <- ind_pred %>%
  aplicar_forma() %>%
  mutate(
    pred_B = pred_simce_colegio + sd_simce_pred * z_forma,
    pred_B = pmin(pmax(pred_B, ptje_min), ptje_max)
  )

pred_individual <- pred_B

# ---- 5. Chequeos de coherencia --------------------------------------
# (i) El promedio de las predicciones individuales de un colegio debe
#     reproducir la predicción del colegio: la plantilla de forma tiene
#     media ~0, así que el reparto no puede correr el centro.
# (ii) La sd de esas predicciones debe reproducir el ancho predicho, o sea
#     razon_sd_B cerca de 1.
chequeo <- pred_individual %>%
  group_by(agno, grado, area, rbd_revisado) %>%
  summarise(
    n_est          = n(),
    media_B        = mean(pred_B, na.rm = TRUE),
    sd_B           = sd(pred_B, na.rm = TRUE),
    pred_colegio   = first(pred_simce_colegio),
    sd_predicha    = first(sd_simce_pred),
    rel_media      = mean(rel_indice, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    dif_media_B = media_B - pred_colegio,
    razon_sd_B  = sd_B / sd_predicha
  )

cat("\nChequeos de coherencia (promedios sobre colegios):\n")
print(chequeo %>%
        group_by(grado, area) %>%
        summarise(
          dif_media_B_abs = mean(abs(dif_media_B), na.rm = TRUE),
          razon_sd_B      = mean(razon_sd_B, na.rm = TRUE),
          .groups = "drop"
        ) %>% mutate(across(where(is.numeric), ~round(.x, 3))))
cat("\n(dif_media_B_abs cerca de 0 y razon_sd_B cerca de 1.)\n\n")

# ---- 6. Guardar ------------------------------------------------------
salida_individual <- pred_individual %>%
  select(agno, grado, area, rbd_revisado, id_usuario_curso,
         gse_etiqueta, depe2_etiqueta, rural_etiqueta,
         k_ensayos, mean_logro,
         # Confiabilidad del índice de cada estudiante: dice cuánta
         # confianza merece su POSICIÓN dentro del colegio, que es lo que
         # determina su predicción. Viaja junto al resultado en vez de
         # quedarse sólo en 01.
         rel_indice,
         pct_ensayo, z_ensayo,
         pred_simce_colegio, sd_simce_pred,
         pred_B)

write_csv(salida_individual, dir_salidas %>% file.path("predicciones_individual.csv"))
saveRDS(pred_individual, dir_salidas %>% file.path("predicciones_individual.rds"))
saveRDS(pred_colegio,    dir_salidas %>% file.path("predicciones_colegio.rds"))
write_csv(chequeo, dir_salidas %>% file.path("chequeo_coherencia.csv"))

cat("Listo. Archivos generados en ", dir_salidas, ":\n",
    " - predicciones_colegio.csv    (media y ancho predichos por colegio)\n",
    " - predicciones_individual.csv (predicción por estudiante)\n",
    " - chequeo_coherencia.csv\n",
    "\nSiguiente paso: 04_validacion_individual.R\n")

