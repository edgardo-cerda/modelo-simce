# =============================================================
# 03_prediccion_nueva_ronda.R  (v9 - una sola versión de predicción)
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
# -------------------------------------------------------------
# CAMBIO v9: SALE LA VERSIÓN A, Y CON ELLA `rho`
# -------------------------------------------------------------
# La versión A era `mu_hat + rho * sd_hat * z_ensayo`: encogía hacia el
# promedio del colegio y devolvía un rango p10-p90 por estudiante.
#
# El problema era `rho`. No es estimable con estos datos —haría falta un
# vínculo alumno-a-alumno entre el ensayo Santillana y el SIMCE, que no
# existe— y su valor salía de reparametrizar un 0.70 elegido a mano en la
# v4. Era el número menos defendible del pipeline, y arrastraba consigo el
# modo `RHO_MODO`, la tabla `rho_sugerido` de 01, la varianza residual en
# dos componentes y un análisis de sensibilidad en 04 que, por
# construcción, no podía concluir nada (sus dos métricas empujan en
# direcciones opuestas y una favorece rho=1 mecánicamente).
#
# El matching por percentil no necesita ningún parámetro libre: la forma
# de la distribución se OBSERVA en ~6.400 colegios reales en vez de
# suponerse. Se pierde el intervalo por estudiante; a cambio, todo lo que
# el script entrega es verificable contra datos.
#
# La predicción no puede validarse estudiante por estudiante, pero SÍ a
# nivel de distribución contra los puntajes individuales reales: ver
# 04_validacion_individual.R.
# =============================================================

library(tidyverse)
# NOTA v9: hasta la v8 hacía falta `library(splines)` acá, aunque este
# script no escriba ningún `ns()`: los modelos de 02b llevaban splines en
# su fórmula y `predict()` los reconstruye evaluando los `predvars`
# guardados en el modelo. El modelo de dispersión pasó a ser lineal, así
# que la dependencia se fue.

# ---- 0. Configuración --------------------------------------------
usuario <- Sys.info()[["user"]]
rutas <- config::get(config = usuario, file = "config.yml")
ruta_outputs <- rutas$ruta_outputs
dir_salidas <- ruta_outputs %>% file.path('modelo_lme_alu_v2')

# ---- 1. Insumos ---------------------------------------------------
ind_features    <- dir_salidas %>% file.path("ind_features.rds") %>% readRDS()
school_features <- dir_salidas %>% file.path("school_features.rds") %>% readRDS()
modelos         <- dir_salidas %>% file.path("modelos_escolares.rds") %>% readRDS()
modelos_sd      <- dir_salidas %>% file.path("modelos_dispersion.rds") %>% readRDS()
limites_sd      <- dir_salidas %>% file.path("limites_dispersion.rds") %>% readRDS()
forma_z         <- dir_salidas %>% file.path("forma_z.rds") %>% readRDS()
cortes_tercil   <- dir_salidas %>% file.path("cortes_tercil.rds") %>% readRDS()
limites_simce   <- dir_salidas %>% file.path("limites_simce.rds") %>% readRDS()

anio_prediccion <- max(school_features$agno)
cat("Prediciendo ronda:", anio_prediccion, "\n\n")

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
           # Exportar sólo el primero, como hacía la v8, dejaba el reporte
           # mostrando una cifra distinta de la que produjo la predicción.
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

# ---- 5. Versión v2 (legado), sólo para comparar ---------------------
# Es lo que hacía la versión anterior: aplicarle los coeficientes del
# modelo de colegio a las features del estudiante. Se conserva para que
# 04 pueda mostrar cuánto se gana con el cambio.
predecir_legado <- function(datos, clave) {

  if (!clave %in% names(modelos)) {
    datos$pred_v2_legado <- NA_real_
    return(datos)
  }

  modelo    <- modelos[[clave]]
  faltantes <- setdiff(all.vars(delete.response(terms(modelo))), names(datos))

  # `mean_logro_enc` (v5) existe SÓLO a nivel de colegio: es el promedio del
  # colegio encogido hacia el promedio de su grupo según la confiabilidad de
  # esa medición (01, secc. 5b-bis), y no tiene contraparte por estudiante.
  # Como esta función es justamente la versión legado —aplicarle los
  # coeficientes del colegio a las features del ESTUDIANTE—, el análogo
  # individual del término de logro es el `mean_logro` del propio estudiante,
  # que es lo que llevaba la fórmula de la v4. Sin esto, predict() aborta con
  # "objeto 'mean_logro_enc' no encontrado" y se cae el script entero.
  if ("mean_logro_enc" %in% faltantes) {
    datos$mean_logro_enc <- datos$mean_logro
    faltantes <- setdiff(faltantes, "mean_logro_enc")
  }

  if (length(faltantes) > 0) {
    warning("La comparación legado se omite en ", clave,
            ": el modelo de 02 usa predictores que no existen a nivel de ",
            "estudiante (", paste(faltantes, collapse = ", "), ").")
    datos$pred_v2_legado <- NA_real_
    return(datos)
  }

  datos$pred_v2_legado <- predict(modelo, newdata = datos)
  datos
}

pred_individual <- pred_B %>%
  group_split(grado, area) %>%
  map_dfr(~ predecir_legado(.x, paste(.x$grado[1], .x$area[1], sep = "_")))

# ---- 6. Chequeos de coherencia --------------------------------------
# (i) El promedio de las predicciones individuales debe reproducir la
#     predicción del colegio (la plantilla de forma tiene media ~0).
# (ii) La sd de las predicciones debe reproducir el ancho predicho, o sea
#     razon_sd cerca de 1. La versión v2 legado daba ~0.1-0.2, que es
#     exactamente el defecto que este diseño vino a corregir.
chequeo <- pred_individual %>%
  group_by(agno, grado, area, rbd_revisado) %>%
  summarise(
    n_est          = n(),
    media_B        = mean(pred_B, na.rm = TRUE),
    sd_B           = sd(pred_B, na.rm = TRUE),
    sd_v2_legado   = sd(pred_v2_legado, na.rm = TRUE),
    pred_colegio   = first(pred_simce_colegio),
    sd_predicha    = first(sd_simce_pred),
    rel_media      = mean(rel_indice, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    dif_media_B   = media_B - pred_colegio,
    razon_sd_B    = sd_B / sd_predicha,         # debería dar ~1
    razon_sd_v2   = sd_v2_legado / sd_predicha  # la v2 daba ~0.1-0.2
  )

cat("\nChequeos de coherencia (promedios sobre colegios):\n")
print(chequeo %>%
        group_by(grado, area) %>%
        summarise(
          dif_media_B_abs = mean(abs(dif_media_B), na.rm = TRUE),
          razon_sd_B      = mean(razon_sd_B, na.rm = TRUE),
          razon_sd_legado = mean(razon_sd_v2, na.rm = TRUE),
          .groups = "drop"
        ) %>% mutate(across(where(is.numeric), ~round(.x, 3))))
cat("\n(dif_media_B_abs cerca de 0 y razon_sd_B cerca de 1. razon_sd_legado\n",
    " muestra cuánta dispersión perdía la versión anterior.)\n\n")

# ---- 7. Guardar ------------------------------------------------------
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
         pred_B, pred_v2_legado)

write_csv(salida_individual, dir_salidas %>% file.path("predicciones_individual.csv"))
saveRDS(pred_individual, dir_salidas %>% file.path("predicciones_individual.rds"))
saveRDS(pred_colegio,    dir_salidas %>% file.path("predicciones_colegio.rds"))
write_csv(chequeo, dir_salidas %>% file.path("chequeo_coherencia.csv"))

cat("Listo. Archivos generados en ", dir_salidas, ":\n",
    " - predicciones_colegio.csv    (media y ancho predichos por colegio)\n",
    " - predicciones_individual.csv (predicción por estudiante y legado v2)\n",
    " - chequeo_coherencia.csv\n",
    "\nSiguiente paso: 04_validacion_individual.R\n")

