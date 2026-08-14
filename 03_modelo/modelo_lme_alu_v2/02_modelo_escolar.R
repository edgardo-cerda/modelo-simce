# =============================================================
# 02_modelo_escolar.R  (v5)
# -------------------------------------------------------------
# Modelo del NIVEL (promedio) del colegio: regresión lineal múltiple,
# una por combinación grado x área, validada out-of-time contra el año
# más reciente.
#
# FÓRMULA v5:
#   promedio_simce ~ mean_logro_enc + nivel_hist_colegio
#
# FÓRMULA v4 (la anterior):
#   promedio_simce ~ mean_logro + pred_final_logro + slope_logro +
#                     n_evals_prom + nivel_hist_colegio
#
# Tres variables salen y una cambia:
#
#  - SALE `slope_logro` (punto 1). Los ensayos no forman una progresión:
#    tienen dificultades propias y no ordenadas. Una pendiente ajustada
#    sobre la secuencia de ensayos mide sobre todo cuáles se aplicaron.
#    Medido out-of-time, sacarla mueve el MAE entre -0.13 y +0.02
#    puntos, o sea nada: el cambio es por interpretabilidad, no por
#    precisión. Se gana además un grado de libertad, que con 150-220
#    filas por grupo no es despreciable.
#
#  - SALE `n_evals_prom` (punto 2). Entraba de forma aditiva, y no hay
#    ninguna razón por la que el número de ensayos deba subir o bajar
#    el SIMCE de un colegio de forma lineal. Lo que el número de
#    ensayos sí indica es cuán PRECISO es `mean_logro`.
#
#  - `mean_logro` pasa a `mean_logro_enc`, la versión encogida hacia el
#    promedio del grupo según su confiabilidad (calculada en 01, secc.
#    5b-bis). Ahí el número de ensayos entra por donde corresponde:
#    menos ensayos -> medidas individuales más ruidosas -> más
#    dispersión aparente entre alumnos -> mayor error del promedio ->
#    más encogimiento -> el modelo se apoya más en el prior histórico.
#
# EXPECTATIVA HONESTA sobre el punto 2: con 45-72 alumnos por colegio,
# la confiabilidad del promedio escolar ya es de 0.90-0.97. El
# encogimiento por lo tanto es suave y NO mejora el MAE (out-of-time
# queda entre neutro y ~0.25 puntos peor). Es un cambio de corrección,
# no de precisión. El script imprime igual la comparación contra la
# fórmula v4 y contra `mean_logro` sin encoger, para poder auditarlo
# en cada corrida.
#
#  - SALE `pred_final_logro`, que es el paso que esta misma nota dejaba
#    pendiente. Cuando el modelo de crecimiento de 01 tenía pendiente,
#    esta variable era intercepto + pendiente*6, o sea una extrapolación:
#    información distinta del promedio. Sacada la pendiente (punto 1),
#    quedó siendo el intercepto solo, y el promedio por colegio de los
#    interceptos encogidos por lme4 ES el `mean_logro` del colegio
#    encogido. La fórmula tenía entonces DOS versiones encogidas de la
#    misma cantidad, con dos mecanismos de encogimiento distintos: el
#    explícito de la secc. 5b-bis de 01 y el implícito de lme4 más el
#    recorte a [0,100].
#
#    La evidencia de que sobraba, medida sobre estos datos:
#      - correlación con `mean_logro_enc`: 0.995 a 0.999; VIF de 73 a 360.
#      - al regresar una sobre otra, el residuo de `pred_final_logro`
#        tiene sd de 0.45-0.85 puntos de logro contra una sd propia de
#        ~9.5 (R² 0.99-0.998). Ese residuo es el recorte a [0,100] y la
#        diferencia entre las dos fórmulas de encogimiento, no señal
#        sobre el colegio. Por eso se saca en vez de residualizarla.
#      - el MAE out-of-time no decide: 10.3 con las dos, 10.2 con una
#        sola (diferencias de ±0.3 puntos entre grupos, o sea ruido).
#        El R² out-of-time sí mejora algo, de 0.716 a 0.737.
#      - lo que decide es la ESTABILIDAD. Dejando fuera un año de
#        entrenamiento por vez, con las dos variables los coeficientes
#        hacen balancín: en 2m matemática saltan a +31.6 y -28.4, y en
#        tres de los cuatro grupos el de logro sale con signo negativo
#        compensado por el otro. Con una sola variable el coeficiente se
#        queda entre 0.6 y 1.7, con signo estable y lectura directa
#        ("un punto más de logro en el ensayo vale ~1 punto SIMCE").
#
#    Se conserva `pred_final_logro` como COLUMNA en `school_features`,
#    `ind_features` y `predicciones_colegio.csv`: sólo deja de entrar al
#    modelo. La especificación anterior queda en `formulas_auditoria`
#    para poder seguir viéndola en cada corrida.
#
# Lo único que cambia es el rol que cumple dentro del pipeline:
# ahora este modelo entrega la MEDIA de la distribución que se le
# va a asignar a cada colegio, y 02b_modelo_dispersion.R entrega
# su ANCHO. Las predicciones individuales de 03 se construyen
# sobre ambos (media + dispersión), en vez de aplicarle los
# coeficientes de colegio a cada estudiante como en la v2.
#
# Por eso importa que este script quede tal cual está: es el
# ancla de todo lo demás, y es el único componente validado
# contra verdad observada directa (el SIMCE promedio publicado).
# =============================================================

library(tidyverse)
library(broom)

# ---- 0. Configuración --------------------------------------------
usuario <- Sys.info()[["user"]]
rutas <- config::get(config = usuario, file = "config.yml")
ruta_outputs <- rutas$ruta_outputs
dir_salidas <- ruta_outputs %>% file.path('modelo_lme_alu_v2')

school_model_data <- dir_salidas %>% file.path('school_model_data.rds') %>% readRDS()

anios     <- sort(unique(school_model_data$agno))
anio_test <- max(anios)
cat("Años disponibles para entrenar/validar:", paste(anios, collapse = ", "), "\n")
cat("Año usado como prueba (out-of-time):", anio_test, "\n\n")

# `nivel_hist_colegio` ahora viene con prior contextual desde 01:
# es la suma de la expectativa según GSE / dependencia / ruralidad y el
# desvío propio del colegio. La fórmula no cambia, pero la variable
# significa algo mejor, sobre todo en colegios con poca o nula historia.
#
# Se puede además incluir `contexto_nivel` como término aparte, para que
# el modelo le dé a la expectativa contextual un peso distinto que al
# desvío del colegio. Al probarlo out-of-time no aportó (MAE entre -0.2
# y +0.3 puntos, peor en un grupo): con ~150-220 filas por grupo es un
# grado de libertad que no se paga. Queda en FALSE por defecto y el
# script imprime igual la comparación para que se pueda auditar en cada
# corrida sobre los datos del momento.
INCLUIR_CONTEXTO_APARTE <- FALSE

# Usar el `mean_logro` encogido por confiabilidad (punto 2) o el crudo.
# En TRUE es la especificación v5. Ponerlo en FALSE deja la fórmula v5
# pero con el logro sin encoger, que es la comparación limpia para
# aislar cuánto aporta el encogimiento por sí solo.
USAR_LOGRO_ENCOGIDO <- TRUE

var_logro <- if (USAR_LOGRO_ENCOGIDO) "mean_logro_enc" else "mean_logro"

formula_base <- as.formula(paste(
  "promedio_simce ~", var_logro, "+ nivel_hist_colegio"
))

# `formula_modelo` se define más abajo, a partir de SPEC_ELEGIDA (secc. 0b).

# Especificaciones que se ajustan sólo para AUDITAR (no para elegir:
# elegir mirando el año de prueba sería seleccionar sobre el test).
# Se imprimen sus MAE junto al del modelo para poder revisar la decisión
# con datos frescos en cada corrida.
# OJO: la v4 completa llevaba además `slope_logro`. Desde v7 esa variable
# vuelve a existir —el modelo de crecimiento de 01 estima pendiente sobre
# el logro ajustado por IRT— pero se deja fuera de esta fórmula a
# propósito: con `pred_final_logro` (que ya ES la proyección al 6º ensayo)
# la pendiente entra dos veces. La comparación que importa la hace la
# tabla de especificaciones de la sección 0b.
#
# `v5_con_pred_final` es la especificación que estuvo vigente hasta que se
# sacó `pred_final_logro` por colinealidad (ver el encabezado). Se deja acá
# para poder ver en cada corrida qué costó ese cambio: si en algún momento
# el MAE de esta fila quedara claramente por debajo del del modelo, habría
# que volver a mirar la decisión — pero revísese también el VIF que se
# imprime más abajo antes de concluir nada.
#
# `v6_sin_irt` es la MISMA fórmula pero con el nivel escolar construido
# sobre el porcentaje de logro observado en vez del puntaje verdadero IRT
# (01 deja las dos columnas calculadas). Es la comparación que dice qué
# aportó la calibración del punto de vista del error de predicción, y por
# eso conviene tenerla impresa en cada corrida y no sólo el día que se
# hizo el cambio.
# CAMBIO v7: sale `n_evals_prom` de la auditoría. Entraba de forma aditiva
# —un término lineal en "cuántos ensayos rindió el colegio en promedio",
# que nunca tuvo lectura defendible— y el número de ensayos ya entra por
# donde corresponde: la precisión de la medida, vía el encogimiento por
# confiabilidad de 01 (secc. 5b-bis) y, desde v7, vía la ponderación por
# 1/se^2 del modelo de crecimiento. Con `pred_final_logro` en la fórmula,
# mantenerlo además como regresor sería contar dos veces lo mismo.
formulas_auditoria <- list(
  v4_sin_slope      = promedio_simce ~ mean_logro + pred_final_logro +
                       nivel_hist_colegio,
  v5_sin_encoger    = promedio_simce ~ mean_logro + nivel_hist_colegio,
  v5_con_pred_final = update(formula_base, . ~ . + pred_final_logro),
  v5_con_contexto   = update(formula_base, . ~ . + contexto_nivel),
  v6_sin_irt        = promedio_simce ~ mean_logro_crudo_enc + nivel_hist_colegio,
  v6_con_irt        = promedio_simce ~ mean_logro_irt_enc + nivel_hist_colegio
)

# ---- 0b. Especificaciones a comparar (NUEVO v7) ----------------------
# El problema que motiva esta comparación: el logro en los ensayos sube
# todos los años y el SIMCE no lo sigue, así que un modelo que lee el
# nivel del ensayo como nivel absoluto proyecta subidas que no ocurren
# (sesgo de +5 a +6 puntos en 2025, ver el pie del script).
#
# `*_c` son las versiones centradas DENTRO de año x grado x área que
# construye 01 (secc. 5b-ter): el ensayo aporta sólo la posición relativa
# del colegio entre sus pares de ese año y el nivel absoluto queda a cargo
# de `nivel_hist_colegio`. La deriva común del año se cancela por
# construcción.
ESPECIFICACIONES <- list(
  actual = promedio_simce ~ mean_logro_enc + nivel_hist_colegio,
  A      = promedio_simce ~ mean_logro_enc + pred_final_logro + nivel_hist_colegio,
  B      = promedio_simce ~ pred_final_logro + nivel_hist_colegio,
  C      = promedio_simce ~ mean_logro_enc_c + nivel_hist_colegio,
  D      = promedio_simce ~ pred_final_logro_c + nivel_hist_colegio,
  E      = promedio_simce ~ mean_logro_enc_c + nivel_hist_colegio + agno
)

# Especificación que se usa para el modelo de producción. Se elige por la
# VENTANA A (entrena 2023, predice 2024); la ventana B (entrena 2023-24,
# predice 2025) se deja como held-out y sólo se reporta. Elegir mirando la
# ventana B sería seleccionar sobre el test.
SPEC_ELEGIDA <- "actual"

stopifnot("SPEC_ELEGIDA no está en ESPECIFICACIONES" =
            SPEC_ELEGIDA %in% names(ESPECIFICACIONES))

# La fórmula de producción sale de la especificación elegida. El switch de
# contexto aparte se mantiene y se aplica sobre ella.
formula_modelo <- ESPECIFICACIONES[[SPEC_ELEGIDA]]
if (INCLUIR_CONTEXTO_APARTE) {
  formula_modelo <- update(formula_modelo, . ~ . + contexto_nivel)
}

# VIF de la fórmula elegida. Se imprime en cada corrida porque el motivo por
# el que salió `pred_final_logro` fue exactamente éste, y porque cualquier
# predictor que se agregue en el futuro puede reintroducir el problema en
# silencio: la colinealidad no empeora el MAE de forma visible, sólo vuelve
# los coeficientes inestables y no interpretables.
vif_formula <- function(f, datos) {
  X <- model.matrix(f, datos)[, -1, drop = FALSE]
  if (ncol(X) < 2) return(setNames(rep(1, ncol(X)), colnames(X)))
  sapply(colnames(X), function(v) {
    r2 <- summary(lm(X[, v] ~ X[, setdiff(colnames(X), v), drop = FALSE]))$r.squared
    1 / (1 - r2)
  })
}

cat("Fórmula del modelo:\n  "); print(formula_modelo)
cat("\n")

grupos <- school_model_data %>% distinct(grado, area)

# ---- 1b. Comparación de especificaciones en dos ventanas (NUEVO v7) ---
# Con tres años sólo hay dos cortes out-of-time posibles. Se usan los dos
# y con roles distintos:
#
#   Ventana A: entrena 2023        -> predice 2024   (para ELEGIR)
#   Ventana B: entrena 2023 + 2024 -> predice 2025   (held-out, sólo se
#                                                     reporta)
#
# Se reporta MAE y también SESGO: el síntoma del problema de deriva es el
# sesgo, y una especificación puede corregirlo sin mover mucho el MAE.
evaluar_spec <- function(f, datos_grupo, anios_train, anio_eval) {
  train <- datos_grupo %>% filter(agno %in% anios_train, !is.na(promedio_simce))
  test  <- datos_grupo %>% filter(agno == anio_eval,     !is.na(promedio_simce))

  if (nrow(train) < 20 || nrow(test) < 10) {
    return(tibble(n_train = nrow(train), n_test = nrow(test),
                  mae = NA_real_, sesgo = NA_real_, r2 = NA_real_,
                  nota = "datos insuficientes"))
  }
  # Un término de año necesita al menos dos años de entrenamiento; con uno
  # solo queda aliaseado con el intercepto. Se marca en vez de devolver un
  # NA sin explicación.
  if ("agno" %in% all.vars(f) && n_distinct(train$agno) < 2) {
    return(tibble(n_train = nrow(train), n_test = nrow(test),
                  mae = NA_real_, sesgo = NA_real_, r2 = NA_real_,
                  nota = "no estimable con 1 año de train"))
  }

  res <- tryCatch({
    m    <- lm(f, data = train)
    pred <- predict(m, newdata = test)
    obs  <- test$promedio_simce
    tibble(n_train = nrow(train), n_test = nrow(test),
           mae   = mean(abs(pred - obs)),
           sesgo = mean(pred - obs),
           r2    = 1 - sum((obs - pred)^2) / sum((obs - mean(obs))^2),
           nota  = NA_character_)
  }, error = function(e) {
    tibble(n_train = nrow(train), n_test = nrow(test),
           mae = NA_real_, sesgo = NA_real_, r2 = NA_real_,
           nota = paste("error:", conditionMessage(e)))
  })
  res
}

anios_ord <- sort(anios)
ventanas <- list(
  A = list(train = anios_ord[1],                  eval = anios_ord[2]),
  B = list(train = anios_ord[1:(length(anios_ord) - 1)], eval = anio_test)
)

comparacion_specs <- map_dfr(names(ventanas), function(v) {
  map_dfr(seq_len(nrow(grupos)), function(i) {
    g <- grupos$grado[i]; a <- grupos$area[i]
    datos_grupo <- school_model_data %>% filter(grado == g, area == a)
    map_dfr(names(ESPECIFICACIONES), function(s) {
      evaluar_spec(ESPECIFICACIONES[[s]], datos_grupo,
                   ventanas[[v]]$train, ventanas[[v]]$eval) %>%
        mutate(ventana = v, spec = s, grado = g, area = a,
               anios_train = paste(ventanas[[v]]$train, collapse = "+"),
               anio_eval = ventanas[[v]]$eval, .before = 1)
    })
  })
})

cat("=============================================================\n")
cat("COMPARACIÓN DE ESPECIFICACIONES (out-of-time)\n")
cat("=============================================================\n")
for (v in names(ventanas)) {
  cat(sprintf("\nVentana %s: entrena %s -> predice %s\n", v,
              paste(ventanas[[v]]$train, collapse = "+"), ventanas[[v]]$eval))
  print(
    comparacion_specs %>%
      filter(ventana == v) %>%
      group_by(spec) %>%
      summarise(grupos = sum(!is.na(mae)),
                mae_medio    = round(mean(mae, na.rm = TRUE), 2),
                sesgo_medio  = round(mean(sesgo, na.rm = TRUE), 2),
                sesgo_abs    = round(mean(abs(sesgo), na.rm = TRUE), 2),
                r2_medio     = round(mean(r2, na.rm = TRUE), 3),
                nota = first(na.omit(nota)) %||% "",
                .groups = "drop") %>%
      arrange(mae_medio) %>%
      as.data.frame()
  )
}

cat("\nDetalle por grupo (MAE / sesgo):\n")
print(
  comparacion_specs %>%
    filter(!is.na(mae)) %>%
    mutate(celda = sprintf("%.1f / %+.1f", mae, sesgo)) %>%
    select(ventana, grado, area, spec, celda) %>%
    pivot_wider(names_from = spec, values_from = celda) %>%
    arrange(ventana, grado, area) %>%
    as.data.frame()
)

cat("\nEspecificación usada para el modelo de producción:", SPEC_ELEGIDA, "\n")
cat("(se elige por la ventana A; la B queda como held-out)\n\n")

modelos    <- list()
resultados <- list()

for (i in seq_len(nrow(grupos))) {

  g <- grupos$grado[i]; a <- grupos$area[i]
  clave <- paste(g, a, sep = "_")

  datos_grupo <- school_model_data %>% filter(grado == g, area == a)

  train <- datos_grupo %>% filter(agno != anio_test)
  test  <- datos_grupo %>% filter(agno == anio_test)

  if (nrow(train) < 20 || nrow(test) < 10) {
    cat("Grupo", clave, ": muy pocos datos, se omite.\n\n")
    next
  }

  modelo <- lm(formula_modelo, data = train)
  modelos[[clave]] <- modelo

  pred <- predict(modelo, newdata = test)
  obs  <- test$promedio_simce

  # Auditoría: las especificaciones alternativas, incluida la v4
  # completa. No se usan para elegir, sólo para poder ver en cada
  # corrida qué costó (o qué ahorró) cada cambio.
  mae_aud <- map_dbl(formulas_auditoria, function(f) {
    tryCatch({
      alt <- lm(f, data = train)
      mean(abs(predict(alt, newdata = test) - obs))
    }, error = function(e) NA_real_)
  })
  mae_alternativa <- mae_aud[["v5_con_contexto"]]

  mae_modelo   <- mean(abs(pred - obs))
  rmse_modelo  <- sqrt(mean((pred - obs)^2))
  mae_baseline <- mean(abs(mean(train$promedio_simce) - obs))
  r2_test      <- 1 - sum((obs - pred)^2) / sum((obs - mean(obs))^2)

  # ¿Cómo le va a los colegios con poca o nula historia propia? Es donde
  # el prior contextual debería notarse.
  sin_hist <- test$n_anios_nivel_hist <= 1
  mae_sin_historia <- if (any(sin_hist)) mean(abs(pred[sin_hist] - obs[sin_hist])) else NA_real_
  mae_con_historia <- if (any(!sin_hist)) mean(abs(pred[!sin_hist] - obs[!sin_hist])) else NA_real_

  resultados[[clave]] <- tibble(
    grado = g, area = a,
    n_train = nrow(train), n_test = nrow(test),
    # promedios del año de prueba: los reporta la presentación junto al MAE
    media_pred_test = mean(pred), media_obs_test = mean(obs),
    mae = mae_modelo, rmse = rmse_modelo, mae_baseline = mae_baseline,
    mejora_vs_baseline_pct = 100 * (mae_baseline - mae_modelo) / mae_baseline,
    r2_test = r2_test,
    mae_alternativa_contexto = mae_alternativa,
    mae_v4_sin_slope  = mae_aud[["v4_sin_slope"]],
    mae_v5_sin_encoger = mae_aud[["v5_sin_encoger"]],
    mae_v5_con_pred_final = mae_aud[["v5_con_pred_final"]],
    mae_sin_irt = mae_aud[["v6_sin_irt"]],
    mae_con_irt = mae_aud[["v6_con_irt"]],
    gana_irt_pts = mae_aud[["v6_sin_irt"]] - mae_aud[["v6_con_irt"]],
    vif_maximo = max(vif_formula(formula_modelo, train)),
    # Sesgo medio del año de prueba. Vale la pena mirarlo: si es grande y
    # del mismo signo en todos los grupos, no es ruido — es que la escala
    # del ensayo se movió entre años y el modelo lo está leyendo como
    # mejora real (ver nota al pie de este script).
    sesgo_test = mean(pred - obs),
    n_test_sin_historia = sum(sin_hist),
    mae_sin_historia = mae_sin_historia,
    mae_con_historia = mae_con_historia
  )

  cat("---", clave, "---\n")
  print(tidy(modelo))
  cat(sprintf(
    "MAE modelo: %.1f | MAE baseline (promedio histórico): %.1f | Mejora: %.0f%% | R2 (test %s): %.2f\n",
    mae_modelo, mae_baseline, resultados[[clave]]$mejora_vs_baseline_pct, anio_test, r2_test
  ))
  cat(sprintf("  Sesgo medio (predicho - observado): %+.1f puntos\n",
              resultados[[clave]]$sesgo_test))
  cat(sprintf(
    "  Auditoría | v4 sin slope: %.1f | v5 sin encoger: %.1f | con pred_final_logro: %.1f | con contexto aparte: %.1f\n",
    mae_aud[["v4_sin_slope"]], mae_aud[["v5_sin_encoger"]],
    mae_aud[["v5_con_pred_final"]], mae_alternativa
  ))
  cat(sprintf("  IRT | sin IRT: %.1f | con IRT: %.1f | gana %+.1f puntos de MAE\n",
              mae_aud[["v6_sin_irt"]], mae_aud[["v6_con_irt"]],
              mae_aud[["v6_sin_irt"]] - mae_aud[["v6_con_irt"]]))
  cat(sprintf("  VIF máximo de la fórmula: %.1f%s\n",
              resultados[[clave]]$vif_maximo,
              if (resultados[[clave]]$vif_maximo > 10)
                "  <-- AVISO: colinealidad alta, los coeficientes no son interpretables"
              else ""))
  cat(sprintf(
    "  MAE en colegios con <=1 año de historia (n=%d): %.1f | con historia: %.1f\n\n",
    sum(sin_hist), mae_sin_historia, mae_con_historia
  ))
}

tabla_resultados <- bind_rows(resultados)
cat("Resumen de validación (todos los grupos):\n")
print(tabla_resultados)

# --- Chequeo del sesgo sistemático (PENDIENTE, no resuelto acá) --------
# Si `sesgo_test` sale positivo y parecido en TODOS los grupos, no es
# ruido muestral: es que el nivel de logro de los ensayos se movió entre
# años sin que el SIMCE de los mismos colegios se moviera, y el modelo
# —que agrupa años y usa el logro en niveles crudos— lo lee como mejora
# real. En los datos de 2023-2025 ese sesgo es de +4.5 a +7.8 puntos,
# con un MAE de ~10: alrededor de la mitad del error no es ruido.
#
# Este script NO lo corrige. La corrección propuesta (centrar/estandarizar
# el logro dentro de año x grado x área, y tomar el nivel del año de la
# serie nacional del SIMCE) quedó fuera del alcance de esta tanda de
# cambios. Se deja el chequeo impreso para que el problema no se pierda
# de vista.
if (nrow(tabla_resultados) > 0) {
  cat(sprintf(
    "\nSesgo medio out-of-time en todos los grupos: %+.1f puntos (rango %+.1f a %+.1f)\n",
    mean(tabla_resultados$sesgo_test),
    min(tabla_resultados$sesgo_test), max(tabla_resultados$sesgo_test)
  ))
  if (all(tabla_resultados$sesgo_test > 2)) {
    cat("  AVISO: sesgo positivo en todos los grupos. Ver la nota de arriba:\n",
        " es el síntoma de la deriva de escala del ensayo entre años.\n")
  }
}

# ---- Gráfico de diagnóstico: observado vs. predicho en el año de prueba ----
# Se guardan también el rbd y el contexto de cada colegio: 05 los usa
# para los tooltips del gráfico interactivo de la presentación (poder
# pasar el mouse sobre un punto y ver de qué colegio se trata es lo que
# hace útil ese gráfico en una reunión).
diag_plot_data <- map_dfr(names(modelos), function(clave) {
  partes <- str_split(clave, "_", n = 2)[[1]]
  g <- partes[1]; a <- partes[2]
  datos_grupo <- school_model_data %>% filter(grado == g, area == a, agno == anio_test)
  tibble(
    grado = g, area = a,
    rbd_revisado  = datos_grupo$rbd_revisado,
    gse_etiqueta  = datos_grupo$gse_etiqueta,
    depe2_etiqueta = datos_grupo$depe2_etiqueta,
    n_anios_nivel_hist  = datos_grupo$n_anios_nivel_hist,
    n_estudiantes = datos_grupo$n_estudiantes,
    mean_logro    = datos_grupo$mean_logro,
    observado = datos_grupo$promedio_simce,
    predicho  = predict(modelos[[clave]], newdata = datos_grupo)
  )
})

p <- ggplot(diag_plot_data, aes(observado, predicho)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  facet_wrap(~ grado + area, scales = "free") +
  labs(
    title = paste("Colegios: SIMCE observado vs. predicho -", anio_test),
    x = "SIMCE observado", y = "SIMCE predicho"
  ) +
  theme_minimal()

ggsave(dir_salidas %>% file.path("diagnostico_observado_vs_predicho.png"), p, width = 8, height = 6)

# Diferencias promedio entre observado y predicho:
print(
  diag_plot_data %>%
    mutate(diferencia = predicho - observado) %>%
    group_by(grado, area) %>%
    summarise(dif_media = mean(diferencia),
              dif_abs_media = mean(abs(diferencia)), .groups = "drop")
)

# ---- Guardar modelos y métricas ----------------------------------------
saveRDS(modelos, dir_salidas %>% file.path("modelos_escolares.rds"))
saveRDS(diag_plot_data, dir_salidas %>% file.path("diag_nivel.rds"))  # lo usa 05
saveRDS(anio_test, dir_salidas %>% file.path("anio_test.rds"))  # lo reusa 04
write_csv(tabla_resultados, dir_salidas %>% file.path("metricas_validacion.csv"))
write_csv(comparacion_specs,
          dir_salidas %>% file.path("comparacion_especificaciones.csv"))

cat("\nListo. Modelos de NIVEL guardados en",
    dir_salidas %>% file.path("modelos_escolares.rds"), "\n")
cat("Siguiente paso: 02b_modelo_dispersion.R (modelo del ANCHO de la distribución)\n")


