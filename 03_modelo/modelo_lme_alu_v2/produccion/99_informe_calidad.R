# =============================================================
# 99_informe_calidad.R
# -------------------------------------------------------------
# Genera un informe HTML con el veredicto de la corrida: si el resultado
# se puede publicar, y si no, qué mirar.
#
# Existe porque el modelo tiene modos de falla que no rompen nada: puede
# devolver números plausibles pero sesgados si el banco de ítems del año
# nuevo no es comparable con el de los anteriores. Un semáforo obliga a
# mirar eso antes de que las cifras salgan a un colegio.
#
# El HTML se arma con R base a propósito: sin rmarkdown ni pandoc, que
# serían dos dependencias más que instalar.
#
# Lo llama el driver, que define antes:
#   INFORME_DIR_SALIDAS, INFORME_ANIO, INFORME_DESTINO, INFORME_TIEMPOS
# =============================================================

suppressMessages({library(dplyr); library(tidyr)})

if (!exists("INFORME_DIR_SALIDAS")) stop("Falta definir INFORME_DIR_SALIDAS")
dir_salidas <- INFORME_DIR_SALIDAS
anio        <- INFORME_ANIO
destino     <- INFORME_DESTINO
tiempos     <- if (exists("INFORME_TIEMPOS")) INFORME_TIEMPOS else NULL

# --- Umbrales ---------------------------------------------------------
# El del banco es el que más importa: 3 puntos de logro son del orden del
# sesgo entre años que el modelo ya arrastra como problema abierto.
UMBRAL_BANCO_AVISO   <- 3     # puntos de logro
UMBRAL_BANCO_ALERTA  <- 6
UMBRAL_DESVIO_AVISO  <- 8     # puntos SIMCE respecto del último año observado
UMBRAL_DESVIO_ALERTA <- 15
UMBRAL_SIN_PRED      <- 0.05  # fracción de colegios sin predicción

# --- Utilidades de HTML -----------------------------------------------
esc <- function(x) {
  x <- as.character(x)
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

tabla_html <- function(d, digitos = 2) {
  if (is.null(d) || nrow(d) == 0) return("<p class='vacio'>Sin datos.</p>")
  d <- as.data.frame(d)
  for (j in seq_along(d)) if (is.numeric(d[[j]])) d[[j]] <- round(d[[j]], digitos)
  filas <- apply(d, 1, function(r)
    paste0("<tr>", paste0("<td>", esc(r), "</td>", collapse = ""), "</tr>"))
  paste0("<table><thead><tr>",
         paste0("<th>", esc(names(d)), "</th>", collapse = ""),
         "</tr></thead><tbody>", paste(filas, collapse = ""), "</tbody></table>")
}

# estado: "ok" | "aviso" | "alerta"
bloque <- function(estado, titulo, cuerpo, accion = NULL) {
  etiqueta <- c(ok = "EN ORDEN", aviso = "REVISAR", alerta = "ATENCIÓN")[[estado]]
  paste0("<div class='bloque ", estado, "'>",
         "<div class='cab'><span class='chip'>", etiqueta, "</span>",
         "<h2>", esc(titulo), "</h2></div>",
         "<div class='cuerpo'>", cuerpo,
         if (!is.null(accion)) paste0("<p class='accion'><b>Qué hacer:</b> ",
                                      accion, "</p>") else "",
         "</div></div>")
}

leer <- function(nombre) {
  r <- file.path(dir_salidas, nombre)
  if (!file.exists(r)) return(NULL)
  if (grepl("\\.csv$", nombre)) utils::read.csv(r, stringsAsFactors = FALSE)
  else readRDS(r)
}

secciones <- character(0)
peor <- "ok"
subir <- function(e) {
  orden <- c(ok = 1, aviso = 2, alerta = 3)
  if (orden[[e]] > orden[[peor]]) peor <<- e
}

# ---- 1. Qué se leyó --------------------------------------------------
irt_items  <- leer("irt_items.rds")
ind_feat   <- leer("ind_features.rds")
pred_col   <- leer("predicciones_colegio.rds")
pred_ind   <- leer("predicciones_individual.rds")

formas <- if (!is.null(irt_items)) {
  irt_items %>%
    filter(agno == anio) %>%
    group_by(grado, area) %>%
    summarise(formas = n_distinct(forma), items = n(), .groups = "drop")
} else NULL

alumnos <- if (!is.null(ind_feat)) {
  ind_feat %>%
    filter(agno == anio) %>%
    group_by(grado, area) %>%
    summarise(colegios = n_distinct(rbd_revisado),
              estudiantes = n(),
              ensayos_por_alumno = round(mean(k_ensayos), 2),
              .groups = "drop")
} else NULL

leido <- if (!is.null(formas) && !is.null(alumnos)) {
  full_join(formas, alumnos, by = c("grado", "area"))
} else NULL

secciones <- c(secciones, bloque(
  "ok", paste0("Qué se leyó del año ", anio),
  paste0("<p>Confirme que estas cifras coinciden con lo que entregó. Si falta ",
         "un ensayo o sobra un grupo, el problema está en los archivos de ",
         "entrada, no en el modelo.</p>", tabla_html(leido))
))

# ---- 2. Comparabilidad del banco -------------------------------------
# El riesgo central de una ronda con pocas formas.
banco <- if (!is.null(irt_items)) {
  irt_items %>%
    filter(is.finite(a), is.finite(b)) %>%
    group_by(agno, grado, area) %>%
    summarise(formas = n_distinct(forma), items = n(),
              logro_esperado = 100 * mean(plogis(a * (0 - b))),
              .groups = "drop")
} else NULL

if (!is.null(banco) && anio %in% banco$agno) {
  salto <- banco %>%
    group_by(grado, area) %>%
    arrange(agno, .by_group = TRUE) %>%
    summarise(formas_nuevo = last(formas),
              nivel_nuevo  = last(logro_esperado),
              nivel_previo = if (n() > 1) mean(logro_esperado[-n()]) else NA_real_,
              .groups = "drop") %>%
    mutate(salto = nivel_nuevo - nivel_previo)

  m <- suppressWarnings(max(abs(salto$salto), na.rm = TRUE))
  est <- if (!is.finite(m) || m < UMBRAL_BANCO_AVISO) "ok"
         else if (m < UMBRAL_BANCO_ALERTA) "aviso" else "alerta"
  subir(est)

  secciones <- c(secciones, bloque(
    est, "Comparabilidad del banco de ítems entre años",
    paste0(
      "<p><code>mean_logro</code> es el porcentaje del banco de ítems del año ",
      "que un colegio contestaría bien. Si el banco cambia de composición ",
      "—sobre todo cuando cambia el número de formas— el nivel se corre y el ",
      "modelo lo lee como si los colegios hubieran cambiado.</p>",
      "<p><i>nivel_esperado</i> es lo que sacaría el estudiante promedio de ese ",
      "año contra el banco de ese año. Mezcla dificultad del banco con nivel de ",
      "la cohorte: mide el movimiento, no lo explica.</p>",
      tabla_html(banco %>% select(agno, grado, area, formas, items, logro_esperado)),
      "<p><b>Salto del año nuevo respecto del promedio de los anteriores:</b></p>",
      tabla_html(salto %>% select(grado, area, formas_nuevo, nivel_previo,
                                  nivel_nuevo, salto))
    ),
    accion = if (est == "ok")
      "Nada. El banco se mantiene comparable."
    else paste0("El salto supera los ", UMBRAL_BANCO_AVISO, " puntos en al menos ",
                "un grupo. Las predicciones de esos grupos pueden estar corridas ",
                "en la misma dirección. Considere usar la especificación centrada ",
                "(<code>C</code> en 02_modelo_escolar.R) o interprete el nivel ",
                "absoluto con cautela y quédese con el ordenamiento.")
  ))
}

# ---- 3. Cobertura de la predicción -----------------------------------
if (!is.null(pred_col)) {
  cob <- pred_col %>%
    group_by(grado, area) %>%
    summarise(colegios = n(),
              sin_prediccion = sum(is.na(pred_simce_colegio)),
              sd_por_modelo = sum(origen_sd == "modelo"),
              sd_historica  = sum(origen_sd != "modelo"),
              .groups = "drop") %>%
    mutate(pct_sin = sin_prediccion / colegios)

  est <- if (max(cob$pct_sin) > UMBRAL_SIN_PRED) "aviso" else "ok"
  subir(est)

  secciones <- c(secciones, bloque(
    est, "Cobertura: cuántos colegios quedaron con predicción",
    paste0("<p>Un colegio queda sin predicción si su grupo no tiene modelo ",
           "entrenado. La columna <i>sd_historica</i> cuenta los que usaron su ",
           "propia dispersión histórica en vez del modelo, que es un respaldo ",
           "válido pero menos preciso.</p>",
           tabla_html(cob %>% select(grado, area, colegios, sin_prediccion,
                                     sd_por_modelo, sd_historica))),
    accion = if (est == "ok") "Nada."
             else "Revise los colegios sin predicción en predicciones_colegio.csv."
  ))
}

# ---- 4. Las predicciones, contra el último año observado -------------
smd <- leer("school_model_data.rds")
if (!is.null(pred_col) && !is.null(smd)) {
  ultimo_obs <- max(smd$agno)
  comparacion <- pred_col %>%
    group_by(grado, area) %>%
    summarise(pred_media = mean(pred_simce_colegio, na.rm = TRUE),
              sd_media   = mean(sd_simce_pred, na.rm = TRUE),
              .groups = "drop") %>%
    left_join(
      smd %>% filter(agno == ultimo_obs) %>%
        group_by(grado, area) %>%
        summarise(observado = mean(promedio_simce), .groups = "drop"),
      by = c("grado", "area")) %>%
    mutate(diferencia = pred_media - observado)

  m <- suppressWarnings(max(abs(comparacion$diferencia), na.rm = TRUE))
  est <- if (!is.finite(m) || m < UMBRAL_DESVIO_AVISO) "ok"
         else if (m < UMBRAL_DESVIO_ALERTA) "aviso" else "alerta"
  subir(est)

  secciones <- c(secciones, bloque(
    est, paste0("Predicciones ", anio, " comparadas con el SIMCE ", ultimo_obs),
    paste0("<p>No tienen por qué coincidir: cambian los colegios de la base y ",
           "puede haber movimiento real. Pero una diferencia grande y del mismo ",
           "signo en todos los grupos es señal de que algo se corrió.</p>",
           tabla_html(comparacion)),
    accion = if (est == "ok") "Nada."
             else "Contraste con el salto de banco de la sección anterior: si van en la misma dirección, es deriva de escala y no cambio real."
  ))
}

# ---- 5. Niveles de aprendizaje ---------------------------------------
if (!is.null(pred_ind)) {
  cortes <- data.frame(
    grado = c("4b","4b","2m","2m"),
    area  = c("lenguaje","matematica","lenguaje","matematica"),
    corte_elemental = c(241, 245, 250, 252),
    corte_adecuado  = c(284, 295, 295, 319),
    stringsAsFactors = FALSE
  )
  niveles <- pred_ind %>%
    left_join(cortes, by = c("grado", "area")) %>%
    mutate(nivel = case_when(pred_B >= corte_adecuado  ~ "Adecuado",
                             pred_B >= corte_elemental ~ "Elemental",
                             TRUE                      ~ "Insuficiente")) %>%
    count(grado, area, nivel) %>%
    group_by(grado, area) %>%
    mutate(pct = round(100 * n / sum(n), 1)) %>%
    ungroup() %>%
    select(-n) %>%
    pivot_wider(names_from = nivel, values_from = pct)

  secciones <- c(secciones, bloque(
    "ok", "Distribución predicha por Nivel de Aprendizaje (%)",
    paste0("<p>Es la lectura que usa el colegio. Los cortes son los oficiales ",
           "de los Estándares de Aprendizaje, no salen de estos datos.</p>",
           tabla_html(niveles, 1),
           "<p class='nota'>Los cortes de 2° medio figuran como no vigentes en el ",
           "portal del Mineduc. Conviene confirmarlos antes de publicar.</p>")
  ))
}

# ---- 6. Coherencia interna -------------------------------------------
chequeo <- leer("chequeo_coherencia.csv")
if (!is.null(chequeo)) {
  coh <- chequeo %>%
    group_by(grado, area) %>%
    summarise(desvio_de_la_media = mean(abs(dif_media_B), na.rm = TRUE),
              razon_sd = mean(razon_sd_B, na.rm = TRUE), .groups = "drop")

  est <- if (max(abs(coh$razon_sd - 1), na.rm = TRUE) > 0.1 ||
             max(coh$desvio_de_la_media, na.rm = TRUE) > 1) "aviso" else "ok"
  subir(est)

  secciones <- c(secciones, bloque(
    est, "Coherencia interna del reparto",
    paste0("<p>El promedio de las predicciones de un colegio debe reproducir la ",
           "predicción de ese colegio (desvío cerca de 0) y su dispersión debe ",
           "reproducir el ancho predicho (razón cerca de 1). Es un chequeo ",
           "aritmético: si falla, hay un problema de código, no de datos.</p>",
           tabla_html(coh, 3)),
    accion = if (est == "ok") "Nada." else "Avise a quien mantiene el modelo: no es un problema de insumos."
  ))
}

# ---- 7. Precisión esperada -------------------------------------------
met <- leer("metricas_validacion.csv")
val <- leer("validacion_distribucional.csv")
if (!is.null(met)) {
  tab <- met %>% select(grado, area, mae, r2_test, sesgo_test)
  cuerpo <- paste0(
    "<p>Esta corrida <b>no se valida contra nada</b>: el año ", anio, " todavía ",
    "no rindió la prueba. Las cifras de abajo son del último año cerrado y son ",
    "la mejor estimación disponible de cuánto se equivoca el modelo.</p>",
    "<p><b>Error a nivel de colegio</b> (puntos SIMCE):</p>", tabla_html(tab),
    if (!is.null(val)) paste0(
      "<p><b>Error de la distribución individual</b>, contra el baseline de ",
      "darle a todos el promedio del colegio:</p>",
      tabla_html(val %>% select(grado, area, qmae_B, qmae_solo_media))) else "")

  est <- if (any(met$sesgo_test > 2, na.rm = TRUE) &&
             all(met$sesgo_test > 0, na.rm = TRUE)) "aviso" else "ok"
  subir(est)

  secciones <- c(secciones, bloque(
    est, "Precisión esperada del modelo",
    cuerpo,
    accion = if (est == "ok") "Nada."
      else paste0("El sesgo es positivo en todos los grupos: el modelo tiende a ",
                  "sobrepredecir. Es un problema conocido y abierto (deriva del ",
                  "ensayo entre años). Considérelo al leer los niveles absolutos.")
  ))
}

# ---- 8. Armado del HTML ----------------------------------------------
veredicto <- switch(peor,
  ok     = list(t = "SE PUEDE PUBLICAR",
                d = "Todos los chequeos pasaron. Las predicciones están en la carpeta de entrega."),
  aviso  = list(t = "REVISAR ANTES DE PUBLICAR",
                d = "Hay chequeos marcados en amarillo. Nada impide usar los resultados, pero conviene leer esas secciones antes de entregarlos."),
  alerta = list(t = "NO PUBLICAR SIN REVISAR",
                d = "Hay al menos un chequeo en rojo. Los números pueden estar sistemáticamente corridos. Lea las secciones marcadas.")
)

tiempo_txt <- if (!is.null(tiempos)) paste0(
  "<p class='nota'>Tiempos: ",
  paste(sprintf("%s %.0f s", names(tiempos), tiempos), collapse = " &middot; "),
  "</p>") else ""

css <- "
body{font-family:'Segoe UI',system-ui,sans-serif;max-width:1000px;margin:0 auto;
 padding:32px 24px;color:#1c2b33;background:#f7f6f1;line-height:1.55}
h1{font-size:1.8rem;margin:0 0 4px}
.sub{color:#5b6b73;margin:0 0 28px}
.veredicto{padding:20px 24px;border-radius:10px;margin-bottom:28px;color:#fff}
.veredicto h2{margin:0 0 6px;font-size:1.35rem}
.veredicto p{margin:0}
.v-ok{background:#1f7a5c}.v-aviso{background:#b8860b}.v-alerta{background:#b3402f}
.bloque{background:#fff;border-radius:10px;margin-bottom:18px;overflow:hidden;
 border:1px solid #e3e8e6}
.cab{display:flex;align-items:center;gap:12px;padding:14px 20px;
 border-bottom:1px solid #eef2f0}
.cab h2{margin:0;font-size:1.08rem;font-weight:600}
.chip{font-size:.7rem;font-weight:700;letter-spacing:.06em;padding:4px 9px;
 border-radius:20px;color:#fff;white-space:nowrap}
.ok .chip{background:#1f7a5c}.aviso .chip{background:#b8860b}.alerta .chip{background:#b3402f}
.ok .cab{background:#f2f9f6}.aviso .cab{background:#fdf7e8}.alerta .cab{background:#fdf1ef}
.cuerpo{padding:16px 20px}
.cuerpo p{margin:0 0 12px}
table{border-collapse:collapse;width:100%;margin:12px 0;font-size:.9rem}
th{background:#f2f5f4;text-align:left;padding:7px 10px;font-weight:600;
 border-bottom:2px solid #e3e8e6}
td{padding:6px 10px;border-bottom:1px solid #eef2f0}
tbody tr:last-child td{border-bottom:none}
code{background:#f2f5f4;padding:1px 5px;border-radius:3px;font-size:.88em}
.accion{background:#f7f9f8;border-left:3px solid #1f7a8c;padding:10px 14px;
 margin:14px 0 0;border-radius:0 4px 4px 0}
.nota{font-size:.85rem;color:#5b6b73}
.vacio{color:#8ca3a8;font-style:italic}
footer{margin-top:32px;padding-top:16px;border-top:1px solid #e3e8e6;
 font-size:.83rem;color:#5b6b73}
"

html <- paste0(
  "<!doctype html><html lang='es'><head><meta charset='utf-8'>",
  "<meta name='viewport' content='width=device-width,initial-scale=1'>",
  "<title>Informe de calidad ", anio, "</title><style>", css, "</style></head><body>",
  "<h1>Predicción SIMCE ", anio, "</h1>",
  "<p class='sub'>Informe de calidad de la corrida &middot; ",
  format(Sys.time(), "%d-%m-%Y %H:%M"), "</p>",
  "<div class='veredicto v-", peor, "'><h2>", veredicto$t, "</h2><p>",
  veredicto$d, "</p></div>",
  paste(secciones, collapse = ""),
  "<footer>Generado por 99_informe_calidad.R. El detalle técnico de la corrida ",
  "está en log_corrida.txt, en esta misma carpeta.", tiempo_txt, "</footer>",
  "</body></html>"
)

writeLines(html, destino, useBytes = TRUE)
cat("          informe escrito:", destino, "\n")
cat("          veredicto:", veredicto$t, "\n")
