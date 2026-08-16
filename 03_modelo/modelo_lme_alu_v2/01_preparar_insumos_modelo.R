# =============================================================
# 01_preparar_insumos_modelo.R  (v10)
# -------------------------------------------------------------
# CAMBIO v10 — SEXO DEL ALUMNO, SÓLO PARA VALIDAR.
#
# Entran dos columnas nuevas y ninguna toca la predicción:
#
#   - `sexo` en `ind_features`: estimado del nombre con el registro de
#     nombres de nacimiento (paquete `guaguas`), en
#     02_cargar_y_consolidar_ensayos.R. Cobertura 99.0%.
#   - `sexo` en `simce_alumno`: administrativo, del archivo de la Agencia.
#     Venía en la fuente desde siempre pero el consolidado lo descartaba;
#     01_cargar_y_consolidar_simce.R ahora lo conserva y homologa (en 2022
#     la columna se llama `gen_alu`, desde 2023 `sexo`).
#
# PARA QUÉ. Es el primer atributo individual presente en las DOS fuentes a
# la vez, así que permite validar la ASIGNACIÓN dentro del colegio, que
# hasta ahora era un punto ciego: el error de cuantiles no cambia si se
# reordena a los estudiantes entre sí. La sección 3c de 04 hace esa
# validación.
#
# POR QUÉ NO ENTRA COMO PREDICTOR. Se midió: la brecha de sexo atraviesa
# el pipeline intacta dentro de theta (en 4b matemática, 0.297 sd en
# theta -> 0.299 en el índice -> 0.287 en la predicción final), o sea que
# el ensayo YA la lleva. Lo que el modelo no captura es la diferencia
# entre la brecha del ensayo y la del SIMCE, y corregirla mejora el error
# de cuantiles entre 0.00 y 0.21 puntos sobre errores de 11 a 18. Ver el
# encabezado de 04 para el detalle.
#
# -------------------------------------------------------------
# CAMBIOS v9 — SIMPLIFICACIÓN. Seis cambios, cuatro de ellos acá.
#
# El criterio de toda esta tanda: quitar, no agregar. Con tres años de
# ensayo, dos ventanas out-of-time y cuatro grupos, elegir entre muchas
# alternativas mirando el MAE es una forma eficiente de sobreajustar. Una
# ELIMINACIÓN, en cambio, se testea contra la hipótesis nula de que no
# cambia nada, que es un test mucho más benigno. Todo lo que sigue se
# eliminó por argumento metodológico y se verificó que no movía nada.
#
# (1) SALE EL MODELO DE CRECIMIENTO (ex sección 4). Eran 12 modelos
#     mixtos sobre decenas de miles de filas, y sus tres salidas
#     —`pred_final_logro`, `slope_logro`, `nivel_est`— no entraban en
#     ninguna fórmula de producción: 02 sacó `slope_logro` en la v5 y
#     `pred_final_logro` en la v7 por colinealidad (correlación 0.995 a
#     0.999 con `mean_logro_enc`, VIF de 73 a 360). Sobrevivían como
#     columnas de auditoría. Con el modelo se va también su insumo,
#     `irt_theta_forma.rds`, y con él el bloque más lento de 00.
#
# (2) EL ÍNDICE INDIVIDUAL (sección 4) SE VUELVE UNIVARIADO. La versión
#     anterior era un BLUP bivariado que componía el área propia con la
#     complementaria. Medido sobre las salidas de la v8, esa composición
#     aporta entre 0.003 y 0.009 de confiabilidad. El "+4 a +10 puntos"
#     que prometía el comentario original es real pero ANTERIOR AL IRT:
#     se calculó cuando la precisión del alumno se aproximaba por
#     sigma^2/k. Con theta, `se_theta` ya deja la confiabilidad propia en
#     0.82-0.91 y no queda nada que la otra área pueda agregar. Dos
#     piezas resolvían el mismo problema; queda la que lo resuelve mejor.
#
#     Además, el ordenamiento que produce el índice es indistinguible de
#     rankear por theta a secas (Spearman dentro del colegio: mediana
#     0.995-0.998). Como la predicción individual usa sólo el RANGO
#     (`pct_ensayo`), el encogimiento que queda importa para interpretar,
#     no para la predicción. Se conserva porque es correcto y ahora cabe
#     en tres líneas.
#
# (3) SE CONGELA EL IRT Y SE BORRA LA RAMA CRUDA. `USAR_IRT` era un
#     switch, y el script arrastraba cada medida en dos o tres versiones
#     (cruda / IRT / theta) por todo el pipeline. La comparación entre
#     ambas (ex sección 8c) fue valiosa una vez y su resultado está
#     documentado; mantenerla como doble vía permanente cuesta la mitad
#     de las columnas de `school_features` y triplica los nombres para la
#     misma cantidad. También sale el ajuste por `mean_dffclt` de la
#     sección 2, que bajo IRT era una corrección doble y cuyos valores no
#     son comparables entre formas (salen de un 3PL por ensayo).
#
# (4) SE ELIMINAN LOS SWITCHES. `USAR_IRT`, `AJUSTAR_DIFICULTAD` y la
#     reparametrización de `rho` eran grados de libertad del investigador
#     sobre un test de dos ventanas. Cada switch que queda en el código
#     es una invitación a re-sintonizar sobre el mismo año de prueba.
#
# (6) SALE `rho` Y CON ÉL LA VERSIÓN A. 03 produce sólo la versión B
#     (matching por percentil), que no necesita ningún parámetro libre.
#     `rho` no era estimable —no hay vínculo alumno-a-alumno entre ensayo
#     y SIMCE— y salía de reparametrizar un 0.70 elegido en la v4: era el
#     número menos defendible del pipeline. La sección 6d conserva las
#     confiabilidades como diagnóstico, sin `rho_sugerido`.
#
# Lo que NO se tocó, y conviene decir por qué: la plantilla `forma_z` por
# tercil (sección 6c) se revisó y se deja. Las plantillas de tercil bajo y
# alto difieren en 0.06-0.14 z en la mediana y hasta 0.49-0.71 z en las
# colas, tiene respaldo teórico (efecto piso/techo) y es una tabla de
# consulta, no un modelo: no cuesta grados de libertad.
#
# -------------------------------------------------------------
# (v5) Renombres que separan el histórico del NIVEL del de la DISPERSIÓN:
#         colegio_efecto_historico -> nivel_hist_colegio
#         desvio_colegio           -> desvio_nivel
#         n_anios_hist             -> n_anios_nivel_hist
#         efecto_historico(.rds)   -> nivel_historico(.rds)
#     Los de dispersión ya estaban bien: sd_hist_colegio, desvio_sd,
#     contexto_sd, n_anios_sd_hist.
#
# -------------------------------------------------------------
# (v4) SIMCE por alumno + contexto del colegio
# -------------------------------------------------------------
# NOVEDAD v4: se incorporan las variables de contexto que trae el
# archivo por RBD y que no se estaban usando:
#
#   cod_grupo      GSE: 1 Bajo, 2 Medio bajo, 3 Medio, 4 Medio alto, 5 Alto
#   cod_depe1      1 Municipal Corporación, 2 Municipal DAEM,
#                  3 Particular subvencionado, 4 Particular pagado,
#                  5 Corporación de administración delegada,
#                  6 Servicio Local de Educación
#   cod_depe2      versión colapsada de la anterior
#   cod_rural_rbd  1 Urbano, 2 Rural
#
# CÓMO ENTRAN, Y POR QUÉ NO COMO SIMPLES PREDICTORES MÁS:
#
# Probadas como columnas adicionales de la regresión final (por
# grado x área, ~150-220 colegios) no aportan casi nada: el MAE
# out-of-time se mueve entre -0.2 y +0.3 puntos, y en un grupo
# empeora. La razón es que `nivel_hist_colegio` YA las
# contiene: el nivel histórico de un colegio es, en buena medida, su
# GSE y su dependencia (la expectativa contextual correlaciona 0.70
# con el nivel histórico). Sumadas aparte sólo gastan grados de
# libertad en una regresión que tiene pocas filas.
#
# Donde sí valen oro es donde la historia falla: sin efecto
# histórico, el contexto sube el R² out-of-time de 0.40 a 0.62-0.66.
# Son un buen SUSTITUTO de la historia, no un complemento de ella.
#
# Por eso entran como PRIOR del efecto histórico. El modelo pasa de
#
#     promedio_simce ~ factor(agno) + (1 | rbd)
# a
#     promedio_simce ~ factor(agno) + GSE + dependencia + ruralidad + (1 | rbd)
#
# y el efecto del colegio se reconstruye como
#
#     nivel_hist_colegio = expectativa_contextual + desvío_del_colegio
#
# Esto arregla tres cosas de una vez:
#   - Colegios SIN historia previa: antes recibían 0 (= "promedio
#     nacional"), mala suposición tanto para un particular pagado GSE
#     alto como para un rural GSE bajo. Ahora reciben lo que se espera
#     de un colegio de su contexto.
#   - Colegios con UN año de historia: el encogimiento de lme4 los tira
#     hacia el promedio de su CONTEXTO, no del país. Prior correcto.
#   - Colegios con historia larga: casi no cambian, que es lo que
#     corresponde — ahí manda la evidencia propia.
#
# El modelo de contexto se estima sobre el universo NACIONAL (~6.400
# colegios de 4b, ~3.000 de 2m, todos los GSE, 30% de ruralidad, las 6
# dependencias), no sobre la base Santillana, que está muy concentrada
# (57-63% particular pagado, GSE promedio ~4, 1-4% rural). Estimar el
# efecto del GSE con 180 colegios de los cuales casi ninguno es rural
# sería pedirle a los datos algo que no tienen: se estiman los
# coeficientes donde hay variación y se transfieren.
#
# Efecto lateral útil: como el contexto llega a los modelos finales
# convertido en UN número, desaparece el problema de las CATEGORÍAS
# AUSENTES. Con 150-220 colegios por grupo es perfectamente posible
# que "corporación de administración delegada" o "rural" no aparezca
# en entrenamiento y sí en el año de prueba, lo que haría fallar
# predict(). Acá no puede pasar.
#
# CONTEXTO REZAGADO: el GSE se recalcula en cada medición y cambia de
# categoría en ~26% de los colegios de un año a otro; la dependencia
# se movió 8% entre 2024 y 2025 por el paso a Servicio Local. Como
# ambas se publican JUNTO con los resultados del año, usar las del año
# objetivo sería filtrar información que no existe al momento de
# predecir. Se usa siempre el último registro ESTRICTAMENTE anterior.
#
# NO cambió la forma de la distribución interna (`forma_z`): se probó
# condicionarla por GSE dentro de cada tercil de nivel y las
# diferencias son de ≤0.06 en z (~2 puntos SIMCE). El tercil de nivel
# ya captura lo que el GSE aportaría.
#
# -------------------------------------------------------------
# (v3) Cambio anterior respecto a la v2: se carga
# `consolidado_datos_simce_alu.parquet`, que trae el SIMCE de
# CADA ESTUDIANTE (2022-2025, 4b y 2m, matemática y lectura, más
# el error estándar de medición eem_*). Se verificó que reproduce
# los promedios por colegio del archivo RBD casi exactamente
# (diferencia mediana ~0.27 pts), o sea es el mismo universo pero
# desagregado.
#
# Eso permite dos cosas que antes eran imposibles:
#
#   1) MODELAR LA DISPERSIÓN: además del promedio del colegio,
#      ahora existe `sd_simce` observada (desviación estándar de
#      los puntajes individuales dentro de cada colegio). Es el
#      target del modelo de 02b_modelo_dispersion.R.
#
#   2) MATCHING POR PERCENTIL: se conoce la FORMA real de la
#      distribución interna de puntajes de un colegio, no sólo su
#      promedio. Se resume como una función de cuantiles sobre el
#      puntaje estandarizado dentro del colegio,
#      z = (ptje_alumno - media_colegio) / sd_colegio,
#      que resulta ser muy estable entre colegios y años (por eso
#      se puede usar como plantilla). Se guarda por (grado, área)
#      y por tercil de nivel del colegio, porque los colegios de
#      bajo rendimiento tienen colas algo más asimétricas a la
#      derecha y los de alto, a la izquierda.
#
# Ambas cosas se calculan con VENTANA EXPANSIVA (sólo años
# estrictamente anteriores al año que se quiere predecir), igual
# que `nivel_hist_colegio`, para no filtrar información del
# futuro hacia el pasado.
#
# Salidas (en `dir_salidas` = <ruta_outputs>/modelo_lme_alu_v2/):
#   - ind_features.rds      : 1 fila por estudiante x año x grado x área.
#                             `pct_ensayo` (percentil dentro del colegio,
#                             lo único que consume la predicción) y
#                             `z_ensayo` = `indice_ensayo`, la posición
#                             encogida por confiabilidad. OJO: `z_ensayo`
#                             NO tiene sd 1 dentro del colegio; ver la
#                             nota de escala en la sección 4.
#   - school_features.rds   : 1 fila por colegio x año x grado x área.
#                             `mean_logro` (nivel del colegio en el
#                             ensayo, en puntos de logro sobre el banco
#                             completo) y `mean_logro_enc`, su versión
#                             encogida por confiabilidad.
#   - school_model_data.rds : school_features + promedio_simce Y sd_simce
#   - nivel_historico.rds   : efecto persistente del colegio (nivel)
#   - simce_alumno.rds      : SIMCE individual en formato largo
#   - simce_dist.rds        : media, sd y cuantiles observados por colegio
#   - forma_z.rds           : plantilla de forma de la distribución interna
#   - cortes_tercil.rds     : cortes para asignar tercil de nivel
#   - confiabilidades.rds   : confiabilidad del SIMCE y del índice
#                             individual. Diagnóstico: desde la v9 ningún
#                             script aguas abajo lo necesita para predecir.
# =============================================================

library(tidyverse)
library(arrow)
library(readxl)
library(lme4)

# ---- 0. Configuración --------------------------------------------
usuario <- Sys.info()[["user"]]
rutas <- config::get(config = usuario, file = "config.yml")
ruta_data_in <- rutas$ruta_data_in
ruta_data_intermedia <- rutas$ruta_data_intermedia
ruta_outputs <- rutas$ruta_outputs

dir_salidas <- ruta_outputs %>% file.path('modelo_lme_alu_v2')
dir_salidas %>% dir.create(showWarnings = FALSE)

# Parámetros -------------------------------------------------------
# Mínimo de alumnos con SIMCE para que la sd interna del colegio sea
# una estimación utilizable
MIN_ALU_SD    <- 15
GRADOS_MODELO <- c("4b", "2m")   # los únicos con ensayo Santillana
# Grilla de percentiles con que se guarda la plantilla de forma.
GRILLA_P      <- seq(0.005, 0.995, by = 0.005)

# --- La medida base del ensayo (congelada en v9) --------------------------
# Es siempre la habilidad estimada por la calibración IRT concurrente de
# 00_irt_calibracion.R. Entra en los dos niveles:
#
#   - INDIVIDUAL (secc. 4): la medida es theta y su error es `se_theta^2`,
#     que la calibración entrega por estudiante. La precisión ya no se
#     aproxima por CUÁNTOS ensayos rindió: se sabe directamente, y depende
#     también de CUÁLES rindió y de cuánto discriminaban esos ítems.
#
#   - ESCOLAR (secc. 5b): `mean_logro` es el promedio del PUNTAJE
#     VERDADERO del estudiante — el porcentaje del banco completo de ítems
#     del año que contestaría bien dado su theta (curva característica del
#     test evaluada en theta). Está en puntos de porcentaje de logro, así
#     que 02, 02b y 03 leen la misma escala de siempre, pero ya no depende
#     de QUÉ ensayos aplicó el colegio. Ésa era la contaminación medida: la
#     dificultad de la batería explicaba entre 2.4% y 13.4% de la varianza
#     de `mean_logro` entre colegios.
#
# Hasta la v8 esto era el switch `USAR_IRT` y el script calculaba TODO por
# duplicado (crudo e IRT). La comparación está documentada en el encabezado
# y ya no se recalcula en cada corrida.

# --- Parámetros del índice individual encogido (sección 4) ---------------
# Mínimo de estudiantes para estimar la varianza verdadera entre alumnos
# DENTRO de un colegio. Bajo este umbral se usa la del grupo completo.
MIN_EST_TAU        <- 5

# Piso para tau^2 como fracción de la varianza observada: evita que un
# colegio donde el ruido explica toda la varianza quede con tau^2 = 0 y,
# por lo tanto, con todos sus estudiantes encogidos exactamente al
# promedio (lo que borraría el ordenamiento interno).
PISO_TAU2          <- 0.05

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

# ---- 1. Cargar datos -----------------------------------------------

## ENSAYOS ----
ensayos_santillana0 <- ruta_data_intermedia %>%
  file.path('ensayo_santillana', 'ensayos_santillana_corregido.parquet') %>%
  read_parquet()

ensayos <- ensayos_santillana0 %>%
  mutate(agno = as.numeric(agno)) |>
  filter(!outlier_iqr, !outlier_isoforest)

## SIMCE AGREGADO POR COLEGIO ----
simce0_rbd <- ruta_data_intermedia %>%
  file.path('simce', 'resultados_simce_rbd_corregido.parquet') %>%
  read_parquet()

simce <- simce0_rbd %>%
  filter(!outlier_iqr, !outlier_isoforest)

## IRT: habilidad por estudiante y parámetros de ítem (v6) ----
# Los produce 00_irt_calibracion.R. theta está en una escala común entre
# los ensayos de un mismo año x grado x área (media 0, sd 1 en la cohorte
# Santillana de ese año), NO entre años.
ruta_irt_theta <- dir_salidas %>% file.path("irt_theta.rds")
ruta_irt_items <- dir_salidas %>% file.path("irt_items.rds")

if (!file.exists(ruta_irt_theta) || !file.exists(ruta_irt_items)) {
  stop("Faltan las salidas de la calibración IRT (", ruta_irt_theta,
       "). Hay que correr antes 00_irt_calibracion.R.")
}

irt_theta <- readRDS(ruta_irt_theta)
irt_items <- readRDS(ruta_irt_items)

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

## SIMCE POR ALUMNO ----
ruta_alu <- ruta_data_intermedia %>%
  file.path('simce', 'consolidado_datos_simce_alu.parquet')

simce_alu0 <- read_parquet(ruta_alu)

## SEXO DEL ALUMNO EN EL ENSAYO (v10) ----
# Estimado a partir del nombre en 02_cargar_y_consolidar_ensayos.R, con el
# registro de nombres de nacimiento (paquete `guaguas`): se asigna un sexo
# cuando más del 75% de las personas con ese nombre lo tienen. Cobertura
# 99.0-99.1% de los estudiantes del ensayo, con un balance de 49.8/49.2.
#
# NO entra en la predicción. Se usa sólo para la validación estratificada
# de 04, que es lo que esta variable habilita de verdad: comprobar si la
# distribución que el modelo predice para cada sexo reproduce la observada.
# La justificación de por qué no entra como predictor está en el
# encabezado de 04; el resumen es que la brecha ya viaja dentro del theta.
sexo_ensayo <- ensayos_santillana0 %>%
  distinct(id_usuario_curso, sexo) %>%
  filter(sexo %in% c("hombre", "mujer"))

cat("\nSexo estimado disponible para", nrow(sexo_ensayo), "estudiantes del ensayo\n")

# ---- 2. Limpieza de ensayos -----------------------------------------
ensayos_limpio <- ensayos %>%
  mutate(
    porcentaje_logro = pmin(porcentaje_logro, 100),
    n_evaluacion = as.integer(n_evaluacion)
  ) %>%
  filter(!is.na(rbd_revisado), !is.na(n_evaluacion), porcentaje_logro > 0)

# SALIÓ EN v9: el ajuste del logro por `mean_dffclt` (los parquet
# `tab_resumen_irt_*`). Bajo IRT era una corrección DOBLE —theta ya viene
# libre de la dificultad de la forma— y además de signo poco confiable:
# esos `mean_dffclt` salen de un 3PL ajustado por separado en CADA ensayo,
# así que cada uno está en su propia escala y no son comparables entre
# formas. Medido contra la calibración concurrente, el orden de dificultad
# que implican es correcto en 4b (Spearman 0.94-1.00) pero no en 2m (0.77).
# `porcentaje_logro` queda como viene de la fuente: se usa sólo como
# respaldo para los pocos estudiantes sin theta (secc. 3) y en los
# descriptivos.

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

# ---- 2b. SIMCE individual a formato largo ---------------------------
# El archivo viene ancho (una columna por área). Se pasa a largo para
# que calce con la llave (agno, grado, area, rbd_revisado) del resto
# del pipeline. Se descartan puntajes ausentes (~20% de las filas: son
# alumnos matriculados que no rindieron o quedaron excluidos).
simce_alumno <- simce_alu0 %>%
  filter(grado %in% GRADOS_MODELO) %>%
  mutate(agno = as.numeric(agno),
         rbd_revisado = as.numeric(rbd)) %>%
  pivot_longer(cols = c(ptje_mate, ptje_lect, eem_mate, eem_lect, eda_mate, eda_lect),
               names_sep = '_',
               names_to = c('.value', 'area')
               ) %>%
  mutate(area = ifelse(area == 'mate', 'matematica', 'lenguaje'),
         # Sexo administrativo (v10). Viene 1/2 desde el archivo de la
         # Agencia —en 2022 la columna se llama `gen_alu` y desde 2023
         # `sexo`; 01_cargar_y_consolidar_simce.R las homologa. Se pasa a
         # etiqueta para que calce con el sexo estimado del ensayo y para
         # que ningún resumen lo promedie como si fuera un número.
         sexo = case_when(sexo == 1 ~ "hombre",
                          sexo == 2 ~ "mujer",
                          TRUE      ~ NA_character_)) %>%
  # Se descartan efectivamente los puntajes ausentes (~20% de las filas).
  # No es cosmético: `simce_dist` contaría esos alumnos en `n_alu_simce`,
  # y las secciones que llaman a density() o quantile() sin na.rm (6c, 8b)
  # abortan si quedan NA.
  filter(!is.na(ptje))

rm(simce_alu0); gc()

cat("SIMCE individual cargado:", nrow(simce_alumno), "puntajes alumno x área\n")

# ---- 2c. Distribución interna observada por colegio ------------------
# Esta tabla es la "verdad" contra la que se validan las predicciones
# individuales en 04_validacion_individual.R.
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

# ---- 2d. Contexto del colegio y universo nacional (NUEVO) ------------
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
# dependencia y ruralidad, porque acá sí hay variación en esas
# variables (la base Santillana no la tiene).
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

# ---- 3. Resumen simple por estudiante --------------------------------
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

# SECCIÓN 4 (modelo de crecimiento): ELIMINADA EN v9 ---------------------
#
# Eran 12 modelos mixtos —el paso más caro del pipeline después de la
# calibración IRT— con la forma
#
#   logro_irt_eval ~ n_evaluacion_c + (1 | id_usuario_curso)
#                                   + (1 + n_evaluacion_c | rbd_revisado)
#
# proyectados al 6º ensayo. Producían `pred_final_logro`, `slope_logro` y
# `nivel_est`, y NINGUNA de las tres entraba en una fórmula de producción:
#
#   - `slope_logro` salió de 02 en la v5. Los ensayos no forman una
#     progresión, así que una pendiente sobre su secuencia mide sobre todo
#     cuáles se aplicaron. El IRT levantó esa objeción para la MEDIDA, pero
#     la variable nunca volvió a ninguna fórmula.
#   - `pred_final_logro` salió de 02 en la v7 por colinealidad: correlación
#     0.995-0.999 con `mean_logro_enc`, VIF de 73 a 360, y coeficientes que
#     hacían balancín (+31.6 y -28.4 en 2m matemática) al dejar un año
#     fuera del entrenamiento. Es esperable: sacada la pendiente, el
#     promedio por colegio de los interceptos encogidos por lme4 ES el
#     `mean_logro` del colegio encogido. Eran dos versiones de la misma
#     cantidad con dos mecanismos de encogimiento distintos.
#   - `nivel_est` no se usó nunca fuera de este bloque.
#
# El ordenamiento de los estudiantes dentro del colegio lo gobierna
# `indice_ensayo` (sección 4), no esta sección, así que su eliminación no
# toca la predicción individual. Con la sección se va su insumo
# `irt_theta_forma.rds` y con él el bloque 4d-bis de 00.

# ---- 4. ÍNDICE INDIVIDUAL ENCOGIDO POR CONFIABILIDAD ------------------
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
# CAMBIO v9 — SALE LA COMPOSICIÓN CON LA OTRA ÁREA. Hasta la v8 esto era
# un BLUP bivariado que además tomaba prestada información del área
# complementaria, con la correlación verdadera entre áreas estimada por
# desatenuación. Medido sobre las salidas de la v8, esa composición aporta
# entre 0.003 y 0.009 de confiabilidad. El "+4 a +10 puntos" que prometía
# el comentario original no era falso: era ANTERIOR AL IRT, cuando la
# precisión se aproximaba por sigma^2/k y la medida propia era mucho más
# ruidosa. Con theta la confiabilidad propia ya está en 0.82-0.91 y no
# queda nada que la otra área pueda agregar. Dos piezas resolvían el mismo
# problema y queda la que lo resuelve mejor.
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
est_entrada <- ensayos_dedup %>%
  group_by(agno, grado, area, rbd_revisado, id_usuario_curso) %>%
  summarise(k_ensayos = n(), .groups = "drop") %>%
  inner_join(
    irt_theta %>% select(agno, grado, area, id_usuario_curso, theta, se_theta,
                         logro_irt),
    by = c("agno", "grado", "area", "id_usuario_curso")
  ) %>%
  mutate(x_est = theta, var_err = se_theta^2)

cat("\n  Estudiantes con theta sobre el total del ensayo:",
    sprintf("%.1f%%",
            100 * nrow(est_entrada) / nrow(distinct(ensayos_dedup, agno, grado,
                                                    area, id_usuario_curso))), "\n")

est_indice <- construir_indice(est_entrada)

# ---- 5. Features a nivel de estudiante --------------------------------
# `pct_ensayo` es la posición del estudiante DENTRO de su propio colegio y
# es la materia prima de la predicción individual de 03: a cada estudiante
# se le asigna el puntaje que corresponde a su percentil.
#
# `z_ensayo` es el mismo índice sin convertir a percentil. Desde la v9 no
# lo consume ninguna predicción (se iba en la versión A, que salió junto
# con `rho`), pero se conserva porque es la magnitud interpretable: dice a
# cuántas desviaciones verdaderas del promedio de su colegio está el
# estudiante, ya descontado el ruido de medición.
ind_features <- resumen_simple %>%
  left_join(
    est_indice %>%
      select(agno, grado, area, rbd_revisado, id_usuario_curso,
             indice_ensayo, rel_indice, z_bruto),
    by = c("agno", "grado", "area", "rbd_revisado", "id_usuario_curso")
  ) %>%
  # Sexo estimado (v10). Viaja con el estudiante para que 04 pueda validar
  # por sexo. Insisto en que NO entra en ninguna fórmula: se verificó que
  # la brecha de sexo atraviesa el pipeline intacta dentro de theta
  # (0.297 -> 0.299 -> 0.287 sd en 4b matemática, de theta al índice y a la
  # predicción final), así que agregarla como predictor sería contarla dos
  # veces sobre la parte que el ensayo ya mide.
  left_join(sexo_ensayo, by = "id_usuario_curso")

cat("\nCobertura del sexo estimado en ind_features:",
    sprintf("%.1f%%", 100 * mean(!is.na(ind_features$sexo))), "\n")

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

# ---- 5b. Features a nivel de colegio ----------------------------------
# Tres features del ensayo por colegio: su NIVEL (`mean_logro`) y dos de
# DISPERSIÓN (`sd_entre_estud` e `iqr_logro_ensayo`, este último algo más
# robusto frente a colegios con outliers). Las de dispersión son
# predictores del modelo de 02b.
#
# CAMBIO v9: una sola versión de cada una, sobre el puntaje verdadero IRT.
# Hasta la v8 convivían las variantes `*_crudo` y `*_irt` y las columnas sin
# sufijo apuntaban a unas u otras según `USAR_IRT`. Eran nueve nombres para
# tres cantidades.
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

    # Para el encogimiento en escala theta (secc. 5b-bis): el promedio de
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

# ---- 5b-bis. Encogimiento de mean_logro por confiabilidad (NUEVO v5) --
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
# usar `mean_logro` en 02 en vez de `mean_logro_enc`.
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
# (CAMBIO v8.)
#
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
# única (0-100) que consumen 02, 02b y 03, pero el encogimiento en sí
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

# ---- 5b-ter. Nivel del ensayo CENTRADO DENTRO DEL AÑO (NUEVO v7) ------
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

# ---- 5c. Contexto REZAGADO de los colegios Santillana (NUEVO) ---------
# Para cada año objetivo, el último registro de contexto estrictamente
# anterior de ese mismo colegio, grado y área. El GSE cambia de categoría
# en ~26% de los colegios de un año a otro y la dependencia se movió 8%
# entre 2024 y 2025 (paso a Servicio Local), así que el rezago no es una
# formalidad: es la única versión de estas variables que existe al
# momento de predecir.
anios_objetivo <- school_features %>% distinct(agno, grado, area)

contexto_rezagado <- map_dfr(sort(unique(anios_objetivo$agno)), function(y) {
  contexto_crudo %>%
    filter(agno < y) %>%
    group_by(grado, area, rbd_revisado) %>%
    slice_max(agno, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    rename(agno_contexto = agno) %>%
    mutate(agno = y)
}) %>%
  preparar_factores()

cobertura_ctx <- school_features %>%
  left_join(contexto_rezagado %>% select(agno, grado, area, rbd_revisado, cod_grupo),
            by = c("agno", "grado", "area", "rbd_revisado")) %>%
  summarise(cobertura = mean(!is.na(cod_grupo))) %>%
  pull(cobertura)
cat(sprintf("\nCobertura del contexto rezagado en colegios Santillana: %.1f%%\n",
            100 * cobertura_ctx))

# Los colegios objetivo de cada grupo, con su contexto rezagado pegado.
colegios_objetivo <- function(a, g, ar) {
  school_features %>%
    filter(agno == a, grado == g, area == ar) %>%
    select(rbd_revisado) %>%
    left_join(
      contexto_rezagado %>%
        filter(agno == a, grado == g, area == ar) %>%
        select(rbd_revisado, cod_grupo, cod_depe1, cod_rural_rbd, f_gse, f_depe, f_rural),
      by = "rbd_revisado"
    )
}

# ---- 6. Efecto histórico del colegio, con prior contextual (MODIFICADO)
# El modelo se ajusta sobre el universo NACIONAL de años estrictamente
# anteriores:
#
#   promedio_simce ~ factor(agno) + GSE + dependencia + ruralidad + (1 | rbd)
#
# y para cada colegio objetivo devuelve:
#   contexto_nivel  = lo que se espera de un colegio de ese contexto,
#                     centrado en el promedio nacional del año de
#                     referencia (0 = "colegio nacional promedio").
#   desvio_nivel  = cuánto se desvía este colegio de esa expectativa,
#                     ya encogido por lme4 según cuánta historia tenga.
#   nivel_hist_colegio = la suma. Misma escala e interpretación
#                     que en la v3 (desviación en puntos respecto del
#                     colegio nacional promedio), así que los modelos de
#                     02 y 02b no necesitan cambiar de fórmula.
#
# Un colegio sin historia queda con desvio_nivel = 0, es decir "se
# comporta como los de su contexto" — bastante mejor prior que el 0 de
# la versión anterior, que equivalía a "se comporta como el país".
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

  # ¿Se puede estimar un intercepto aleatorio por colegio? Sólo si hay
  # más observaciones que colegios, es decir si al menos parte de los
  # colegios aparece en 2+ años. Con un solo año previo (caso de 2023,
  # que sólo tiene 2022 antes) hay exactamente una fila por colegio: el
  # efecto aleatorio no es identificable y lme4 aborta con
  # "number of levels of each grouping factor must be < number of
  # observations". La parte de contexto SÍ es estimable en ese caso
  # (miles de colegios reparten 5 GSE x 6 dependencias x 2 ruralidades),
  # así que se ajusta sólo la parte fija y el desvío del colegio se
  # calcula a mano como residuo encogido.
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
    # muestreo de su promedio (sd interna^2 / n alumnos). La proporción
    # que es señal es lambda = var_real / (var_real + var_error), y el
    # residuo se multiplica por ella. Sin esto, un colegio de 15 alumnos
    # con un promedio alto por azar arrastraría ese azar al año
    # siguiente como si fuera una característica suya.
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

  # Centrado: predicción media de la parte fija sobre el universo
  # nacional en el año de referencia.
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
  estimar_nivel_historico(simce_nacional, colegios_objetivo(a, g, ar), a, g, ar,
                           sd_tipica = sd_tipica_previa(a, g, ar)) %>%
    mutate(agno = a, grado = g, area = ar)
})

# ---- 6b. Dispersión histórica, con prior contextual (MODIFICADO) ------
# Misma lógica para el ancho de la distribución interna:
#
#   sd_simce ~ factor(agno) + GSE + dependencia + ruralidad + (1 | rbd)
#
# Advertencia honesta sobre esta pieza: el contexto explica MUY poco de
# la dispersión (R² ~0.01 en el universo nacional). La sd interna baja
# de forma monótona pero suave con el GSE (42.2 puntos en GSE bajo a
# 37.6 en GSE alto, 4b matemática) y el particular pagado es el más
# homogéneo (37.5), pero esas diferencias son chicas comparadas con la
# variación entre colegios del mismo estrato. Acá el contexto sirve casi
# sólo como valor por defecto para colegios sin historia. Se deja porque
# es gratis y es mejor que la constante nacional que se usaba antes,
# pero no hay que esperarle mucho.
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

  # Mismo problema de identificabilidad que en la sección 6: con un solo
  # año previo hay una fila por colegio y el intercepto aleatorio no es
  # estimable. Acá el encogimiento manual es aún más necesario, porque la
  # sd muestral de un colegio chico es ruidosísima: su varianza de
  # muestreo es aproximadamente sd^2 / (2(n-1)).
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
  estimar_sd_historica(dist_nacional, colegios_objetivo(a, g, ar), a, g, ar) %>%
    mutate(agno = a, grado = g, area = ar)
})


# ---- 6c. FORMA de la distribución interna (NUEVO) ---------------------
# Plantilla de cuantiles del puntaje estandarizado dentro del colegio.
# Se calcula con años estrictamente anteriores al año objetivo, y por
# tercil de nivel del colegio: en colegios de bajo rendimiento la cola
# derecha es algo más larga y en los de alto pasa lo contrario (efecto
# piso/techo de la prueba). Se guarda además la versión "todos" como
# respaldo cuando un tercil queda con pocos datos.
#
# Esta es la pieza que reemplaza el supuesto de normalidad: en vez de
# asumir que los puntajes dentro de un colegio se distribuyen normal,
# se usa la forma empírica observada en ~6.400 colegios reales.
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

# Cortes de tercil para asignar cada colegio a una plantilla de forma
# en el momento de predecir (usando su media PREDICHA, no la observada).
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

# ---- 6d. Confiabilidades (diagnóstico) --------------------------------
# CAMBIO v9: esta sección volvió a ser SÓLO diagnóstico. Entre la v5 y la
# v8 producía `rho_sugerido`, el multiplicador que la versión A de 03
# aplicaba a la posición del estudiante. Esa versión salió (ver el
# encabezado, punto 6): `rho` no era estimable —haría falta un vínculo
# alumno-a-alumno entre ensayo y SIMCE— y salía de reparametrizar un 0.70
# elegido a mano en la v4. Ningún script aguas abajo lee ya esta tabla
# para predecir.
#
# Lo que sigue siendo útil saber: cuánto puede, como máximo, correlacionar
# el ensayo con el SIMCE a nivel individual. La correlación observable
# entre dos mediciones está acotada por sus confiabilidades.
#
#  - Confiabilidad del SIMCE dentro del colegio: 1 - eem^2 / var_interna
#    (el archivo de alumnos trae el error estándar de medición).
#  - Confiabilidad del ÍNDICE de la sección 4 (`conf_indice`): la del
#    estimador encogido que ordena a los estudiantes dentro del colegio.
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

# ---- 7. Pegar todo a nivel de colegio y de estudiante ------------------
# Tabla de contexto por colegio, con etiquetas legibles: sirve para
# reportar (comparar un colegio contra su estrato GSE, por ejemplo) y
# para auditar de dónde salió su expectativa.
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
  mutate(
    across(c(contexto_nivel, desvio_nivel, nivel_hist_colegio),
           ~ replace_na(.x, 0)),
    n_anios_nivel_hist    = replace_na(n_anios_nivel_hist, 0L),
    n_anios_sd_hist = replace_na(n_anios_sd_hist, 0L),
    sin_historia    = n_anios_nivel_hist == 0,
    sin_contexto    = is.na(cod_grupo)
  ) %>%
  # Respaldo final por si un grupo entero quedó sin modelo de dispersión.
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

cat("\nColegios sin historia previa (reciben el prior de su contexto):",
    sum(school_features$sin_historia), "de", nrow(school_features), "\n")
cat("Colegios sin contexto conocido (reciben expectativa 0):",
    sum(school_features$sin_contexto), "\n")


# ---- 8. Cruce final con el SIMCE del MISMO año (para entrenar) --------
# Ahora se traen DOS targets: el promedio (modelo de 02) y la sd interna
# (modelo de 02b). El promedio sigue viniendo del archivo oficial por
# RBD; la sd sólo puede venir del archivo de alumnos.
school_model_data <- school_features %>%
  inner_join(
    simce_limpio %>% select(agno, grado, area, rbd_revisado, promedio_simce),
    by = c("agno", "grado", "area", "rbd_revisado")
  ) %>%
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

# Composición de la base Santillana. Vale la pena mirarla cada vez: es
# la razón por la que los coeficientes de contexto NO se estiman acá
# sino en el universo nacional.
cat("\nComposición de la base Santillana por contexto (último año, matemática):\n")
print(
  school_features %>%
    filter(agno == max(agno), area == "matematica") %>%
    count(grado, depe2_etiqueta, gse_etiqueta) %>%
    pivot_wider(names_from = gse_etiqueta, values_from = n, values_fill = 0)
)

# ---- 8b. Descriptivos para la presentación (NUEVO) ---------------------
# Estas tablas se calculan ACÁ y no en 05_figuras_presentacion.R porque
# dependen del archivo de alumnos completo (~2,6 M filas) y de las bases
# nacionales, que en este punto ya están en memoria. Lo que se guarda es
# el resumen —unas pocas decenas de filas y una grilla de densidad de 256
# puntos por grupo—, no los datos crudos: la presentación queda liviana y
# no necesita acceso a los parquet.
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

# Composición por GSE: colegios con ensayo vs. universo nacional. Es la
# evidencia de que la base no es representativa (limitación del punto 5).
comp_gse <- bind_rows(
  school_features %>%
    filter(agno == max(agno), area == "matematica") %>%
    distinct(grado, rbd_revisado, gse_etiqueta) %>%
    count(grado, gse_etiqueta, name = "n") %>%
    mutate(fuente = "santillana"),
  simce_nacional %>%
    filter(agno == max(agno), grado %in% GRADOS_MODELO, area == "matematica") %>%
    left_join(etiquetas_gse, by = "cod_grupo") %>%
    distinct(grado, rbd_revisado, gse_etiqueta) %>%
    count(grado, gse_etiqueta, name = "n") %>%
    mutate(fuente = "nacional")
) %>%
  filter(!is.na(gse_etiqueta)) %>%
  mutate(gse_etiqueta = factor(gse_etiqueta,
                               levels = etiquetas_gse$gse_etiqueta)) %>%
  group_by(fuente, grado) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

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

# --- Resumen por año de cada fuente ------------------------------------
# Tamaño de cada base, en los términos en que se describe en la
# presentación: cuántos colegios y cuántos alumnos distintos hay por año.
desc_simce_anio <- simce_alumno %>%
  group_by(agno) %>%
  summarise(
    n_colegios = n_distinct(rbd_revisado),
    n_alumnos  = n_distinct(idalumno),
    n_puntajes = n(),                      # alumno x área
    .groups = "drop"
  )

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

# --- Varianza dentro y entre colegios ----------------------------------
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

# --- Un colegio de ejemplo ---------------------------------------------
# Para mostrar en concreto que dentro de un solo colegio hay mucha
# variación se elige el colegio con más alumnos del grupo de referencia.
# Se exportan sus puntajes (unos cientos de valores) y la densidad
# nacional del mismo grupo, como referencia.
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

# --- SIMCE promedio por GSE --------------------------------------------
# Sobre el universo nacional, que es donde hay variación en GSE.
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

cat("\nDescriptivos para la presentación (último año):\n")
print(desc_simce_alu %>% filter(agno == max(agno)))

# SECCIÓN 8c (comparación IRT vs. logro crudo): ELIMINADA EN v9 ---------
#
# Comparaba las dos versiones que el script calculaba en paralelo. Su
# resultado ya está documentado y no cambia: la calibración mueve al
# colegio de percentil de forma SISTEMÁTICA —los que aplicaron ensayos
# fáciles bajan y los que aplicaron difíciles suben— y la dificultad de la
# batería explicaba entre 2.4% y 13.4% de la varianza de `mean_logro` entre
# colegios. Mantener la comparación viva obligaba a calcular todo el
# pipeline por duplicado: se corrió una vez, decidió, y su costo permanente
# no se justifica.

# ---- 9. Guardar --------------------------------------------------------
saveRDS(descriptivos,      dir_salidas %>% file.path("descriptivos.rds"))
saveRDS(ind_features,      dir_salidas %>% file.path("ind_features.rds"))
saveRDS(school_features,   dir_salidas %>% file.path("school_features.rds"))
saveRDS(school_model_data, dir_salidas %>% file.path("school_model_data.rds"))
saveRDS(nivel_historico,  dir_salidas %>% file.path("nivel_historico.rds"))
saveRDS(contexto_colegio,  dir_salidas %>% file.path("contexto_colegio.rds"))
saveRDS(simce_alumno,      dir_salidas %>% file.path("simce_alumno.rds"))
saveRDS(simce_dist,        dir_salidas %>% file.path("simce_dist.rds"))
saveRDS(forma_z,           dir_salidas %>% file.path("forma_z.rds"))
saveRDS(cortes_tercil,     dir_salidas %>% file.path("cortes_tercil.rds"))
saveRDS(limites_simce,     dir_salidas %>% file.path("limites_simce.rds"))
saveRDS(confiabilidades,   dir_salidas %>% file.path("confiabilidades.rds"))

cat("\nListo. Objetos guardados en", dir_salidas, "(*.rds)\n")

cat("\n-------------------------------------------------------------\n")
cat("AVISO v9 - columnas ELIMINADAS. Cualquier script aguas abajo que no\n")
cat("sea 02, 02b, 03 o 04 (típicamente los de 04_presentacion/) hay que\n")
cat("revisarlo a mano:\n")
cat("   pred_final_logro, slope_logro, nivel_est  (salió el modelo de\n")
cat("                                              crecimiento, secc. 4)\n")
cat("   mean_logro_crudo, mean_logro_irt y todas las variantes *_crudo,\n")
cat("   *_irt, *_theta                            (queda sólo mean_logro)\n")
cat("   n_evals / n_evals_prom                    (ahora k_ensayos /\n")
cat("                                              k_ensayos_prom)\n")
cat("   rel_est, tiene_otra_area                  (el índice es univariado)\n")
cat("   rho_sugerido y todo lo de rho             (salió la versión A)\n")
cat("-------------------------------------------------------------\n")
