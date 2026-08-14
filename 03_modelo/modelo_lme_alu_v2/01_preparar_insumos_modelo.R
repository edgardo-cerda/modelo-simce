# =============================================================
# 01_preparar_insumos_modelo.R  (v5)
# -------------------------------------------------------------
# CAMBIOS v5 (revisión metodológica). Cinco cambios, cuatro de ellos
# tocan este script:
#
# (1) `slope_logro` sale del modelo de nivel Y del script: el modelo de
#     crecimiento de la sección 4 dejó de estimar pendiente, así que la
#     variable ya no se calcula ni siquiera como diagnóstico. Motivo: los
#     ensayos no forman una progresión, tienen dificultades propias. En
#     4b matemática 2025 el logro medio por ensayo va 61.0, 62.2, 68.2,
#     60.1, 70.4, 72.8; en 2m matemática el ensayo 3 rinde 32.8 y el 5
#     rinde 40.4. Una pendiente ajustada sobre eso mide qué ensayos se
#     aplicaron, no si el colegio mejoró.
#
# (2) `n_evals_prom` sale del modelo de nivel y se reemplaza por un
#     ENCOGIMIENTO de `mean_logro` según su confiabilidad (sección
#     5b-bis). El número de ensayos importa como precisión de la medida,
#     no como nivel.
#
# (2+6) Índice individual NUEVO (sección 4b): encogido por confiabilidad
#     individual e incorporando el área complementaria con peso
#     proporcional a lo que aporta. Reemplaza a `pred_final_logro_raw`
#     como base del ordenamiento dentro del colegio.
#
# (5) Renombres para separar el histórico del NIVEL del de la
#     DISPERSIÓN, que antes se confundían:
#         colegio_efecto_historico -> nivel_hist_colegio
#         desvio_colegio           -> desvio_nivel
#         n_anios_hist             -> n_anios_nivel_hist
#         efecto_historico(.rds)   -> nivel_historico(.rds)
#     Los de dispersión ya estaban bien: sd_hist_colegio, desvio_sd,
#     contexto_sd, n_anios_sd_hist.
#
# El punto 4 (spline en el modelo de dispersión) es sólo de 02b.
#
# ATENCIÓN: la sección 6d reparametriza `rho`. Ver el comentario largo
# ahí antes de tocar 03: el 0.70 de la v4 NO se puede reutilizar tal
# cual sobre el índice nuevo.
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
#                             pct_ensayo y z_ensayo son la posición del
#                             estudiante DENTRO de su colegio. v5 agrega
#                             `indice_ensayo` (estimador encogido y
#                             compuesto con la otra área), `rel_indice`
#                             (su confiabilidad), `k_ensayos`,
#                             `tiene_otra_area`, y las versiones v4 del
#                             ordenamiento para auditar el cambio
#                             (`z_ensayo_v4`, `pct_ensayo_v4`, ambas
#                             construidas sobre `pred_final_logro`).
#                             OJO: `z_ensayo` YA NO tiene sd 1 dentro del
#                             colegio; ver la nota de escala en 4b.
#   - school_features.rds   : 1 fila por colegio x año x grado x área
#                             (features de dispersión; v5 agrega
#                             `mean_logro_enc` y `conf_mean_logro`)
#   - school_model_data.rds : school_features + promedio_simce Y sd_simce
#   - nivel_historico.rds  : efecto persistente del colegio (nivel)
#   - simce_alumno.rds      : SIMCE individual en formato largo
#   - simce_dist.rds        : media, sd y cuantiles observados por colegio
#   - forma_z.rds           : plantilla de forma de la distribución interna
#   - cortes_tercil.rds     : cortes para asignar tercil de nivel
#   - confiabilidades.rds   : confiabilidad del ensayo, del SIMCE y del
#                             índice individual nuevo. Desde la v5 NO es
#                             sólo diagnóstico: trae `rho_sugerido`, que
#                             es el valor que 03 usa en la versión A.
#
# Nota: el paso 4 ajusta hasta 12 modelos mixtos de crecimiento
# sobre decenas de miles de filas; puede tardar varios minutos.
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

# --- IRT (v6) ------------------------------------------------------------
# En TRUE, la medida base del ensayo deja de ser el porcentaje de logro
# observado y pasa a ser la habilidad estimada por la calibración IRT
# concurrente de 00_irt_calibracion.R. Entra en los DOS niveles:
#
#   - INDIVIDUAL (secc. 4b): `x_est` es theta y el error de medición es
#     `se_theta^2`, que la calibración entrega por estudiante. Reemplaza al
#     par (promedio de logro, sigma^2/k): antes la precisión de un
#     estudiante se aproximaba por CUÁNTOS ensayos rindió; ahora se sabe
#     directamente, y también depende de CUÁLES rindió y de cuánto
#     discriminaban esos ítems.
#
#   - ESCOLAR (secc. 5b): `mean_logro` pasa a ser el promedio del PUNTAJE
#     VERDADERO del estudiante, definido como el porcentaje del banco
#     completo de ítems del año que contestaría bien dado su theta
#     (curva característica del test evaluada en theta). Sigue estando en
#     puntos de porcentaje de logro —así 02, 02b, 03 y 04 no cambian de
#     escala ni de interpretación— pero ya no depende de QUÉ ensayos aplicó
#     el colegio. Ésa era la contaminación medida: la dificultad de la
#     batería aplicada explicaba entre 2.4% y 13.4% de la varianza de
#     `mean_logro` entre colegios.
#
# En FALSE el script se comporta como la v5 (logro observado). Las dos
# versiones se calculan SIEMPRE, porque la sección 8c las compara; el
# switch sólo decide cuál queda en las columnas que consumen 02 y 02b.
USAR_IRT <- TRUE

# --- Parámetros del índice individual encogido (sección 4b, v5) ---------

# Descuento clásico de dificultad (promedio de logro por ensayo). Es el
# sustituto pobre del IRT y sólo tiene sentido con USAR_IRT = FALSE: theta
# ya viene libre de la dificultad de la forma.
AJUSTAR_DIFICULTAD <- FALSE
# Mínimo de estudiantes para estimar la varianza verdadera entre alumnos
# DENTRO de un colegio. Bajo este umbral se usa la del grupo completo.
MIN_EST_TAU        <- 5

# Piso para tau^2 como fracción de la varianza observada: evita que un
# colegio donde el ruido explica toda la varianza quede con tau^2 = 0 y,
# por lo tanto, con todos sus estudiantes encogidos exactamente al
# promedio (lo que borraría el ordenamiento interno).
PISO_TAU2          <- 0.05

# Techo para la correlación verdadera entre áreas. La desatenuación
# divide por confiabilidades estimadas, y si éstas quedan bajas puede
# producir correlaciones > 1, que no significan nada.
RHO_AREAS_MAX      <- 0.95

# Mínimo de observaciones para creerle a la dificultad estimada de un
# ensayo. Hay casos degenerados en los datos (4b matemática 2024, ensayo
# 3, tiene 54 observaciones); esos vuelven al promedio del grupo.
MIN_OBS_ENSAYO     <- 200

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
       "). Hay que correr antes 00_irt_calibracion.R, o poner USAR_IRT <- FALSE.")
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

# --- Habilidad por ENSAYO, en la misma escala de logro (v7) -------------
# `irt_theta_forma.rds` trae una fila por estudiante x forma rendida, con
# los parámetros de ítem fijos de la calibración concurrente. Se traduce
# cada theta al mismo porcentaje sobre el banco completo que `logro_irt`,
# con lo cual las mediciones de un mismo estudiante en ensayos distintos
# quedan comparables entre sí: es lo que permite estimar crecimiento sin
# confundirlo con la dificultad de la forma que le tocó.
ruta_irt_theta_forma <- dir_salidas %>% file.path("irt_theta_forma.rds")

if (!file.exists(ruta_irt_theta_forma)) {
  stop("Falta ", ruta_irt_theta_forma, ". Es una salida nueva de ",
       "00_irt_calibracion.R (bloque 4d-bis): hay que volver a correrlo.")
}

irt_theta_forma <- readRDS(ruta_irt_theta_forma) %>%
  group_by(agno, grado, area) %>%
  group_modify(function(.x, .y) {
    it <- irt_items %>%
      filter(agno == .y$agno, grado == .y$grado, area == .y$area,
             is.finite(a), is.finite(b))
    .x$logro_irt_eval <- if (nrow(it) == 0) NA_real_ else
      curva_verdadera(.x$theta_forma, it$a, it$b)
    .x
  }) %>%
  ungroup()

cat("IRT por ensayo:", nrow(irt_theta_forma), "mediciones |",
    n_distinct(irt_theta_forma$id_usuario_curso), "estudiantes |",
    "mediana de ensayos por estudiante: ",
    median(table(paste(irt_theta_forma$agno, irt_theta_forma$grado,
                       irt_theta_forma$area, irt_theta_forma$id_usuario_curso))),
    "\n")

stopifnot(
  "irt_theta trae theta o se_theta no finitos" =
    all(is.finite(irt_theta$theta) & is.finite(irt_theta$se_theta)),
  "la transformación a puntaje verdadero dejó NA" =
    !any(is.na(irt_theta$logro_irt))
)

## SIMCE POR ALUMNO (NUEVO) ----
ruta_alu <- ruta_data_intermedia %>%
  file.path('simce', 'consolidado_datos_simce_alu.parquet')

simce_alu0 <- read_parquet(ruta_alu)

# ---- 2. Limpieza de ensayos -----------------------------------------
ensayos_limpio <- ensayos %>%
  mutate(
    porcentaje_logro = pmin(porcentaje_logro, 100),
    n_evaluacion = as.integer(n_evaluacion)
  ) %>%
  filter(!is.na(rbd_revisado), !is.na(n_evaluacion), porcentaje_logro > 0)

# Agregar ponderación porcentaje de logro  ----
# NOTA: con file.path() y no paste0(). En config.yml unos perfiles traen la
# ruta terminada en "/" y otros no; con paste0() el script sólo corre en los
# primeros (en el perfil `Usuario` producía
# ".../data_intermediatab_resumen_irt_matematica.parquet" y abortaba).
ponderar_logro_mate <- ruta_data_intermedia %>%
  file.path("tab_resumen_irt_matematica.parquet") %>%
  read_parquet()

ponderar_logro_leng <- ruta_data_intermedia %>%
  file.path("tab_resumen_irt_lenguaje.parquet") %>%
  read_parquet()
# Crear variale de ensayo para hacer cruce 
ensayos_limpio<-ensayos_limpio |> 
  mutate(
    n_ensayo = as.numeric(str_extract(evaluacion,"\\d")) 
  ) |> 
  left_join(
    ponderar_logro_mate |> 
      dplyr::select(
        n_ensayo
        ,grado
        ,"mean_dffclt_mate"=mean_dffclt
      ) |> 
      mutate(
        area = "matematica"
      ),by = c("n_ensayo","area","grado")
  ) |>
  left_join(
    ponderar_logro_leng |> 
      dplyr::select(
        n_ensayo
        ,grado
        ,"mean_dffclt_leng"=mean_dffclt
      ) |> 
      mutate(
        area = "lenguaje"
      ),by = c("n_ensayo","area","grado")
  )


# ponderar puntajes de logro 
# función 
ajustar_logro_irt <- function(porcentaje,
                              dificultad,
                              dificultad_ref = 0,
                              beta = 0.5) {
  
  # Casos extremos
  ifelse(
    porcentaje == 100,
    100,
    
    ifelse(
      porcentaje == 0,
      0,
      
      {
        # Convertir porcentaje a proporción
        p <- porcentaje / 100
        
        # Pasar a escala logit
        logit_p <- qlogis(p)
        
        # Ajustar por dificultad del ensayo
        logit_ajustado <- logit_p +
          beta * (dificultad - dificultad_ref)
        
        # Volver a porcentaje
        plogis(logit_ajustado) * 100
      }
    )
  )
}

ensayos_limpio <- ensayos_limpio %>%
  mutate(
    porcentaje_logro_ajustado_leng = ajustar_logro_irt(
      porcentaje = porcentaje_logro,
      dificultad = mean_dffclt_leng,
      dificultad_ref = 0,
      beta = 0.5
    )
  )

ensayos_limpio <- ensayos_limpio %>%
  mutate(
    porcentaje_logro_ajustado_mate = ajustar_logro_irt(
      porcentaje = porcentaje_logro,
      dificultad = mean_dffclt_mate,
      dificultad_ref = 0,
      beta = 0.5
    )
  )

# consolidar variable en porcentaje de logro
# OJO (v6): esta ponderación por `mean_dffclt` sólo se aplica con
# USAR_IRT = FALSE. Con el IRT activo sería una corrección DOBLE, y además
# de signo poco confiable: los `mean_dffclt` de tab_resumen_irt_*.parquet
# salen de un 3PL ajustado por separado en cada ensayo, así que cada uno
# está en su propia escala y no son comparables entre formas. Medido contra
# la calibración concurrente, el orden de dificultad que implican es el
# correcto en 4b (Spearman 0.94-1.00) pero no en 2m (0.77).
ensayos_limpio <- ensayos_limpio %>%
  mutate(
    porcentaje_logro_respaldo = porcentaje_logro
    ,porcentaje_logro = if (USAR_IRT) porcentaje_logro else case_when(
      area == "matematica"~porcentaje_logro_ajustado_mate
      ,area == "lenguaje"~porcentaje_logro_ajustado_leng
    )
  )
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
  mutate(area = ifelse(area == 'mate', 'matematica', 'lenguaje')) %>%
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
# CAMBIO v6: se llevan las DOS medidas por estudiante.
#
#   mean_logro_crudo = promedio del porcentaje de logro de los ensayos que
#                      rindió. Depende de CUÁLES rindió: un alumno que sólo
#                      dio el ensayo fácil sale mejor evaluado sin ser mejor.
#   mean_logro_irt   = puntaje verdadero, o sea qué porcentaje del banco
#                      COMPLETO de ítems del año contestaría bien según su
#                      theta. Misma escala 0-100, pero ya no depende de qué
#                      ensayos le tocaron.
#
# `mean_logro` (la columna que consumen 5b, 02 y 02b) es una de las dos
# según USAR_IRT. Se conservan ambas para poder compararlas en la secc. 8c.
resumen_simple <- ensayos_dedup %>%
  group_by(id_usuario_curso, agno, grado, area, rbd_revisado) %>%
  summarise(
    n_evals          = n(),
    mean_logro_crudo = mean(porcentaje_logro),
    .groups = "drop"
  ) %>%
  left_join(
    irt_theta %>% select(agno, grado, area, id_usuario_curso,
                         theta, se_theta, mean_logro_irt = logro_irt),
    by = c("agno", "grado", "area", "id_usuario_curso")
  ) %>%
  mutate(
    # Un estudiante sin theta (grupo no calibrado) conserva su logro crudo:
    # es peor medida, pero deja al alumno en la base en vez de borrarlo.
    mean_logro_irt = coalesce(mean_logro_irt, mean_logro_crudo),
    mean_logro     = if (USAR_IRT) mean_logro_irt else mean_logro_crudo
  )

cat("\nEstudiantes sin theta que conservan su logro crudo:",
    sum(is.na(resumen_simple$theta)), "de", nrow(resumen_simple), "\n")

# ---- 4. Modelo de crecimiento por estudiante (lme4) -------------------
# CAMBIO v7: vuelve la PENDIENTE, y el modelo corre sobre el logro
# ajustado por IRT en vez del logro crudo.
#
# Por qué ahora sí. La pendiente se había sacado porque los ensayos no
# eran comparables entre sí: cada forma tiene su propia dificultad, así
# que una "mejora" entre el ensayo 1 y el 4 podía ser sólo una forma más
# fácil. Con `logro_irt_eval` esa objeción desaparece — cada medición se
# expresa como porcentaje sobre el MISMO banco completo, sin importar qué
# forma se rindió — y estimar crecimiento vuelve a ser defendible.
#
#   logro_irt_eval ~ n_evaluacion_c + (1 | id_usuario_curso)
#                                   + (1 + n_evaluacion_c | rbd_revisado)
#
# Tres detalles de diseño:
#
#   - `n_evaluacion_c = n_evaluacion - 6`. Centrar en el 6º ensayo hace
#     que el intercepto SEA la predicción al 6º ensayo, sin extrapolar a
#     mano: `pred_final_logro` sale directo del intercepto total.
#   - Ponderación por `1/se_theta_forma^2`. Cada medición trae su propio
#     error estándar del IRT; una hecha con pocos ítems pesa menos que
#     una precisa. Los pesos se normalizan a media 1 dentro del grupo
#     para no cambiar la escala de la varianza residual.
#   - La pendiente es POR COLEGIO, no por estudiante. Se probó primero
#     con pendiente individual y no es identificable: los estudiantes
#     rinden 2,7 ensayos en promedio y en 2023 menos de 2, así que lme4
#     rechaza el modelo (más efectos aleatorios que observaciones) en los
#     cuatro grupos de 2023 y devuelve un ajuste singular en otro. El
#     problema de fondo no es que falle: es que fallaba SÓLO en algunos
#     años, y entonces `pred_final_logro` significaba una cosa en 2023
#     (intercepto + pendiente común) y otra en 2024-25 (con pendiente
#     individual). Para un modelo que se entrena en un año y predice otro
#     esa inconsistencia del predictor es peor que la pérdida de detalle.
#     El colegio tiene cientos de mediciones, así que su pendiente sí es
#     identificable en los 12 grupos y la definición queda uniforme.
#     Tampoco se pierde el ordenamiento individual: dentro del colegio
#     eso lo gobierna `indice_ensayo` (secc. 4b), no esta variable.
#
# Si ni siquiera la pendiente por colegio converge, se cae a pendiente
# sólo fija y queda registrado en el log.
#
# Se devuelven `pred_final_logro` (predicción al 6º ensayo, acotada a
# [0,100]), `slope_logro` (pendiente del estudiante, ya encogida) y
# `nivel_est`. El acotado genera EMPATES en los extremos, así que para
# ordenar estudiantes dentro del colegio NO se usa: eso lo sigue haciendo
# `indice_ensayo` de la sección 4b.
ENSAYO_OBJETIVO <- 6   # a qué ensayo se proyecta

ajustar_crecimiento_grupo <- function(datos_grupo) {

  datos_grupo <- datos_grupo %>%
    mutate(n_evaluacion_c = n_evaluacion - ENSAYO_OBJETIVO,
           w = 1 / se_theta_forma^2,
           w = w / mean(w, na.rm = TRUE))

  f_completa <- logro_irt_eval ~ n_evaluacion_c +
    (1 | id_usuario_curso) + (1 + n_evaluacion_c | rbd_revisado)
  f_simple   <- logro_irt_eval ~ n_evaluacion_c +
    (1 | id_usuario_curso) + (1 | rbd_revisado)

  ctrl <- lmerControl(optimizer = "bobyqa",
                      optCtrl = list(maxfun = 2e5))

  modelo <- tryCatch(
    lmer(f_completa, data = datos_grupo, weights = w, control = ctrl),
    error = function(e) {
      cat("    [pendiente por colegio falló:", conditionMessage(e),
          "-> se usa pendiente sólo fija]\n")
      NULL
    }
  )

  if (is.null(modelo)) {
    modelo <- lmer(f_simple, data = datos_grupo, weights = w, control = ctrl)
  }

  fe <- fixef(modelo)

  re_col <- ranef(modelo)$rbd_revisado %>%
    rownames_to_column("rbd_revisado")
  # Con pendiente sólo fija la columna no existe: todos los colegios
  # comparten la del efecto fijo.
  if (!"n_evaluacion_c" %in% names(re_col)) re_col$n_evaluacion_c <- 0

  efecto_colegio <- re_col %>%
    transmute(
      rbd_revisado       = as.numeric(rbd_revisado),
      colegio_intercepto = `(Intercept)`,
      colegio_pendiente  = n_evaluacion_c
    )

  ranef(modelo)$id_usuario_curso %>%
    rownames_to_column("id_usuario_curso") %>%
    transmute(
      id_usuario_curso      = as.integer(id_usuario_curso),
      estudiante_intercepto = `(Intercept)`
    ) %>%
    left_join(distinct(datos_grupo, id_usuario_curso, rbd_revisado),
              by = "id_usuario_curso") %>%
    left_join(efecto_colegio, by = "rbd_revisado") %>%
    mutate(
      # El intercepto ya está centrado en ENSAYO_OBJETIVO, así que ES la
      # predicción a ese ensayo.
      intercepto_hat   = fe[["(Intercept)"]] + coalesce(colegio_intercepto, 0) +
                         estudiante_intercepto,
      pred_final_logro = pmin(pmax(intercepto_hat, 0), 100),
      slope_logro      = fe[["n_evaluacion_c"]] + coalesce(colegio_pendiente, 0),
      nivel_est        = estudiante_intercepto
    ) %>%
    select(id_usuario_curso, pred_final_logro, slope_logro, nivel_est)
}

grupos_crecimiento <- ensayos_dedup %>% distinct(agno, grado, area)

# El modelo necesita la medición IRT de CADA ensayo, no el promedio del
# estudiante. Se pega por (estudiante, ensayo); los ensayos sin theta
# —grupo no calibrado— quedan fuera del ajuste.
# Un mismo número de ensayo puede corresponder a DOS formas: en 2023
# matemática conviven la "BAS" y la "EXT" (E1BAS / E1EXT), y hay
# estudiantes que rindieron ambas. Sin colapsar, el join duplicaría esas
# filas y el estudiante entraría dos veces al mismo punto de la
# trayectoria. Se combinan las mediciones de un mismo ensayo por varianza
# inversa —que es la forma correcta de promediar dos estimaciones de la
# misma cantidad con precisiones distintas— y el error estándar resultante
# es el de la combinación, menor que el de cada una por separado.
irt_eval_unico <- irt_theta_forma %>%
  filter(is.finite(logro_irt_eval), is.finite(se_theta_forma), se_theta_forma > 0) %>%
  group_by(agno, grado, area, id_usuario_curso, n_evaluacion) %>%
  summarise(
    logro_irt_eval = weighted.mean(logro_irt_eval, 1 / se_theta_forma^2),
    se_theta_forma = sqrt(1 / sum(1 / se_theta_forma^2)),
    .groups = "drop"
  )

ensayos_crecimiento <- ensayos_dedup %>%
  inner_join(irt_eval_unico,
             by = c("agno", "grado", "area", "id_usuario_curso", "n_evaluacion"))

cat("\nEnsayos con medición IRT para el modelo de crecimiento:",
    nrow(ensayos_crecimiento), "de", nrow(ensayos_dedup),
    sprintf("(%.1f%%)\n", 100 * nrow(ensayos_crecimiento) / nrow(ensayos_dedup)))

crecimiento_individual <- map_dfr(seq_len(nrow(grupos_crecimiento)), function(i) {
  a  <- grupos_crecimiento$agno[i]
  g  <- grupos_crecimiento$grado[i]
  ar <- grupos_crecimiento$area[i]
  cat("Ajustando modelo de crecimiento:", a, g, ar, "...\n")

  datos_grupo <- ensayos_crecimiento %>% filter(agno == a, grado == g, area == ar)
  if (nrow(datos_grupo) == 0) {
    cat("    [sin mediciones IRT: se omite el grupo]\n")
    return(NULL)
  }
  ajustar_crecimiento_grupo(datos_grupo) %>%
    mutate(agno = a, grado = g, area = ar)
})

# Respaldo para quienes no tienen medición IRT en ningún ensayo (grupo sin
# calibrar, o ítems descartados). El modelo no los pudo ajustar, pero
# borrarlos acá los sacaría de `ind_features` —el join de la sección 5 es
# inner— y con ellos se iría el colegio entero en los casos sin calibrar.
# Se les asigna su propio logro crudo como proyección y la pendiente
# mediana de su grupo: peor medida que la del modelo, pero los mantiene en
# la base con una estimación honesta en vez de un NA.
n_antes <- nrow(crecimiento_individual)

crecimiento_individual <- resumen_simple %>%
  select(agno, grado, area, id_usuario_curso, mean_logro_crudo) %>%
  left_join(crecimiento_individual,
            by = c("agno", "grado", "area", "id_usuario_curso")) %>%
  group_by(agno, grado, area) %>%
  mutate(
    sin_irt          = is.na(pred_final_logro),
    slope_logro      = coalesce(slope_logro, median(slope_logro, na.rm = TRUE), 0),
    pred_final_logro = coalesce(pred_final_logro, mean_logro_crudo),
    nivel_est        = coalesce(nivel_est, 0)
  ) %>%
  ungroup() %>%
  select(-mean_logro_crudo) %>%
  left_join(ensayos_dedup %>%
              group_by(id_usuario_curso, agno, grado, area) %>%
              summarise(porcentaje_logro = mean(porcentaje_logro, na.rm = TRUE),
                        n_ensayos = n(),
                        .groups = 'drop'),
            by = c('id_usuario_curso', 'agno', 'grado', 'area'))

cat("\nEstudiantes con crecimiento ajustado por IRT:", n_antes, "de",
    nrow(crecimiento_individual),
    sprintf("(%.1f%%). El resto usa su logro crudo como proyección.\n",
            100 * n_antes / nrow(crecimiento_individual)))

cat("\nPendiente estimada por grupo (puntos de logro por ensayo):\n")
print(
  crecimiento_individual %>%
    group_by(agno, grado, area) %>%
    summarise(n = n(),
              slope_media = round(mean(slope_logro), 3),
              slope_sd    = round(sd(slope_logro), 3),
              pred_media  = round(mean(pred_final_logro), 1),
              pct_en_borde = round(100 * mean(pred_final_logro %in% c(0, 100)), 1),
              .groups = "drop") %>%
    as.data.frame()
)


# ---- 4b. ÍNDICE INDIVIDUAL ENCOGIDO POR CONFIABILIDAD (NUEVO v5) ------
# Reemplaza a `pred_final_logro_raw` como base del ordenamiento de los
# estudiantes dentro de su colegio. Resuelve dos cosas de una vez:
#
#   (a) No todos los estudiantes están medidos con la misma precisión.
#       El 15-22% rinde UN solo ensayo, y su promedio es mucho más
#       ruidoso que el de uno que rindió seis. Antes esto entraba al modelo de colegio como
#       `n_evals_prom` sumado linealmente, que no es donde el número de
#       ensayos actúa.
#
#   (b) Cada estudiante rinde ensayos de AMBAS áreas y los resultados
#       correlacionan. La otra área es información sobre la misma
#       persona, y sirve justamente donde la propia escasea.
#
# CÓMO. Para el estudiante i del colegio c en el área a se descompone
# su promedio de logro, centrado en el promedio de su colegio:
#
#     x_i = theta_i + e_i,   Var(theta) = tau^2,  Var(e_i) = sigma^2/k_i
#
# donde theta_i es su posición VERDADERA dentro del colegio, k_i es
# cuántos ensayos rindió, sigma^2 es la varianza de un mismo estudiante
# entre ensayos (estimable con quienes rindieron 2+) y tau^2 es la
# varianza verdadera entre estudiantes del mismo colegio. De ahí sale su
# confiabilidad individual:
#
#     rel_i = tau^2 / (tau^2 + sigma^2/k_i)
#
# Con las dos áreas, el estimador óptimo de theta_a es la regresión
# bivariada de theta_a sobre (z_a, z_b) — un BLUP. En unidades de
# desviación verdadera (z = x/tau), con rho la correlación VERDADERA
# entre áreas:
#
#     V = [[1/ra, rho], [rho, 1/rb]] ,  c = [1, rho]
#     theta_a_hat = c' V^-1 (z_a, z_b)'
#
# que desarrollado y estabilizado numéricamente queda
#
#     w_a = ra (1 - rho^2 rb) / (1 - rho^2 ra rb)
#     w_b = rho rb (1 - ra)   / (1 - rho^2 ra rb)
#
# Dos propiedades que importan y que son la razón de escribirlo así:
#
#   - Cuando el estudiante NO tiene la otra área, basta poner rb = 0:
#     w_a -> ra y w_b -> 0. El respaldo es CONTINUO, no una fórmula
#     distinta. Si fueran dos fórmulas separadas habría un salto
#     sistemático entre el 78% que tiene ambas áreas y el 22% que no,
#     y ese salto se metería en el ranking interno del colegio.
#   - El peso de la otra área es proporcional a lo que aporta: crece con
#     rho y con rb, y se apaga cuando la medida propia ya es buena
#     (factor 1 - ra). Un estudiante con seis ensayos casi no toma
#     prestado; uno con un solo ensayo sí.
#
# Ganancia medida en confiabilidad efectiva: +4 a +10 puntos en
# promedio, y +7 a +16 puntos para los estudiantes con un solo ensayo.
#
# ESCALA — LEER ANTES DE TOCAR 03. El índice queda expresado en
# unidades de desviación VERDADERA dentro del colegio, y por lo tanto
# su desviación estándar es sqrt(rel) < 1, no 1 como antes. Eso es
# deliberado: así el encogimiento de un estudiante mal medido llega
# hasta la predicción final y no se pierde al re-estandarizar. Pero
# obliga a reinterpretar RHO_ENSAYO_SIMCE en 03: antes multiplicaba un
# puntaje OBSERVADO estandarizado, ahora multiplica una habilidad
# ESTIMADA. La sección 6d calcula el rho equivalente; usarlo sin
# convertir encogería dos veces.

cat("\nConstruyendo índice individual encogido (secc. 4b)...\n")

# --- (i) Dificultad de cada ensayo -------------------------------------
# Se calcula siempre (sirve de diagnóstico) pero sólo se descuenta si
# AJUSTAR_DIFICULTAD es TRUE.
dificultad_ensayo <- ensayos_dedup %>%
  group_by(agno, grado, area, n_evaluacion) %>%
  summarise(dif_bruta = mean(porcentaje_logro), n_obs_ensayo = n(), .groups = "drop") %>%
  group_by(agno, grado, area) %>%
  mutate(
    dif_grupo  = weighted.mean(dif_bruta, n_obs_ensayo),
    dif_ensayo = if_else(n_obs_ensayo >= MIN_OBS_ENSAYO, dif_bruta, dif_grupo)
  ) %>%
  ungroup()

cat("  Dispersión de la dificultad entre ensayos, por grupo (último año):\n")
print(
  dificultad_ensayo %>%
    filter(agno == max(agno)) %>%
    group_by(grado, area) %>%
    summarise(n_ensayos = n(),
              dif_min = round(min(dif_bruta), 1),
              dif_max = round(max(dif_bruta), 1),
              sd_dif  = round(sd(dif_bruta), 1), .groups = "drop")
)

ensayos_ind <- ensayos_dedup %>%
  left_join(dificultad_ensayo %>%
              select(agno, grado, area, n_evaluacion, dif_ensayo, dif_grupo),
            by = c("agno", "grado", "area", "n_evaluacion")) %>%
  mutate(
    logro_aj = if (AJUSTAR_DIFICULTAD) {
      porcentaje_logro - dif_ensayo + dif_grupo
    } else porcentaje_logro
  )

# --- (ii) sigma^2: varianza de un mismo estudiante ENTRE ensayos -------
# Sólo identificable con quienes rindieron 2 o más. Los grados de
# libertad son n_observaciones - n_estudiantes.
sigma2_grupo <- ensayos_ind %>%
  group_by(agno, grado, area, id_usuario_curso) %>%
  mutate(k_est = n(), media_est = mean(logro_aj)) %>%
  ungroup() %>%
  filter(k_est >= 2) %>%
  group_by(agno, grado, area) %>%
  summarise(
    sigma2 = sum((logro_aj - media_est)^2) / pmax(n() - n_distinct(id_usuario_curso), 1),
    .groups = "drop"
  )

# --- (iii) Nivel observado de cada estudiante --------------------------
# CAMBIO v6: los pasos (iv) y (v) pasaron a ser una función. Lo único que
# necesitan del insumo es una medida por estudiante (`x_est`) y la varianza
# de su error de medición (`var_err`); todo lo demás —descontar el ruido de
# la varianza interna del colegio, encoger, componer con la otra área— es
# idéntico venga esa medida del promedio de logro o de la calibración IRT.
# Con la función escrita una sola vez, las dos versiones se construyen con
# la misma aritmética y la comparación de la sección 8c es limpia.
#
#   versión "crudo": x_est = promedio de logro, var_err = sigma^2 / k
#   versión "irt"  : x_est = theta,             var_err = se_theta^2
#
# La diferencia de fondo entre ambas no es la escala sino de dónde sale la
# precisión: en la primera se aproxima por cuántos ensayos rindió el
# estudiante, y en la segunda la entrega la calibración, que además sabe
# CUÁLES rindió y cuánto discriminaban esos ítems.
construir_indice <- function(est_entrada, etiqueta) {

  cat("\n  --- índice individual, versión:", etiqueta, "---\n")

  # --- (iv) tau^2 verdadera entre estudiantes del mismo colegio ----------
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
      tau2      = pmax(coalesce(tau2, tau2_g), PISO_TAU2 * coalesce(tau2_g, 0)),
      tau2      = if_else(is.na(tau2) | tau2 <= 0, tau2_respaldo, tau2),
      rel_est   = tau2 / (tau2 + var_err),
      z_bruto   = x_c / sqrt(tau2)
    ) %>%
    select(-tau2_respaldo)

  stopifnot(
    "tau2 no finito en la sección 4b" = all(is.finite(est_base$tau2)),
    "rel_est fuera de [0,1] en la sección 4b" =
      all(est_base$rel_est >= 0 & est_base$rel_est <= 1)
  )

  # --- (v) Compuesto con la otra área (BLUP bivariado) -------------------
  areas_disp <- sort(unique(est_base$area))
  usar_cruce <- length(areas_disp) == 2
  if (!usar_cruce) {
    warning("Se esperaban exactamente 2 áreas para el compuesto; encontradas: ",
            paste(areas_disp, collapse = ", "),
            ". El índice usará sólo el área propia.")
  }

  otra_area <- est_base %>%
    transmute(agno, grado, rbd_revisado, id_usuario_curso,
              area_otra = area, z_otra = z_bruto, rel_otra = rel_est)

  # Con dos áreas, rev() da el mapeo cruzado (a->b, b->a). Si no hay
  # exactamente dos, area_otra queda en NA para que el join no empareje
  # cada área CONSIGO MISMA, que daría rel_otra = rel_est y correlación 1.
  mapa_areas <- tibble(
    area      = areas_disp,
    area_otra = if (usar_cruce) rev(areas_disp) else NA_character_
  )

  est_cruce <- est_base %>%
    left_join(mapa_areas, by = "area") %>%
    left_join(otra_area,
              by = c("agno", "grado", "rbd_revisado", "id_usuario_curso", "area_otra"))

  # Correlación VERDADERA entre áreas: la observada, desatenuada por las
  # confiabilidades de ambas medidas. Se estima por año y grado (no por
  # área: es simétrica) y sobre puntajes ya centrados en el colegio, que
  # es la correlación relevante para ordenar dentro del colegio.
  rho_areas <- est_cruce %>%
    filter(!is.na(z_otra)) %>%
    group_by(agno, grado) %>%
    summarise(
      n_pares    = n() %/% 2,
      r_observada = if (n() >= 30) {
        cor(z_bruto, z_otra, use = "complete.obs")
      } else NA_real_,
      rel_a      = mean(rel_est, na.rm = TRUE),
      rel_b      = mean(rel_otra, na.rm = TRUE),
      # Si no se pudo estimar, 0 = "no tomar prestado de la otra área".
      # Es el respaldo conservador: el índice queda igual que sin cruce.
      rho_ab_grupo = coalesce(
        pmin(pmax(r_observada / sqrt(rel_a * rel_b), 0), RHO_AREAS_MAX), 0
      ),
      .groups = "drop"
    )

  cat("  Correlación entre áreas (dentro del colegio), observada y desatenuada:\n")
  print(rho_areas %>% mutate(across(where(is.numeric), ~round(.x, 3))))

  est_indice <- est_cruce %>%
    left_join(rho_areas %>% select(agno, grado, rho_ab_grupo),
              by = c("agno", "grado")) %>%
    mutate(
      rho_ab = if (usar_cruce) coalesce(rho_ab_grupo, 0) else 0,
      ra     = rel_est,
      rb     = coalesce(rel_otra, 0),      # 0 = "no tiene la otra área"
      zb     = coalesce(z_otra, 0),
      den    = 1 - rho_ab^2 * ra * rb,
      w_propia = ra * (1 - rho_ab^2 * rb) / den,
      w_otra   = rho_ab * rb * (1 - ra)   / den,
      # Índice final: posición estimada dentro del colegio, en unidades de
      # desviación verdadera. Su sd dentro del colegio es sqrt(rel), no 1.
      indice_ensayo = w_propia * z_bruto + w_otra * zb,
      # Confiabilidad efectiva del índice compuesto = c' V^-1 c.
      rel_indice    = w_propia + rho_ab * w_otra,
      tiene_otra_area = !is.na(z_otra)
    )

  cat("  Confiabilidad del índice individual, por número de ensayos rendidos\n",
      "  (rel_propia = sólo su área; rel_indice = con la otra área):\n")
  print(
    est_indice %>%
      filter(agno == max(agno)) %>%
      group_by(grado, area, k_ensayos) %>%
      summarise(n = n(),
                rel_propia = round(mean(rel_est), 3),
                rel_indice = round(mean(rel_indice), 3),
                ganancia_pp = round(100 * (mean(rel_indice) - mean(rel_est)), 1),
                .groups = "drop") %>%
      filter(n >= 50)
  )

  cat("  Cobertura de la otra área:",
      sprintf("%.0f%%", 100 * mean(est_indice$tiene_otra_area)), "\n")

  est_indice
}

# --- Insumo "crudo": promedio de logro observado ------------------------
est_entrada_crudo <- ensayos_ind %>%
  group_by(agno, grado, area, rbd_revisado, id_usuario_curso) %>%
  summarise(k_ensayos = n(), x_est = mean(logro_aj), .groups = "drop") %>%
  left_join(sigma2_grupo, by = c("agno", "grado", "area")) %>%
  # Respaldo de sigma^2 si un grupo entero no tuviera estudiantes con 2+
  # ensayos (no ocurre en 2023-2025, pero dejarlo en NA propagaría NA a
  # rel_est y de ahí al índice de todo el grupo, en silencio).
  group_by(grado, area) %>%
  mutate(sigma2 = coalesce(sigma2, mean(sigma2, na.rm = TRUE))) %>%
  ungroup() %>%
  mutate(sigma2 = coalesce(sigma2, var(x_est, na.rm = TRUE)),
         var_err = sigma2 / k_ensayos)

# --- Insumo "irt": theta y su error estándar ---------------------------
# `se_theta` ya es el error de medición del estudiante, así que no hace
# falta estimar sigma^2 ni dividir por k: la calibración lo entrega directo
# y además diferenciado por CUÁLES formas rindió, no sólo cuántas.
est_entrada_irt <- est_entrada_crudo %>%
  select(agno, grado, area, rbd_revisado, id_usuario_curso, k_ensayos) %>%
  inner_join(
    irt_theta %>% select(agno, grado, area, id_usuario_curso, theta, se_theta,
                         logro_irt),
    by = c("agno", "grado", "area", "id_usuario_curso")
  ) %>%
  mutate(x_est = theta, var_err = se_theta^2)

cat("\n  Estudiantes con theta sobre el total del ensayo:",
    sprintf("%.1f%%", 100 * nrow(est_entrada_irt) / nrow(est_entrada_crudo)), "\n")

est_indice_crudo <- construir_indice(est_entrada_crudo, "crudo (v5)")
est_indice_irt   <- construir_indice(est_entrada_irt,   "IRT (v6)")

est_indice <- if (USAR_IRT) est_indice_irt else est_indice_crudo

# ---- 5. Features a nivel de estudiante --------------------------------
# pct_ensayo y z_ensayo son la posición del estudiante DENTRO de su
# propio colegio. Son la materia prima de las dos versiones de
# predicción individual:
#   - versión A (dispersión): usa z_ensayo
#   - versión B (matching por percentil): usa pct_ensayo
#
# CAMBIO v5: el índice sobre el que se ordena ya no es la predicción del
# modelo de crecimiento sino `indice_ensayo`, el estimador encogido por
# confiabilidad de la sección 4b. Dos diferencias:
#
#   - Ya no extrapola la pendiente a un ensayo 6 hipotético. Esa
#     extrapolación arrastraba la pendiente entre ensayos, que es en
#     buena parte artefacto de qué ensayo se aplicó y no aprendizaje
#     (es el mismo motivo por el que `slope_logro` sale del modelo de
#     colegio en 02).
#   - Encoge de forma DIFERENCIAL: un estudiante con un ensayo se acerca
#     más al promedio de su colegio que uno con seis. El shrinkage de
#     lme4 también encogía, pero de forma prácticamente uniforme y sin
#     dejar disponible la confiabilidad individual, que es lo que
#     permite pesar la otra área.
#
# El ordenamiento v4 se conserva como `z_ensayo_v4` / `pct_ensayo_v4`,
# construidos sobre `pred_final_logro`, para poder comparar los dos
# ordenamientos (la comparación se imprime unas líneas más abajo).
ind_features <- resumen_simple %>%
  inner_join(
    crecimiento_individual,
    by = c("id_usuario_curso", "agno", "grado", "area")
  ) %>%
  left_join(
    est_indice %>%
      select(agno, grado, area, rbd_revisado, id_usuario_curso,
             k_ensayos, indice_ensayo, rel_indice, rel_est, z_bruto,
             tiene_otra_area),
    by = c("agno", "grado", "area", "rbd_revisado", "id_usuario_curso")
  ) 

# El índice debe existir para todos: ambas tablas salen de `ensayos_dedup`
# con la misma llave. Si esto falla, hay un problema de duplicados aguas
# arriba y conviene detenerse antes de que se propague en silencio.
stopifnot(
  "Estudiantes sin indice_ensayo tras el join de la sección 4b" =
    !any(is.na(ind_features$indice_ensayo))
)

ind_features <- ind_features %>%
  group_by(agno, grado, area, rbd_revisado) %>%
  mutate(
    n_est_colegio = n(),
    # percentil dentro del colegio, con corrección (rank - 0.5)/n para
    # que ningún estudiante quede en 0 o 1 exactos.
    pct_ensayo = (rank(indice_ensayo, ties.method = "average") - 0.5) / n(),
    # Posición dentro del colegio. OJO: a diferencia de la v4, NO se
    # re-estandariza a sd 1. `indice_ensayo` ya viene en unidades de
    # desviación verdadera, así que su sd dentro del colegio es
    # sqrt(rel) < 1 y el encogimiento por confiabilidad sobrevive hasta
    # la predicción final. Re-estandarizar acá lo borraría.
    # Con menos de 3 estudiantes el centrado no significa nada: 0.
    z_ensayo = if (n() >= 3) indice_ensayo else 0,
    # Versión v4 del ordenamiento, sólo para auditar el cambio.
    z_ensayo_v4 = if (n() >= 3 && sd(pred_final_logro) > 0) {
      (pred_final_logro - mean(pred_final_logro)) / sd(pred_final_logro)
    } else 0,
    pct_ensayo_v4 = (rank(pred_final_logro, ties.method = "average") - 0.5) / n()
  ) %>%
  ungroup()

cat("\nCambio de ordenamiento interno respecto de la v4 (correlación de rangos\n",
    "dentro del colegio; 1 = ningún cambio):\n")
print(
  ind_features %>%
    filter(agno == max(agno), n_est_colegio >= 10) %>%
    group_by(grado, area, rbd_revisado) %>%
    summarise(rho_rangos = cor(pct_ensayo, pct_ensayo_v4, method = "spearman"),
              .groups = "drop") %>%
    group_by(grado, area) %>%
    summarise(n_colegios = n(),
              rho_mediano = round(median(rho_rangos, na.rm = TRUE), 3),
              rho_p10     = round(quantile(rho_rangos, 0.10, na.rm = TRUE), 3),
              .groups = "drop")
)

# ---- 5b. Features a nivel de colegio ----------------------------------
# NUEVO: además de las de nivel, features de DISPERSIÓN del ensayo
# (sd_entre_estud ya existía pero era sólo referencial; ahora es
# predictor del modelo de dispersión) e iqr_logro_ensayo, que resultó
# algo más robusto que la sd frente a colegios con outliers.
#
# CAMBIO v6: se agregan las mismas tres features calculadas sobre el
# puntaje verdadero IRT (`*_irt`) además de las del logro crudo
# (`*_crudo`). Las columnas sin sufijo —`mean_logro`, `sd_entre_estud`,
# `iqr_logro_ensayo`, que son las que consumen 02, 02b y 03— apuntan a una
# de las dos según USAR_IRT. Mantener ambas cuesta tres columnas y permite
# que la sección 8c compare las dos especificaciones sin volver a correr
# nada.
school_features <- ind_features %>%
  group_by(agno, grado, area, rbd_revisado) %>%
  summarise(
    n_estudiantes    = n(),
    pred_final_logro = mean(pred_final_logro, na.rm = TRUE),
    n_evals_prom     = mean(n_evals, na.rm = TRUE),

    # OJO con los nombres: summarise() evalúa en orden y con enmascaramiento
    # de datos, así que si la media se llamara `mean_logro_crudo` —igual que
    # la columna de entrada— el `sd()` de la línea siguiente recibiría el
    # ESCALAR recién calculado y no la columna, y devolvería NA en silencio.
    # Por eso las salidas se nombran distinto y se renombran después.
    prom_logro_crudo   = mean(mean_logro_crudo, na.rm = TRUE),
    sd_entre_estud_crudo = sd(mean_logro_crudo, na.rm = TRUE),
    iqr_logro_crudo    = quantile(mean_logro_crudo, 0.90, names = FALSE) -
                         quantile(mean_logro_crudo, 0.10, names = FALSE),

    prom_logro_irt     = mean(mean_logro_irt, na.rm = TRUE),
    sd_entre_estud_irt = sd(mean_logro_irt, na.rm = TRUE),
    iqr_logro_irt      = quantile(mean_logro_irt, 0.90, names = FALSE) -
                         quantile(mean_logro_irt, 0.10, names = FALSE),
    .groups = "drop"
  ) %>%
  rename(mean_logro_crudo = prom_logro_crudo,
         mean_logro_irt   = prom_logro_irt) %>%
  mutate(
    mean_logro       = if (USAR_IRT) mean_logro_irt     else mean_logro_crudo,
    sd_entre_estud   = if (USAR_IRT) sd_entre_estud_irt else sd_entre_estud_crudo,
    iqr_logro_ensayo = if (USAR_IRT) iqr_logro_irt      else iqr_logro_crudo
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
# CAMBIO v6: el encogimiento se aplica por igual a las dos versiones del
# nivel escolar (cruda e IRT), para que la comparación de la sección 8c
# enfrente especificaciones equivalentes y no una encogida contra otra sin
# encoger. `encoger_nivel()` es la misma aritmética de siempre, escrita una
# vez y llamada dos veces.
encoger_nivel <- function(d, col_media, col_sd, out_enc, out_conf, out_var_err) {
  media <- d[[col_media]]
  sd_e  <- d[[col_sd]]
  clave <- paste(d$agno, d$grado, d$area)

  # La sd entre estudiantes es NA en colegios con 1 alumno: se les asigna la
  # sd típica de su grupo, peor que su dato pero mejor que un NA.
  sd_aux  <- ave(sd_e, clave, FUN = function(x) coalesce(x, median(x, na.rm = TRUE)))
  var_err <- sd_aux^2 / pmax(d$n_estudiantes, 1)

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

school_features <- school_features %>%
  encoger_nivel("mean_logro_crudo", "sd_entre_estud_crudo",
                "mean_logro_crudo_enc", "conf_mean_logro_crudo",
                "var_err_logro_crudo") %>%
  encoger_nivel("mean_logro_irt", "sd_entre_estud_irt",
                "mean_logro_irt_enc", "conf_mean_logro_irt",
                "var_err_logro_irt") %>%
  # Las columnas sin sufijo son las que consumen 02, 02b y 03.
  mutate(
    mean_logro_enc  = if (USAR_IRT) mean_logro_irt_enc   else mean_logro_crudo_enc,
    conf_mean_logro = if (USAR_IRT) conf_mean_logro_irt  else conf_mean_logro_crudo,
    var_err_logro   = if (USAR_IRT) var_err_logro_irt    else var_err_logro_crudo
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
# Se dejan las dos versiones —sobre el nivel encogido y sobre la
# proyección al 6º ensayo— porque 02 compara ambas especificaciones.
school_features <- school_features %>%
  group_by(agno, grado, area) %>%
  mutate(
    mean_logro_enc_c   = mean_logro_enc - mean(mean_logro_enc, na.rm = TRUE),
    pred_final_logro_c = pred_final_logro - mean(pred_final_logro, na.rm = TRUE)
  ) %>%
  ungroup()

cat("\nDeriva del ensayo entre años (media del grupo que se descuenta al centrar):\n")
print(
  school_features %>%
    group_by(agno, grado, area) %>%
    summarise(n_colegios = n(),
              media_logro_enc  = round(mean(mean_logro_enc, na.rm = TRUE), 1),
              media_pred_final = round(mean(pred_final_logro, na.rm = TRUE), 1),
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

# ---- 6d. Confiabilidades y calibración de rho -------------------------
# Ya NO es sólo diagnóstico: desde la v5 esta sección produce el valor de
# rho que 03 usa en la versión A. Ver el bloque "REPARAMETRIZACIÓN DE
# rho" más abajo, que es la parte que hay que leer antes de tocar 03.
#
# ¿Cuánto puede, como máximo, correlacionar el ensayo con el SIMCE a
# nivel individual? La correlación observable entre dos mediciones está
# acotada por sus confiabilidades.
#
#  - Confiabilidad del SIMCE dentro del colegio: 1 - eem^2 / var_interna
#    (el archivo de alumnos trae el error estándar de medición).
#  - Confiabilidad del promedio de ensayos del estudiante (`conf_ensayo`):
#    ICC entre estudiantes (tras descontar la dificultad de cada ensayo),
#    corregida por Spearman-Brown según cuántos ensayos rindió. Es la
#    medida de la v4 y se conserva para poder comparar corridas viejas.
#  - Confiabilidad del ÍNDICE de la sección 4b (`conf_indice`): la que
#    corresponde ahora, porque el índice ya no es el promedio crudo de
#    ensayos sino el estimador encogido y compuesto con la otra área.
#    Es mayor que `conf_ensayo`, y esa diferencia es exactamente la
#    ganancia de los puntos 2 y 6.
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

conf_ensayo <- ensayos_dedup %>%
  group_by(agno, grado, area, n_evaluacion) %>%
  mutate(x = porcentaje_logro - mean(porcentaje_logro)) %>%   # quita dificultad del ensayo
  group_by(agno, grado, area, id_usuario_curso) %>%
  filter(n() >= 2) %>%
  mutate(x_medio = mean(x)) %>%
  group_by(grado, area) %>%
  summarise(
    n_estudiantes = n_distinct(paste(agno, id_usuario_curso)),
    var_within = sum((x - x_medio)^2) / (n() - n_estudiantes),
    var_total  = var(x),
    n_prom     = n() / n_estudiantes,   # ensayos por estudiante
    icc        = pmax(var_total - var_within, 0) / pmax(var_total, 1e-9),
    conf_ensayo = n_prom * icc / (1 + (n_prom - 1) * icc),
    .groups = "drop"
  ) %>%
  select(grado, area, icc, n_prom, conf_ensayo)

# Confiabilidad del ÍNDICE de la sección 4b (encogido + compuesto con la
# otra área). Es la que corresponde ahora, no `conf_ensayo`: el índice ya
# no es el promedio crudo de ensayos.
conf_indice <- est_indice %>%
  group_by(grado, area) %>%
  summarise(
    conf_indice     = mean(rel_indice, na.rm = TRUE),
    conf_indice_p10 = quantile(rel_indice, 0.10, na.rm = TRUE, names = FALSE),
    k_medio         = mean(k_ensayos),
    .groups = "drop"
  )

# --- REPARAMETRIZACIÓN DE rho (IMPORTANTE) -----------------------------
# En la v4, 03 calculaba
#
#     pred_A = mu + rho_v4 * sd_hat * z_std
#
# con z_std = puntaje OBSERVADO del ensayo, estandarizado a sd 1 dentro
# del colegio. Ese rho_v4 tenía que absorber DOS cosas a la vez: que el
# ensayo mide con error, y que ensayo y SIMCE no son la misma prueba. Por
# eso su techo era sqrt(conf_ensayo * conf_simce).
#
# En la v5, z_ensayo ya es una habilidad ESTIMADA (encogida por
# confiabilidad, en unidades de desviación verdadera). El error de
# medición del ensayo ya está descontado ahí. El multiplicador que
# corresponde ahora es la correlación entre la habilidad VERDADERA en el
# ensayo y el puntaje OBSERVADO de SIMCE, cuyo techo es sólo
# sqrt(conf_simce).
#
# Equivalencia entre ambas escalas: como Var(z_ensayo) = conf_indice
# dentro del colegio, mientras que Var(z_std) = 1,
#
#     rho_v5 = rho_v4 / sqrt(conf_indice)
#
# Usar el 0.70 de la v4 tal cual sobre el índice nuevo encogería DOS
# veces (una en el índice, otra en el rho) y dejaría las predicciones
# individuales sistemáticamente demasiado apretadas en torno al promedio
# del colegio — exactamente el defecto que la v3 vino a arreglar.
RHO_V4_REFERENCIA <- 0.70

confiabilidades <- conf_simce %>%
  select(grado, area, conf_simce) %>%
  inner_join(conf_ensayo, by = c("grado", "area")) %>%
  left_join(conf_indice, by = c("grado", "area")) %>%
  mutate(
    # Techo de la v4, sobre puntaje observado estandarizado (se conserva
    # para poder comparar corridas viejas).
    rho_maximo        = sqrt(conf_simce * conf_ensayo),
    # Techo de la v5, sobre la habilidad estimada.
    rho_maximo_indice = sqrt(conf_simce),
    # Valor sugerido para 03: el equivalente del 0.70 de la v4 en la
    # escala nueva, acotado por el techo.
    rho_sugerido      = pmin(RHO_V4_REFERENCIA / sqrt(conf_indice),
                             rho_maximo_indice)
  )

cat("\nConfiabilidades estimadas y rho equivalente para 03:\n")
print(confiabilidades %>%
        select(grado, area, conf_simce, conf_ensayo, conf_indice,
               rho_maximo, rho_maximo_indice, rho_sugerido) %>%
        mutate(across(where(is.numeric), ~round(.x, 3))))
cat("\n  conf_indice es la confiabilidad del índice individual NUEVO\n",
    " (encogido + compuesto). rho_sugerido es el valor que 03 debe usar\n",
    " sobre `z_ensayo` de la v5; NO reutilizar el 0.70 de la v4.\n")

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
# promedio se calcula por alumno-área, que es la unidad que después usa
# el modelo de crecimiento.
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

# ¿Sube el logro a medida que avanzan los ensayos del año? Es la serie que
# motivó sacar la pendiente del modelo (punto 1): sube y baja según la
# dificultad de cada ensayo, no de forma monótona.
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

# ---- 8c. EFECTO DEL IRT: comparación y gráfico (NUEVO v6) --------------
# Todo lo de esta sección compara las dos versiones que el script ya
# calculó: la cruda (porcentaje de logro observado) y la IRT (puntaje
# verdadero). No decide nada — USAR_IRT ya decidió — pero deja la evidencia
# en cada corrida.

# Dificultad de la BATERÍA que aplicó cada colegio, en puntos de logro y
# medida ANTES del IRT: cuánto más fácil (+) o difícil (-) fue el conjunto
# de ensayos que administró, respecto del promedio de su grupo. Es la
# variable que el IRT tiene que neutralizar, así que es el eje natural
# contra el cual mirar la corrección.
dificultad_bateria <- ensayos_ind %>%
  group_by(agno, grado, area, rbd_revisado) %>%
  summarise(dif_bateria = mean(dif_ensayo - dif_grupo), .groups = "drop")

comparacion_irt <- school_features %>%
  select(agno, grado, area, rbd_revisado, n_estudiantes,
         mean_logro_crudo, mean_logro_irt) %>%
  left_join(dificultad_bateria, by = c("agno", "grado", "area", "rbd_revisado")) %>%
  group_by(agno, grado, area) %>%
  mutate(
    dif_puntos = mean_logro_irt - mean_logro_crudo,
    # Lo que importa operativamente no es el nivel (las dos escalas no
    # tienen por qué coincidir en media) sino el CAMBIO DE POSICIÓN: si un
    # colegio sube o baja respecto de los demás.
    pct_crudo  = percent_rank(mean_logro_crudo),
    pct_irt    = percent_rank(mean_logro_irt),
    salto_pp   = 100 * (pct_irt - pct_crudo)
  ) %>%
  ungroup()

# Confiabilidad del índice individual en las dos versiones.
conf_ind_comp <- bind_rows(
  est_indice_crudo %>% mutate(version = "crudo"),
  est_indice_irt   %>% mutate(version = "irt")
) %>%
  group_by(version, agno, grado, area) %>%
  summarise(conf_indice = mean(rel_indice, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = version, values_from = conf_indice,
              names_prefix = "conf_indice_")

tabla_efecto_irt <- comparacion_irt %>%
  group_by(agno, grado, area) %>%
  summarise(
    n_colegios      = n(),
    r_niveles       = cor(mean_logro_crudo, mean_logro_irt),
    dif_media_pts   = mean(dif_puntos),
    dif_abs_pts     = mean(abs(dif_puntos - mean(dif_puntos))),
    salto_abs_medio = mean(abs(salto_pp)),
    salto_p90       = quantile(abs(salto_pp), 0.90, names = FALSE),
    salto_max       = max(abs(salto_pp)),
    # Si el salto correlaciona con la dificultad de la batería, la
    # corrección es SISTEMÁTICA: los colegios que aplicaron ensayos fáciles
    # bajan y los que aplicaron difíciles suben. Si fuera ruido, daría ~0.
    r_salto_dificultad = cor(salto_pp, dif_bateria, use = "complete.obs"),
    .groups = "drop"
  ) %>%
  left_join(conf_ind_comp, by = c("agno", "grado", "area")) %>%
  mutate(gana_conf_pp = 100 * (conf_indice_irt - conf_indice_crudo))

cat("\n\n=============================================================\n")
cat("EFECTO DEL IRT SOBRE LOS INSUMOS DEL MODELO\n")
cat("=============================================================\n")
cat("r_niveles          correlación entre el logro crudo y el IRT por colegio\n")
cat("salto_abs_medio    cambio absoluto medio de percentil del colegio (pp)\n")
cat("r_salto_dificultad correlación del salto con la dificultad de la batería\n")
cat("                   (negativa = quien aplicó ensayos fáciles baja)\n")
cat("gana_conf_pp       ganancia de confiabilidad del índice individual (pp)\n\n")
print(
  tabla_efecto_irt %>%
    transmute(agno, grado, area, n_colegios,
              r_niveles = round(r_niveles, 3),
              salto_abs_medio = round(salto_abs_medio, 1),
              salto_p90 = round(salto_p90, 1),
              salto_max = round(salto_max, 1),
              r_salto_dif = round(r_salto_dificultad, 2),
              conf_crudo = round(conf_indice_crudo, 3),
              conf_irt   = round(conf_indice_irt, 3),
              gana_conf_pp = round(gana_conf_pp, 1)) %>%
    as.data.frame()
)

# --- Gráfico de tres paneles -------------------------------------------
anio_fig <- max(comparacion_irt$agno)

# (1) Dificultad de cada forma: logro observado vs. dificultad IRT.
forma_fig <- irt_items %>%
  filter(agno == anio_fig, is.finite(a), is.finite(b)) %>%
  group_by(grado, area, forma) %>%
  summarise(esperado_theta0 = 100 * mean(plogis(a * (0 - b))),
            logro_observado = 100 * mean(p_correcta), .groups = "drop") %>%
  mutate(panel = "1. Dificultad de cada ensayo",
         grupo = paste(grado, area))

p1 <- ggplot(forma_fig, aes(logro_observado, esperado_theta0, color = grupo)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
  geom_point(size = 2.5, alpha = 0.85) +
  labs(title = "1. Dificultad de cada ensayo",
       subtitle = "Cada punto es una forma. El eje Y es lo que sacaría el MISMO\nestudiante medio; el eje X, el logro que se observó.",
       x = "Logro observado (%)", y = "Logro esperado a habilidad media (%)",
       color = NULL) +
  theme_minimal() + theme(legend.position = "bottom")

# (2) El colegio: logro crudo vs. IRT, coloreado por la dificultad de su
#     batería. La nube se inclina: los colegios de batería fácil (claros)
#     quedan por debajo de la diagonal.
col_fig <- comparacion_irt %>% filter(agno == anio_fig, n_estudiantes >= 5)

p2 <- ggplot(col_fig, aes(mean_logro_crudo, mean_logro_irt, color = dif_bateria)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
  geom_point(alpha = 0.7, size = 1.6) +
  scale_color_gradient2(low = "#B2182B", mid = "grey80", high = "#2166AC",
                        midpoint = 0,
                        name = "Dificultad de la batería aplicada (+ = más fácil)",
                        guide = guide_colorbar(title.position = "top",
                                               title.hjust = 0.5,
                                               barwidth = 14, barheight = 0.6)) +
  facet_wrap(~ grado + area, scales = "free") +
  labs(title = "2. Nivel del colegio: antes y después del IRT",
       subtitle = "Los colegios que aplicaron ensayos fáciles (azul) caen bajo la diagonal",
       x = "Logro medio crudo (%)", y = "Logro medio IRT (%)") +
  theme_minimal() + theme(legend.position = "bottom")

# (3) La prueba de que la corrección es sistemática y no ruido: el cambio
#     de percentil contra la dificultad de la batería aplicada.
p3 <- ggplot(col_fig, aes(dif_bateria, salto_pp)) +
  geom_hline(yintercept = 0, color = "grey60") +
  geom_vline(xintercept = 0, color = "grey60") +
  geom_point(alpha = 0.55, size = 1.4) +
  geom_smooth(method = "lm", se = FALSE, color = "#B2182B", linewidth = 0.8) +
  facet_wrap(~ grado + area, scales = "free") +
  labs(title = "3. La corrección es sistemática, no ruido",
       subtitle = "Cambio de puesto del colegio contra la dificultad de la batería que aplicó",
       x = "Dificultad de la batería aplicada (puntos de logro; + = más fácil)",
       y = "Cambio de percentil (pp)") +
  theme_minimal()

png(dir_salidas %>% file.path("efecto_irt.png"),
    width = 2200, height = 2600, res = 190)
gridExtra::grid.arrange(
  p1, p2, p3, ncol = 1, heights = c(1, 1.25, 1.25),
  top = grid::textGrob(
    sprintf("Efecto de la calibración IRT sobre los insumos del modelo (%s)", anio_fig),
    gp = grid::gpar(fontsize = 15, fontface = "bold"))
)
dev.off()

write_csv(tabla_efecto_irt, dir_salidas %>% file.path("efecto_irt.csv"))
saveRDS(comparacion_irt,    dir_salidas %>% file.path("comparacion_irt.rds"))

cat("\nGráfico guardado en", dir_salidas %>% file.path("efecto_irt.png"), "\n")

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
cat("AVISO v5 - nombres cambiados. Cualquier script aguas abajo que no\n")
cat("sea 02, 02b, 03 o 04 (típicamente 05_figuras_presentacion.R) hay que\n")
cat("revisarlo a mano:\n")
cat("   colegio_efecto_historico -> nivel_hist_colegio\n")
cat("   desvio_colegio           -> desvio_nivel\n")
cat("   n_anios_hist             -> n_anios_nivel_hist\n")
cat("   efecto_historico.rds     -> nivel_historico.rds\n")
cat("Y dos cambios de SIGNIFICADO que no cambian de nombre:\n")
cat("   z_ensayo   ya no tiene sd 1 dentro del colegio (ver secc. 4b)\n")
cat("   rho        cambió de escala; 03 lo lee de confiabilidades\n")
cat("-------------------------------------------------------------\n")
