# Primer modelo para estimar y predecir SIMCE (colegio e individual)

## 1. Datos y qué se cruza con qué

- **`ensayos_santiyana.csv`**: 1 fila por estudiante x ensayo Santillana (2023-2025, grados **4b** y **2m** únicamente, áreas lenguaje/matemática, 1 a 6 ensayos por año). Se usa `rbd_revisado` para identificar el colegio (tiene menos datos faltantes y corrige ~700 casos donde `rbd` venía mal).
- **`resultados_simce_rbd.csv`**: promedio SIMCE oficial por colegio x año x grado x área (2022-2025; incluye además 6b-2024 y 8b-2025, que **no** tienen ensayo asociado y por lo tanto quedan fuera de este modelo).
- El cruce es por **(año, grado, área, rbd_revisado)**. Solo esa combinación de columnas tiene sentido: un colegio puede tener SIMCE muy distinto en 4b que en 2m, o en lenguaje que en matemática.

Limpieza aplicada (ver `01_preparar_datos.R`):
- Se acota `porcentaje_logro` a 100 (≈0.01% de filas venían con valores hasta 160, probablemente error de origen).
- Se colapsan estudiantes con 2 registros para el mismo número de ensayo (versiones "basal"/"extenso" del mismo test).
- Se eliminan ~6.000 filas duplicadas exactas que trae `resultados_simce_rbd.csv` para 2024.
- Se descartan colegios con `promedio_simce = 0` (sin resultado publicado ese año/área, no es un puntaje real).

## 2. Features

A nivel **estudiante** (por año x grado x área), a partir de sus ensayos del año:

| feature | descripción |
|---|---|
| `mean_logro` | promedio de % de logro en todos los ensayos rendidos |
| `pred_final_logro` | % de logro esperado en el ensayo 6, según un modelo de crecimiento (ver abajo) — mismo punto de referencia para todos, hayan o no llegado a rendir el 6° |
| `slope_logro` | pendiente de mejora/caída durante el año, del modelo de crecimiento |
| `n_evals` | cuántos de los 6 ensayos rindió |

A nivel **colegio**, las mismas 4 variables agregadas (promedio simple entre sus estudiantes) más `n_estudiantes`, `sd_entre_estud` (dispersión, sólo referencial) y `colegio_efecto_historico` (ver punto 3c).

### 2a. Modelo de crecimiento por estudiante (`pred_final_logro`, `slope_logro`)

En vez de calcular la pendiente "a mano" (regresión simple por estudiante) e imputar 0 a quienes sólo rindieron 1 ensayo (~27% de los casos), se ajusta un modelo mixto (`lme4::lmer`) por cada (año, grado, área), que **encoge (shrinkage)** las estimaciones hacia el promedio según cuánta evidencia real hay.

**Ajuste hecho tras probarlo con los datos reales:** el diseño original — pendiente aleatoria *por estudiante* — resultó **no identificable**: con tantos estudiantes de 1 solo ensayo, el modelo terminaba con más parámetros aleatorios que observaciones, y `lme4` lo rechaza (con razón — no hay evidencia para estimar una tendencia individual con un solo punto). Se resolvió subiendo la pendiente a nivel de **colegio** (que sí tiene datos de sobra) y dejando sólo el **intercepto** como aleatorio por estudiante:

```r
lmer(porcentaje_logro ~ ensayo_num + (1 + ensayo_num | rbd_revisado) + (1 | id_usuario_curso))
```

Interpretación: la tendencia durante el año (¿el colegio mejora o cae de ensayo en ensayo?) se estima a nivel de colegio; cada estudiante hereda la tendencia de su colegio y aporta su propio nivel (cuánto está por sobre o bajo esa trayectoria). `pred_final_logro` se acota a [0, 100] porque es una extrapolación y en casos extremos puede salirse de rango.

## 3. El modelo principal

Regresión lineal múltiple, **una por combinación grado x área** (4b-lenguaje, 4b-matemática, 2m-lenguaje, 2m-matemática):

```
promedio_simce ~ mean_logro + pred_final_logro + slope_logro + n_evals_prom + colegio_efecto_historico
```

**¿Por qué empezar acá y no con random forest / XGBoost?**
Con ~100-220 colegios por grupo, un modelo lineal simple es un punto de partida honesto: es interpretable ante un equipo directivo, fácil de auditar, difícil de sobreajustar, y **se puede aplicar directamente a nivel de estudiante** reusando los mismos coeficientes (ver punto 5). Es la base sobre la que conviene iterar, no el modelo final. (El modelo de crecimiento del punto 2a y el efecto histórico del punto 3c sí son mixtos — la complejidad se puso ahí, donde hay datos de sobra para sostenerla, no en el modelo final con ~100-220 filas por grupo.)

### 3c. Efecto histórico del colegio (`colegio_efecto_historico`)

Aprovecha que `resultados_simce_rbd.csv` trae SIMCE desde 2022, aunque los ensayos recién parten en 2023: para cada año que se quiere predecir, se ajusta un modelo mixto (intercepto aleatorio por colegio, controlando por año) usando **sólo** el SIMCE de años **estrictamente anteriores** — nunca el del año objetivo, para no filtrar información del futuro. Con un solo año previo disponible (caso de 2023, que sólo tiene 2022 antes) no se puede separar año de colegio, así que se usa el promedio_simce de ese año, centrado, como aproximación. Colegios sin historia previa reciben efecto 0.

## 4. Validación (out-of-time)

Se entrena con los años más antiguos y se prueba en el año más reciente disponible (2025), que es el escenario real: predecir un año que aún no se conoce con datos de años ya cerrados. **Resultados reales** de correr el pipeline completo (`02_modelo_escolar.R`) sobre los datos entregados:

| grupo | MAE modelo | MAE "predecir el promedio histórico" | mejora | R² (test 2025) |
|---|---|---|---|---|
| 4b · lenguaje | 9.8 pts | 16.1 pts | 39% | 0.58 |
| 4b · matemática | 9.5 pts | 18.6 pts | 49% | 0.69 |
| 2m · lenguaje | 9.0 pts | 21.0 pts | 57% | 0.78 |
| 2m · matemática | 13.2 pts | 38.2 pts | 66% | 0.86 |

`colegio_efecto_historico` resultó el predictor más fuerte en los 4 grupos (t entre 7 y 16, muy por sobre las features de ensayo del año en curso) — el historial del colegio pesa mucho, y los ensayos del año aportan encima de eso. Es información real: estas cifras salieron de correr el script tal cual está entregado, con los datos que enviaste (`ensayos_santiyana.csv` + `resultados_simce_rbd.csv`), no de un prototipo.

## 5. De colegio a individuo

El SIMCE oficial **no** entrega puntajes por estudiante, sólo promedios por colegio — por eso no existe una variable "SIMCE individual" contra la cual entrenar directamente. La estrategia de este primer modelo:

1. Entrenar el modelo con datos agregados por colegio (que sí tienen verdad conocida).
2. Aplicar el **mismo modelo, mismos coeficientes**, reemplazando cada feature del colegio por la del estudiante individual (su propio `mean_logro`, `pred_final_logro`, `slope_logro`, `n_evals`; `colegio_efecto_historico` es la misma para todos los estudiantes de un mismo colegio, porque describe al colegio, no al estudiante).

Esto funciona porque el modelo es lineal en variables que significan lo mismo a ambos niveles. El supuesto detrás es que la relación ensayo→SIMCE dentro de un colegio es parecida a la relación entre colegios. `03_prediccion_nueva_ronda.R` incluye un chequeo automático: el promedio de las predicciones individuales de un colegio debería quedar cerca de la predicción hecha directamente a nivel de colegio — al correrlo, la diferencia dio esencialmente 0 (~1e-15, ruido de punto flotante), como corresponde matemáticamente a un modelo lineal aplicado a una variable que es el promedio de sus componentes.

## 6. Cómo correr

```
simce_model/
├── data/
│   ├── ensayos_santiyana.csv        <- copiar aquí
│   └── resultados_simce_rbd.csv     <- copiar aquí
├── 01_preparar_datos.R
├── 02_modelo_escolar.R
├── 03_prediccion_nueva_ronda.R
└── output/                          <- se genera solo
```

```r
install.packages(c("tidyverse", "broom", "lme4"))  # una sola vez

setwd("simce_model")   # o abrir el .Rproj en esa carpeta
source("01_preparar_datos.R")   # ajusta 12 modelos mixtos; toma ~1 minuto
source("02_modelo_escolar.R")
source("03_prediccion_nueva_ronda.R")
```

Los tres scripts fueron efectivamente corridos de punta a punta con `ensayos_santiyana.csv` y `resultados_simce_rbd.csv` para validar que el pipeline funciona sin errores; `01_preparar_datos.R` tardó ~42 segundos en total.

Salidas en `output/`: `metricas_validacion.csv`, `diagnostico_observado_vs_predicho.png`, `modelos_escolares.rds`, `efecto_historico.rds`, `predicciones_colegio.csv`, `predicciones_individual.csv`, `chequeo_coherencia.csv`.

Para la **siguiente ronda real** (2026): a medida que los colegios vayan rindiendo los ensayos de 2026, basta con ir agregando esas filas a `ensayos_santiyana.csv` y volver a correr `01` y `03` — no hace falta esperar los 6 ensayos ni el SIMCE oficial (que suele publicarse recién a mediados del año siguiente).

## 7. Limitaciones y siguientes pasos

- **Sin verdad individual**: la predicción por estudiante es una extrapolación razonada, no está calibrada contra SIMCE real de estudiantes. Debe presentarse como estimación (idealmente como rango, no como número seco), no como equivalencia oficial.
- **La pendiente durante el año se estima por colegio, no por estudiante**: con ~27% de los estudiantes rindiendo un solo ensayo, no hay suficiente evidencia para una tendencia individual (ver punto 2a). Si en años futuros más estudiantes acumulan varios ensayos, vale la pena reintentar una pendiente por estudiante.
- **Falta modelar la dispersión, no sólo el promedio**: hoy la predicción individual hereda muy poca variabilidad real entre estudiantes de un mismo colegio (el modelo nunca vio esa variabilidad durante el entrenamiento). Un segundo modelo que prediga la SD del SIMCE por colegio permitiría calibrar el ancho de las predicciones individuales.
- **Matching por percentil como alternativa a "aplicar los mismos coeficientes"**: si en algún momento se consigue el SIMCE individual del colegio (sin vínculo al ensayo, sólo la distribución), se puede asignar a cada estudiante el percentil equivalente de esa distribución real, en vez de extrapolar coeficientes de colegio a individuo.
- **Sin variables de contexto del colegio** (dependencia, ruralidad, comuna, grupo socioeconómico): el archivo `simce2m2025_rbd_preliminar.xlsx` del proyecto las trae para 2m-2025, y podrían mejorar el modelo si se consiguen para más años/grados.
- **Modelos no lineales** (gradient boosting / random forest / GAM) como segunda iteración, una vez que este modelo lineal esté validado en producción y sirva de referencia comparativa (baseline a superar).
