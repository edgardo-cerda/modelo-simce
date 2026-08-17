# Modelo para estimar y predecir SIMCE — colegio e individual (v3)

> **Qué cambia respecto a la versión anterior.** Antes el pipeline entrenaba sólo con promedios por colegio y la predicción individual era una extrapolación: aplicarle los coeficientes del colegio a cada estudiante. Con `consolidado_datos_simce_alu.parquet` ahora hay **SIMCE por alumno**, y eso permite modelar lo que faltaba —cuán distintos son entre sí los estudiantes de un mismo colegio— y **validar** las predicciones individuales contra puntajes reales. Se implementan las dos versiones pedidas: **A) media + dispersión** y **B) matching por percentil**.

---

## 1. Datos

| archivo | contenido | rol |
|---|---|---|
| `ensayos_santillana_corregido.parquet` | 1 fila por estudiante × ensayo (2023–2025, 4b y 2m, lenguaje/matemática, 1 a 6 ensayos) | predictores |
| `resultados_simce_rbd_corregido.parquet` | promedio SIMCE oficial por colegio × año × grado × área (2022–2025) | target del nivel |
| **`consolidado_datos_simce_alu.parquet`** *(nuevo)* | **SIMCE por alumno** (2.6M filas, 2022–2025, `ptje_mate` / `ptje_lect` + error estándar de medición `eem_*`) | target de la dispersión, plantilla de forma y **verdad para validar** |

Dos verificaciones antes de usar el archivo de alumnos:

- **Es el mismo universo que el archivo por RBD.** Al agregarlo por colegio reproduce el `promedio_simce` oficial casi exactamente (diferencia mediana 0.27 puntos, correlación 0.994). No hay que elegir entre uno y otro: el agregado sigue siendo el target del nivel, el individual aporta todo lo demás.
- **Los ensayos cubren la cohorte completa.** El número de estudiantes con ensayo por colegio es 107–112% de los que rinden SIMCE (mediana por grupo). No es una submuestra selectiva de "los que se preparan": es prácticamente el curso entero, con algunos que rinden ensayo y después no dan el SIMCE. Esto es lo que hace legítimo comparar percentiles internos entre ambas fuentes; si la cobertura fuera de 40%, el matching por percentil estaría sesgado y habría que descartarlo.

Limpieza (ver `01_preparar_datos.R`): se acota `porcentaje_logro` a 100; se colapsan ensayos duplicados (versiones basal/extenso); se eliminan duplicados exactos de 2024 en el archivo RBD; se descartan `promedio_simce = 0`; en el archivo de alumnos se descartan puntajes ausentes (~20%: matriculados que no rindieron o quedaron excluidos) y se exige **n ≥ 15 alumnos** para usar la sd interna de un colegio, porque con menos su error relativo (~1/√(2(n−1))) la vuelve ruido.

---

## 2. La estructura del problema

La predicción individual se descompone en tres piezas, cada una estimada donde hay evidencia para sostenerla:

```
puntaje_estudiante  =  MEDIA del colegio        (modelo 02, validado contra SIMCE oficial)
                    +  ANCHO del colegio        (modelo 02b, validado contra sd real por alumno)
                    ×  POSICIÓN del estudiante  (ensayos, dentro de su propio colegio)
```

Las dos versiones se diferencian **sólo en la tercera pieza** — cómo se traduce "posición dentro del colegio" a puntos SIMCE. Comparten media y ancho.

Esto también aclara por qué la v2 se quedaba corta: tenía la pieza 1 y el ranking de la pieza 3, pero le faltaba la 2, y sin ancho las predicciones individuales salían todas apiñadas alrededor del promedio del colegio.

---

## 3. Features

**Estudiante** (por año × grado × área): `mean_logro`, `pred_final_logro` y `slope_logro` (del modelo mixto de crecimiento, sin cambios respecto a la v2), `n_evals`, y **nuevo**: `pct_ensayo` y `z_ensayo` — el percentil y la posición estandarizada del estudiante **dentro de su propio colegio**.

El índice sobre el que se ordena a los estudiantes es `pred_final_logro_raw` (la versión **sin acotar** a [0,100] de `pred_final_logro`: el acotado generaba empates en los extremos que arruinaban el ranking). Ya viene encogido por el modelo mixto, así que un estudiante con un solo ensayo queda cerca del promedio de su colegio en vez de irse a un extremo por ruido de una sola medición — que es exactamente lo que se quiere de un ranking.

**Colegio**: las mismas agregadas, más `colegio_efecto_historico` (nivel histórico, ahora con prior contextual — ver punto 3b), y **features de dispersión**: `sd_entre_estud`, `iqr_logro_ensayo` (p90−p10 del logro entre estudiantes, más robusto que la sd) y `sd_hist_colegio` (cuán heterogéneo ha sido el colegio en años anteriores).

Todo lo histórico —nivel, dispersión y forma— se calcula con **ventana expansiva**: sólo años estrictamente anteriores al año que se predice.

### 3b. Variables de contexto: GSE, dependencia y ruralidad

`cod_grupo` (GSE: Bajo → Alto), `cod_depe1` / `cod_depe2` (dependencia, incluyendo la nueva categoría Servicio Local de Educación), `cod_rural_rbd` (urbano/rural).

**No entran como predictores adicionales de la regresión final.** Probadas así, el MAE out-of-time se mueve entre −0.2 y +0.3 puntos y en un grupo empeora. La razón es que `colegio_efecto_historico` ya las contiene: el nivel histórico de un colegio es, en buena medida, su GSE y su dependencia (la expectativa contextual correlaciona **0.70** con el nivel histórico). Sumadas aparte sólo gastan grados de libertad en una regresión de ~150–220 filas.

Donde sí valen oro es donde la historia falla. Sin efecto histórico, el contexto sube el R² out-of-time de 0.40 a **0.62–0.66**. Son un buen *sustituto* de la historia, no un complemento.

Por eso entran como **prior del efecto histórico**:

```
antes:  promedio_simce ~ factor(agno) + (1 | rbd)
ahora:  promedio_simce ~ factor(agno) + GSE + dependencia + ruralidad + (1 | rbd)

colegio_efecto_historico = expectativa_contextual + desvío_del_colegio
```

Esto resuelve tres cosas a la vez:

| situación del colegio | antes | ahora |
|---|---|---|
| sin historia previa | recibía 0 = "promedio del país" | recibe lo esperado para su contexto |
| 1 año de historia | encogido hacia el promedio del país | encogido hacia el promedio de su contexto |
| historia larga | — | prácticamente igual (manda su propia evidencia) |

**Los coeficientes de contexto se estiman en el universo nacional, no en la base Santillana.** Los ~6.400 colegios de 4b y ~3.000 de 2m tienen todos los GSE, 30% de ruralidad y las 6 dependencias. La base Santillana está muy concentrada: **57–63% particular pagado, GSE promedio ~4, 1–4% rural**. Estimar el efecto del GSE con 180 colegios de los cuales casi ninguno es rural sería pedirle a los datos algo que no tienen. Se estiman donde hay variación y se transfieren.

Beneficio lateral: como el contexto llega a los modelos finales convertido en un solo número, **desaparece el problema de las categorías ausentes**. Con 150–220 colegios por grupo es perfectamente posible que "corporación de administración delegada" o "rural" no aparezca en entrenamiento y sí en el año de prueba — eso hace fallar `predict()` en R, y de hecho falló al probarlo. Con esta arquitectura no puede pasar.

**El contexto se usa siempre rezagado.** El GSE se recalcula en cada medición y cambia de categoría en **~26% de los colegios de un año a otro**; `cod_depe2` se movió 8% entre 2024 y 2025 por el paso a Servicio Local. Ambas se publican junto con los resultados del año, así que usarlas del año objetivo sería filtrar información inexistente al momento de predecir. Se usa el último registro estrictamente anterior (cobertura 99.4%).

**Sobre la dispersión, una advertencia honesta:** el contexto explica muy poco de la sd interna (R² ≈ 0.01 en el universo nacional). La sd baja de forma monótona pero suave con el GSE (42.2 puntos en GSE bajo → 37.6 en GSE alto, 4b matemática) y el particular pagado es el más homogéneo (37.5), pero esas diferencias son chicas frente a la variación entre colegios del mismo estrato. Ahí el contexto sirve como valor por defecto para colegios sin historia, no como fuente de señal.

**Lo que no cambió:** se probó condicionar la plantilla de forma (`forma_z`) por GSE dentro de cada tercil de nivel, y las diferencias son de ≤0.06 en z (~2 puntos SIMCE). El tercil de nivel ya captura lo que el GSE aportaría; la versión B queda igual.

---

## 4. Modelo de dispersión (`02b_modelo_dispersion.R`) — versión A, pieza 2

```
sd_simce ~ sd_entre_estud + iqr_logro_ensayo + sd_hist_colegio + mean_logro + colegio_efecto_historico
```

Una regresión por grado × área, ponderada por `n_alu_simce` (la sd de un colegio de 20 alumnos es mucho más ruidosa que la de uno de 200; sin ponderar, los colegios chicos dominan el ajuste con puro ruido). La predicción se acota al rango observado en entrenamiento.

Por qué entra cada bloque:

- `sd_hist_colegio` — la heterogeneidad de un colegio es **persistente**; es el predictor individualmente más informativo.
- `sd_entre_estud` / `iqr_logro_ensayo` — la señal fresca del año en curso, lo único que puede detectar un cambio que la historia no ve.
- nivel (`mean_logro`, `colegio_efecto_historico`) — la dispersión depende del nivel por efecto piso/techo: un colegio muy arriba o muy abajo tiene menos recorrido interno.

---

## 5. Las dos versiones de predicción individual (`03_prediccion_nueva_ronda.R`)

### Versión A — media + dispersión modelada

```
pred_A     = mu_hat + rho · sd_hat · z_ensayo
sd_residual = sd_hat · sqrt(1 − rho²)          →  rango p10–p90 = pred_A ± 1.28 · sd_residual
```

El parámetro `rho` es la correlación supuesta entre ensayo y SIMCE **a nivel individual, dentro del colegio**. Que no sea 1 es la parte importante: hace que la predicción **se encoja hacia el promedio del colegio** (regresión a la media). Es lo correcto para un pronóstico puntual —predecirle el extremo a un estudiante extremo maximiza el error esperado— y a cambio deja la incertidumbre residual explícita, que es lo que se usa para reportar un rango en vez de un número seco.

**Versión A es la apropiada cuando la pregunta es sobre un estudiante**: "¿cuánto se espera que saque, con cuánta incertidumbre?".

### Versión B — matching por percentil

```
pred_B = mu_hat + sd_hat · Q_forma(pct_ensayo)
```

`Q_forma` es la **forma empírica** de la distribución interna de puntajes, calculada en `01` sobre los ~6.400 colegios reales del archivo de alumnos: se estandariza dentro de cada colegio, `z = (ptje − media_colegio) / sd_colegio`, y se guarda la función de cuantiles de esos `z`, por grado, área y **tercil de nivel del colegio** (los colegios de bajo rendimiento tienen cola derecha algo más larga y los de alto, lo contrario — efecto piso/techo de la prueba). Reemplaza el supuesto de normalidad por la forma que los datos efectivamente tienen. El tercil se asigna con la media **predicha**, no la observada, porque en producción la observada no existe.

**Versión B no encoge**: por construcción, el conjunto de predicciones de un colegio reproduce la distribución que ese colegio debería tener —media, ancho y colas—. **Es la apropiada cuando la pregunta es sobre el grupo**: "¿cómo se va a ver la distribución de mi curso?", "¿cuántos estudiantes quedarían en nivel insuficiente?", "¿quiénes están en la cola de riesgo?". A cambio, cada predicción individual es puntualmente menos precisa que la de A, porque asume ranking perfecto.

### 5b. De dónde sale `rho`, y por qué no se puede estimar

`rho` **no es estimable** con estos datos: haría falta un vínculo alumno-a-alumno entre el ensayo Santillana y el registro SIMCE, y ese vínculo no existe (`id_usuario_curso` vs. `mrun`/`idalumno`). Pero sí está **acotado**: la correlación entre dos mediciones no puede superar la raíz del producto de sus confiabilidades. `01_preparar_datos.R` estima ambas sobre los datos entregados:

| grado · área | confiabilidad ensayo (promedio del estudiante) | confiabilidad SIMCE (dentro del colegio) | techo de `rho` |
|---|---|---|---|
| 4b · matemática | 0.89 | 0.84 | 0.86 |
| 4b · lenguaje | 0.86 | 0.85 | 0.85 |
| 2m · lenguaje | 0.83 | 0.84 | 0.83 |
| 2m · matemática | 0.79 | 0.64 | 0.71 |

(La del SIMCE sale del error estándar de medición que trae el propio archivo: `1 − eem²/var_interna`. La del ensayo, del ICC entre estudiantes descontando la dificultad de cada ensayo, corregido por Spearman-Brown según cuántos ensayos rindió cada uno.)

El default es **`RHO_ENSAYO_SIMCE <- 0.70`**, conservador y por debajo del techo en los cuatro grupos. `04` reporta la sensibilidad a `rho`, pero **el valor no debe elegirse optimizando contra esa tabla**: la métrica individual de `04` está sesgada por construcción a favor de `rho = 1` (ver punto 6).

---

## 6. Validación (`04_validacion_individual.R`)

No se puede evaluar estudiante por estudiante sin el vínculo entre fuentes. Pero sí se puede evaluar algo casi tan útil, y que la v2 no podía evaluar en absoluto: **si el conjunto de predicciones de un colegio reproduce el conjunto de puntajes que ese colegio realmente obtuvo**.

1. **Error de cuantiles** — se comparan los percentiles 5, 10, …, 95 de las predicciones contra los observados del mismo colegio, y se promedia el error absoluto (distancia de Wasserstein-1 discretizada). Captura errores de nivel, de ancho y de forma en un solo número. Es la métrica principal.
2. **Error individual bajo ranking perfecto** — a cada estudiante se le asigna el puntaje observado de su mismo percentil dentro del colegio. Es una **cota optimista**, no una estimación: supone que el ensayo ordena perfectamente. Se reporta porque acota por abajo lo que se le puede prometer a un colegio, y porque permite mirar la cobertura de los rangos de A. Por la misma razón **no sirve para elegir `rho`**.

Ambas se comparan contra dos referencias: la v2 legado (coeficientes de colegio aplicados al estudiante) y "sólo la media" (a todos el mismo puntaje predicho). Todo out-of-time: los modelos de `02` y `02b` excluyen el año de prueba, y la forma y la dispersión histórica usan ventana expansiva.

### Resultados esperados

⚠️ **Estas cifras vienen de un prototipo en Python que replica la lógica del pipeline sobre los mismos parquet, no de correr los scripts R** (el entorno donde se prepararon no tiene R instalado). El prototipo usa features simplificadas —`mean_logro` en vez de las del modelo mixto de crecimiento— así que el pipeline completo debería dar resultados iguales o algo mejores. **Correr `01`→`04` y reemplazar esta tabla con la salida real es el primer paso pendiente.**

**Modelo de dispersión, prueba out-of-time en 2025** (MAE sobre la sd interna, en puntos):

| grupo | MAE modelo | baseline "misma sd para todos" | baseline "su propia sd histórica" | R² |
|---|---|---|---|---|
| 4b · matemática | 3.5 | 4.4 | 4.1 | 0.31 |
| 4b · lenguaje | 4.2 | 5.4 | 4.9 | 0.29 |
| 2m · lenguaje | 5.5 | 6.3 | 5.9 | 0.20 |
| 2m · matemática | 6.0 | 6.9 | 6.3 | 0.23 |

Modesto pero real, y le gana tanto a la constante como a la historia del propio colegio. Para dimensionarlo: la sd interna típica va de 41 puntos (4b matemática) a 53 (2m matemática), así que el error es del orden del 8–11% del ancho que se está estimando.

**Error de cuantiles de la predicción individual, 2025** (puntos SIMCE, promedio por colegio):

| grupo | versión B | v2 legado | sólo la media | B con media y sd observadas |
|---|---|---|---|---|
| 4b · matemática | 11.0 | ≈ sólo media | 29.5 | 4.1 |
| 4b · lenguaje | 11.3 | ≈ sólo media | 36.1 | 5.2 |
| 2m · lenguaje | 11.0 | ≈ sólo media | 32.2 | 5.0 |
| 2m · matemática | 16.0 | ≈ sólo media | 42.5 | 6.7 |

Dos lecturas:

- **La mejora es grande**: de ~30–42 puntos de error distribucional a ~11–16. Modelar la dispersión y la forma es la mayor parte de lo que faltaba.
- **La última columna dice dónde está el error restante.** Si a la versión B se le entregan la media y la sd *observadas* del colegio, el error cae a 4–7 puntos. Es decir: la plantilla de forma es muy buena y casi todo el error remanente viene de predecir la media y el ancho del colegio, no de cómo se reparte a los estudiantes. **La siguiente iteración debe atacar el modelo de colegio, no el paso de colegio a individuo.**

---

## 7. Cómo correr

```
simce_model/
├── 01_preparar_datos.R          <- ahora también carga el parquet de alumnos
├── 02_modelo_escolar.R          <- modelo del NIVEL (sin cambios de diseño)
├── 02b_modelo_dispersion.R      <- NUEVO: modelo del ANCHO
├── 03_prediccion_nueva_ronda.R  <- reescrito: versiones A y B
├── 04_validacion_individual.R   <- NUEVO: validación contra SIMCE por alumno
└── output/modelo_lme/           <- se genera solo
```

```r
install.packages(c("tidyverse", "broom", "lme4", "arrow", "config"))

source("01_preparar_datos.R")        # modelos mixtos + plantillas de forma
source("02_modelo_escolar.R")
source("02b_modelo_dispersion.R")
source("03_prediccion_nueva_ronda.R")
source("04_validacion_individual.R") # sólo corre sobre un año ya cerrado
```

`01` espera el archivo nuevo en `<ruta_data_intermedia>/simce/consolidado_datos_simce_alu.parquet`; si está en otra carpeta, hay una sola línea que ajustar (marcada en el script). El paso más lento sigue siendo el de los modelos mixtos de crecimiento, más el cálculo de las plantillas de forma sobre ~2M puntajes individuales.

**Salidas principales:** `predicciones_individual.csv` (columnas `pred_A`, `pred_A_inf`, `pred_A_sup`, `pred_B` y `pred_v2_legado` para comparar), `predicciones_colegio.csv` (media y ancho predichos), `metricas_dispersion.csv`, `validacion_distribucional.csv`, `validacion_individual.csv`, `validacion_sensibilidad_rho.csv`, y los tres PNG de diagnóstico.

**Chequeos automáticos que conviene mirar en cada corrida** (los imprime `03`): el promedio de las predicciones individuales de un colegio debe reproducir la predicción del colegio en ambas versiones; la sd de las predicciones de B debe dar ~1 × el ancho predicho, la de A ~`rho` × el ancho, y la de la v2 legado ~0.1–0.2 × el ancho — ese último número es, en una línea, el problema que esta versión resuelve.

---

## 8. Cuál usar, y qué no prometer

- **Reporte a un estudiante o apoderado** → versión A, **siempre como rango**, nunca como número seco. Con `rho = 0.70` y una sd interna de ~45 puntos, el rango p10–p90 mide unos 80 puntos. Es ancho, y esa amplitud es información honesta, no una falla del modelo: es la incertidumbre que efectivamente hay.
- **Reporte a un equipo directivo sobre un curso o colegio** → versión B: cuántos estudiantes en cada nivel de logro, quiénes están en la cola de riesgo, cómo se compara la distribución esperada con la del año pasado.
- **Nunca** presentar ninguna de las dos como equivalente a un puntaje SIMCE oficial. El SIMCE no reporta puntajes individuales, y estas son estimaciones basadas en otro instrumento.

---

## 9. Limitaciones y siguientes pasos

- **Sigue sin haber vínculo alumno-a-alumno.** Es la limitación de fondo: impide estimar `rho`, impide validar predicción por predicción y obliga a apoyarse en el supuesto de que el ranking del ensayo se parece al del SIMCE. Si alguna vez se consigue un cruce (aunque sea parcial y anonimizado, para una muestra de colegios), se puede estimar `rho` directamente y calibrar el modelo individual contra verdad real. **Es, por lejos, lo que más valor agregaría.**
- **El error remanente está en el modelo de colegio, no en el paso a individuo** (punto 6). Las variables de contexto ya se incorporaron (punto 3b) y ayudan sobre todo donde falta historia; para el resto, el siguiente paso son modelos no lineales, ahora que el lineal sirve de referencia comparativa.
- **La base Santillana no es representativa del país** (57–63% particular pagado, GSE promedio ~4, 1–4% rural). Los modelos finales se ajustan sobre ella, así que sus coeficientes valen para esa población. Si la base se diversifica —o si se quiere ofrecer el modelo a un colegio municipal rural— hay que revisar el ajuste, no dar por hecho que transfiere. `04` reporta el error desglosado por estrato GSE justamente para vigilar eso.
- **`rho` es un parámetro asumido, no estimado.** Está acotado por confiabilidad y el default es conservador, pero es un supuesto y debe declararse como tal ante cualquier usuario del modelo.
- **La pendiente durante el año se sigue estimando por colegio, no por estudiante** (~27% rinde un solo ensayo). Si en años futuros más estudiantes acumulan varios ensayos, vale la pena reintentar la pendiente individual.
- **La plantilla de forma es nacional, no del propio colegio.** Con más años acumulados se podría usar la forma histórica del colegio mismo cuando tenga suficientes datos, encogiéndola hacia la nacional según cuánta evidencia tenga — la misma lógica de shrinkage que ya se usa en el resto del pipeline.
- **Cobertura de los rangos de A sin verificar de verdad.** La cobertura que reporta `04` se calcula bajo emparejamiento por ranking, así que es optimista. La cobertura real sólo se puede medir con datos vinculados.
