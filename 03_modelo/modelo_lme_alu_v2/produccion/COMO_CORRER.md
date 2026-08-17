# Cómo predecir una nueva ronda

Guía para quien ejecuta. No hace falta saber R.

---

## La primera vez, en un computador nuevo

**1. Instalar R.** Desde <https://cran.r-project.org/bin/windows/base/>. Aceptar todas las opciones por defecto.

**2. Instalar los paquetes.** Abrir `instalar_dependencias.R` con RStudio y presionar *Source*. Si no hay RStudio, abrir una consola en esta carpeta y correr:

```
Rscript instalar_dependencias.R
```

Toma varios minutos y sólo se hace una vez. Al final debe decir `Todo listo`.

**3. Configurar las rutas.** Abrir `rutas.txt` con el Bloc de notas y escribir las tres carpetas de datos. Usar barras normales `/`, sin comillas.

---

## Cada vez que llega una ronda de ensayos

**1. Copiar los Excel** a la carpeta de ensayos, respetando el nombre de archivo. El nombre no es decorativo: de ahí sale el año, el grado, el área y el número de ensayo. Ver `CONTRATO_DE_DATOS.md`.

**2. Verificar que estén descargados.** Si los datos están en OneDrive, seleccionar la carpeta, hacer clic derecho y elegir **"Conservar siempre en este dispositivo"**. Un archivo que figura pero no está bajado hace fallar la corrida.

**3. Doble clic en `CHEQUEAR.bat`.** Revisa los archivos sin calcular nada, en menos de un minuto. Si algo está mal, dice qué y cómo arreglarlo. Conviene hacerlo siempre antes del paso siguiente.

**4. Doble clic en `PREDECIR.bat`.** Corre todo. Toma entre 10 y 30 minutos según cuántos ensayos haya. **No cerrar la ventana.**

**5. Leer el informe.** Al terminar se abre solo en el navegador. Empieza con un veredicto:

| Veredicto | Qué significa |
|---|---|
| 🟢 **SE PUEDE PUBLICAR** | Todos los chequeos pasaron. |
| 🟡 **REVISAR ANTES DE PUBLICAR** | Se puede usar, pero hay secciones que conviene leer. |
| 🔴 **NO PUBLICAR SIN REVISAR** | Los números pueden estar sistemáticamente corridos. |

---

## Dónde quedan los resultados

En la carpeta de salidas, dentro de `modelo_lme_alu_v2/entregas/<año>/`:

| Archivo | Qué es |
|---|---|
| `informe_calidad.html` | El veredicto y los chequeos. **Abrir esto primero.** |
| `predicciones_colegio.csv` | Una fila por colegio: promedio y dispersión predichos. |
| `predicciones_individual.csv` | Una fila por estudiante: puntaje predicho. |
| `log_corrida.txt` | Detalle técnico. Sirve si algo falló. |

---

## Si algo falla

La ventana negra no se cierra sola: el mensaje de error queda a la vista. Casi todos los problemas son de estos tres tipos:

**"No se encontró R"** — R no está instalado, o se instaló para otro usuario de Windows. Volver al paso 1.

**"archivo(s) no están descargados"** — es OneDrive. Clic derecho sobre la carpeta → "Conservar siempre en este dispositivo".

**"nombre que no se entiende"** — un Excel no sigue la convención de nombres. El mensaje dice cuál. Ver `CONTRATO_DE_DATOS.md`.

Si el error es otro, adjuntar `log_corrida.txt` al pedir ayuda: ahí está el detalle completo.

---

## Cuando llega un SIMCE nuevo

Esto es distinto y **no lo hace `PREDECIR.bat`**. Predecir usa los coeficientes ya calculados; cuando llega un SIMCE nuevo hay que recalcularlos para que el modelo lo aproveche.

El chequeo lo detecta y avisa: *"Hay SIMCE que los modelos no vieron"*. No impide predecir, pero conviene re-estimar antes.

La re-estimación corre, **en este orden**, desde la carpeta del proyecto:

```
Rscript 01_preparar_datos/01_cargar_y_consolidar_simce.R
Rscript 01_preparar_datos/04_limpieza_errores_y_outliers_simce.R
Rscript 03_modelo/modelo_lme_alu_v2/00_irt_calibracion.R
Rscript 03_modelo/modelo_lme_alu_v2/01a_insumos_simce.R
Rscript 03_modelo/modelo_lme_alu_v2/01b_insumos_ensayo.R
Rscript 03_modelo/modelo_lme_alu_v2/02_modelo_escolar.R
Rscript 03_modelo/modelo_lme_alu_v2/02b_modelo_dispersion.R
Rscript 03_modelo/modelo_lme_alu_v2/03_prediccion_nueva_ronda.R
Rscript 03_modelo/modelo_lme_alu_v2/04_validacion_individual.R
```

Toma bastante más que predecir: `01a` solo son unos 6 minutos, y `00` varios más.

**Conviene que la haga alguien que conozca el modelo.** No es sólo apretar play: es el momento de revisar si las métricas de precisión se movieron, si los coeficientes se mantienen estables al incorporar el año nuevo, y si las decisiones que quedaron abiertas siguen siendo válidas. Los criterios están en `REGISTRO_VERSIONES_Y_PRUEBAS.txt`, en la carpeta de arriba.

---

## Lo que el modelo no puede hacer

Vale la pena tenerlo presente al entregar los resultados.

- **No se valida contra el año que predice.** El año nuevo todavía no rindió la prueba. La precisión que reporta el informe es la medida sobre el último año cerrado.
- **La predicción individual es una posición esperada dentro del curso**, no un puntaje garantizado. No existe forma de validarla estudiante por estudiante: no hay vínculo entre el alumno del ensayo y el del SIMCE.
- **El nivel absoluto es lo más frágil.** El logro de los ensayos se mueve entre años por razones que no son sólo mejora real. Por eso el informe revisa la comparabilidad del banco de ítems y avisa cuando se corre.
