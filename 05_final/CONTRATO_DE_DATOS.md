# Contrato de datos

Qué archivos espera el modelo, cómo se llaman y qué tienen adentro.

Es la parte más frágil del proceso: el pipeline deduce metadata del **nombre del archivo** y del **nombre de las hojas**. Si eso no calza, la corrida se detiene — `CHEQUEAR.bat` lo detecta antes de calcular nada.

---

## 1. Excel de ensayos

### Dónde van

```
<data_in>/ensayos_santillana/<lo que sea>/*.xlsx
```

La carpeta `ensayos_santillana` debe llamarse exactamente así. Adentro puede haber subcarpetas con cualquier nombre (hoy se usa `2025_medicion_santillana/Lenguaje/`): el modelo busca de forma recursiva.

### Cómo se llaman

El nombre del archivo **es** la metadata. Debe contener:

| Dato | Cómo se detecta | Ejemplo |
|---|---|---|
| Año | cuatro dígitos que empiezan con `20` | `2026` |
| Grado | si empieza con `IIM` → 2° medio; si no → 4° básico | `IIM_...` o `4B_...` |
| Área | si contiene `LEN` → lenguaje; si no → matemática | `..._LEN.xlsx` |
| N.º de ensayo | el dígito inmediatamente después de `Ensayo` | `Ensayo3` |

Sólo se leen archivos `.xlsx` que contengan `Ensayo` en el nombre.

**Ejemplos válidos**

```
4B_Ensayo1_2026_MAT.xlsx      -> 2026, 4° básico, matemática, ensayo 1
IIM_Ensayo4_2026_LEN.xlsx     -> 2026, 2° medio, lenguaje, ensayo 4
4B_Ensayo2BAS_2023_MAT.xlsx   -> 2023, 4° básico, matemática, ensayo 2, forma BAS
```

**Dos advertencias**

- **El número de ensayo es de un solo dígito.** `Ensayo10` se leería como ensayo 1, en silencio. El chequeo lo detecta y detiene la corrida. Si algún año hay diez o más ensayos, hay que ajustar el patrón en `00_irt_calibracion.R`.
- **`IIM` tiene que ir al principio.** `Ensayo1_IIM_2026_LEN.xlsx` se clasificaría como 4° básico.

### Qué tienen adentro

Dos hojas, con estos nombres exactos:

**Hoja `Matriz`** — una fila por ítem, con la respuesta correcta.

| Columna | Contenido |
|---|---|
| `item_id` | identificador numérico del ítem, único dentro de la prueba |
| `item_no` | número de orden |
| `clave_correcta_s` | la alternativa correcta (`A`, `B`, `C`, `D`) |

Un ítem puede tener **dos alternativas aceptadas** (`"B, C"`): pasa cuando después de aplicar la prueba se acepta más de una opción. El modelo lo maneja. Lo que no puede es una clave vacía.

**Hoja `Datos`** — una fila por estudiante.

| Columna | Contenido |
|---|---|
| `id_usuario_curso` | identificador del estudiante. **La llave.** Nunca el nombre: hay homónimos |
| `id_colegio` | identificador del establecimiento |
| `item_<n>_id_<id>` | una columna por ítem con la respuesta (`A`–`D`, o `-` si omitió) |

El patrón `item_<n>_id_<id>` es obligatorio: de ahí sale el cruce con la hoja `Matriz`.

---

## 2. Resultados SIMCE

```
<data_in>/resultados_simce/*.zip
```

Los zip tal cual los entrega la Agencia de Calidad, sin descomprimir. Los procesan los scripts de `01_preparar_datos/`.

**Sólo hacen falta cuando llega un SIMCE nuevo.** Para predecir una ronda no se tocan.

---

## 3. Archivos ya procesados

```
<data_intermedia>/ensayo_santillana/ensayos_santillana_corregido.parquet
<data_intermedia>/simce/resultados_simce_rbd_corregido.parquet
<data_intermedia>/simce/consolidado_datos_simce_alu.parquet
```

Los generan los scripts de `01_preparar_datos/`. El modelo los lee, no los crea.

---

## 4. Un detalle sobre OneDrive

Si los datos están en OneDrive con **Archivos a pedido**, un archivo puede figurar con su tamaño real sin estar descargado. R falla al abrirlo con un mensaje que no explica nada.

`CHEQUEAR.bat` lo detecta leyendo un byte de cada archivo, que es la única prueba confiable.

Para evitarlo: clic derecho sobre la carpeta de datos → **"Conservar siempre en este dispositivo"**.

---

## 5. Qué pasa si cambia el número de ensayos

El modelo se adapta solo: no hay nada que configurar. Está probado desde una única forma por grado y área.

Lo que **sí** hay que mirar en ese caso es el aviso de **comparabilidad del banco** en el informe. `mean_logro` es el porcentaje del banco de ítems del año que un colegio contestaría bien; con una sola forma ese banco tiene ~40 ítems en vez de ~240, y si su dificultad se corre respecto de años anteriores, el modelo lo lee como si los colegios hubieran cambiado.

El informe lo mide y avisa cuando el salto supera los 3 puntos.
