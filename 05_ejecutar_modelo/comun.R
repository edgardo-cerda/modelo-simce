# =============================================================
# comun.R
# -------------------------------------------------------------
# Funciones que comparten el validador y el master. No se ejecuta solo:
# lo cargan los otros con source().
# =============================================================

# Raíz del proyecto. Los scripts del modelo leen "config.yml" con ruta
# RELATIVA, así que hay que ejecutarlos con el directorio de trabajo
# puesto ahí.
#
# Se busca subiendo carpetas hasta encontrar config.yml, en vez de contar
# niveles. Contar niveles ata este script a la estructura del repositorio y
# se rompe en silencio apenas alguien mueve o renombra una carpeta, que es
# exactamente lo que pasó cuando la carpeta de producción se sacó de
# 03_modelo/ y quedó en la raíz.
raiz_proyecto <- function(dir_inicio) {
  d <- normalizePath(dir_inicio, mustWork = TRUE)
  for (i in 1:8) {
    if (file.exists(file.path(d, "config.yml"))) return(d)
    padre <- normalizePath(file.path(d, ".."), mustWork = FALSE)
    if (identical(padre, d)) break
    d <- padre
  }
  stop("No se encontró config.yml subiendo desde ", dir_inicio, ".\n",
       "Esta carpeta tiene que estar dentro del proyecto modelo-simce.")
}

# Carpeta con los scripts del modelo. Se identifica por su contenido —los
# dos scripts que la ruta de predicción necesita— y no por su nombre, por
# el mismo motivo de arriba. Se busca hasta dos niveles bajo la raíz.
carpeta_modelo <- function(dir_raiz) {
  nivel1 <- list.dirs(dir_raiz, recursive = FALSE)
  nivel1 <- nivel1[!grepl("[/\\\\]\\.", nivel1)]      # ignorar .git, .Rproj.user
  candidatos <- c(nivel1, unlist(lapply(nivel1, list.dirs, recursive = FALSE)))

  tiene <- function(d) {
    all(file.exists(file.path(d, c("00_calibracion_irt.R",
                                   "01b_insumos_ensayo.R",
                                   "03_prediccion.R"))))
  }
  ok <- candidatos[vapply(candidatos, tiene, logical(1))]

  if (length(ok) == 0) {
    stop("No se encontraron los scripts del modelo dentro de ", dir_raiz, ".\n",
         "Se buscó una carpeta con 00_calibracion_irt.R, 01b_insumos_ensayo.R\n",
         "y 03_prediccion.R, hasta dos niveles bajo la raíz.")
  }
  if (length(ok) > 1) {
    warning("Hay más de una carpeta con los scripts del modelo:\n  ",
            paste(ok, collapse = "\n  "), "\nSe usa la primera.")
  }
  normalizePath(ok[1])
}

# --- rutas.txt --------------------------------------------------------
leer_rutas <- function(archivo) {
  if (!file.exists(archivo)) {
    stop("No se encuentra rutas.txt en ", archivo)
  }
  lineas <- readLines(archivo, warn = FALSE, encoding = "UTF-8")
  lineas <- lineas[!grepl("^\\s*#", lineas) & grepl("=", lineas)]
  if (length(lineas) == 0) stop("rutas.txt no tiene ninguna ruta configurada.")
  # Las rutas de Windows traen ":" pero nunca "=", así que partir por el
  # primer "=" es seguro.
  clave <- trimws(sub("=.*$", "", lineas))
  valor <- trimws(sub("^[^=]*=", "", lineas))
  setNames(valor, clave)
}

# --- config.yml -------------------------------------------------------
# Los scripts del modelo leen sus rutas de config.yml, indexado por nombre
# de usuario de Windows. En vez de pedirle al equipo que edite un YAML, el
# master sincroniza ese archivo a partir de rutas.txt. Se edita por líneas
# para no reformatear ni perder los perfiles de otras personas.
sincronizar_config <- function(ruta_config, usuario, rutas) {

  bloque <- c(
    paste0(usuario, ":"),
    paste0("  ruta_data_in: \"", rutas[["data_in"]], "\""),
    paste0("  ruta_data_intermedia: \"", rutas[["data_intermedia"]], "\""),
    paste0("  ruta_outputs: \"", rutas[["data_out"]], "\"")
  )

  if (!file.exists(ruta_config)) {
    writeLines(c("default:", bloque[-1], "", bloque), ruta_config)
    return("creado")
  }

  lineas <- readLines(ruta_config, warn = FALSE)
  inicio <- which(grepl(paste0("^", usuario, "\\s*:\\s*$"), lineas))

  if (length(inicio) == 0) {
    writeLines(c(lineas, "", bloque), ruta_config)
    return("agregado")
  }

  # El perfil existe: se reemplaza su bloque completo (las líneas
  # indentadas que siguen al encabezado).
  i <- inicio[1]
  j <- i + 1
  while (j <= length(lineas) && grepl("^\\s+\\S", lineas[j])) j <- j + 1
  nuevas <- c(lineas[seq_len(i - 1)], bloque, if (j <= length(lineas)) lineas[j:length(lineas)])

  if (identical(nuevas, lineas)) return("sin cambios")
  writeLines(nuevas, ruta_config)
  "actualizado"
}

# --- Nombres de archivo de ensayo -------------------------------------
# El nombre del archivo ES la metadata. Esta función implementa el mismo
# formato que espera 00_calibracion_irt.R, y se usa para validarlo antes de
# correr nada. Ver DATOS_REQUERIDOS.md.
parsear_nombre_ensayo <- function(nombres) {
  data.frame(
    archivo = nombres,
    agno    = suppressWarnings(as.integer(sub(".*?(20\\d\\d).*", "\\1", nombres))),
    grado   = ifelse(grepl("^IIM", nombres), "2m", "4b"),
    area    = ifelse(grepl("LEN", nombres), "lenguaje", "matematica"),
    ensayo  = suppressWarnings(as.integer(sub(".*Ensayo(\\d).*", "\\1", nombres))),
    # Un "Ensayo10" se leería como ensayo 1: el patrón del pipeline toma un
    # solo dígito. Se detecta acá para avisar antes de que pase.
    ensayo_multidigito = grepl("Ensayo\\d\\d", nombres),
    stringsAsFactors = FALSE
  )
}

# --- Archivos de OneDrive ---------------------------------------------
# Con "Archivos a pedido" activado, un archivo puede figurar con su tamaño
# real pero no estar descargado. R falla al abrirlo con un mensaje que no
# explica nada. Leer un byte es la prueba real de que está disponible.
archivo_disponible <- function(ruta) {
  if (!file.exists(ruta)) return(FALSE)
  con <- tryCatch(file(ruta, "rb"), error = function(e) NULL)
  if (is.null(con)) return(FALSE)
  on.exit(close(con), add = TRUE)
  !inherits(tryCatch(readBin(con, "raw", 1), error = function(e) e), "error")
}

# --- Registro de chequeos ---------------------------------------------
nuevo_registro <- function() {
  data.frame(estado = character(), chequeo = character(),
             detalle = character(), stringsAsFactors = FALSE)
}

anotar <- function(reg, estado, chequeo, detalle = "") {
  rbind(reg, data.frame(estado = estado, chequeo = chequeo,
                        detalle = detalle, stringsAsFactors = FALSE))
}

imprimir_registro <- function(reg) {
  simbolo <- c(OK = "  [ok]   ", AVISO = "  [aviso]", ERROR = "  [ERROR]")
  for (i in seq_len(nrow(reg))) {
    cat(simbolo[[reg$estado[i]]], reg$chequeo[i], "\n")
    if (nzchar(reg$detalle[i])) {
      for (l in strsplit(reg$detalle[i], "\n", fixed = TRUE)[[1]]) {
        cat("            ", l, "\n")
      }
    }
  }
}
