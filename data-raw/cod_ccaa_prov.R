# ============================================================================
# Construcción de la tabla territorial de referencia
# ============================================================================
#
# Este script importa y valida la relación oficial entre:
#
#   - códigos de provincia;
#   - nombres de provincia;
#   - códigos de comunidad autónoma;
#   - nombres de comunidad autónoma.
#
# La tabla se utilizará posteriormente para normalizar los nombres y códigos
# territoriales de los datasets fuente.
#
# El fichero RDS original se conserva en:
#
#   data-raw/sources/cod_ccaa_prov.rds
#
# El objeto resultante se guarda como dato INTERNO del paquete y, por tanto,
# no será accesible mediante:
#
#   indicaR::cod_ccaa_prov
#
# Las funciones internas del paquete sí podrán utilizarlo.
#
# ============================================================================


# ============================================================================
# 1. Ruta al fichero fuente
# ============================================================================

path_cod_ccaa_prov <- file.path(
  "data-raw",
  "sources",
  "cod_ccaa_prov.rds"
)


# ============================================================================
# 2. Comprobar que existe el fichero
# ============================================================================

if (!file.exists(path_cod_ccaa_prov)) {
  stop(
    paste0(
      "No se encuentra el fichero territorial de referencia:\n",
      path_cod_ccaa_prov
    ),
    call. = FALSE
  )
}


# ============================================================================
# 3. Leer el objeto
# ============================================================================

cod_ccaa_prov <- readRDS(
  path_cod_ccaa_prov
)


# ============================================================================
# 4. Comprobar la estructura
# ============================================================================

columnas_esperadas <- c(
  "CPRO",
  "NPRO",
  "CCA",
  "NCA"
)

columnas_faltantes <- setdiff(
  columnas_esperadas,
  names(cod_ccaa_prov)
)

if (length(columnas_faltantes) > 0L) {
  stop(
    paste0(
      "Faltan columnas obligatorias en `cod_ccaa_prov`: ",
      paste(
        columnas_faltantes,
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}


# Conservamos únicamente las columnas necesarias y en un orden conocido.
cod_ccaa_prov <- cod_ccaa_prov[
  columnas_esperadas
]


# ============================================================================
# 5. Normalizar tipos
# ============================================================================
#
# Los códigos deben mantenerse como character.
#
# Esto es fundamental porque códigos como:
#
#   "04"
#   "01"
#
# perderían el cero inicial si se convirtieran a integer.
#

cod_ccaa_prov$CPRO <- as.character(
  cod_ccaa_prov$CPRO
)

cod_ccaa_prov$NPRO <- as.character(
  cod_ccaa_prov$NPRO
)

cod_ccaa_prov$CCA <- as.character(
  cod_ccaa_prov$CCA
)

cod_ccaa_prov$NCA <- as.character(
  cod_ccaa_prov$NCA
)


# ============================================================================
# 6. Validar códigos
# ============================================================================

if (anyNA(cod_ccaa_prov$CPRO)) {
  stop(
    "`CPRO` contiene valores NA.",
    call. = FALSE
  )
}

if (anyNA(cod_ccaa_prov$CCA)) {
  stop(
    "`CCA` contiene valores NA.",
    call. = FALSE
  )
}


# Los códigos provinciales deben tener exactamente dos dígitos.
if (any(!grepl("^[0-9]{2}$", cod_ccaa_prov$CPRO))) {
  stop(
    "`CPRO` contiene códigos con formato no válido.",
    call. = FALSE
  )
}


# Los códigos de comunidad autónoma deben tener exactamente dos dígitos.
if (any(!grepl("^[0-9]{2}$", cod_ccaa_prov$CCA))) {
  stop(
    "`CCA` contiene códigos con formato no válido.",
    call. = FALSE
  )
}


# ============================================================================
# 7. Validar nombres
# ============================================================================

if (anyNA(cod_ccaa_prov$NPRO) || any(cod_ccaa_prov$NPRO == "")) {
  stop(
    "`NPRO` contiene nombres de provincia vacíos o NA.",
    call. = FALSE
  )
}

if (anyNA(cod_ccaa_prov$NCA) || any(cod_ccaa_prov$NCA == "")) {
  stop(
    "`NCA` contiene nombres de comunidad autónoma vacíos o NA.",
    call. = FALSE
  )
}


# ============================================================================
# 8. Validar unicidad de provincias
# ============================================================================
#
# Cada código de provincia debe identificar una única provincia.
#

if (anyDuplicated(cod_ccaa_prov$CPRO)) {
  stop(
    "`CPRO` contiene códigos de provincia duplicados.",
    call. = FALSE
  )
}


# ============================================================================
# 9. Validar correspondencia CCAA código-nombre
# ============================================================================
#
# Una comunidad autónoma aparecerá varias veces, una por provincia.
# Pero cada código CCA debe corresponder siempre al mismo nombre NCA.
#

relacion_ccaa <- unique(
  cod_ccaa_prov[
    c(
      "CCA",
      "NCA"
    )
  ]
)

if (anyDuplicated(relacion_ccaa$CCA)) {
  stop(
    paste0(
      "Existe algún código `CCA` asociado a más de un nombre de ",
      "comunidad autónoma."
    ),
    call. = FALSE
  )
}


# ============================================================================
# 10. Ordenar la tabla
# ============================================================================

cod_ccaa_prov <- cod_ccaa_prov[
  order(
    cod_ccaa_prov$CCA,
    cod_ccaa_prov$CPRO
  ),
]

rownames(cod_ccaa_prov) <- NULL


# ============================================================================
# 11. Guardar como dato interno del paquete
# ============================================================================

usethis::use_data(
  cod_ccaa_prov,
  internal = TRUE,
  overwrite = TRUE
)
