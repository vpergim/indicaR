# =============================================================================
# Construcción de la cartografía interna
# =============================================================================
#
# Este script construye los objetos cartográficos utilizados por las funciones
# de visualización del paquete.
#
# La cartografía se obtiene mediante mapSpain únicamente durante el desarrollo
# del paquete. Las funciones públicas NO deben descargar cartografía ni llamar
# a mapSpain en tiempo de ejecución.
#
# Objetos construidos:
#
#   mapa_ccaa
#     Geometrías de comunidades y ciudades autónomas.
#
#   mapa_provincias
#     Geometrías provinciales.
#
#   mapa_canarias_linea
#     Línea decorativa utilizada para señalar el desplazamiento de Canarias.
#
#   mapa_canarias_recuadro
#     Recuadro completo alrededor de Canarias desplazadas.
#
# Los objetos se guardan definitivamente en R/sysdata.rda desde
# data-raw/build_internal_data.R. Este script NO debe escribir sysdata.rda.
#
# Fuente cartográfica:
#   mapSpain, a partir de cartografía administrativa de Eurostat GISCO.
#
# Parámetros fijados deliberadamente:
#   - resolución: 1:3 millones ("03")
#   - CRS: ETRS89, EPSG:4258
#   - Canarias desplazadas junto a la Península
#
# Fijar estos parámetros explícitamente evita que futuras modificaciones de los
# valores por defecto de mapSpain cambien silenciosamente la cartografía
# incluida en el paquete.
# =============================================================================


# -----------------------------------------------------------------------------
# Dependencia de desarrollo
# -----------------------------------------------------------------------------

if (!requireNamespace("mapSpain", quietly = TRUE)) {
  stop(
    paste0(
      "El paquete 'mapSpain' es necesario para reconstruir la cartografía ",
      "interna. Instálelo antes de ejecutar este script."
    ),
    call. = FALSE
  )
}


# -----------------------------------------------------------------------------
# Parámetros cartográficos comunes
# -----------------------------------------------------------------------------

resolucion_mapa <- "03"
epsg_mapa <- 4258
mover_canarias <- TRUE


# -----------------------------------------------------------------------------
# Comunidades y ciudades autónomas
# -----------------------------------------------------------------------------

mapa_ccaa <- mapSpain::esp_get_ccaa(
  moveCAN = mover_canarias,
  resolution = resolucion_mapa,
  epsg = epsg_mapa
)


# -----------------------------------------------------------------------------
# Provincias
# -----------------------------------------------------------------------------

mapa_provincias <- mapSpain::esp_get_prov(
  moveCAN = mover_canarias,
  resolution = resolucion_mapa,
  epsg = epsg_mapa
)


# -----------------------------------------------------------------------------
# Elementos auxiliares de Canarias
#
# "right" reproduce el tipo de línea decorativa utilizado habitualmente por
# mapSpain para indicar que Canarias se muestran desplazadas.
#
# "box" genera un rectángulo completo. Conservamos ambos objetos para que las
# futuras funciones gráficas puedan ofrecer las dos alternativas sin volver a
# depender de mapSpain.
# -----------------------------------------------------------------------------

mapa_canarias_linea <- mapSpain::esp_get_can_box(
  style = "right",
  moveCAN = mover_canarias,
  epsg = epsg_mapa
)

mapa_canarias_recuadro <- mapSpain::esp_get_can_box(
  style = "box",
  moveCAN = mover_canarias,
  epsg = epsg_mapa
)


# -----------------------------------------------------------------------------
# Validaciones básicas
# -----------------------------------------------------------------------------

objetos_cartograficos <- list(
  mapa_ccaa = mapa_ccaa,
  mapa_provincias = mapa_provincias,
  mapa_canarias_linea = mapa_canarias_linea,
  mapa_canarias_recuadro = mapa_canarias_recuadro
)

# Los mapas territoriales son objetos `sf`, mientras que los elementos
# auxiliares de Canarias devueltos por mapSpain son geometrías `sfc`.
# Ambos tipos son válidos para su uso posterior con ggplot2::geom_sf().
tipos_validos <- vapply(
  objetos_cartograficos,
  function(x) {
    inherits(x, "sf") || inherits(x, "sfc")
  },
  logical(1)
)

objetos_invalidos <- names(objetos_cartograficos)[
  !tipos_validos
]

if (length(objetos_invalidos) > 0L) {
  stop(
    paste0(
      "Los siguientes objetos cartográficos no son objetos 'sf' ni 'sfc': ",
      paste(objetos_invalidos, collapse = ", "),
      "."
    ),
    call. = FALSE
  )
}

crs_objetos <- vapply(
  objetos_cartograficos,
  function(x) {
    sf::st_crs(x)$epsg
  },
  integer(1)
)

if (any(is.na(crs_objetos)) || any(crs_objetos != epsg_mapa)) {
  stop(
    paste0(
      "La cartografía no utiliza de forma homogénea EPSG:",
      epsg_mapa,
      "."
    ),
    call. = FALSE
  )
}


# -----------------------------------------------------------------------------
# Validaciones de identificadores territoriales
#
# codauto y cpro serán las claves preferentes para relacionar posteriormente
# los datos estadísticos con las geometrías. Deben existir y ser únicos.
# -----------------------------------------------------------------------------

if (!"codauto" %in% names(mapa_ccaa)) {
  stop(
    "`mapa_ccaa` no contiene la columna esperada `codauto`.",
    call. = FALSE
  )
}

if (anyNA(mapa_ccaa$codauto)) {
  stop(
    "`mapa_ccaa$codauto` contiene valores NA.",
    call. = FALSE
  )
}

if (anyDuplicated(mapa_ccaa$codauto)) {
  stop(
    "`mapa_ccaa$codauto` contiene códigos duplicados.",
    call. = FALSE
  )
}

if (!"cpro" %in% names(mapa_provincias)) {
  stop(
    "`mapa_provincias` no contiene la columna esperada `cpro`.",
    call. = FALSE
  )
}

if (anyNA(mapa_provincias$cpro)) {
  stop(
    "`mapa_provincias$cpro` contiene valores NA.",
    call. = FALSE
  )
}

if (anyDuplicated(mapa_provincias$cpro)) {
  stop(
    "`mapa_provincias$cpro` contiene códigos duplicados.",
    call. = FALSE
  )
}


# -----------------------------------------------------------------------------
# Resumen para inspección manual
# -----------------------------------------------------------------------------

message(
  "Cartografía construida correctamente:\n",
  "- CCAA: ", nrow(mapa_ccaa), "\n",
  "- Provincias: ", nrow(mapa_provincias), "\n",
  "- Línea de Canarias: ", length(mapa_canarias_linea), " geometría(s)\n",
  "- Recuadro de Canarias: ", length(mapa_canarias_recuadro), " geometría(s)\n",
  "- CRS: EPSG:", epsg_mapa
)
