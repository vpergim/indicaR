# Variables utilizadas mediante evaluación no estándar
# -----------------------------------------------------
#
# R CMD check no puede identificar estáticamente algunas variables que se
# evalúan dentro de expresiones de dplyr o ggplot2, ni determinados objetos
# internos utilizados por las funciones de normalización.
#
# Se declaran aquí explícitamente para indicar que son nombres conocidos por
# el paquete y evitar falsos positivos en el análisis estático.

utils::globalVariables(
  c(
    ".data",
    "id_dataset",
    "variable",
    "comentarios",
    "operacion",
    "organismo",
    "periodo",
    "cod_ccaa_prov",
    "diccionario_territorial"
  )
)
