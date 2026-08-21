# Mapa por provincias

Representa valores por provincia mediante un mapa coroplético de España.
Los datos se relacionan con la cartografía mediante el código
provincial.

## Usage

``` r
grafico_mapa_provincias(
  datos,
  codigo = "cod_provincia",
  valor = "valor",
  titulo = NULL,
  titulo_leyenda = NULL,
  paleta = "PuBu",
  n_clases = 5L,
  accuracy = 0.1,
  canarias = c("linea", "recuadro", "ninguno"),
  incluir_ceuta_melilla = FALSE
)
```

## Arguments

- datos:

  Un `data.frame` con los datos que se desean representar.

- codigo:

  Cadena de texto con el nombre de la columna que contiene el código
  provincial. Por defecto, `"cod_provincia"`.

- valor:

  Cadena de texto con el nombre de la variable numérica que se desea
  representar. Por defecto, `"valor"`.

- titulo:

  Título del gráfico. Por defecto, `NULL`.

- titulo_leyenda:

  Título de la leyenda. Por defecto, `NULL`.

- paleta:

  Nombre de una paleta secuencial de RColorBrewer. Por defecto,
  `"PuBu"`.

- n_clases:

  Número de intervalos utilizados para clasificar los valores. Por
  defecto, `5`.

- accuracy:

  Precisión de las etiquetas mostradas sobre el mapa. Por defecto,
  `0.1`.

- canarias:

  Elemento utilizado para señalar la posición desplazada de Canarias.
  Puede ser `"linea"`, `"recuadro"` o `"ninguno"`.

- incluir_ceuta_melilla:

  Si es `TRUE`, mantiene Ceuta y Melilla en el mapa. Por defecto,
  `FALSE`.

## Value

Un objeto de clase `ggplot`.

## Details

Canarias se muestran desplazadas junto a la Península. El usuario puede
añadir una línea, un recuadro o ningún elemento auxiliar alrededor de
ellas.

Cada provincia debe tener como máximo una observación. La función no
agrega ni resume observaciones automáticamente.

## Examples

``` r
datos_ejemplo <- data.frame(
  cod_provincia = sprintf("%02d", 1:52),
  valor = seq(50, 90, length.out = 52)
)

p <- grafico_mapa_provincias(
  datos_ejemplo
)
p


if (FALSE) { # \dontrun{
datos <- consultar_indicador(
  "ID_DEL_INDICADOR",
  anyo = 2024,
  tipo_territorio = "provincia"
)

grafico_mapa_provincias(datos)
} # }
```
