# Listar indicadores disponibles

Devuelve una tabla con los indicadores disponibles en el paquete y su
información descriptiva básica.

## Usage

``` r
listar_indicadores()
```

## Value

Un `data.frame` con una fila por indicador y las siguientes columnas:

- id_dataset:

  Identificador único del indicador dentro del paquete. Es la clave que
  debe utilizarse en funciones como
  [`info_indicador()`](https://vpergim.github.io/indicaR/reference/info_indicador.md)
  y, posteriormente, en las funciones de consulta de datos.

- variable:

  Nombre o descripción de la variable estadística representada por el
  indicador.

- comentarios:

  Información adicional sobre el indicador cuando está disponible. Puede
  contener aclaraciones metodológicas o sobre la fuente.

- operacion:

  Operación o fuente estadística de la que procede el indicador.

- organismo:

  Organismo responsable de la información estadística.

- periodo:

  Período de referencia disponible según la metadata de la fuente. Para
  conocer la cobertura temporal calculada a partir de los datos
  almacenados puede utilizarse
  [`info_indicador()`](https://vpergim.github.io/indicaR/reference/info_indicador.md).

## Details

Esta función está pensada como punto de entrada para descubrir qué
indicadores pueden consultarse y obtener el `id_dataset` necesario para
utilizar otras funciones de la API, como
[`info_indicador()`](https://vpergim.github.io/indicaR/reference/info_indicador.md).

## See also

[`info_indicador()`](https://vpergim.github.io/indicaR/reference/info_indicador.md)

## Examples

``` r
listar_indicadores()
#> # A tibble: 90 × 5
#>    id_dataset                         variable       operacion organismo periodo
#>    <chr>                              <chr>          <chr>     <chr>     <chr>  
#>  1 CCAA_SEXO_A_01                     ESPERANZA DE … Indicado… INE       1975-2…
#>  2 CCAA_SEXO_A_02                     ESPERANZA DE … Indicado… INE       1975-2…
#>  3 CCAA_SEXO_EDAD_A_01                TASA DE MORTA… Tablas d… INE       1991-2…
#>  4 CCAA_SEXO_EDAD_A_02                ESPERANZA DE … Tablas d… INE       1991-2…
#>  5 CCAA_SEXO_EDAD_NIVELEDUCATIVO_A_01 ESPERANZA DE … Indicado… INE       2016-2…
#>  6 CV_SECT1_TAMANYOEMP_A_01           EMPRESAS por … DIRCE     INE       2020-2…
#>  7 CVPROV_TIPOVIVIENDA_M_01           COMPRAVENTA D… Estadíst… INE       2007M0…
#>  8 TERRITORIO_A_01                    INTENSIDAD DE… Encuesta… INE       2014-2…
#>  9 TERRITORIO_A_02                    GASTOS TOTALE… Encuesta… INE       2014-2…
#> 10 TERRITORIO_A_03                    TOTAL CONSUMO… Encuesta… INE       2015-2…
#> # ℹ 80 more rows
```
