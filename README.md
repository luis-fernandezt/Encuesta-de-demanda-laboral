# Análisis de la Encuesta de Demanda Laboral (ENADEL)

La **ENADEL** tiene como base la caracterización de las necesidades de capital humano de las empresas y las brechas existentes en el mercado laboral.

Este es un **breve análisis exploratorio**, estimando el **número de vacantes actuales**, por **regiones**. 
---

## Requisitos previos y preparación de la base de datos

1. **Base de datos**  
   Se trabajó con la base de datos **ENADEL 2024 y 2025**, disponibles en:
   
   [ENADEL 2024 - CSV](https://www.subtrab.gob.cl/wp-content/uploads/2025/05/ENADEL_2024_externa.csv)    
   
   [ENADEL 2025 - CSV](https://www.subtrab.gob.cl/wp-content/uploads/2026/08/FORMATO-CSV-ENADEL.zip)

2. **Librerías necesarias**

```r
library(readr)
library(tidyverse)
library(survey)
library(srvyr)
library(calidad)
library(knitr)
```

3. **Aplicar Factor de expansion**
Según el documento metodológico de ENADEL, el diseño muestral es probabilístico y estratificado, con estratos definidos por las 16 regiones. Se aplica un factor de expansión y estimación, utilizado library(survey)  sugerido por el mismo documento:

```r
base_enadel_svy <- base_enadel |> 
  as_survey_design( 
    ids = varunit, # Unidad Primaria de Muestreo 
    strata = varstrat, # Pseudoestratos (327 niveles) 
    weights = pond_final # Factor de expansión calibrado 
  ) 
```

⚠️ En adelante considerar que las estimaciones estan sugeras a altos coeficientes de variación, por lo que las cifras son solo referenciales y con fines exploratorios. Pero la mayoría NO son utilizables para la adecuada toma de decisiones.

---

## Distribución de Vacantes Actuales por regiones, año 2024.

| Región | Tamaño muestral | N° vacantes actuales 2024 | Distribución % |  CV   |
|:------:|:---------------:|:--------------------:|:--------------:|:-----:|
|   13   |       468       |        58.254        |     48,72      | 1,03  |
|   7    |       504       |        38.692        |     32,36      | 0,49  |
|   8    |       530       |        22.463        |     18,79      | 0,59  |
|   5    |       550       |        20.224        |     16,92      | 0,86  |
|   9    |       405       |        8.717         |      7,29      | 0,33  |
|   4    |       295       |        8.021         |      6,71      | 0,45  |
|   16   |       388       |        7.333         |      6,13      | 0,29  |
|   10   |       482       |        6.336         |      5,30      | 0,24  |
|   12   |       299       |        3.998         |      3,34      | 0,23  |
|   3    |       298       |        3.528         |      2,95      | 0,28  |
|   14   |       346       |        1.819         |      1,52      | 0,33  |
|   1    |       218       |        1.499         |      1,25      | 0,66  |
|   15   |       236       |        1.099         |      0,92      | 0,28  |
|   11   |       135       |        1.081         |      0,90      | 0,39  |
|   2    |       339       |       -14.298        |     -11,96     | -0,32 |
|   6    |       472       |       -49.204        |     -41,15     | -0,29 |
| Total  |      5.965      |       119.562        |     100,00     | 0,57  |

---

## Distribución de Vacantes Actuales por regiones, año 2025.

| Región | Tamaño muestral | N° vacantes actuales 2025 | Distribución % |   CV   |
|:------:|:---------------:|:--------------------:|:--------------:|:------:|
|   13   |       313       |      58.184,76       |     333,88     |  0,46  |
|   7    |       459       |       9.519,27       |     54,62      |  0,31  |
|   9    |       282       |       4.390,11       |     25,19      |  0,28  |
|   4    |       386       |       4.308,66       |     24,72      |  0,30  |
|   16   |       292       |       3.436,26       |     19,72      |  0,40  |
|   1    |       307       |       3.369,54       |     19,34      |  0,49  |
|   14   |       312       |       2.832,30       |     16,25      |  0,54  |
|   10   |       416       |       2.265,63       |     13,00      |  1,01  |
|   15   |       227       |        707,57        |      4,06      |  0,20  |
|   11   |       184       |        446,14        |      2,56      |  0,76  |
|   12   |       283       |        195,90        |      1,12      |  2,74  |
|   6    |       472       |        11,05         |      0,06      | 270,66 |
|   8    |       424       |      -1.174,70       |     -6,74      | -2,98  |
|   3    |       285       |      -1.453,64       |     -8,34      | -0,72  |
|   5    |       542       |      -4.248,38       |     -24,38     | -1,60  |
|   2    |       392       |      -65.363,62      |    -375,07     | -0,13  |
| Total  |      5.576      |      17.426,83       |     100,00     |  1,69  |

---

##  A partir de las dos tablas anteriores, elaboré un consolidado que contiene las variaciones absolutas y relativas, participaciones e incidencias.


| Región | Vacantes 2024 | Vacantes 2025 |  Var.abs  | Var.rel | Participación | Incidencias |
|:------:|:-------------:|:-------------:|:---------:|:-------:|:-------------:|:-----------:|
|   13   |    58.254     |   58.184,8    |   -69,2   |  -0,1   |     333,9     |    -0,1     |
|   7    |    38.692     |    9.519,3    | -29.172,7 |  -75,4  |     54,6      |   -306,5    |
|   9    |     8.717     |    4.390,1    | -4.326,9  |  -49,6  |     25,2      |    -98,6    |
|   4    |     8.021     |    4.308,7    | -3.712,3  |  -46,3  |     24,7      |    -86,2    |
|   16   |     7.333     |    3.436,3    | -3.896,7  |  -53,1  |     19,7      |   -113,4    |
|   1    |     1.499     |    3.369,5    |  1.870,5  |  124,8  |     19,3      |    55,5     |
|   14   |     1.819     |    2.832,3    |  1.013,3  |  55,7   |     16,3      |    35,8     |
|   10   |     6.336     |    2.265,6    | -4.070,4  |  -64,2  |     13,0      |   -179,7    |
|   15   |     1.099     |     707,6     |  -391,4   |  -35,6  |      4,1      |    -55,3    |
|   11   |     1.081     |     446,1     |  -634,9   |  -58,7  |      2,6      |   -142,3    |
|   12   |     3.998     |     195,9     | -3.802,1  |  -95,1  |      1,1      |  -1.940,9   |
|   6    |    -49.204    |     11,0      | 49.215,0  | -100,0  |      0,1      |  445.428,2  |
|   8    |    22.463     |   -1.174,7    | -23.637,7 | -105,2  |     -6,7      |   2.012,2   |
|   3    |     3.528     |   -1.453,6    | -4.981,6  | -141,2  |     -8,3      |    342,7    |
|   5    |    20.224     |   -4.248,4    | -24.472,4 | -121,0  |     -24,4     |    576,0    |
|   2    |    -14.298    |   -65.363,6   | -51.065,6 |  357,2  |    -375,1     |    78,1     |

---

## Calidad vacantes actuales 2025,

El documento metodológico indica que es posible evaluar la **calidad** de los datos. En este caso evalué la variable **b11_1** Vacantes actuales abiertas a mayo de 2025 (total), usando la siguiente estructura:

```r
vacantes_total <- create_total(var = "b11_1", design = base_enadel_svy)
evaluacion_dotacion_total <- assess(vacantes_total)
```

---


|   stat    |    se     |  df   |   n   |  cv  |         eval_n         |    eval_df    | eval_cv  |    label     |
|:---------:|:---------:|:-----:|:-----:|:----:|:----------------------:|:-------------:|:--------:|:------------:|
| 17.426,83 | 29.514,87 | 5.260 | 5.576 | 1,69 | sufficient sample size | sufficient df | cv > 0.3 | non-reliable |

---

> R.version$version.string "R version 4.3.0 (2023-04-21 ucrt)"

