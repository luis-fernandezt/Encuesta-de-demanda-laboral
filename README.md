# Análisis de la Encuesta de Demanda Laboral (ENADEL 2024)

La **ENADEL 2024** tiene como base la caracterización de las necesidades de capital humano de las empresas y las brechas existentes en el mercado laboral.

Este es un **breve análisis exploratorio**, estimando el **número de empresas**, **número de trabajadores** y **número de vacantes actuales**, por **regiones** y **actividad económica**.  
Se analiza en detalle el comportamiento en las regiones de **Los Ríos** y **Los Lagos**.

---

## Requisitos previos y preparación de la base de datos

1. **Base de datos**  
   Se trabajó con la base de datos **ENADEL 2024**, disponible en:  
   [ENADEL 2024 - CSV](https://www.subtrab.gob.cl/wp-content/uploads/2025/05/ENADEL_2024_externa.csv)

2. **Librerías necesarias**

```r
library(readr)
library(tidyverse)
library(survey)
library(srvyr)
library(knitr)
```

3. **Aplicar Factor de expansion**
Según el documento metodológico de ENADEL 2024, el diseño muestral es probabilístico y estratificado, con estratos definidos por las 16 regiones y 12 agrupaciones de ramas de actividad económica, pero no se menciona un nivel de conglomerado específico (clusters). Por tanto, no hay una variable conglomerado oficial en los datos.

Se aplica factor de expansión y estimación, utilizado library(survey) y library(srvyr):

```r
ENADEL_2024 %>% 
  as_survey_design(
    ids = ~1,
    weights = fact_emp,
    strata = estrato
  )
```

⚠️ En adelante considerar que las estimaciones estan sugeras a altos coeficientes de variación, por lo que las cifras son solo referenciales.

---

## Distribución de Empresas por regiones

| Región | Tamaño muestral | N° empresas | Distribución % |  CV  |
|:------:|:---------------:|:-----------:|:--------------:|:----:|
|   13   |       468       |   46.037    |     50,11      | 0,01 |
|   5    |       550       |    8.421    |      9,17      | 0,06 |
|   8    |       530       |    6.334    |      6,89      | 0,03 |
|   7    |       504       |    5.057    |      5,50      | 0,04 |
|   6    |       472       |    4.702    |      5,12      | 0,02 |
|   10   |       482       |    4.238    |      4,61      | 0,01 |
|   9    |       405       |    3.328    |      3,62      | 0,01 |
|   4    |       295       |    2.866    |      3,12      | 0,03 |
|   2    |       339       |    2.765    |      3,01      | 0,05 |
|   16   |       388       |    1.819    |      1,98      | 0,07 |
|   1    |       218       |    1.679    |      1,83      | 0,02 |
|   14   |       346       |    1.654    |      1,80      | 0,11 |
|   12   |       299       |     987     |      1,07      | 0,00 |
|   3    |       298       |     875     |      0,95      | 0,03 |
|   15   |       236       |     606     |      0,66      | 0,02 |
|   11   |       135       |     505     |      0,55      | 0,08 |
| Total  |      5.965      |   91.873    |     100,00     | 0,08 |

---

## Distribución de Trabajadores por regiones

| Región | Tamaño muestral | N° trabajadores | Distribución % |  CV  |
|:------:|:---------------:|:---------------:|:--------------:|:----:|
|   13   |       468       |    2.983.952    |     58,20      | 0,12 |
|   5    |       550       |     365.547     |      7,13      | 0,14 |
|   8    |       530       |     283.912     |      5,54      | 0,09 |
|   2    |       339       |     272.138     |      5,31      | 0,62 |
|   6    |       472       |     239.185     |      4,67      | 0,12 |
|   7    |       504       |     201.082     |      3,92      | 0,15 |
|   10   |       482       |     174.390     |      3,40      | 0,09 |
|   4    |       295       |     144.349     |      2,82      | 0,34 |
|   9    |       405       |     135.225     |      2,64      | 0,09 |
|   14   |       346       |     97.819      |      1,91      | 0,42 |
|   16   |       388       |     65.345      |      1,27      | 0,11 |
|   1    |       218       |     59.815      |      1,17      | 0,18 |
|   3    |       298       |     38.820      |      0,76      | 0,10 |
|   12   |       299       |     34.252      |      0,67      | 0,11 |
|   15   |       236       |     16.377      |      0,32      | 0,09 |
|   11   |       135       |     14.748      |      0,29      | 0,19 |
| Total  |      5.965      |    5.126.956    |     100,00     | 0,00 |

---

## Distribución de Vacantes Actuales por regiones

| Región | Tamaño muestral | N° vacantes actuales | Distribución % |  CV   |
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

## Estimaciones a nivel regional.

## Estimación de Empresas según Actividad Económica CIIU - Región de Los Ríos

|             CIIU Seccion             | Tamaño muestral | N° empresas | Distribución % |  CV  |
|:------------------------------------:|:---------------:|:-----------:|:--------------:|:----:|
|      Industrias manufactureras       |       63        |     420     |     25,39      | 0,43 |
|             Agricultura              |       35        |     289     |     17,47      | 0,16 |
|               Comercio               |       70        |     223     |     13,48      | 0,08 |
|             Construcción             |       26        |     213     |     12,88      | 0,11 |
| Alojamiento y de servicio de comidas |       59        |     182     |     11,00      | 0,06 |
|     Transporte y almacenamiento      |       39        |     123     |      7,44      | 0,08 |
|      Actividades profesionales       |       16        |     71      |      4,29      | 0,26 |
| Servicios administrativos y de apoyo |       19        |     70      |      4,23      | 0,21 |
|        Actividades artísticas        |        7        |     22      |      1,33      | 0,30 |
|      Actividades inmobiliarias       |        2        |     11      |      0,67      | 0,73 |
|      Suministro de electricidad      |        3        |      9      |      0,54      | 0,45 |
|       Actividades financieras        |        1        |      4      |      0,24      | 1,00 |
|       Actividades de la salud        |        1        |      3      |      0,18      | 1,00 |
|        Administración pública        |        1        |      3      |      0,18      | 1,00 |
|   Explotación de minas y canteras    |        1        |      3      |      0,18      | 1,00 |
|    Otras actividades de servicios    |        1        |      3      |      0,18      | 1,00 |
|          Suministro de agua          |        1        |      3      |      0,18      | 1,00 |
|     Información y comunicaciones     |        1        |      2      |      0,12      | 1,00 |
|                Total                 |       346       |    1.654    |     100,00     | 0,11 |

---

## Estimación de Trabajadores según Actividad Económica CIIU - Región de Los Ríos

|             CIIU Seccion             | Tamaño muestral | N° trabajadores | Distribución % |  CV  |
|:------------------------------------:|:---------------:|:---------------:|:--------------:|:----:|
|      Industrias manufactureras       |       63        |     46.685      |     47,73      | 0,82 |
|             Agricultura              |       35        |     23.856      |     24,39      | 0,59 |
|             Construcción             |       26        |      9.463      |      9,67      | 0,31 |
|               Comercio               |       70        |      4.309      |      4,41      | 0,15 |
| Alojamiento y de servicio de comidas |       59        |      4.273      |      4,37      | 0,15 |
|     Transporte y almacenamiento      |       39        |      3.249      |      3,32      | 0,26 |
| Servicios administrativos y de apoyo |       19        |      3.071      |      3,14      | 0,29 |
|      Actividades profesionales       |       16        |      1.278      |      1,31      | 0,28 |
|        Actividades artísticas        |        7        |       511       |      0,52      | 0,36 |
|      Suministro de electricidad      |        3        |       456       |      0,47      | 0,65 |
|      Actividades inmobiliarias       |        2        |       199       |      0,20      | 0,78 |
|       Actividades financieras        |        1        |       128       |      0,13      | 1,00 |
|     Información y comunicaciones     |        1        |       110       |      0,11      | 1,00 |
|       Actividades de la salud        |        1        |       96        |      0,10      | 1,00 |
|    Otras actividades de servicios    |        1        |       60        |      0,06      | 1,00 |
|   Explotación de minas y canteras    |        1        |       30        |      0,03      | 1,00 |
|          Suministro de agua          |        1        |       27        |      0,03      | 1,00 |
|        Administración pública        |        1        |       18        |      0,02      | 1,00 |
|                Total                 |       346       |     97.819      |     100,00     | 0,42 |

---

## Estimación de Vacantes según Actividad Económica CIIU - Región de Los Ríos

|             CIIU Seccion             | Tamaño muestral | Estimación | Distribución % |  CV  |
|:------------------------------------:|:---------------:|:----------:|:--------------:|:----:|
| Servicios administrativos y de apoyo |       19        |    453     |     24,90      | 0,98 |
|             Agricultura              |       35        |    452     |     24,85      | 0,67 |
|             Construcción             |       26        |    266     |     14,62      | 0,67 |
|     Transporte y almacenamiento      |       39        |    217     |     11,93      | 0,69 |
| Alojamiento y de servicio de comidas |       59        |    142     |      7,81      | 0,44 |
|               Comercio               |       70        |    105     |      5,77      | 0,36 |
|        Actividades artísticas        |        7        |     83     |      4,56      | 0,96 |
|      Industrias manufactureras       |       63        |     64     |      3,52      | 0,39 |
|      Suministro de electricidad      |        3        |     15     |      0,82      | 1,00 |
|      Actividades profesionales       |       16        |     8      |      0,44      | 0,70 |
|       Actividades de la salud        |        1        |     6      |      0,33      | 1,00 |
|        Administración pública        |        1        |     6      |      0,33      | 1,00 |
|     Información y comunicaciones     |        1        |     2      |      0,11      | 1,00 |
|       Actividades financieras        |        1        |     0      |      0,00      | NaN  |
|      Actividades inmobiliarias       |        2        |     0      |      0,00      | NaN  |
|   Explotación de minas y canteras    |        1        |     0      |      0,00      | NaN  |
|    Otras actividades de servicios    |        1        |     0      |      0,00      | NaN  |
|          Suministro de agua          |        1        |     0      |      0,00      | NaN  |
|                Total                 |       346       |   1.819    |     100,00     | 0,33 |

---

## Estimación de Empresas según Actividad Económica CIIU - Región de Los Lagos

|             CIIU Seccion             | Tamaño muestral | N° empresas | Distribución % |  CV  |
|:------------------------------------:|:---------------:|:-----------:|:--------------:|:----:|
|      Industrias manufactureras       |       73        |     722     |     17,04      | 0,10 |
|             Agricultura              |       51        |     684     |     16,14      | 0,10 |
|               Comercio               |       60        |     586     |     13,83      | 0,09 |
|             Construcción             |       34        |     549     |     12,95      | 0,13 |
|     Transporte y almacenamiento      |       55        |     459     |     10,83      | 0,10 |
| Servicios administrativos y de apoyo |       36        |     443     |     10,45      | 0,17 |
| Alojamiento y de servicio de comidas |       60        |     377     |      8,90      | 0,06 |
|      Actividades profesionales       |       20        |     137     |      3,23      | 0,31 |
|     Información y comunicaciones     |       16        |     75      |      1,77      | 0,39 |
|          Suministro de agua          |       15        |     50      |      1,18      | 0,21 |
|      Actividades inmobiliarias       |       15        |     39      |      0,92      | 0,26 |
|    Otras actividades de servicios    |       18        |     36      |      0,85      | 0,20 |
|       Actividades de la salud        |        8        |     29      |      0,68      | 0,46 |
|              Enseñanza               |        5        |     19      |      0,45      | 0,62 |
|        Actividades artísticas        |        9        |     18      |      0,42      | 0,31 |
|       Actividades financieras        |        5        |     10      |      0,24      | 0,37 |
|      Suministro de electricidad      |        1        |      3      |      0,07      | 1,00 |
|        Administración pública        |        1        |      2      |      0,05      | 1,00 |
|                Total                 |       482       |    4.238    |     100,00     | 0,01 |

---

## Estimación de Trabajadores según Actividad Económica CIIU - Región de Los Ríos

|             CIIU Seccion             | Tamaño muestral | N° trabajadores | Distribución % |  CV  |
|:------------------------------------:|:---------------:|:---------------:|:--------------:|:----:|
|             Agricultura              |       51        |     33.779      |     19,37      | 0,18 |
|               Comercio               |       60        |     30.488      |     17,48      | 0,33 |
|      Industrias manufactureras       |       73        |     26.216      |     15,03      | 0,15 |
| Servicios administrativos y de apoyo |       36        |     25.997      |     14,91      | 0,35 |
|             Construcción             |       34        |     23.096      |     13,24      | 0,25 |
|     Transporte y almacenamiento      |       55        |     14.506      |      8,32      | 0,20 |
| Alojamiento y de servicio de comidas |       60        |      8.867      |      5,08      | 0,16 |
|          Suministro de agua          |       15        |      3.336      |      1,91      | 0,76 |
|     Información y comunicaciones     |       16        |      2.232      |      1,28      | 0,55 |
|      Actividades profesionales       |       20        |      2.201      |      1,26      | 0,36 |
|       Actividades de la salud        |        8        |      1.011      |      0,58      | 0,50 |
|    Otras actividades de servicios    |       18        |       938       |      0,54      | 0,28 |
|        Actividades artísticas        |        9        |       548       |      0,31      | 0,50 |
|              Enseñanza               |        5        |       433       |      0,25      | 0,66 |
|      Actividades inmobiliarias       |       15        |       412       |      0,24      | 0,25 |
|       Actividades financieras        |        5        |       148       |      0,08      | 0,43 |
|      Suministro de electricidad      |        1        |       132       |      0,08      | 1,00 |
|        Administración pública        |        1        |       50        |      0,03      | 1,00 |
|                Total                 |       482       |     174.390     |     100,00     | 0,09 |

---

## Estimación de Vacantes según Actividad Económica CIIU - Región de Los Ríos

|             CIIU Seccion             | Tamaño muestral | Estimación | Distribución % |  CV  |
|:------------------------------------:|:---------------:|:----------:|:--------------:|:----:|
|      Industrias manufactureras       |       73        |   1.692    |     26,70      | 0,43 |
|               Comercio               |       60        |   1.478    |     23,33      | 0,52 |
| Servicios administrativos y de apoyo |       36        |    874     |     13,79      | 0,39 |
|             Construcción             |       34        |    855     |     13,49      | 0,53 |
|             Agricultura              |       51        |    505     |      7,97      | 0,34 |
|     Transporte y almacenamiento      |       55        |    303     |      4,78      | 3,11 |
| Alojamiento y de servicio de comidas |       60        |    233     |      3,68      | 0,28 |
|     Información y comunicaciones     |       16        |    153     |      2,41      | 0,55 |
|      Actividades profesionales       |       20        |     91     |      1,44      | 0,69 |
|          Suministro de agua          |       15        |     78     |      1,23      | 0,64 |
|        Actividades artísticas        |        9        |     44     |      0,69      | 0,78 |
|      Suministro de electricidad      |        1        |     12     |      0,19      | 1,00 |
|      Actividades inmobiliarias       |       15        |     10     |      0,16      | 0,66 |
|       Actividades de la salud        |        8        |     4      |      0,06      | 1,00 |
|              Enseñanza               |        5        |     2      |      0,03      | 1,00 |
|    Otras actividades de servicios    |       18        |     2      |      0,03      | 1,00 |
|       Actividades financieras        |        5        |     0      |      0,00      | NaN  |
|        Administración pública        |        1        |     0      |      0,00      | NaN  |
|                Total                 |       482       |   6.336    |     100,00     | 0,24 |

---

> R.version$version.string "R version 4.3.0 (2023-04-21 ucrt)"
