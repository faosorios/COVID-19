# Estimación de cambios de régimen para el número de casos de COVID-19 en Chile

[![R](https://img.shields.io/badge/Made%20with-R%20under%20development-success)](https://cran.r-project.org/)
[![segmented](https://img.shields.io/badge/segmented-1.1--0-red)](https://cran.r-project.org/package=segmented)
[![DOI](https://img.shields.io/badge/DOI-10.10.13140/RG.2.2.32798.28485-blue)](http://doi.org/10.13140/RG.2.2.32798.28485)

Código en R para determinar cambios de régimen en el número de casos de pacientes con
COVID-19 en Chile. Los datos son obtenidos desde la pagina web:
https://github.com/MinCiencia/Datos-COVID19/tree/master/output/producto3

Se utiliza un modelo de regresión con crecimiento exponencial para datos Poisson. La metodología
para la determinación de los puntos de cambio es discutida en: Muggeo, V.M.R. (2003). Estimating regression models with unknown break-points. *Statistics in Medicine* **22**, 3055-3071.

El procedimiento permite determinar los tiempos de duplicación (en días) del número de casos de COVID-19 en Chile
estimar las tasas de crecimiento y los cambios de régimen. Detalles de la implementación y código original
disponible en: [doi: 10.13140/RG.2.2.32798.28485](http://doi.org/10.13140/RG.2.2.32798.28485)

Para ejecutar interactivamente hacer click en el siguiente icono: [![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/aLoNsolml/COVID-19/master?urlpath=lab).

Paquetes R (base) usados: stats, graphics, grDevices, utils, datasets, methods, Autoloads, base

Otros paquetes (requerido): segmented_1.1-0

CONTENIDOS:
- segmeted19.R: funciones R para estimar los puntos de cambio en regresión Poisson con tendencia exponencial (código original por Muggeo, Sottile y Porcu, 2020).
- segmentedChile.R: comandos en R para obtener los resultados del Reporte *Estimación de puntos de cambio usando Regresión Segmentada para datos del brote de COVID-19 en Chile* (28 de Abril), disponible la página web del [Departamento de Matemática, UTFSM](http://matematica.usm.cl/covid-19-en-chile)
- chile_segmented_report.ipynb: jupiter notebook que permite **actualizar** los resultados del reporte del día 28 de Abril.
