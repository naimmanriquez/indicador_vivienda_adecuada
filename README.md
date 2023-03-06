![N|Solid](https://fiuat.mx/images/uat.png)

# Proyecto de investigación sobre vivienda adecuada - Programa Nacional Estrategico de Vivienda/CONACYT
## Introducción
Repositorio de datos y códigos para generar indicador de vivienda adecuada por entidad federativa y regiones. Para este análisis se toma de referencia la Encuesta Nacional de Vivienda del Instituto Nacional de Estadística y Geografía - INEGI. En el caso del concepto de vivienda adecuada se toman los elementos de referencia que propone ONU-Hábitat para caracterizar lo que debe tener una vivienda para considerarla adecuada: seguridad en la tenencia, disponibilidad de los servicios, asequibilidad, accesibilidad, habitabilidad, adecuación cultural y ubicación. 

#### Seguridad en la tenencia
Garantizar protección jurídica de los ocupantes.
#### Disponibilidad de los servicios
Conexión y dotación de servicios básicos como agua, luz y drenaje
#### Asequibilidad
Que las personas puedan acceder a una vivienda sin sacrificar otras necesidades básicas.
#### Accesibilidad
Que la vivienda pueda atender las necesidades especiales de los ocupantes.
#### Habitabilidad
Garantizar la seguridad física de los habitantes al protegerlos del frío, la humedad, el calor, el viento, las lluvias, los riesgos climáticos y los peligros estructurales, así como garantizar que la vivienda sea un espacio habitable suficiente.
#### Adecuación cultural
Que las viviendas y su entorno respeten la identidad cultural de sus ocupantes
#### Ubicación
Garantizar el acceso a empleo, servicios de salud, escuelas, guarderías, servicios e instalaciones sociales, fuera de zonas de riesgo o contaminadas

## Metodología
Para calcular el índice de vivienda adecuada se tomó de referencia la Encuesta Nacional de Vivienda, dicha encuesta contiene información capaz de cubrir los siete elementos que define ONU-Hábitat como vivienda adecuada.

1.	Se definió un número total de puntos conforme a los elementos que comprenden a la vivienda adecuada. Con estos puntajes, se obtuvo la suma de puntos para todos los hogares dentro de la muestra de la Encuesta Nacional de Vivienda, 2020.
2.	Posteriormente, se utilizó el procedimiento de estratificación univariado de Dalenius-Hodges, (Cumulative root frequency method) para obtener los puntos de corte que minimizan la variabilidad intra-grupos.

## Análisis por entidad federativa y por región.
Para el análisis descriptivo se toma de referencia a nivel regional
Las regiones son las siguientes:
#### *	Norte: 
Baja California, Sonora, Chihuahua, Coahuila, Nuevo León y Tamaulipas.
#### *	Norte-occidente: 
Baja California Sur, Sinaloa, Nayarit, Durango y Zacatecas.
#### *	Centro-norte: 
Jalisco, Aguascalientes, Colima, Michoacán y San Luis Potosí.
#### *	Centro: 
Guanajuato, Querétaro, Hidalgo, Estado de México, Ciudad de México, Morelos, Tlaxcala y Puebla.
