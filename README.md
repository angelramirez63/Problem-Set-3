# Problem-set-3

#### Juan Esteban Díaz Torres - 202020319  
#### Natalia Plata Ángel - 201730699  
#### Ángel David Ramírez Torres - 202112704  
#### Juan Pablo Grimaldos - 202122627

## Tabla de Contenidos
- [Documentación](#Documentación)
- [Scripts](#Scripts)
- [Stores](#Stores)
- [Views](#Views)

## Documentación
Esta carpeta contiene el archivo final con la solución del taller en formato pdf y una carpeta comprimida con el archivo .tex.

## Scripts
La carpeta *Scripts* contiene: (i) los scripts utilizados para limpiar y preprocesar la base y para agregar las variables espaciales y las provenientes del procesamiento del lenguaje natural de los títulos y la descripción de las propiedades, (ii) los scripts utilizados para generar los mapas, figuras y las tablas de estadísticas descriptivas, y (iii) los scripts utilizados para implementar los algoritmos utilizados para predecir el precio de los inmuebles. A continuación, se detalla la estructura y contenido:

- **Script para consolidar la base y agregar variables:** Los archivos *1_Union_datos_geograficos.R*, *2_Union_datos_geograficos_p2.R*, *3_Variables Texto.R* y *4_Variables_texto_final.R* fueron utilizados para agregar las variables espaciales y las provenientes del procesamiento del lenguaje natural de los títulos y las descripciones de las propiedades y para consolidar la base que usamos para entrenar los modelos.  
  
  > Las fuentes de los atributos espaciales son Datos Abiertos Bogotá y OpenStreetMap

- **Script de estadísticas descriptivas:** Los archivos *5_Figuras.R* y *6_Tabla_estadisticas_descriptivas.R* fueron utilizados para realizar los mapas de la distribución de las propiedades según precio por metro cuadrado y estrato y la tabla de estadísticas descriptivas que compara para las bases de entrenamiento y testeo algunas de las características más importantes de las propiedades. 

- **Implementación de los algoritmos:** en los archivos *7_Elastic Net y Random Forest.R*, 8_XGBoost.R*, *9_Neural_Network.R*, *10_Neural_Network_v2.R* y *11_SuperLearner.R* se realiza la implementación de los algoritmos que están en los nombres de los archivos para predecir precios de propiedades en la ciudad de Bogotá con los requerimientos específicos de cada uno de los algoritmos.

## Stores
Esta carpeta contiene las bases de datos provistas como insumos para realizar el taller (*train.csv, test.csv y sample_submission.csv*), las bases de datos de los atributos espaciales obtenidas de Datos Abiertos Bogotá para crear las variables espaciales, y las bases de datos consolidadas que se usaron para el entrenamiento de los modelos.  

> La base de arbolado_urbano.zip está comprimida porque por su tamaño no podía ser subida de otra forma

- **Predicciones:** Se incluye la carpeta *submits* con las predicciones enviadas a Kaggle por los miembros del equipo. Estas predicciones están en formato .csv.

## Views
Esta carpeta contiene las gráficas, mapas y tablas generadas en la solución del taller y que se encuentran en el documento final. Los mapas y las gráficas están en formato PNG y la tabla en formato txt.

### Mapas y gráficas:

- **mapa1.png**, **mapa2.png** y **mapa3.png**: Mapas que muestran la distribución de las propiedades en Bogotá por precio del metro cuadrado y estrato.

- **barras1.png**: Precio por metro según los percentiles de distancia al CBD. 

- **bloquescvespacial.png:** Gráfica que ejemplifica la validación cruzada espacial realizada para el XGBoost.

### Tablas (formato TXT):

- **tabla_estadisticas_descriptivas.txt**: Tabla de estadísticas descriptivas que compara características entre las propiedades en la base de entrenamiento y testeo.  

## Instrucciones para replicar el trabajo

1. Correr los scripts *1_Union_datos_geograficos.R*, *4_Variables_texto_final.R* y *2_Union_datos_geograficos_p2.R* en ese orden para obtener la base consolidada usada para entrenar los modelos.  
2. Correr los scripts *5_Figuras.R* y *6_Tabla_estadisticas_descriptivas.R* para generar los mapas, figuras y la tabla de estadísticas descriptivas.  
3. Correr los scripts de los modelos para entrenarlos:  
   - *7_Elastic Net y Random Forest.R*
   - *8_XGBoost.R*  
   - *9_Neural_Network.R* y *10_Neural_Network_v2.R*  
   - *11_SuperLearner.R*  
