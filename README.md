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
Esta carpeta contiene el archivo final con la solución del taller en formato pdf y una carpeta comprimida con el archivo .tex 

## Scripts
La carpeta *Scripts* contiene el script utilizado para limpiar y procesar las bases de datos, el script utilizado para realizar la estadísticas descriptivas y cuatro script con la implentación de los algoritmos utilizados para predecir la pobreza de los hogares. A continuación, se detalla la estructura y contenido:

- **Script de limpieza:** El archivo *1_Limpieza_Consolidado_v2.R* toma las bases de datos provistas como inputs para realizar el taller y realiza la imputacion de valores faltantes, tratamiento de valores extremos, limpieza de los datos y la consolidacion de las bases de entrenamiento y de testeo a nivel de hogar. 


- **Script de estadísticas descriptivas:** En el archivo *2_Estadísticas_Descriptivas.R* se realizan las tablas con las estadísticas descriptivas para las personas clasificadas como *Pobres* y *No Pobres* con el comparar las diferencias entree ambos grupos. 

- **Implementación de los algoritmos:** Cinco archivos *Elastic Net y Logit.R*, *CART.R*, *Random_Forest.R* y *Boosting.R* donde se realiza la implementación y la ejecución de la prediccion de la pobreza de los hogares por medio de Elastic Net y Regresion Logística (Logit), árbol de prediccióm, Random Forest y Adaptative Boosting (Adaboost) respectivamente. 

- **Carpeta de scripts iniciales:** Finalmente, se incluye la carpeta *Deprecated* con los scripts que cada miembro del equipo utilizó para comenzar a desarrollar el taller y posteriormente fueron remplazados por scripts consolidados después de un proceso de ajuste. 

## Stores
Esta carpeta contiene las bases de datos provistas como insumos para realizar el taller (*train_personas.rds, train_hogares.csv, test_personas.csv, test_hogares.csv y sample_submission.csv*), la base datos *db_final.rds* consolidada usando el script *1_Limpieza_Consolidado_v2.R* y la carpeta *Predicciones*. Todos los archivos están almacenados en formato csv salvo *train_personas.rds* y *db_final.rds*. Se decidió cambiar el formato de estos archivos para poderlos subirlos a github y facilitar el trabajo. 

- **Predicciones:** Se incluye una carpeta con las predicciones enviadas a Kaggle por los miembros del equipo. Estas predicciones están en formato .csv 

## Views
Esta carpeta contiene las gráficas y tablas generadas en la solución del taller y que se encuentran en el documento final. Las gráficas están en formato PNG y las tablas en formato tet.

### Gráficas (formato PNG):

- **desbalance_de_clases.png:** Gráfica que muestra la proporcion de pobres (Sí) y no pobres (No) en la base de datos de entrenmaiento para la variables Pobre. Esta gráfica se realizo para caracterizar el desbalance de clases en está variable. 


### Tablas (formato TXT):

- **Métricas de desempeño de los modelos** En la carpeta están presentes los resultados de varias métricas de desempeño obtenidos por las combinaciones de hiperpárametros consideradas en la implementación del los algoritmos *ElasticNet* , *Random Forest* y *Adaboost* las demas tablas con están presentes en el archivo .tex del documento

- **tabla_estadisticas_descriptivas.tex:** Tabla realizada usando script "2_Estadísticas_Descriptivas.R"

## Instrucciones para replicar el trabajo

1. Correr el script de limpieza *1_Limpieza_Consolidado_v2.R*
2. Correr el script de estadísticas descriptivas *2_Estadísticas_Descriptivas.R*
3. Correr los scripts de los modelos: 
  - *3_Elastic Net y Logit.R*
  - *4_Boosting.R*
  - *5_Random_Forest.R*
