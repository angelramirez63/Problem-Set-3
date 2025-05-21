#========================== Redes neuronales (MLNN) ============================

#Alistar ambiente de trabajo ---------------------------------------------------

#Limpiar ambiente de trabajo 
cat("\014")
rm(list = ls())

#Establecer que R use el ambiente con python 3.10 para poder usar Keras
#   Este el ambiente creado en mi computador con que contiene python 3.10 y tensoflow
#   2.16 para poder usar Keras. Es importante que creen un ambiente donde funcionen 
#   Todas las dependencias de Keras para que funcione el paquete 
library(reticulate)
use_condaenv("r-tensorflow", required = TRUE) 
library(keras)

#Cargar paquetes
library("pacman")
p_load(tidyverse, #Manejo de datos
       rio, #Guardar archivo
       here, #Definir directorio cuando hay un .Rproj
       stargazer, #Presentación resultados
       caret,  #For model training and tunning
       Metrics, #Evaluation Metrics for ML
       skimr, #Análisis inicial de la base
       sf #Manejo de datos espaciales
)

#Definir directorio
wd <- here()
wd_stores <- paste0(wd, "/stores/")
rm(wd)
setwd(wd_stores)
rm(wd_stores)

#Cargar datos
db <- readRDS("datos_modelos.rds" )

#Renombrar algunas variables para poder graficarlas 
db <- db %>%
         rename(
           has_terraza = `has_terraza(s)?`, 
           has_ascensor = `has_ascensor(es)?`, 
           has_patio =`has_patio(s)?`,
           has_zonas_verdes = `has_zona(s)?_verde(es)?`, 
           has_chimenea = `has_chimenea(s)`
           
         )


#Data pre processing -----------------------------------------------------------


##Funciones para gráficar -----------------#####
PlotDensity <- function(var_name, db_name) {
  
  db <- get(db_name)
  db %>%
    ggplot(aes_string(x = var_name)) +
    geom_density(fill = "lightblue", color = "#e9ecef", alpha = 0.8) +
    ggtitle(paste0("Distribución ", var_name)) + 
    theme_minimal()
}


PlotBars <- function(var_name, db_name) {
  
  db <- get(db_name)
  db %>% 
    ggplot(aes_string( x = var_name)) +
    geom_bar() + 
    ggtitle(paste0("Gráfico de barras", " " ,var_name)) + 
    theme_minimal()
}


#Lista variable estructurales 
vars_esctructura <- c(
  "price", "bedrooms", "property_type", "cocina_americana",
  "cocina_integral", "parqueadero_visitantes", "lavanderia", "gimnasio",
  "balcon", "seguridad", "walking_closet", "has_bbq",
  "has_terraza", "has_deposito","has_conjunto",
  "has_ascensor", "has_patio", "has_duplex", "has_piscina",
  "has_sauna", "has_jacuzzi", "has_altillo", "has_zonas_verdes",
  "n_banos", "area", "ESTRATO", "n_parqueaderos", "grupo_area"
)


##Graficos variables estructurales ----------------------#####

#Graficos de barras variables estructurales 
for (variable in vars_esctructura){
  
 print(PlotBars(variable, "db"))
  
}

#Graficos de densidad variables estructurales 
for (variable in vars_esctructura){
  
  print(PlotDensity(variable, "db"))
  
}

##Remover variables con near zero var-------------------------####
#Removemos estabas variables porque no aportan mucha información

#Remover variables sin ninguna varianza (todos los valores con 0 en este caso)
db <- db %>% select_if(~ !all(is.na(.)) & length(unique(.))>1) 

#Remover variables con varianza casi cero 
db <- db %>% st_drop_geometry()
zero_var_check <- nearZeroVar(db, saveMetrics = T, names = T)
zero_var_check <- zero_var_check %>% 
                  filter(nzv == TRUE)

db <- db %>% 
  select(-has_chimenea, -has_sauna, -has_jacuzzi, -has_altillo, -has_zonas_verdes)
rm(zero_var_check)

#Se removieron 15 variables con near zero var

#Remover variable de texto sin procesar(ya fueron usadas para crear otras variables)
db <- db %>% 
      select(-text, -title, -description)

#Remover variables que tienen otras variables con la misma información (nos qudamos con las upz para reducir el costo computacional)
db <- db %>% 
      select(-NOMBRE_UPZ, -SCANOMBRE, -CODIGO_MAN, -localidad)

##Resumir variables de crimen ---------------------------------####
#Esto es para reducir la dimensionalidad del dataframe

db <- db %>%  
      mutate(n_hurtos_vehiculos = n_hurtosautos + n_hurtosbicis + n_hurtosmotos, 
             n_delitos_fisicos = n_homicidios + n_lesiones + n_delitossexales + n_delitossexales + n_violenciaintra, 
             n_hurtos_propiedades = n_hurtosresidencias + n_hurtoscomercio, 
             n_hurto_personas = n_hurtopersonas + n_hurtoscelular) %>% 
      select(-n_hurtosautos, -n_hurtosbicis, -n_hurtosmotos, -n_homicidios,
             -n_lesiones, -n_delitossexales, -n_delitossexales, -n_violenciaintra, 
             -n_hurtopersonas, -n_hurtoscelular, -n_hurtosresidencias, -n_hurtoscomercio)

#Después de este proceso removí 25 variables 

##Tratar outliers ----------------------------------------------####

###Tratar valores atípicos variable de área------------------####

#Cargar base train original para comparar con nuestra base 
train_original <- read.csv("train.csv") 
aptos_gigantes_db_original <- train_original %>% filter(surface_total > 1000) #En la original solo hay 10 apartamentos gigantes. Se imputaron areas muy grandes para mas de 2000 aptos
id_aptos_gigantes_originales <- aptos_gigantes_db_original$property_id
rm(train_original, aptos_gigantes_db_original)


#Remover de la base de entrenamiento los 10 apartamentos gigantes originales (para que no alteren el promedio de la imputacion)
`%notin%` <- Negate(`%in%`) #Negación operador in 
db <- db %>% 
      filter(property_id %notin% id_aptos_gigantes_originales)


#Imputar de forma diferente el área de los aptos para evitar aptos gigantes 
db <- db %>% 
           group_by(bedrooms, 
                    n_banos, 
                    ESTRATO) %>% 
          mutate(area = ifelse(area > 1000, mean(area), area)) %>% 
          ungroup()


apartamentos_gigantes <- db %>% 
  filter(area >1000) #Después de la imputación quedaron 140
rm(apartamentos_gigantes)


##Remover algunas variables que no vamos a usar ----------------####

#Quitar variables espaciales (porque ya las utilizamos)
db <- db %>% 
      select(-lat, -lon)

#Guardar los índices para hacer las predicciones y juntas las bases más adelante
property_id <- db %>% select(property_id)
property_id_test <- db %>% 
                    filter(train == 0) %>% 
                    select(property_id)

prices <- db %>% select(property_id, price)
db <- db %>% 
      select(-property_id) #Quitamos property id para hacer el one hot encoding 
      

##One hot encoding(volver factor variables dummys) ---------------####

#Volver factor estrato factor y train antes del one hot encoding 
db <- db %>% 
         mutate(ESTRATO = as.factor(ESTRATO), 
                train = as.factor(train), 
                ) 
#Esta variable no fue tan imporante en XGboost y si esta generando problemas
db <- db %>% 
        select(-grupo_econom_manz)

db <- db %>% 
      mutate(n_localidad = as.integer(n_localidad), 
             cbd_distancia = as.double(cbd_distancia))

#Guardar nombres de las variables númericas
factor_columns <- db[sapply(db, is.factor)]
factor_columns <- colnames(factor_columns)
print(factor_columns)

categoricas_db <- db %>% 
                  select(all_of(factor_columns))

#One hot encoding para las variables categorícas 
dmy <- caret::dummyVars(
  ~ .,
  data =categoricas_db,
  sep = "_", # Separador para las variables dummy
  drop = TRUE,
  fullRank = TRUE # Evitar la multicolinealidad
)
categoricas_db <- as.data.frame(predict(dmy, newdata = categoricas_db)) #Hay una dummy por upz
rm(dmy)

categoricas_db[, "property_id"] <- property_id



#Normalizar variables númericas 
numericas_db <- db %>%
                select(price,!all_of(factor_columns))

numericas_db <- scale(numericas_db) %>% 
                as.data.frame()

numericas_db[, "property_id"] <- property_id
numericas_db[, "price"] <- prices$price


#Volver a unir las bases 
db_clean <- left_join(numericas_db, categoricas_db, by = "property_id")
rm(categoricas_db, numericas_db)

db_clean <- db_clean %>% 
            select(-property_id)
rm(prices)


##Verificar que las variables esten en el formato correcto---------####

#Para que keras y sus depedencias funcionen las variables: 
#(i) No deben tener missing values
#(ii) Las variables deden ser númericas. Las variables categorícas se deben convertir a dummys
#(iii) Las variables deben estar en escalas con valores que no sean muy extremos


#Verificar si hay missing values 
any(is.na(db_clean)) #Hay missing values en los precios de las observaciones de test
#Verificar si hay variables no númericas 
str(db_clean) #Todas las variables on númericas
#Las variables ya fueron normalizadas

#=============================Entrenar la red===================================


#Dividir los datos en entrenamiento y testeo------------------------------------

#Entrenamiento  
test_db <- db_clean %>% 
            filter(train_1 == 0) %>% 
            select(-train_1)

#Testeo
train_db <- db_clean %>% 
            filter(train_1 == 1) %>% 
            select(-train_1)


#Volver los datos una matriz para que Keras los pueda usar

#Entrenamiento 
X_train <- train_db %>% 
            select(-price, -ln_price)
X_train <- as.matrix(X_train)
class(X_train)

y_train <- train_db %>% 
            select(price)

y_train <- as.matrix(y_train)
class(y_train)


#Testeo
X_test <- test_db %>% 
          select(-price, -ln_price)
X_test <- as.matrix(X_test)
class(X_test)

#Rescalar variable de respuesta ¡¡¡¡¡¡¡¡¿Por qué esto mejora tanto el modelo?!!!!!!!!
y_train <- y_train / 1e6 #1e6 es igual a un millón


#Especificar la arquitectura de la red -----------------------------------------

set.seed("123") #Para que se pueden replicar los resultados

#Hiperparámetros: 
#(i)Número de nodos por capa oculta 
#(ii) Función de activación 
#(iii) Función de la capa de salida
#(iv) Número de capas ocultas 
#(v) Método para optimizar la función de perdida (Es gradiente descent o una variación)


model <- keras_model_sequential()

model %>% 
  layer_dense(units = 10, activation = "relu", input_shape = c(136)) %>% 
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 1)
summary(model)

#Compilar el modelo ------------------------------------------------------------

model %>% compile(loss = "mse",
                  optimizer = optimizer_adam(), #Método para minizar la función de pérdido - Stocastic Gradient Descent
                  metrics = list("mean_absolute_error") 
)


#Entrenar el modelo-------------------------------------------------------------

history_original <- model %>% fit(
  X_train, y_train, 
  epochs = 20, 
  batch_size = 40 , 
  validation_split = 0.3
)

history_original 

#Predicción a Kaggle -----------------------------------------------------------

sample_submission <- read.csv("submission_template.csv")

#Generar predicción de los precios  
price_prediction <- model %>% 
  predict(X_test, batch_size = 40)

#Revertir escala: pasar de millones a valor original
price_prediction <- price_prediction * 1e6
price_prediction %>%  head() #Parece que las predicciones están bien


#Agregar el identificador de la propiedad


#Como agregar la property id a las predicciones

property_id_test[, "price"] <- price_prediction
final_prediction <- property_id_test
rm(property_id_test)
write.csv(final_prediction, "MLLNN_10_NodesHiddenLayer_1_HiddenLayer_RELU_ActivationFunction_Linear_ExitActivationFunction_Adam_optimizer.csv", row.names = FALSE)


#======================= Entrenar Red más compleja =============================

#Especificar la arquitectura de la red -----------------------------------------

set.seed("123") #Para que se pueden replicar los resultados

#Hiperparámetros: 
#(i)Número de nodos por capa oculta 
#(ii) Función de activación 
#(iii) Función de la capa de salida
#(iv) Número de capas ocultas 
#(v) Método para optimizar la función de perdida (Es gradiente descent o una variación)


model2 <- keras_model_sequential() %>% 



#============================== Playground  ====================================
##Ideas para mejorar el modelo---------------------------------------------------

#(1)Remover valores atipicos de la variable de precios 
#(2)Hacer componentes principales para mejorar el modelo 













