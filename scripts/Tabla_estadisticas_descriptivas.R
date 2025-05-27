#====================== Tabla estadísticas descriptivas  =======================


#Alistar ambiente de trabajo ---------------------------------------------------

#Limpiar ambiente
rm(list = ls())
cat("\014")

require(pacman)
#Cargar paquetes
p_load(tidyverse,
       here,
       skimr,
       rio,
       xtable, 
       knitr, 
       stargazer)

#Definir el directorio
wd <- here()

#Cargar datos
db <- readRDS('stores/datos_modelos.rds') 

#Alistar datos -----------------------------------------------------------------
db <- readRDS('stores/datos_modelos.rds') 

vars <- c(
  "price", "bedrooms", "n_banos", "area", "n_parqueaderos", "ESTRATO", #Variables estructurales 
  "distnearestpark", "distnearesttransmi", "distnearestCAI", "distnearestHospital",  #Amenidades locales 
  "train"
  
)


#Variables para hacer las estadísticas descriptivas 
db_clean <- db %>% select(all_of(vars))

#Renombrar variables
db_clean <- db_clean %>% 
            rename(
              Precio = price, 
              Habitaciones = bedrooms, 
              Baños = n_banos, 
              Area = area, 
              Parqueaderos = n_parqueaderos, 
              Estrato = ESTRATO, 
              Dist_Parque = distnearestpark, 
              Dist_TM = distnearesttransmi, 
              Dist_Hopsital = distnearestHospital,
              Dist_CAI = distnearestCAI
            )

#Cambiar metros por kilometros
db_clean <- db_clean %>% 
            mutate(across(starts_with("Dist"), ~ .x / 1000))

##Dividir bases entre entrenamiento y testeo 

#Base de entrenamieto  
db_train <- db_clean %>%  
  filter(train == 1) %>% 
  select(-train, -Precio) %>% 
  as.data.frame()


#Base de datos no pobres
db_test <- db_clean %>%  
  filter(train == 0) %>% 
  select(-train, -Precio) %>% 
  as.data.frame()

#Tablas estadísticas descriptivas-----------------------------------------------


#Estadísticas descriptivas entrenamiento 
stargazer(db_train, type = "latex", 
          title = "Estadísticas Descriptivas Base de Entranimiento",
          digits = 3, summary.stat = c("mean", "sd", "min", "max", "median"))

#Estadísticas descriptivas testeo 
stargazer(db_test, type = "latex", 
          title = "Estadísticas Descriptivas Base de Testeto",
          digits = 3, summary.stat = c("mean", "sd", "min", "max", "median"))







