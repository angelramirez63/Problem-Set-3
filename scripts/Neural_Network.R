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

#Distribución variables estructurales del apto ---------------------------------


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

#Remover variables con near zero var---------------------####
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

#Resumir variables de crimen ---------------------------------------------------
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

#Tratar outliers ---------------------------------------------------------------

#Solo tratare los outliers de la base de entrenamiento 
#Crear base con los datos de entrenamiento 
train_db <- db %>% 
            filter(train == 1) %>% 
            select(-train)


##2sigma rule limites --------------------------####


#Función para el lower bound
lower_bound <- function(var_name, db_name) {
  
  #Obtener base de datos 
  db <- get(db_name)
  var_vector <- db[[var_name]] #Se pone doble parentísis cuadro para obtener un vector en vez de un dataframe
  
  #Calcular el intervalo de two_sigma_rule
  mean_var <- mean(var_vector, na.rm = TRUE)
  sd_var <- sd(var_vector, na.rm = TRUE)
  lower_bound <- mean_var - 2 * sd_var

  #Retornar resultado
  return(lower_bound)

}

#Función para el upper bound
upper_bound <- function(var_name, db_name) {
  
  #Obtener base de datos 
  db <- get(db_name)
  var_vector <- db[[var_name]]
  
  #Calcular el intervalo de two_sigma_rule
  mean_var <- mean(var_vector, na.rm = TRUE)
  sd_var <- sd(var_vector, na.rm = TRUE)
  upper_bound <- mean_var + 2 * sd_var
  
  #Retornar resultado 
  return(upper_bound)
}



##Visualizar distribución de las variables (otra vez) ---------------####


#Visualizar variables estructurales más importantes
vars_esctructura_1 <-  c("price", "ln_price", "bedrooms", "n_banos", "n_parqueaderos", "area", "ESTRATO")

for (variable in vars_esctructura_1){
  
  print(PlotBars(variable, "train_db"))
  
}

for (variable in vars_esctructura_1){
  
  print(PlotDensity(variable, "train_db"))
  
}

#Visualizar variables estructurales restantes
vars_esctructura_2 <- c("has_bbq", "has_terraza", "has_deposito", "has_conjunto", 
                        "has_ascensor", "has_patio", "has_duplex", "has_piscina", 
                        "grupo_area", "cocina_americana")

for (variable in vars_esctructura_2){
  
  print(PlotBars(variable, "train_db"))
  
}

for (variable in vars_esctructura_2){
  
  print(PlotDensity(variable, "train_db"))
  
}

#La variable grupo area tiene una izquierda muy pesada y una derecha muy muy larga


##Tratar valores atípicos variable de área---------------------####

#Los apartamento con 2500mts de area sigue estando después de la limpieza inicial 
apartamentos_gigantes <- train_db %>% 
                          filter(area >1000) #¡Hay 2693 aparamentos con un area mayor a 1000m2ts! 
PlotDensity("area", "apartamentos_gigantes")

#Visualizar características apartamentos gigantes 
for (variable in vars_esctructura_1){
  
  print(PlotBars(variable, "apartamentos_gigantes"))
  
}
#No parecen ser diferentes en otros características estructurales aparte del área 

#Cargar base train original para comparar con nuestra base 
train_original <- read.csv("train.csv") 
aptos_gigantes_db_original <- train_original %>% filter(surface_total > 1000) #En la original solo hay 10 apartamentos gigantes. Se imputaron areas muy grandes para mas de 2000 aptos
id_aptos_gigantes_originales <- aptos_gigantes_db_original$property_id
rm(train_original, aptos_gigantes_db_original)


#Remover de la base de entrenamiento los 10 apartamentos gigantes originales (para que no alteren el promedio de la imputacion)
`%notin%` <- Negate(`%in%`) #Negación operador in 
train_db <- train_db %>% 
            filter(property_id %notin% id_aptos_gigantes_originales)

#Imputar de forma diferente el área de los aptos para evitar aptos gigantes 
train_db <- train_db %>% 
            group_by(bedrooms, 
                     n_banos, 
                     ESTRATO) %>% 
            mutate(area = ifelse(area > 1000, mean(area), area)) %>% 
            ungroup()
summary(train_db$area)

apartamentos_gigantes <- train_db %>% 
                        filter(area >1000) #Después de la imputación quedaron 146

#Caracterizar apartamentos gigantes que quedan
for (variable in vars_esctructura_1){
  
  print(PlotDensity(variable, "apartamentos_gigantes"))
  
}
#Los apartamento gigantes que queden si tiene en promedio más baños y habitaciones que los demás
rm(apartamentos_gigantes)

##Tratar valores atípicos variable de precio------------------####

#Precio base de entrenamiento previo a remover los outliers 
PlotDensity("price", "train_db") + geom_vline(xintercept = lower_bound("price", "train_db"),linetype="dashed",color="red",size=0.7) +
  geom_vline(xintercept = upper_bound("price", "train_db"),linetype="dashed",color="red",size=0.7)


#Conservar valores extremos
apartamentos_caros <- train_db %>% 
  filter(lower_bound("price", "train_db") >= price | price >= upper_bound("price", "train_db"))

#Caracterización apartamentos caros
for (variable in vars_esctructura_1){
  
  print(PlotDensity(variable, "extreme_values"))
  
}

for (variable in vars_esctructura_2){
  
  print(PlotDensity(variable, "extreme_values"))
  
}

#(i)Son en su mayoría apartamentos estrato 6
#(ii) Por lo demas tienen valores parecidos a los todo la base para baños, habitaciones, área y otras características estrurales
#No a remover estos valores porque no quiero mover sistematicamente apartamentos de Estrato 6
rm(apartamentos_caros)


#=============================== Playground ====================================

#Cargar base train original 
train_original <- read.csv("train.csv") 

aptos_gigantes_db_origianl <- train_original %>% filter(surface_total > 1000)

##Ideas para mejorar el modelo---------------------------------------------------

#(1)Remover valores atipicos de la variable de precios 
#(2)Hacer componentes principales para mejorar el modelo 

