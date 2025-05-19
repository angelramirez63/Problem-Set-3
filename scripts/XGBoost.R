### Modelos predictivos ###

rm(list = ls())

# Preparación ------------------------------------------------------------------
## Librerias -------------------------------------------------------------------
if(!require(pacman)) install.packages("pacman") ; require(pacman)

p_load(tidyverse, # Manipular dataframes
       rio, # Importar datos fácilmente
       sf, # Leer/escribir/manipular datos espaciales
       tidymodels, # Modelado de datos limpios y ordenados
       rattle, # Interfaz gráfica para el modelado de datos
       spatialsample, # Muestreo espacial para modelos de aprendizaje automático
       ranger,
       rpart,
       xgboost,
       nnet, 
       VIM, 
       here,
       caret,
       janitor) 

## Datos -----------------------------------------------------------------------

datos <- readRDS("stores/db_final.rds")

#Limpiar nombres de variables

# Limpiar nombres de variables

train <- train %>% clean_names()

# Verificar los nuevos nombres
print(names(train))


#Crear nuevas variables

#Variable de crímen generalizado

train <- datos %>% 
  mutate(n_crimenes =  n_homicidios+ n_lesiones+ n_hurtopersonas+
         n_hurtosautos+ n_hurtosresidencias+ n_hurtosbicis+ n_hurtosmotos+
         n_hurtoscomercio+ n_hurtoscelular+ n_delitossexales+ n_violenciaintra )


datos <- datos %>%
  mutate(across(c(property_id, city, property_type, operation_type, 
                  cocina_integral, cocina_americana, parqueadero_visitantes,
                  lavanderia, gimnasio, balcon, seguridad, walking_closet,
                  starts_with("has"), CODIGO_MAN, CODIGO_UPZ ), as.factor))

datos <- datos %>% 
  select(-bigramas, - trigramas)

# Filtrar las observaciones donde train == 1
train <- datos %>% 
  filter(train == 1)

#Ver los nombres de cada una de las variables para construir los modelos

variable_names <- names(train)
print(variable_names)

#Ojo: Hay variables con factores con un solo nivel (Identificación)

# Identificar variables categóricas
factor_vars <- sapply(train, is.factor)

# Verificar la cantidad de niveles en cada variable categórica
levels_count <- sapply(train[factor_vars], function(x) length(unique(x)))
print(levels_count)

# Identificar variables con menos de 2 niveles
single_level_vars <- names(which(levels_count < 2))

# Verificar las variables a eliminar
print(single_level_vars)



#XG Boost 1  -------------------------------------------------------------------


# Configuración correcta del trainControl para regresión
ctrl <- trainControl(
  method = "cv",
  number = 10,
  summaryFunction = defaultSummary,  # Para regresión
  savePredictions = "final",
  verboseIter = TRUE
)

#Grilla de búsqueda 

grid_xbgoost <- expand.grid(nrounds = c(250,500),
                            max_depth = c(1, 5),
                            eta = c(0.1,  0.01), 
                            gamma = c(0, 1), 
                            min_child_weight = c(10, 25),
                            colsample_bytree = c(0.4, 0.7, 1), 
                            subsample = c(0.4))
grid_xbgoost

set.seed(1511)

Xgboost_tree <- train(price ~ bedrooms+ property_type+ localidad+ scanombre+ 
                      epe_upz+ nombre_upz+ luminarias+ distnearestlibrary+
                      distnearestschool+ distnearestmuseum+ distnearestpark+
                      distnearestmall+ distnearesttransmi+ estrato+ 
                      aval_comer_manz+ aval_catas_manz+ grupo_econom_manz+
                      distnearestsitp+ n_homicidios+ n_lesiones+ n_hurtopersonas+
                      n_hurtosautos+ n_hurtosresidencias+ n_hurtosbicis+n_hurtosmotos+
                      n_hurtoscomercio+ n_hurtoscelular+ n_delitossexales+ n_violenciaintra+
                      num_restaurantes_manz+ distcicloruta+ cbd_distancia+ distnearest_cai+
                      distnearest_hospital+ distnearest_gym+ distnearest_convenience_store+ 
                      distnearest_pharmacy+ n_arboles_upz+ cocina_americana+ 
                      has_bbq+ has_terraza_s+ has_deposito + has_chimenea_s +
                      has_conjunto + has_ascensor_es + has_patio_s + has_duplex +
                      has_piscina + has_sauna + has_jacuzzi + has_altillo +
                      has_zona_s_verde_es + n_banos+ area+
                      n_parqueaderos,
                      data=train,
                      method = "xgbTree", 
                      trControl = ctrl,
                      tuneGrid=grid_xbgoost,
                      metric = "MAE",
                      na.action = na.pass
)        

Xgboost_tree
