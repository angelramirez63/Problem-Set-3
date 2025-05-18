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
       here) 
## Directorio ------------------------------------------------------------------
wd <- here()
setwd(wd)

## Datos -----------------------------------------------------------------------

datos_espaciales <- readRDS("stores/db_final.rds")
datos_texto <- readRDS("stores/base_limpia.rds")

datos_espaciales_limpio <- datos_espaciales %>%
  select(-any_of(setdiff(names(datos_texto), "property_id")))

datos <- left_join(datos_texto, datos_espaciales_limpio, by="property_id")

datos <- datos %>%
  mutate(across(c(property_id, city, property_type, operation_type, 
      cocina_integral, cocina_americana, parqueadero_visitantes,
      lavanderia, gimnasio, balcon, seguridad, walking_closet,
      starts_with("has"), CODIGO_MAN, CODIGO_UPZ ), as.factor))

datos<- st_as_sf(datos)
geom <- st_geometry(datos)
datos_knn <- st_drop_geometry(datos) %>%
  select(-bigramas, - trigramas, -geometry.y)

# Variables a excluir de la imputación
excluir_manual <- c("price")
excluir_texto <- names(datos)[sapply(datos, is.character)]
variables_a_excluir <- union(excluir_manual, excluir_texto)

# Aplicar KNN imputando solo sobre las demás variables
datos_imputados <- kNN(
  datos_knn,
  variable = setdiff(names(datos_knn), variables_a_excluir),
  k = 5,            # puedes ajustar k según el tamaño de tu base
  imp_var = FALSE   # no crear columnas extra con sufijo _imp
)

st_geometry(datos_imputados) <- geom
datos_imputados_sf <- st_as_sf(datos_imputados)

datos_imputados_sf <- datos_imputados_sf %>% mutate(ln_price = log(price))
export(datos_imputados_sf, 'datos_modelos.rds')

test <- datos_imputados_sf %>% filter(is.na(price))
train <- datos_imputados_sf %>% filter(!is.na(price))

#Modelos -----------------------------------------------------------------------

## Folds para validación -------------------------------------------------------

###Alistamos lo folds

set.seed(560)
block_folds <- spatial_block_cv(train, v = 5)
autoplot(block_folds)

## Eliminamos geometry 
train<- as.data.frame(train)
train<- train %>% select(-geometry,-SCANOMBRE, -CODIGO_MAN, -CODIGO_UPZ)
test<- as.data.frame(test)
test<- test %>% select(-geometry, -SCANOMBRE, -CODIGO_MAN, -CODIGO_UPZ)

### configuramos tidymodels (receta)

rec_1 <- recipe(ln_price ~ . , data = train) %>%
  step_dummy(all_nominal_predictors()) %>%  # crea dummies para las variables categóricas
  step_zv(all_predictors()) %>%   #  elimina predictores con varianza cero (constantes)
  step_normalize(all_predictors())  # normaliza los predictores. 

### creamos specificación

reg_lineal <- linear_reg() %>% set_engine("lm") 

### Configuramos flujo de trabajo

workflow_1 <- workflow() %>% 
  # Agregar la receta de preprocesamiento de datos. En este caso la receta 1
  add_recipe(rec_1) %>%
  # Agregar la especificación del modelo de regresión Elastic Net
  add_model(reg_lineal)

### configuramos modelos
set.seed(10001)

tune_res1 <- tune_grid(
  workflow_1,         # El flujo de trabajo que contiene: receta y especificación del modelo
  resamples = block_folds,  # Folds de validación cruzada espacial
  metrics = metric_set(mae), # metrica
)

### ver resultado
collect_metrics(tune_res1)

### Mejor metrica (en este caso es lm entonces no hay hiperparametros)
best_tune<- select_best(tune_res1, metric = "mae")

###Finalizamos flujo
res1_final <- finalize_workflow(workflow_1, best_tune)

#Sacamos los coef (para el caso de regression)
reg_coef <- fit(res1_final, data = train)

## Creamos submiission 1
pred1_ln<-as.vector(predict(reg_coef,test))
pred1_ln<-pred1_ln[[1]]

sub1<- test %>% mutate(price=exp(pred1_ln))  %>% 
  mutate(price=round(price))  %>% 
  select(property_id, price)

###export submission
export(sub1, 'Stores/submits/reg_lineal.csv')
