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
       janitor) 

## Directorio ------------------------------------------------------------------
wd <- here()
setwd(wd)

## Datos -----------------------------------------------------------------------

datos <- readRDS("stores/db_final.rds")

datos <- datos %>%
  mutate(across(c(property_id, city, property_type, operation_type, 
      cocina_integral, cocina_americana, parqueadero_visitantes,
      lavanderia, gimnasio, balcon, seguridad, walking_closet,
      starts_with("has"), CODIGO_MAN, CODIGO_UPZ, ESTRATO), as.factor))

datos <- datos %>% clean_names()

datos<- st_as_sf(datos)
geom <- st_geometry(datos)
datos_knn <- datos %>% st_drop_geometry() %>%
  select(-bigramas, - trigramas)

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

#Devolvemos la geometría
st_geometry(datos_imputados) <- geom
datos_imputados_sf <- st_as_sf(datos_imputados)

#Agregamos ln del precio
datos <- datos_imputados_sf %>% mutate(ln_price = log(price))
export(datos, 'stores/datos_modelos.rds')

#Separamos en train y test
test <- datos %>% filter(is.na(price))
train <- datos %>% filter(!is.na(price))

#Eliminamos objetos innecesarios
rm(geom, datos_knn, datos_imputados, datos_imputados_sf)

#Modelos -----------------------------------------------------------------------

## Folds para validación -------------------------------------------------------

###Alistamos lo folds

set.seed(5600)
block_folds <- spatial_block_cv(train, v = 5)
autoplot(block_folds)

## Eliminamos geometry 
train<- as.data.frame(train)
train<- train %>% select(-geometry,-scanombre, -codigo_man, -codigo_upz, 
                         -description, -title)
test<- as.data.frame(test)
test<- test %>% select(-geometry, -scanombre, -codigo_man, -codigo_upz)

## Elastic Net -----------------------------------------------------------------
### configuramos tidymodels (receta)

rec_1 <- recipe(ln_price ~ . , data = train) %>%
  step_rm(property_id, city, operation_type, nombre_upz, text,
          grupo_econom_manz, cocina_integral, parqueadero_visitantes, 
          lavanderia, walking_closet, balcon, seguridad, gimnasio, price) %>% # Eliminamos variables que no queremos como predictores
  step_dummy(all_nominal_predictors()) %>%  # crea dummies para las variables categóricas
  step_novel(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%   #  elimina predictores con varianza cero (constantes) 
  step_normalize(all_numeric_predictors())  # normaliza los predictores. 

### creamos specificación

elastic_net <- linear_reg(penalty = tune(), mixture = tune()) %>%
  set_engine("glmnet") 


### Configuramos flujo de trabajo

workflow_1 <- workflow() %>% 
  # Agregar la receta de preprocesamiento de datos. En este caso la receta 1
  add_recipe(rec_1) %>%
  # Agregar la especificación del modelo de regresión Elastic Net
  add_model(elastic_net)

### configuramos modelos

fit_res1 <- tune_grid(
  workflow_1,
  resamples = block_folds,    # Folds de validación cruzada espacial
  metrics = metric_set(mae)
)

### Ver resultado
collect_metrics(fit_res1)

### Mejor metrica (en este caso es lm entonces no hay hiperparametros)
best_tune<- select_best(fit_res1, metric = "mae")

###Finalizamos flujo
res1_final <- finalize_workflow(workflow_1, best_tune)

# Entrena el modelo en toda la base de entrenamiento
final_fit <- fit(res1_final, data = train)

coefs <- tidy(final_fit)

non_zero_coefs <- coefs %>%
  filter(term != "(Intercept)", estimate != 0)

important_vars <- coefs %>%
  filter(term != "(Intercept)", estimate != 0) %>%
  mutate(abs_estimate = abs(estimate)) %>%
  arrange(desc(abs_estimate))

important_var_names <- important_vars$term

## Creamos submission 1
pred1_ln<-as.vector(predict(final_fit,test))
pred1_ln<-pred1_ln[[1]]

sub2<- test %>% mutate(price=exp(pred1_ln))  %>% 
  mutate(price=round(price))  %>% 
  select(property_id, price)

###export submission
export(fit_res1, "stores/submits/elastic_net.rds")
export(sub2, 'stores/submits/elastic_net_floor.csv')

#Ahora probamos con elastic net con grilla
fit_res2 <- tune_grid(
  workflow_1,
  resamples = block_folds,    # Folds de validación cruzada espacial
  grid = grid_refinada,        # Grilla de valores de penalización
  metrics = metric_set(mae)
)

### Ver resultado
collect_metrics(fit_res2)
show_best(fit_res2, metric = "mae", n = 5) #No mejora respecto al modelo sin grilla
best_tune2 <- select_best(fit_res2, metric = "mae")


## Random Forest ---------------------------------------------------------------

#Seleccionamos solo las variables con non zero coeffficients en el elastic net
train_rf <- train %>% select(-property_id, -city, -operation_type, -nombre_upz, -text,
                             -grupo_econom_manz, -cocina_integral, -parqueadero_visitantes, 
                             -lavanderia, -walking_closet, -balcon, -seguridad, -gimnasio, -n_localidad,
                             -ept_2017, -n_homicidios,-n_lesiones, -n_hurtopersonas, -n_hurtosresidencias,
                             -n_hurtosbicis, -n_delitossexales, -n_violenciaintra, -localidad)

# Preprocesamiento con receta
rec_rf <- recipe(ln_price ~ . , data = train_rf) %>%
  step_rm(price) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_novel(all_nominal_predictors()) %>%
  step_zv(all_predictors())  # NO normalizamos porque Random Forest no lo necesita

# Especificación del modelo Random Forest
rf_spec <- rand_forest(
  mtry = tune(),      # Número de predictores aleatorios por split
  min_n = tune(),     # Mínimo de observaciones por hoja
  trees = 500         # Número de árboles (puedes aumentar)
) %>%
  set_engine("ranger") %>%
  set_mode("regression")

# Flujo de trabajo
workflow_rf <- workflow() %>%
  add_recipe(rec_rf) %>%
  add_model(rf_spec)

# Tamaño del grid: depende del número de predictores
# Necesitamos saber cuántos predictores hay luego del preprocesamiento
prep_recipe <- prep(rec_rf)
num_pred <- ncol(bake(prep_recipe, new_data = NULL)) - 1  # menos la variable respuesta

# Grilla de hiperparámetros
rf_grid <- grid_regular(
  mtry(range = c(10, 45)),
  min_n(range = c(2, 20)),
  levels = 6
)

#Activamos la paralelización
#library(future)
#plan(multisession, workers=6)

# Ajuste por validación cruzada espacial
set.seed(5600)
fit_res_rf <- tune_grid(
  workflow_rf,
  resamples = block_folds,
  grid = rf_grid,
  metrics = metric_set(mae)
)


### Ver resultado
collect_metrics(fit_res_rf)
show_best(fit_res_rf, metric = "mae", n = 5) #No mejora respecto al modelo sin grilla

export(fit_res_rf, "stores/submits/random_forest_500_ngrid.rds")

### Mejor metrica (en este caso es lm entonces no hay hiperparametros)
best_tune_rf <- select_best(fit_res_rf, metric = "mae")

###Finalizamos flujo
rf_final <- finalize_workflow(workflow_rf, best_tune_rf)

# Entrena el modelo en toda la base de entrenamiento
final_fit_rf <- fit(rf_final, data = train_rf)

## Creamos submission 
pred_ln_rf<-as.vector(predict(final_fit_rf,test))
pred_ln_rf<-pred_ln_rf[[1]]

pred_rf<- test %>% mutate(price=exp(pred_ln_rf))  %>% 
  mutate(price=round(price))  %>% 
  select(property_id, price)

###export submission
export(pred_rf, 'stores/submits/random_forest_500_ngrid.csv')

