### Boosting ###

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
       DiagrammeR,
       janitor) 

## Datos -----------------------------------------------------------------------

datos <- readRDS("db_final.rds")

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

# Filtrar las observaciones en train y test set

train <- datos %>% 
  filter(train == 1)

test <- datos %>% 
  filter(train ==0)

# Limpiar nombres de variables

train <- train %>% clean_names()
test <- test %>% clean_names()

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

grid_xbgoost1 <- expand.grid(nrounds = c(250,500), #Iteraciones o árboles que se van a estimar para aprender más con cada uno.
                            max_depth = c(1, 5), #Qué tan complejo debe ser cada árbol (mayor complejidad = mayor profunidad).
                            eta = c(0.1,  0.01),  #Tasa de aprendizaje.
                            gamma = c(0, 1), #Nivel mínimo de reducción en la función de pérdida.
                            min_child_weight = c(10, 25), #Mínimo número de observaciones por nodo.
                            colsample_bytree = c(0.4, 0.7, 1), #Porcentaje total de variables utilizadas en cada iteración.
                            subsample = c(0.4)) # Porcentaje de la muestra utilizada para entrenar el modelo.
grid_xbgoost1

set.seed(1511)

Xgboost_tree1 <- train(price ~ bedrooms+ property_type + distnearestlibrary+
                      distnearestschool+ distnearestmuseum+ distnearestpark+
                      distnearestmall+ distnearesttransmi+ estrato+ 
                      aval_comer_manz+ aval_catas_manz +
                      distnearestsitp+ n_homicidios+ n_lesiones+ n_hurtopersonas+
                      n_hurtosautos+ n_hurtosresidencias+ n_hurtosbicis+n_hurtosmotos+
                      n_hurtoscomercio+ n_hurtoscelular+ n_delitossexales+ n_violenciaintra+
                      num_restaurantes_manz+ distcicloruta+ cbd_distancia+ distnearest_cai+
                      distnearest_hospital+ distnearest_gym+ distnearest_convenience_store+ 
                      distnearest_pharmacy+ cocina_americana+ 
                      has_bbq+ has_terraza_s+ has_deposito + has_chimenea_s +
                      has_conjunto + has_ascensor_es + has_patio_s + has_duplex +
                      has_piscina + has_sauna + has_jacuzzi + has_altillo +
                      has_zona_s_verde_es + n_banos+ area+
                      n_parqueaderos,
                      data=train,
                      method = "xgbTree", 
                      trControl = ctrl,
                      tuneGrid=grid_xbgoost1,
                      metric = "MAE",
                      na.action = na.pass
)        

Xgboost_tree1

#"Aggregating results
#Selecting tuning parameters
#Fitting nrounds = 500, max_depth = 5, eta = 0.1, gamma = 1, colsample_bytree = 1, min_child_weight = 10, subsample = 0.4 on full training set"



na_rows <- test %>%
  filter(!complete.cases(.))
nrow(na_rows)

# Predicción de los valores 

pred_values1 <- predict(Xgboost_tree1, newdata = test)

#Importancia de variables en XGBoost 1 

variables_importance_XG1 <- varImp(Xgboost_tree1)
print(variables_importance_XG1)

#Visualizar árbol

tree_plot <- xgboost::xgb.plot.tree(
  model = Xgboost_tree1$finalModel,
  trees = 1:2,
  plot_width = 1000,
  plot_height = 500)
tree_plot

#Exportar a Kaggle

subXG1<- test %>% mutate(price=pred_values1) %>% select(property_id, price)
subXG1 <- st_drop_geometry(subXG1)
export(subXG1, 'submits/XGBoost1intento.csv')

#XG Boost 2  -------------------------------------------------------------------

# Validación cruzada espacial

train <- train %>%
  mutate(cbd_distancia = as.numeric(cbd_distancia))


# definimos nuestra variable como sf
train_sf <- st_as_sf(
  train,
  # "coords" is in x/y order -- so longitude goes first!
  coords = c("lon", "lat"),
  # Set our coordinate reference system to EPSG:4326,
  # the standard WGS84 geodetic coordinate reference system
  crs = 4326
)

set.seed(1511)
block_folds <- spatial_block_cv(train_sf, v = 5)
block_folds

autoplot(block_folds)

p_load("purrr")

walk(block_folds$splits, function(x) print(autoplot(x)))

rec_1 <- recipe(price ~bedrooms+ property_type + distnearestlibrary+
                  distnearestschool+ distnearestmuseum+ distnearestpark+
                  distnearestmall+ distnearesttransmi+ estrato+ 
                  aval_comer_manz+ aval_catas_manz +
                  distnearestsitp+ n_homicidios+ n_lesiones+ n_hurtopersonas+
                  n_hurtosautos+ n_hurtosresidencias+ n_hurtosbicis+n_hurtosmotos+
                  n_hurtoscomercio+ n_hurtoscelular+ n_delitossexales+ n_violenciaintra+
                  num_restaurantes_manz+ distcicloruta + distnearest_cai+
                  distnearest_hospital+ distnearest_gym+ distnearest_convenience_store+ 
                  distnearest_pharmacy+ cocina_americana+ 
                  has_bbq+ has_terraza_s+ has_deposito + has_chimenea_s +
                  has_conjunto + has_ascensor_es + has_patio_s + has_duplex +
                  has_piscina + has_sauna + has_jacuzzi + has_altillo +
                  has_zona_s_verde_es + n_banos+ area+
                  n_parqueaderos , data = train) %>%
  step_dummy(all_nominal_predictors()) %>%  # crea dummies para las variables categóricas
  step_interact(terms = ~ distnearestpark:matches("property_type") + area:matches("property_type")) %>% 
  step_novel(all_nominal_predictors()) %>%   # para las clases no antes vistas en el train. 
  step_zv(all_predictors()) %>%   #  elimina predictores con varianza cero (constantes)
  step_normalize(all_predictors())   # normaliza los predictores. 

xgboost_spec <- boost_tree(
  trees = tune(),           # número de árboles
  tree_depth = tune(),      # profundidad del árbol
  learn_rate = tune(),      # tasa de aprendizaje (eta)
  loss_reduction = tune(),  # gamma: reducción mínima de pérdida
  sample_size = tune(),     # subsample
  mtry = tune(),            # número de predictores a muestrear en cada división 
) %>%
  set_engine("xgboost") %>%
  set_mode("regression")  # o "classification" si es clasificación


workflow_1 <- workflow() %>% 
  # Agregar la receta de preprocesamiento de datos. En este caso la receta 1
  add_recipe(rec_1) %>%
  # Agregar la especificación del modelo de regresión Elastic Net
  add_model(xgboost_spec)

library(tidymodels)

# Definir rangos de hiperparámetros

xgboost_grid <- expand_grid(
  trees = c(250, 500, 1000),
  learn_rate = c(0.01, 0.1),
  tree_depth = c(3, 6, 10),
  loss_reduction = c(0, 1, 10),
  sample_size = c(0.4, 0.75, 1),
  mtry = c(7, 25)  # ejemplo para 6 o más predictores
)


set.seed(1511)

tune_res1 <- tune_grid(
  workflow_1,         # El flujo de trabajo que contiene: receta y especificación del modelo
  resamples = block_folds,  # Folds de validación cruzada espacial
  grid = xgboost_grid,        # Grilla de valores de penalización
  metrics = metric_set(mae)  # metrica
)

collect_metrics(tune_res1)

# Utilizar 'select_best' para seleccionar el mejor valor.
best_tune_res1 <- select_best(tune_res1, metric = "mae")
best_tune_res1

# Finalizar el flujo de trabajo 'workflow' con el mejor valor de parametros
res1_final <- finalize_workflow(workflow_1, best_tune_res1)

XG_final1_fit <- fit(res1_final, data = train)

predicciones <- predict(XG_final1_fit, new_data = test)

subXG2<- test %>% mutate(price=predicciones)  %>% 
  select(property_id, price)

subXG2 <- st_drop_geometry(subXG2)

subXG2 <- data.frame(
  property_id = test$property_id,
  price = pull(predicciones, .pred)  # extrae la columna como vector
)

write.csv(subXG2, "submits/XGBoost2intento.csv", row.names = FALSE)




