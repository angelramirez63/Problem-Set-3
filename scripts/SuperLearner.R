rm(list = ls())

library(pacman)

p_load(tidyverse,
       dplyr,
       janitor,
       purrr,
       sf,
       nnls,
       data.table,
       SuperLearner
)

# Crear el directorio 
setwd("C:/Users/Adram/OneDrive - Universidad de los Andes/8 OCTAVO SEMESTRE/BDML/Problem-Set-3/stores")

#_________________________Cargamoslos datos_____________________________________

db <- readRDS("db_final.rds")

#------------------Vamos a corregir algunas variables---------------------------
# Concatenar title y description
db$text <- str_to_lower(paste(db$title, db$description, sep = " "))
db <- db %>% 
  select(-title, -description)

# Cocina americana #
db <- db %>%
  mutate(
    cocina_americana = if_else(
      str_detect(
        text,
        regex("cocina (tipo )?(americana|americano|americno|america|americado|amerciana|amerikana)|cocina abierta",
              ignore_case = TRUE)
      ),
      1, 0
    )
  )

# Cocina integral #
db <- db %>%
  mutate(
    cocina_integral = if_else(
      str_detect(
        str_to_lower(text),
        "\\bcocina (tipo )?integral\\b"
      ),
      1, 0
    )
  )

# Parqueadero visitantes #
db <- db %>%
  mutate(
    parqueadero_visitantes = if_else(
      str_detect(
        str_to_lower(text),
        "parqueaderos? (de|para) visitantes?"
      ),
      1, 0
    )
  )

# Lavandería #
db <- db %>%
  mutate(
    lavanderia = if_else(
      str_detect(str_to_lower(text), "\\blavander[ií]a\\b|\\blavandera\\b"),
      1, 0
    )
  )

# gimnasio #
db <- db %>%
  mutate(
    gimnasio = if_else(
      str_detect(str_to_lower(text), "gimnasio|gym"),
      1, 0
    )
  )

# Balcón #
db <- db %>%
  mutate(
    balcon = if_else(
      str_detect(str_to_lower(text), "\\bbalcon(es)?\\b|\\bbalcn\\b"),
      1, 0
    )
  )

# Vigilancia #
db <- db %>%
  mutate(
    seguridad = if_else(
      str_detect(str_to_lower(text), "\\bseguridad\\b|\\bvigilancia\\b"),
      1, 0
    )
  )

# Walking closet #
db <- db %>%
  mutate(
    walking_closet = if_else(
      str_detect(str_to_lower(text), "walking closet(h|s)?"),
      1, 0
    )
  )

#----------------------Corrección completa--------------------------------------

# Eliminamos variables irrelevantes
db <- db %>% 
  select(-bigramas, - trigramas, -city, -operation_type,
         -grupo_area, -text)

#Variable de crímen generalizado
train <- db %>% 
  mutate(n_crimenes =  n_homicidios+ n_lesiones+ n_hurtopersonas+
           n_hurtosautos+ n_hurtosresidencias+ n_hurtosbicis+ n_hurtosmotos+
           n_hurtoscomercio+ n_hurtoscelular+ n_delitossexales+ n_violenciaintra)

# Convertir a factores
db <- db %>%
  mutate(across(c(property_id, property_type, cocina_integral, 
                  cocina_americana, parqueadero_visitantes,
                  lavanderia, gimnasio, balcon, seguridad, walking_closet,
                  starts_with("has"), CODIGO_MAN, CODIGO_UPZ ), as.factor))

# Renombrar
names(db) <- names(db) %>%
  tolower() %>%                            # Asegura que todo esté en minúsculas
  gsub("^has_", "", .) %>%                # Elimina el prefijo "has_"
  gsub("\\(es\\)|\\(s\\)", "", .) %>%     # 3. Eliminar "(es)" y "(s)"
  gsub("\\?", "", .)                      # 4. Eliminar signos de interrogación

# Filtrar las observaciones en train y test set
train <- db %>% 
  filter(train == 1)

test <- db %>% 
  filter(train ==0)

#__________________________SuperLearner_________________________________________

# Semilla para reproducibilidad
set.seed(102030)

# Creamos 5 folds para validación cruzada
folds <- 5
index <- split(sample(1:length(y)), rep(1:folds, length = length(y)))

db_0 <- st_drop_geometry(db)