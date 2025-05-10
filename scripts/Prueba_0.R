library(pacman)

p_load(tidyverse,
       tidytext,
       dplyr,
       tidyr,
       stringr,
       readr,
       topicmodels,
       cluster,
       tm,
       tokenizers
)

rm(list = ls())

# Crear el directorio 
setwd("C:/Users/Adram/OneDrive - Universidad de los Andes/8 OCTAVO SEMESTRE/BDML/Problem-Set-3/stores")

# Cargamos la base de datos de train
db_tr <- read.csv("train.csv") %>% 
  as_tibble()

# Concatenar title y description
db_tr$text <- paste(db_tr$title, db_tr$description, sep = " ")

##------ Buscamos las palabras más repetidas ------##
palabras <- data.frame(text = db_tr$text) %>% 
  unnest_tokens(word, text) %>% 
  count(word, sort = TRUE)

##------ Buscamos conjunto de dos palabras más repetidas ------##
# Bigramas
db_tr <- db_tr %>%
  mutate(bigramas = tokenize_ngrams(text, n = 2))

# Desanidar todos los bigramas en una sola columna
df_bigramas <- db_tr %>%
  select(bigramas) %>%
  unnest_longer(bigramas)

# Contar frecuencia de cada bigrama
frecuencias_bigramas <- df_bigramas %>%
  count(bigramas, sort = TRUE)

##------ Buscamos conjunto de tres palabras más repetidas ------##
# Trigramas
db_tr <- db_tr %>%
  mutate(trigramas = tokenize_ngrams(text, n = 3))

# Desanidar todos los trigramas en una sola columna
df_trigramas <- db_tr %>%
  select(trigramas) %>%
  unnest_longer(trigramas)

# Contar frecuencia de cada trigrama
frecuencias_trigramas <- df_trigramas %>%
  count(trigramas, sort = TRUE)

##------ Código para hacer la búsqueda de palabras más sencilla ------##

# Palabra a buscar
palabra_objetivo <- "estrato"

# Filtrar bigramas
bigramas_filtrados <- frecuencias_bigramas %>%
  filter(str_detect(bigramas, fixed(palabra_objetivo)))

# Filtrar trigramas
trigramas_filtrados <- frecuencias_trigramas %>%
  filter(str_detect(trigramas, fixed(palabra_objetivo)))

##------ construir variables ------##

# Área
db_tr <- db_tr %>%
  mutate(
    area_m2 = str_extract(text, regex("\\b\\d+\\s*(m2|mts2|mts|metros cuadrados)\\b", ignore_case = TRUE)),
    area_m2 = str_extract(area_m2, "\\d+"),  # Extrae solo el número
    area_m2 = as.numeric(area_m2)            # Lo convierte en número
  )

# Baños
db_tr <- db_tr %>%
  mutate(
    banos = case_when(
      str_detect(text, regex("\\b(5|cinco) banos\\b", ignore_case = TRUE)) ~ 5,
      str_detect(text, regex("\\b(4|cuatro) banos\\b", ignore_case = TRUE)) ~ 4,
      str_detect(text, regex("\\b(3|tres) banos\\b", ignore_case = TRUE)) ~ 3,
      str_detect(text, regex("\\b(2|dos) banos\\b", ignore_case = TRUE)) ~ 2,
      str_detect(text, regex("\\b(1|un|uno) bano\\b", ignore_case = TRUE)) ~ 1,
      TRUE ~ NA_real_  # Si no detecta nada, deja como NA
    )
  )

# Habitaciones
db_tr <- db_tr %>%
  mutate(
    habitaciones = case_when(
      str_detect(text, regex("\\b(1|una|un) (habitacion(es)?|alcoba(s)?)\\b", ignore_case = TRUE)) ~ 1,
      str_detect(text, regex("\\b(2|dos) (habitacion(es)?|alcoba(s)?)\\b", ignore_case = TRUE)) ~ 2,
      str_detect(text, regex("\\b(3|tres) (habitacion(es)?|alcoba(s)?)\\b", ignore_case = TRUE)) ~ 3,
      str_detect(text, regex("\\b(4|cuatro) (habitaciones?|alcoba(s)?)\\b", ignore_case = TRUE)) ~ 4,
      str_detect(text, regex("\\b(5|cinco) (habitaciones?|alcoba(s)?)\\b", ignore_case = TRUE)) ~ 5,
      str_detect(text, regex("\\b(6|seis) (habitaciones?|alcoba(s)?)\\b", ignore_case = TRUE)) ~ 6,
      str_detect(text, regex("\\b(7|siete) (habitaciones?|alcoba(s)?)\\b", ignore_case = TRUE)) ~ 7,
      str_detect(text, regex("\\b(8|ocho) (habitaciones?|alcoba(s)?)\\b", ignore_case = TRUE)) ~ 8,
      str_detect(text, regex("\\b(9|nueve) (habitaciones?|alcoba(s)?)\\b", ignore_case = TRUE)) ~ 9,
      str_detect(text, regex("\\b(10|diez) (habitaciones?|alcoba(s)?)\\b", ignore_case = TRUE)) ~ 10,
      str_detect(text, regex("\\b(11|once) (habitaciones?|alcoba(s)?)\\b", ignore_case = TRUE)) ~ 11,
      TRUE ~ NA_real_
    )
  )

# Cocina americana
db_tr <- db_tr %>%
  mutate(
    cocina_americana = if_else(
      str_detect(
        text,
        regex("cocina (tipo )?(americana|americano|americno|america|americado|amerciana)|cocina abierta",
              ignore_case = TRUE)
      ),
      1, 0
    )
  )

# Cocina integral
db_tr <- db_tr %>%
  mutate(
    cocina_integral = if_else(
      str_to_lower(str_trim(text)) %in% c("cocina integral", 
                                          "cocina tipo integral"),
      1, 0
    )
  )

# Parqueadero visitantes
db_tr <- db_tr %>%
  mutate(
    parqueadero_visitantes = if_else(
      str_to_lower(str_trim(text)) %in% c("parqueadero de visitantes", 
                                          "parqueaderos de visitantes",
                                          "parqueadero para visitantes",
                                          "parqueaderos para visitantes"),
      1, 0
    )
  )

# Parqueadero
db_tr <- db_tr %>%
  mutate(
    parqueaderos = case_when(
      str_detect(text, regex("\\b(1|un) (parqueadero(s)?|garaje(s)?)\\b", ignore_case = TRUE)) ~ 1,
      str_detect(text, regex("\\b(2|dos) (parqueadero(s)?|garaje(s)?)\\b", ignore_case = TRUE)) ~ 2,
      str_detect(text, regex("\\b(3|tres) (parqueadero(s)?|garaje(s)?)\\b", ignore_case = TRUE)) ~ 3,
      str_detect(text, regex("\\b(4|cuatro) (parqueadero(s)?|garaje(s)?)\\b", ignore_case = TRUE)) ~ 4,
      str_detect(text, regex("\\b(5|cinco) (parqueadero(s)?|garaje(s)?)\\b", ignore_case = TRUE)) ~ 5,
      str_detect(text, regex("\\b(6|seis) (parqueadero(s)?|garaje(s)?)\\b", ignore_case = TRUE)) ~ 6,
      str_detect(text, regex("\\b(7|siete) (parqueadero(s)?|garaje(s)?)\\b", ignore_case = TRUE)) ~ 7,
      str_detect(text, regex("\\b(8|ocho) (parqueadero(s)?|garaje(s)?)\\b", ignore_case = TRUE)) ~ 8,
      TRUE ~ NA_real_
    )
  )

# Lavandería
db_tr <- db_tr %>%
  mutate(
    lavanderia = if_else(
      str_to_lower(str_trim(text)) %in% c("lavanderia", 
                                          "lavandera"),
      1, 0
    )
  )

# gimnasio
db_tr <- db_tr %>%
  mutate(
    gimnasio = if_else(
      str_to_lower(str_trim(text)) %in% c("gimnasio", 
                                          "gym"),
      1, 0
    )
  )

# Balcón
db_tr <- db_tr %>%
  mutate(
    balcon = if_else(
      str_to_lower(str_trim(text)) %in% c("balcon", 
                                          "balcn",
                                          "balcones"),
      1, 0
    )
  )

# Seguridad
db_tr <- db_tr %>%
  mutate(
    seguridad = if_else(
      str_to_lower(str_trim(text)) %in% c("vigilancia", 
                                          "seguridad",
                                          ),
      1, 0 # Vamos a tomar seguridad y vigilanica como el mímso indicativo
    )
  )

# walking clotet
db_tr <- db_tr %>%
  mutate(
    walking_closet = if_else(
      str_to_lower(str_trim(text)) %in% c("walking closet", 
                                          "walking closeth",
                                          "walking closets"
      ),
      1, 0 # Vamos a tomar seguridad y vigilanica como el mímso indicativo
    )
  )

# Estrato
db_tr <- db_tr %>%
  mutate(
    estrato = case_when(
      str_detect(text, regex("\\b(6|seis) estrato\\b", ignore_case = TRUE)) ~ 6,
      str_detect(text, regex("\\b(5|cinco) estrato\\b", ignore_case = TRUE)) ~ 5,
      str_detect(text, regex("\\b(4|cuatro) estrato\\b", ignore_case = TRUE)) ~ 4,
      str_detect(text, regex("\\b(3|tres) estrato\\b", ignore_case = TRUE)) ~ 3,
      str_detect(text, regex("\\b(2|dos) estrato\\b", ignore_case = TRUE)) ~ 2,
      str_detect(text, regex("\\b(1|uno|un) estrato\\b", ignore_case = TRUE)) ~ 1,
      TRUE ~ NA_real_
    )
  )

#_____________________________________________________________________________#

# Definir palabras clave y expresiones regulares
keywords <- c("bbq", "terraza(s)?", "deposito", "chimenea(s)", "conjunto",
              "ascensor(es)?", "patio(s)?", "duplex", "piscina", "sauna",
              "jacuzzi", "altillo", "zona(s)? verde(es)?")

patterns <- paste0("\\b(", keywords, ")\\b")

# Crear variables binarias
for (i in seq_along(keywords)) {
  var_name <- paste0("has_", gsub(" ", "_", keywords[i]))
  datos[[var_name]] <- as.integer(str_detect(tolower(datos$text_combined), regex(patterns[i], ignore_case = TRUE)))
}

#Ver missings
sum(is.na(datos$metros_cuadrados))
sum(is.na(datos$estrato))

#_____________________________________________________________________________#

