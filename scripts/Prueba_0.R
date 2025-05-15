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
       tokenizers,
       sf
)

rm(list = ls())

# Crear el directorio 
setwd("C:/Users/Adram/OneDrive - Universidad de los Andes/8 OCTAVO SEMESTRE/BDML/Problem-Set-3/stores")

#______________________________________________________________
#________________________BASES_________________________________
#______________________________________________________________

# Cargamos la base de datos de train
db_tr <- read.csv("train.csv") %>% 
  as_tibble()

# Cargamos la base de datos de test
db_ts <- read.csv("test.csv") %>% 
  as_tibble()

# Creamos un identificador para saber de qué base es cada observación
db_tr <- db_tr %>% 
  mutate(train = 1)

db_ts <- db_ts %>% 
  mutate(train = 0)

# Unimos las bases
db <- bind_rows(db_tr, db_ts)

#_____________________________________________________________
#_____________________Manipulación de texto______________________
#_____________________________________________________________

# Concatenar title y description
db$text <- str_to_lower(paste(db$title, db$description, sep = " "))
db <- db %>% 
  select(-title, -description)

##------ Buscamos las palabras más repetidas ------##
palabras <- data.frame(text = db$text) %>% 
  unnest_tokens(word, text) %>% 
  count(word, sort = TRUE)

##------ Buscamos conjunto de dos palabras más repetidas ------##
# Bigramas
db <- db %>%
  mutate(bigramas = tokenize_ngrams(text, n = 2))

# Desanidar todos los bigramas en una sola columna
df_bigramas <- db %>%
  select(bigramas) %>%
  unnest_longer(bigramas)

# Contar frecuencia de cada bigrama
frecuencias_bigramas <- df_bigramas %>%
  count(bigramas, sort = TRUE)

##------ Buscamos conjunto de tres palabras más repetidas ------##
# Trigramas
db <- db %>%
  mutate(trigramas = tokenize_ngrams(text, n = 3))

# Desanidar todos los trigramas en una sola columna
df_trigramas <- db %>%
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

# Área #
sumar_m2 <- function(texto) { # Función para extraer y sumar todos los valores de metros cuadrados
  
  coincidencias <- str_extract_all(
    texto,
    regex("\\b\\d+\\s*(m2|mts2|mts|metros cuadrados)\\b", ignore_case = TRUE)
  )[[1]] # Extrae todas las expresiones con número seguido de unidad de área
  
  numeros <- str_extract(coincidencias, "\\d+")
  numeros <- as.numeric(numeros) # Extrae los números de esas coincidencias
  
  if (length(numeros) == 0) return(NA_real_) else return(sum(numeros, na.rm = TRUE))
} # Suma los valores; si no hay, devuelve NA

db <- db %>% # Aplicar a la base
  mutate(area_m2 = sapply(text, sumar_m2))

# Baños #
contar_banos <- function(texto) { # Función que extrae y suma todas las menciones de baños en el texto
  patrones <- list(
    "5" = "\\b(5|cinco) banos\\b",
    "4" = "\\b(4|cuatro) banos\\b",
    "3" = "\\b(3|tres) banos\\b",
    "2" = "\\b(2|dos) banos\\b",
    "1" = "\\b(1|un|uno) bano\\b"
  )
  
  total <- 0
  for (valor in names(patrones)) {
    coincidencias <- str_count(texto, regex(patrones[[valor]], ignore_case = TRUE))
    total <- total + as.numeric(valor) * coincidencias
  }
  
  if (total == 0) return(NA_real_) else return(total)
}   # Si no encontró nada, devolver NA

db <- db %>% # Aplicar la función sobre la base de datos
  mutate(banos = sapply(text, contar_banos))

# Habitaciones #
sumar_habitaciones <- function(texto) {
  coincidencias <- str_extract_all(
    texto,
    regex("\\b(\\d+|una|un|dos|tres|cuatro|cinco|seis|siete|ocho|nueve|diez|once)\\s+(habitacion(es)?|alcoba(s)?)\\b",
          ignore_case = TRUE)
  )[[1]]
  
  palabras_a_numeros <- c(
    "una" = 1, "un" = 1, "dos" = 2, "tres" = 3, "cuatro" = 4,
    "cinco" = 5, "seis" = 6, "siete" = 7, "ocho" = 8, "nueve" = 9,
    "diez" = 10, "once" = 11
  )
  
  numeros <- str_extract(coincidencias, "\\b(\\d+|una|un|dos|tres|cuatro|cinco|seis|siete|ocho|nueve|diez|once)\\b")
  numeros <- tolower(numeros)
  
  # Traducir palabras a números
  convertidos <- suppressWarnings(as.numeric(numeros))
  convertidos[is.na(convertidos) & numeros %in% names(palabras_a_numeros)] <- palabras_a_numeros[numeros[is.na(convertidos) & numeros %in% names(palabras_a_numeros)]]
  
  if (length(convertidos) == 0) return(NA_real_) else return(sum(convertidos, na.rm = TRUE))
}

db <- db %>%
  mutate(habitaciones = sapply(text, sumar_habitaciones))

# Cocina americana #
db <- db %>%
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

# Cocina integral #
db <- db %>%
  mutate(
    cocina_integral = if_else(
      str_to_lower(str_trim(text)) %in% c("cocina integral", 
                                          "cocina tipo integral"),
      1, 0
    )
  )

# Parqueadero visitantes #
db <- db %>%
  mutate(
    parqueadero_visitantes = if_else(
      str_to_lower(str_trim(text)) %in% c("parqueadero de visitantes", 
                                          "parqueaderos de visitantes",
                                          "parqueadero para visitantes",
                                          "parqueaderos para visitantes"),
      1, 0
    )
  )

# Parqueadero #
db <- db %>%
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

# Lavandería #
db <- db %>%
  mutate(
    lavanderia = if_else(
      str_to_lower(str_trim(text)) %in% c("lavanderia", 
                                          "lavandera"),
      1, 0
    )
  )

# gimnasio #
db <- db %>%
  mutate(
    gimnasio = if_else(
      str_to_lower(str_trim(text)) %in% c("gimnasio", 
                                          "gym"),
      1, 0
    )
  )

# Balcón #
db <- db %>%
  mutate(
    balcon = if_else(
      str_to_lower(str_trim(text)) %in% c("balcon", 
                                          "balcn",
                                          "balcones"),
      1, 0
    )
  )

# Seguridad #
db <- db %>%
  mutate(
    seguridad = if_else(
      str_to_lower(str_trim(text)) %in% c("vigilancia", 
                                          "seguridad"
                                          ),
      1, 0 # Vamos a tomar seguridad y vigilanica como el mímso indicativo
    )
  )

# walking clotet #
db <- db %>%
  mutate(
    walking_closet = if_else(
      str_to_lower(str_trim(text)) %in% c("walking closet", 
                                          "walking closeth",
                                          "walking closets"
      ),
      1, 0 # Vamos a tomar seguridad y vigilanica como el mímso indicativo
    )
  )

# Definir palabras clave y expresiones regulares
keywords <- c("bbq", "terraza(s)?", "deposito", "chimenea(s)", "conjunto",
              "ascensor(es)?", "patio(s)?", "duplex", "piscina", "sauna",
              "jacuzzi", "altillo", "zona(s)? verde(es)?")

patterns <- paste0("\\b(", keywords, ")\\b")

# Crear variables binarias
for (i in seq_along(keywords)) {
  var_name <- paste0("has_", gsub(" ", "_", keywords[i]))
  db[[var_name]] <- as.integer(str_detect(tolower(db$text), regex(patterns[i], ignore_case = TRUE)))
}

#______________________________________________________________________________

# Observar coincidencias

sum(db_tr$bedrooms == db_tr$rooms, na.rm = TRUE)

sum(db_tr$surface_total == db_tr$surface_covered, na.rm = TRUE)

#_______________________________________________________________
#________________________IMPUTACIÓN________________________________#
#_______________________________________________________________

# Reemplazamos los NA de las variables por los datos de texto
db <- db %>%
  mutate(n_banos = coalesce(bathrooms, banos)) %>% 
  select(-banos, -bathrooms)
db <- db %>% 
  mutate(area = coalesce(surface_covered, area_m2)) %>% 
  select(-surface_covered, -surface_total, -area_m2)

# Asumimos que si en la descripción no tiene parqueadero en verdad no tiene
db <- db %>%
  mutate(n_parqueaderos = if_else(is.na(parqueaderos), 0, parqueaderos)) %>% 
  select(-parqueaderos)

# Para la imputación de area vamos a usar el promedio agrupado
# Para eso necesitamos la otra base de datos

datos_unidos <- readRDS("datos_unidos.rds") %>% 
  select(property_id, SCANOMBRE, ESTRATO, CODIGO_UPZ)

db <- db %>%
  left_join(datos_unidos, by = "property_id")

# Imputaremos area agrupando por algunas variables
db <- db %>%
  group_by(SCANOMBRE, bedrooms, n_banos, ESTRATO, property_type) %>%
  mutate(area = if_else(is.na(area), mean(area, na.rm = TRUE), area)) %>%
  ungroup()

db <- db %>% # Para los valores restantes
  group_by(CODIGO_UPZ, bedrooms, n_banos, ESTRATO, property_type) %>%
  mutate(area = if_else(is.na(area), mean(area, na.rm = TRUE), area)) %>%
  ungroup()



# Imputaremos baños agrupando por algunas variables
db <- db %>%
  group_by(SCANOMBRE, bedrooms, ESTRATO, property_type, area) %>%
  mutate(n_banos = if_else(is.na(n_banos), mean(n_banos, na.rm = TRUE), n_banos)) %>%
  ungroup()

db <- db %>% # Para los valores restantes
  group_by(CODIGO_UPZ, bedrooms, ESTRATO, property_type, area) %>%
  mutate(n_banos = if_else(is.na(n_banos), mean(n_banos, na.rm = TRUE), n_banos)) %>%
  ungroup()

# Eliminamos variables inutiles
db <- db %>% 
  select(-habitaciones, -rooms)


#_______________________________________________________________________________

# Observar la cantidad de missing values de cada variable
missing_values<-colSums(is.na(db_ts))
missing_tab<-data.frame(
  Miss_val=missing_values
)
missing_tab
