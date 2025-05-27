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
       skimr,
       visdat,
       corrplot,
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
                                          "walking closets", 
                                          "walk in closet",
                                          "walk in closets"
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

#Identificamos que rooms y bedrooms tienen casi todas las variables non-missing
#Iguales. Por ende, removemos rooms y la variable de habitaciones de texto.

db <- db %>% 
  select(-rooms, -habitaciones)

#------------------------------------------------------------------------------
                              #MISSING VALUES#

# Reemplazamos los NA de las variables por los datos de texto
db <- db %>%
  mutate(n_banos = coalesce(bathrooms, banos)) %>% 
  select(-banos, -bathrooms)
db <- db %>% 
  mutate(area = coalesce(surface_covered, area_m2)) %>% 
  select(-surface_covered, -surface_total, -area_m2)

#Verificamos la distribución de missings por variables

db_miss <- skim(db) %>% dplyr::select( skim_variable, n_missing)
Nobs <- nrow(db) #number of observations
Nobs

db_miss<- db_miss %>% mutate(p_missing= n_missing/Nobs)
head(db_miss)

db_miss <- db_miss %>% arrange(-n_missing) ## or arrange(desc(n_missing))

db_miss<- db_miss %>% filter(n_missing!= 0)
head(db_miss, 10)
tail(db_miss, 10)

## Visualización de la estructura de los Missing Values

ggplot(db_miss, aes(x = reorder(skim_variable, +p_missing) , y =  p_missing)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  coord_flip() +
  labs(title = "N Missing values por Variable", x = "Var Nombre", y = "Missings")+ 
  theme(axis.text = element_text(size = 5))  

#Alta concentración de missings en variables relacionadas con:
#Área, baños, parqueaderos, precio

## Crear db con == 1 si missing

db_missing <- db %>% mutate_all(~ifelse(!is.na(.), 1, 0))

## Dropeando variables con ningún missing o todos missing

db_missing <-  db_missing %>%  dplyr::select(which(apply(db_missing, 2, 
                                                         sd) > 0))

#Matriz de correlación para Missing Values

M <- cor(db_missing)
corrplot(M) 

                            #DISTRIBUCIONES#

#Vamos a revisar la distribución de cada variable antes de realizar imputaciones.


#1). Área en Metros Cuadrados (servirá para imputar la variable de surface total)

ggplot(db, aes(area)) +
  geom_histogram(color = "#000000", fill = "#0099F8") +
  geom_vline(xintercept = median(db$area, na.rm = TRUE),
             linetype = "dashed", 
             color = "red") +
  geom_vline(xintercept = mean(db$area, na.rm = TRUE),
             linetype = "dashed",
             color = "blue") +  
  ggtitle("Distribución de Área M2 Antes de Imputar") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))

log_area <- log(db$area) #Se crea variable de área logarítmica 

ggplot(db, aes(log_area)) +
  geom_histogram(color = "#000000", fill = "#0099F8") +
  geom_vline(xintercept = median(db$log_area, na.rm = TRUE),
             linetype = "dashed", 
             color = "red") +
  geom_vline(xintercept = mean(db$log_area, na.rm = TRUE),
             linetype = "dashed",
             color = "blue") +  
  ggtitle("Distribución de Log Área M2 Antes de Imputar") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))

#Asimetría a la derecha. Rojo = Mediana, Azúl = Media. Valores muy grandes
#en la cola derecha.

#2). Baños 

ggplot(db, aes(n_banos)) +
  geom_histogram(color = "#000000", fill = "#0099F8") +
  geom_vline(xintercept = median(db$n_banos, na.rm = TRUE),
             linetype = "dashed", 
             color = "red") +
  geom_vline(xintercept = mean(db$n_banos, na.rm = TRUE),
             linetype = "dashed",
             color = "blue") +  
  ggtitle("Distribución de Baños Antes de Imputar") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))

#Asimetría a la derecha. Rojo = Mediana, Azúl = Media. Valores muy grandes
#en la cola derecha.

#3). Parqueaderos 

ggplot(db, aes(parqueaderos)) +
  geom_histogram(color = "#000000", fill = "#0099F8") +
  geom_vline(xintercept = median(db$parqueaderos, na.rm = TRUE),
             linetype = "dashed", 
             color = "red") +
  geom_vline(xintercept = mean(db$parqueaderos, na.rm = TRUE),
             linetype = "dashed",
             color = "blue") +  
  ggtitle("Distribución de Parqueaderos Antes de Imputar") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))

#Asimetría a la derecha. Rojo = Mediana, Azúl = Media. Valores muy grandes
#en la cola derecha.

#4). Precio 

ggplot(db, aes(price)) +
  geom_histogram(color = "#000000", fill = "#0099F8") +
  geom_vline(xintercept = median(db$price, na.rm = TRUE),
             linetype = "dashed", 
             color = "red") +
  geom_vline(xintercept = mean(db$price, na.rm = TRUE),
             linetype = "dashed",
             color = "blue") +  
  ggtitle("Distribución de Precio $COP Antes de Imputar") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))

#Asimetría a la derecha. Rojo = Mediana, Azúl = Media. Valores muy grandes
#en la cola derecha. Distribución un poco más uniforme.

#Ahora procedemos a imputar los valores faltantes.

#_______________________________________________________________
#________________________IMPUTACIÓN________________________________#
#_______________________________________________________________

datos_unidos <- readRDS("datos_unidos.rds") %>% 
  select(property_id, SCANOMBRE, ESTRATO, CODIGO_UPZ, localidad,  n_localidad)

db <- db %>%
  left_join(datos_unidos, by = "property_id")

# ESTRATO //////////////////////////////////////////////////////////////////////

table(db$ESTRATO) #Distribución antes de imputación

#Antes de imputar, vimos que hay algunas casas con estrato 0

estrato0 <- db %>% 
  filter(ESTRATO == 0) 

print(estrato0)

#Vamos a imputar con la moda (categoría más frecuente) por barrio

# Función para calcular el modo
get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Imputación de ESTRATO con el modo del grupo (cuando es 0 o NA)
db <- db %>%
  group_by(SCANOMBRE, bedrooms, n_banos, property_type, area) %>%
  mutate(ESTRATO = if_else(ESTRATO == 0 | is.na(ESTRATO), 
                           get_mode(ESTRATO[ESTRATO != 0 & !is.na(ESTRATO)]), 
                           ESTRATO)) %>%
  ungroup()

db <- db %>%
  group_by(CODIGO_UPZ, bedrooms, n_banos, property_type, area) %>%
  mutate(ESTRATO = if_else(ESTRATO == 0 | is.na(ESTRATO), 
                           get_mode(ESTRATO[ESTRATO != 0 & !is.na(ESTRATO)]), 
                           ESTRATO)) %>%
  ungroup()

db <- db %>%
  group_by(localidad, bedrooms, n_banos, property_type, area) %>%
  mutate(ESTRATO = if_else(ESTRATO == 0 | is.na(ESTRATO), 
                           get_mode(ESTRATO[ESTRATO != 0 & !is.na(ESTRATO)]), 
                           ESTRATO)) %>%
  ungroup()

db <- db %>%
  group_by(SCANOMBRE, bedrooms, property_type, area) %>%
  mutate(ESTRATO = if_else(ESTRATO == 0 | is.na(ESTRATO), 
                           get_mode(ESTRATO[ESTRATO != 0 & !is.na(ESTRATO)]), 
                           ESTRATO)) %>%
  ungroup()

db <- db %>%
  group_by(CODIGO_UPZ, bedrooms, property_type, area) %>%
  mutate(ESTRATO = if_else(ESTRATO == 0 | is.na(ESTRATO), 
                           get_mode(ESTRATO[ESTRATO != 0 & !is.na(ESTRATO)]), 
                           ESTRATO)) %>%
  ungroup()

db <- db %>%
  group_by(localidad, bedrooms, property_type, area) %>%
  mutate(ESTRATO = if_else(ESTRATO == 0 | is.na(ESTRATO), 
                           get_mode(ESTRATO[ESTRATO != 0 & !is.na(ESTRATO)]), 
                           ESTRATO)) %>%
  ungroup()

db <- db %>%
  group_by(SCANOMBRE, bedrooms, property_type) %>%
  mutate(ESTRATO = if_else(ESTRATO == 0 | is.na(ESTRATO), 
                           get_mode(ESTRATO[ESTRATO != 0 & !is.na(ESTRATO)]), 
                           ESTRATO)) %>%
  ungroup()

db <- db %>%
  group_by(CODIGO_UPZ, bedrooms, property_type) %>%
  mutate(ESTRATO = if_else(ESTRATO == 0 | is.na(ESTRATO), 
                           get_mode(ESTRATO[ESTRATO != 0 & !is.na(ESTRATO)]), 
                           ESTRATO)) %>%
  ungroup()

db <- db %>%
  group_by(localidad, bedrooms, property_type) %>%
  mutate(ESTRATO = if_else(ESTRATO == 0 | is.na(ESTRATO), 
                           get_mode(ESTRATO[ESTRATO != 0 & !is.na(ESTRATO)]), 
                           ESTRATO)) %>%
  ungroup()

# Verificar missings en test
missing_estrato <- db %>% 
  filter(is.na(ESTRATO) & train == 0) 

print(missing_estrato)


# Verificar missings en train
missing_estrato2 <- db %>% 
  filter(is.na(ESTRATO) & train == 1) 

print(missing_estrato2)

#Eliminar obs con NAs faltantes en train (2)

db <- db %>% 
  filter(!(is.na(ESTRATO) & train == 1))


# ÁREA ///////////////////////////////////////////////////////////////////////

# Asumimos que si en la descripción no tiene parqueadero en verdad no tiene
db <- db %>%
  mutate(n_parqueaderos = if_else(is.na(parqueaderos), 0, parqueaderos)) %>% 
  select(-parqueaderos)

# Imputaremos area agrupando por algunas variables. 
# Se imputa con la mediana por la asimetría preexistente en la distribución.
db <- db %>%
  group_by(SCANOMBRE, bedrooms, n_banos, ESTRATO, property_type) %>%
  mutate(area = if_else(is.na(area), median(area, na.rm = TRUE), area)) %>%
  ungroup()

db <- db %>% # Para los valores restantes
  group_by(CODIGO_UPZ, bedrooms, n_banos, ESTRATO, property_type) %>%
  mutate(area = if_else(is.na(area), median(area, na.rm = TRUE), area)) %>%
  ungroup()

#Todavía quedan valores faltantes para área. Para lidiar con esto, vamos a ver
#Cómo se distribuyen los missings por ahora. 
#Corriendo el comando de la matriz de correlación, no se observaciones con 
#missings altamente correlacionados con los de área. Entonces, se procederá
#A imputar a partir de localidad en lugar de UPZ para los faltantes.


db <- db %>%
  group_by(localidad, bedrooms, n_banos, ESTRATO, property_type) %>%
  mutate(area = if_else(is.na(area), median(area, na.rm = TRUE), area)) %>%
  ungroup()

#Ahora quedan menos, pero vamos a dejar de filtrar por baños y habitaciones.

db <- db %>%
  group_by(n_localidad, bedrooms, ESTRATO, property_type) %>%
  mutate(area = if_else(is.na(area), median(area, na.rm = TRUE), area)) %>%
  ungroup()

db <- db %>%
  group_by(n_localidad, ESTRATO, property_type) %>%
  mutate(area = if_else(is.na(area), median(area, na.rm = TRUE), area)) %>%
  ungroup()

# Verificar missings en test
missing_area_obs <- db %>% 
  filter(is.na(area) & train == 0) 

print(missing_area_obs)

# Verificar missings en train
missing_area_obs2 <- db %>% 
  filter(is.na(area) & train == 1) 

print(missing_area_obs2)

#Se eliminan las obs faltantes en train (12)

# Eliminar las observaciones donde 'area' es NA y 'train' es 1
db <- db %>% 
  filter(!(is.na(area) & train == 1))

# Verificar el resultado
print(dim(db))  # Verificar el nuevo tamaño del dataframe

#Verificamos missings para el conjunto de testeo

missing_area <- sum(is.na(db$area) & db$train == 0)
print(missing_area)


# BAÑOS ///////////////////////////////////////////////////////////////////////

# Imputaremos baños agrupando por algunas variables

db <- db %>% # Para agrupar por area en grupos de 10
  mutate(grupo_area = floor(area / 10))

db <- db %>%
  group_by(SCANOMBRE, bedrooms, ESTRATO, property_type, area) %>%
  mutate(n_banos = if_else(is.na(n_banos), median(n_banos, na.rm = TRUE), n_banos)) %>%
  ungroup()

db <- db %>% # Para los valores restantes
  group_by(CODIGO_UPZ, bedrooms, ESTRATO, property_type, grupo_area) %>%
  mutate(n_banos = if_else(is.na(n_banos), median(n_banos, na.rm = TRUE), n_banos)) %>%
  ungroup()

db <- db %>% # Para los valores restantes
  group_by(localidad, bedrooms, ESTRATO, property_type, grupo_area) %>%
  mutate(n_banos = if_else(is.na(n_banos), median(n_banos, na.rm = TRUE), n_banos)) %>%
  ungroup()

# Verificar missings en test
missing_bano_obs <- db %>% 
  filter(is.na(n_banos) & train == 0) 

print(missing_bano_obs)

# Verificar missings en train
missing_bano_obs2 <- db %>% 
  filter(is.na(n_banos) & train == 1) 

print(missing_bano_obs2)

#Valores restantes

db <- db %>% # Para los valores restantes
  group_by(SCANOMBRE, bedrooms, ESTRATO, property_type) %>%
  mutate(n_banos = if_else(is.na(n_banos), median(n_banos, na.rm = TRUE), n_banos)) %>%
  ungroup()

db <- db %>% # Para los valores restantes
  group_by(CODIGO_UPZ, bedrooms, ESTRATO, property_type) %>%
  mutate(n_banos = if_else(is.na(n_banos), median(n_banos, na.rm = TRUE), n_banos)) %>%
  ungroup()

db <- db %>% # Para los valores restantes
  group_by(SCANOMBRE, ESTRATO, property_type) %>%
  mutate(n_banos = if_else(is.na(n_banos), median(n_banos, na.rm = TRUE), n_banos)) %>%
  ungroup()

db <- db %>% # Para los valores restantes
  group_by(CODIGO_UPZ, ESTRATO, property_type) %>%
  mutate(n_banos = if_else(is.na(n_banos), median(n_banos, na.rm = TRUE), n_banos)) %>%
  ungroup()

#Remover observaciones en train con valores faltantes

db <- db %>% 
  filter(!(is.na(n_banos) & train == 1))

# UPZ ///////////////////////////////////////////////////////////////////////

#Vamos a imputar UPZ por su categoría más frecuente para cada barrio

upz_barrio <- db %>%
  group_by(SCANOMBRE) %>%
  summarise(Codigo_UPZ_ref = get_mode(CODIGO_UPZ[!is.na(CODIGO_UPZ)])) %>%
  ungroup()

# Asignar el Código UPZ usando el valor más frecuente por barrio
db <- db %>%
  left_join(upz_barrio, by = "SCANOMBRE") %>%
  mutate(CODIGO_UPZ = if_else(is.na(CODIGO_UPZ), Codigo_UPZ_ref, CODIGO_UPZ)) %>%
  select(-Codigo_UPZ_ref)  # Eliminar la columna auxiliar

# Verificar missings en test
missing_UPZ <- db %>% 
  filter(is.na(CODIGO_UPZ) & train == 0) 

print(missing_UPZ)

# Verificar missings en train
missing_UPZ2 <- db %>% 
  filter(is.na(CODIGO_UPZ) & train == 1) 

print(missing_UPZ2)

#Imputación Manual

db <- db %>%
  mutate(CODIGO_UPZ = case_when(
    SCANOMBRE == "SIBERIA II" ~ "8222",
    SCANOMBRE == "PARAMO I" ~ "99",
    TRUE ~ CODIGO_UPZ  # Mantener los valores existentes
  ))

#Remover variables en train con UPZ faltante

db <- db %>% 
  filter(!(is.na(CODIGO_UPZ) & train == 1))

#Remover variables en train con SCANOMBRE faltante

db <- db %>% 
  filter(!(is.na(SCANOMBRE) & train == 1))

#_______________________________________________________________________________

# Observar la cantidad de missing values de cada variable
missing_values<-colSums(is.na(db))
missing_tab<-data.frame(
  Miss_val=missing_values
)
missing_tab

#------------------------------------------------------------------------------

                              #OUTLIERS#

#Chequeo de cuantiles

#ÁREA //////////////////////////////////////////////////////////////////////////

log_area <- log(db$area)

ggplot(db, aes(y = log_area)) +
  geom_boxplot(fill = "#0099F8", color = "black") +
  ggtitle("Boxplot de Log Area M2") +
  theme_classic()


print(paste("Max antes de winsorizar:", max(db$area, na.rm = TRUE)))
print(paste("Min antes de winsorizar:", min(db$area, na.rm = TRUE)))

print(quantile(db$area, c(0.05, 0.95), na.rm = TRUE))

# Winsorizar en el percentil 97.5
up_threshold_area <- quantile(db$area, 0.95, na.rm = TRUE)  
down_threshold_area <- quantile(db$area, 0.05, na.rm = TRUE)

# Rounding

db$area <- ifelse(db$area > up_threshold_area,
                                up_threshold_area, db$area)
db$area <- ifelse(db$area < down_threshold_area,
                  down_threshold_area, db$area)

print(paste("Max después de winsorizar:", max(db$area, na.rm = TRUE)))
print(paste("Min después de winsorizar:", min(db$area, na.rm = TRUE)))

# Verificación

summary(db$area)

ggplot(db, aes(log_area)) +
  geom_histogram(color = "#000000", fill = "#0099F8") +
  geom_vline(xintercept = median(db$log_area, na.rm = TRUE),
             linetype = "dashed", 
             color = "red") +
  geom_vline(xintercept = mean(db$log_area, na.rm = TRUE),
             linetype = "dashed",
             color = "blue") +  
  ggtitle("Distribución de Log Área M2 Después de Imputar") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))

#BAÑOS //////////////////////////////////////////////////////////////////////////

# Redondear y convertir a entero
db <- db %>%
  mutate(n_banos = round(as.numeric(n_banos)))

# Verificar el resultado
print(head(db$n_banos))


ggplot(db, aes(y = n_banos)) +
  geom_boxplot(fill = "#0099F8", color = "black") +
  ggtitle("Boxplot de Baños Después de Imputar") +
  theme_classic()

#Hay pocos valores atípicos principalmente por encima del boxplot. Se corrige con menor penalización.

print(paste("Max antes de winsorizar:", max(db$n_banos, na.rm = TRUE)))



# Winsorizar en el percentil 97.5
up_threshold_banos <- quantile(db$area, 0.975, na.rm = TRUE)  


# Rounding

db$n_banos <- ifelse(db$n_banos > up_threshold_banos,
                  up_threshold_banos, db$n_banos)

print(paste("Max después de winsorizar:", max(db$n_banos, na.rm = TRUE)))

ggplot(db, aes(n_banos)) +
  geom_histogram(color = "#000000", fill = "#0099F8") +
  geom_vline(xintercept = median(db$n_banos, na.rm = TRUE),
             linetype = "dashed", 
             color = "red") +
  geom_vline(xintercept = mean(db$n_banos, na.rm = TRUE),
             linetype = "dashed",
             color = "blue") +  
  ggtitle("Distribución de Baños Después de Imputar") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))

#PARQUEADEROS //////////////////////////////////////////////////////////////////

print(head(db$n_parqueaderos))

ggplot(db, aes(y = n_parqueaderos)) +
  geom_boxplot(fill = "#0099F8", color = "black") +
  ggtitle("Parqueaderos Después de Imputar") +
  theme_classic()

#Se observan extremadamente pocos valores atípicos, no se imputa.

ggplot(db, aes(n_parqueaderos)) +
  geom_histogram(color = "#000000", fill = "#0099F8") +
  geom_vline(xintercept = median(db$n_parqueaderos, na.rm = TRUE),
             linetype = "dashed", 
             color = "red") +
  geom_vline(xintercept = mean(db$n_parqueaderos, na.rm = TRUE),
             linetype = "dashed",
             color = "blue") +  
  ggtitle("Distribución de Parqueaderos Después de Imputar") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))

#PRECIO ///////////////////////////////////////////////////////////////////////

print(head(db$price))

ggplot(db, aes(y = price)) +
  geom_boxplot(fill = "#0099F8", color = "black") +
  ggtitle("Precio (No se imputó)") +
  theme_classic()


print(paste("Max antes de winsorizar:", max(db$price, na.rm = TRUE)))


print(quantile(db$price, c(0.05, 0.95), na.rm = TRUE))

# Winsorizar en el percentil 97.5

up_threshold_price  <- quantile(db$price, 0.95, na.rm = TRUE)  


# Rounding

db$price <- ifelse(db$price > up_threshold_price,
                     up_threshold_price, db$price)

print(paste("Max después de winsorizar:", max(db$price, na.rm = TRUE)))

ggplot(db, aes(price)) +
  geom_histogram(color = "#000000", fill = "#0099F8") +
  geom_vline(xintercept = median(db$price, na.rm = TRUE),
             linetype = "dashed", 
             color = "red") +
  geom_vline(xintercept = mean(db$price, na.rm = TRUE),
             linetype = "dashed",
             color = "blue") +  
  ggtitle("Distribución de Precio Después de Imputar") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))

#Exportar datos ----------------------------------------------------------------
export(db, 'base_limpia.rds')



