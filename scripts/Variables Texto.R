# Variables creadas a partir de la descripción ---------------------------------

#Expresiones Regulares (Regex)

# Concatenar title y description
datos$text_combined <- paste(datos$title, datos$description, sep = " ")

# Definir palabras clave y expresiones regulares
keywords <- c("bbq", "balcon(es)?", "walk in closet", 
              "estudio", "garaje(s)?", "jacuzzi", "terraza(s)?",
              "remodelado", "remodelada", "parqueadero(s)?",
              "chimenea(s)?", "lavanderia(s)?", "gimnasio(s)?", "parque(s)?",
              "ascensor", "vigilancia", "patio(s)?", "vestier", "transmilenio",
              "sauna", "comunal(es)?", "duplex", "cctv", "walking closet",
              "cocina", "zona(s)? social(es)?", "sala", "comedor(es)?", "caldera(s)?",
              "seguridad", "bano(s)? privado(s)?", "bano(s)? social(es)?", 
              "calefaccion", "blindado(s)?", "blindada(s)?", "deposito")
patterns <- paste0("\\b(", keywords, ")\\b")

# Crear variables binarias
for (i in seq_along(keywords)) {
  var_name <- paste0("has_", gsub(" ", "_", keywords[i]))
  datos[[var_name]] <- as.integer(str_detect(tolower(datos$text_combined), regex(patterns[i], ignore_case = TRUE)))
}

# Diccionario para palabras de estrato
strato_dict <- c("uno" = 1, "dos" = 2, "tres" = 3, "cuatro" = 4, "cinco" = 5, "seis" = 6)

# Regex para estrato
pattern_estrato <- "(?i)estrato\\s*(\\d|uno|dos|tres|cuatro|cinco|seis)"

# Capturar el estrato
datos$estrato_raw <- str_extract(datos$text_combined, pattern_estrato)
datos$estrato_raw <- str_extract(datos$estrato_raw, "(?i)(\\d|uno|dos|tres|cuatro|cinco|seis)")
datos$estrato <- as.numeric(ifelse(datos$estrato_raw %in% names(strato_dict), strato_dict[datos$estrato_raw], datos$estrato_raw))

# Regex para metros cuadrados
pattern_m2 <- "(?i)(\\d{2,6})\\s*(m2|m²|mts|metros cuadrados)"

# Capturar metros cuadrados
datos$metros_cuadrados <- str_extract(datos$text_combined, pattern_m2)
datos$metros_cuadrados <- as.numeric(str_extract(datos$metros_cuadrados, "\\d{2,6}"))

#Ver missings
sum(is.na(datos$metros_cuadrados))
sum(is.na(datos$estrato))

