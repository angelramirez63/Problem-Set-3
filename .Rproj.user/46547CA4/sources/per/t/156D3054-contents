#Problem Set 3 -----------------------------------------------------------------
rm(list = ls())

# Preparación espacio de trabajo -----------------------------------------------

## Librerias--------------------------------------------------------------------

rm(list = ls())

if(!require(pacman)) install.packages("pacman") ; require(pacman)

remotes::install_cran("httr2") 
install.packages("httr2", type = "binary")

p_load(tidyverse, openxlsx, stringi, rio, stringi, leaflet, here,
       osmdata, sf, httr2, stargazer, caret, MLmetrics, ggspatial, ranger)

## Directorio ------------------------------------------------------------------
wd <- here()
wd_stores <- paste0(wd, "/stores/")
setwd(wd_stores)
rm(wd_stores)

#Creación de funciones ---------------------------------------------------------

#Función para obtener los datos que requerimos de OpenStreetMap
obtener_osmdata <- function(llave, valor, tipo_dato){
  
  ### Utilizamoas osm para bogota
  data <- opq(bbox = getbb("Bogotá Colombia")) %>%
    add_osm_feature(key = llave , value = valor)
  
  #Cambios el tipo de objeto
  data<- osmdata_sf(data)
  
  #Dejamos poligonos y name y id
  
  if (tipo_dato=='linea'){
    data <- data$osm_lines %>% 
      select(osm_id, name) 
  }
  
  else if (tipo_dato=='puntos'){
    data <- data$osm_points %>% 
      select(osm_id, name) 
  }
  
  else if (tipo_dato=='poligono'){
    data <- data$osm_polygons %>% 
      select(osm_id, name) 
  }
  
  # Convertimos a tipo de objeto sf
  data<-st_as_sf(data, crs=4326)
  
  #Cambiamos crs
  data<-st_transform(data, crs=4326)
  return(data)
  
}

#Función para hallar distancia mínima
distneastfeat<-function(data_original, data_feat, n_variable, tipo_dato){
  
  ##Calculamos centroides si poligono
  if (tipo_dato=='poligono'){
    data_feat <- st_centroid(data_feat, byid = T)
  }
  else{
    NA
  }
  ##Calculamos distancias
  dist_matrix <- st_distance(x = data_original, y = data_feat)
  
  #Distancia minima
  dist_min <- apply(dist_matrix, 1, min)  
  data_original[,n_variable]<-dist_min 
  return(data_original)
}

#Para traer más variables
distneastfeatvar<- function(data_original, data_feat, 
                          n_variable_distancia, 
                          tipo_dato = "punto", 
                          variables_adicionales = NULL,
                          sufijo_adicional = "_cercano") {
  
  # Calcular centroides si data_feat es polígono
  if (tipo_dato == "poligono") {
    data_feat <- st_centroid(data_feat, byid = TRUE)
  }
  
  # Calcular matriz de distancias
  dist_matrix <- st_distance(data_original, data_feat)
  
  # Obtener distancia mínima y su posición
  dist_min <- apply(dist_matrix, 1, min)
  idxs_mas_cercanos <- apply(dist_matrix, 1, which.min)
  
  # Asignar distancia mínima
  data_original[[n_variable_distancia]] <- dist_min
  
  # Si se piden variables adicionales, asignarlas también
  if (!is.null(variables_adicionales)) {
    if (!all(variables_adicionales %in% names(data_feat))) {
      stop("Alguna de las variables adicionales no está en 'data_feat'.")
    }
    
    for (var in variables_adicionales) {
      nuevo_nombre <- paste0(var, sufijo_adicional)
      data_original[[nuevo_nombre]] <- data_feat[[var]][idxs_mas_cercanos]
    }
  }
  
  return(data_original)
}

#Halla distancias de forma iterativa - esta ocupa menos RAM
nearestpointloop <- function(base_objetivo, base_fuente, variables_fuente, sufijo = "_cercano") {
  # Validaciones
  if (!inherits(base_objetivo, "sf") || !inherits(base_fuente, "sf")) {
    stop("Ambas bases deben ser objetos 'sf'.")
  }
  
  if (!all(variables_fuente %in% names(base_fuente))) {
    stop("Algunas variables no existen en la base fuente.")
  }
  
  # Crear matriz de distancias (filas = base_objetivo, columnas = base_fuente)
  distancias <- st_distance(base_objetivo, base_fuente)  # matriz [nrow(objetivo) x nrow(fuente)]
  
  # Para cada fila de base_objetivo, encontrar el índice del punto más cercano en base_fuente
  idxs_mas_cercanos <- apply(distancias, 1, which.min)
  
  # Para cada variable solicitada, pegarla desde el punto más cercano
  for (var in variables_fuente) {
    nuevo_nombre <- paste0(var, sufijo)
    base_objetivo[[nuevo_nombre]] <- base_fuente[[var]][idxs_mas_cercanos]
  }
  
  return(base_objetivo)
}

# Cargar datos -----------------------------------------------------------------

## Datos de apartamentos -------------------------------------------------------
train <- read.csv("train.csv") 
test <- read.csv("test.csv") 
datos <- rbind(test, train)
datos <- st_as_sf(datos, coords = c("lon", "lat"),crs=4326)

## Geografía de Bogotá ---------------------------------------------------------

# Localidades
localidades <- st_read('loca/LOCA.shp')
localidades <- localidades %>% select(LocNombre)
localidades <- st_transform(localidades, crs=4326)
datos <- st_join(datos, localidades, join = st_within)

# Sectores
sector <- st_read('sector/SECTOR.shp')
sector <- st_transform(sector, crs=4326)
sector <-sector %>% select(SCANOMBRE)
sector <- st_make_valid(sector)
datos <- st_join(datos, sector, join = st_within)


## Datos Abiertos Bogotá -------------------------------------------------------

#Importamos los datos
indicador_loc <- st_read("IndicadorLocalidad/IndicadorLoc.shp")
indicador_upz <- st_read("esp_pub_efe_upz/EPE_UPZ.shp")
luminarias_upz <- st_read("luminarias_upz/Luminarias_UPZ.shp")
seguridad_nocturna <- st_read("puntosseguridadnocturna/PuntosSeguridadNocturna.shp")
bibliotecas <- st_read("redbibliotecacademica/RedBibliotecAcademica.shp")
museo <- st_read("museos/Museo.shp")
colegios <- st_read("Colegios/Colegios03_2024.shp")
recaudo_predial <- st_read("rpredial/RPREDIAL.shp")

#Homogenizar datos geográficos
datasets <- c("colegios", "indicador_loc", "indicador_upz", "luminarias_upz",
              "seguridad_nocturna", "bibliotecas", "museo","recaudo_predial")

for (data_name in datasets) {
  # Obtener la base de datos por su nombre
  data <- get(data_name)
  
  # Aplicar las transformaciones
  data <- data %>%
    st_transform(crs = 4326) %>%
    st_make_valid()
  
  # Reasignar el objeto transformado al nombre original
  assign(data_name, data)
}

#Unimos los datos de Datos Abiertos a los aptos

#Indicador de espacio público por localidad 
indicador_loc <- indicador_loc %>% select(EPE_2017, EPT_2017,
                                          EPV_2017, geometry)
datos <- st_join(datos, indicador_loc, join = st_within)

#Indicador de espacio público por UPZ
indicador_upz <- indicador_upz %>% select(EPE, geometry, 
                                          CODIGO_UPZ, NOMBRE)  %>%
  rename(EPE_UPZ = EPE, NOMBRE_UPZ = NOMBRE)
datos <- st_join(datos, indicador_upz, join = st_within)

# Luminarias por upz
luminarias_upz <- luminarias_upz %>% 
  select(geometry, TOTAL) %>%
  rename(luminarias = TOTAL) 
datos <- st_join(datos, luminarias_upz, join = st_within)

#Bibliotecas
datos <- distneastfeat(datos, bibliotecas, 'distnearestlibrary', 'puntos')

#Colegios 
datos <- distneastfeat(datos, colegios, 'distnearestschool', 'puntos')

#Museos
datos <- distneastfeat(datos, museo, 'distnearestmuseum', 'puntos')

#Recaudo predial
recaudo_predial <- recaudo_predial %>% 
  select(geometry, Sum_VALOR_) %>%
  rename(recaudo_predial = Sum_VALOR_)

datos <- st_join(datos, recaudo_predial, join = st_within)

## Datos OpenStreetMap ---------------------------------------------------------

#Revisamos datos disponibles de OSM
available_features()
available_tags("shop")

#Aplicamos 'obtener_osmdata' para obtener los parques y centros comerciales de Bogotá
parques<- obtener_osmdata('leisure', 'park', 'poligono')
malls <- obtener_osmdata('shop', 'mall', 'poligono')
malls <- st_make_valid(malls)

#Aplicamos 'distneastfeat' para hallar la distancia mínima
datos <- distneastfeatvar(datos, parques, 'distnearestpark', 'poligono')
datos <- distneastfeatvar(datos, malls, 'distnearestmall', 'poligono')

export(datos, 'datos_unidos.rds')