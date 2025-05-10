#==================== Agregar variables espaciales Parte 2 =====================


#Alistar ambiente de trabajo ---------------------------------------------------
cat("\014")
rm(list = ls())


##Cargar paquetes---------####
require("pacman")

p_load(tidyverse, #Manejo de datos
       openxlsx, #Abrir archivos xlsx
       rio, #Guardar archivos
       leaflet, #Visualización espacial interactiva
       here, #Definir directorio cuando hay un .Rproj
       osmdata, #Obtener datos de OSM
       sf, #Manejo de datos espaciales
       stargazer #Presentación resultados
)

##Definir directorio----------####

wd <- here()
wd_stores <- paste0(wd, "/stores/")
setwd(wd_stores)
rm(wd_stores)

##Cargar datos---------####

datos <- readRDS("datos_unidos.rds")
localidades <- st_read('loca/LOCA.shp') %>% 
               st_transform(localidades, crs=4326)  
#Quitar a Sumapaz para concentrarnos el área urbana de Bogotá
localidades <- localidades %>% 
               filter(LocNombre != "SUMAPAZ")



#Helper fuctions ---------------------------------------------------------------

obtener_osmdata_v2 <- function(llave, valor){
  
  ### Utilizamoas osm para bogota
  data <- opq(bbox = getbb("Bogotá Colombia")) %>%
    add_osm_feature(key = llave , value = valor)
  
  #Cambios el tipo de objeto
  data<- osmdata_sf(data)
  
  #Dejamos poligonos y name y id
  data <- data$osm_polygons %>% 
      select(osm_id, name)
  
  #Convertimos a tipo de objeto sf
  data<-st_as_sf(data, crs=4326)
  
  #Cambiamos crs
  data<-st_transform(data, crs=4326)
  return(data)
  
}



#Identificar donde esta el CBD de Bogotá ---------------------------------------


##Información utilizada para definir el CBD ---------####
#Usamos dos encuestas para ver si el CBD ha cambiado en el tiempo, teniendo en cuenta que lo datos de los aptos están en 2020
#(i) Encuesta de Movilidad Bogotá 2019
#https://observatorio.movilidadbogota.gov.co/sites/observatorio.movilidadbogota.gov.co/files/2024-06-02/encuesta/Cartilla%20resultados%20Encuesta%20de%20Movilidad%202019.pdf
#(ii) Encuesta de Movilidad Bogotá 2023
#https://observatorio.movilidadbogota.gov.co/sites/observatoriodes.movilidadbogota.gov.co/files/2024-05-29/encuesta/Cartilla%20Encuesta%20de%20Movilidad%202023.pdf


#Conclusión: el CBD está ubicado en las localiades de Chapinero, Santa Fé, Teusaquillo y Mártires que son las que reciben
#            mas viajes a la hora pico por motivo de trabajo 

#Intuición cálcular el CBD: calcular el centro de estás cuatro localiades y usar ese punto para calcular las distancias


##Obtener un punto para el CBD ------####

#Conservar localidades que reciben el mayor flujo de personas en la hora pico (6am-7am) que se desplazan hacia su trabajo 
localidades_cbd <- localidades %>% 
                   filter(LocNombre == "CHAPINERO"| 
                          LocNombre == "TEUSAQUILLO"| 
                          LocNombre == "SANTA FE" | 
                          LocNombre =="LOS MARTIRES") %>% 
                   select(LocNombre, geometry)

#Obtener los centroides de las localidades centroides 
localidades_cbd <- localidades_cbd %>% st_centroid()

#Coordenadas de los centroides
localidades_cbd <- localidades_cbd %>% 
                   mutate(lon = st_coordinates(localidades_cbd)[, "X"]) %>% 
                   mutate(lat = st_coordinates(localidades_cbd)[, "Y"])


#Calcular el CBD como el punto en la mitad de las localidades ya mencionadas
cbd_pt <- data.frame(
  lon = mean(localidades_cbd$lon), 
  lat = mean(localidades_cbd$lat)
)


cbd_pt <- st_as_sf(cbd_pt, coords = c("lon", "lat"), crs = 4326)


#Ver la donde queda ubicado el CBD en el mapa. El CBD si parece estar ubicado en el centro
#de las cuatro localidades consideradas
cbd_plot <- ggplot() + 
  geom_sf(data = localidades) + 
  geom_sf_text(data = localidades, aes(label = LocNombre), size = 3) +
  geom_sf(data = cbd_pt, col = "red")
  

##Crear variable distancia al cbd ------####
datos$cbd_distancia <- st_distance( x = datos, y = cbd_pt)



#Distancia al CAI más cercano -------####
  















