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
       stargazer, #Presentación resultados
       geojsonR #Leer archivos geojson 
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

#Obtener datos de OSM
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


#Distancia mínima a la amenidad 
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


#Identificar donde esta el CBD de Bogotá ---------------------------------------


##Información utilizada para definir el CBD ---------####
#Usamos dos encuestas para ver si el CBD ha cambiado en el tiempo, teniendo en cuenta que lo datos de los aptos están entre 2020 y 2021
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



#Distancia al CAI más cercano --------------------------------------------------

##Datos puntos cai------------####
  
#Fuente de los datos 
##https://datosabiertos.bogota.gov.co/dataset/centro-de-atencion-accion-para-bogota-d-c

#Cargar datos
puntos_cai <- st_read("comandoatencioninmediata")
puntos_cai <- puntos_cai %>% 
              select(CAINOMBRE, CAILONGITU, CAILATITUD, geometry)
              
puntos_cai <- st_transform(puntos_cai, crs = 4326)

##Crear variables distancia al CAI mas cercano------------####
datos <- distneastfeat(datos, puntos_cai, 'distnearestCAI', 'puntos')



#Amenidades de OSM -------------------------------------------------------------

#Wiki de features: https://wiki.openstreetmap.org/wiki/Map_features#Office

##Hospitales-----------####

#Obtener la información de OSM
hospitales <- obtener_osmdata_v2('amenity', 'hospital')

#Crear variable distancia al hospital mas cercano 
datos <- distneastfeat(datos, hospitales, "distnearestHospital", "poligono")


##Gimnasios---------####

#Obtener la información de OSM
gyms <- obtener_osmdata_v2('leisure', 'fitness_centre')

#Crear variable distancia al gimnasio mas cercano 
datos <- distneastfeat(datos, gyms, "distnearestGym", "poligono")


##Super Mercados----------####

#Obtener la información de OSM
mini_market <- obtener_osmdata_v2('shop', 'convenience')


#Crear variable distancia a la tienda de barrio mas cercana
datos <- distneastfeat(datos, mini_market, "distnearestConvenienceStore", "poligono")


##Farmacias---------####
farmacias <- obtener_osmdata_v2('amenity', 'pharmacy')
datos <- distneastfeat(datos, farmacias, "distnearestPharmacy", "poligono")





#Arbolado en Bogotá ------------------------------------------------------------

##Datos arbolado en Bogotá-----------#####

#Fuente de los datos
#https://datosabiertos.bogota.gov.co/dataset/censo-arbolado-urbano

#Descripcíon de la base
#Conjunto de plantas de las especies correspondientes a los biotipos árbol, arbusto, 
#palma o helecho arborescente, ubicados en suelo urbano de la ciudad de Bogotá D.C. 
#(es una medida de la ubicación de la naturaleza en la ciudad y la naturaleza es una 
#amenidad)

#Cargar datos
arbolado <- st_read("arboaldo_urbano")
arbolado <- st_transform(arbolado, crs = 4326)


##Agrupar base a nivel arboles por UPZ------------####

arbolado <- arbolado %>% st_drop_geometry() #Vamos a usar el código de la upz para unir las bases 

arbolado <- arbolado %>%  
            filter(!is.na(Codigo_UPZ)) #Remover árboles que no están ubicados en ninguna upz. Son 18

arbolado <- arbolado %>% 
            mutate(cod_upz_int = substring(Codigo_UPZ, 4, length(Codigo_UPZ))) #Conservar solo el número. El código esta en formato UPZ_número

arbolado_x_upz <- arbolado %>% 
                  group_by(cod_upz_int) %>% 
                  summarise(n_arboles_upz = n()) #Cada observación es un árbol distinto

arbolado_x_upz <- arbolado_x_upz %>% 
                  rename(CODIGO_UPZ = cod_upz_int)


##Agregar variable árboles por upz-------------####
datos <- left_join(datos, arbolado_x_upz, by = "CODIGO_UPZ")  #Quedan 55 missings values correspondientes a los apartamentos para los que no tenemos upz


#Exportar datos ----------------------------------------------------------------
export(datos, 'datos_unidos.rds')









