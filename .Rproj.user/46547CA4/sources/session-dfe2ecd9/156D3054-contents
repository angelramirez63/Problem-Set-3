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

#Funcion para distancia minima
distminpoints<-function(data_orginial,ext_points){
  
  minimos<-c()
  
  for (i in 1:nrow(data_orginial)){
    
    vector_dist<-st_distance(data_orginial[i,],ext_points)
    min<-min(vector_dist)
    
    minimos<-c(minimos,min)
    print(minimos)
    
    rm(vector_dist)
    
  }
  
  return(minimos)
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
manzanas_estr<-st_read("manz_estrato/ManzanaEstratificacion.shp")
sitp<- st_read("psitp/PSITP.shp")
delitos<-st_read("dai/DAILoc.shp")
restbar<-st_read("egba/EGBA.shp")
cicloruta<-st_read("ciclorruta/ciclorruta.shp")
manzanas_aval<-st_read("avaluo_manz/Avaluo_Manzana.shp")
manzanas_area_res <- st_read("AconstruidaResidencialSHP/AconstruidaResidencial.shp")
manzanas_area_com <- st_read("AconstruidaComercialSHP/AconstruidaComercial.shp")

# Estaciones de transmilenio
transmi <- st_read('https://gis.transmilenio.gov.co/arcgis/services/Troncal/consulta_estaciones_troncales/MapServer/WFSServer?request=GetCapabilities&service=WFS')
transmi<- as.data.frame(transmi) %>% dplyr::select(-Shape)
transmi <- st_as_sf(transmi, coords = c("LONGITUD", "LATITUD"),
                    crs = 4326) %>% st_make_valid()

#Homogenizar datos geográficos
datasets <- c("colegios", "indicador_loc", "indicador_upz", "luminarias_upz",
              "seguridad_nocturna", "bibliotecas", "museo","recaudo_predial", 
              "manzanas_estr", "sitp", "delitos", "restbar", "cicloruta", 
              "manzanas_aval")

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

#Transmi
datos <- distneastfeat(datos, transmi, 'distnearesttransmi', 'puntos')


#Recaudo predial
recaudo_predial <- recaudo_predial %>% 
  select(geometry, Sum_VALOR_) %>%
  rename(recaudo_predial = Sum_VALOR_)

datos <- st_join(datos, recaudo_predial, join = st_within)

#Estrato
manzanas_estr <- manzanas_estr %>% 
  select(geometry, ESTRATO)
sf_use_s2(FALSE) ### Linea necesaria pára correr el codigo
st_nearest_feature(datos, manzanas_estr)
datos <- st_join(datos, manzanas_estr, join = st_nearest_feature)

#Avaluo manzana
manzanas_aval <- manzanas_aval %>% 
  select(geometry, AVALUO_COM, AVALUO_CAT, GRUPOP_TER) %>%
  rename(aval_comer_manz = AVALUO_COM, aval_catas_manz = AVALUO_CAT,
         grupo_econom_manz = GRUPOP_TER)
sf_use_s2(FALSE) ### Linea necesaria pára correr el codigo
st_nearest_feature(datos, manzanas_aval)
datos <- st_join(datos, manzanas_aval, join = st_nearest_feature)

#SITP
datos <-distneastfeat(datos, sitp, 'distnearestsitp','punto')

#Delitos
remove <- grep("TOT|VAR|22|18|24|23", names(delitos))
delitos <- delitos[, -remove]

#Renombrar variables
nombres_anteriores <- c('CMLP', 'CMHP', 'CMHR', 'CMHA', 'CMHB', 'CMHCE', 'CMHM', 'CMDS', 'CMVI')
reemplazos <- c("lesionesperson", "hurtopersonas", 'hurtosresidencias', 'hurtosautos', 'hurtosbicis',  'hurtoscel', 'hurtosmotos', 'delitos_sexual', 'violencia_intra')
nombres_n <- str_replace_all(names(delitos), setNames(reemplazos, nombres_anteriores))
nombres_n <- gsub("CMHC", "hurtoscomercio", nombres_n) 
nombres_n <- gsub("CMH", "homicidios", nombres_n)   
colnames(delitos)<-nombres_n

#Promedio de delitos para los año de venta de  las proepiedades 2019, 2020, 2022as
delitos<-as.data.frame(delitos)

delitos <- delitos %>%
  mutate(n_homicidios = rowMeans(dplyr::select(., starts_with("homicidios"))),
         n_lesiones = rowMeans(dplyr::select(., starts_with("lesionesperson"))),
         n_hurtopersonas = rowMeans(dplyr::select(., starts_with("hurtopersonas"))),
         n_hurtosautos=rowMeans(dplyr::select(., starts_with("hurtosautos"))),
         n_hurtosresidencias=rowMeans(dplyr::select(., starts_with("hurtosresidencias"))),
         n_hurtosbicis=rowMeans(dplyr::select(., starts_with("hurtosbicis"))),
         n_hurtosmotos=rowMeans(dplyr::select(., starts_with("hurtosmotos"))),
         n_hurtoscomercio=rowMeans(dplyr::select(., starts_with("hurtoscomercio"))),
         n_hurtoscelular=rowMeans(dplyr::select(., starts_with("hurtoscel"))),
         n_delitossexales=rowMeans(dplyr::select(., starts_with("delitos_sexual"))),
         n_violenciaintra=rowMeans(dplyr::select(., starts_with("violencia_intra"))))

# Nos quedamos con los promedios e identificador (localidad) y Quitamos las tildes a la variable localidad
delitos <-delitos %>% rename('n_localidad'='CMIULOCAL','localidad'='CMNOMLOCAL')  %>%
  dplyr::select(localidad,starts_with("n_"))  %>% 
  mutate(localidad=chartr("áéíóúÁÉÍÓÚ", "aeiouAEIOU", localidad)) %>%
  mutate(localidad=tolower(localidad))

# Unimos los delitos promedio a los datos
datos <- datos %>% rename('localidad'='LocNombre')  %>%
  mutate(localidad=tolower(localidad)) # Cambiamos nombre de localidad para pegar variables
datos <- left_join(x=datos, y=delitos, by='localidad')



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