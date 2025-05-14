#Problem Set 3 -----------------------------------------------------------------
rm(list = ls())

# Preparación espacio de trabajo -----------------------------------------------

## Librerias--------------------------------------------------------------------

if(!require(pacman)) install.packages("pacman") ; require(pacman)

p_load(tidyverse, stringi, rio, 
       leaflet, here, osmdata, sf, 
       ggplot2, ggspatial, leaflet)

## Directorio ------------------------------------------------------------------
wd <- here()
wd_stores <- paste0(wd, "/stores/")
setwd(wd)
rm(wd_stores)

# Cargar datos -----------------------------------------------------------------
datos <- readRDS("stores/datos_unidos.rds")
test <- datos %>% filter(is.na(price))
train <- datos %>% filter(!is.na(price))

# Mapas ------------------------------------------------------------------------

## Manipulación de datos -------------------------------------------------------
# Importamos limites de bogota (localidades), cambiamos sistema de referencia. 
bogota_lim <- st_read('stores/loca/LOCA.shp')
bogota_lim<- bogota_lim  %>% st_transform(crs=4326)

# Sectores
sector <- st_read('stores/sector/SECTOR.shp')
sector <- sector %>% 
  st_transform(crs=4326) %>%
  st_make_valid(sector)
sector <-sector %>% select(SCANOMBRE)

# 4.2 Obtneción de las vias de bogota para fines esteticos del mapa ------

#Vías principales
vias_principales <- opq(bbox=getbb('Bogotá Colombia')) %>% 
  add_osm_feature(key='highway', value='primary') %>%
  osmdata_sf()
vias_principales <- vias_principales$osm_lines

#Avenidas
avenidas <- opq(bbox=getbb('Bogotá Colombia')) %>% 
  add_osm_feature(key='highway', value='trunk') %>%
  osmdata_sf()
avenidas <- avenidas$osm_lines

#Vías secundarias
vias_secundarias <- opq(bbox=getbb('Bogotá Colombia')) %>% 
  add_osm_feature(key='highway', value='secondary') %>%
  osmdata_sf()
vias_secundarias <- vias_secundarias$osm_lines

## Mapa distribución geografica por precio -------------------------------------

# Dividimos el precio en quintiles
train <- train %>%
  mutate(precio_millones = price/1000000, 
         precio_quintiles = cut(precio_millones, 
                                breaks = quantile(precio_millones, probs = seq(0, 1, 0.2), na.rm = TRUE), 
                                include.lowest = TRUE))

# Obtenemos intervalos de los quintiles y los convertimos a string (manualmente)
intervalos <- levels(train$precio_quintiles)
intervalos <- c('300-390', '390-490', '490-650', '650-890', '890-1650')

#Codigo del mapa
map1 <- ggplot() +
  geom_sf(data = bogota_lim, fill = NA, size = 1, color = 'cornflowerblue') +
  geom_sf(data = vias_principales, fill = NA, size = 0.6, col = 'gray50') +
  geom_sf(data = avenidas, fill = NA, size = 0.6, col = 'gray50') +
  geom_sf(data = vias_secundarias, fill = NA, size = 0.6, col = 'gray50') +
  geom_sf(data = train, aes(color = precio_quintiles), shape = 15, size = 1.2) +
  scale_color_manual(values = c("#ffe3e3", "#ffa8a8", "#ff6b6b", "#fa5252", "#c92a2a"),
                     labels = intervalos) + # Usar los intervalos en la leyenda
  coord_sf(xlim = c(-74.2, -74.03), ylim = c(4.57, 4.8)) +
  annotation_north_arrow(data = train, location='topleft', style = north_arrow_fancy_orienteering())+
  theme_bw() +
  labs(color = "Precio (Millones COP)")  # Etiqueta de la leyenda


#Observamos y guardamos el mapa
map1
ggsave("views/mapa1.png", width = 6, height = 4, plot = map1)

## Estrato promedio por sector -------------------------------------------------
datos <- datos %>% rename(sector = SCANOMBRE.x)

datos_estr <- datos %>%
  select(sector, ESTRATO, geometry) %>%
  group_by(sector) %>%
  mutate(estrato_prom = as.character(round(mean(ESTRATO, na.rm = TRUE))))

# Graficamos mapa del EPV

#Codigo del mapa
map2 <- ggplot() +
  geom_sf(data = datos_estr, aes(fill = estrato_prom), size = 0.3) +
  scale_fill_manual(values = c("#f3e5f5", "#e1bee7", "#ce93d8", "#ba68c8", "#ab47bc", "#8e24aa", "#6a1b9a")) +
  coord_sf(xlim = c(-74.22, -74.01), ylim = c(4.57, 4.8)) +
  theme(plot.title = element_text(hjust = 0)) +
  theme_bw()  +
  labs(fill = "Estrato promedio por sector") +
  annotation_north_arrow(data = datos_estr, location='topleft', style = north_arrow_fancy_orienteering())

map2