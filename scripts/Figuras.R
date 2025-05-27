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
rm(wd)

# Cargar datos -----------------------------------------------------------------
datos <- readRDS("stores/datos_modelos.rds")
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
  st_transform(crs=4326) 
sector <-sector %>% select(SCANOMBRE)

# Obtneción de las vias de bogota para fines esteticos del mapa 

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

## Mapa distribución geografica de precios -------------------------------------

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

## Mapa distribución geografica de precio por metro cuadrado  ------------------

#Calculamos el precio por metro cuadrado y lo dividimos en quintiles
train <- train %>%
  mutate(precio_m2 = precio_millones/area, 
         precio_m2_qui = cut(precio_m2, 
                                breaks = quantile(precio_m2, probs = seq(0, 1, 0.2), na.rm = TRUE), 
                                include.lowest = TRUE))

# Obtenemos intervalos de los quintiles y los convertimos a string (manualmente)
intervalos <- levels(train$precio_m2_qui)
intervalos_des <- c('0.12-2.98', '2.98-4.32', '4.32-5.29', '5.29-6.43', '6.43-30.3')

#Codigo del mapa
map2 <- ggplot() +
  geom_sf(data = bogota_lim, fill = NA, size = 1, color = 'springgreen4') +
  geom_sf(data = vias_principales, fill = NA, size = 0.6, col = 'gray70') +
  geom_sf(data = avenidas, fill = NA, size = 0.6, col = 'gray70') +
  geom_sf(data = vias_secundarias, fill = NA, size = 0.6, col = 'gray70') +
  geom_sf(data = train, aes(color = precio_m2_qui), shape = 16, size = 0.7) +
  scale_color_manual(values = c("#fcb5b5", "#ff9999", "#ff6b6b", "#fa5252", "#c92a2a"),
                     labels = intervalos_des) + # Usar los intervalos en la leyenda
  coord_sf(xlim = c(-74.18, -74.02), ylim = c(4.57, 4.77)) +
  theme_bw() +
  theme(panel.grid = element_blank(), axis.text = element_text(size = 6)) +  # quitar el grid
  labs(color = "Precio por metro cuadrado \n(Millones COP)") # Etiqueta de la leyenda


#Observamos y guardamos el mapa
map2
ggsave("views/mapa2.png", width = 5, height = 4, plot = map2)

## Estrato promedio por sector -------------------------------------------------
manzanas_estr<-st_read("stores/manz_estrato/ManzanaEstratificacion.shp")
manzanas_estr <- manzanas_estr %>% mutate(estrato_chr = as.character(ESTRATO))
# Graficamos mapa del EPV

#Codigo del mapa
map3 <- ggplot() +
  geom_sf(data = bogota_lim, fill = NA, size = 1, color = 'springgreen4') +
  geom_sf(data = vias_principales, fill = NA, size = 0.6, col = 'gray70') +
  geom_sf(data = avenidas, fill = NA, size = 0.6, col = 'gray70') +
  geom_sf(data = manzanas_estr, aes(fill = estrato_chr), color = NA) +
  scale_fill_manual(values = c("#f3e5f5", "#e1bee7", "#ce93d8", "#ba68c8", "#ab47bc", "#8e24aa", "#6a1b9a")) +
  coord_sf(xlim = c(-74.18, -74.02), ylim = c(4.57, 4.77)) +
  theme(plot.title = element_text(hjust = 0)) +
  theme_bw()  +
  theme(panel.grid = element_blank(), axis.text = element_text(size = 6)) +  # quitar el grid
  labs(fill = "Estrato por manzana")

map3
ggsave("views/mapa3.png", width = 5, height = 4, plot = map3)

## Precio por distancia al cbd -------------------------------------------------
# Crear variable de percentiles 
train <- train %>%
  mutate(percentil_dist = ntile(cbd_distancia, 100))

train <- train %>%
  mutate(decil_dist = ntile(cbd_distancia, 100))

#Quintiles
train <- train %>%
  mutate(quintil_dist = ntile(cbd_distancia, 5))


# Calcular el precio promedio por metro cuadrado por percentil
resumen <- train %>%
  group_by(decil_dist) %>%
  summarise(precio_m2_prom = mean(precio_m2, na.rm = TRUE))

# Graficar
bar1 <- ggplot(resumen, aes(x = factor(decil_dist), y = precio_m2_prom)) +
  geom_bar(stat = "identity", fill = "#ab47bc", color =   "#6a1b9a", width = 0.7) +
  labs(x = "Percentil de distancia al centro de negocios",
       y = "Precio promedio por metro cuadrado (Millones COP)") +
  theme_bw() +
  theme(panel.grid = element_blank())

bar1
ggsave("views/barras1.png", width = 5, height = 4, plot = bar1)

## Tabla de estadísticas descriptivas ------------------------------------------



