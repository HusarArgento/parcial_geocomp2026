
#Librerias ---------------------------------------------------------------------
library(tidyverse)
library(sf)
library(geos)
library(leaflet)
library(igraph)
library(tidygraph)
library(sfnetworks)
library(osmdata)
library(units)
library(mapview)
options(scipen = 999)

#Consigna A: Obtención de la red desde OSM--------------------------------------

# 1. Área de estudio (polígono real, no bbox)
PERGAMINO <- getbb("Pergamino, Buenos Aires, Argentina",
                   format_out = "sf_polygon")
# si devuelve una lista o varios polígonos, quedate con el primero/mayor:
# PERGAMINO <- PERGAMINO[[1]]  (si es lista)
PERGAMINO <- st_make_valid(st_transform(PERGAMINO, 4326))

# 2. Carpeta de descarga
dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)

# 3. Tipos de vía
tipos <- c("motorway", "motorway_link", "trunk", "trunk_link",
           "primary", "primary_link", "secondary", "secondary_link",
           "tertiary", "tertiary_link", "unclassified", "residential",
           "living_street")

# 4. Descarga + lectura filtrada
calles_osm <- oe_get(
  place = "Argentina",
  layer = "lines",
  provider = "geofabrik",
  download_directory = "data/raw",
  boundary = PERGAMINO,
  boundary_type = "clipsrc",     # recorta al polígono
  query = "SELECT * FROM lines WHERE highway IN ('motorway','motorway_link','trunk','trunk_link','primary','primary_link','secondary','secondary_link','tertiary','tertiary_link','unclassified','residential','living_street')",
  #query = paste0(
  #  "SELECT * FROM lines WHERE highway IN ('",
   # paste(tipos, collapse = "','"), "')"
  #),
  extra_tags = c("maxspeed", "oneway", "lanes", "surface", "name"),
  quiet = FALSE
)
class(calles_osm)
names(calles_osm)
nrow(calles_osm)
st_geometry_type(calles_osm)
st_crs(calles_osm)
file.info(list.files("data/raw", pattern = "\\.pbf$", full.names = TRUE))[, c("size", "mtime")]

st_area(PERGAMINO) |> set_units("km2")
leaflet() |> addTiles() |> addPolygons(data = st_transform(PERGAMINO, 4326))


#Descarga de la red vial
#- Fuente: OpenStreetMap, mediante extracto de Geofabrik.
#- Herramienta: osmextract, función oe_get().
#- Área de estudio: Pergamino, Buenos Aires.
#- Filtro: categorías seleccionadas de la etiqueta highway.
#- Atributos adicionales: maxspeed, oneway, lanes, surface y name.
#- Cantidad obtenida: 2.846 registros.
#- Formato: objeto espacial sf.
#- Geometría: MULTILINESTRING.
#- Sistema de referencia: WGS 84 (EPSG:4326).
#- 05-11-2026 15:09

#Consigna B: Preparacion de datos----------------------------------------------

#Reproyectamos
calles_per <- st_transform(calles_osm, 5347)
st_crs(calles_per)

#Limpiar geometrías vacías, duplicadas o con errores; convertir MULTILINESTRING  a LINESTRING

##Verificar
summary(calles_per) #Estadisticas del callejero
sum(st_is_empty(calles_per)) # Corroborar geometrías vacías
sum(!st_is_valid(calles_per)) # Corroborar geometrías inválidas
table(st_geometry_type(calles_per)) # Corroborar tipologias geométricas
nrow(calles_per) # Verificar cantidad  de geometrías
table(calles_per$highway, useNA = "ifany")# distribucion x tipos de vía
table(calles_per$maxspeed)

#El hechicero castea
st_geometry_type(calles_per) |> table() 
calles_line <- st_cast(calles_per, "LINESTRING") ## Casteamos de Multistring a Line cuan Gandalf en Moria 
st_geometry_type(calles_line) |> table() # Después

mapview(calles_line)

#Redondeo, por precaucion trabajamos con una copia
calles_linered <- calles_line
st_geometry(calles_linered) <- st_geometry(calles_linered) |>
  lapply(function(x) round(x, 0)) |>
  st_sfc(crs = st_crs(calles_linered))

#calles_linered <- st_snap_to_grid(calles_line, size = 1)
#No funciona

table(calles_linered$highway, DOBLE_MANO = calles_linered$oneway, exclude = NULL)

## Imputamos a doble mano todas las calles que no están clasificadas
calles_linered <- calles_linered |>
  mutate(oneway = ifelse(is.na(oneway), "no", oneway))

### Duplicamos calles con doblevía
calle_aux <- calles_linered |> filter(oneway == "no") |> st_reverse()
calles_linered <- rbind(calles_linered, calle_aux)


# Imputacion de Velocidades: ¡PROBLEMA!
#La mayoría de las calles residenciales y algunas avenidas arrojan NA en sus velociddades
#llegamos a esta conclusión mirando el siguiente grafico

#Graficamos para explorar. 
ggplot(calles_linered) +
  geom_sf(aes(color = is.na(maxspeed)), linewidth = 0.5) +
  scale_color_manual(
    values = c("TRUE" = "gray80", "FALSE" = "firebrick"),
    labels = c("TRUE" = "Sin velocidad (NA)", "FALSE" = "Con velocidad")
  ) +
  theme_minimal() +
  labs(color = "Estado")

#Exploramos con tablitas
table(calles_per$highway, calles_linered$maxspeed, useNA = "ifany")

calles_linerer %>%
  st_drop_geometry() %>%
  count(highway, maxspeed, sort = TRUE)

calles_linered %>%
  st_drop_geometry() %>%
  filter(
    highway %in% c("primary", "secondary", "tertiary", "trunk"),
    is.na(maxspeed)
  ) %>%
  with(table(name, highway))

#Grafico para ver tipos de vias
ggplot(calles_per) +
  geom_sf(aes(color = highway), linewidth = 0.5) +
  scale_color_brewer(palette = "Paired") +
  theme_minimal() +
    labs(
    title = "Red vial de Pergamino por tipo de vía",
    subtitle = "Clasificación según OpenStreetMap (highway)",
    color = "Tipo de vía"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    legend.position = "right"
  )

#Posiblemente se deba a conflicto municipal que busca reformar velocidades, el proyecto de ley
#está en discusion, pero el municipio procedió de manera unilateral instalando ya en calle la 
#cartelería de la reforma, sin haber sido aprobada la misma. El proyecto busca reducir velocidades
#de avenidas a 40 km/h y afecta a varias residenciales. Vamos a imputar las velocidades teniendo en cuenta
# el articulo 51 de la Ley Nacional de transito 24449 y la ley provincial 13.927. El nuevo proyecto
#busca En Pergamino, corredores troncales y de gran movimiento (como Avenida de Mayo, Colón, Ameghino, Paraguay, 
#Rocha, Hipólito Yrigoyen y Juan B. Justo) están  señalizados a 40 km/h, pero legalmente como la reforma no se votó
#vamos a imputarle 60 km/h de acuerdo a ley provincial y nacional.

#Verificamos las avenidas mencionadas en el proyecto de ley
calles_per <- calles_linered %>% 
  filter(grepl("Colón|Ameghino|Paraguay|Rocha|Yrigoyen|Justo", name, ignore.case = TRUE)) %>% 
  select(name, highway, maxspeed)

#Imputamos
calles_lineimp <- calles_linered |>
  mutate(
    maxspeed = case_when(
      is.na(maxspeed) & grepl("Juan B. Justo|Ameghino|Paraguay|Colón|Rocha|Yrigoyen", name, ignore.case = TRUE) ~ 60, 
      is.na(maxspeed) & highway == "residential" ~ 40, 
      !is.na(maxspeed) ~ as.numeric(maxspeed), 
      TRUE ~ 40)
  )

##Longitud
#Medimos:
calles_lineimp <- calles_lineimp |>
  mutate(
    longitud = st_length(geometry)
  )

#Verificamos estadisticas para construir los rangos o umbrales
summary(calles_lineimp$longitud)

summary(calles_lineimp$longitud)
# Min.   1st Qu.  Median    Mean    3rd Qu.  Max. 
#0.00   73.59   110.63    169.92  216.51   1832.31 

calles_lineimp <- calles_lineimp %>%
  mutate(longitud_m = as.numeric(st_length(calles_lineimp)))

#Hacemos una columna nueva para mantener la de unidades y que quede explicitada
#la distancia en metros. La decision se justifica en mantener una columna con las 
#unidades originales, para tenerla a disposicion para futuras operaciones espaciales
#y que si algun otro usuario toma el codigo sepa en que unidad de medida  estan expresadas

calles_lineimp |>
  filter(longitud_m == 0)

#Simple feature collection with 34 features and 17 fields
#Es decir 17 calles de 0 metros.
#Vamos a clasificar por tramos para ver mejor que ocurre

lineint <- calles_lineimp |>  
  mutate(intervalos = case_when(
      longitud_m > 0  & longitud_m <= 1  ~ "0-1 m",  
      longitud_m > 1  & longitud_m <= 5  ~ "1-5 m",
      longitud_m > 5  & longitud_m <= 10 ~ "5-10 m",
      longitud_m > 10 & longitud_m <= 20 ~ "10-20 m",
      longitud_m > 20 & longitud_m <= 50 ~ "20-50 m",
      longitud_m > 50                    ~ "+ 50 m",
      TRUE                               ~ "0 m o inválidos" 
    )
  )

summary(lineint$intervalos)

lineint %>%
  st_drop_geometry() %>%
  count(intervalos, sort = TRUE)

#1          + 50 m 3982
#2         20-50 m  478
#3         10-20 m  209
#4          5-10 m   99
#5           1-5 m   50
#6 0 m o inválidos   34
#7           0-1 m   20

table(lineint$intervalos)
#+ 50 m           0-1 m 0 m o inválidos           1-5 m         10-20 m         20-50 m          5-10 m 
# 3982              20              34              50             209             478              99 

calles_clean <- lineint |> 
  filter(longitud_m > 2) 

#Como ultimo paso antes de construir la red, vamos a establecer un corte operativo de 2metros
#en las calles con el objeto de remover inconsistencias derivadas de la descarga de OSM, si nos 
#fijamos en las tablas , esto equivale al 2% y, en términos de movilidad urbana real, 
#un tramo de calle o avenida menor a 2 metros tiene una  funcionalidad casi nula  para el 
#tránsito vehicular o peatonal macro 


# Armado red urbana -------------------------------------------
