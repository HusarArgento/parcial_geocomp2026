
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



st_geometry_type(calles_per) |> table() 
calles_line <- st_cast(calles_per, "LINESTRING") ## Casteamos de Multistring a Line cuan Gandalf en Moria 
st_geometry_type(calles_line) |> table() # Después

mapview(calles_line)
mapview(calles_osm)

calles_linered <- st_snap_to_grid(calles_line, size = 1)



