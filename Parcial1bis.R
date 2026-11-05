
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

summary(calles_per)

# Geometrías vacías
sum(st_is_empty(calles_per))

# Geometrías inválidas
sum(!st_is_valid(calles_per))

# Tipos geométricos
table(st_geometry_type(calles_per))

# Cantidad de geometrías
nrow(calles_per)

## Casteamos de Multi a Line 

st_geometry_type(calles_per) |> table()

# Conversión
calles_line <- st_cast(calles_per, "LINESTRING")

# Después
st_geometry_type(calles_line) |> table()


#guardamos
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
st_write(PERGAMINO,   "data/processed/pergamino_area.gpkg",    delete_dsn = TRUE)
st_write(calles_osm,  "data/processed/calles_osm_raw.gpkg",    delete_dsn = TRUE)
st_write(calles_line, "data/processed/calles_line.gpkg",       delete_dsn = TRUE)