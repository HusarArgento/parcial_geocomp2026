#Librerias ---------------------------------------------------------------------
library(tidyverse)
library(sf)
library(geos)
library(leaflet)
library(igraph)
library(tidygraph)
library(sfnetworks)
library(osmdata)
options(scipen = 999)

#Consigna A: Obtención de la red desde OSM--------------------------------------

#Defino area de estudio
PERGAMINO <- getbb(
  "Pergamino, Buenos Aires,Argentina",
  format_out = "sf_polygon")

#La consulta

q <- opq(bbox = st_bbox(st_transform(PERGAMINO, 4326)),
         timeout = 300,
         memsize = 1073741824) |> 
  add_osm_feature(key = "highway",
                  value = c( 
                    "motorway", "motorway_link",
                    "trunk", "trunk_link",
                    "primary", "primary_link",
                    "secondary", "secondary_link",
                    "tertiary", "tertiary_link",
                    "unclassified", "residential",
                    "living_street") 
                  )
#Descarga datos
calles_osm <- osmdata_sf(q)

file.info(list.files("data/raw", pattern = "\\.pbf$", full.names = TRUE))[, c("size", "mtime")]
