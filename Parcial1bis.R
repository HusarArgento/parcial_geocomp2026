
#Librerias ---------------------------------------------------------------------
library(tidyverse)
library(sf)
library(geos)
library(dbscan)
library(leaflet)
library(igraph)
library(tidygraph)
library(sfnetworks)
library(osmdata)
library(units)
library(mapview)
library(dbscan)
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


# Consigna C: Armado red urbana -------------------------------------------
perga_net <- as_sfnetwork(lineint, directed = TRUE)

##Limpieza_ 
# Subdividir (2) -> Simplificar (1) -> Simplificar intersectiones (4) -> Suavizado de pseudo nodes (3)
# 1. Subdividir: Corta las líneas en cada intersección geométrica (cuando se 
#cruzan ) Necesitamos que cada tramo entre intersecciones sea una arista independiente 
#antes de empezar a fusionar o simplificar nodos..
perga_net2 <-  perga_net |>
  convert(to_spatial_subdivision) |>

  #2. Simplificar: Elimina vértices redundantes que no aportan a la topología 
#(reduciendo el peso y la cantidad de puntos innecesarios en el objeto sf).
  activate("edges") |>
  arrange(edge_length()) |>
  filter(!edge_is_multiple()) |>
  filter(!edge_is_loop())

#3. Simplificar Intersecciones: Agrupa nodos muy cercanos (por ejemplo, intersecciones 
#complejas, rotondas desdobladas  en un único nodo. 

# Retener las coordenadas de los nodos.
clusters <- perga_net2 |>
  activate("nodes") |>
  st_coordinates() |>
  
# Nodos dentro de una distancia de 3 (mts) entre sí estarán en el mismo clúster.
# A un nodo se le asigna un clúster incluso si es el único miembro de ese clúster.
  dbscan(eps = 3, minPts = 1) |>
  pluck("cluster")

perga_net2 <- perga_net2 |>
  activate("nodes") |>
  mutate(cls = clusters,               # Sumar la informacion de los clusters a los nodos de la red
         cmp = group_components()) |>  # Verificar que los  nodos agrupados estén conectados en la red.

# Contraer la red (utilizando centroide del clúster y eliminando nodos individuales)
  convert(to_spatial_contracted,
          cls, # cmp,
          simplify = TRUE)


# 3. Smooth Pseudo Nodes: elimina pseudonodos, es decir, nodos que no representan 
#intersecciones reales.
perga_net2 <- perga_net2 |>
  convert(to_spatial_smooth)

components(perga_net)$no #Cantidad de componentes: 391
components(perga_net2)$no #Cantidad de componentes: 32
components(perga_net2)$csize 
#Cantidad de aristas de las componentes (tamaño componentes)
#[1]    4392    3    1   35   23   13    2    1    1    1    1   27   18    1    4    1    2    1    3    2    3    1    2    1
#[25]    1     1    1    1    1    1    2    2

#Previo al procesamiento nuestra red (perga_net), tenía 391 componentes conexas ($no[1] 391) . 
#Es decir, una red cruda de de  cientos de pedazos debido a discontinuidadesa tales como micro-desfasajes en las esquinas o nodos no conectados 
#y otros errores de digitalización.
#Luego  de procesar topologicamente la red,  (perga_net2),  el número de componentes bajo a 32 ($no[1] 32), 
#eliminando 359 falsas rupturas 

#Por otro lado, el tamaño de las componentes ($csize) nos muestra  que la estructura se concentra masivamente en una componente principal de 4392$ elementos, 
#mientras que los $31 fragmentos restantes pertenecen a componentes secundarias o islotes de conectividad perifericos. 
#Para proceder con los análisis de ruteo y accesibilidad, conservaremos solo  la componente principal, descartando las redes secundarias por tratarse de artefactos 
#de digitalización inconclusos.

# Visualizamos componetes: Extraemos los nodos de la red y lo convertimos en un objeto espacial, 
# luego extraemos un vector que identifica a que componentes se vincula cada nodo, 
#luego lo convertimos en factores, es decir variables categoricas, agregandolas a la nueva 
#columna, "componente".

perganet_aux <- perga_net2 |>
  st_as_sf("nodes") |>
  mutate(componente = factor(components(perga_net2)$membership)) |>
  st_transform(4326)

factpal <- colorFactor(topo.colors(length(unique(perganet_aux$componente))),
                      perganet_aux$componente)

perganet_aux |>
  leaflet() |>
  addTiles() |>
  addPolygons(data = st_transform(st_as_sf(perga_net2, "edges"), 4326)) |>
  addCircles(color = ~factpal(componente))


#Nota especial sobre la componente 4:
#Chequeando las geometrias por separado la componente 4 llamo mi atencion especialmente por su rectangular 
#con lo que podria ser una  playa de maniobras, o un circuito cerrado de una fabrica /  polo industrial, por sus  viales internos. 
#Se dalo visto en clase, de  que no siempre las  componentes se deben a errores de digitalizacion, sino que a veces
#constituyen infraestructuras espacuales


### Retenemos el componente principal de la red y ponderamos la red por la distancia del eje
perganet_clean <- perga_net2 |>
  filter(group_components() == 1) |>    
  activate("edges") |>
  mutate(weight = edge_length())        

## Agregamos informacion de componentes a los nodos 
perganet_clean <- perganet_clean |>
  activate("nodes") |>
  mutate(comp = group_components())

# Reporte de nodos y aristas
igraph::gorder(perganet_clean) # nodos
#[1] 4392 nodos

igraph::gsize(perganet_clean)  # ejes
#[1] 10695


#El reporte
resumen_perga <- tibble(
  red = "Pergamino (Componente Principal)",
  nodos = igraph::gorder(perganet_clean),
  ejes = igraph::gsize(perganet_clean),
  longitud_km = perganet_clean |> 
    activate("edges") |> 
    as_tibble() |> 
    mutate(len = st_length(geometry)) |> 
 
        # 1. Extrae los datos de la tabla y los convierte en un vector plano de R
    pull(len) |> 
    
    # 2. Acumula todos los tramos individuales en un único valor total (mantiene la clase de unidad)
    sum(na.rm = TRUE) |> 
    
    # 3. Asegura y fija explícitamente la unidad base del acumulado como metros
    units::set_units("m") |> 
    
    # 4. Realiza la conversión matemática automática de metros a kilómetros
    units::set_units("km") |> 
    
    # 5. Despoja la etiqueta de unidad para dejar un número plano apto para el tibble y round()
    as.numeric() |> 
    round(2),
  
  grado_medio = mean(igraph::degree(perganet_clean)) |> round(2),
  densidad = igraph::edge_density(perganet_clean) |> round(6)
)

#Red                               Nodos  Ejes  Longitud   GradoM  Denisdad
#Pergamino (Componente Principal)	 4392	 10695	795.14	    4.87	0.00055

# Tenemos 4392 Nodos es decir el total de intersecciones de la zona urbana de Pergamino  
#y 10.695 aristas o tramos viales dirigidos. Por otro lado, el grado medio 4.87 
#nos dice que tenemos casi 5 conexiones por nodo, por lo cual seguramente convergen 
#esquinas de 3 y 4 ramas, como el caso del CID Campeador en CABA, sumado al efecto 
#del grafo dirigido (aristas de doble sentido contabilizadas en ambas direcciones).

# La densidad de una red mide  el porcentaje de conexiones reales frente al total 
# de conexiones teóricas posibles si cada nodo estuviera conectado directamente con todos 
# los demás al mismo tiempo. El número de aristas reales es de 10.695, con un porcentaje de 0.05%  
# muestra una proporcion realmente pequeña. Si la densidad diera cercana a 1, significaría 
#que la ciudad es un entramado del caos en la que cada esquina tendría  una autopista directa 
#hacia cualquier otro punto de la ciudad. 
# Seria interesante compararla con CABA, y con megaurbes como Chongqing en China. 
#Ademas es ideal para pensar esas marañas para pensar las megalopolis distopicas 
#de generos literarios como el cyberpunk 


#Hacemos grafico
ggplot() +
  # Dibujamos las aristas coloreadas según maxspeed_imp como factor
  geom_sf(
    data = st_as_sf(perganet_clean, "edges"), 
    aes(color = as.factor(maxspeed_imp)), 
    linewidth = 0.6, 
    alpha = 0.85
  ) +
  scale_color_viridis_d(
    name = "Velocidad\n(km/h)",
    option = "viridis", 
    na.value = "grey80"
    ) + 
  theme_void() +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 12),
    plot.background = element_rect(fill = "white", color = NA) 
  ) +
  labs(title = "Jerarquía Vial de Pergamino")

#Consigna D: Ruteo  -------------------------------------------

POI <- data.frame(
  poi  = c("Parque General San Martin",
           "Hospital San Jose",
           "Instituto Superior JVG",
           "Centro Civico", 
           "Barrio Guemes Norte"),
  tipo = "Orig",
  x    = c(-60.5727, -60.5857, -60.5816, -60.5747, -60.5996),   # longitud
  y    = c(-33.9066, -33.9101, -33.8923, -33.8972, -33.8867)    # latitud
) |>
  st_as_sf(coords = c("x", "y"), crs = 4326) |>   # tabla -> objeto sf
  st_transform(st_crs(perganet_clean))                     # al mismo CRS que las calles

#Ahora incrustamos}
perganet_blend <- perganet_clean |>
  activate("nodes") |> 
  st_network_blend(POI) |> #incrusto puntos
  activate("edges") |>
  mutate(weight = edge_length()) |> #recalculo aristas
  activate("nodes")

# Matriz de costos para los POI - ponderada por longitud (5x5)
cost_matrix <- st_network_cost(
  perganet_blend,
  from = POI,
  to = POI,   #la principial diferencia con el ejercicio que vimos de plazas y escuelas, aca es entr los puntos
  weights = "weight"
)
rownames(cost_matrix) <- POI$poi
colnames(cost_matrix) <- POI$poi

# Ver la matriz formateada
print(cost_matrix)

# Units: [m]
#                               Parque General San Martin   Hospital San Jose    Instituto Superior JVG
# Parque General San Martin                     0.000          1731.172               2950.299
# Hospital San Jose                          2247.467             0.000               2592.803
# Instituto Superior JVG                     2456.966          2332.539                  0.000
# Centro Civico                              1481.736          2254.806               1484.538
# Barrio Guemes Norte                        4295.463          4064.790               1960.592
                            
                              # Centro Civico     Barrio Guemes Norte

# Parque General San Martin     1833.0993            4563.845
# Hospital San Jose             2092.4118            4135.132
# Instituto Superior JVG         975.2294            2115.333
# Centro Civico                    0.0000            3061.165
# Barrio Guemes Norte           2915.3189               0.000

# Vamos por tiempo ahora
perganet_time <- perganet_blend |>
  activate("edges") |>
  mutate(
    # 1. Definimos velocidad base (con tu case_when o maxspeed_imp)
    speed_kmh = case_when(
      !is.na(maxspeed_imp) ~ maxspeed_imp,
      TRUE                 ~ 40
    ),
    speed  = units::set_units(speed_kmh, "km/h"), #como el dato esta plano le etiquetamos una unidad para despues pasarla a la que necesitamos que es metros por segundo
    speed  = units::set_units(speed, "m/s"),
    dist_m = edge_length(),
    time   = weight / speed,         # Resultado en segundos (m / (m/s)) porque los metros se anulan automaticamente
    time   = units::set_units(time, "minutes"),# Lo pasamos a minutos
    time   = units::drop_units(time),          # Quitamos unidades para igraph
      # 4. Asignamos ese 'time' limpio al 'weight' que exige la red
    weight = time
  ) |>
  select(geometry, weight, maxspeed_imp) |>
  activate("nodes")

# Matriz de costos temporal (5x5)
cost_matrix_time <- st_network_cost(
  perganet_time,
  from = POI,
  to = POI,
  weights = "weight"
  )

rownames(cost_matrix_time) <- POI$poi
colnames(cost_matrix_time) <- POI$poi

print(cost_matrix_time)
# 
#                                 Parque General San Martin   Hospital San Jose    Instituto Superior JVG
# Parque General San Martin                  0.000000          2.369875               3.789398
# Hospital San Jose                          3.081857          0.000000               2.839834
# Instituto Superior JVG                     3.333415          2.479769               0.000000
# Centro Civico                              2.037436          2.807233               2.088816
# Barrio Guemes Norte                        5.369822          5.092738               2.806413
# 
#                                Centro Civico    Barrio Guemes Norte
# Parque General San Martin      2.590901            5.651578
# Hospital San Jose              2.592526            5.093470
# Instituto Superior JVG         1.473192            2.788012
# Centro Civico                  0.000000            4.342451
# Barrio Guemes Norte            4.086161            0.000000

