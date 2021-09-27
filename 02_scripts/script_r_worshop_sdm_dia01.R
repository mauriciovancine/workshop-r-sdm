#' ---
#' titulo: oficina sdm moco
#' autor: mauricio vancine
#' data: 25-09-2021
#' ---

# prepare r -------------------------------------------------------------
# packages
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(rnaturalearth)) install.packages("rnaturalearth")
if(!require(rnaturalearthdata)) install.packages("rnaturalearthdata")
if(!require(nngeo)) install.packages("nngeo")
if(!require(sf)) install.packages("sf")
if(!require(tmap)) install.packages("tmap")
if(!require(spocc)) install.packages("spocc")
if(!require(CoordinateCleaner)) install.packages("CoordinateCleaner")
if(!require(spThin)) install.packages("spThin")
if(!require(mapview)) install.packages("mapview")
if(!require(mapedit)) install.packages("mapedit")
if(!require(raster)) install.packages("raster")
if(!require(viridis)) install.packages("viridis")
if(!require(usdm)) install.packages("usdm")
if(!require(ENMTools)) install.packages("ENMTools")

# options
options(timeout = 1e5)
options(scipen = 50)

# directories
dir.create("03_dados")
dir.create("03_dados/01_ocorrencias")
dir.create("03_dados/02_variaveis")

# 1. occurrences ----------------------------------------------------------
# import limit ----
li <- rnaturalearth::ne_countries(scale = 50, continent = "South America", returnclass = "sf") %>%
  sf::st_union(rnaturalearth::ne_countries(scale = 50, country = "France", returnclass = "sf")) %>%
  sf::st_crop(rnaturalearth::ne_countries(continent = "South America", returnclass = "sf")) %>%
  sf::st_union() %>%
  nngeo::st_remove_holes() %>%
  sf::st_as_sf()
li

tm_shape(li) +
  tm_polygons()

# download ----
# species
# sp <- "Chrysocyon brachyurus"
sp <- "Marmosops incanus"


# spocc - pode demorar um tempo...
occ_spocc <- spocc::occ(query = sp,
                        from = c("gbif", "inat", "vertnet", "idigbio", "ecoengine"),
                        has_coords = TRUE,
                        limit = 1e5)
occ_spocc

# specieslink
occ_splink <- readr::read_csv(
  paste0("https://api.splink.org.br/records/Format/CSV/scientificName/", gsub(" ", "%20", sp)), col_types = cols())
occ_splink

# get data
occ_spocc_data <- spocc::occ2df(occ_spocc) %>%
  dplyr::mutate(species = sp,
                longitude = as.numeric(longitude),
                latitude = as.numeric(latitude),
                year = date %>% lubridate::year(),
                base = prov %>% stringr::str_to_lower()) %>%
  dplyr::select(name, species, longitude, latitude, year, base)
occ_spocc_data

occ_splink_data <- occ_splink %>%
  tidyr::drop_na(decimalLongitude, decimalLatitude) %>%
  dplyr::mutate(species = sp,
                name = scientificName,
                longitude = as.numeric(decimalLongitude),
                latitude = as.numeric(decimalLatitude),
                year = as.numeric(year),
                base = "specieslink") %>%
  dplyr::select(name, species, longitude, latitude, year, base)
occ_splink_data

# combine data
occ_data <- dplyr::bind_rows(occ_spocc_data, occ_splink_data)
occ_data

# map
occ_data_vector <- occ_data %>%
  tidyr::drop_na(longitude, latitude) %>%
  dplyr::mutate(lon = longitude, lat = latitude) %>%
  dplyr::filter(lon >= -180, lon <= 180, lat >= -90, lat <= 90) %>%
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
occ_data_vector

tm_shape(li, bbox = occ_data_vector) +
  tm_polygons() +
  tm_shape(occ_data_vector) +
  tm_dots(size = .2, shape = 21, col = "steelblue") +
  tm_graticules(lines = FALSE)

# spatial limit filter ----
# crop to limit
occ_data_sptlim <- occ_data_vector %>%
  dplyr::mutate(sptlim_filter = as.logical(sf::st_intersects(occ_data_vector, li, sparse = FALSE)))
occ_data_sptlim

# map
tm_shape(li) +
  tm_polygons() +
  tm_shape(occ_data_sptlim %>%
             filter(sptlim_filter == TRUE)) +
  tm_dots(size = .2, shape = 21, col = "steelblue")

# date filter ----
# verify
table(occ_data_sptlim$year, useNA = "always")

# year > 1970 and < 2021
occ_data_sptlim_date <- occ_data_sptlim %>%
  dplyr::mutate(date_filter = ifelse(
    is.na(year) == FALSE &
      year >= 1970 &
      year <= lubridate::today() %>% lubridate::year(),
    TRUE, FALSE))
occ_data_sptlim_date

# map
tm_shape(li) +
  tm_polygons() +
  tm_shape(occ_data_sptlim_date %>%
             filter(sptlim_filter == TRUE,
                    date_filter == TRUE)) +
  tm_dots(size = .2, shape = 21, col = "steelblue")

# bias filter ----
# flag data
occ_data_sptlim_date_bias <- CoordinateCleaner::clean_coordinates(
  x = sf::st_drop_geometry(occ_data_sptlim_date),
  species = "species",
  lon = "longitude",
  lat = "latitude",
  outliers_mtp = 2,
  tests = c("capitals", # radius around capitals
            "centroids", # radius around country and province centroids
            "duplicates", # records from one species with identical coordinates
            "equal", # equal coordinates
            "gbif", # radius around GBIF headquarters
            "institutions", # radius around biodiversity institutions
            "seas", # in the sea
            "urban", # within urban area
            "validity", # outside reference coordinate system
            "zeros" # plain zeros and lat = lon
  )) %>%
  tibble::as_tibble() %>%
  dplyr::mutate(lon = longitude, lat = latitude) %>%
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
occ_data_sptlim_date_bias

# map
tm_shape(li) +
  tm_polygons() +
  tm_shape(occ_data_sptlim_date_bias %>%
             filter(sptlim_filter == TRUE,
                    date_filter == TRUE,
                    .summary == TRUE)) +
  tm_dots(size = .2, shape = 21, col = "steelblue")


# spatial distance filter ----
filter_thin <- spThin::thin(loc.data = occ_data_sptlim_date_bias,
                            lat.col = "latitude",
                            long.col = "longitude",
                            spec.col = "species",
                            thin.par = 50,
                            reps = 1,
                            write.files = FALSE,
                            write.log.file = FALSE,
                            locs.thinned.list.return = TRUE,
                            verbose = TRUE) %>%
  .[[1]] %>%
  tibble::as_tibble() %>%
  dplyr::rename_with(tolower) %>%
  dplyr::mutate(sptdist_filter = TRUE)
filter_thin

# join
occ_data_sptlim_date_bias_sptdist <- dplyr::left_join(
  x = occ_data_sptlim_date_bias,
  y = filter_thin,
  by = c("longitude", "latitude")) %>%
  dplyr::mutate(sptdist_filter = replace_na(sptdist_filter, FALSE)) %>%
  dplyr::relocate(sptdist_filter, .after = date_filter)
occ_data_sptlim_date_bias_sptdist

# map
tm_shape(li) +
  tm_polygons() +
  tm_shape(occ_data_sptlim_date_bias_sptdist %>%
             filter(sptlim_filter == TRUE,
                    date_filter == TRUE,
                    sptdist_filter == TRUE,
                    .summary == TRUE)) +
  tm_dots(size = .2, shape = 21, col = "steelblue")

# apply filters ----
occ_data_filter <- occ_data_sptlim_date_bias_sptdist %>%
  filter(sptlim_filter == TRUE,
         date_filter == TRUE,
         sptdist_filter == TRUE,
         .summary == TRUE) %>%
  dplyr::select(species, longitude, latitude)
occ_data_filter

mapview::mapview(occ_data_filter)

# manual editing ----
occ_data_filter_edit <- mapedit::editFeatures(occ_data_filter) # atencao para o Done!
occ_data_filter_edit

# verificar
mapview::mapview(occ_data_filter_edit)

# export ----
# vetor
occ_data_filter_edit %>%
  sf::st_write("03_dados/01_ocorrencias/occ_data_filter_edit.shp")

# tabela
occ_data_filter_edit %>%
  sf::st_drop_geometry() %>%
  readr::write_csv("03_dados/01_ocorrencias/occ_data_filter_edit.csv")

# 2. variables ------------------------------------------------------------
# download variables ----
download.file(url = "https://biogeo.ucdavis.edu/data/worldclim/v2.1/base/wc2.1_10m_bio.zip",
              destfile = "03_dados/02_variaveis/wc2.1_10m_bio.zip", mode = "wb")

# unzip
unzip(zipfile = "03_dados/02_variaveis/wc2.1_10m_bio.zip",
      exdir = "03_dados/02_variaveis/00_raw")

# import
env <- dir(path = "03_dados/02_variaveis/00_raw", pattern = ".tif$", full.names = TRUE) %>%
  raster::stack() %>%
  raster::brick()
env

# rename
names(env)
names(env) <- c("bio01", paste0("bio", 10:19), paste0("bio0", 2:9))
names(env)
env

# plot
plot(env$bio01)

# extent and resolution ----
# adjust extent and resolution
env_li <- env %>%
  raster::crop(li) %>%
  raster::mask(li) %>%
  raster::aggregate(fact = .5/res(env)[1])
env_li

# plot
tm_shape(env_li$bio01) +
  tm_raster(palette = "-RdBu", n = 10) +
  tm_shape(li) +
  tm_borders(col = "black") +
  tm_layout(legend.position = c("right", "bottom"))

# collinearity ----
# correlation
ENMTools::raster.cor.matrix(env_li, method = "pearson")
ENMTools::raster.cor.plot(env_li)

# vif
env_li_vif <- usdm::vif(env_li)
env_li_vif

# vifstep
env_li_vifstep <- usdm::vifstep(env_li, th = 2)
env_li_vifstep

# vifcor
env_li_vifcor <- usdm::vifcor(env_li, th = .7)
env_li_vifcor

# select
env_li_vif <- usdm::exclude(env_li, env_li_vifstep)
env_li_vif

env_li_cor <- usdm::exclude(env_li, env_li_vifcor)
env_li_cor

# scale ----
env_li_vif_scale <- raster::scale(env_li_vif)
env_li_vif_scale

# plot
plot(env_li_vif, col = viridis::viridis(100))
plot(env_li_vif_scale, col = viridis::viridis(100))

# export ----
raster::writeRaster(x = env_li_vif_scale,
                    filename = paste0("03_dados/02_variaveis/", names(env_li_vif_scale)),
                    bylayer = TRUE,
                    format = "GTiff",
                    overwrite = TRUE)

# end ---------------------------------------------------------------------
