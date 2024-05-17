# this is the code for outputting weather data from big sandy and Moccasin, MT 
# into an xlsx file
# 

# inventory - this is the list of all the weather stations in the NOAA database
inventory_url <- "https://www.ncei.noaa.gov/pub/data/ghcn/daily/ghcnd-inventory.txt"

# label the columns
inventory <- read_table(inventory_url,
                        col_names = c("station", "lat", 
                                      "lon", "variable", 
                                      "start", "end"))


# these are the coordinates for our three field sites
area <- c("Big Sandy", "Moccasin", "Bozeman", "Havre")
lats <- c(48.177540, 47.109558, 45.678871, 48.527908)
longs <- c(-110.114060, -109.884338, -111.060815, 109.684799)

coordinates <- data.frame(area, lats, longs)

### Big Sandy ###
bs_coordinates <- 
  coordinates %>%
  mutate(lats_conv = lats * 2 * pi/360,
         longs_conv = longs * 2 * pi/360) %>%
  filter(area == "Big Sandy")

bs_top_3_stations <- 
  inventory %>%
  dplyr::select(station, lat, lon, start, end) %>%
  mutate(lat_r = lat *2 *pi/360,
         lon_r = lon *2 *pi/360,
         d = 1.609344 * 3963 * acos((sin(lat_r) * sin(bs_coordinates$lats_conv)) + cos(lat_r) * cos(bs_coordinates$lats_conv) * cos(bs_coordinates$longs_conv - lon_r))) %>%
  filter(start < 1960 & end > 2023) %>%
  dplyr::select(-c(start, end)) %>%
  distinct() %>%
  arrange(d) %>%
  top_n(n = -3, d) %>%
  pull(lat)

bs_station_1 <- 
  bs_top_3_stations %>%
  slice(1) %>%
  pull(station)

bs_station_2 <-
  bs_top_3_stations %>%
  slice(2) %>%
  pull(station)

bs_station_3 <-
  bs_top_3_stations %>%
  slice(3) %>%
  pull(station)

data <-
  read_csv(glue("https://ncei.noaa.gov/pub/data/ghcn/daily/by_station/{bs_station_1}.csv.gz"),
           show_col_types = FALSE,
           col_names = c("station", "date", 
                         "variable", "value", "a", "b", 
                         "c", "d"))


bs_weather_data <-
  read_csv(glue("https://ncei.noaa.gov/pub/data/ghcn/daily/by_station/{bs_station_1}.csv.gz"), 
           show_col_types = FALSE,
           col_names = c("station", "date", 
                         "variable", "value", "a", "b", 
                         "c", "d")) %>%
  dplyr::select(station, date, variable, value) %>%
  bind_rows(read_csv(glue("https://ncei.noaa.gov/pub/data/ghcn/daily/by_station/{bs_station_2}.csv.gz"),
                     show_col_types = FALSE,
                     col_names = c("station", "date",
                                   "variable", "value", 
                                   "a", "b", "c", "d")) %>%
              dplyr::select(station, date, variable, value)) %>%
  bind_rows(read_csv(glue("https://ncei.noaa.gov/pub/data/ghcn/daily/by_station/{bs_station_3}.csv.gz"),
                     show_col_types = FALSE,
                     col_names = c("station", "date",
                                   "variable", "value", 
                                   "a", "b", "c", "d")) %>%
              dplyr::select(station, date, variable, value)) %>%
  filter(variable %in% c("TMAX", "TMIN", "PRCP", "SNOW")) %>%
  mutate(date = ymd(date)) %>%
  filter(date > "1950-01-01") %>%
  filter(value > -50) %>%
  left_join(bs_top_3_stations %>%
              dplyr::select(station, d, lat, lon), 
            by = "station") %>%
  mutate(year = lubridate::year(date))

bs_weather_data %>%
  dplyr::select(variable) %>%
  distinct()

### Moccasin ###
moc_coordinates <- 
  coordinates %>%
  mutate(lats_conv = lats * 2 * pi/360,
         longs_conv = longs * 2 * pi/360) %>%
  filter(area == "Moccasin")

moc_top_3_stations %>%
  dplyr::select(lat, lon) %>%
  pull(lat)

moc_top_3_stations <- 
  inventory %>%
  dplyr::select(station, lat, lon, start, end) %>%
  mutate(lat_r = lat *2 *pi/360,
         lon_r = lon *2 *pi/360,
         d = 1.609344 * 3963 * acos((sin(lat_r) * sin(moc_coordinates$lats_conv)) + cos(lat_r) * cos(moc_coordinates$lats_conv) * cos(moc_coordinates$longs_conv - lon_r))) %>%
  filter(start < 1960 & end > 2023) %>%
  dplyr::select(-c(start, end)) %>%
  distinct() %>%
  arrange(d) %>%
  top_n(n = -3, d) 

moc_station_1 <- 
  moc_top_3_stations %>%
  slice(1) %>%
  pull(station)

moc_station_2 <-
  moc_top_3_stations %>%
  slice(2) %>%
  pull(station)

moc_station_3 <-
  moc_top_3_stations %>%
  slice(3) %>%
  pull(station)

library(rmarkdown)

render("paper1.rmd", output_format = "word_document")


write.xlsx(moc_weather_data, "moccasin_prcp.xlsx", rowNames = FALSE)
write.xlsx(bs_weather_data, "bigsandy_weather.xlsx", rowNames = FALSE)

moc_weather_data <-
  read_csv(glue("https://ncei.noaa.gov/pub/data/ghcn/daily/by_station/{moc_station_1}.csv.gz"),
           show_col_types = FALSE,
           col_names = c("station", "date", 
                         "variable", "value", "a", 
                         "b", "c", "d")) %>%
  dplyr::select(station, date, variable, value) %>%
  bind_rows(read_csv(glue("https://ncei.noaa.gov/pub/data/ghcn/daily/by_station/{moc_station_2}.csv.gz"),
                     show_col_types = FALSE,
                     col_names = c("station", "date",
                                   "variable", "value", 
                                   "a", "b", "c", "d")) %>%
              dplyr::select(station, date, variable, value)) %>%
  bind_rows(read_csv(glue("https://ncei.noaa.gov/pub/data/ghcn/daily/by_station/{moc_station_3}.csv.gz"),
                     show_col_types = FALSE,
                     col_names = c("station", "date",
                                   "variable", "value",
                                   "a", "b", "c", "d")) %>%
              dplyr::select(station, date, variable, value)) %>%
  filter(variable %in% c("TMAX", "PRCP", "SNOW")) %>%
  mutate(date = ymd(date)) %>%
  filter(date > "1950-01-01") %>%
  filter(value > -50) %>%
  left_join(moc_top_3_stations %>%
              dplyr::select(station, d, lat, lon), 
            by = "station") %>%
  mutate(year = lubridate::year(date))