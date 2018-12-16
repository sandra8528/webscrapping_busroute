#-----------------------------------------------------------------------------#
# SCRAPE BUS ROUTE
#-----------------------------------------------------------------------------#
library(rvest)
library(tidyverse)
# Get All Locations
location <- read_html(paste0("https://mobile.nwstbus.com.hk/text",
                             "/bldgsearch.php?mode=2&l=2")) %>%
  html_nodes("#location") %>%
  html_nodes("option")
values <- location %>% html_attr("value")
locations <- location %>% html_text()
location <- tibble(
  location = locations,
  location_value = values
)
location <- location[-1, ]

# Get All districts
districts <- list()
for (loc in location$location_value) {
  url <- sprintf(paste0("https://mobile.nwstbus.com.hk/text/bldgsearch.php?",
                        "l=2&location=%s&submitForm=location&locationValue=&",
                        "districtValue=&areaValue=&mode=2"),
                 loc)
  dis <- read_html(url) %>%
    html_nodes("#district") %>%
    html_nodes("option")
  district <- tibble(
    location_value = loc,
    district = dis %>% html_text(),
    district_value = dis %>% html_attr("value")
  )
  district <- district[-1, ]
  districts <- bind_rows(districts, district)
}

# Get all areas
areas <- list()
for (i in seq(nrow(districts))) {
  sloc <- districts$location_value[i]
  sdis <- districts$district_value[i]
  url <- sprintf(paste0("https://mobile.nwstbus.com.hk/text/bldgsearch.",
                        "php?l=2&district=%s&location=%s&submitForm=dist",
                        "rict&locationValue=&districtValue=&areaValue=&mode=2"),
                 sdis, sloc)
  ar <- read_html(url) %>%
    html_nodes("#area") %>%
    html_nodes("option")
  area <- tibble(
    location_value = sloc,
    district_value = sdis,
    area = ar %>% html_text(),
    area_value = ar %>% html_attr("value")
  )
  area <- area[-1, ]
  areas <- bind_rows(areas, area)
}

# Get all buildings
buildings <- list()
for (i in seq(nrow(areas))) {
  sloc <- areas$location_value[i]
  sdis <- areas$district_value[i]
  sarea <- areas$area_value[i]
  url <- sprintf(paste0("https://mobile.nwstbus.com.hk/text/bldgsearch.php",
                        "?l=2&area=%s&location=%s&district=%s&submitForm=",
                        "area&locationValue=&districtValue=&areaValue=&mode=2"),
                 sarea, sloc, sdis)
  build <- read_html(url) %>%
    html_nodes("#bldg") %>%
    html_nodes("option")
  building <- tibble(
    location_value = sloc,
    district_value = sdis,
    area_value = sarea,
    building = build %>% html_text(),
    building_value = build %>% html_attr("value")
  )
  building <- building[-1, ]
  buildings <- bind_rows(buildings, building)
}

# Build out building data------------------------------------------------------
buildings_data <- location %>%
  inner_join(districts) %>%
  inner_join(areas) %>%
  inner_join(buildings)

write_csv(buildings_data, "buildings.csv")

# For each building find nearby bus routes-------------------------------------
buildings_data$nearby_route <- NA
for (i in seq(nrow(buildings))) {
  sloc <- buildings_data$location_value[i]
  sdis <- buildings_data$district_value[i]
  sarea <- buildings_data$area_value[i]
  sbldg <- buildings_data$building_value[i]
  url <- sprintf(paste0(
    "https://mobile.nwstbus.com.hk/text/getnearby.php?l=2&bldg=%s&location=%s&",
    "district=%s&area=%s&submitForm=bldg&",
    "locationValue=&districtValue=&areaValue=&mode=2"
  ), sbldg, sloc, sdis, sarea)
  route <- read_html(url) %>%
    html_table()
  route <- route[sapply(route, function(x) "路线" %in% names(x))]
  route <- do.call("bind_rows", route)
  routes <- route[["路线"]]
  routes <- paste(unique(routes), collapse = ";")
  buildings_data$nearby_route[i] <- routes
  cat(sprintf("finished %s\n", i))
}

library(xlsx)
write.xlsx(buildings_data, file = "buildings_route.xlsx", row.names(FALSE))
