library(tigris)
library(stringr)
library(jsonlite)

wd <- "C:/Users/Nickb/Box/ca_senate_sim"
setwd(wd)

ca <- tigris::state_legislative_districts(state = "CA",
                                          house = "upper",
                                          year = 2024) #change whenever redistricting happens
plot(ca$geometry)

ca <- ca %>%
  mutate(district = as.integer(str_extract(NAMELSAD, "\\d+$"))) %>% #pulling district num
  arrange(district)

districts <- seq(1:40)
districts <- paste0("district", districts, ".html") #connecting them to a district page

st_write(ca, "files/other/ca_senate_districts.geojson", driver = "GeoJSON")
