library(geosphere)
library(lubridate)
library(rebird)
library(tidyverse)

my_lat <- 42.3601
my_lng <- -71.0589

hotspots <- read_csv("hotspots.csv", col_names=FALSE)
names(hotspots) <- c("locId", "reg1", "reg2", "reg3",
                     "lat", "lng", "locName", "last", "species")

x <- hotspots %>% mutate(dist=distHaversine(c(my_lng, my_lat), cbind(lng, lat)) / 1000)

get_hotspot <- function(hs){
  print(sprintf("Pulling hotspot %s...", hs))
  ebirdregion(loc=hs, provisional=TRUE)
}

target_hotspots <- (hotspots
  %>% mutate(dist=distHaversine(c(my_lng, my_lat), cbind(lng, lat)) / 1000)
  %>% dplyr::filter(dist < 100)
  %>% group_by(reg3)
  %>% dplyr::filter(rank(desc(species)) < 40))

sightings <- (target_hotspots
  %>% `$`(locId)
  %>% map(get_hotspot)
  %>% bind_rows)

notables <- ebirdnotable(locID='US-MA', hotspot=TRUE, back=14, provisional=TRUE)

write_csv(sightings, "sightings.csv")
write_csv(notables, "notables.csv")
write_csv(target_hotspots, "target_hotspots.csv")
