library(leaflet)
library(lubridate)
library(htmltools)
library(shiny)
library(DT)
library(tidyverse)
library(RColorBrewer)

all_sightings <- read_csv('sightings.csv')
notables <- read_csv('notables.csv') %>%
  select(speciesCode, locId) %>% mutate(is_notable=TRUE) %>% unique
all_hotspots <- read_csv("target_hotspots.csv")
max_dist <- ceiling(max(all_hotspots $ dist))

species <- (read_csv("eBird_Taxonomy_v2019.csv")
  %>% select(family=FAMILY, speciesCode=SPECIES_CODE,
           comName=PRIMARY_COM_NAME)
  %>% inner_join(select(all_sightings, speciesCode)))

ignore_if <- function(ignore, value, len=NULL, default=TRUE){
  len <- `if`(is.null(len), length(value), len)
  `if`(ignore, rep(default, len), value)
}

hsid2name <- function(id)
  all_hotspots %>% filter(locId == id) %>% `$`(locName)

ebird_url <- function(endpoint, id, name){
  href <- sprintf("https://ebird.org/%s/%s", endpoint, id)
  map2(name, href, ~ tags $ a(.x, href=.y))
}

species_url <- partial(ebird_url, "species")
hotspot_url <- partial(ebird_url, "hotspot")
tags2char <- function(x) map(x, doRenderTags) %>% unlist
