filter <- dplyr::filter

server <- function(input, output){
  sightings <- reactive({
    valid_hss <- filter(all_hotspots, dist < input $ radius) %>% `$`(locId)
    valid_species <- (species
      %>% filter(
          ignore_if(is.null(input $ species),
                    comName %in% input $ species),
          ignore_if(is.null(input $ family), family %in% input $ family),
          ignore_if(input $ regex == "",
                    str_detect(comName,
                               regex(input $ regex, ignore_case=TRUE))))
      %>% `$`(speciesCode))

    (all_sightings
      %>% filter(speciesCode %in% valid_species,
               locId %in% valid_hss,
               obsDt > today() - days(input $ history))
      %>% left_join(notables)
      %>% mutate(is_notable=coalesce(is_notable, FALSE)))
  })

  output $ hotspot <- renderUI({
    hsid <- input $ map_marker_click $ id
    `if`(is.null(hsid),
         "All Hotspots",
         hotspot_url(hsid, hsid2name(hsid))) %>%
      (tags $ h2)
  })

  output $ hotspot_sightings <- renderDataTable({
    hsid <- input $ map_marker_click $ id
    (sightings()
      %>% filter(`if`(is.null(hsid), TRUE, locId == hsid))
      %>% arrange(desc(is_notable), desc(obsDt))
      %>% mutate(Species=tags2char(species_url(speciesCode, comName)),
               Notable=ifelse(is_notable, "Yes", ""),
               Date=stamp("Sep 2")(obsDt))
      %>% select(Species, Date, Count=howMany, Notable))
  }, escape=FALSE, options=list(pageLength=15))

  hotspots <- reactive({
    x <- (sightings()
      %>% group_by(locId, locName, lat, lng)
      %>% summarise(individuals=sum(howMany, na.rm=TRUE),
                  species=n_distinct(speciesCode),
                  notables=paste(unique(tags2char(species_url(speciesCode[is_notable], comName[is_notable]))),
                                 collapse='<br>'))
      %>% mutate(label=sprintf("<b>%s (%d spcs., %d indv.)</b><br> %s",
                             tags2char(hotspot_url(locId, locName)),
                             species, individuals, notables)))
    x
  })

  output $ map <- renderLeaflet({
    pal <- function(individuals, species){
      x <- `if`(input $ color_by == "Species", species, individuals)
      colorBin(brewer.pal(7, "RdPu"), n=7, x)(x)
    }

    (hotspots()
      %>% arrange(species)
      %>% leaflet()
      %>% addProviderTiles("CartoDB.Voyager")
      %>% addCircleMarkers(
          ~ lng, ~ lat,
          popup=~ label,
          layerId=~ locId,
          # radius=2,
          fillOpacity=0.9,
          stroke=TRUE,
          color=~ pal(individuals, species)))
    ## clusterOptions=markerClusterOptions())
    })
}
