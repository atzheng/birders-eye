ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      paste("Last updated", max(all_sightings $ obsDt)),
      tags $ h2("Filters"),
      selectInput("species",
        "Species",
        sort(species $ comName),
        multiple=TRUE),
      selectInput("family",
        "Family",
        sort(unique(species $ family)),
        multiple=TRUE),
      textInput("regex", "Species Regex"),
      sliderInput("radius", "Show hotspots within radius (km)", min=0,
                  max=max_dist, value=max_dist),
      sliderInput("history", "Show sightings within days", min=0,
                  max=14, value=7),
      radioButtons("color_by", "Color by",
                   choices=c("Individuals", "Species"), selected="Species"),
      htmlOutput("hotspot"),
      dataTableOutput("hotspot_sightings")
    ),
    mainPanel(leafletOutput(outputId="map", height=1000))))

