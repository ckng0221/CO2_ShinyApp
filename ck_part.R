# import data -- CK
library(dplyr)

co2<-read.csv("co2.csv")
con_pro<-read.csv('con_pro.csv')
co2$cement_co2_per_capita[is.na(co2$cement_co2_per_capita)] <- 0
co2$coal_co2_per_capita[is.na(co2$coal_co2_per_capita)] <- 0
co2$flaring_co2_per_capita[is.na(co2$flaring_co2_per_capita)] <- 0
co2$gas_co2_per_capita[is.na(co2$gas_co2_per_capita)] <- 0
co2$oil_co2_per_capita[is.na(co2$oil_co2_per_capita)] <- 0
co2$other_co2_per_capita[is.na(co2$other_co2_per_capita)] <- 0
data_long <- gather(co2, sources, emission_per_capita, cement_co2_per_capita:other_co2_per_capita, factor_key=TRUE)
data2_long<- gather(data_long, emission_type, annual_emission, co2, consumption_co2, factor_key=TRUE)
intersect_country<-intersect(unique(co2$country),unique(con_pro$Country))

co2_yearly_col <- "#014419"
co2_cumulative_col <- "#011f44"

## --- new

# Read for countries 
countries = read.csv("input_data/countries_codes_and_coordinates.csv")
worldcountry = geojson_read("input_data/50m.geojson", what = "sp")
current_year <- max(co2$Year) 

  
# country_geoms = read.csv("input_data/country_geoms.csv") # pending

# for CO2 absolute only
colnames(co2)

# co2 absolute, co2 per capita, co2 cumulative 
co2_map <- co2 %>%
    select(Year, country, co2, co2_per_capita, cumulative_co2)

# co2 - per capita 
co2_map

# merge countries
co2_map <- merge(co2_map, countries, by = "country")

# select large countries for mapping polygons
co2_large_countries = co2_map %>% filter(alpha3 %in% worldcountry$ADM0_A3)
if (all(co2_large_countries$alpha3 %in% worldcountry$ADM0_A3)==FALSE) { print("Error: inconsistent country names")}
co2_large_countries = co2_large_countries[order(co2_large_countries$alpha3),]

# create plotting parameters for map
bins = c(0,10,50,100,500,1000,Inf)
co2_pal <- colorBin("Greens", domain = co2_large_countries$cases_per_million, bins = bins)


# ====== Map Plotting ==============================
map_plotting <- function(){

  plot_map <- worldcountry[worldcountry$ADM0_A3 %in% co2_large_countries$alpha3, ]
  
  # create cv base map 
  basemap <- leaflet(plot_map) %>% 
    addTiles() %>% 
    addLayersControl(
      position = "bottomright",
      overlayGroups = c("CO2 (Yearly)", "CO2 per Capita", "CO2 (Cumulative)"), 
      options = layersControlOptions(collapsed = FALSE)) %>% 
    hideGroup(c("CO2 (Cumulative)", "CO2 per Capita")) %>% 
    addProviderTiles(providers$CartoDB.Positron) %>%
    fitBounds(~-100,-60,~60,70) %>%
    addLegend("bottomright", pal = co2_pal, values = ~co2_large_countries$deaths_per_million,
              title = "<small>CO2 per capita</small>") 
  return(basemap)
}
basemap<- map_plotting()

co2_pal

### SHINY UI ###
ui <- bootstrapPage(
  tags$head(includeHTML("gtag.html")),
  navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
             HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;"
                  class="active" href="#">CO2 Tracker</a>'), id="nav",
             windowTitle = "CO2 TRACKER",
    # tab
             tabPanel("CO2 World",
                      div(class="outer",
                          tags$head(includeCSS("styles.css")),
                          leafletOutput("mymap", width="100%", height="100%"),
                          
                          absolutePanel(id = "controls", class = "panel panel-default",
                                        top = 75, left = 55, width = 250, fixed=TRUE,
                                        draggable = TRUE, height = "auto",
                                        
                                        span(tags$i(h6("CO2 for Countries")), style="color:#045a8d"),
                                        h3(textOutput("reactive_case_count"), align = "right"),
                                        h4(textOutput("reactive_death_count"), align = "right"),
                                        h6(textOutput("clean_date_reactive"), align = "right"),
                                        h6(textOutput("reactive_country_count"), align = "right"),
                                        plotOutput("epi_curve", height="130px", width="100%"),
                                        plotOutput("cumulative_plot", height="130px", width="100%"),
                                        
                                        sliderTextInput("plot_date",
                                                        label = h5("Select mapping date"),
                                                        choices = sort(unique(co2_map$Year)),
                                                        selected = current_year,
                                                        grid = FALSE,
                                                        animate=animationOptions(interval = 3000, loop = FALSE)
                                                        )
                                                        # min = min(co2_map$Year),
                                                        # max = max(co2_map$Year)
                      )
             )
  )
  )
)


server = function(input, output, session) {
  
  # CO2 date
  formatted_date = reactive({
    input$plot_date
  })
  
  output$clean_date_reactive <- renderText({
   formatted_date()
  })
  
  reactive_db = reactive({
    co2_map %>% filter(Year == formatted_date())
  })
  
  
  reactive_db_large = reactive({
    large_countries = reactive_db() %>% filter(alpha3 %in% worldcountry$ADM0_A3)
    worldcountry_subset = worldcountry[worldcountry$ADM0_A3 %in% large_countries$alpha3, ]
    large_countries = large_countries[match(worldcountry_subset$ADM0_A3, large_countries$alpha3),]
    large_countries
  })
  
  
  reactive_polygons = reactive({
    worldcountry[worldcountry$ADM0_A3 %in% reactive_db_large()$alpha3, ]
  })

  output$reactive_case_count <- renderText({
    paste0(prettyNum(sum(reactive_db()$cases), big.mark=","), " MT CO2-eq")
  })
  

  output$mymap <- renderLeaflet({ 
    basemap
  })
  
  observeEvent(input$plot_date, {
    leafletProxy("mymap") %>%
      clearMarkers() %>%
      clearShapes() %>%

      # Cumulative
      addCircleMarkers(data = reactive_db(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(cumulative_co2)^(1/4),
                       fillOpacity = 0.2, color = co2_cumulative_col, group = "CO2 (Cumulative)") %>%
                       # label = sprintf(
                       #   "<strong> %s (Cumulative)</strong><br/>
                       #                 CO2 Cumulative: %s <br/>",
                       #                 reactive_db()$country,
                       #                 reactive_db()$co2 %>%
                       #                      lapply(htmltools::HTML),
                       #                         labelOptions = labelOptions(
                       #                           style = list("font-weight" = "normal",
                       #                                        padding = "3px 8px",
                       #                                        "color" = covid_col),
                       #                           textsize = "15px",
                       #                           direction = "auto",
                       #                           )
                       #                         )

    # Yearly
     addCircleMarkers(data = reactive_db(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(co2)^(1/2),
                      fillOpacity = 0.1, color = co2_yearly_col, group = "CO2 (Yearly)"
      ) %>%
    
    # CO2_capita
    addCircleMarkers(data = reactive_db(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(co2_per_capita)^(1),
                     fillOpacity = 0.1, color = co2_yearly_col, group = "CO2 per Capita"
    ) %>%
    
    addPolygons(data = reactive_polygons(),
                stroke = FALSE,
                smoothFactor = 0.1,
                fillOpacity = 0.15,
                fillColor = ~co2_pal(reactive_db_large()$cumulative_co2)
                )
      
    
  }
  )
  }


shinyApp(ui, server)

# countries %>% filter(country=='USA')
