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
co2_percapita_col <- "#eb3443"

## --- new

# ===== Ploting Functions ============================
# Cumulative plot
cumulative_plot = function(df, plot_date) {
  plot_df = subset(df, Year<=plot_date)
  g1 = ggplot(plot_df, aes(x = Year, y = cumulative_co2, color='Global')) + geom_line() + geom_point(size = 1, alpha = 0.8) +
    ylab("CO2 (Cumulative)") +  xlab("Date") + theme_bw() + 
    scale_colour_manual(values=c(co2_yearly_col)) +
    scale_y_continuous(labels = function(l) {trans = l / 1000000; paste0(trans, "M")}) +
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), 
          plot.margin = margin(5, 12, 5, 5))
  g1
}

# Yearly plot
yearly_plot = function(df, plot_date) {
  plot_df = subset(df, Year<=plot_date)
  g1 = ggplot(plot_df, aes(x = Year, y = co2, color='Global')) + geom_line() + geom_point(size = 1, alpha = 0.8) +
    ylab("CO2 (Yearly)") +  xlab("Date") + theme_bw() + 
    scale_colour_manual(values=c(co2_yearly_col)) +
    scale_y_continuous(labels = function(l) {trans = l / 1000000; paste0(trans, "M")}) +
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), 
          plot.margin = margin(5, 12, 5, 5))
  g1
}

# Read for countries 
countries = read.csv("input_data/countries_codes_and_coordinates.csv")
worldcountry = geojson_read("input_data/50m.geojson", what = "sp")
current_year <- max(co2$Year) 


# co2 absolute, co2 per capita, co2 cumulative 
co2_map <- co2 %>%
    select(Year, country, co2, co2_per_capita, cumulative_co2)


# merge countries
co2_map <- merge(co2_map, countries, by = "country")


# select large countries for mapping polygons
co2_large_countries = co2_map %>% filter(alpha3 %in% worldcountry$ADM0_A3)
if (all(co2_large_countries$alpha3 %in% worldcountry$ADM0_A3)==FALSE) { print("Error: inconsistent country names")}
co2_large_countries = co2_large_countries[order(co2_large_countries$alpha3),]

# create plotting parameters for map
bins = c(0,10,50,100,500,1000,Inf)
co2_pal <- colorBin("Greens", domain=co2_large_countries$co2, bins = bins)

#World subset
co2_world <- co2 %>% filter(country=='World')

#World subset
co2_world <- co2 %>% filter(country=='World')

#World subset
co2_world <- co2 %>% filter(country=='World')

#World subset
co2_world <- co2 %>% filter(country=='World')

#World subset
co2_world <- co2 %>% filter(country=='World')


# ====== Map Plotting ==============================
map_plotting <- function(){

  plot_map <- worldcountry[worldcountry$ADM0_A3 %in% co2_large_countries$alpha3, ]
  
  # create cv base map 
  basemap <- leaflet(plot_map) %>% 
    addTiles() %>% 
    addLayersControl(
      position = "bottomright",
      overlayGroups = c("CO2 (Yearly)", "CO2 (Cumulative)"), 
      options = layersControlOptions(collapsed = FALSE)) %>% 
    hideGroup(c("CO2 (Cumulative)")) %>% 
    addProviderTiles(providers$CartoDB.Positron) %>%
    fitBounds(~-100,-60,~60,70) %>%
    addLegend("bottomright", pal = co2_pal, values = ~co2_large_countries$co2,
              title = "<small>CO2 Production</small>") 
  return(basemap)
}
basemap<- map_plotting()

<<<<<<< HEAD

# =========SHINY UI =====================
co2_text <- expression("CO"[2])
co2_text

# =========SHINY UI =====================

co2_text <- expression("CO"[2])
co2_text

# =========SHINY UI =====================

co2_text <- expression("CO"[2])
co2_text

# =========SHINY UI =====================

=======
co2_text <- expression("CO"[2])
co2_text

a# =========SHINY UI =====================
>>>>>>> 845d2ad0e82343edd9df17fb5be16dc496298d68
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
                                        span(tags$i(h6("Global CO2 Emission")), style="color:#045a8d"),
<<<<<<< HEAD

                                        h6(textOutput("reactive_co2"), align = "left"),
                                        h6(textOutput("reactive_co2_cumulative"), align = "left"),
                                        h6(strong(textOutput("clean_date_reactive"), align = "center")),

                                        h5(textOutput("reactive_co2"), align = "left"),
                                        h5(textOutput("reactive_co2_cumulative"), align = "left"),
                                        h6(textOutput("clean_date_reactive"), align = "center"),

                                        h5(textOutput("reactive_co2"), align = "left"),
                                        h5(textOutput("reactive_co2_cumulative"), align = "left"),
                                        h6(textOutput("clean_date_reactive"), align = "center"),

                                        h5(textOutput("reactive_co2"), align = "left"),
                                        h5(textOutput("reactive_co2_cumulative"), align = "left"),
                                        h6(textOutput("clean_date_reactive"), align = "center"),

=======
                                        h5(textOutput("reactive_co2"), align = "left"),
                                        h5(textOutput("reactive_co2_cumulative"), align = "left"),
                                        h6(textOutput("clean_date_reactive"), align = "center"),
>>>>>>> 845d2ad0e82343edd9df17fb5be16dc496298d68
                                        h6(textOutput("reactive_country_count"), align = "right"),
                                        plotOutput("yearly_plot", height="130px", width="100%"),
                                        plotOutput("cumulative_plot", height="130px", width="100%"),
                                        
                                        sliderTextInput("plot_date",
                                                        label = h5("Select mapping date"),
                                                        choices = sort(unique(co2_map$Year)),
                                                        selected = current_year,
                                                        grid = FALSE,
                                                        animate=animationOptions(interval = 3000, loop = FALSE)
                                                        )
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
  
  reactive_db_world = reactive({
    co2 %>% filter(Year == formatted_date(), country=='World')
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

  output$reactive_co2 <- renderText({
    paste0(paste("Yearly: ", prettyNum(reactive_db_world()$co2, big.mark=","), sep = '\n'), " MT")
<<<<<<< HEAD

  })

  output$reactive_co2_cumulative <- renderText({
    paste0(paste("Cumulative: ", prettyNum(reactive_db_world()$cumulative_co2, big.mark=","), sep='\n'), " MT")
  })

  })

  output$reactive_co2_cumulative <- renderText({
    paste0(paste("Cumulative: ", prettyNum(reactive_db_world()$cumulative_co2, big.mark=","), sep='\n'), " MT")
  })

=======
>>>>>>> 845d2ad0e82343edd9df17fb5be16dc496298d68
  })

  output$reactive_co2_cumulative <- renderText({
    paste0(paste("Cumulative: ", prettyNum(reactive_db_world()$cumulative_co2, big.mark=","), sep='\n'), " MT")
  })
<<<<<<< HEAD
=======

>>>>>>> 845d2ad0e82343edd9df17fb5be16dc496298d68

  output$mymap <- renderLeaflet({ 
    basemap
  })
  
  observeEvent(input$plot_date, {
    leafletProxy("mymap") %>%
      clearMarkers() %>%
      clearShapes() %>%

    # Yearly
     addCircleMarkers(data = reactive_db(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(co2)^(1/2.5),
                      fillOpacity = 0.1, color = co2_yearly_col, group = "CO2 (Yearly)", 
                      label = sprintf(
                      "<strong> %s (MT)</strong><br/>
                                       CO2: %g<br/>
                                       CO2 per capita: %g<br/>",
                      reactive_db()$country,
                      reactive_db()$co2,
                      reactive_db()$co2_per_capita) %>%
                        lapply(htmltools::HTML),
                      labelOptions = labelOptions(
                        style = list("font-weight" = "normal",
                                     padding = "3px 8px",
                                     "color" = co2_yearly_col),
                        textsize = "15px",
                        direction = "auto",
                                                
                                    )
      ) %>%
      
      # Cumulative
      addCircleMarkers(data = reactive_db(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(cumulative_co2)^(1/4),
                       fillOpacity = 0.2, color = co2_cumulative_col, group = "CO2 (Cumulative)",
                       label = sprintf(
                         "<strong> %s (MT)</strong><br/>
                                       CO2 Cumulative: %s <br/>",
                         reactive_db()$country,
                         reactive_db()$cumulative_co2) %>%
                         lapply(htmltools::HTML),
                       labelOptions = labelOptions(
                         style = list("font-weight" = "normal",
                                      padding = "3px 8px",
                                      "color" = co2_cumulative_col),
                         textsize = "15px",
                         direction = "auto",
                         
                       )
      ) %>%

    
    addPolygons(data = reactive_polygons(),
                stroke = FALSE,
                smoothFactor = 0.1,
                fillOpacity = 0.15,
                fillColor = ~co2_pal(reactive_db_large()$cumulative_co2)
                )
  })
  
  output$cumulative_plot <- renderPlot({
    cumulative_plot(co2_world, formatted_date())
  })
  
  output$yearly_plot <- renderPlot({
    yearly_plot(co2_world, formatted_date())
  })
  
}

shinyApp(ui, server)
# z
<<<<<<< HEAD

# # paste0(prettyNum(z$co2, big.mark=","), " MT CO2-eq")
# 
# z <- filter(co2_world, Year==2019)
# # z <- paste0(prettyNum(z, co2$cumulative_co2, big.mark=","), " MT CO2-eq")c
# co2_map %>% filter(Year==1900)
# 
# # 
# # print("hi \n hi")
# # cat('hi', 'la', sep ="\n")
# # paste(name,  address, cityState, sep="\n")
# 
# sort(unique(co2$Year))


=======
# paste0(prettyNum(z$co2, big.mark=","), " MT CO2-eq")

# z <- filter(co2_world, Year==2019)
# z <- paste0(prettyNum(z, co2$cumulative_co2, big.mark=","), " MT CO2-eq")
# z
# 
# print("hi \n hi")
# cat('hi', 'la', sep ="\n")
# paste(name,  address, cityState, sep="\n")
>>>>>>> 845d2ad0e82343edd9df17fb5be16dc496298d68
