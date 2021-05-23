
# load required packages
if(!require(magrittr)) install.packages("magrittr", repos = "http://cran.us.r-project.org")
if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(maps)) install.packages("maps", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(reshape2)) install.packages("reshape2", repos = "http://cran.us.r-project.org")
if(!require(ggiraph)) install.packages("ggiraph", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(geojsonio)) install.packages("geojsonio", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
if(!require(tidyr)) install.packages("tidyr", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")
if(!require(patchwork)) install.packages("patchwork", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")


# set mapping colour for each outbreak
co2_yearly_col <- "#014419"
co2_cumulative_col <- "#011f44"
co2_percapita_col <- "#eb3443"


# import and process data for dashboard (YJ)----------------------------------------------------------------------
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
data_long$color <- leaflet::colorFactor(
  palette = "RdYlBu", domain = data_long$sources
)(data_long$sources)


# ==== Read countries map (CK) ==========
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


#===== Ploting Functions ========
# Cumulative plot
cumulative_plot = function(df, plot_date) {
  plot_df = subset(df, Year<=plot_date)
  g1 = ggplot(plot_df, aes(x = Year, y = cumulative_co2, color='Global')) + 
    geom_line() + geom_point(size = 1, alpha = 0.8) +
    ylab("CO2 (MT)") +  xlab("Date") + theme_bw() + labs(title="Cumulative") +
    scale_colour_manual(values=c(co2_yearly_col)) + 
    scale_y_continuous(labels = function(l) {trans = l / 1000000; paste0(trans, "M")}) +
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), 
          plot.margin = margin(5, 12, 5, 5))
  g1
}

# Yearly plot
yearly_plot = function(df, plot_date) {
  plot_df = subset(df, Year<=plot_date)
  g1 = ggplot(plot_df, aes(x = Year, y = co2, color='Global')) + 
    geom_line() + geom_point(size = 1, alpha = 0.8) +
    ylab("CO2 (MT/year)") +  xlab("Date") + theme_bw() + labs(title="Yearly") +
    scale_colour_manual(values=c(co2_yearly_col)) +
    scale_y_continuous(labels = function(l) {trans = l / 1000000; paste0(trans, "M")}) +
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), 
          plot.margin = margin(5, 12, 5, 5))
  g1
}

# ====== Map Plotting ==============================
map_plotting <- function(){
  plot_map <- worldcountry[worldcountry$ADM0_A3 %in% co2_large_countries$alpha3, ]
  
  # create base map 
  basemap <- leaflet(plot_map) %>% 
    addTiles() %>% 
    addLayersControl(
      position = "bottomright",
      overlayGroups = c("CO2 (Yearly)", "CO2 (Cumulative)"), 
      options = layersControlOptions(collapsed = FALSE)) %>% 
    hideGroup("CO2 (Cumulative)") %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    fitBounds(~-100,-60,~60,70) %>%
    addLegend("bottomright", pal = co2_pal, values = ~co2_large_countries$co2,
              title = "<small>CO2 Production</small>") 
return(basemap)
}
basemap<- map_plotting()


# ====== Sameer Part ==========
# Variables
co2_details <- co2 %>% 
                filter(country == 'World', Year >= 1959) %>% 
                select(c(Year, country, co2))

temperature <- read.csv("temperature.csv")
temp_detail <- temperature %>%
                filter(Year >= 1880) %>% 
                select(c(Year,No_Smoothing))
temp_det1 = temp_detail %>% 
              arrange(temp_detail$Year) %>% 
              na.omit(temp_detail) %>% 
              rename('Global Temperature (deg C)'=No_Smoothing)

co2_ppm <-  read.csv("co2ppm.csv")
co2_ppm <- co2_ppm %>%
            filter(year >= 1959) %>%
            select(c(year,co2ppm))
co2_ppm <- co2_ppm %>%
            rename('Year'=year)

# ------ Graphs --------

# Graph 1 : P1 & P2
globaltemp_plot1_func <- function(){
  co2_overyear_data <- left_join(co2_details, co2_ppm, by="Year")
  co2_overyear <- co2_overyear_data %>% 
    arrange(co2_overyear_data$Year) %>%
    na.omit(co2_overyear_data) %>%
    rename('CO2 Emission (million tonnes per year)'= co2,
           'CO2 (parts per million)'= co2ppm)
  
  
  # Left Plot
  p1 <- co2_overyear %>% 
    ggplot(aes(x = Year , y =`CO2 (parts per million)`)) + 
    geom_line(color = "green", size = 2) +
    labs(title="CO2 (PPM) level in atmosphere", 
         caption = "Source: : Global Monitoring Laboratory 
         \n https://gml.noaa.gov/ccgg/trends/data.html") +
    scale_x_continuous(breaks = seq(1959,2020,5), limits = c(1959,2020))+
    scale_y_continuous(breaks = seq(300,420,20), limits = c(300,420))+
    theme(panel.background = element_rect(fill = "#BFE3CD", 
                                          colour = "#6D9EC1", size = 2, 
                                          linetype = "solid"))
  
  # Right Plot
  p2 <- co2_overyear %>%
    ggplot(aes(x = Year , y =`CO2 Emission (million tonnes per year)`)) + 
    geom_line(color = "blue", size = 2) +
    labs(title="Annual production-based emissions of CO2", 
         caption = "Source: Our World in Data \n https://ourworldindata.org/co2-emissions") +
    scale_x_continuous(breaks = seq(1959,2020,5), limits = c(1959,2020))+
    scale_y_continuous(breaks = seq(5000,40000,5000), limits = c(5000,40000))+
    theme(panel.background = element_rect(fill = "#BFE3CD", 
                                          colour = "#6D9EC1",
                                          size = 2,
                                          linetype = "solid"))
  
  leftright_graph <- plot(p1+p2)
  leftright_graph
}
globaltemp_plot1_func()



#P3 This graph illustrates the change in global temperature (deg C) from 1880  to 2019.
globaltemp_plot2_func <- function(){
  p3 = temp_det1 %>% 
    ggplot(aes(x = Year , y =`Global Temperature (deg C)`)) + 
    geom_line(color = "red", size = 2) +
    labs(title="Change in global temperature (deg C) from 1880 to 2020",
         caption = "Source: Global Climate Change \n https://climate.nasa.gov/vital-signs/global-temperature/") +
    scale_x_continuous(breaks = seq(1880,2020,20), limits = c(1880,2020))+
    scale_y_continuous(breaks = seq(-0.5,1,0.1), limits = c(-0.5,1.1))+
    theme(panel.background = element_rect(fill = "#B0ECB0", colour = "#6D9EC1", size = 2, linetype = "solid"))
  p3
}




#========== SHINY UI ==========
ui <- bootstrapPage(
  tags$head(includeHTML("gtag.html")),
  navbarPage(theme = shinytheme("flatly"), collapsible = FALSE,
             HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" 
                  class="active" href="#">CO2 Tracker</a>'), id="nav",
             windowTitle = "CO2 TRACKER",
             # Global Map Tab
             tabPanel("CO2 World",
                      div(class="outer",
                          tags$head(includeCSS("styles.css")),
                          leafletOutput("mymap", width="100%", height="100%"),
                          
                          absolutePanel(id = "controls", class = "panel panel-default",
                                        top = 75, left = 55, width = 300, fixed=TRUE,
                                        draggable = TRUE, height = "auto",
                                        span(tags$i(h6("Global CO2 Emission")), style="color:#045a8d"),
                                        h6(strong(textOutput("reactive_co2")), align = "center"),
                                        h6(strong(textOutput("reactive_co2_cumulative")), align = "center"),
                                        br(),
                                        h5(strong(textOutput("clean_date_reactive")), align = "center"),
                                        # h6(textOutput("reactive_country_count"), align = "right"),
                                        br(),
                                        plotOutput("yearly_plot", height="130px", width="100%"),
                                        plotOutput("cumulative_plot", height="130px", width="100%"),
                                        
                                        sliderTextInput("plot_date",
                                                        label = h5("Select Year"),
                                                        choices = sort(unique(co2_map$Year)),
                                                        # choices = seq(min(co2_map$Year), max(co2_map$Year), 100),
                                                        selected = current_year,
                                                        grid = FALSE,
                                                        animate=animationOptions(interval = 2000, loop = FALSE)
                                        )
                          )
                      )
             ),
             
             # Dashboard Tab
             tabPanel("Sources of Emission",
                      titlePanel(""),
                      sidebarLayout(
                        position = "left",
                        sidebarPanel(
                          pickerInput("country", "Select country:", 
                                      choices = unique(intersect_country),
                                      selected = "Malaysia"),
                          sliderInput("minimum_year",
                                      "Minimum year:",
                                      min = 1970,
                                      max = 2015,
                                      value = 1970),
                          br(),
                          htmlOutput("selected_var"),
                          br(),
                          plotlyOutput('pie',width = "100%", height = "150%"),
                          sliderInput("pie_minimum_year",
                                      "Select year:",
                                      min = 1970,
                                      max = 2019,
                                      value = 1970,
                                      # choices = c(1970:2019),
                                      # selected = 1970,
                                      # grid = FALSE,
                                      animate=animationOptions(interval = 500, loop = FALSE))
                         ),
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Sources of Emission", 
                                     h6("PRIMARY SOURCES OF CO2 EMISSION PER CAPITA"),
                                     plotlyOutput("plot")),
                            tabPanel("Type of Emission", 
                                     h6("TYPE OF CO2 EMISSION"),
                                     plotlyOutput('percountry'))
                          )
                        )
                      )),
             
             # Sameer Part
             tabPanel("Atmospheric CO2",
                      titlePanel(""),
                      sidebarLayout(
                        position = "left", 
                        sidebarPanel(
                          width = 2,
                          h6("Relationship between CO2 & Global Temperature")
                        ),
                        mainPanel(
                          tabsetPanel(
                              # Tab 1
                              tabPanel("CO2 Concentration vs. CO2 Emission",
                                       h6(
                                          "The atmospheric level of carbon dioxide has been steadily rising since the 1960's. 
                                          In 2019, carbon dioxide levels reached 411 parts per million, 
                                          in comparison to 1960 levels which stood at about 317 parts per million. 
                                          Projections for 2020 show concentrations of carbon dioxide have increased to 414 parts per million. 
                                          Emissions of carbon dioxide largely come from human activities such as burning fossil fuels and deforestation. 
                                          Data is taken from Mauna Loa CO2 annual mean data. Data has been measured at Mauna Loa Observatory, 
                                          Hawaii as it constitutes the longest record of direct carbon dioxide measurements in the atmosphere."
                                       ),
                                       h6(
                                          "Global climate change is mostly triggered by
                                          carbon dioxide emissions.
                                          It’s widely recognized that to avoid the worst impacts of climate change, 
                                          the world needs to urgently reduce emissions. This graph illustrates the change in Carbon Dioxide (CO2)
                                          emission based on annual production from 1959 to 2019. CO2 emissions data from Our World 
                                          in Data and the Global Carbon Project."
                                       ),
                                       br(),
                                       plotOutput("globaltemp_plot1")),
                              # Tab 2
                              tabPanel("Global Temperature",
                                       h6(
                                       "This graph illustrates the change in global temperature (deg C) from 1880 to 2019. 
                                       Nineteen of the warmest years have occurred since 2000, with the exception of 1998. 
                                       The year 2020 tied with 2016 for the warmest year on record since record-keeping began in 1880."
                                       ),
                                       h6(
                                       "Data is taken from Global Climate Change."
                                       ),
                                       br(),
                                       plotOutput('globaltemp_plot2'))
                          )
                        ))),
             
             tabPanel("User Guide",
                      titlePanel("Global CO2 Tracker"), 
                      
                      h5("Global CO2 Tracker (GCT) is an online web app that provides data and dashboard for monitoring global CO2 emission. It uses data to illuminate the state of CO2 level worldwide and tells the stories of how CO2 contributed by each countries. GCT allows anyone to access information about where and how CO2 emission are changing around the world.
"),
                      br(),
                      sidebarLayout(
                        position = "left",
                        sidebarPanel(
                        
                        h4("Overview"),
                        h5("The map & dashboards on GCT allow you to explore hundreds of spatial datasets that 
                        help explain when, where and how CO2 emission are changing around the world."),
                        
                        h5(" The map tell a visual story about how CO2 emmission is changing in a particular place. 
                          The dashboards for sources of CO2 emission help answer important questions about CO2 change in any country and enable you to view
                           statistics through interactive charts and graphs."),
                       h5("Let's get started with our step-by-step instructions.") ),
                         
                      
                        mainPanel(
                          tabsetPanel(
                            tabPanel("CO2 World Map", 
                                     h3("Use the map"),
                                     h5("The map allows you to visualize and analyze spatial data. 
                                     Below you'll find key functionalities to help you explore the 
                                     map visualizations using the function available on the map."),
                                     img(src = "co2worldmap_illustration.png", width = 1000),
                                     br(),
                                     br(),
                                     strong(":: Yearly/ Cumulative CO2 Emission"),
                                     h5("Select to display yearly or cumulative CO2 emission on the map. 
                                        Yearly CO2 Emission is indicated by green bubble while cumulative CO2 Emission is indicated by blue bubble. 
                                        The size of the bubble show the amount of CO2 emission."),
                                     strong(":: Adjust Display Year"),
                                     h5("Adjust the slider to select year for visualization.
                                        Once a year range had been selected, visualization on the map will adjusted accordingly
                                        with CO2 data up until the year selected."),
                                     strong(":: Play Button"),
                                     h5("Click the play button to see how CO2 emission changes across the year globally.")
                                     ),
                                     
                            tabPanel("Sources of Emission", 
                                     h3("Use the charts"),
                                     h5("The charts allow you to visualize and analyze sources of CO2 emission. 
                                     Below you'll find key functionalities to help you explore the charts."),
                                     img(src = "dashboard_illustration.png", width = 1000),
                                     br(),
                                     br(),
                                     strong(":: Dropdown for Country Selection"),
                                     h5("Select a country to further analyze on its sources of emission. All the display charts will be adjusted based on country selected.
                                        This country selection apply to all the charts in display."),
                                     strong(":: Adjust Display Year"),
                                     h5("Adjust the slider to select year range for visualization.
                                        Once a year range had been selected, visualization on the charts will adjusted accordingly
                                        with CO2 data.
                                        This year slider only apply to chart on [sources of emission] and [type of emission]."),
                                     strong(":: Select Type of Charts"),
                                     h5("Switch between different tabs to view sources of emission or type of emission (production emission vs consumption emission) 
                                        for a selected country."),
                                     strong(":: Summary Text"),
                                     h5("The largest sources of cumulative emission displayed in text for selected country."),
                                     strong(":: Play Button for Cumulative Proportion"),
                                     h5("This play button is associated only with donut chart on [Cumulative Proportion of CO2 Emission Sources]. Click the play button to see 
                                        how the cumulative CO2 emission proportion from each sources changes across the year on selected country.")
                                     )
                          )
                        )
                      )
                    )
             
             ))
 


### SHINY SERVER ###

server = function(input, output, session) {
 # ===== Server for Global Map (CK) =======================
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
    paste0(paste("Yearly:  ", prettyNum(reactive_db_world()$co2, big.mark=","), sep = '\n'), " MT/year")
  })
  
  output$reactive_co2_cumulative <- renderText({
    paste0(paste("Cumulative:  ", prettyNum(reactive_db_world()$cumulative_co2, big.mark=","), sep='\n'), " MT")
  })
  
  
  output$mymap <- renderLeaflet({ 
    basemap
  })
  
  observeEvent(input$plot_date, {
    leafletProxy("mymap") %>%
      clearMarkers() %>%
      clearShapes() %>%
      
      # for country border
      addPolygons(data = reactive_polygons(),
                  stroke = FALSE,
                  smoothFactor = 0.1,
                  fillOpacity = 0.15,
                  fillColor = ~co2_pal(reactive_db_large()$cumulative_co2)
      ) %>%

      # Yearly
      addCircleMarkers(data = reactive_db(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(co2)^(1/2.5),
                       fillOpacity = 0.2, color = co2_yearly_col, group = "CO2 (Yearly)", 
                       label = sprintf(
                         "<strong> %s (MT/year)</strong><br/>
                                       CO2: %.0f<br/>
                                       CO2 per capita: %s<br/>",
                         reactive_db()$country,
                         reactive_db()$co2,
                         reactive_db()$co2_per_capita) %>% 
                         lapply(htmltools::HTML),
                       labelOptions = labelOptions(
                         style = list("font-weight" = "normal",
                                      padding = "3px 8px",
                                      "color" = co2_yearly_col),
                         textsize = "15px",
                         direction = "auto")) %>%
      
      # Cumulative
      addCircleMarkers(data = reactive_db(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(cumulative_co2)^(1/3),
                       fillOpacity = 0.2, color = co2_cumulative_col, group = "CO2 (Cumulative)",
                       label = sprintf(
                         "<strong> %s (MT)</strong><br/>
                                       Cumulative CO2: %.0f <br/>",
                         reactive_db()$country,
                         reactive_db()$cumulative_co2) %>%
                          lapply(htmltools::HTML),
                       labelOptions = labelOptions(
                         style = list("font-weight" = "normal",
                                      padding = "3px 8px",
                                      "color" = co2_cumulative_col),
                         textsize = "15px",
                         direction = "auto"))
  })
  
  output$cumulative_plot <- renderPlot({
    cumulative_plot(co2_world, formatted_date())
  })
  
  output$yearly_plot <- renderPlot({
    yearly_plot(co2_world, formatted_date())
  })
 
  
  ## Server for Dashboard (YJ) ---------------------------------------------------------------------------------------
  output$plot <- renderPlotly(
    data_long[which(data_long$Year>=input$minimum_year),] %>% filter(country == input$country) %>% 
      
      ggplot(aes(x=Year, y=emission_per_capita, fill=reorder(sources, -emission_per_capita))) +
      ylab(label = 'CO2 Emission per capita (MT)') +
      geom_area()+
      labs(fill = "Sources") +
      theme_bw() + 
      theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
    
  )
  
  # output$pie <- renderPlot(
  #  data_long %>% filter(country == input$country, Year==2019, emission_per_capita!=0) %>%
  #  ggplot(aes(x=2, y=emission_per_capita, fill=reorder(sources, -emission_per_capita))) +
  #    geom_bar(stat="identity", color="grey") +
  #    geom_text(aes(label = paste0((round(((emission_per_capita/sum(emission_per_capita))*100),digits=1)), "%")), position = position_stack(vjust=0.5))+
  #    coord_polar(theta="y", start=0) +
  #    labs(fill = "Sources") +
  #    xlim(0.5,2.5)+
  #    theme_void()
  #   )
  
  output$pie <- renderPlotly(
    
    data_long %>% 
      filter(country == input$country, Year<=input$pie_minimum_year, emission_per_capita!=0) %>%
      group_by(sources)%>%
      mutate(cumsum=cumsum(emission_per_capita))%>%
      filter(Year==input$pie_minimum_year)%>%
      plot_ly(labels = ~sources, values = ~cumsum, marker = list(colors=~color)) %>% 
      add_pie(hole = 0.6)%>% 
      layout(title = " CUMULATIVE PROPORTION OF CO2 EMISSION SOURCES",  showlegend = T,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             autosize=F,
             width=500,
             height=330,
             margin=list(
               l=50,
               r=50,
               b=10,
               t=40,
               pad=5)
     
  ))

  
  output$percountry = renderPlotly(
    
    con_pro %>%
      filter(Country == input$country, Record %in% c('CBA_MtCO2perCap', 'PBA_MtCO2perCap')) %>%
      gather(key = 'Year', value = 'Emission', -c(1:2)) %>%
      mutate(Year = as.numeric(substr(Year, 2, 5))) %>%
      mutate(Emission=as.numeric(Emission))%>%
      filter(Year>input$minimum_year) %>%
      mutate(Record = ifelse(Record == 'CBA_MtCO2perCap', 'Consumption', 'Production')) %>%
      
      ggplot(aes(x = Year, y = Emission, color = Record)) +
      geom_line(size = 2) +
      ylab(label = 'CO2 Emission per capita (MT)') +
      scale_color_discrete(name = 'Emission Type') +
      theme_bw() + 
      theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
    
  )
  
  output$selected_var <- renderText({ 
    
    perct<-data_long %>% 
      filter(country == input$country, emission_per_capita!=0)%>%
      group_by(sources)%>%
      mutate(cumsum=cumsum(emission_per_capita))%>%
      filter(Year==2019)%>%
      ungroup(sources)%>%
      mutate(pect=round((cumsum/sum(cumsum))*100,digits=2))
      max_sources<-perct$sources[which.max(perct$pect)]
      max_perct<-perct$pect[which.max(perct$pect)]
    paste("As of 2019, ", "<b>",max_perct,"</b>", "% of co2 was contributed by ","<b>",sub("_.*", "", max_sources),"</b>"," as the largest sources of co2 emission in", "<b>",input$country,"</b>",".")
  })
  
  
  output$globaltemp_plot1 <- renderPlot({
    globaltemp_plot1_func()
  })

  output$globaltemp_plot2 <- renderPlot({
    globaltemp_plot2_func()
  })
  
}

#runApp(shinyApp(ui, server), launch.browser = TRUE)
shinyApp(ui, server)
#library(rsconnect)
#deployApp(account="vac-lshtm")

