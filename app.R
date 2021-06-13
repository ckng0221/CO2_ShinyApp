
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
if(!require(hash)) install.packages("hash", repos = "http://cran.us.r-project.org")

# set mapping colour for each outbreak
co2_yearly_col <- "#014419"
co2_cumulative_col <- "#011f44"
co2_percapita_col <- "#eb3443"
co2_html <- HTML(paste0("CO",tags$sub("2 ")))

# import and process data for dashboard (YJ)----------------------------------------------------------------------
co2<-read.csv("input_data/co2.csv")
con_pro<-read.csv('input_data/con_pro.csv')
co2$cement_co2_per_capita[is.na(co2$cement_co2_per_capita)] <- 0
co2$coal_co2_per_capita[is.na(co2$coal_co2_per_capita)] <- 0
co2$flaring_co2_per_capita[is.na(co2$flaring_co2_per_capita)] <- 0
co2$gas_co2_per_capita[is.na(co2$gas_co2_per_capita)] <- 0
co2$oil_co2_per_capita[is.na(co2$oil_co2_per_capita)] <- 0
co2$other_co2_per_capita[is.na(co2$other_co2_per_capita)] <- 0
names(co2)[names(co2) == "cement_co2_per_capita"] <- "Cement"
names(co2)[names(co2) == "coal_co2_per_capita"] <- "Coal"
names(co2)[names(co2) == "flaring_co2_per_capita"] <- "Flaring"
names(co2)[names(co2) == "gas_co2_per_capita"] <- "Gas"
names(co2)[names(co2) == "oil_co2_per_capita"] <- "Oil"
names(co2)[names(co2) == "other_co2_per_capita"] <- "Other"
data_long <- gather(co2, sources, emission_per_capita, Cement:Other, factor_key=TRUE)
data2_long<- gather(data_long, emission_type, annual_emission, co2, consumption_co2, factor_key=TRUE)
intersect_country<-intersect(unique(co2$country),unique(con_pro$Country))
data_long$color <- leaflet::colorFactor(
  palette = "PiYG", domain = data_long$sources
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

library(ggtext)
#===== Plotting Functions ========
# Cumulative plot
cumulative_plot = function(df, plot_date) {
  plot_df = subset(df, Year<=plot_date)
  g1 = ggplot(plot_df, aes(x = Year, y = cumulative_co2, color='Global')) + 
    geom_line() + geom_point(size = 1, alpha = 0.8) +
    ylab(bquote(~CO[2]~ "(mil tonnes)")) +  xlab("Date") + theme_bw() + labs(title="Cumulative") +
    scale_colour_manual(values=c(co2_yearly_col)) + 
    scale_y_continuous(labels = function(l) {trans = l / 1000000; paste0(trans, "M")}) +
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=12), 
          plot.margin = margin(5, 12, 5, 5))
  g1
}

# Yearly plot
yearly_plot = function(df, plot_date) {
  plot_df = subset(df, Year<=plot_date)
  g1 = ggplot(plot_df, aes(x = Year, y = co2, color='Global')) + 
    geom_line() + geom_point(size = 1, alpha = 0.8) +
    ylab(bquote(~CO[2]~ '(mil tonnes/year)')) +  xlab("Date") + theme_bw() + labs(title="Yearly") +
    scale_colour_manual(values=c(co2_yearly_col)) +
    scale_y_continuous(labels = function(l) {trans = l / 1000000; paste0(trans, "M")}) +
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=12), 
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
      overlayGroups = c(HTML("CO<sub>2</sub> (Yearly)"), HTML("CO<sub>2</sub> (Cumulative)")), 
      options = layersControlOptions(collapsed = FALSE)) %>% 
    hideGroup(HTML("CO<sub>2</sub> (Cumulative)")) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    fitBounds(~-100,-60,~60,70) %>%
    addLegend("bottomright", pal = co2_pal, values = ~co2_large_countries$co2,
              title = "<small>CO<sub>2</sub> Production <br>(mil tonnes/year)</small>") 
return(basemap)
}
basemap<- map_plotting()


# ====== Sameer Part ==========
# Variables
co2_details <- co2 %>% 
                filter(country == 'World', Year >= 1959) %>% 
                select(c(Year, country, co2))

temperature <- read.csv("input_data/temperature.csv")
temp_detail <- temperature %>%
                filter(Year >= 1880) %>% 
                select(c(Year,No_Smoothing))
temp_det1 = temp_detail %>% 
              arrange(temp_detail$Year) %>% 
              na.omit(temp_detail) %>% 
              rename('Global Temperature (deg C)'=No_Smoothing)

co2_ppm <-  read.csv("input_data/co2ppm.csv")
co2_ppm <- co2_ppm %>%
            filter(year >= 1959) %>%
            select(c(year,co2ppm))
co2_ppm <- co2_ppm %>%
            rename('Year'=year)

# Relationship chart ================================================
SL<-read.csv("input_data/SeaLevelRiseYearly.csv")
co2_details1 <- co2 %>% 
  filter(country == 'World', Year >= 1993) %>%   
  select(c(Year, co2)) 

temp_co2<-left_join(co2_details1, temp_det1, by="Year")
temp_co2<-left_join(temp_co2, SL, by="Year")

ay <- list(
  tickfont = list(color = "green"),
  overlaying = "y",
  side = "right",
  title = "Global Temperature (°C)"
)
ay1 <- list(
  tickfont = list(color = "green"),
  overlaying = "y",
  side = "right",
  title = "Global Mean Sea Level"
)



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
    labs(title=bquote(~CO[2]~ "(PPM) Level in Atmosphere"), 
         caption = "\n\nSource: : Global Monitoring Laboratory \n https://gml.noaa.gov/ccgg/trends/data.html") +
    scale_x_continuous(breaks = seq(1959,2020,5), limits = c(1959,2020))+
    scale_y_continuous(breaks = seq(300,420,20), limits = c(300,420))+
    theme(panel.background = element_rect(fill = "#dff0ef", 
                                          colour = "#6D9EC1", size = 2, 
                                          linetype = "solid"))
  
  # Right Plot
  p2 <- co2_overyear %>%
    ggplot(aes(x = Year , y =`CO2 Emission (million tonnes per year)`)) + 
    geom_line(color = "blue", size = 2) +
    labs(title=bquote("Annual Production-based Emissions of" ~CO[2]), 
         caption = "\n\nSource: Our World in Data \n https://ourworldindata.org/co2-emissions") +
    scale_x_continuous(breaks = seq(1959,2020,5), limits = c(1959,2020))+
    scale_y_continuous(breaks = seq(5000,40000,5000), limits = c(5000,40000))+
    theme(panel.background = element_rect(fill = "#dff0ef", 
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
    labs(title="Change in Global Temperature (°C) from 1880 to 2020",
         caption = "\n\nSource: Global Climate Change \n https://climate.nasa.gov/vital-signs/global-temperature/") +
    scale_x_continuous(breaks = seq(1880,2020,20), limits = c(1880,2020))+
    scale_y_continuous(breaks = seq(-0.5,1,0.1), limits = c(-0.5,1.1))+
    theme(panel.background = element_rect(fill = "#daf0e0", 
                                          colour = "#6D9EC1", size = 2, linetype = "solid"))
  p3
}

# ---- Top & Bottom Countries plot ----------
ranking_plot <- function(data, column, year, ranking) {
  # Return top 10 countries plot
  # Data : dataframe ; 
  # column : dataframe columns (only co2, co2_per_capita, cumulative_co2)
  
  # filter top 10 countries according to arguments
  top_countries <- data %>% 
    filter(Year==year) %>% 
    filter(country %in% unique(co2_map$country)) %>%
    select(country, {{column}})
  if (ranking == "highest") {
    top_countries <-  top_countries[order(-top_countries[2]), ]
    top10_countries <- head(top_countries, 10)
    top10_countries$country <- factor(top10_countries$country, 
                                      levels = unique(top10_countries$country)[order(top10_countries[[2]], 
                                                                                     decreasing = TRUE)])
    bar_color <- 'rgb(158, 202, 225)'
  } else if (ranking == "bottom") {
    top_countries <-  top_countries[order(top_countries[2]), ]
    top10_countries <- head(top_countries, 10)
    top10_countries$country <- factor(top10_countries$country, 
                                      levels = unique(top10_countries$country)[order(top10_countries[[2]], 
                                                                                     decreasing = FALSE)])
    bar_color <- 'rgb(122, 230, 150)'
  }

  # initial plotly plot
  fig <- plot_ly(
    x = top10_countries$country,
    y = top10_countries[[2]],
    type = "bar",
    marker = list(color = bar_color,
                  line = list(color = 'rgb(8,48,107)',
                              width = 1.5)),
    # texttemplate = '%{y:.2s}', 
    textposition = 'outside'
  )
  
  # For y-axis title based on selected column
  choose_title <- function() {
    title_col <- colnames(top10_countries[2])
    if (title_col == "co2") {
      return('CO<sub>2</sub> Production (mil tonnes/year)')
    } else if (title_col == "co2_per_capita") {
      return('CO<sub>2</sub> Production per capita (tonnes/year)')
    } else if (title_col == "cumulative_co2") {
      return('Cumulative CO<sub>2</sub> Production (mil tonnes/year)')
    }
  }
  # for different wordings for different columns
  if (column=="cumulative_co2") { 
    connect_text <- "as at"
  } else {
    connect_text <- "in"
  }
  
  if (ranking == "highest") {
    title_text <- sprintf('<b>Top 10 Countries %s Year %s </b>', connect_text, year)
  } else if (ranking == "bottom") {
    title_text <- sprintf('<b>Bottom 10 Countries %s Year %s </b>', connect_text, year)
  }
  fig <- fig %>% layout(title = title_text,
                        yaxis = list(title = choose_title(),
                                     titlefont = list(
                                       size = 12)))
  return(fig)
}
# Hash table for column name matching
index_table <- hash()
index_table[["CO2 Production"]] <- "co2"
index_table[["CO2 Production per Capita"]] <- "co2_per_capita"
index_table[["Cumulative CO2 Production"]] <- "cumulative_co2"


#========== SHINY UI ==========
ui <- bootstrapPage(
  # tags$head(includeHTML("gtag.html")),
  navbarPage(theme = shinytheme("cosmo"), collapsible = FALSE,
             HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" 
                  class="active" href="#">CO<sub>2</sub> Tracker</a>'), id="nav",
             windowTitle = HTML("CO2 TRACKER"),
             # Global Map Tab
             tabPanel(HTML("Global CO<sub>2</sub>"),
                      div(class="outer",
                          tags$head(includeCSS("styles.css")),
                          leafletOutput("mymap", width="100%", height="100%"),
                          
                          absolutePanel(id = "controls", class = "panel panel-default",
                                        top = 75, left = 55, width = 300, fixed=TRUE,
                                        draggable = TRUE, height = "auto",
                                        span(tags$i(h4(HTML("Global CO<sub>2</sub> Emission"))), style="color:#045a8d"),
                                        br(),
                                        h5(strong(textOutput("reactive_co2")), align = "center"),
                                        h5(strong(textOutput("reactive_co2_cumulative")), align = "center"),
                                        br(),
                                        h3(strong(textOutput("clean_date_reactive")), align = "center"),
                                        # h6(textOutput("reactive_country_count"), align = "right"),
                                        br(),
                                        plotOutput("yearly_plot", height="200px", width="100%"),
                                        plotOutput("cumulative_plot", height="200px", width="100%"),
                                        
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
                                      value = 1970,
                                      sep = ""),
                          br(),
                          htmlOutput("selected_var"),
                         
                          plotlyOutput('pie',width = "300%", height = "300%"),
                          sliderInput("pie_minimum_year",
                                      "Select year:",
                                      min = 1970,
                                      max = 2019,
                                      value = 1970,
                                      # choices = c(1970:2019),
                                      # selected = 1970,
                                      # grid = FALSE,
                                      animate=animationOptions(interval = 500, loop = FALSE),
                                      sep = ""
                                      )
                         ),
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Sources of Emission", 
                                     h6(HTML("PRIMARY SOURCES OF CO<sub>2</sub> EMISSION PER CAPITA")),
                                     plotlyOutput("plot")),
                            tabPanel("Type of Emission", 
                                     h6(HTML("TYPE OF CO<sub>2</sub> EMISSION")),
                                     plotlyOutput('percountry'))
                          )
                        )
                      )),
             
             # Atmospheric CO2 tab
             tabPanel(HTML("Atmospheric CO<sub>2</sub>"),
                      titlePanel(HTML("Relationship between CO<sub>2</sub> & Global Temperature")),
                      sidebarLayout(
                        position = "left", 
                        sidebarPanel(
                          width = 2,
                          h6(HTML("How CO<sub>2</sub> affect global temperature and mean sea level?")),
                          radioButtons("relationship", h3(""),
                                              choices = list("Temperature" = 1, "Mean Sea Level" = 2)
                                       ,selected = 1),
                        ),
                        mainPanel(
                          tabsetPanel(
                            # Tab 
                            tabPanel("Relationship",
                                     br(),
                            plotlyOutput("relationshipchart_temp"),
                            plotlyOutput("relationshipchart_GMSL")),
                              # Tab 1
                              tabPanel(HTML("CO<sub>2</sub> Concentration vs. CO<sub>2</sub> Emission"),
                                       h6(HTML(
                                          "The atmospheric level of carbon dioxide (CO<sub>2</sub>) has been steadily rising since the 1960's. 
                                          In 2019, carbon dioxide levels reached 411 parts per million, 
                                          in comparison to 1960 levels which stood at about 317 parts per million. 
                                          Projections for 2020 show concentrations of carbon dioxide have increased to 414 parts per million. 
                                          Emissions of carbon dioxide largely come from human activities such as burning fossil fuels and deforestation. 
                                          Data is taken from Mauna Loa CO<sub>2</sub> annual mean data. Data has been measured at Mauna Loa Observatory, 
                                          Hawaii as it constitutes the longest record of direct carbon dioxide measurements in the atmosphere."
                                       )),
                                       h6(HTML(
                                          "Global climate change is mostly triggered by
                                          carbon dioxide emissions.
                                          It’s widely recognized that to avoid the worst impacts of climate change, 
                                          the world needs to urgently reduce emissions. This graph illustrates the change in Carbon Dioxide (CO<sub>2</sub>)
                                          emission based on annual production from 1959 to 2019. CO<sub>2</sub> emissions data from Our World 
                                          in Data and the Global Carbon Project."
                                       )),
                                       br(),
                                       plotOutput("globaltemp_plot1")),
                              # Tab 2
                              tabPanel("Global Temperature",
                                       h6(
                                       "This graph illustrates the change in global temperature (°C) from 1880 to 2019. 
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
             
             # Country Ranking Tab
             tabPanel("Country Ranking",
                      titlePanel(""),
                      sidebarLayout(
                        position = "left",
                        sidebarPanel(
                          width = 3,
                          pickerInput("carbon_index", HTML("Select CO<sub>2</sub> Index:"),
                                      choices = c("CO2 Production",
                                                  "CO2 Production per Capita",
                                                  "Cumulative CO2 Production"),
                                      selected = "CO2 Production"),
                          sliderInput("wanted_year",
                                      "Year:",
                                      min = 1970,
                                      max = 2019,
                                      value = 2019,
                                      sep = ""),
                          br(),
                        ),
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Top Countries",
                                     br(),
                                     plotlyOutput("topcountry_plot")),
                            tabPanel("Bottom Countries",
                                     br(),
                                     plotlyOutput("bottomcountry_plot"))
                          )
                        )
                      )),
             
             # User guide tab
             tabPanel("User Guide",
                      titlePanel(HTML("CO<sub>2</sub> Tracker")),
                      
                      h5(HTML("Global CO<sub>2</sub> Tracker (GCT) is an online web app that provides data and dashboard for monitoring 
                              global CO<sub>2</sub> emission. It uses data to illuminate the state of CO<sub>2</sub> level worldwide and tells the stories of 
                              how CO<sub>2</sub> contributed by each countries. GCT allows anyone to access information about where and 
                              how CO<sub>2</sub> emission are changing around the world.")),
                      br(),
                      sidebarLayout(
                        position = "left",
                        sidebarPanel(
                        
                        h4("Overview"),
                        h5(HTML("The map & dashboards on GCT allow you to explore hundreds of spatial datasets that 
                        help explain when, where and how CO<sub>2</sub> emission are changing around the world.")),
                        
                        h5(HTML(" The map tell a visual story about how CO<sub>2</sub> emmission is changing in a particular place. 
                          The dashboards for sources of CO<sub>2</sub> emission help answer important questions about CO<sub>2</sub>
                          change in any country and enable you to view statistics through interactive charts and graphs.")),
                       h5("Let's get started with our step-by-step instructions.") ),
                         
                      
                        mainPanel(
                          tabsetPanel(
                            tabPanel(HTML("CO<sub>2</sub> World Map"), 
                                     h3("Use the map"),
                                     h5("The map allows you to visualize and analyze spatial data. 
                                     Below you'll find key functionalities to help you explore the 
                                     map visualizations using the function available on the map."),
                                     # img(src = "co2worldmap_illustration.png", width = 1000),
                                     img(src = "CO2interface.jpg", width = 800),
                                     br(),
                                     br(),
                                     strong(HTML(":: Yearly/ Cumulative CO<sub>2</sub> Emission")),
                                     h5(HTML("Select to display yearly or cumulative CO<sub>2</sub> emission on the map. 
                                        Yearly CO<sub>2</sub> Emission is indicated by green bubble while cumulative CO<sub>2</sub> Emission is indicated by blue bubble. 
                                        The size of the bubble show the amount of CO<sub>2</sub> emission.")),
                                     strong(":: Adjust Display Year"),
                                     h5(HTML("Adjust the slider to select year for visualization.
                                        Once a year range had been selected, visualization on the map will adjusted accordingly
                                        with CO<sub>2</sub> data up until the year selected.")),
                                     strong(":: Play Button"),
                                     h5(HTML("Click the play button to see how CO<sub>2</sub> emission changes across the year globally."))
                                     ),
                                     
                            tabPanel("Sources of Emission", 
                                     h3("Use the charts"),
                                     h5(HTML("The charts allow you to visualize and analyze sources of CO<sub>2</sub> emission. 
                                     Below you'll find key functionalities to help you explore the charts.")),
                                     img(src = "dashboard_illustration.png", width = 1000),
                                     br(),
                                     br(),
                                     strong(":: Dropdown for Country Selection"),
                                     h5("Select a country to further analyze on its sources of emission. All the display charts will be adjusted based on country selected.
                                        This country selection apply to all the charts in display."),
                                     strong(":: Adjust Display Year"),
                                     h5(HTML("Adjust the slider to select year range for visualization.
                                        Once a year range had been selected, visualization on the charts will adjusted accordingly
                                        with CO<sub>2</sub> data.
                                        This year slider only apply to chart on [sources of emission] and [type of emission].")),
                                     strong(":: Select Type of Charts"),
                                     h5("Switch between different tabs to view sources of emission or type of emission (production emission vs consumption emission) 
                                        for a selected country."),
                                     strong(":: Summary Text"),
                                     h5("The largest sources of cumulative emission displayed in text for selected country."),
                                     strong(":: Play Button for Cumulative Proportion"),
                                     h5(HTML("This play button is associated only with bar chart for [Cumulative Percentage of CO<sub>2</sub> Emission by Sources]. Click the play button to see 
                                        how the cumulative CO<sub>2</sub> emission percentages change across the year on selected country."))
                                     )
                          )
                        )
                      )
                    ),
             # Download CSV Data Tab
             tabPanel("Data",
                      h2("Datasets"),
                      HTML("This page is to download the dependency datasets of CO<sub>2</sub> in CSV format.<br>"),
                      # CO2 Data
                      HTML("<h3>CO<sub>2</sub> Dataset</h3>"),
                      numericInput("maxrows", "Rows to show", 25),
                      verbatimTextOutput("rawtable"),
                      "Adapted from data collected, aggregated, and documented by ", 
                      tags$a(href="https://github.com/owid/co2-data/blob/master/owid-co2-data.csv", 
                             "Hannah Ritchie, Max Roser and Edouard Mathieu."),
                      br(),
                      br(),
                      downloadButton("downloadCsv", "Download as CSV"),tags$br(),tags$br(),
                      
                      # Global Temperature Data
                      HTML("<h3>Global Temperature Dataset</h3>"),
                      numericInput("maxrows", "Rows to show", 25),
                      verbatimTextOutput("rawtable2"),
                      downloadButton("downloadCsv2", "Download as CSV"),tags$br(),tags$br(),
                      
                      # Mean Sea Level
                      HTML("<h3>Mean Sea Level Dataset</h3>"),
                      numericInput("maxrows", "Rows to show", 25),
                      verbatimTextOutput("rawtable3"),
                      downloadButton("downloadCsv3", "Download as CSV"),tags$br(),tags$br(),                   
                      
                      
             ),
             # About tab
             tabPanel("About",
                      HTML("<h2>About</h2><br>
                      This Shiny App is developed for the Group Project of University of Malaya 
                      <b>WQD7001 Principles of Data Science</b> by <b>Group 10</b> for <b>2021 March Semester</b>.<br><br>
                      Members of Group 10:
                      <ul>
                        <b>
                          <li>Thai Yuan Jiun  (17218822)</li>
                          <li>Sameer Kumar Maurya  (S2038179)</li>
                          <li>Ng Choon Khon  (S2028941)</li>
                          <li>Anere Goodness Ayobami  (S2039808)</li>
                        </b>
                      </ul>
                      <br>
                      For the all the data and Shiny App can be found on 
                      <a href='https://github.com/ckng0221/CO2_ShinyApp'>GitHub</a>
                      and <a href='https://ckng21.shinyapps.io/Global_CO2_Tracker/'>shinyapps.io</a>.
                           "),
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
    paste0(paste("Yearly: ", prettyNum(formatC(reactive_db_world()$co2, mode='integer'),
                                        big.mark=","), sep = '\n'), " mil tonnes/year")
  })
  
  output$reactive_co2_cumulative <- renderText({
    paste0(paste("Cumulative: ", prettyNum(formatC(reactive_db_world()$cumulative_co2, mode='integer'),
                                                     big.mark=","), sep='\n'), " mil tonnes")
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
                       fillOpacity = 0.2, color = co2_yearly_col, group = HTML("CO<sub>2</sub> (Yearly)"), 
                       label = sprintf(
                         "<strong>%s</strong><br/>
                                       CO<sub>2</sub>: <strong>%s mil tonnes/year</strong><br/>
                                       CO<sub>2</sub> per capita: <strong>%.2f tonnes/year</strong><br/>",
                         reactive_db()$country,
                         prettyNum(formatC(reactive_db()$co2, mode='integer'), big.mark = ','),
                         reactive_db()$co2_per_capita) %>% 
                         lapply(htmltools::HTML),
                       labelOptions = labelOptions(
                         style = list("font-weight" = "normal",
                                      padding = "4px 8px",
                                      "color" = co2_yearly_col),
                         textsize = "15px",
                         direction = "auto")) %>%
      
      # Cumulative
      addCircleMarkers(data = reactive_db(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(cumulative_co2)^(1/3),
                       fillOpacity = 0.2, color = co2_cumulative_col, group = HTML("CO<sub>2</sub> (Cumulative)"),
                       label = sprintf(
                         "<strong>%s</strong><br/>
                                       Cumulative CO<sub>2</sub>: <strong>%s mil tonnes</strong><br/>",
                         reactive_db()$country,
                         prettyNum(formatC(reactive_db()$cumulative_co2, mode='integer'), big.mark=',')) %>%
                          lapply(htmltools::HTML),
                       labelOptions = labelOptions(
                         style = list("font-weight" = "normal",
                                      padding = "4px 8px",
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
      ylab(label = HTML('CO<sub>2</sub> Emission per capita (tonnes)')) +
      geom_area()+
      labs(fill = "Sources") +
      theme_bw() + 
      theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
    
  )
  

 output$pie <- renderPlotly(

    data_long %>%
      filter(country == input$country, Year<=input$pie_minimum_year, emission_per_capita!=0) %>%
      group_by(sources)%>%
      mutate(cumsum=cumsum(emission_per_capita))%>%
      filter(Year==input$pie_minimum_year)%>%
      ungroup(sources)%>%
      mutate(pect=round((cumsum/sum(cumsum))*100,digits=2))%>%
      plot_ly(x = ~pect,
        y=  ~reorder(sources,pect),
        marker = list(color="rgb(207,181,59)"),
        type='bar',
        orientation="h"
    )%>%
    layout(title = HTML(" CUMULATIVE PERCENTAGE OF CO<sub>2</sub> EMISSION"),  showlegend = F,
                      xaxis = list(title="Cumulative percentage (%)",showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(title="",showgrid = FALSE, zeroline = FALSE, showticklabels = T),
                      autosize=F,
                      width=500,
                      height=370,
                      margin=list(
                        l=50,
                        r=50,
                        b=20,
                        t=40,
                        pad=5)
    )
  )


  
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
      ylab(label = HTML('CO<sub>2</sub> Emission per capita (tonnes)')) +
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
    paste("As of 2019, ", "<b>",max_perct,"</b>", "% of CO<sub>2</sub> was contributed by ",
          "<b>",sub("_.*", "", max_sources),"</b>"," as the largest sources of CO<sub>2</sub> emission in",
          "<b>",input$country,"</b>",".")
  })
  
  
  output$globaltemp_plot1 <- renderPlot({
    globaltemp_plot1_func()
  })

  output$globaltemp_plot2 <- renderPlot({
    globaltemp_plot2_func()
  })
  
  output$relationshipchart_temp <- renderPlotly({
    
      fig <- plot_ly(temp_co2)
      fig <- fig %>% add_trace(x = ~Year, y = ~co2,type="bar", name = HTML("CO<sub>2</sub> Emission (ppm)"))
      fig <- fig %>% add_lines(x = ~Year, y = ~`Global Temperature (deg C)`, name = "Global Temperature (°C)", yaxis = "y2")
      fig <- fig %>% layout(
        title = HTML("Relationship between CO<sub>2</sub> Emission and Global Temperature"), yaxis2 = ay,
        xaxis = list(title="Year")
      )
      fig
  })
     
      output$relationshipchart_GMSL <- renderPlotly({
      fig1 <- plot_ly(temp_co2)
      fig1 <- fig1 %>% add_trace(x = ~Year, y = ~co2,type="bar", name = HTML("CO<sub>2</sub> Emission (ppm)"))
      fig1 <- fig1 %>% add_lines(x = ~Year, y = ~GMSL, name = "Global Mean Sea Level", yaxis = "y2")
      fig1 <- fig1 %>% layout(
        title = HTML("Relationship between CO<sub>2</sub> Emission and Global Mean Sea Level"), yaxis2 = ay1,
        xaxis = list(title="Year")
      )
      fig1
   
  })
  
  # output to download data
  output$downloadCsv <- downloadHandler(
    filename = function() {
      paste("input_data/co2", ".csv", sep="")
    },
    content = function(file) {
      # co2_sub = co2 %>% select(c(country, Year, co2_per_capita,consumption_co2_per_capita,
      #                            cumulative_co2, cement_co2_per_capita, coal_co2_per_capita, 
      #                            flaring_co2_per_capita, gas_co2_per_capita,oil_co2_per_capita,
      #                            other_co2_per_capita))
      # names(co2_sub) = c("country", "year", "co2_per_capita", "Consumption_CO2_per_capita", "cumulative_co2", "co2_per_capita(Cement)",
      #                    "co2_per_capita(Coal)", "co2_per_capita(Flaring)", "co2_per_capita(Gas)", "co2_per_capita(Oil)", "co2_per_capita(Other)")
      # write.csv(co2_sub, file)
      write.csv(co2, file) # CK modified
    }
  )
  
  output$rawtable <- renderPrint({
  #  co2_sub = co2 %>% select(c(country, Year, co2_per_capita,consumption_co2_per_capita,
  #                           cumulative_co2, cement_co2_per_capita, coal_co2_per_capita, 
  #                           flaring_co2_per_capita, gas_co2_per_capita,oil_co2_per_capita,
  #                           other_co2_per_capita))
  # names(co2_sub) = c("country", "year", "co2_per_capita", "Consumption_CO2_per_capita", "cumulative_co2", "co2_per_capita(Cement)",
  #                            "co2_per_capita(Coal)", "co2_per_capita(Flaring)", "co2_per_capita(Gas)", "co2_per_capita(Oil)", "co2_per_capita(Other)")
    orig <- options(width = 1500)
    # print(tail(co2_sub, input$maxrows), row.names = FALSE)
    print(head(co2, input$maxrows), row.names = FALSE) # CK modified
    options(orig)
  })
  
  # Global Temperature Dataset
  output$downloadCsv2 <- downloadHandler(
    filename = function() {
      paste("input_data/temp", ".csv", sep="")
    },
    content = function(file) {
      temp_sub = temperature %>% select(Year, No_Smoothing)
      names(temp_sub) = c("Year","Temp")
      write.csv(temp_sub, file)
    }
  )
  
  output$rawtable2 <- renderPrint({
    temp_sub = temperature %>% select(Year, No_Smoothing)
    names(temp_sub) = c("Year","Temp")
    orig <- options(width = 1500)
    print(tail(temp_sub, input$maxrows), row.names = FALSE)
    options(orig)
  })
  
  # GMSL Mean Sea Level
  output$downloadCsv3 <- downloadHandler(
    filename = function() {
      paste("input_data/SeaLevelRiseYearly", ".csv", sep="")
    },
    content = function(file) {
      SL_sub = SL %>% select(Year, GMSL)
      names(SL_sub) = c("Year","MSL")
      write.csv(SL_sub, file)
    }
  )
  
  output$rawtable3 <- renderPrint({
    SL_sub = SL %>% select(Year, GMSL)
    names(SL_sub) = c("Year","MSL")
    orig <- options(width = 1500)
    print(tail(SL_sub, input$maxrows), row.names = FALSE)
    options(orig)
  })
  
  # Top countries server
  output$topcountry_plot <- renderPlotly(
    ranking_plot(data = co2, column = index_table[[input$carbon_index]], year=input$wanted_year, ranking="highest" )
  )
  
  output$bottomcountry_plot <- renderPlotly(
    ranking_plot(data = co2, column = index_table[[input$carbon_index]], year=input$wanted_year, ranking="bottom" )
  )
  
  
}

#runApp(shinyApp(ui, server), launch.browser = TRUE)
shinyApp(ui, server)
#library(rsconnect)
#deployApp(account="vac-lshtm")


