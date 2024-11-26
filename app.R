#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#


library(shiny)
library(leaflet)
library(dplyr)
library(readxl)
library(knitr)
library(kableExtra)
library(htmltools)
library(sf)
library(purrr)

Housing <- read_excel("AAC data_v1.xlsx", 
                          sheet = "Housing Data", col_types = c("skip", 
                                                                "skip", "text", "skip", "skip", "skip", 
                                                                "skip", "skip", "skip", "skip", "skip", 
                                                                "skip", "numeric"))

AAC_data_v1 <- read_excel("AAC data_v1.xlsx",sheet = "Plants Data",col_types = c("numeric","text", "text","skip", "skip",
                                                                                 "numeric","numeric", "numeric", "skip","skip",
                                                                                 "skip"),skip = 3)

Power_Plants <- read_excel("AAC data_v1.xlsx", sheet = "Power Plants",
                           col_types = c("text","skip", "skip", "skip", "skip", "skip", 
                                         "numeric", "skip", "skip", "numeric", "numeric"))

Sales <- read_excel("sales.xlsx", col_types = c("skip", "numeric", "numeric", "numeric"))


GDP <- read_excel("AAC data_v1.xlsx", 
                          sheet = "StateWise GDP", col_types = c("skip", "text",
                                                                 "skip", "skip", "skip", "skip", "skip", 
                                                                 "skip", "skip", "skip", "skip", "skip", 
                                                                 "skip", "numeric"))

Projects <- read_excel("AAC data_v1.xlsx", 
           sheet = "Project CityWise", col_types = c("skip", 
                                                     "skip", "text", "numeric", "skip", 
                                                     "numeric", "numeric"))

Cement <- read_excel("AAC data_v1.xlsx", 
                          sheet = "Cement Data", col_types = c("text", 
                                                               "text", "skip", "skip", "numeric", 
                                                               "skip", "numeric", "numeric"))


data_map <- read_sf("india_district_administered.geojson") %>% st_transform(data_map, crs = '+proj=longlat +datum=WGS84')%>%left_join(Housing%>%rename_at("District",~"NAME_2"))

rm(Housing)

States_map <- read_sf("INDIA_STATES.geojson") %>% st_transform(States_map, crs = '+proj=longlat +datum=WGS84')%>%left_join(GDP,by = 'STCODE11')

rm(GDP)

create_table <- function(sno){
  df <- AAC_data_v1 %>%
    filter(Sno. == sno)
  
  table <- df %>%
    select("Company" = Company,"Plant"=Plant,"Plant Capacity(m3 p.a.)" = `Plant Capacity (Cubic meter per annum)`)%>%
    t()%>%
    kableExtra::kable(format="html",align = 'c')
}


create_table_PP <- function(name){
  df_1 <- Power_Plants %>%
    filter(Name == name)
  
  table <- df_1 %>%
    select("Name" = Name,"Capacity(MW)"=`Capacity(MW)`)%>%
    t()%>%
    kableExtra::kable(format="html",align = 'c')
}


create_table_h <- function(NAME_2){
  df_2 <- data_map %>%
    filter(NAME_2 == NAME_2)
  
  table <- df_2 %>%
    select("State" = NAME_1,"District" = NAME_2,"PMAY Houses Built (23-24)"=`2023-2024`)%>%
    t()%>%
    kableExtra::kable(format="html",align = 'c')
}


AAC_data_v1 <- AAC_data_v1 %>%
  mutate(table = map_chr(Sno.,create_table,.progress = FALSE))


Power_Plants <- Power_Plants %>%
  mutate(table = map_chr(Name,create_table_PP,.progress = FALSE))


AAC_data_v1$Plant<-NULL

pal <- colorFactor(c("#0f9d58","#F5C000","#0194F5","#F50501","#86E0E7",
                     "#676668","#676668","#676668","#676668","#676668",
                     "#676668","#676668","#676668","#676668","#676668",
                     "#676668","#676668","#676668","#676668","#676668"),
                   levels = AAC_data_v1$Company)


pal_h <- colorNumeric(palette = "plasma", domain = data_map$`2023-2024`,na.color = "#FFFFFF",reverse = TRUE)


pal_gdp <- colorNumeric(palette = "plasma", domain = States_map$`2022–23`,na.color = "#FFFFFF",reverse = TRUE)


library(sf)
# Define UI for application that draws a histogram
ui <- fluidPage(leafletOutput('mymap',height = 900,width = "80%"))

# Define server logic required to draw a histogram
server <- function(input, output) {
  # draw the histogram with the specified number of bins
  map = leaflet(data = data_map)%>%
    addProviderTiles(providers$Stadia.StamenTonerLite)%>%
    setView(lat = 21.7679,lng = 78.8718,zoom = 5)%>%
    addPolygons(data = data_map,group = "PMAY Housing",fillColor =  pal_h(data_map$`2023-2024`),weight = 0.5, dashArray = "3",label = paste0("PMAY Houses Built(23-24) :",data_map$`2023-2024`),fillOpacity = 0.3,highlightOptions = highlightOptions(weight = 5,color = "#666",dashArray = "",fillOpacity = 0.7,bringToFront = FALSE))%>%
    addPolygons(data = States_map,group = "GDP",fillColor =  pal_gdp(States_map$`2022–23`),weight = 0.5, dashArray = "3",label = paste0("GDP: ₹",States_map$`2022–23`/100," Cr."),fillOpacity = 0.3,highlightOptions = highlightOptions(weight = 5,color = "#666",dashArray = "",fillOpacity = 0.7,bringToFront = FALSE))%>%
    addMarkers(lng = Power_Plants$Longitude, lat = Power_Plants$Latitude,clusterOptions = markerClusterOptions(),label = ~lapply(Power_Plants$table, htmltools::HTML),group = "Thermal Power Plants",icon = ~icons(iconUrl = "triangle-png-42404.png",iconWidth = 18, iconHeight = 18))%>%
    addCircleMarkers(lng = AAC_data_v1$Longitude,lat = AAC_data_v1$Latitude, color = ~pal(AAC_data_v1$Company),radius = 5,fillOpacity =1, label = ~lapply(AAC_data_v1$table, htmltools::HTML),group = "AAC Block Plants")%>%
    addCircles(lng = AAC_data_v1$Longitude,lat = AAC_data_v1$Latitude, color = ~pal(AAC_data_v1$Company),radius = AAC_data_v1$`Plant Capacity (Cubic meter per annum)`/20,fillOpacity =0.5, label = ~lapply(AAC_data_v1$table, htmltools::HTML),group = "AAC Block Production Capacity",opacity = 0.8)%>%
    addCircles(lng = Sales$Longitude, lat = Sales$Latitude,label = paste0("Sales :",Sales$`Gross Total`),group = "Sales",radius = Sales$`Gross Total`/2500)%>%
    addCircles(lng = Projects$Longitude, lat = Projects$Latitude,label = paste0(Projects$City,"Smart City Projects :",Projects$`Total Projects`),group = "Smart City Projects",radius = Projects$`Total Projects`*200,color = '#A0934A')%>%
    addCircles(lng = Cement$Longitude, lat = Cement$Latitude,label = paste0(Cement$`Company/Plant Name`," ",Cement$Location," ","Capacity :",Cement$`Capacity (million tonnes)`),group = "Cement Plants",radius = Cement$`Capacity (million tonnes)`*4000,color = '#A1094B')%>%
    addLegend(data = AAC_data_v1%>%filter(Company == "Biltech" | Company == "Magicrete" | Company == "HIL" | Company == "Bigbloc" | Company == "Renacon"),
              position = "bottomright",
              pal = pal, values = ~Company,
              title = "AAC Block Plants Legend",
              opacity = 0.5,group = "AAC Block Legend")%>%
    addLegend(data = data_map,
              position = "topright",
              pal = pal_h, values = ~`2023-2024`,
              title = "PMAY Houses Legend",
              opacity = 0.5,group = "PMAY Legend")%>%
    addLegend(data = States_map,
              position = "bottomleft",
              pal = pal_gdp, values = States_map$`2022–23`,
              title = "GDP Legend (₹ Cr.)",
              opacity = 0.5,group = "GDP Legend")%>%
    addLayersControl(
      overlayGroups = c("AAC Block Plants","AAC Block Production Capacity","Thermal Power Plants","PMAY Housing","Sales","Smart City Projects","GDP","Cement Plants","AAC Block Legend","PMAY Legend","GDP Legend")
    )%>%
    hideGroup(c("PMAY Housing","Sales","GDP","Smart City Projects","Cement Plants","PMAY Legend","GDP Legend"))
  output$mymap = renderLeaflet(map)
}

# Run the application 
shinyApp(ui = ui, server = server)
