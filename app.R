# Kuivuusriskien hallinta Suomessa
## Sara Todorovic (sara.todorovic@syke.fi)


# Clear workspace
rm(list = ls())

### INITIALISE -----------------------------------------------------------------


# App version
app_v <- "002 (14.12.2022)"


# Import libraries
library(shiny)
library(ggplot2)
library(tidyverse)
library(spdplyr)
library(rgdal)
library(rgeos)
library(reshape2)
library(raster)
library(data.table)
library(shinythemes)
library(shinyjs)
library(htmltools)
library(leaflet)
library(sf)
library(ggiraph)
library(DT)
library(reactable)
library(stringr)
library(tippy)
library(shinyBS)
library(crosstalk)
library(htmlwidgets)
library(geojsonio)

#wd <- setwd("C:/Users/e1007642/Documents/Kuivuusriskit KUHASUO shiny/drought-risk-finland")

# # css path
csspath <- "app_style.css"


### Load & wrangle data --------------------------------------------------------

# Load data: Drought risk index, drought hazard SPI6, drought vulnerability (by municipalities)
kuivuus <- geojsonio::geojson_read("data/DRI_SPI6_07_10_2022_WGS84.geojson", what = "sp")


# Select needed columns
kuivuus <- kuivuus[,c(2,7,24,25,26,27,29,30,31,33,35)]
names(kuivuus) <- c('Kunta', 'DHI','Väestörakenne','Kuivuusherkkyys','Alkutuotanto',
                    'Peltoisuus','Maatalous','Teollisuus','Yhdyskunnat','DVI','DRI') 
kuivuus <- subset(kuivuus, DRI >= 0) %>%
  mutate(
    DVI_class = case_when(
      DVI >= 0 & DVI <0.437 ~ 1,
      DVI >=0.437 & DVI < 0.54 ~2,
      DVI >=0.54 & DVI < 0.601 ~3,
      DVI >=0.601 & DVI < 0.667 ~4,
      DVI >= 0.667 ~5),
    DRI_class = case_when(
      DRI >= 0 & DRI <0.298 ~ 1,
      DRI >=0.298 & DRI < 0.384 ~2,
      DRI >=0.384 & DRI < 0.451 ~3,
      DRI >=0.451 & DRI < 0.544 ~4,
      DRI >= 0.544 ~5)
  ) %>%
  arrange(desc(DRI)) %>%
  mutate(
    DRI_nro = row_number()
  ) %>%
  arrange(desc(DVI)) %>%
  mutate(
    DVI_nro = row_number()
  )

kuivuus_df <- data.frame(kuivuus)

# Select TOP 20 municipalities for DVI barchart and wrangle data
DVI_top20 <- subset(kuivuus, DVI_nro <= 20)
# nrow(DVI_top20)
#names(DVI_top20)
DVI_top20 <- data.frame(DVI_top20[,c(1,3:9, 15)])
#colnames(DVI_top20)

DVI_top20 <- DVI_top20 %>%
  gather(key = "Haavoittuvuustekijä",
         value="Osuus", -1, -"DVI_nro")


# Data for donut chart with share of vulnerability indicators 
kuivuus2 <- data.frame(kuivuus[,c(1,3:9)])
kuivuus2 <- kuivuus2 %>%
  gather(key = "Haavoittuvuustekijä",
         value="Osuus", -1)

# Parameters for selecting municipality
locations <- unique(kuivuus2$Kunta) %>%
  sort()


#### ShinyApp Server -----------------------------------------------------------

server <- function(input, output, session){
  
  # DRI
  bins_DRI <- c(0, 0.298, 0.384, 0.451, 0.544, 1)
  pal_DRI <- colorBin(c("#440154","#3b528b","#21918c", "#5ec962", "#fde725"), bins = bins_DRI)
  
  # DVI
  bins_DVI <- c(0, 0.437, 0.54, 0.601, 0.667, 1)
  pal_DVI <- colorBin(c("#440154","#3b528b","#21918c", "#5ec962", "#fde725"), bins = bins_DVI)
  
  # DHI SPI6
  bins_SPI <- c(0, 0.575, 0.671, 0.73, 0.79, 1)
  pal_SPI <- colorBin(c("#440154","#3b528b","#21918c", "#5ec962", "#fde725"), bins = bins_SPI)
  
  
  # tooltip <- paste(
  #   "Kunta: ", kuivuus$Kunta, "<br/>",
  #   "Kuivuusriski: ", round(kuivuus$DRI, 2), "<br/>",
  #   sep="") %>%
  #   lapply(htmltools::HTML)
  
  
  output$map1 <- renderLeaflet({
    
    leaflet(kuivuus) %>%
      addProviderTiles(providers$CartoDB.Positron,
                       option=leafletOptions(minZoom = 5, maxZoom = 7)) %>%
      addPolygons(
        stroke = TRUE,
        weight = 1,
        color = 'black',
        smoothFactor = 0.3, 
        fillOpacity = 0.9,
        fillColor = ~pal_DRI(DRI),
        label = paste(
          "Kunta: ", kuivuus$Kunta, "<br/>",
          "Kuivuusriski: ", round(kuivuus$DRI, 2), "<br/>",
          sep="")%>% lapply(htmltools::HTML),
        labelOptions = labelOptions( 
          style = list("font-weight" = "normal", padding = "3px 8px"), 
          textsize = "13px", 
          direction = "auto",
        ),
        group = "Kuivuusriski (DRI)"
      ) %>%
      
      addPolygons(
        stroke = TRUE,
        weight = 1,
        color = 'black',
        smoothFactor = 0.3, 
        fillOpacity = 0.9,
        fillColor = ~pal_SPI(DHI),
        label = paste(
          "Kunta: ", kuivuus$Kunta, "<br/>",
          "Kuivuusvaara: ", round(kuivuus$DHI, 2), "<br/>",
          sep="")%>% lapply(htmltools::HTML),
        labelOptions = labelOptions( 
          style = list("font-weight" = "normal", padding = "3px 8px"), 
          textsize = "13px", 
          direction = "auto",
        ),
        group = "Kuivuusvaara (DHI)"
      ) %>%
      
      addPolygons(
        stroke = TRUE,
        weight = 1,
        color = 'black',
        smoothFactor = 0.3, 
        fillOpacity = 0.9,
        fillColor = ~pal_DVI(DVI),
        label = paste(
          "Kunta: ", kuivuus$Kunta, "<br/>",
          "Kuivuushaavoittuvuus: ", round(kuivuus$DVI, 2), "<br/>",
          sep="") %>% lapply(htmltools::HTML), 
        labelOptions = labelOptions( 
          style = list("font-weight" = "normal", padding = "3px 8px"), 
          textsize = "13px", 
          direction = "auto"),
        group = "Kuivuushaavoittuvuus (DVI)"
      ) %>%
      
      addLegend(
        labels = c("Pienin", "","", "", "Suurin"),
        values=~round(DRI,2),
        colors = c("#440154","#3b528b","#21918c", "#5ec962", "#fde725"),
        opacity=0.9, 
        title = "Indeksin arvo", 
        position = "bottomright"
      ) %>%
      
      # Radiobuttons for each column
      addLayersControl(
        baseGroups = c("Kuivuusriski (DRI)", "Kuivuusvaara (DHI)", "Kuivuushaavoittuvuus (DVI)"),
        options = layersControlOptions(collapsed = F)) 
    
  
  })

  # NOT WORKING ATM. Update the location selectInput on map click (https://www.r-bloggers.com/2016/03/r-shiny-leaflet-using-observers/)
  # observeEvent(input$map1_marker_click, { 
  #   p <- input$map1_marker_click
  #   if(!is.null(p$id)){
  #     if(is.null(input$location) || input$location!=p$id) updateSelectInput(session, "location", selected=p$id)
  #   }
  # })
  
  
  # TOP 20 municipalities
  output$plo <- renderggiraph({

    library(hrbrthemes)
    library(forcats)


    DVI_top20 <- DVI_top20 %>%
      mutate(Kunta = fct_reorder(Kunta, desc(DVI_nro)))
    
    colpal <- c('#7fc97f','#beaed4','#fdc086','#ffff99','#386cb0','#f0027f','#bf5b17')


    plo <- ggplot(

      data= DVI_top20,
      aes(fill = Haavoittuvuustekijä, y = Kunta, x =  Osuus)) +

      geom_bar(position = "fill", stat = "identity") +
      
      scale_x_continuous(n.breaks=10, label = scales::percent)+
      guides(fill=guide_legend(ncol=3))+

      
      scale_fill_brewer(palette = "Paired")+
      # # 
      # scale_color_manual(values = c("Alkutuotanto" = '#7fc97f',
      #                               "Maatalous"="#beaed4",
      #                               "Teollisuus"="#fdc086",
      #                               "Yhdyskunnat" = "#ffff99",
      #                               'Kuivuusherkkyys'= "#386cb0",
      #                               'Peltoisuus'= "#f0027f",
      #                               'Väestörakenne'="#bf5b17")) +
      

      
      # Style settings
      theme(axis.title.x=element_blank(),
            axis.text.x = element_text(size=25, face = "bold"),
            axis.text.y = element_text(size=25),
            axis.title.y = element_text(size = 25),
            panel.background = element_blank(),
            axis.line = element_line(colour="grey"),
            legend.position ="bottom",
            legend.title=element_blank(),
            legend.justification = "centre",
            legend.margin = margin(),
            legend.background = element_blank(),
            legend.text = element_text(size=20),
            legend.spacing.y = unit(0.5, "cm"),
            legend.spacing.x = unit(0.75, "cm"),
            legend.box = "vertical",
            legend.box.just = 'left',
            legend.key.height = unit(1.2, "cm"),
            legend.key.size = unit(1, "cm"),
            legend.box.background = element_rect(alpha("white", 0.3), color =NA),
            plot.title = element_text(size=25))
              

    # display plot
    ggiraph(code = print(plo),
            width_svg = 17,
            height_svg = 11.3)

  })
  
  # Create donut chart for vulnerability
  
  output$plo2 <- renderggiraph({
    
    library(ggrepel)
    
    # Hole size
    hsize <- 2
    
    # Data with vulnerability indicators and their share per municipality
    kuivuus2 <- kuivuus2 %>%
      mutate(x = hsize,
             perc = Osuus/sum(kuivuus2[kuivuus2$Kunta == input$location,"Osuus"]))
    
    # sum(kuivuus2[kuivuus2$Kunta == "Helsinki","Osuus"])
    # View(kuivuus2)

    # Create plot
    plo2 <- ggplot(
      kuivuus2[kuivuus2$Kunta == input$location,], aes(x = hsize, y = perc, fill = Haavoittuvuustekijä)) +

      geom_col(color = "black") +
      geom_text_repel(aes(label = paste(sprintf("%0.0f", round(perc*100, digits = 0)), " %")),
                position = position_stack(vjust = 0.5),
                size =20) +
      
      labs(title= paste(input$location,
                        "\nKuivuushaavoittuvuusindeksi (DVI): ", round(kuivuus_df[kuivuus_df$Kunta == input$location, "DVI"],2))) +
      coord_polar(theta = "y") +
      scale_fill_brewer(palette = "Paired") +
      xlim(c(0.2, hsize + 0.5)) +
      guides(fill=guide_legend(ncol=2))+

      theme(panel.background = element_blank(),
            panel.grid = element_blank(),
            axis.title = element_blank(),
            axis.ticks = element_blank(),
            axis.text = element_blank(),
            legend.background = element_blank(),
            legend.text = element_text(size=50),
            legend.spacing.y = unit(1, "cm"),
            legend.spacing.x = unit(1.5, "cm"),
            legend.box = "vertical",
            legend.box.just = 'left',
            legend.key.height = unit(1.25, "cm"),
            legend.key.size = unit(1.5, "cm"),
            plot.background = element_blank(),
            legend.box.background = element_rect(alpha("white", 0.3), color =NA),
            legend.position="bottom",
            legend.title=element_blank(),
            plot.title = element_text(size=55, hjust=0.5, vjust = -1))
    

    
    # display plot
    ggiraph(code = print(plo2),
            width_svg = 17,
            height_svg = 17)




  })

    # Create table
  output$table1 <- renderReactable({
    
    # Create dataframe 
    DRI_table <- data.frame(kuivuus[,c('Kunta', 'DHI','DVI', 'DRI')]) 
    DRI_table <- DRI_table %>% drop_na(DHI, DVI, DRI)
    DRI_table[,c(2:4)] <- round(DRI_table[,c(2:4)],2)
    
    
    
    reactable(
      DRI_table[c('Kunta', 'DRI','DHI', 'DVI')],
      columns = list(
        Kunta = colDef(name = "Kunta", filterable = TRUE),
        DRI = colDef(name = "DRI"),
        DHI = colDef(name = 'DHI'),
        DVI = colDef(name = 'DVI')),
        
      highlight = TRUE
    )
    
    
  })
  
   
}


#### ShinyApp User Interface ---------------------------------------------------
ui <- shinyUI(fluidPage(
  
  
  useShinyjs(),
  # Style from css file
  theme = "app_style.css",

  # Set fonts and style
  tags$head(tags$link(rel = "stylesheet",
                      type = "text/css",
                      href="//fonts.googleapis.com/css?family=Raleway"),
            htmltools::includeCSS(csspath)),
  
  
  headerPanel(
    title=tags$a(href='https://www.syke.fi/fi-FI', target="_blank"),
    windowTitle = "Kuivuusriskit Suomessa"),
  
  titlePanel(h3("Kuivuusriskit Suomessa")),
  
  
  
  # First tab #########
  
  tabsetPanel(
    tabPanel("Kuivuusriski", fluid = TRUE,
             # Sidebar
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 id = "sidebar",
                 
                 strong("Tarkastele kuivuusriskiä Suomessa"),
                 helpText("Sivustolle on koottu tietoa arvioidusta kuivuusriskistä, kuivuusvaarasta ja haavoittuvuudesta kuivuudelle Suomessa kuntatasolla. Valitse haluamasi karttataso tai tarkastele kuvaajista ja taulukosta riskin suuruutta kunnittain."),
                 div(),
                 br(),
                 helpText("Kuivuushaavoittuvuuden muodostuminen valitussa kunnassa. Valitse kunta alasvetovalikosta."),
                 
                 # Select input municipality
                 selectInput(inputId = "location",
                             label = HTML("Valitse kunta"),
                             choices = locations,selected = ""),
                 
                 # Graph
                 ggiraphOutput("plo2", 
                               width = "100%",
                               height = "100%"),
                 
                 helpText("Lue lisää muuttujista ja menetelmistä 'Lisätietoa'-välilehdeltä."),
                 div(),
                 br(),
                 HTML(paste("<p id='version-info' style='color: grey; font-size: small;'>Versio<br>", 
                            app_v, "</p>")),
                 
                 
               ),
               
               
               
               # Main panel
               mainPanel(
                 
                 fluidRow(
                   
                   column(9,
                          
                          strong("KARTTA: Tarkastele kuivuusriskiä, kuivuusvaaraa ja kuivuushaavoittuvuutta Suomen kunnissa"),
                          # Map
                          leafletOutput("map1", height = 750, width = "100%"),
                          
                   ),
                   column(7,
                          br(),
                          strong("TAULUKKO: Kuivuusriski (DRI), kuivuusvaara (DHI) ja kuivuushaavoittuvuus (DVI) Suomen kunnissa taulukoituna."),
                          reactableOutput("table1", width = "100%")
                   ),
                   column(9,
                          br(),
                          strong("KUVAAJA: TOP 20 kuivuudelle haavoittuvinta kuntaa. Jokaisen kunnan kohdalla kuvataan haavoittuvuusindeksin muodostavien indikaattorien osuudet."),
                          # Graph
                          ggiraphOutput("plo", 
                                        width = "100%",
                                        height = "100%"),)
                 ),
               )
               
             )),
    
    
    # Second tab ##########
    
    tabPanel("Lisätietoa", fluid = TRUE,
             # Sidebar
             sidebarLayout(
               sidebarPanel(
                 width = 2,
                 id = "sidebar",
                 strong("Tällä sivulla:"),
                 em("taustaa, lisätietoa, yhteystiedot ja palaute."),
                 
                 
                 
               ),
               # Main panel
               mainPanel(
                 
                 fluidRow(
                   column(10,
                          includeMarkdown('userguide/information_droughtrisk_FI.rmd'))
                   
                   
                 ))
             )
    )
  ))
)




### Run ShinyApp ---------------------------------------------------------------

shinyApp(ui = ui, server = server)
