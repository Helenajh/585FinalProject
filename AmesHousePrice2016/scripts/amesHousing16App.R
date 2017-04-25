## working directory as project directory 
library(shiny)
library(tidyverse)
library(leaflet)
library(plotly)

##------------------------------------------------------
## reading ames housing 16 data and some preprocessing 
##------------------------------------------------------
amesHousingDat <- read.csv("../data/StoryCountyIA-NEW_house-LatLong.csv", 
                           stringsAsFactors = F)

## Calculate Month Share (out of sales in the whole year 2016)
monthShareDat <- amesHousingDat %>% 
  ggvis::compute_count(~ factor(month)) %>% 
  mutate(month = factor(c(1:12)),
         percent = count_ / 952 * 100,
         text =  c("5.57% share", "4.83% share", "9.03% share",
                   "7.04% share", "11.1% share", "16.6% share",
                   "10.4% share", "10.4% share", "6.72% share",
                   "6.93% share", "6.41% share", "4.94% share")) %>% 
       data.frame() 

## month average sale 
monthAvgSale <- amesHousingDat %>% group_by(month) %>% 
  summarise(saleMeanPerMonth = mean(Sale.Price))

## add house image url & house detail info into data set 
amesHousingDat <- amesHousingDat %>% 
  mutate(hourseImgURL = paste0("https://beacon.schneidercorp.com/PhotoEngine/Photo/165/0", Parcel.ID, "/0/1.jpg"),
  houseInfoURL = paste0("https://beacon.schneidercorp.com/Application.aspx?AppID=165&LayerID=2145&PageTypeID=4&PageID=1108&Q=1851842183&KeyValue=0", Parcel.ID)) %>% 
  data.frame() 

## add Sales price range: 1: <$100K; 2: <$150K; 3: <$200K; 4: <250K; 5: <300K; 6:>300K
idx_1 <- amesHousingDat$Sale.Price < 100000
idx_2 <- amesHousingDat$Sale.Price < 150000 & amesHousingDat$Sale.Price >= 100000
idx_3 <- amesHousingDat$Sale.Price < 200000 & amesHousingDat$Sale.Price >= 150000
idx_4 <- amesHousingDat$Sale.Price < 250000 & amesHousingDat$Sale.Price >= 200000
idx_5 <- amesHousingDat$Sale.Price < 300000 & amesHousingDat$Sale.Price >= 250000
idx_6 <- amesHousingDat$Sale.Price >= 300000 

priceRange <- rep(NA, nrow(amesHousingDat))
priceRange[idx_1] <- 1
priceRange[idx_2] <- 2
priceRange[idx_3] <- 3
priceRange[idx_4] <- 4
priceRange[idx_5] <- 5
priceRange[idx_6] <- 6

amesHousingDat$priceRange <- as.character(priceRange)

## add priceRange color 
#priceRangeColorsVec <- c("#2166ac", "#67a9cf", "#7fbf7b", "#998ec3", "#ef8a62", "#b2182b")
priceRangeColorsVec <- c("#fa9fb5", "#1b9e77", "#984ea3", "#fb8072", "#377eb8", "#e41a1c")
priceRangeVec <-  c("<$100K", "$100K ~ $150K", "$150K ~ $200K", "$200K ~ $250K",
                    "$250K ~ $300K", ">$300K")

amesHousingDat$priceRangeColor <- as.character(sapply(amesHousingDat$priceRange, 
                                                      function(priceRange){
  if (priceRange == "1") {
    priceRangeColorsVec[1]
  } else if (priceRange == "2") {
    priceRangeColorsVec[2]
  } else if (priceRange == "3") {
    priceRangeColorsVec[3]
  } else if (priceRange == "4") {
    priceRangeColorsVec[4]
  } else if (priceRange == "5") {
    priceRangeColorsVec[5]
  } else {
    priceRangeColorsVec[6]
  }
}))

icons <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = amesHousingDat$priceRangeColor
)

##------------------------------------------------------
## UI 
##------------------------------------------------------

ui <- bootstrapPage(' ',
                    
navbarPage("2016 Ames Iowa House Sale",
  ## tab1: Housing Sales vs Time 
  tabPanel("Sales by Month",
                  fluidRow(column(3, offset = 1,
                          radioButtons("houseByMonthBarPie",
                                       "Bar Plot or Pie chart:",
                                       c("Bar plot" = "bar",
                                        "Pie chart" = "pie")))),
                 fluidRow(column(6,  plotlyOutput('houseByMonth')),
                          column(6,  plotlyOutput('boxplotSaleByMonth')))
    ),
    
    ## tab 2: Sales Overview
  tabPanel("Sales Overview",
         #  plotlyOutput("salePriceClass"),
         # fluidRow(column(8, plotlyOutput("salePriceClass"))),
         # fluidRow(column(8, plotlyOutput("saleAssessedValue")))
         fluidRow(
           column(10, plotlyOutput("salePriceClass")),
           column(10, plotlyOutput("saleAssessedValue"))
         )
           
  ),
  
    ## tab 3: House Sale History Search
    tabPanel("House Sale History Search",
             sidebarLayout(
               sidebarPanel(
                 
                 selectInput("Type", label = h3("Select Building Type"), 
                             choices = unique(amesHousingDat$Occupancy),
                             selected = "Single-family detached (includes detached townhouses)"),
                 
                 checkboxGroupInput("Bedrooms", label = h3("Select number of bedrooms"), 
                                    choices = list("1 bed(orange)" = 1, "2 beds (blue)" = 2, 
                                                   "3 beds (red)" = 3,  "4 beds (green)" = 4, 
                                                   "5 beds (purple)" = 5, "6 beds (yellow)" = 6,
                                                   "7 beds (pink)" = 7, "8 beds (brown)" = 8),
                                    selected = 3), 
                 
                 sliderInput("Year", label = h3("Select Year Built"), min = 1890, 
                             max = 2016, value = c(1980,1995))
               ),
               
               # Show a plot of the generated distribution
               mainPanel(
                 plotlyOutput("salePriceAreaBed")
                 
               )
             )
       ),
  
  
   ## tab4: Housing Sales vs Location
   tabPanel("Sales vs Location",
            selectInput("priceRange", 
                        label = h4("Select Housing Price Range"),
                        choices = list(
                          "<$100K" = "1",
                          "$100K ~ $150K" = "2",
                          "$150K ~ $200K" = "3",
                          "$200K ~ $250K" = "4",
                          "$250K ~ $300K" = "5",
                          ">$300K" = "6"
                        ),
                        multiple = TRUE,
                        selected = "2"),
            leafletOutput("houseMap", height = "500px")
            
 #  ),
   )
),
tags$style(type = 'text/css', '.navbar { background-color: #3498DB;
                                               font-family: Arial;
           font-size: 17px;
           color: #FF0000; }',
           
           '.navbar-dropdown { background-color: #3498DB;
           font-family: Arial;
           font-size: 17px;
           color: #FF0000; }',
           "html, body {width:100%;height:100%}")
)


##------------------------------------------------------
## Server 
##------------------------------------------------------
server <- function(input, output) {
  ## tab1: plot: House Sale by Month, 2016 Ames Iowa
  output$houseByMonth <- renderPlotly({
    if (input$houseByMonthBarPie == "bar") {
      monthShareDat %>% 
        plot_ly(x = ~factor(month), y = ~count_, type = 'bar', text = ~text,
                marker = list(color = 'rgb(158,202,225)',
                              line = list(color = 'rgb(8,48,107)',
                                          width = 1.5))) %>%
        layout(title = "2016 Ames Iowa House Sale by Month",
               xaxis = list(title = ""),
               yaxis = list(title = ""))
    } else if (input$houseByMonthBarPie == "pie") {
      plot_ly(monthShareDat, labels = ~factor(month, ordered = TRUE), 
              sort = FALSE,
              direction = "clockwise",
              values = ~count_, type = 'pie') %>%
        layout(title = '2016 Ames Iowa House Sale by Month',
               xaxis = list(showgrid = FALSE, zeroline = FALSE, 
                            showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, 
                            showticklabels = FALSE),
               showlegend = TRUE)
    }
  })
  
  ## tab1: boxplot for ames month sale 
  output$boxplotSaleByMonth <- renderPlotly({
    plot_ly(x = amesHousingDat$month) %>%
      add_boxplot(y = amesHousingDat$Sale.Price, showlegend = F) %>%
      add_trace(x = seq(1,12), y = monthAvgSale$saleMeanPerMonth, 
                showlegend = F, name = '',
                mode = 'markers') %>% 
      layout(
        xaxis = list(range = c(0,13)),
        yaxis = list(range = c(0, 900000)))
  })
  
  ##------------------------------------------------------
  ## tab2: Sales Overview
  ##------------------------------------------------------
  output$salePriceClass <- renderPlotly({
    
    # crosstalk::SharedData$new(amesHousingDat)   %>% 
    #   plot_ly(x = ~Assessed.Value, y = ~Sale.Price,
    #           # Hover text:
    #           text = ~paste("Price: ", Sale.Price, '$<br>Year:', Year.Built),
    #           color = ~class,
    #           size = ~Sale.Price/400,
    #           frame = ~Year.Built#,
    #       #    interval = 0.00001
    #   ) %>%
    #   add_trace(type = "scatter", showlegend = T) %>%
    #   layout(
    # #    xaxis = list(range = c(1880, 2020)),
    #     yaxis = list(range = c(0, 900000))) %>%
    #   layout(xaxis = list(title = "Assessed Value"),
    #          yaxis = list(title = "Sale Price")) 
    # #%>% 
    # #  highlight("plotly_hover")
    # #  }
    
    amesHousingDat %>% plot_ly(x = ~Year.Built, y = ~Sale.Price,
                          # Hover text:
                          text = ~paste("Price: ", Sale.Price, '$<br>Year:', Year.Built),
                          color = ~class
    ) %>%
      add_trace(type = "scatter") %>%
      layout(
        xaxis = list(range = c(1880, 2020)),
        yaxis = list(range = c(0, 900000))) %>% 
      add_lines(x = ~Year.Built, y = ~fitted(loess(Sale.Price~Year.Built)), 
                showlegend = F, line = list(color = "purple")) %>% 
         layout(xaxis = list(title = "Year Built"),
                yaxis = list(title = "Sale Price")) 
    
  })
  
  output$saleAssessedValue <- renderPlotly({
    amesHousingDat %>% 
      plot_ly(x = ~Assessed.Value, y = ~Sale.Price,
              # Hover text:
              text = ~paste("Sale Price: ", Sale.Price, 
                            '$<br>Assessed Value:', Assessed.Value, 
                            '$<br>Age:', class,
                            '<br>Year of Built:', Year.Built)) %>%
      add_trace(type = "scatter", color = ~Sale.Condition) %>% 
      add_lines(x = ~Assessed.Value,
                y = ~fitted(lm(Sale.Price ~ Assessed.Value)), 
                showlegend = F) %>%
      layout(xaxis = list(title = "Assessed Value"),
             yaxis = list(title = "Sale Price"))
  })
  
  ##------------------------------------------------------
  ## tab3: House Sale History Search
  ##------------------------------------------------------
  bedroomsColVec <- c( "orange",  "royalblue", "red", 
                       "green3",  "purple",    "yellow1",
                        "pink",    "brown")
  bedroomsColVec <- setNames(bedroomsColVec, as.character(seq(1,8)))

  stypeShapeVec <- c(20, 0, 1, 2,3,4,5,6)
  stypeShapeVec <- setNames(stypeShapeVec, c("One story", "Two story", "Split foyer",
                                             "Split level","One and one-half story:  2nd level unfinished",
                                             "One and one-half story:  2nd level finished",
                                             "Two and one-half story:  3rd level unfinished",
                                             "Two and one-half story:  3rd level finished"))
  
  ames_tab2_subset <- reactive(
    amesHousingDat %>% filter(Occupancy == input$Type, Bedrooms %in% c(input$Bedrooms), 
                              Year.Built > input$Year[1], Year.Built < input$Year[2])
  )
  
  output$salePriceAreaBed <- renderPlotly({
    # g <- ames_tab2_subset() %>% 
    #   ggplot(aes(y = Sale.Price/1000, x = Total.Living.Area, 
    #              colour = as.factor(Bedrooms),
    #              shape = factor(Style),
    #              text = paste("Sale Price: ", Sale.Price/1000, 
    #                           'K$<br>Total Living Area:', Total.Living.Area, 
    #                           '<br>Year of Built:', Year.Built,
    #                           '<br>Finished basement area:', Finished.Bsmt.Area)
    #   ),                  alpha = 0.5
    #   ) + 
    #   scale_colour_manual(values = bedroomsColVec, guide = FALSE) + 
    #   scale_shape_manual("Stype", values = stypeShapeVec) + 
    #   geom_point() + 
    #   theme_classic() + 
    #   xlab("Total Living Area (sq ft)") + 
    #    ylab("\n Sale Price(k) \n") + 
    #    theme(legend.position = "bottom",legend.justification = c(0,0))
    # ggplotly(g)  %>% layout(legend = list(x = 0, y = -50, orientation = 'h'),
    #                         xaxis= list(showticklabels = FALSE))
    # %>% layout(legend = list(orientation = 'h'))
    ames_tab2_subset() %>%  
        plot_ly(x = ~Sale.Price/1000, y = ~Total.Living.Area,
            # Hover text:
                text = ~paste("Sale Price: ", Sale.Price/1000,
                             'K$<br>Total Living Area:', Total.Living.Area,
                              '<br>Year of Built:', Year.Built,
                              '<br>Finished basement area:', Finished.Bsmt.Area)) %>%
      add_trace(type = "scatter", color = ~as.factor(Bedrooms), colors = bedroomsColVec, 
                symbol = ~Style, symbols = stypeShapeVec) %>% 
      layout(# legend = list(orientation = 'h'),
             xaxis = list(title = "Total Living Area (sq ft)"),
             yaxis = list(title = "Sale Price(k)")
            )

  })
  

  ##------------------------------------------------------
  ## tab4: location map 
  ##------------------------------------------------------
  ## Show a popup at the given location 
  
  subPriceRangeAmesData <- reactive({
   df <- amesHousingDat %>% dplyr::filter(priceRange %in% as.character(input$priceRange)) 
   return(df)
  })
  
  popupContent <- function(d) {
    content <- paste(
                     paste0("<b>Price: </b>",  "$", d$Sale.Price/1000), 'K<br/>',
                     "<b>Bedrooms:</b>", d$Bedrooms, "<br/>",
                     "<b>Address:</b>", d$Address, "<br/>",
                     "<b>Built Year:</b>", d$Year.Built, "<br/>",
                     paste0("<b><a href='",d$houseInfoURL, "'>House website</a></b>"),
                     "<img src = ", d$hourseImgURL, ">"
                     )
   return(content)
  }
  
  output$houseMap <- renderLeaflet({
     leaflet(amesHousingDat) %>% 
      addTiles() %>%
      fitBounds(
        ~min(long), ~min(lat), ~max(long), ~max(lat)
      ) %>%
      addLegend("bottomleft", 
                colors = priceRangeColorsVec, labels = priceRangeVec, 
                title = "House Sale Price",
                opacity = 1)
    #%>%
    #  fitBounds(-93.69, 41.98, -93.65, 42.29)
  })
  
  
  observe({
    if (nrow(subPriceRangeAmesData()) == 0) {
      leafletProxy('houseMap', data = amesHousingDat) %>% clearMarkers()
    } else {
    #  cat(nrow(subPriceRangeAmesData()), "\n")
      leafletProxy('houseMap', data = subPriceRangeAmesData()) %>% 
    #    clearMarkers() %>% 
        clearGroup('A') %>% 
        addCircleMarkers( group = 'A',
                          lng = ~long, lat = ~lat,
                          color = ~priceRangeColor,
                          radius = 3,  fillOpacity = 0.7, 
                          popup = popupContent(subPriceRangeAmesData()))  
    }

  })

}



##------------------------------------------------------
## run
##------------------------------------------------------
shinyApp(ui, server)