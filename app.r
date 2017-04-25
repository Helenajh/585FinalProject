
#
setwd("U:/Helena/585/Final_project")
library(plotly)
library(shiny)
library(ggplot2)
library(rvest)


amesHousingDat <- read.csv("Ames_housing.csv", header = T)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  titlePanel("House Sale History Search"),
  
  sidebarLayout(
    sidebarPanel(
    
      selectInput("Type", label = h3("Select Building Type"), 
                  choices = unique(amesHousingDat$Occupancy),
                  selected = "Single-family detached (includes detached townhouses)"),
      
      checkboxGroupInput("Bedrooms", label = h3("Select number of bedrooms"), 
                         choices = list("1 bed(orange)" = 1, "2 beds (blue)" = 2, "3 beds (red)" = 3, 
                                        "4 beds (green)" = 4, "5 beds (purple)" = 5, "6 beds (yellow)" = 6,
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
)




server <- function(input, output) {

  ames_tab2_subset <- reactive(
    amesHousingDat %>% filter(Occupancy == input$Type, Bedrooms %in% c(input$Bedrooms), 
                            Year.Built > input$Year[1], Year.Built < input$Year[2])
    )
  
  output$salePriceAreaBed <- renderPlotly({
    ames_tab2_subset() %>% 
      ggplot(aes(y = Sale.Price/1000, x = Total.Living.Area, 
                 colour = as.factor(Bedrooms),
                 shape = factor(Style),
                 alpha = 0.5
      )
      ) + 
      geom_point() + 
      theme_classic() + 
      xlab("Total Living Area (sq ft)") + 
      ylab("\n Sale Price(k) \n") + 
      # scale_colour_discrete(name  = "Bedroom") + 
      theme(legend.position = "none")
    #    theme(legend.position = "bottom") 
  })
}


# Run the application 
shinyApp(ui = ui, server = server)




