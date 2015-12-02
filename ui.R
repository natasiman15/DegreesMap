library(shiny)
library(ggvis)

shinyUI(fluidPage(
  titlePanel("Science & Engineering Degrees Awarded"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("yearnum","year",1990,2011,value=1990,step=2,sep="")
    ),
    mainPanel(
      ggvisOutput("ggvisPlot"),
      
      ggvisOutput("ggvisPlot2")
    )
  )
))