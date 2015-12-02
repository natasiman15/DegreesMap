
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

# http://www.nsf.gov/statistics/seind14/content/chapter-8/8-18_all.xls


  server = function(input, output) {
    
    require(XLConnect)
    library(ggvis)
    library(ggplot2)
    library(reshape)
    library(maps)
    library(dplyr)
    library(mapproj)
    library(stringr)
    link <- "http://www.nsf.gov/statistics/seind14/content/chapter-8/8-18_all.xls"
    temp <- tempfile()
    download.file(link,temp, method = "curl")
    
    n = 6
    
    colnames1 <- readWorksheetFromFile(temp, "8-18_all", header = FALSE, startRow =4, endRow = 4, startCol = 48, endCol = 56)
    colnames2 <- readWorksheetFromFile(temp, "8-18_all", header = FALSE, startRow =4, endRow = 4, startCol = 58, endCol = 69)
    colnames <- cbind(colnames1, colnames2)
    
    States <- readWorksheetFromFile(temp, "8-18_all", header = FALSE, startRow =6, endRow =56, startCol = 1, endCol = 1)
    Degrees1 <- readWorksheetFromFile(temp, "8-18_all", header= FALSE, startRow = 6, endRow = 56, startCol = 48, endCol = 56)
    Degrees2 <- readWorksheetFromFile(temp, "8-18_all", header= FALSE, startRow = 6, endRow = 56, startCol = 58, endCol = 69)
    
    Num_deg1 <-readWorksheetFromFile(temp, "8-18_all", header= FALSE, startRow = 6, endRow = 56, startCol = 2, endCol = 10)
    Num_deg2 <- readWorksheetFromFile(temp, "8-18_all", header= FALSE, startRow = 6, endRow = 56, startCol = 12, endCol = 23)
    Num_deg <- cbind(States,Num_deg1,Num_deg2)
    
    Degrees <- cbind(States,Degrees1, Degrees2)
    #rownames(Degrees) <- Degrees$Col1
    #Degrees <- subset(Degrees, select = -Col1)
    colnames(Degrees) <- cbind("States",colnames)
    colnames(Degrees)[1] <- "State"
    colnames(Num_deg) <- cbind("States",colnames)
    colnames(Num_deg)[1] <- "State"
    
    rm(colnames1,colnames2,States,Degrees1,Degrees2,colnames,Num_deg1,Num_deg2)
    
   
    Degrees$State <- tolower(Degrees$State)
    Degrees$State <- str_trim(Degrees$State,side="right")
    colnames(Degrees)[2:22] <- paste("Y",colnames(Degrees)[2:22],"Y",sep="")
    
    
    Num_deg$State <- tolower(Num_deg$State)
    Num_deg$State <- str_trim(Num_deg$State,side="right")
    colnames(Num_deg)[2:22] <- paste("Y",colnames(Num_deg)[2:22],"Y",sep="")
    
    
    
    #IMPORT STATES
    states <- map_data("state")
    state_data <- merge(states,Degrees, by.x="region",by.y="State")
    state_data_num <- merge(states,Num_deg, by.x="region",by.y="State")
    
    add_title <- function(vis, ..., x_lab = " ", title = "Plot Title") 
    {
      add_axis(vis, "x", title = x_lab) %>% 
        add_axis("x", orient = "top", ticks = 0, title = title,
                 properties = axis_props(
                   axis = list(stroke = "white"),
                   labels = list(fontSize = 0)
                 ), ...)
    }
 
    observeEvent(input$yearnum,{
      year <- paste("Y",input$yearnum,"Y",sep="")
      state_data %>%
      ggvis(~long,~lat) %>%
      group_by(group) %>%
      layer_paths(fill = as.name(year)) %>%
        add_title(title="Degrees Awarded per 1000 Individuals in 18-24 Year Old Pop") %>%
       bind_shiny("ggvisPlot")
    })
    
    observeEvent(input$yearnum,{
      year <- paste("Y",input$yearnum,"Y",sep="")
      state_data_num %>%
        ggvis(~long,~lat) %>%
        group_by(group) %>%
        layer_paths(fill = as.name(year)) %>%
        add_title(title = "Total Bachelors Degrees Awarded") %>%
        bind_shiny("ggvisPlot2")
    })
    
    
  }
    
