#### LIBRARIES ####

#require(remotes)
#install_version("shiny.semantic", version = "0.4.0", repos = "http://cran.us.r-project.org")

library(shiny)
library(shiny.semantic)
library(leaflet)
library(data.table)
library(geosphere)
library(dplyr)
library(ggplot2)
library(cowplot)

#### DATA ####

shipsData <- fread("./data/ships.csv")

#### MODULES ####

shipSelection_UI <- function(id, shipsData) {
  ns <- NS(id)
  
  tagList(
    header(
      title = "Shiny Dashboard of Vessels Activity",
      description = ""
    ),
    dropdown_input(
      input_id = ns("shipType"),
      choices = unique(shipsData$ship_type),
      type = "search selection multiple"
    ),
    dropdown_input(
      input_id = ns("shipName"),
      choices = NULL,
      type = "search selection multiple"
    ),
    tableOutput(
     outputId = ns("shipData")
    ),
    leafletOutput(
      outputId = ns("shipMap")
    ),
    plotOutput(
      outputId = ns("shipPlot")
    ),
    textOutput(
      outputId = ns("dataCredits")
    ),
    textOutput(
      outputId = ns("author")
    )
  )
}

shipSelection_server <- function(id, shipsData) {
  moduleServer(
    id,
    function(input, output, session) {
      observe({
        data <- shipsData[shipsData$ship_type %in% input$shipType,]
        update_dropdown_input(
          session, 
          input_id = "shipName",
          choices= unique(data$SHIPNAME)
        )
      })
      shipData <- reactive({
        #ship type
        validate(need(input[["shipType"]] != "", "Please select a ship type"))
        data <- shipsData[shipsData$ship_type %in% input[["shipType"]],]
        #ship name
        validate(need(input[["shipName"]] != "", "Please select a ship name"))
        data <- data[data$SHIPNAME %in% input[["shipName"]],]
        #create id
        data$pts_ID <- 1:nrow(data)
        #distance between consecutive points
        data.dist <- data %>% 
          group_by(SHIPNAME) %>% 
          mutate(lag_LAT = lag(LAT), lag_LON = lag(LON)) %>% 
          mutate(distance = diag(distm(cbind(lag_LON, lag_LAT), cbind(LON, LAT), fun = distHaversine)),
                 timespan = difftime(DATETIME, lag(DATETIME), units = "secs")) %>% 
          mutate(dist_tot = sum(distance,na.rm=T)) %>% 
          slice_max(distance) %>% 
          slice_max(timespan) %>%
          select(pts_ID,SHIPNAME,ship_type,LENGTH,DESTINATION,port,distance,dist_tot,DATETIME) %>%
          rename_with(toupper) %>%
          rename(DISTANCE_SEGMENT = DISTANCE, DISTANCE_TOTAL = DIST_TOT,
                 SHIP_NAME = SHIPNAME, CURRENT_PORT = PORT, SHIP_LENGTH = LENGTH) %>%
          ungroup()
        #for mapmaking & text
        data.map <- data[c(data.dist$PTS_ID,data.dist$PTS_ID-1),]
        data.dist$PTS_ID <- NULL
        data.dist$DATETIME <- as.character(data.dist$DATETIME)
        data.dist$TOTAL_OBSERVATIONS <- nrow(data)
        #return values
        return(list(data.dist,data.map,data))
      })
      #output distances
      output$shipData <- renderTable({shipData()[[1]]})
      #output map
      output$shipMap <- renderLeaflet({
        leaflet() %>%
          addProviderTiles(providers$Stamen.TonerLite,
                           options = providerTileOptions(noWrap = TRUE)) %>%
          addCircleMarkers(lng=shipData()[[2]]$LON[1],
                           lat=shipData()[[2]]$LAT[1],
                           color = "blue") %>%
          addCircleMarkers(lng=shipData()[[2]]$LON[2],
                           lat=shipData()[[2]]$LAT[2],
                           color= "blue") %>%
          addPolylines(lng=shipData()[[2]]$LON,
                       lat=shipData()[[2]]$LAT)
      })
      #output plots
      output$shipPlot <- renderPlot({
        #get data, sample if needed
        data <- shipData()[[3]]
        if(nrow(data)>1000){
          data <- data[sample(1:nrow(data),1000),]
        }
        #speed
        if(!all(data$SPEED <= 5)){
          p <- ggplot(data,aes(x=SPEED)) +
            geom_histogram(bins=20,fill="darkblue") +
            scale_x_continuous(limits=c(5,100),breaks=c(5,25,50,75,100)) +
            labs(y="COUNT",x="KNOTS") +
            ggtitle("SHIP SPEED (ONLY VALUES > 5 KNOTS)")
        }else{
          p <- ggplot() +
            theme_void() +
            geom_text(aes(0,0,label="ALL VALUES LESS THAN 5 KNOTS")) +
            xlab(NULL)
        }
        #course
        q <- as.data.frame(table(data$COURSE))
        q$Var1 <- as.numeric(q$Var1)
        q$group <- "a"
        if(nrow(q)>1){
          q <- ggplot(q, aes(x = Var1, y = Freq, col=group, group = group)) +
            theme(legend.position = "none") +
            geom_line(color = "darkblue") +
            scale_x_continuous(limits=c(0,360),breaks=c(0,90,180,270)) +
            coord_polar() +
            labs(y="COUNT",x="ANGLES") +
            ggtitle("OBSERVATIONS ANGLES OF SHIP COURSE")
        }else{
          q.val <- paste0('ALL VALUES EQUAL TO ',q$Var1)
          q <- ggplot() +
            theme_void() +
            geom_text(aes(0,0,label=q.val)) +
            xlab(NULL)
        }
        #time
        r <- data %>% 
          group_by(SHIPNAME) %>% 
          mutate(lag_LAT = lag(LAT), lag_LON = lag(LON)) %>% 
          mutate(distance = diag(distm(cbind(lag_LON, lag_LAT), cbind(LON, LAT), fun = distHaversine)),
                 timespan = difftime(DATETIME, lag(DATETIME), units = "mins")) %>% 
          mutate(dist_tot = sum(distance,na.rm=T)) %>% 
          ungroup()
        r$timespan <- abs(as.numeric(r$timespan))
        r <- r[!r$timespan %in% boxplot.stats(r$timespan)$out,]
        r <- ggplot(r, aes(x=timespan)) +
          theme(axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank()) +
          geom_boxplot(width=0.025) +
          coord_flip() +
          xlab("MINUTES") +
          ggtitle("TIMESPAN BETWEEN SHIP OBSERVATIONS")
        #merge plots
        s <- plot_grid(p,q,r,nrow=1)
        s
      })
      #credits
      output$dataCredits <- renderText({"Data: Appsilon"})
      output$author <- renderText({"Author: Fabián Santos"})
    }
  )
}

#### APP UI ####

ui <- semanticPage(
  shipSelection_UI("shp1", shipsData)
)

#### APP SERVER ####

server <- function(input, output, session) {
  shipSelection_server("shp1", shipsData)
}

#### CONSTRUCT APP ####

shinyApp(ui, server)



