library(shiny)
library(shinyjs)
library(leaflet)
library(shinythemes)
library(leaflet.extras)
library(DT)
library(janitor)
library(ggplot2)
library(dplyr)
library(scales)
library(tidyverse)
library(lubridate)
library(RSocrata)
library(leaflet)
library(scales)
library(sf)
library(sp)
library(rgdal)
library(rsconnect)

#years_ago = today() - years(4)
#crash_url = glue::glue("https://data.cityofchicago.org/Transportation/Traffic-Crashes-Crashes/85ca-t3if?$where=CRASH_DATE > '{years_ago}'")
#crash_raw = as_tibble(read.socrata(crash_url))


#crash = crash_raw %>%
#  arrange(desc(crash_date)) %>%
#  transmute(
#    injuries = as.factor(if_else(injuries_total > 0, 'Sim', 'Não')),
#    crash_date = as.Date(crash_date, format = "%D"),
#    crash_hour,
#    crash_year = year(crash_date),
#    crash_month = factor(month.abb[month(crash_date)], levels = month.abb),
#    crash_weekday = factor(weekdays(crash_date, abbreviate = TRUE),
#                           levels = c('dom','seg','ter','qua','qui','sex','sáb')),
#    report_type = if_else(report_type == "", "UNKNOWN", report_type),
#    num_units,
#    posted_speed_limit,
#    weather_condition,
#    lighting_condition,
#    roadway_surface_cond,
#    first_crash_type,
#    trafficway_type,
#    prim_contributory_cause,
#    latitude, longitude
#  ) %>%
#  .[!(.$longitude > -40),] %>%
#  na.omit() 

crash = read.csv("crash.csv")[-1] %>%
    transmute(
        injuries,
        crash_date = as.Date(crash_date),
        crash_hour,
        crash_year,
        crash_month = factor(crash_month, levels = month.abb),
        crash_weekday = factor(crash_weekday,
                               levels = c('dom','seg','ter','qua','qui','sex','sáb')),
        report_type,
        num_units,
        posted_speed_limit,
        weather_condition,
        lighting_condition,
        roadway_surface_cond,
        first_crash_type,
        trafficway_type,
        prim_contributory_cause,
        latitude, longitude)

beats_shp = readOGR(
    dsn = "./ChicagoPoliceBeats",
    layer = "chiBeats")


#mi = min(crash$crash_date)
#ma = max(crash$crash_date)
#va = c(min(crash$crash_date+1),min(crash$crash_date+2))

ferid_n = c('Houve feridos', 'Não houve feridos', 'Todos')
colors = c("#FDC25A", "#FD7834", "#E9151C", "#891E14", "#45150C", "#000000")
global_colors = setNames(colors[1:2], unique(crash$injuries))
gc = setNames(c("navy", "red"), unique(crash$injuries)) 
theme_set(theme_minimal() + theme(legend.position="bottom"))



ui = navbarPage("Acidentes no Trânsito em Chicago", id = 'nav',
                theme = shinytheme("united"),
                tabPanel("Visualização",
                         useShinyjs(),
                         mainPanel(div(class = "bigmap",
                                       tags$div(
                                           tags$style("div.bigmap {
                                         position: fixed;
                                         top: 41px;
                                         left: 0;
                                         right: 0;
                                         bottom: 0;
                                         overflow: hidden;
                                         padding: 0;}")), 
                                       
                                       leafletOutput("map",width = "100%", height = "100%"))),
                         absolutePanel(draggable = TRUE, bottom = 15,
                                       actionButton("showSidebar", "Mostrar Opções"),
                                       actionButton("hideSidebar", "Ocultar Opções")),
                         
                         sidebarPanel(id = "Sidebar",
                                      selectInput("type_m",
                                                  "Tipo de Acidente:",
                                                  ferid_n),
                                      sliderInput('range',"Período de Tempo:",
                                                  min = min(crash$crash_date),
                                                  max = max(crash$crash_date),
                                                  value = c(min(crash$crash_date+1),min(crash$crash_date+2))
                                                  #min = mi,
                                                  #max = ma,
                                                  #value = va
                                      ),
                                      radioButtons('qlmapa', "Tipo de mapa:",
                                                   choices = list("Pontos" = 1,
                                                                  "Heatmap" = 2,
                                                                  "Polígonos" = 3)),
                                      plotOutput('gg',height = 260)
                         )),
                tabPanel("Gráficos",
                         fluidRow(
                             column(4,
                                    h5("Acidentes por Ano"),
                                    plotOutput("g1")),
                             column(4,
                                    h5("Acidentes por Mês"),
                                    plotOutput("g2")),
                             column(4,
                                    h5("Acidentes por Dia"),
                                    plotOutput("g3"))),
                         fluidRow(
                             column(4, offset = 1,
                                    h5("Acidentes por Hora"),
                                    plotOutput("g4")),
                             column(4, offset = 2,
                                    dataTableOutput("table"),
                                    br(),
                                    br(),
                                    htmlOutput("t1"),
                                    htmlOutput("t2"))
                             
                             
                         )))

server = function(input, output) {
    
    pal = colorFactor(c("navy", "red"), domain = c('Sim', 'Não'))
    
    df = reactive({
        if(input$type_m == 'Houve feridos'){
            subset(crash, crash_date >= input$range[1] & 
                       crash_date <= input$range[2] &
                       injuries == 'Sim')}
        else if(input$type_m == 'Não houve feridos'){
            subset(crash, crash_date >= input$range[1] & 
                       crash_date <= input$range[2] &
                       injuries == 'Não')}
        else if(input$type_m == 'Todos'){
            subset(crash, crash_date >= input$range[1] & 
                       crash_date <= input$range[2])}
    })
    
    output$map =
        renderLeaflet({
            leaflet() %>%
                addTiles() %>%
                setView(lng = -87.5754412, lat = 41.9223509, zoom = 10)
        })
    
    observe({
        if(input$qlmapa == 1){
            if(nrow(df()) > 40000){
                leafletProxy("map", data = df()) %>%
                    clearMarkers() %>%
                    clearHeatmap() %>% 
                    clearControls() %>%
                    clearMarkerClusters() %>%
                    addCircleMarkers(lat = ~latitude,
                                     lng = ~longitude,
                                     fillOpacity = 0.7,
                                     color = ~pal(injuries),
                                     stroke = FALSE,
                                     radius = 1,
                                     clusterOptions = markerClusterOptions)}
            else {
                leafletProxy("map", data = df()) %>%
                    clearMarkers() %>%
                    clearHeatmap() %>% 
                    clearShapes() %>%
                    clearControls() %>%
                    clearMarkerClusters() %>%
                    addCircleMarkers(lat = ~latitude,
                                     lng = ~longitude,
                                     fillOpacity = 0.7,
                                     color = ~pal(injuries),
                                     stroke = FALSE,
                                     radius = 1.6,
                                     clusterOptions = markerClusterOptions)
            }
        } 
        if(input$qlmapa == 2){
            leafletProxy("map", data = df()) %>%
                clearMarkers() %>%
                clearHeatmap() %>% 
                clearShapes() %>%
                clearControls() %>%
                clearMarkerClusters() %>%
                addHeatmap(
                    lng = ~longitude, lat = ~latitude,
                    blur = 15, radius = 7)
        } 
        
        if(input$qlmapa == 3){
            pnt = cbind(df()$latitude,df()$longitude) %>% data.frame
            coordinates(pnt) = ~ X2 + X1
            proj4string(pnt) = CRS("+proj=longlat")
            pnt = spTransform(pnt, proj4string(beats_shp))
            
            d = over(pnt, beats_shp)
            bs = beats_shp %>% st_as_sf
            
            innj = d %>% count(district, beat_num, sector, beat) %>% 
                inner_join(bs, by = c("district" = "district" ,
                                      "beat_num" = "beat_num",
                                      "sector" =  "sector",
                                      "beat" = "beat")) %>%
                .[1:(nrow(.)-3),] %>% 
                st_as_sf 
            
            pla = colorBin("YlOrRd", domain = innj$n) #cores do mapa
            
            leg = sprintf(
                "Distrito <strong>%s</strong><br/>
                %1.0f acidentes",
                innj$district, innj$n) %>% 
                lapply(HTML) #legenda
            
            leafletProxy("map", data = innj) %>%
                clearMarkers() %>%
                clearHeatmap() %>% 
                clearShapes() %>%
                clearControls() %>%
                clearMarkerClusters() %>%
                addPolygons(
                    color = "#444444",
                    weight = 1, 
                    smoothFactor = 0.5,
                    opacity = 1.0, 
                    fillOpacity = 1, 
                    fillColor = ~ pla(n),
                    highlight = highlightOptions(
                        weight = 2,
                        color = "#666",
                        bringToFront = TRUE),
                    label = leg,
                    labelOptions = labelOptions(
                        style = list("font-weight" = "normal", padding = "2px 5px"),
                        textsize = "12px",
                        direction = "auto")
                ) %>%
                addLegend(pal = pla, values = ~(n), opacity = 1, 
                          title = 'Total', position = 'topleft')
        } 
        
        
    })
    
    output$g1 = renderPlot({
        df() %>%
            count(crash_year, injuries) %>%
            group_by(injuries) %>%
            mutate(percent = n / sum(n)) %>%
            ungroup() %>%
            ggplot(aes(factor(crash_year),percent, fill = injuries)) +
            geom_bar(stat = "identity",position = 'dodge') +
            scale_y_continuous(labels = percent_format()) +
            labs(y = "% dos Acidentes", x = NULL, fill = "Acidente com machucados:") + 
            scale_fill_manual(values = global_colors) + 
            geom_text(aes(label=n),stat='identity',hjust = 0.6,
                      position = position_dodge(width = 0.9), angle = 90)
    })
    output$g2 = renderPlot({
        df() %>%
            count(crash_month, injuries) %>%
            group_by(injuries) %>%
            mutate(percent = n / sum(n)) %>%
            ungroup() %>%
            ggplot(aes(crash_month, percent, fill = injuries)) +
            geom_bar(stat = "identity",position = 'dodge') +
            scale_y_continuous(labels = percent_format()) +
            labs(y = "% dos Acidentes", x = NULL, fill = "Acidente com machucados:") + 
            scale_fill_manual(values = global_colors) + 
            geom_text(aes(label=n),stat='identity',hjust = 0.6,
                      position = position_dodge(width = 0.9), angle = 90)
    })
    
    output$g3 = renderPlot({
        df() %>%
            count(crash_weekday, injuries) %>%
            group_by(injuries) %>%
            mutate(percent = n / sum(n)) %>%
            ungroup() %>%
            ggplot(aes(crash_weekday,percent, fill = injuries)) +
            geom_bar(stat = "identity",position = 'dodge') +
            scale_y_continuous(labels = percent_format()) +
            labs(y = "% dos Acidentes", x = NULL, fill = "Acidente com machucados:") + 
            scale_fill_manual(values = global_colors) + 
            geom_text(aes(label=n),stat='identity',hjust = 0.6,
                      position = position_dodge(width = 0.9), angle = 90)
    })
    
    output$g4 = renderPlot({
        df() %>%
            count(crash_hour, injuries) %>%
            group_by(injuries) %>%
            mutate(percent = n / sum(n)) %>%
            ungroup() %>%
            ggplot(aes(factor(crash_hour),percent, fill = injuries)) +
            geom_bar(stat = "identity",position = 'dodge') +
            scale_y_continuous(labels = percent_format()) +
            labs(y = "% dos Acidentes", x = NULL, fill = "Acidente com machucados:") + 
            scale_fill_manual(values = global_colors) + 
            geom_text(aes(label=n),stat='identity',hjust =  0.6,
                      position = position_dodge(width = 0.9), angle = 90)
    })
    
    
    output$gg = renderPlot({
        ggplot(data = st_as_sf(beats_shp)) + 
            geom_sf(fill = 'white') + 
            geom_point(data = df(), aes(x = longitude, 
                                        y = latitude, 
                                        col = injuries), 
                       size = .2) + 
            theme_void() + 
            scale_color_manual(values = gc) +
            labs(color = "Machucados:")
    }, bg="transparent")
    
    output$table = DT::renderDataTable({
        df() %>%
            count(injuries) %>%
            mutate(Porcentagem = n / sum(n)) %>%
            setNames(c('Feridos','Ocorrido','Porcentagem')) %>% 
            adorn_totals %>% 
            data.frame
    })
    
    
    output$t1 = renderText({
        most = df() %>% count(crash_date)
        paste("O dia com mais acidentes foi", 
              strong(most[which.max(most$n),1]),
              "com", 
              strong(max(most$n)),
              'acidentes.') %>% HTML
    })
    
    output$t2 = renderText({
        if(input$type_m == 'Não houve feridos'){
            "Não houve feridos."
        } else {
            most = df() %>% count(crash_date,injuries) %>% subset(injuries == "Sim")
            paste("O dia com o maior número de machucados foi", 
                  strong(most[which.max(most$n),1]),
                  "com", 
                  strong(max(most$n)),
                  "feridos.") %>% HTML}
    })
    
    
    observeEvent(input$showSidebar, {
        shinyjs::show(id = "Sidebar")
    })
    observeEvent(input$hideSidebar, {
        shinyjs::hide(id = "Sidebar")
    })
    
}  


shinyApp(ui = ui, server = server)

#setAccountInfo(name='enzobertoldi',
               # token='FC5D34B2BC68F56B997B9D25D4019700',
                #secret='avoQQPrhf+3vmdDCPBaMGawWqmz+QG5YDD4k0s+K')

#deployApp()

