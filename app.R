library(shiny)
library(shinydashboard)
library(shinythemes)
library(highcharter)
library(tidyverse)
library(rsconnect)
library(leaflet)
library(htmltools)

load('dataviz.RData')

shinyApp(
  ui = navbarPage("COVID-19 R.M.", theme = shinytheme("cosmo"),
                  tabPanel("Casos",
                           sidebarLayout(
                             sidebarPanel(
                               h2("Casos"),
                               'Seleccione la métrica en el desplegable de abajo, así como el rango de fechas que desea consultar.', 
                               br(),
                               br(),
                               dateRangeInput("daterange",
                                              "Rango de fechas:",
                                              start = as.Date('2021-01-01'),
                                              end = max(casos_comp$date),
                                              min = min(casos_comp$date),
                                              max = max(casos_comp$date)),
                               selectInput(
                                 inputId = 'metrica',
                                 label = 'Métrica',
                                 choices = c('IA14', 'UCI por millón de hab.', 'Hospitalizados por cada 100.000', 'Fallecidos por cada 100.000'))),
                             mainPanel(
                               highchartOutput("plot")
                             )),
                            fluidRow(
                              wellPanel(
                                h3('Notas'),
                                htmlOutput('texto_casos')
                              )
                            )
                           ),
                  
                  tabPanel("Vacunación",
                           sidebarLayout(
                             sidebarPanel(
                               h2("Vacunación"),
                               'Seleccione la métrica en el desplegable de abajo, así como el rango de fechas que desea consultar.', br(),
                               dateRangeInput("daterange_vac",
                                              "Rango de fechas:",
                                              start = min(vac_comp$date_vac),
                                              end = max(vac_comp$date_vac),
                                              min = min(vac_comp$date_vac),
                                              max = max(vac_comp$date_vac)),
                               selectInput(
                                 inputId = 'metrica_vac',
                                 label = 'Métrica',
                                 choices = c('% vacunas administradas', 'Pauta completada', 'Vacunas administradas', 'Vacunas entregadas'))),
                             mainPanel(
                               highchartOutput("plot_vac")
                             )),
                           
                           fluidRow(
                             wellPanel(
                               h3('Notas'),
                               htmlOutput('texto_vac')
                             )
                           )
                           
                           ),
                  
                  tabPanel("Municipios",  sidebarLayout(
                    sidebarPanel(
                      width = 5,
                      h2("Mapa por municipios"),
                      'Seleccione la métrica en el desplegable de abajo.', br(),
                      selectInput(
                        inputId = 'metrica_map',
                        label = 'Métrica',
                        choices = c('IA14', 'Nivel de alerta')), textOutput('last_updt')),
                    mainPanel(
                      width = 12-5,
                      leafletOutput('mapa_mu'))),
                    br(),
                    fluidRow(
                      column(6, wellPanel(h3('Municipios con mayor incidencia'),
                        highchartOutput('max_mun')
                      )),
                      column(6, wellPanel(h3('Municipios con menor incidencia'),
                                highchartOutput('min_mun')))
                    ),
                    fluidRow(
                      wellPanel(
                        h3('Notas'),
                        htmlOutput('texto_mapa')
                      )
                    )
                    )),
                  
  server = function(input, output, session) {
    
    output$plot <- renderHighchart({
      
      dfplot <- reactive(casos_comp[casos_comp$date >= input$daterange[1] & casos_comp$date <= input$daterange[2],])
      
      observe({
        updateDateRangeInput(session, "datarange",
                             start = input$daterange[1],
                             end = input$daterange[2])
      })
      
      riesgo_in_df <- unique(dfplot()[, 12])
      
      dfcol <- data.frame(riesgo = levels(casos_comp$riesgo.mu), 
                          colors = c('#73ff98', '#96deff', "#eef086","#f7a66f","#f56969"))
      
      colors <- dfcol[dfcol$riesgo %in% riesgo_in_df, 2]
      
      if (input$metrica == 'IA14'){
        
        highchart() %>% 
          
          hc_add_series(dfplot(), 'column', hcaes(x = date, y = ia14.mu, group = riesgo.mu),
                        color = colors) %>% 
          
          hc_add_series(dfplot(),'line', hcaes(x = date, y = ia14.es),
                        color = 'black', name = 'Media IA14 España') %>% 
          hc_title(
            text = "Incidencia acumulada (14 días) y riesgo de contagio en la R. de Murcia",
            margin = 20,
            align = "left",
            style = list(color = "#454545", useHTML = TRUE)) %>% 
          hc_xAxis(type = "datetime", dateTimeLabelFormats = list(day = '%Y-%M-%D')) %>%
          hc_yAxis(title = list(text = "IA14"))
      } 
      
      
      
      else if (input$metrica == 'UCI por millón de hab.'){
        
        highchart() %>%
          hc_xAxis(type = "datetime", dateTimeLabelFormats = list(day = '%Y-%M-%D')) %>%
          hc_add_series(name = 'UCI por millón de habitantes RM', data = dfplot(), type ='column', hcaes(date, y = intensive_care_per_1000000.mu),
                        color = '#ff7a7a') %>%
          hc_add_series(name = 'Media España', data = dfplot(), type = 'line', hcaes(date, intensive_care_per_1000000.es),
                        color = 'black') %>%
          hc_title(
            text = "Ingresados en UCI por millón de habitantes en la Región de Murcia",
            margin = 20,
            align = "left",
            style = list(color = "#454545", useHTML = TRUE)) %>%
          hc_yAxis(title = list(text = "UCI por millón de hab."))
      }
      
      else if(input$metrica == 'Hospitalizados por cada 100.000') {
        highchart() %>%
          hc_xAxis(type = "datetime", dateTimeLabelFormats = list(day = '%Y-%M-%D')) %>%
          hc_add_series(name = 'Hospitalizados por cada cien mil hab. RM', data = dfplot(), type ='column', hcaes(date, y = hospitalized_per_100000.mu),
                        color = '#ff7a7a') %>%
          hc_add_series(name = 'Media España', data = dfplot(), type = 'line', hcaes(date, hospitalized_per_100000.es),
                        color = 'black') %>%
          hc_title(
            text = "Hospitalizados por cada cien mil habitantes en la Región de Murcia",
            margin = 20,
            align = "left",
            style = list(color = "#454545", useHTML = TRUE)) %>%
          hc_yAxis(title = list(text = "Hospitalizados por cada 100.000."))
      }
      else if (input$metrica == 'Fallecidos por cada 100.000') {
        highchart() %>%
          hc_xAxis(type = "datetime", dateTimeLabelFormats = list(day = '%Y-%M-%D')) %>%
          hc_add_series(name = 'Fallecidos por cada cien mil hab. RM', data = dfplot(), type = 'column', hcaes(date, y = deceassed_per_100000.mu),
                        color = '#ff7a7a') %>%
          hc_add_series(name = 'Media España', data = dfplot(), type = 'line', hcaes(date, deceassed_per_100000.es),
                        color = 'black') %>%
          hc_title(
            text = "Fallecidos por cada cien mil habitantes en la Región de Murcia",
            margin = 20,
            align = "left",
            style = list(color = "#454545", useHTML = TRUE)) %>%
          hc_yAxis(title = list(text = "Fallecidos por cada 100.000."))
      }
      
    })
    
    output$plot_vac <- renderHighchart({
      
      dfplot_vac <- reactive(vac_comp[vac_comp$date_vac >= input$daterange_vac[1] & vac_comp$date_vac <= input$daterange_vac[2],])
      
      observe({
        updateDateRangeInput(session, "daterange_vac",
                             start = input$daterange_vac[1],
                             end = input$daterange_vac[2])
      })
      
      if (input$metrica_vac == '% vacunas administradas'){
      
      highchart() %>% 
        hc_xAxis(type = "datetime", dateTimeLabelFormats = list(day = '%Y-%M-%D')) %>%
        hc_add_series(name = 'Región de Murcia', dfplot_vac(), 'column', hcaes(date_vac, y = vac_perc_entregadas.mu),
                      color = '#34eb83') %>% 
        hc_add_series(name = 'Media España', data = dfplot_vac(), type = 'line', hcaes(date_vac, vac_perc_entregadas.es),
                      color = 'black') %>% 
        hc_title(
          text = "Porcentaje de vacunas administradas sobre las entregadas",
          margin = 20,
          align = "left",
          style = list(color = "#454545", useHTML = TRUE)) %>% 
        hc_yAxis(title = list(text = "% vacunas administradas"))
      }
      
      else if (input$metrica_vac == 'Pauta completada'){
        
        highchart() %>% 
          hc_xAxis(type = "datetime", dateTimeLabelFormats = list(day = '%Y-%M-%D')) %>%
          hc_add_series(name = 'Región de Murcia', dfplot_vac(), 'column', hcaes(date_vac, y = vac_dosis_pauta_completada_1000.mu),
                        color = '#34eb83') %>% 
          hc_add_series(name = 'Media España', data = dfplot_vac(), type = 'line', hcaes(date_vac, vac_dosis_pauta_completada_1000.es),
                        color = 'black') %>% 
          hc_title(
            text = "Pautas de vacunación completadas",
            margin = 20,
            align = "left",
            style = list(color = "#454545", useHTML = TRUE)) %>% 
          hc_yAxis(title = list(text = "Nº de pautas completadas"))
      }
      
      else if (input$metrica_vac == 'Vacunas administradas'){
        
        highchart() %>% 
          hc_xAxis(type = "datetime", dateTimeLabelFormats = list(day = '%Y-%M-%D')) %>%
          hc_add_series(name = 'Región de Murcia', dfplot_vac(), 'column', hcaes(date_vac, y = vac_dosis_administradas_1000.mu),
                        color = '#34eb83') %>% 
          hc_add_series(name = 'Media España', data = dfplot_vac(), type = 'line', hcaes(date_vac, vac_dosis_administradas_1000.es),
                        color = 'black') %>% 
          hc_title(
            text = "Vacunas administradas",
            margin = 20,
            align = "left",
            style = list(color = "#454545", useHTML = TRUE)) %>% 
          hc_yAxis(title = list(text = "Nº de vacunas administradas"))
      }
      
      else if (input$metrica_vac == 'Vacunas entregadas'){
        
        highchart() %>% 
          hc_xAxis(type = "datetime", dateTimeLabelFormats = list(day = '%Y-%M-%D')) %>%
          hc_add_series(name = 'Región de Murcia', dfplot_vac(), 'column', hcaes(date_vac, y = vac_dosis_entregadas_1000.mu),
                        color = '#34eb83') %>% 
          hc_add_series(name = 'Media España', data = dfplot_vac(), type = 'line', hcaes(date_vac, vac_dosis_entregadas_1000.es),
                        color = 'black') %>% 
          hc_title(
            text = 'Vacunas entregadas',
            margin = 20,
            align = "left",
            style = list(color = "#454545", useHTML = TRUE)) %>% 
          hc_yAxis(title = list(text = "Nº de vacunas entregadas"))
      }
      
    })
    
    output$mapa_mu <- renderLeaflet({
      
      if (input$metrica_map == 'IA14'){
        
      labels <- sprintf(
       "<strong>%s</strong><br/> IA14: %g",
       mapa_mu@data$NAMEUNIT, mapa_mu@data$IA14) %>% lapply(htmltools::HTML)
      
      leaflet(mapa_mu) %>%
        addTiles(urlTemplate = 'https://{s}.basemaps.cartocdn.com/rastertiles/light_all/{z}/{x}/{y}.png') %>% 
        addPolygons(color = "#9c9c9c", weight = 1,
                    opacity = 0.9, fillOpacity = 0.6,
                    fillColor = ~colorBin("YlOrRd", domain = IA14)(IA14),
                    highlightOptions = highlightOptions(color = "#575757", weight = 2,
                                                        bringToFront = TRUE),
                    label = labels,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto"))}
      else if (input$metrica_map == 'Nivel de alerta') {
        factpal <- colorFactor(heat.colors(length(levels(mapa_mu@data$Nivel_alerta))),
                               mapa_mu@data$Nivel_alerta)
        
        
        labels2 <- sprintf(
          "<strong>%s</strong><br/>Nivel de alerta: %s",
          mapa_mu@data$NAMEUNIT, mapa_mu@data$Nivel_alerta) %>% lapply(htmltools::HTML)
        
        leaflet(mapa_mu) %>%
          addTiles(urlTemplate = 'https://{s}.basemaps.cartocdn.com/rastertiles/light_all/{z}/{x}/{y}.png') %>% 
          addPolygons(color = "#9c9c9c", weight = 1,
                      opacity = 0.9, fillOpacity = 0.6,
                      fillColor = ~factpal(Nivel_alerta),
                      highlightOptions = highlightOptions(color = "#575757", weight = 2,
                                                          bringToFront = TRUE),
                      label = labels2,
                      labelOptions = labelOptions(
                        style = list("font-weight" = "normal", padding = "3px 8px"),
                        textsize = "15px",
                        direction = "auto"))}
      
    })
    
    output$last_updt <- renderText({
      paste0('Última actualización: ', last_update)
    })
    
    output$max_mun <- renderHighchart({
      
      top5_IA14_municipios <- mapa_mu@data %>% arrange(desc(IA14)) %>% head(5)
      
      highchart() %>% 
        hc_chart(type ="bar") %>%
        hc_xAxis(categories = top5_IA14_municipios$NAMEUNIT) %>% 
        hc_add_series(data = top5_IA14_municipios$IA14, name = "IA14")
    })
    
    
    
    output$min_mun <- renderHighchart({
      
      bot5_IA14_municipios <- mapa_mu@data %>% arrange(IA14) %>% head(5)
      
      highchart() %>% 
        hc_chart(type ="bar") %>%
        hc_xAxis(categories = bot5_IA14_municipios$NAMEUNIT) %>% 
        hc_add_series(data = bot5_IA14_municipios$IA14, name = "IA14")
    })
    
    output$texto_casos <- renderUI({
      HTML('
            <p>Para este cuadro de mandos, se utilizan datos del repositorio de <a href="https://github.com/montera34/escovid19data">escovid19data</a>.&nbsp;</p>
            <p>Puesto que en muchas comunidades no se contabilizan casos durante el fin de semana, las m&eacute;tricas est&aacute;n calculadas con una<strong>&nbsp;media m&oacute;vil</strong> simple a 5 d&iacute;as, con el fin de suavizar las series.</p>
            <p>Descripci&oacute;n de las variables:</p>
            <ul>
                <li><strong>IA14</strong>: Incidencia acumulada de contagios por COVID en los &uacute;ltimos 14 d&iacute;as.</li>
                <li><strong>UCI por mill&oacute;n de hab.</strong>: N&uacute;mero de ingresados en unidades de cuidado intensivo.</li>
                <li><strong>Hospitalizados por cada 100.000</strong>: N&uacute;mero de hospitalizados por cada cien mil habitantes.</li>
                <li><strong>Fallecidos por cada 100.000</strong>: N&uacute;mero de fallecidos por cada cien mil habitantes.</li>
            </ul>
           '
           )
    })
    
    output$texto_vac <- renderUI({
      HTML('
            <p>Para este cuadro de mandos, se utilizan datos del repositorio de <a href="https://github.com/montera34/escovid19data">escovid19data</a>.&nbsp;</p>
            <p>A diferencia del cuatro de mandos de casos, en este no se utilizan datos de fin de semana suavizados, sino que prescindimos de ellos, ya que la mayor&iacute;a de m&eacute;tricas son acumulativas.</p>
            <p>En el repositorio mencionado previamente, no se dispone de datos sobre vacunaci&oacute;n de los d&iacute;as 5 y 6 de febrero de 2021.</p>
            <p>Descripci&oacute;n de las variables:</p>
            <ul>
                <li><strong>% vacunas administradas</strong>: Porcentaje de vacunas administradas frente a entregadas.</li>
                <li><strong>Pauta completada</strong>: N&uacute;mero de pautas completadas o dobles vacunaciones por cada mil habitantes.</li>
                <li><strong>Vacunas administradas</strong>: N&uacute;mero de vacunas administradas por cada mil habitantes.</li>
                <li><strong>Vacunas entregadas</strong>: N&uacute;mero de vacunas entregadas por cada mil habitantes.</li>
            </ul>
           '
      )
    }) 
    
    output$texto_mapa <- renderUI({
      HTML('
            <p>Para este cuadro de mandos, se utilizan datos de la <a href="https://www.murciasalud.es/pagina.php?id=458869">Consejer&iacute;a de Salud de la Regi&oacute;n de Murcia</a>.&nbsp;</p>
            <p>Los niveles de riesgo de contagio pueden variar de los del resto de cuadro de mandos, ya que los datos utilizados en este son extra&iacute;dos del <a href="http://www.murciasalud.es/pagina.php?id=472956&idsec=6574">portal sanitario de la Regi&oacute;n de Murcia</a>.</p>
           '
      )
    })
    
  }
)



