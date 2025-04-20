#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
## app.R ##
library(shiny)
library(shinydashboard)
library(shinylive)
library(dplyr)
library(tidyr)
library(DT)
library(readr)
library(leaflet)
library(stringr)

#clues <- read_rds('../inteligencia/unidades/clues.rds') %>%
clues <- read_rds('clues.rds') %>%
  filter(ENTIDAD == 'SONORA',
         `ESTATUS DE OPERACION` == 'EN OPERACION',
         `NOMBRE DE LA INSTITUCION` %in% c(
         'INSTITUTO DE SEGURIDAD Y SERVICIOS SOCIALES PARA LOS TRABAJADORES DEL ESTADO',
         'INSTITUTO MEXICANO DEL SEGURO SOCIAL',
         'SECRETARIA DE LA DEFENSA NACIONAL',
         'SECRETARIA DE MARINA',
         'SECRETARIA DE SALUD',
         'SERVICIOS DE SALUD IMSS BIENESTAR',
         'SERVICIOS MEDICOS ESTATALES',
         'SERVICIOS MEDICOS MUNICIPALES')
  ) %>%
  mutate(`NOMBRE DE LA INSTITUCION` = case_when(
    `NOMBRE DE LA INSTITUCION` == 'INSTITUTO DE SEGURIDAD Y SERVICIOS SOCIALES PARA LOS TRABAJADORES DEL ESTADO' ~ 'ISSSTE',
    `NOMBRE DE LA INSTITUCION` == 'INSTITUTO MEXICANO DEL SEGURO SOCIAL' ~ 'IMSS',
    `NOMBRE DE LA INSTITUCION` == 'SECRETARIA DE LA DEFENSA NACIONAL' ~ 'DEFENSA',
    `NOMBRE DE LA INSTITUCION` == 'SECRETARIA DE MARINA' ~ 'MARINA',
    `NOMBRE DE LA INSTITUCION` == 'SECRETARIA DE SALUD' ~ 'SALUD',
    `NOMBRE DE LA INSTITUCION` == 'SERVICIOS DE SALUD IMSS BIENESTAR' ~ 'IMSS-BIENESTAR',
    `NOMBRE DE LA INSTITUCION` == 'SERVICIOS MEDICOS ESTATALES' ~ 'ISSSTESON',
    T ~ `NOMBRE DE LA INSTITUCION`
  ), `CLAVE DE LA JURISDICCION` = case_when(
    MUNICIPIO == 'PITIQUITO' ~ '02',
    T ~ `CLAVE DE LA JURISDICCION`
  ))
opciones <- clues %>%
  count(`CLAVE DE LA JURISDICCION`, MUNICIPIO) %>%
  select(-n)

lista <- list(
  'Todos' = unique(opciones$MUNICIPIO),
  '01' = unique(opciones$MUNICIPIO[opciones$`CLAVE DE LA JURISDICCION` == '01']),
  '02' = unique(opciones$MUNICIPIO[opciones$`CLAVE DE LA JURISDICCION` == '02']),
  '03' = unique(opciones$MUNICIPIO[opciones$`CLAVE DE LA JURISDICCION` == '03']),
  '04' = unique(opciones$MUNICIPIO[opciones$`CLAVE DE LA JURISDICCION` == '04']),
  '05' = unique(opciones$MUNICIPIO[opciones$`CLAVE DE LA JURISDICCION` == '05']),
  '06' = unique(opciones$MUNICIPIO[opciones$`CLAVE DE LA JURISDICCION` == '06'])
)

#clues %>%
#  count(`NOMBRE DE TIPOLOGIA`) %>%
#  View()


## UI ##
ui <- dashboardPage(skin = 'red',
  dashboardHeader(title = 'DGSSP'),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Mapa", tabName = "mapa", icon = icon("map"))
    ),
    selectInput(inputId = 'DSB',
                label = 'Seleccione el Distrito de Salud para el Bienestar',
                choices = c(sort(unique(clues$`CLAVE DE LA JURISDICCION`)), 'Todos'),
                selected = 'Todos'),
    selectInput(inputId = 'INSTITUCION',
                label = 'Seleccione la institución de salud',
                choices = c(unique(clues$`NOMBRE DE LA INSTITUCION`), 'Todos'),
                selected = 'Todos'),
    selectInput(inputId = 'MUNICIPIO',
                label = 'Seleccione el municipio',
                choices = c(unique(opciones$MUNICIPIO), 'Todos'),
                selected = 'Todos')
  ),
  dashboardBody(
    tags$head(tags$style(HTML('
      .main-header .logo {
        font-family: "Georgia", Times, "Times New Roman", serif;
        font-weight: bold;
        font-size: 24px;
      }
    '))),
    tabItems(
      tabItem(tabName = 'dashboard',
        fluidRow(
          #box(title = 'Unidades por nivel de atención por municipio',
              DTOutput('data1')#)
          ),
        fluidRow(
          #box(title = 'Tipo de unidades por municipio',
              DTOutput('data2')#),
        )
      ),
      tabItem(tabName = 'mapa',
              fluidPage(
                leafletOutput('mapa', height = 600)
              )
      )
    )
  )
)

server <- function(input, output, session) {
  ## Base de datos ##
  clues_cl <- reactive({
    if (input$DSB == 'Todos' & input$INSTITUCION == 'Todos' & input$MUNICIPIO == 'Todos') {
      clues <- clues
    } else if (input$DSB != 'Todos' & input$INSTITUCION == 'Todos' & input$MUNICIPIO == 'Todos') {
      clues <- clues %>%
        filter(`CLAVE DE LA JURISDICCION` == input$DSB)
    } else if (input$DSB != 'Todos' & input$INSTITUCION != 'Todos' & input$MUNICIPIO == 'Todos') {
      clues <- clues %>%
        filter(`CLAVE DE LA JURISDICCION` == input$DSB,
               `NOMBRE DE LA INSTITUCION` == input$INSTITUCION)
    } else if (input$DSB != 'Todos' & input$INSTITUCION != 'Todos' & input$MUNICIPIO != 'Todos') {
      clues <- clues %>%
        filter(`CLAVE DE LA JURISDICCION` == input$DSB,
               `NOMBRE DE LA INSTITUCION` == input$INSTITUCION,
               MUNICIPIO == input$MUNICIPIO)
    } else if (input$DSB != 'Todos' & input$INSTITUCION == 'Todos' & input$MUNICIPIO != 'Todos') {
      clues <- clues %>%
        filter(`CLAVE DE LA JURISDICCION` == input$DSB,
               #`NOMBRE DE LA INSTITUCION` == input$INSTITUCION,
               MUNICIPIO == input$MUNICIPIO)
    } else if (input$DSB == 'Todos' & input$INSTITUCION != 'Todos' & input$MUNICIPIO == 'Todos') {
      clues <- clues %>%
        filter(#`CLAVE DE LA JURISDICCION` == input$DSB,
               `NOMBRE DE LA INSTITUCION` == input$INSTITUCION #,
               #MUNICIPIO == input$MUNICIPIO
               )
    } else if (input$DSB == 'Todos' & input$INSTITUCION != 'Todos' & input$MUNICIPIO != 'Todos') {
      clues <- clues %>%
        filter(#`CLAVE DE LA JURISDICCION` == input$DSB,
          `NOMBRE DE LA INSTITUCION` == input$INSTITUCION,
          MUNICIPIO == input$MUNICIPIO)
    } else if (input$DSB == 'Todos' & input$INSTITUCION == 'Todos' & input$MUNICIPIO != 'Todos') {
      clues <- clues %>%
        filter(#`CLAVE DE LA JURISDICCION` == input$DSB,
          #`NOMBRE DE LA INSTITUCION` == input$INSTITUCION,
          MUNICIPIO == input$MUNICIPIO)
    }
  })
  ## Tabla No.1 ##
  output$data1 <- renderDT({
    clues <- clues_cl() %>%
      count(MUNICIPIO, `NIVEL ATENCION`) %>%
      pivot_wider(names_from = `NIVEL ATENCION`, values_from = n, values_fill = 0) %>%
      full_join(tibble(MUNICIPIO = NA, `PRIMER NIVEL` = NA, `SEGUNDO NIVEL` = NA, `TERCER NIVEL` = NA)) %>%
      filter(!is.na(MUNICIPIO)) %>%
      select(MUNICIPIO, `PRIMER NIVEL`, `SEGUNDO NIVEL`, `TERCER NIVEL`) %>%
      replace(is.na(.), 0) %>%
      datatable(extensions = 'Buttons', caption = 'Unidades por nivel de atención por municipio',
                options = list(dom = 'Blfrtip',
                               buttons = c('excel')#,
                               #lengthMenu = list(c(-1),
                              #                   c("All"))
                               ))
  })
  ## Tabla No.2 ##
  output$data2 <- renderDT({
    clues <- clues_cl() %>%
      count(MUNICIPIO, `NOMBRE DE TIPOLOGIA`) %>%
      pivot_wider(names_from = `NOMBRE DE TIPOLOGIA`, values_from = n, values_fill = 0) %>%
      datatable(extensions = 'Buttons', caption = 'Tipo de unidades por municipio',
                options = list(dom = 'Blfrtip',
                               scrollX = TRUE,
                               #columnDefs = list(list(visible = FALSE, targets = 7:(ncol(.)-1))),
                               buttons = c('excel')#,
                               #lengthMenu = list(c(-1),
                              #                   c("All"))
                               ))
  })
  ## Mapa ##
  output$mapa <- renderLeaflet({
    leaflet() %>%
      addTiles(group = 'Mapa') %>%
      addMarkers(group = 'Hospitales',
                 data = clues_cl() %>%
                   filter(str_detect(`NOMBRE DE TIPOLOGIA`, pattern = 'HOSPITAL')),
                 lat = ~as.numeric(LATITUD),
                 lng = ~as.numeric(LONGITUD),
                 popup = ~paste('Nombre de la unidad: <br>', `NOMBRE DE LA UNIDAD`, '<br>',
                                'Institución: <br>', `NOMBRE DE LA INSTITUCION`,'<br>',
                                'Tipo de unidad: <br>', `NOMBRE DE TIPOLOGIA`,'<br>',
                                'Dirección de la unidad: <br>', VIALIDAD, ' ', `NUMERO EXTERIOR`, ', ', ASENTAMIENTO, '<br>',
                                LOCALIDAD, ', ', MUNICIPIO, sep = '')) %>%
      addMarkers(group = 'Centros de Salud, UMF, CMF',
                 data = clues_cl() %>%
                   filter(str_detect(`NOMBRE DE TIPOLOGIA`, pattern = 'CENTRO DE SALUD|CLÍNICA |UNIDAD DE MEDICINA|NÚCLEO|CENTROS AVANZADOS|CONSULTORIO')),
                 lat = ~as.numeric(LATITUD),
                 lng = ~as.numeric(LONGITUD),
                 popup = ~paste('Nombre de la unidad: <br>', `NOMBRE DE LA UNIDAD`, '<br>',
                                'Institución: <br>', `NOMBRE DE LA INSTITUCION`,'<br>',
                                'Tipo de unidad: <br>', `NOMBRE DE TIPOLOGIA`,'<br>',
                                'Dirección de la unidad: <br>', VIALIDAD, ' ', `NUMERO EXTERIOR`, ', ', ASENTAMIENTO, '<br>',
                                LOCALIDAD, ', ', MUNICIPIO, sep = '')) %>%
      addMarkers(group = 'Casas de Salud',
                 data = clues_cl() %>%
                   filter(str_detect(`NOMBRE DE TIPOLOGIA`, pattern = 'CASA DE SALUD')),
                 lat = ~as.numeric(LATITUD),
                 lng = ~as.numeric(LONGITUD),
                 popup = ~paste('Nombre de la unidad: <br>', `NOMBRE DE LA UNIDAD`, '<br>',
                                'Institución: <br>', `NOMBRE DE LA INSTITUCION`,'<br>',
                                'Tipo de unidad: <br>', `NOMBRE DE TIPOLOGIA`,'<br>',
                                'Dirección de la unidad: <br>', VIALIDAD, ' ', `NUMERO EXTERIOR`, ', ', ASENTAMIENTO, '<br>',
                                LOCALIDAD, ', ', MUNICIPIO, sep = '')) %>%
      addMarkers(group = 'Oficina distrital',
                 data = clues_cl() %>%
                   filter(str_detect(`NOMBRE DE SUBTIPOLOGIA`, pattern = 'JURISDICCIONALES')),
                 lat = ~as.numeric(LATITUD),
                 lng = ~as.numeric(LONGITUD),
                 popup = ~paste('Nombre de la unidad: <br>', `NOMBRE DE LA UNIDAD`, '<br>',
                                'Institución: <br>', `NOMBRE DE LA INSTITUCION`,'<br>',
                                'Tipo de unidad: <br>', `NOMBRE DE TIPOLOGIA`,'<br>',
                                'Dirección de la unidad: <br>', VIALIDAD, ' ', `NUMERO EXTERIOR`, ', ', ASENTAMIENTO, '<br>',
                                LOCALIDAD, ', ', MUNICIPIO, sep = '')) %>%
      addMarkers(group = 'Almacenes',
                 data = clues_cl() %>%
                   filter(str_detect(`NOMBRE DE TIPOLOGIA`, pattern = 'ALMACEN')),
                 lat = ~as.numeric(LATITUD),
                 lng = ~as.numeric(LONGITUD),
                 popup = ~paste('Nombre de la unidad: <br>', `NOMBRE DE LA UNIDAD`, '<br>',
                                'Institución: <br>', `NOMBRE DE LA INSTITUCION`,'<br>',
                                'Tipo de unidad: <br>', `NOMBRE DE TIPOLOGIA`,'<br>',
                                'Dirección de la unidad: <br>', VIALIDAD, ' ', `NUMERO EXTERIOR`, ', ', ASENTAMIENTO, '<br>',
                                LOCALIDAD, ', ', MUNICIPIO, sep = '')) %>%
      addMarkers(group = 'Otros',
                 data = clues_cl() %>%
                   filter(str_detect(`NOMBRE DE TIPOLOGIA`, pattern = 'OTROS|LABORATORIO|MÓVIL|NO ESPECIFICADO')),
                 lat = ~as.numeric(LATITUD),
                 lng = ~as.numeric(LONGITUD),
                 popup = ~paste('Nombre de la unidad: <br>', `NOMBRE DE LA UNIDAD`, '<br>',
                                'Institución: <br>', `NOMBRE DE LA INSTITUCION`,'<br>',
                                'Tipo de unidad: <br>', `NOMBRE DE TIPOLOGIA`,'<br>',
                                'Dirección de la unidad: <br>', VIALIDAD, ' ', `NUMERO EXTERIOR`, ', ', ASENTAMIENTO, '<br>',
                                LOCALIDAD, ', ', MUNICIPIO, sep = '')) %>%
      addLayersControl(baseGroups = 'Mapa',
                       overlayGroups = c('Hospitales',
                                         'Centros de Salud, UMF, CMF',
                                         'Casas de Salud',
                                         'Oficina distrital',
                                         'Almacenes',
                                         'Otros'),
                       options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup(group = c('Hospitales',
                          'Centros de Salud, UMF, CMF',
                          'Casas de Salud',
                          'Oficina distrital',
                          'Almacenes',
                          'Otros'))

      })

  observeEvent(input$DSB, {
    municipios <- lista

    updateSelectInput(session = session,
                      inputId = 'MUNICIPIO',
                      label = 'Seleccione el municipio',
                      choices = c(lista[[input$DSB]], 'Todos'),
                      selected = 'Todos')

  })
}

shinyApp(ui, server)

shinylive::export(appdir = "../inteligencia/unidades", destdir = "../inteligencia/unidades/docs")
