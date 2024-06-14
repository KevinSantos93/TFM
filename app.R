# app.R
library(dplyr)
library(shiny)
library(readxl)
library(shinyjs)
library(ggplot2)
library(DT)
library(gghighlight)
theme_set(theme_bw())

# Cargar los datos desde el archivo de Excel
datos <- read_excel("resultados.xlsx", sheet = "notas")
items <- read_excel("resultados.xlsx", sheet = "descripcion")
estudiantes <- read_excel("resultados.xlsx", sheet = "estudiantes")

# Crear la aplicación Shiny
ui <- fluidPage(
  #Estilos css
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
  
  titlePanel("Visualizador de Notas y Descripciones"),
  
  # Menú desplegable para seleccionar código
  selectInput("codigo", "Selecciona un código:", choices = unique(datos$CÓDIGO.DE.INFRAESTRUCTURA)),
  
  # Histograma de notas
  fluidRow(
    plotOutput("histograma"),
    plotOutput("estudiantes", height = "200px")
  ),
  
  
  # Tabla con ítems y descripciones
  DT::dataTableOutput('tabla_items')
)

server <- function(input, output) {
  # Histograma
  output$histograma <- renderPlot({
    
    filtro <- datos %>%
      filter(CÓDIGO.DE.INFRAESTRUCTURA == input$codigo)
    
    datos$color <- if_else(datos$nota > (floor(filtro$nota * 10) / 10) & datos$nota <= ceiling(filtro$nota * 10) / 10, T, F)
    datos %>%
      ggplot(aes(x = nota, fill = color)) +
      geom_histogram(color = "black", breaks = seq(0, 10, by = 0.1)) +
      labs(title = "Resultados nacionales", x = "Nota", y = "Frecuencia") +
      scale_x_continuous(breaks = seq(0, 10, 0.5), lim = c(2, 6.5)) + 
      theme(legend.position = "none",
            text = element_text(size = 20),  # Cambia el tamaño de la letra en general
            axis.title = element_text(size = 18),  # Tamaño de las etiquetas de los ejes
            axis.text = element_text(size = 18),  # Tamaño de los números en los ejes
            plot.title = element_text(size = 20)) +  # Tamaño del título del gráfico)
      scale_fill_manual(values=c('#5BC0EB', 'gray'))
  })
  
  output$estudiantes <- renderPlot({
    filtro <- estudiantes %>%
      filter(codigo == input$codigo)
    
    filtro$desempeño <-ifelse(filtro$nota < 5, "Bajo", ifelse(filtro$nota < 7.5, "Medio", "Alto"))
    filtro$desempeño <- factor(filtro$desempeño, levels = c("Alto", "Medio", "Bajo"))
    
    # Crear el bar plot
    ggplot(filtro, aes(x = 1, fill = desempeño, order = desempeño)) + 
      geom_bar(position = "fill") +
      labs(x = "",
           y = "Porcentaje",
           title = "Distribución de Desempeño de los estudiantes",
           fill = "Desempeño") +
      coord_flip() +
      scale_x_discrete(labels = NULL) +
      theme(text = element_text(size = 20),  # Cambia el tamaño de la letra en general
            axis.title = element_text(size = 18),  # Tamaño de las etiquetas de los ejes
            axis.text = element_text(size = 18),  # Tamaño de los números en los ejes
            plot.title = element_text(size = 20)) +  # Tamaño del título del gráfico) +
      scale_fill_manual(values=c('#9BC53D', '#5BC0EB', '#FFBF78'))
  })

  
  # Tabla con ítems y descripciones
  output$tabla_items <- DT::renderDataTable({
    filtro <- datos %>%
      filter(CÓDIGO.DE.INFRAESTRUCTURA == input$codigo)
    filtro <- filtro[,2:21]
    
    itemsConAciertos <- cbind(items, t(filtro[]))
    colnames(itemsConAciertos) <- c("Item","Dominio", "Contenido", "Descripción", "% de acierto")
    itemsConAciertos
    
    DT::datatable(itemsConAciertos[,2:5]) %>%
      formatStyle(
        '% de acierto',
        target = 'row',
        backgroundColor = styleInterval(0.35, c('#FFBF78','white'))
      )
  })
}
 
shinyApp(ui, server)
