#===============================================================================
#  GRAFICO INTERACTIVO DISEÑADO CON SHINY
#===============================================================================

# Se importan las bibliotecas
library(shiny)
library(tidyverse)
library(DBI)
library(VIM)
library(fastDummies)
library(missForest)
require(rCharts)
library(RColorBrewer)
library(ggplot2)

# Conexión a MySQL
bd <- DBI::dbConnect(RMySQL::MySQL(),
                      dbname = "",
                      host = "database-1.chtu56j8cqsz.us-east-1.rds.amazonaws.com",
                      user = "admin",
                      password = "Peru123.",
                      Port     = 3306)

# Se asigna la base de datos de trabajo
dbGetQuery(bd, 'USE TAREA_UPC')

# Se obtienen los datos desde la base de datos
df <- dbGetQuery(bd,
  'SELECT * FROM train
  UNION 
  (SELECT A.PassengerId, B.Survived, A.Pclass, A.Name, A.Sex, A.Age, A.SibSp, A.Parch, A.Ticket, A.Fare, A.Cabin, A.Embarked 
  FROM test A
  LEFT JOIN gender_submission B on A.PassengerId=B.PassengerId)')

# Se desconecta de la base de datos
dbDisconnect(bd)

# Cantidad de Datos
ndatos <- nrow(df)
ndatos

# Convirtiendo Información
df$PassengerId<-as.integer(df$PassengerId)
df$Survived<-as.factor(df$Survived)
df$Pclass<-as.integer(df$Pclass)
df$Age<-as.integer(df$Age)
df$SibSp<-as.integer(df$SibSp)
df$Parch<-as.integer(df$Parch)
df$Fare<-as.integer(df$Fare)
#df$Sex<-as.factor(df$Sex)

# Mostrar en qué columnas tienen valores perdidos
cidx_perd <- which(colSums(is.na(df))!=0)
cidx_perd

# Cantidad de valores perdidos en las columnas
nperdidos <- colSums(is.na(df[,cidx_perd]))
nperdidos

# Porcentaje de valores perdidos en las columnas
pperdidos <- 100*nperdidos/ndatos
pperdidos

# La variable Cabin, tiene el 77% de datos nulos, se decide eliminar
df$Cabin <- NULL

# Para fines de análisis se eliminan los campos PassengerId y Name
df$PassengerId<-NULL
df$Name<-NULL

# Tratamiento de etiquetas
df <- na.omit(df)
df["Sex"][df["Sex"] == "male"] <- "Male"
df["Sex"][df["Sex"] == "female"] <- "Female"
df <- df %>% mutate(Status = case_when(Survived == 1 ~ "Survived", Survived == 0 ~ "Non Survived"))
df["Embarked"][df["Embarked"] == "C"] <- "Cherbourg"
df["Embarked"][df["Embarked"] == "Q"] <- "Queenstown"
df["Embarked"][df["Embarked"] == "S"] <- "Southampton"
df["Pclass"][df["Pclass"] == 1] <- "1. Upper"
df["Pclass"][df["Pclass"] == 2] <- "2. Middle"
df["Pclass"][df["Pclass"] == 3] <- "3. Lower"

str(df)
head(df)

# Interfaz de usuario
ui <- fluidPage(
  #tags$style(type='text/css', '.sidebar { height: 90vh; overflow-y: auto; }'),
  
  #Titulo
  headerPanel("Interacción con datos de pasajeros del Titanic"),
  
  sidebarLayout(fluid=TRUE,
    #Columna lateral para filtros
    sidebarPanel(
      style = "overflow-y:scroll; max-height: 550px; position:fixed; width:inherit;",
      
      sliderInput("input_bins",
                  "Número de intervalos de edad:",
                  min = 1,
                  max = 30,
                  value = 15),
      
      checkboxInput("cajaverif", label = "Añadir densidad", value = TRUE),
      
      sliderInput("input_edad",
                  "Seleccione el rango de edad:",
                  min = min(df$Age),
                  max = max(df$Age),
                  value = c(min(df$Age), max(df$Age))),
      
      selectInput(inputId = "input_sex",
                  label = "Seleccione el sexo:",
                  choices = c("All", unique(as.character(df$Sex))),
                  selected = "All"),
      
      selectInput(inputId = "input_status",
                  label = "Seleccione el estado del pasajero:",
                  choices = c("All", unique(as.character(df$Status))),
                  selected = "All"),
      
      selectInput(inputId = "input_embarked", 
                  label = "Seleccione el puerto de embarcación:", 
                  choices = unique(as.character(df$Embarked)),
                  multiple = TRUE, selected = NULL),
      
      selectInput(inputId = "input_pclass",
                  label = "Seleccione la clase de ticket:",
                  choices = c("All", sort(unique(as.character(df$Pclass)))),
                  selected = "All"),
      
      ),
    
    #Columna central de gráficos
    mainPanel(
      fluidRow(
        column(12, plotOutput("frecuenciaEdad"))
      ),
      fluidRow(
        column(6, plotOutput("frecuenciaSexo")),
        column(6, plotOutput("frecuenciaClaseTicket"))
      ),
      fluidRow(
        column(12, plotOutput("dispersionSexoTarifa"))
      ),
      fluidRow(
        column(12, plotOutput("frecuenciaEdadTicketEstado"))
      ),
      fluidRow(
        column(12, plotOutput("frecuenciaEdadSexoEstado"))
      )
    )
  )
  
)

# Lógica del servidor
server <- function(input, output) {
  
  #Seccion de filtros
  df_filtro_sex <- reactive({
    if(input$input_sex == "All") {
      df
    } else {
      df %>% filter(Sex == input$input_sex)
    }
  })
  
  df_filtro_edad <- reactive({
    df_filtro_sex() %>% filter(Age %in% (min(input$input_edad):max(input$input_edad)))
  })
  
  df_filtro_status <- reactive({
    if(input$input_status == "All") {
      df_filtro_edad()
    } else {
      df_filtro_edad() %>% filter(Status == input$input_status)
    }
  })
  
  df_filtro_embarked <- reactive({
    if(is.null(input$input_embarked)) {
      df_filtro_status()
    } else {
      subset(df_filtro_status(), Embarked %in% input$input_embarked)
    }
  })
  
  df_filtro_pclass <- reactive({
    if(input$input_pclass == "All") {
      df_filtro_embarked()
    } else {
      df_filtro_embarked() %>% filter(Pclass == input$input_pclass)
    }
  })
  
  #Seccion de graficos
  output$frecuenciaEdad = renderPlot({
    x <- df_filtro_pclass()$Age
    bins <- seq(min(x), max(x), length.out = input$input_bins + 1)
    h <- hist(x, breaks = bins, col = "deepskyblue", border = "white", 
         main = "Gráfico N° 1. Frecuencia de pasajeros por edad",
         xlab = "Edad", ylab = "Frecuencia de pasajeros")
    
    #Agregar densidad
    xfit <- seq(min(x), max(x), length=80)
    yfit <- dnorm(xfit, mean=mean(x), sd=sd(x))
    yfit <- yfit*diff(h$mids[1:2])*length(x)
    
    if (input$cajaverif==T){lines(xfit, yfit, col="red", lwd=2)}
  })
  
  output$frecuenciaSexo = renderPlot({
    x <- table(df_filtro_pclass()$Sex)
    barplot(x, main = "Gráfico N° 2a. Frecuencia de pasajeros por sexo",
         xlab = "Sexo", ylab = "Frecuencia de pasajeros", 
         col = c("lightpink", "cornflowerblue"))
  })
  
  output$frecuenciaClaseTicket = renderPlot({
    x <- table(df_filtro_pclass()$Pclass)
    barplot(x, main = "Gráfico N° 2b. Frecuencia de pasajeros por \nclase de ticket",
            xlab = "Clase de Ticket", ylab = "Frecuencia de pasajeros", 
            col = brewer.pal(5, "Set2"))
  })
  
#  output$dispersionFamilia = renderPlot({
#    x <- df_filtro_pclass()
#    plot(x = x$SibSp, y = x$Parch, 
#         main = "Distribución de familiares de pasajeros",
#         xlab = "# de hermanos y parejas", ylab = "# de padres e hijos",
#         col = c("orangered2", "lawngreen"))
#    legend(x = "topleft", legend = c("Non Survived", "Survived"),
#           fill = c("orangered2", "lawngreen"), title = "Estado")
#    
#  })
  
  output$frecuenciaEdadTicketEstado = renderPlot({
    x <- df_filtro_pclass()
    ggplot(x, aes(Age, fill = factor(Status)), colour = Status) +
      geom_histogram() +
      facet_grid(.~Pclass) +
      theme_bw() +
      labs(title = "Gráfico N° 4. Frecuencia de pasajeros por clase de ticket, \nedad y estado",
           fill = "Estado", x = "Edad", y = "Frecuencia de pasajeros") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  })
  
  output$frecuenciaEdadSexoEstado = renderPlot({
    x <- df_filtro_pclass()
    ggplot(x, aes(Age, fill = factor(Status)), colour = Status) +
      geom_histogram() +
      facet_grid(.~Sex) +
      theme_bw() +
      labs(title = "Gráfico N° 5. Frecuencia de pasajeros por clase de ticket, \nsexo y estado",
           fill = "Estado", x = "Edad", y = "Frecuencia de pasajeros") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  })
  
  output$dispersionSexoTarifa = renderPlot({
    plotdf <- df_filtro_pclass() #filter(df, Sex %in% df_filtro_sex()$Sex, Status %in% df_filtro_status()$Status)
    ggplot(dat = plotdf, aes(x=Age, y=Fare, color=Sex, shape=Status)) + 
      geom_point(size=2, alpha=0.5) + scale_y_continuous(label = scales::dollar) + 
      ggtitle("Gráfico N° 3. Distribución de pasajeros por edad, \nsexo, tarifa y estado.") + 
      theme_bw() + theme (plot.title = element_text(hjust = 0.5, face="bold")) + 
      labs(x="Edad", y="Tarifa")

  })
}

shinyApp(ui, server)