#

# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Estadística Computacional"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      radioButtons("tarea", label="Escoge tarea",
                   choices = c(
                     "Aceptación Rechazo"="aceptacionRechazo",
                     "Funcion Inversa"="funInv",
                     "Integracion por Monte Carlo"="IMC",
                     "Metropolis-Hastings" = "MH"
                   ),
                   selected="aceptacionRechazo"
      )
    ),
    # Show a plot of the generated distribution
    mainPanel(
      conditionalPanel(
        condition="input.tarea=='aceptacionRechazo'",
        h2("Aceptación-Rechazo"),
        textInput(
          inputId="expresion1", 
          label="Funcion f",
          value="function(x) 2*x"
        ),
        selectInput(
          inputId="expresion2", 
          label="Funcion g",
          choices=c("Uniforme(xmin, xmax)"="unif", "Exponencial(1) truncada a (xmin,xmax)"="exp", "Normal(0,1) truncada a (xmin,xmax)"="norm")
        ),
        sliderInput("xmin", "xmin", min=-30, max=30, value=0),
        sliderInput("xmax", "xmax", min=-30, max=20, value=1),
        sliderInput("M", "M", min=0.1, max=100, value=1),
        numericInput("nsim", "Número de simulaciones", value=100),
        actionButton("button1", "Correr"),
        plotOutput("Grafica"),
        h3("Resultados"),
        p("Tasa de exito", textOutput("tasa_exito")),
        plotOutput("hist_sim"),
        sliderInput("nbins", "nbins", value=20, min=10, max=100)
      ),
      
      
      #------------------------------------------------------------------------
      #                  FUNCION INVERSA
      #------------------------------------------------------------------------
      
      
      conditionalPanel(
        condition="input.tarea=='funInv'",
        h2("Funcion inversa"), 
        sliderInput("lambda","Valor de lambda",  min=0.01, max=0.99, value=0.5, step=0.001),
        sliderInput("nsim","Numero de simulaciones",  min=10, max=1000, value=500, step=1),
        plotOutput("simulated")
        ),
      
      #------------------------------------------------------------------------
      #                  Integracion por Monte Carlo 
      #------------------------------------------------------------------------
      
      conditionalPanel(
        condition = "input.tarea=='IMC'", 
        h2("Integracion por Monte Carlo"), 
        textInput(
          inputId="funci", 
          label="Funcion que desea integrar",
          value="function(x) 2*sqrt(4-x^2)"
        ),
        sliderInput("alpha","Valor de alpha (Nivel de confianza)",  min=0.001, max=0.5, value=0.05),
        sliderInput("nsim2","Numero de simulaciones",  min=10, max=10000, value=500),
        
        numericInput("b","Limite superior",  min=-100, max=100, value=2),
        numericInput("a","Limite inferior",  min=-100, max=100, value=0),
        
        numericInput("nmin", "numero minimo", min=2, max=10000, value=1000),
        numericInput("nmax", "numero maximo", min=2, max=10000, value=10000),
        
       plotOutput("Grafica22"),
       textOutput("Resultado")

      ),
      
      #------------------------------------------------------------------------
      #                  Metropolis-Hastings
      #------------------------------------------------------------------------

      conditionalPanel(
        condition="input.tarea=='MH'",
        h2("Metropolis-Hastings"),
        fluidRow(

          selectInput("y","Variable Independiente", c(names(p14))),
          selectInput("x","Variable Dependiente", c(names(p14))),
          sliderInput("ns","Numero de simulaciones",  min=10, max=10000, value=500),
          sliderInput("ns","Numero de cadenas que se quieren simular",  min=10, max=10000, value=500),
          sliderInput("ns","Longitud de las cadenas",  min=10, max=10000, value=500)
        ),
        actionButton("button1", "Correr"),
        #Create a new row for the table.
        fluidRow(
          DT::dataTableOutput("table")
          
        ), 
    
        plotOutput("g1"),

        plotOutput("greg"),
        plotOutput("bayes3"),
        tableOutput("bayes"),
        tableOutput("bayes2")
      )
      
    )
    
  )
))
