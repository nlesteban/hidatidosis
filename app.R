#
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(tidyverse)
#
load("data/datos.RData")
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Dashboard: Hidatidosis"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
#
    mainPanel(width = 12,
              
              column(width = 2,
                     selectInput("ano1",
                                  "Año inicio:",
                                  choices = 2007:2021,
                                  selected = 2007
                                  )),
              column(width = 2,
                     selectInput("ano2",
                                  "Año fin:",
                                 choices = 2007:2021,
                                 selected = 2021)),
              
              column(width = 3,
                     h4(textOutput("text0")))
    ),
    
    
    # Show a plot of the generated distribution
    mainPanel(width = 12,
      
      column(width = 12, 
             
             column(width = 4,
                    plotOutput("plot1", height = 250)),
             column(width = 4, offset = 0,
                    plotOutput("plot2", height = 250)),
             column(width = 4, offset = 0,
                    plotOutput("plot3", height = 250))
             
      
             )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  ### Info 0 
  
  output$text0 = renderText({
    
    paste0("Período ", input$ano1, "-", input$ano2, ". Casos totales: ", sum(tabla1()$n))
    
  })
  
  
  ### Info 1 
  
  tabla1 = reactive({
    
    t1 %>% filter(between(ae , as.numeric(input$ano1), as.numeric(input$ano2))) %>% 
      group_by(sexo2) %>% summarise(n = sum(n, na.rm=T))
  })
  
  output$plot1 = renderPlot({
    
    max = max(tabla1()$n)*1.1
    
    tabla1() %>% ggplot(aes(x = sexo2, y = n)) +
      geom_bar(stat = "identity", fill = "#40004b") +
      geom_text(aes(label=n), position=position_dodge(width=0.9), vjust=-0.25)+
      labs(title = "Casos totales de hidatidosis por sexo",
           subtitle = "Chile, período sleecionado",
           y = "Total")+
      lims(y = c(0,max))+
      theme_minimal() +
      theme(axis.title.x = element_blank())
    
    })  
  
  ### Info 2
  
  tabla2 = reactive({
    
    t2 %>% filter(between(ae , as.numeric(input$ano1), as.numeric(input$ano2))) %>% 
      group_by(edad) %>% summarise(n = sum(n, na.rm=T))
  })
  
  
  output$plot2 = renderPlot({
    
    tabla2() %>% ggplot(aes(x = edad, y = n)) +
      geom_bar(stat = "identity", fill = "#40004b") +
      labs(title = "Casos totales de hidatidosis por edad",
           subtitle = "Chile, período selecionado",
           y = "Total")+
      theme_minimal()+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
            axis.title.x = element_blank())
    })  
  
  ### Info 3
  
  tabla3 = reactive({
    
    t3 %>% filter(between(ae , as.numeric(input$ano1), as.numeric(input$ano2))) 
  })
  
  
  output$plot3 = renderPlot({
    
    tabla3() %>% ggplot(aes(x = ae, y = n)) +
      geom_line(stat = "identity", color = "#40004b", linewidth = 1) +
      geom_point(color = "#40004b")+
      labs(title = "Evolución de los casos anuales de hidatidosis",
           subtitle = "Chile, período selecionado",
           y = "Total")+
      scale_x_continuous(breaks = seq(2007, 2021, 1))+
      lims(y = c(0,450))+
      theme_minimal()+
      
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
            axis.title.x = element_blank())
    
    
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
options(shiny.host = '0.0.0.0')
options(shiny.port = 4000)

