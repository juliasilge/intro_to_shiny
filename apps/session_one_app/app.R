library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(southafricastats)

mortality_zaf <- mortality_zaf %>%
    filter(!(indicator %in% c("All causes")))

# body of the UI
ui <- fluidPage(
   
   # application title
    titlePanel("Mortality in South African Provinces"),
    
    # sidebar with input
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "province",
                        label = "Province:",
                        choices = unique(mortality_zaf$province),
                        selected = "Gauteng", 
                        multiple = TRUE),
            checkboxInput(inputId = "show_data",
                          label = "Show table?",
                          value = FALSE)
        ),
        
        # show the output
        mainPanel(
            plotOutput(outputId = "mortalityPlot"),
            dataTableOutput(outputId = "mortalityTable")
        )
    )
)

# calculations behind the scenes
server <- function(input, output) {
    
    selected_df <- reactive({
        mortality_zaf %>%
            filter(province %in% input$province)
    })
    
    output$mortalityPlot <- renderPlot({
        selected_df() %>%
            ggplot(aes(year, deaths, color = indicator)) +
            geom_line(alpha = 0.8, size = 1.5) +
            facet_wrap(~province, scales = "free") +
            theme_minimal(base_size = 18)
    })
    
    output$mortalityTable <- renderDataTable({
        if(input$show_data){
            DT::datatable(data = selected_df())
        }
    })
    
}

# run the application 
shinyApp(ui = ui, server = server)

