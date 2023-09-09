#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
pacman::p_load(shiny, tidyverse, magrittr, readxl, 
               janitor, see, directlabels,
               conflicted)
conflicts_prefer(dplyr::filter)

## read in the need data
co2_tbl <- read_csv("annual-co-emissions-by-region.csv") %>% 
  clean_names()

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Global emissions have not yet peaked"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("years",
                  "Shown years:",
                  min = 1750,
                  max = 2021,
                  step = 25,
                  value = c(1750, 2021)),
      
      selectInput("method", "Method for line:",
                  c("Linear" = "lm",
                    "Loess" = "loess",
                    "Gam" = "gam")),
      
      radioButtons("se", 
                   "Standard deviation:",
                   c("Yes" = "yes",
                     "No" = "no")),
      
    ),
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("line_plot")
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$line_plot <- renderPlot({

    se_flag <- switch(input$se,
                      yes = TRUE,
                      no = FALSE)
    
    co2_tbl %>% 
      filter(entity %in% c("World", "Europe", "China", "Africa")) %>% 
      filter(year >= input$years[1] & year <= input$years[2]) %>% 
      ggplot(aes(year, annual_co2_emissions, color = entity)) +
      theme_bw() +
      geom_line() +
      stat_smooth(method = input$method, se = se_flag) +
      theme(legend.position = "none") +
      scale_x_continuous(expand = c(0, 30), breaks = c(seq(1750, 2021, by = 25), 2021)) +
      geom_dl(aes(label = entity), method = list(dl.trans(x = x + 0.2), "last.points", cex = 0.8))
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

