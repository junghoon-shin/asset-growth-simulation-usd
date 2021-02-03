library(grid)
library(magrittr)
library(plotly)
library(RColorBrewer)
library(shiny)
library(shinythemes)
library(tidyverse)

# Set ggplot2 theme

theme_set(theme_bw(base_family = "Lato"))

# Define color

qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
qual_col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals))) %>% unique
line_color = qual_col_vector[c(5:6, 52)]

# Define functions

annual_return = function(monthly_investment, annual_increment, profit, duration, year) {
    sum(monthly_investment * annual_increment^(year - 1) * profit^(1:12/12)) * profit^(duration - year)
}

annual_return = Vectorize(annual_return, "year")

final_return = function(initial, monthly_investment, annual_increment, profit, duration, inflation) {
    total_input = initial + sum(monthly_investment * 12 * annual_increment^(0:(duration - 1)))
    unadjusted_return = (initial * profit^duration) + sum(annual_return(monthly_investment, annual_increment, profit, duration, 1:duration))
    adjusted_return = ((initial * profit^duration) + sum(annual_return(monthly_investment, annual_increment, profit, duration, 1:duration))) / inflation^duration
    return(c(total_input = total_input, unadjusted = unadjusted_return, adjusted = adjusted_return))
}

final_return = Vectorize(final_return, "duration")

# Build up user interface

ui = fluidPage(
    
    tags$head(
        tags$style(HTML("
            @import url('https://fonts.googleapis.com/css2?family=Lato:ital,wght@0,100;0,300;0,400;0,700;0,900;1,100;1,300;1,400;1,700;1,900&family=Open+Sans:ital,wght@0,300;0,400;0,600;0,700;0,800;1,300;1,400;1,600;1,700;1,800&display=swap');
            body {
                font-family: Lato, 'Open Sans', sans-serif;
            }"))
    ),
    
    titlePanel("Asset growth simulation"),
    
    hr(),
    
    fluidRow(
        column(6,
               sliderInput("initial", "Initial investment (thousand $)", min = 1, max = 200, value = 20, width = "100%"),
               sliderInput("monthly", "Investment per month (hundred $)", min = 1, max = 200, value = 50, width = "100%"),
               sliderInput("increment", "Investment increment per year (%)", min = 0, max = 100, value = 5, width = "100%")
        ),
        column(6, 
               sliderInput("profit", "Target profit (%)", min = 0, max = 100, value = 20, width = "100%"),
               sliderInput("inflation", "Inflation (%)", min = 0, max = 20, value = 2, width = "100%"),
               sliderInput("duration", "Investment period (years)", min = 1, max = 50, value = 30, width = "100%")
        )
    ),
    
    hr(),
    
    plotlyOutput("total_area", width = "100%", height = "auto")
)

# Instruct the server

server = function(input, output, session) {
    
    # Simulate the scenario to get a reactive result table
    
    return_simulation = reactive({
        final_return(initial = input$initial/1000,
                     monthly_investment = input$monthly/10000,
                     annual_increment = 1 + input$increment/100, 
                     profit = 1 + input$profit/100, 
                     duration = 1:input$duration,
                     inflation = 1 + input$inflation/100) %>% 
            t %>% 
            as_tibble %>%
            mutate(`Years from now` = row_number()) %>%
            bind_rows(tibble(total_input = input$initial/1000, unadjusted = input$initial/1000, adjusted = input$initial/1000, `Years from now` = 0), .) %>%
            pivot_longer(-`Years from now`, names_to = "category", values_to = "Amount (million USD)") %>%
            mutate(category = case_when(category == "total_input" ~ "Total input",
                                        category == "unadjusted" ~ "Total return",
                                        category == "adjusted" ~ "Inflation-adjusted return") %>%
                     factor(levels = c("Total return", "Inflation-adjusted return", "Total input")))
    })
    
    # Draw the simulation result as a reactive ggplot
    
    base_plot = reactive({
        return_simulation() %>% 
            ggplot(mapping = aes(x = `Years from now`, y = `Amount (million USD)`)) +
            geom_line(mapping = aes(color = category)) +
            scale_x_continuous(expand = c(0, 0)) +
            scale_color_manual(values = line_color, guide = guide_legend(title = NULL)) +
            theme(legend.key = element_rect(fill = "transparent"), 
                  legend.background = element_rect(fill = "transparent"))
    })
    
    output$total_area = renderPlotly({
        ggplotly(base_plot(),
                 height = session$clientData$output_total_area_width/1.4,
                 tooltip = c("Years from now", "Amount (million USD)")) %>%
            layout(legend = list(x = 0.03, y = 0.95, font = list(size = 14)))
    })
}

# Create Shiny app

shinyApp(ui = ui, server = server)