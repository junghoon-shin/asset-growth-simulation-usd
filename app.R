library(magrittr)
library(shiny)
library(tidyverse)
library(shinythemes)

ui = fluidPage(
    
    titlePanel("Asset growth simulation"),
    
    hr(),
    
    fluidRow(
        column(6,
               sliderInput("initial", "Initial investment (thousand $)", min = 1, max = 500, value = 20, width = "100%"),
               sliderInput("monthly", "Investment per month (hundred $)", min = 1, max = 500, value = 50, width = "100%"),
               sliderInput("increment", "Investment increment per year (%)", min = 0, max = 100, value = 5, width = "100%")
        ),
        column(6, 
               sliderInput("profit", "Target profit (%)", min = 0, max = 100, value = 20, width = "100%"),
               sliderInput("inflation", "Inflation (%)", min = 0, max = 20, value = 2, width = "100%"),
               sliderInput("duration", "Period (years)", min = 1, max = 50, value = 30, width = "100%")
        )
    ),
    
    hr(),
    
    plotOutput("growth")
    
)
?fluidRow

server = function(input, output) {
    
    annual_return = function(initial_investment, annual_increment, profit, duration, year) {
        sum(initial_investment * annual_increment^(year - 1) * profit^(1:12/12)) * profit^(duration - year)
    }
    
    annual_return = Vectorize(annual_return, "year")
    
    final_return = function(principal, initial_investment, annual_increment, profit, duration, inflation) {
        unadjusted_return = (principal * profit^duration) + sum(annual_return(initial_investment, annual_increment, profit, duration, 1:duration))
        adjusted_return = ((principal * profit^duration) + sum(annual_return(initial_investment, annual_increment, profit, duration, 1:duration))) / inflation^duration
        return(c(unadjusted = unadjusted_return, adjusted = adjusted_return))
    }
    
    final_return = Vectorize(final_return, "duration")
    
    output$growth <- renderPlot({
        
        return_simulation = final_return(principal = input$initial/1000,
                                         initial_investment = input$monthly/10000,
                                         annual_increment = 1 + input$increment/100, 
                                         profit = 1 + input$profit/100, 
                                         duration = 1:input$duration, 
                                         inflation = 1 + input$inflation/100) %>% 
            t %>% 
            as_tibble %>%
            mutate(year = row_number())
        
        return_simulation %<>% bind_rows(tibble(unadjusted = input$initial/1000, adjusted = input$initial/1000, year = 0), .)
        
        return_simulation %<>%
            pivot_longer(-year, names_to = "adjustment", values_to = "amount") %>%
            mutate(adjustment = factor(adjustment, levels = c("unadjusted", "adjusted")))
        
        return_simulation %>% ggplot(mapping = aes(x = year, y = amount)) +
            geom_line(mapping = aes(color = adjustment), size = 1) +
            scale_x_continuous(expand = c(0, 0)) +
            scale_y_continuous(trans = "log10") +
            scale_color_brewer(type = "qual", labels = c("Total return", "Inflation-adjusted return"), guide = guide_legend(title = NULL)) +
            labs(x = "Years from now", y = "Asset (million USD)") +
            theme(legend.position = c(0, 1), legend.justification = c(0, 1), legend.key = element_rect(fill = "transparent"), legend.background = element_rect(fill = "transparent"),
                  plot.margin = unit(c(5.5, 11, 5.5, 5.5), "points"))
        
    })
    
}

shinyApp(ui = ui, server = server)