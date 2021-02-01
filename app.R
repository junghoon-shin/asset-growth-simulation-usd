library(magrittr)
library(shiny)
library(tidyverse)
library(shinythemes)
library(grid)

theme_set(theme_bw(base_family = "Lato"))

ui = fluidPage(
    
    titlePanel("Asset growth simulation"),
    
    hr(),
    
    fluidRow(
        column(6,
               sliderInput("initial", h4("Initial investment (thousand $)", style = "font-weight: bold"), min = 1, max = 500, value = 20, width = "100%"),
               sliderInput("monthly", h4("Investment per month (hundred $)", style = "font-weight: bold"), min = 1, max = 500, value = 50, width = "100%"),
               sliderInput("increment", h4("Investment increment per year (%)", style = "font-weight: bold"), min = 0, max = 100, value = 5, width = "100%")
        ),
        column(6, 
               sliderInput("profit", h4("Target profit (%)", style = "font-weight: bold"), min = 0, max = 100, value = 20, width = "100%"),
               sliderInput("inflation", h4("Inflation (%)", style = "font-weight: bold"), min = 0, max = 20, value = 2, width = "100%"),
               sliderInput("duration", h4("Investment period (years)", style = "font-weight: bold"), min = 1, max = 50, value = 30, width = "100%")
        )
    ),
    
    hr(),
    
    textO
    
    hr3("")
    helpText("In the plot below, You can drag any area to see "),
    
    plotOutput("total_area",
               click = clickOpts("mouse_click"),
               brush = brushOpts("mouse_drag")),
    
    plotOutput("dragged_area")
)

server = function(input, output) {
    
    # Define functions
    
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

    get_xlim = function(mouse_drag) {
        if (is.null(mouse_drag)) return(c(0, 0))
        else return(c(mouse_drag$xmin, mouse_drag$xmax))
    }
    
    get_ylim = function(mouse_drag) {
        if (is.null(mouse_drag)) return(c(0, 0))
        else return(c(mouse_drag$ymin, mouse_drag$ymax))
    }
    
    get_click_location = function(mouse_click) {
        if (is.null(mouse_click)) return(list(x = 0, y = 0))
        else return(list(x = mouse_click$x, y = mouse_click$y))
    }
    
    # Assign user mouse input to R objects
    
    drag_xlim = reactive({
        get_xlim(input$mouse_drag)
    })
    
    drag_ylim = reactive({
        get_ylim(input$mouse_drag)
    })
    
    click_location = reactive({
        get_click_location(input$mouse_click)
    })
    
    # Simulate the scenario to get a reactive result table
    
    return_simulation = reactive({
        
        final_return(principal = input$initial/1000,
                     initial_investment = input$monthly/10000,
                     annual_increment = 1 + input$increment/100, 
                     profit = 1 + input$profit/100, 
                     duration = 1:input$duration,
                     inflation = 1 + input$inflation/100) %>% 
            t %>% 
            as_tibble %>%
            mutate(year = row_number()) %>%
            bind_rows(tibble(unadjusted = input$initial/1000, adjusted = input$initial/1000, year = 0), .) %>%
            pivot_longer(-year, names_to = "adjustment", values_to = "amount") %>%
            mutate(adjustment = factor(adjustment, levels = c("unadjusted", "adjusted")))
        
    })
    
    # Draw the simulation result as a reactive ggplot

    base_plot = reactive({
        
        return_simulation() %>% ggplot(mapping = aes(x = year, y = amount)) +
            geom_line(mapping = aes(color = adjustment), size = 1) +
            scale_x_continuous(expand = c(0, 0)) +
            scale_y_continuous(trans = "log10") +
            scale_color_brewer(type = "qual", labels = c("Total return", "Inflation-adjusted return"), guide = guide_legend(title = NULL)) +
            labs(x = "Years from now", y = "Asset (million USD)") +
            theme(legend.position = c(0, 1), 
                  legend.justification = c(0, 1), 
                  legend.key = element_rect(fill = "transparent"), 
                  legend.background = element_rect(fill = "transparent"),
                  plot.margin = unit(c(5.5, 11, 5.5, 5.5), "points"))
        
    })
    
    total_area = reactive({
        
        base_plot() + annotate(geom = "text", 
                               label = list(bquote(.(round(click_location()$x, 1))*","~.(round(click_location()$y, 1)))), 
                               x = click_location()$x, 
                               y = click_location()$y, 
                               hjust = 1.5,
                               vjust = -0.5,
                               parse = T)
    
    })
    
    dragged_area = reactive({
        
        base_plot() + coord_cartesian(xlim = drag_xlim(), ylim = drag_ylim())
        
    })
    
    output$total_area = renderPlot(total_area())
    
    output$dragged_area = renderPlot(dragged_area())
    
}

shinyApp(ui = ui, server = server)