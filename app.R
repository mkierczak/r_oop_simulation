## app.R ##
library(shiny)
library(shinydashboard)
library(R6)
library(tidyverse)
library(ggplot2)
source('class_agent.R')
source('class_world.R')
source('simulation.R')

visualize_frame <- function(results, what='sick', frame=1) {
  mapping <- tribble(
    ~param, ~title, ~color_min, ~color_max,
    "sick", "Sick individuals", "white", "red",
    "dead", "Dead individuals", "white", "black",
    "naive", "Naive individuals", "white", "slateblue",
    "immune", "Immune individuals", "white", "olivedrab"
  )
  params <- mapping |> filter(param == what)
  fill_sym <- sym(what)
  minval <- results |> select(fill_sym) |> min()
  maxval <- results |> select(fill_sym) |> max()
  results <- results |> filter(step == frame)
  p <- ggplot(results, aes(x = col, y = row)) +
    geom_tile(aes(fill = !!fill_sym), color='orange') +
    scale_fill_gradient(low = params$color_min, 
                        high = params$color_max,
                        limits = c(minval, maxval)
                      ) +
    theme_minimal() +
    labs(x = "X", y = "Y", fill = params$title) +
    ggtitle(paste(params$title, "- step", frame)) +
    theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
  return(p)
}

ui <- dashboardPage(
  dashboardHeader(title = 'Epidemics Simulator'),
  dashboardSidebar(
    sidebarMenu(id = 'tabs',
      menuItem("Settings", tabName = "settings", icon = icon("sliders")),
      menuItem("Simulation", tabName = "simulation", icon = icon("dashboard")),
      menuItem("Info", tabName = "info", icon = icon("info"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = 'settings',
        box(
          sliderInput("world_size", "Size of the world:", 5, 100, 20),
          textInput("pop_size", "Population size:", value = 1000, width = 100),
          sliderInput("perc_sick", "Initial percentage of sick individuals:", 0, 100, 1),
          sliderInput("perc_immune", "Initial percentage of immune individuals:", 0, 100, 0),
          sliderInput("p_beta", "P(transmission):", 0, 100, 50),
          sliderInput("p_recov", "P(recovery):", 0, 100, 10),
          sliderInput("p_death", "P(death):", 0, 100, 5),
          sliderInput("n_steps", "Number of steps:", 5, 100, 50) ,
        ),
        box(
          h2('Simulation Parameters'),
           textOutput('world_size'),
           textOutput('pop_size'),
           textOutput('perc_sick'),
           textOutput('perc_immune'),
           textOutput('p_beta'),
           textOutput('p_recov'),
           textOutput('p_death'),
           textOutput('n_steps'),
           br(),
           actionButton("runBtn","Run"),
        ),
        box(
          plotOutput('summaryPlot'),
        )
      ),
      tabItem(tabName = 'simulation',
        sliderInput("frame", "Step:", 0, 50, 0),
        box(plotOutput('framePlot_naive', height = "300px")),
        box(plotOutput('framePlot_sick', height = "300px")),
        box(plotOutput('framePlot_immune', height = "300px")),
        box(plotOutput('framePlot_dead', height = "300px"))
      ),
      tabItem(tabName = 'info',
        h2('Info')
      )
    )
  )
)

server <- function(input, output, session) { 
  img <- png::readPNG('dr_plague.png')
  output$summaryPlot <- renderPlot(grid::grid.raster(img))
  output$world_size <- reactive({
    paste0('- world size: ', input$world_size, ' x ', input$world_size, ' cells')
  })
  output$pop_size <- reactive({
    density <- round(as.numeric(input$pop_size) / as.numeric(input$world_size)^2, digits=2)
    paste0('- population size: ', input$pop_size, ' (', density, ' ind./cell)')
  })
  output$perc_sick <- reactive({
    perc_sick <- as.numeric(input$perc_sick)/100
    n_sick <- floor(perc_sick * as.numeric(input$pop_size))
    paste0('- sick: ', n_sick, ' (',input$perc_sick,'%)')
  })
  output$perc_immune <- reactive({
    perc_immune <- as.numeric(input$perc_immune)/100
    n_immune <- floor(perc_immune * as.numeric(input$pop_size))
    paste0('- immune: ', n_immune, ' (',input$perc_immune,'%)')
  })
  output$p_beta <- reactive({
    paste0('- P(transmission) = ', input$p_beta, '%')
  })
  output$p_recov <- reactive({
    paste0('- P(recovery) = ', input$p_recov, '%')
  })
  output$p_death <- reactive({
    paste0('- P(death) = ', input$p_death, '%')
  })
  output$n_steps <- reactive({
    paste0('- N simulation steps: ', input$n_steps)
  })

  # Update slider for plotting
  observe({
    a_val <- input$n_steps
    
    # Define new min and max for slider B based on A
    new_min <- 0
    new_max <- a_val

    # Update slider B
    updateSliderInput(session, "frame",
                      min = 0,
                      max = new_max,
                      value = 0)
  })

  observeEvent(input$runBtn, {
    results <- list()
    initial <- c()
    probs <- c()
    perc_immune <- as.numeric(input$perc_immune)/100
    n_immune <- floor(perc_immune * as.numeric(input$pop_size))
    perc_sick <- as.numeric(input$perc_sick)/100
    n_sick <- floor(perc_sick * as.numeric(input$pop_size))
    n_naive <- as.numeric(input$pop_size) - (n_sick + n_immune)
    initial['naive'] <- n_naive
    initial['immune'] <- n_immune
    initial['sick'] <- n_sick
    probs['beta'] <- as.numeric(input$p_beta) / 100
    probs['recovery'] <- as.numeric(input$p_recov) / 100
    probs['death'] <- as.numeric(input$p_death) / 100
    world <- init_simulation(as.numeric(input$world_size), initial)
    counts <- world$get_counts()
    counts$step <- 1
    results[[1]] <- counts
    N <- as.numeric(input$n_steps)
    withProgress(message = 'Running simulation...', value = 0, {
      for (i in 2:N+1) {
        world <- run_step(world, probs)
        counts <- world$get_counts()
        counts$step <- i
        results[[i]] <- counts
        incProgress(1/N, detail = paste0("step ", i-1, ' of ', N))
      }
    })
    results <- do.call(rbind, results)
    gg <- results |> group_by(step) |> 
  summarise(n_sick = sum(sick), n_naive = sum(naive), n_immune = sum(immune), n_dead = sum(dead)) |> 
  pivot_longer(starts_with("n_"), names_to = 'measure') |> 
  ggplot(aes(x = step, y = value, col=measure)) + geom_line() + theme_minimal()
    output$summaryPlot <- renderPlot(gg)
    output$framePlot_naive <- renderPlot(visualize_frame(results, what='naive', frame=input$frame+1))
    output$framePlot_sick <- renderPlot(visualize_frame(results, what='sick', frame=input$frame+1))
    output$framePlot_immune <- renderPlot(visualize_frame(results, what='immune', frame=input$frame+1))
    output$framePlot_dead <- renderPlot(visualize_frame(results, what='dead', frame=input$frame+1))
  })

}
shinyApp(ui, server)
