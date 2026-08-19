## ui.r ##
library('shinydashboard')

ui <- dashboardPage(
  dashboardHeader(title = 'Dr. Contagio'),
  dashboardSidebar(
    sidebarMenu(id = 'tabs',
      menuItem("Settings", tabName = "settings", icon = icon("sliders")),
      menuItem("Simulation", tabName = "simulation", icon = icon("dashboard")),
      menuItem("Info", tabName = "info", icon = icon("info")),
      img(src = "plague_doctor_crow3.webp", height='200px', width='200px', style="display: block; margin: auto;")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = 'settings',
        box(
          selectInput("disease_select", label = h3("Select disease"), choices=NULL),
          sliderInput("world_size", "Size of the world:", 5, 100, 20),
          textInput("pop_size", "Population size:", value = 1000, width = 100),
          sliderInput("perc_sick", "Initial percentage of sick individuals:", 0, 100, 1),
          sliderInput("perc_immune", "Initial percentage of immune individuals:", 0, 100, 0),
          fluidRow(
            column(4, textInput("p_beta", "P(transmission):", value=50, width=100)),
            column(4, textInput("p_recov", "P(recovery):", value=10, width=100)),
            column(4, textInput("p_death", "P(death):", value=1, width=100)),
          ),
          sliderInput("n_steps", "Number of days:", 5, 100, 50) ,
        ),
        box(
          h2('Simulation Parameters'),
           textOutput('world_size_out'),
           textOutput('pop_size_out'),
           textOutput('perc_sick_out'),
           textOutput('perc_immune_out'),
           textOutput('p_beta_out'),
           textOutput('p_recov_out'),
           textOutput('p_death_out'),
           textOutput('n_steps_out'),
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
        includeMarkdown("info.md")
      )
    )
  )
)
