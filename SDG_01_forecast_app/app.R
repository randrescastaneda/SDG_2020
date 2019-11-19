library(shiny)
source("setup.R")

# Define UI for application that draws poverty projections
ui <- fluidPage(

  # Application title
  titlePanel("Global Poverty forecast"),

  # Sidebar with a slider input for alpha and extra growth
  sidebarLayout(
    sidebarPanel(
      selectInput("extragrowth",
                  "Select growth",
                  extragrowth,
                  selected = 0
      ),
      selectInput("alpha",
                  "Select alpha",
                  alpha,
                  selected = 0)
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotlyOutput("line_chart", height = "600px") # Main chart handle and height
    )
  )
)

# Define server logic
server <- function(input, output) {

  output$line_chart <- renderPlotly({
    # Filter model to plot
    tmp <- pr_wld_act %>%
      filter(alpha == input$alpha | year <=  2018,
             extragrowth == input$extragrowth  | year <= 2018)

    #----------------------------------------------------------
    #   charts
    #----------------------------------------------------------

    #--------- prepare theme

    ggthemr_reset()
    ggthemr('flat')

    # gradient of line
    gr_pl <- paletteer_dynamic(package = "cartography", palette = "blue.pal",
                               n = 12, direction = -1)
    gr_pl <- gr_pl[3:length(gr_pl)]  # remove darkest colors

    clr_point <- swatch()[c(3, 5, 4, 6, 8, 9)]

    #--------- plot

    plain <- theme(
      #axis.text = element_blank(),
      #axis.line = element_blank(),
      #axis.ticks = element_blank(),
      panel.border = element_blank(),
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "white"),
      #axis.title = element_blank(),
      plot.title = element_text(hjust = 0.5),
      # legend.position = "bottom",
      legend.position = "none",
      legend.box = "horizontal"
    )


    wld_p <- ggplot() +
      geom_point(data = cty,
                 aes(x = year, y = headcount,
                     size = poor_pop, fill = region),
                 alpha = .7, pch = 21) +
      scale_fill_manual(values = clr_point) +
      geom_line(data = tmp,
                aes(x = year,  y = headcount, colour  = extragrowth),
                size = 1.5) +
      scale_colour_manual(values = gr_pl, aesthetics = c("colour")) +
      scale_y_continuous(
        labels = scales::percent,
        limits = c(0, 0.8),
        breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7)
      ) +
      labs(y = "Poverty rate (%)",
           x = "",
           size = "Poor population\n(Millions)") + plain
    ggplotly(wld_p)

  })
}

# Run the application
shinyApp(ui = ui, server = server)
