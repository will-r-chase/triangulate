##shiny app to select points

#enter your rds file for ordered points dataframe of your image outline
#file comes from get_edge_points.R
giraffe <- readRDS("giraffe_arranged.rds")


library(ggplot2)
shinyApp(
  ui = basicPage(
    fluidRow(
      column(width = 4,
             plotOutput("plot", height = 400,
                        click = "plot_click"  # Equiv, to click=clickOpts(id="plot_click")
             ),
             h4("Clicked points"),
             DT::DTOutput("plot_clickedpoints")
      ),
      column(width = 4,
             plotOutput("selected_plot", height = 400)),
      column(width = 2,
             wellPanel(actionButton("newplot", "New plot"))
      )
    )
  ),
  server = function(input, output, session) {
    data <- giraffe
    
    vals <- reactiveValues(
       keeprows = rep(TRUE, nrow(data)
    ))
    
    output$plot <- renderPlot({
      ggplot(data, aes(x,y)) +
        geom_path() +
        xlim(0, 2000) +
        ylim(0, 2500) +
        theme_void() +
        scale_y_reverse() 
    })
    output$selected_plot <- renderPlot({
      keep    <- data[ vals$keeprows, , drop = FALSE]
      exclude <- data[!vals$keeprows, , drop = FALSE]
      ggplot(exclude, aes(x,y)) +
        geom_path() +
        xlim(0, 2000) +
        ylim(0, 2500) +
        theme_void() +
        scale_y_reverse() 
    })
    observeEvent(input$plot_click, {
      res <- nearPoints(data, input$plot_click, maxpoints = 1, allRows = TRUE)
      vals$keeprows <- xor(vals$keeprows, res$selected_)
    })
    output$plot_clickedpoints <- DT::renderDT({
      exclude <- data[!vals$keeprows, , drop = FALSE]},
      extensions = c('Buttons'), 
      options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv'),
        pageLength = nrow(data)
    ))
  }
)
    