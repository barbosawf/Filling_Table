library(DT)
library(shiny)

dat <- data.frame(
    V1 = c(as.character(numericInput("x11", "", 0)), as.character(numericInput("x21", "", 0))),
    V2 = c(as.character(numericInput("x21", "", 0)), as.character(numericInput("x22", "", 0)))
)

ui <- fluidPage(fluidRow(
    column(5, DT::dataTableOutput('my_table')),
    column(2),
    column(5, verbatimTextOutput("test"))
))

server <- function(input, output, session) {
    output$my_table <- DT::renderDataTable(
        dat,
        selection = "none",
        options = list(
            searching = FALSE,
            paging = FALSE,
            ordering = FALSE,
            dom = "t"
        ),
        server = FALSE,
        escape = FALSE,
        rownames = FALSE,
        colnames = c("", ""),
        callback = JS(
            "table.rows().every(function(i, tab, row) {
                  var $this = $(this.node());
                  $this.attr('id', this.data()[0]);
                  $this.addClass('shiny-input-container');
                  });
                  Shiny.unbindAll(table.table().node());
                  Shiny.bindAll(table.table().node());"
        )
    )
    
    output$test <- renderText({
        as.character(input$x11)
    })
    
}

shinyApp(ui, server)
