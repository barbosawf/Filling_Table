library(shiny)
library(gt)
gt_tbl <-
  gtcars |>
  gt() |>
  fmt_currency(columns = msrp, decimals = 0) |>
  cols_hide(columns = -c(mfr, model, year, mpg_c, msrp)) |>
  cols_label_with(columns = everything(), fn = toupper) |>
  data_color(columns = msrp, method = "numeric", palette = "viridis") |>
  sub_missing() |>
  opt_interactive(use_compact_mode = TRUE)

ui <- fluidPage(
  gt_output(outputId = "table")
)

server <- function(input, output, session) {
  output$table <- render_gt(expr = gt_tbl)
}

shinyApp(ui = ui, server = server)
