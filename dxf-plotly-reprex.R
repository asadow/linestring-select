library(tidyverse)
library(plotly)
library(shiny)
library(reactable)
library(sf)
library(lwgeom)
library(bslib)

dxf <- st_read(here('d3map/giraffe360_demo_residential.dxf'))

dxf_gp <- dxf |> 
  group_by(geometry_type = st_geometry_type(dxf)) |>
  nest()

dxf_lns <- dxf_gp |> 
  filter(geometry_type == "LINESTRING") |> 
  unnest(cols = c(data)) |> 
  mutate(
    geometry = st_zm(geometry, "LINESTRING"),
    ini = st_startpoint(geometry),
    end = st_endpoint(geometry)
  )

dxf_lns <- dxf_lns |> 
  mutate(
    across(
      c(ini, end),
      list(
        x = \(point) st_coordinates(point)[,1],
        y = \(point) st_coordinates(point)[,2]
      ),
      .names = "{.fn}{.col}"
    )
  )

dxf_lns$key <- row.names(dxf_lns)
dxf_lns$col <- "black"

ui <- bslib::page_fluid(
  plotlyOutput("floor_plot")
)

server <- function(input, output, session) {
    
  dxf_lns <- reactiveVal(dxf_lns)
  
  output$floor_plot <- renderPlotly({
    dat <- dxf_lns()
    d <- event_data("plotly_selected")
    
    if (!is.null(d)) {
      dat[dat$key %in% d$key, "col"] <- "blue"
    }
    
    p <- ggplot(dat, aes(geometry = geometry, col = I(col), key = key)) +
      geom_sf(data = dat) +
      geom_point(aes(x = xini, y = yini), alpha = 0) +
      theme(axis.title.x = element_blank(), axis.title.y = element_blank())
    
    p |>
      ggplotly() |>
      event_register("plotly_selected") |>
      layout(dragmode = "lasso")
  })
  
}

shinyApp(ui, server)
