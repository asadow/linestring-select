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
      geom_segment(aes(x = xini, y = yini, xend = xend, yend = yend), alpha = 0) +
      geom_point(aes(x = xini, y = yini), alpha = 0) +
      theme(axis.title.x = element_blank(), axis.title.y = element_blank())
    
    p <- if (!is.null(input$hot)) {
      df <- hot_to_r(input$hot)
      df_no_c <- df |> filter(is.na(coordinates))
      df_ya_c <- df |> filter(!is.na(coordinates))
      p +
        annotate(
          "point",
          x = df_no_c$x,
          y = df_no_c$y,
          colour = "red",
          size = 2
        ) +
        annotate(
          "point",
          x = df_ya_c$x,
          y = df_ya_c$y,
          colour = "limegreen",
          size = 2
        )
    } else {
      p
    }
    
    p |>
      ggplotly() |>
      config(
        displaylogo = FALSE,
        displayModeBar = TRUE,
        scrollZoom = TRUE,
        modeBarButtonsToRemove = c('zoom2d',
                                   'autoScale', 
                                   'hoverClosestCartesian', 
                                   'hoverCompareCartesian', 
                                   'toImage')
      ) |> 
      event_register("plotly_selected") |>
      layout(dragmode = "lasso") |> 
      layout(showlegend = FALSE)
  })
  
}

shinyApp(ui, server)
