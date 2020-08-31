library(shiny)
library(colourpicker)
library(ggplot2)
library(shinyjs)
library(reshape2)

iris2 <- melt(iris, id.vars = "Species")

theme_set(theme_minimal() +
            theme(legend.position = "top"))


ui <- fluidPage(
  useShinyjs(),
  fluidRow(textInput("title", "Theme name", "MyTheme")),
  fluidRow(
    column(
      3,
      colourpicker::colourInput("pgcol", "Select page background", "gray",
                                allowTransparent = TRUE)
    ),
    column(
      3,
      colourpicker::colourInput("plotcol", "Select plot background", "white",
                                allowTransparent = TRUE)
    ),
    column(
      3,
      colourpicker::colourInput("titlecol", "Select Tile background", "gray",
                                allowTransparent = TRUE)
    ),
    column(
      3,
      colourpicker::colourInput(
        "annotationcol",
        "Select text colour",
        "black",
        allowTransparent = TRUE
      )
    )
  ),
  fluidRow(
    column(
      1,
      colourpicker::colourInput("col1", "Select colour1", "#002d72",
                                allowTransparent = TRUE)
    ),
    column(
      1,
      colourpicker::colourInput("col2", "Select colour2", "#da291c",
                                allowTransparent = TRUE)
    ),
    column(
      1,
      colourpicker::colourInput("col3", "Select colour3", "#f2cd00",
                                allowTransparent = TRUE)
    ),
    column(
      1,
      colourpicker::colourInput("col4", "Select colour4", "#658d1b",
                                allowTransparent = TRUE)
    ),
    column(
      1,
      colourpicker::colourInput("col5", "Select colour5", "#0033a0",
                                allowTransparent = TRUE)
    ),
    column(
      1,
      colourpicker::colourInput("col6", "Select colour6", "#00aecc",
                                allowTransparent = TRUE)
    ),
    column(
      1,
      colourpicker::colourInput("col7", "Select colour7", "#95b9cb",
                                allowTransparent = TRUE)
    ),
    column(
      1,
      colourpicker::colourInput("col8", "Select colour8", "#0d1c42",
                                allowTransparent = TRUE)
    )
  ),
  wellPanel(fluidRow(
    id = 'wp1',
    column(6, plotOutput("plot")),
    column(6, plotOutput("plot2"))
  ), id = 'wp2')
  ,
  downloadButton("downloadData", "Download theme")
)

server <- function(input, output) {
  values <- reactiveValues()
  values$theme_out <- '{
        "name": "Light theme LS colours",
        "background": "#FFFFFF",
        "foreground": "#000000",
        "tableAccent": "#000000",
        "maximum": "#e97f77",
        "center": "#c1d1a4",
        "minimum": "#99acb7",
        "null": "#b2b2b2",
        "dataColors": [
            "#002d72",
            "#da291c",
            "#f2cd00",
            "#658d1b",
            "#0033a0",
            "#00aecc",
            "#95b9cb",
            "#0d1c42",
            "#5095d0",
            "#a7a9ac",
            "#1457a8",
            "#012e6d",
            "#6487a3",
            "#d1d3d4",
            "#58595b",
            "#000000"
        ]}'
  output$plot <- renderPlot({
    cbp1 <-
      c(
        input$col1,
        input$col2,
        input$col3,
        input$col4,
        input$col5,
        input$col6,
        input$col7,
        input$col8
      )
    ggplot(iris, aes(Sepal.Length, Sepal.Width)) + geom_point(aes(color = Species, size =
                                                                    4)) + scale_color_manual(values = cbp1) +
      ggtitle("Plot1") +
      labs(y = "Width", x = "Length") +
      theme(
        panel.background = element_rect(
          fill = input$plotcol,
          colour = input$plotcol,
          size = 0.5,
          linetype = "solid"
        ),
        plot.background = element_rect(fill = input$plotcol),
        panel.grid.major = element_line(
          size = 0.5,
          linetype = 'solid',
          colour = input$annotationcol
        ),
        panel.grid.minor = element_line(
          size = 0.25,
          linetype = 'solid',
          colour = input$annotationcol
        ),
        axis.text.x = element_text(color = input$annotationcol),
        axis.text.y = element_text(color = input$annotationcol),
        axis.title.x = element_text(color = input$annotationcol),
        axis.title.y = element_text(color = input$annotationcol),
        legend.text=element_text(color=input$annotationcol,size=10),
        plot.title = element_text(color=input$annotationcol, size=14)
      )
  })
  
  output$plot2 <- renderPlot({
    cbp1 <-
      c(input$col4, input$col5, input$col6, input$col7, input$col8)
    ggplot(iris2, aes(x = Species, y = value, fill = variable)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(
        values = cbp1,
        name = "Iris\nMeasurements",
        breaks = c(
          "Sepal.Length",
          "Sepal.Width",
          "Petal.Length",
          "Petal.Width"
        ),
        labels = c("Sepal Length", "Sepal Width", "Petal Length", "Petal Width")
      ) +
      ggtitle("Plot2") +
      labs(y = "Freq", x = "Width") +
      theme(
        panel.background = element_rect(
          fill = input$plotcol,
          colour = input$plotcol,
          size = 0.5,
          linetype = "solid"
        ),
        plot.background = element_rect(fill = input$plotcol),
        panel.grid.major = element_line(
          size = 0.5,
          linetype = 'solid',
          colour = input$annotationcol
        ),
        panel.grid.minor = element_line(
          size = 0.25,
          linetype = 'solid',
          colour = input$annotationcol
        ),
        axis.text.x = element_text(color = input$annotationcol),
        axis.text.y = element_text(color = input$annotationcol),
        axis.title.x = element_text(color = input$annotationcol),
        axis.title.y = element_text(color = input$annotationcol),
        legend.text=element_text(color=input$annotationcol,size=10),
        plot.title = element_text(color=input$annotationcol, size=14)
      )
  })
  
  observeEvent(input$pgcol, {
    runjs(
      sprintf(
        "
            document.getElementById('%s').style.backgroundColor = '%s';",
        "wp1",
        input$pgcol
      )
    )
    runjs(
      sprintf(
        "
            document.getElementById('%s').style.backgroundColor = '%s';",
        "wp2",
        input$pgcol
      )
    )
    runjs(
      sprintf(
        "
            document.getElementById('%s').style.backgroundColor = '%s';",
        "wp3",
        input$pgcol
      )
    )
  })
  
  toListen <- reactive({
    list(
      input$col1,
      input$col2,
      input$col3,
      input$col4,
      input$col5,
      input$col6,
      input$col7,
      input$col8,
      input$title,
      input$pgcol,
      input$plotcol,
      input$titlecol,
      input$annotationcol
    )
  })
  
  observeEvent(toListen(), {
    values$theme_out = paste(
      sprintf(
        '{
            "name": "%s",
            "background": "#FFFFFF",
            "foreground": "#000000",
            "tableAccent": "#000000",
            "maximum": "#e97f77",
            "center": "#c1d1a4",
            "minimum": "#99acb7",
            "null": "#b2b2b2",
            "dataColors": ["%s","%s","%s","%s","%s","%s","%s","%s"],
            "visualStyles": {
        "multiRowCard": {
            "*": {
                "dataLabels": [
                    {
                        "color": {
                            "solid": {
                                "color": "%s"
                            }
                        },
                        "fontSize": 12,
                        "fontFamily": "Arial"
                    }
                ],
                "categoryLabels": [
                    {
                        "show": true,
                        "color": {
                            "solid": {
                                "color": "%s"
                            }
                        },
                        "fontSize": 12,
                        "fontFamily": "Arial"
                    }
                ],
                "cardTitle": [
                    {
                        "color": {
                            "solid": {
                                "color": "%s"
                            }
                        },
                        "background": {
                            "solid": {
                                "color": "%s"
                            }
                        },
                        "fontSize": 10,
                        "fontFamily": "Arial"
                    }
                ],
                "card": [
                    {
                        "outline": "None",
                        "outlineColor": {
                            "solid": {
                                "color": ""
                            }
                        },
                        "outlineWeight": 1,
                        "barShow": true,
                        "barColor": {
                            "solid": {
                                "color": "%s"
                            }
                        },
                        "barWeight": 5,
                        "cardPadding": 20,
                        "cardBackground": {
                            "solid": {
                                "color": "%s"
                            }
                        }
                    }
                ]
            }
        },
        "card": {
            "*": {
                "labels": [
                    {
                        "show": true,
                        "color": {
                            "solid": {
                                "color": "%s"
                            }
                        },
                        "fontSize": 12,
                        "fontFamily": "Arial"
                    }
                ]
            }
        },
        "page": {
            "*": {
                "background": [
                    {
                        "color": {
                            "solid": {
                                "color": "%s"
                            }
                        },
                        "transparency": 0
                    }
                ],
                "outspace": [
                    {
                        "color": {
                            "solid": {
                                "color": "%s"
                            }
                        }
                    }
                ]
            }
        },
        "*": {
            "*": {
                "background": [
                    {
                        "show": true,
                        "color": {
                            "solid": {
                                "color": "%s"
                            }
                        },
                        "transparency": 0
                    }
                ],
                "*": [
                    {
                        "fontFamily": "Arial",
                        "fontSize": 12,
                        "wordWrap": true
                    }
                ],
                "valueAxis": [
                    {
                        "show": true,
                        "titleColor":{"solid":{"color":"%s"}},
                        "showAxisTitle": true,
                        "axisScale": "linear",
                        "labelColor": {
                            "solid": {
                                "color": "%s"
                            }
                        },
                        "color": {
                            "solid": {
                                "color": "%s"
                            }
                        },
                        "*": {
                            "showMarker": true
                        },
                        "fontSize": 9,
                        "fontFamily": "Arial"
                    }
                ],
                "title": [
                    {
                        "show": true,
                        "alignment": "center",
                        "fontColor": {
                            "solid": {
                                "color": "%s"
                            }
                        },
                        "background": {
                            "solid": {
                                "color": "%s"
                            }
                        },
                        "fontSize": 14,
                        "fontFamily": "Arial"
                    }
                ],
                "legend": [
                    {
                        "show": true,
                        "position": "Top Left",
                        "showTitle": false,
                        "labelColor": {
                            "solid": {
                                "color": "%s"
                            }
                        },
                        "fontSize": 8,
                        "fontFamily": "Arial"
                    }
                ],
                "categoryAxis": [
                    {
                        "show": true,
                        "titleColor":{"solid":{"color":"%s"}},
                        "showAxisTitle": true,
                        "labelColor": {
                            "solid": {
                                "color": "%s"
                            }
                        },
                        "fontSize": 9,
                        "fontFamily": "Arial",
                        "color": {
                            "solid": {
                                "color": "%s"
                            }
                        }
                    }
                ],
                "filterCard": [
                    {
                        "$id": "Applied",
                        "foregroundColor": {
                            "solid": {
                                "color": "%s"
                            }
                        }
                    },
                    {
                        "$id": "Available",
                        "border": true
                    }
                ]
                }},',
        input$title,
        input$col1,
        input$col2,
        input$col3,
        input$col4,
        input$col5,
        input$col6,
        input$col7,
        input$col8,
        input$annotationcol,
        input$annotationcol,
        input$annotationcol,
        input$plotcol,
        input$col1,
        input$plotcol,
        input$annotationcol,
        input$pgcol,
        input$pgcol,
        input$plotcol,
        input$annotationcol,
        input$annotationcol,
        input$annotationcol,
        input$annotationcol,
        input$titlecol,
        input$annotationcol,
        input$annotationcol,
        input$annotationcol,
        input$annotationcol,
        input$plotcol
      ),
      sprintf(
        '"slicer": {
            "*": {
                "*": [
                    {
                        "fontSize": 10,
                        "fontColor": {
                            "solid": {
                                "color": "%s"
                            }
                        }
                    }
                ],
                "selection": [
                    {
                        "singleSelect": false,
                        "selectAllCheckboxEnabled": true
                    }
                ],
                "header": [
                    {
                        "show": false,
                        "fontColor": {
                            "solid": {
                                "color": "%s"
                            }
                        },
                        "textSize": 10,
                        "fontFamily": "Arial"
                    }
                ],
                "items": [
                    {
                        "fontColor": {
                            "solid": {
                                "color": "%s"
                            }
                        },
                        "fontSize": 10,
                        "fontFamily": "Arial"
                    }
                ]
            }
        }
        }
                                       }',
        input$annotationcol,
        input$annotationcol,
        input$annotationcol
      ),
      spe = ""
    )
  })
  
  
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$title, ".json", sep = "")
    },
    content = function(file) {
      writeLines(values$theme_out, file)
    }
  )
  
}


# Run the application
shinyApp(ui = ui, server = server)
