#' Explore Timeseries in shiny app 
#' 
#' Pass data created with process_ts_data and launch the shiny app 
#' 
#' @param data A dataframe created with process_ts_data 
#' @param title The title for the app


launch_ts_explore <- function(data, title = ""){
  
  #For testing 
  # data <- get_business_turnover() %>%
  #   filter(data_type == "INDEX") %>%
  #   process_ts_data(
  #     value = value,
  #     category = industry,
  #     series = series_type,
  #     date = date,
  #     frequency = "month"
  #   )


  ##Get unique values to be passed to UI 
  series_types <- unique(data$series_type)
  
  #define color palette 
  #get unique categories 
  cats <- unique(data$category)
  
  #define colors based on length 
  #if fewer than 13 categories can use Set 3 
  #otherwise combine dark and pastel versions of set2 
  #and retain as many as required 
  palette <- if(length(cats) < 13){
    RColorBrewer::brewer.pal(length(cats), "Set3")
  } else {
    palette <- c(
      RColorBrewer::brewer.pal(8, "Dark2"),
      RColorBrewer::brewer.pal(8, "Pastel2")
    )[1:length(unique(data$category))]
    
  }
  
  #name each color according to level 
  names(palette) <- unique(data$category)
  
  
  #Date range 
  min_date <- min(data$date)
  max_date <- max(data$date)
  
  #Filter down to current ref period for bar chart 
  bar_data <- data %>% 
    filter(
      date == max(date)
    )
  
  
  ui <- fluidPage(
    #title panel as defined in functuon
    titlePanel(title),
      fluidRow(
        #inputs
        #Select series 
        column(
          radioButtons(
            inputId = "series",
            label = "Which series do you wish to visualise?",
            choices = series_types,
            selected = series_types[2]
          ),
          width = 3
        ),
        #bar chart visualisation 
        column(
          radioButtons(
            inputId = "barDisplay",
            label = "What do you want to display on the bar chart",
            choices = c("Current Value", "% Change"),
            selected = "Current Value"
          ),
          width = 3
        ),
        #percentage change
        column(
          radioButtons(
            inputId = "pct",
            label = "Where do you want to view change from?",
            choices = c("Last Period", "Last Year"),
            selected = "Last Period"
          ),
          width = 3
        ),
        #Add date range input 
        # column(
        #   dateRangeInput(
        #     inputId = "range",
        #     label = "Select a date range",
        #     start = min(data$date),
        #     end = max(data$date),
        #     min = min(data$date),
        #     max = max(data$date)
        #   ),
        #   width = 2
        # ),
        #reset graph action button 
        column(
          actionButton(
            inputId = "reset",
            label = "Reset Lineplot"
          ),
          width = 3
        )
      ), 
      #Bar plot and line plot
      fluidRow(
        column(
          width = 4,
          plotlyOutput("bar")
        ),
        column(
          width = 8,
          plotlyOutput("line")
        )
      )
    )
  
  
  server <- function(input, output, session) {
    
    ##Filter data to selected series 
    
    filtered_series <- reactive({
      data %>% 
        filter(
          series_type == input$series
        )
    })
    
    ##Filter data for bar plot 
    bar_data <- reactive({
      filtered_series() %>%
        data.frame() %>% 
        filter(
          series_type == input$series, 
          date == max(date)
        ) %>%
        #add rownumber for plotly click event referencing
        mutate(
          key = row_number()  
        )
    })
    
    ## Bar plot ----
    
    output$bar <- renderPlotly({
      
      #Determine column to plot 
      #and identify axis label as 2nd element
      plot_col <- if(input$barDisplay == "Current Value"){
        c("value", paste0("Current Value\n", input$series))
      } else {
        if(input$pct == "Last Period"){
          c("prd_chg", paste0("% Change from Last Period\n", input$series))
        } else {
          c("year_chg", paste0("% Change on Last Year\n", input$series))
        }
      }
      
      ggbar <- ggplot(
        data = bar_data(),
        aes(
          y = .data[[plot_col[1]]],
          x = category,
          color = category, 
          fill = category,
          group = category#,
          #text = label
        )
      ) +
        xlab("Category") + 
        ylab(plot_col[2]) + 
        scale_x_discrete(labels = ~str_wrap(bar_data()$category, width = 25)) +
        scale_color_manual(name = "category", values = palette) +
        scale_fill_manual(name = "category", values = palette) +
        geom_col() +
        coord_flip() +
        theme_classic() +
        theme(legend.position = "none",
              plot.title = element_text(hjust = -2)) 
      
      
      ggplotly(ggbar, 
               tooltip = "text") %>% 
        config(displayModeBar = F) 
      
      })
    
    #Set up reactive value that contains plotly clicks
    clicked <- reactiveVal(NULL)
    
    observeEvent(
     event_data("plotly_click"), {
       key <- event_data("plotly_click")[1, 4]
       
       clicked_cat <- as.character(bar_data()$category[bar_data()$key == key])
       
       clicked(c(clicked(), clicked_cat))
     }
    )
    
    #Reset the reactive value when reset plot is clicked 
    
    observeEvent(
      input$reset, {
        #reset clicked to empty string when reset button clicked
        clicked(NULL)
      }
    )
    
    ##Set up line plot and heatmap 
    
    ##Filter data to selected categories in bar chart 
    selected_cats <- reactive({
      filtered_series() %>% 
         filter(category %in% clicked()) #%>% 
        # mutate(
        #   category = str_wrap(category, width = 15)
        #   )
        
    })
    
    output$line <- renderPlotly({
      
      if(is.null(clicked())){
        gg <- ggplot() 
        ggplotly(gg)
      } else {
        gg <- ggplot(
          data = selected_cats(),
          aes(
            x = date,
            y = value, 
            fill = category,
            color = category,
            text = label,
            group = 1
          )
        ) + 
          geom_point(size = .5) +
          geom_line(size = .4) +
          scale_x_date(
            breaks = seq.Date(from = min(selected_cats()$date),
                              to = max(selected_cats()$date) + lubridate::years(1),
                              by = "12 months"),
            date_labels = "%b\n%Y") +
          scale_color_manual(name = "category",
                             values = palette) +
          scale_fill_manual(name = "category", 
                            values = palette)  +
          labs(x = "Date",
               y = "Value") + 
          theme_classic() 
        
        ggplotly(gg,
                 tooltip = "text"
                 ) %>%
          layout(
            xaxis = list(rangeslider = list(visible = T)),
            legend=list(title=list(text=' '))
          )
      }
      
      
      
    })
    
    
    
  }
  
  shinyApp(ui, server)
  
}