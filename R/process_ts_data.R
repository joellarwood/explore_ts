#' Process data for ts_explore app 
#' 
#' Create the nescessary columns for a dataframe to be passed to ts_explore app. 
#' Input data must be a tidy dataframe. The dataframe may contain multiple types 
#' of timeseries (i.e. seasonally adjusted, original, and trend) but can only 
#' contain one type of measurement (i.e. the actual value or index)
#' 
#' @param data A data frame containing columns indicating series type, value, date, and category
#' @param value Name of column containing the observed value to be plotted
#' @param category Name of column containing the category the observation belongs to
#' @param series Name of column containing the series type (i.e. original, seasonally adjusted). If all data are from same series type then a column must be still be present in the data (i.e. inidicating all data are from original series)  
#' @param date Name of column containing date of observation 
#' @param 
#' @param frequency A character vector indicating the frequency of collection. One of "month" or "quarter", "week" or "fortnight"
#' 

process_ts_data <- function(data, value, category, series, date, frequency){
  
  if(!(frequency %in% c("month", "quarter", "week", "fortnight"))){
    stop("Frequency must be one of 'month' or 'quarter'")
  }
  
  #Only accept 16 categories or fewer 
  if(length(unique(data$category)) > 15){
    stop("No more than 16 categories can be handled by the app at this point")
  }
  
  
  #Set lag 
  year_lag <- dplyr::case_when(
    frequency == "month" ~ 12,
    frequency == "quarter" ~ 4,
    frequency == "week" ~ 52,
    frequency == "fortnight" ~ 26
  )
  
  # Select, Group and create change measures 
  
  add_change <- data %>% 
    #select columns
    #this will also return columns in this order so positional selection can be used 
    #in the app 
    select(
      {{ category}},
      {{ series }}, 
      {{ date }},
      {{ value }}
    ) %>% 
    #set grouping strcuture
    dplyr::group_by(
      {{ category}},
      {{ series }}, 
    ) %>% 
    #arrange by date 
    dplyr::arrange({{ date }}) %>% 
    #create change measures 
    dplyr::mutate(
      prd_chg = (value - dplyr::lag(value, 1))/dplyr::lag(value, 1) * 100,
      year_chg = (value - dplyr::lag(value, year_lag))/dplyr::lag(value, year_lag) * 100
    ) %>% 
    #set uniform names in returned data 
    dplyr::rename(
      category = {{ category }},
      series_type = {{ series }},
      value = {{ value }}
    ) %>% 
    #arrange by category for consistent color treatment 
    #in linked plots 
    arrange(category) %>% 
    #remove grouping structure 
    ungroup() %>% 
    #Add label for hopver 
    mutate(
      label = paste0("<b>", category, "</b><br>",
                    format.Date(date, "%b %Y"), "<br>",
                    "Value: ", value, "<br>",
                    "% Change (previous period): ", round(prd_chg, 2), "% <br>",
                    "% Change (previous year): ", round(year_chg, 2), "% <br>")
    )
  
  
  
  #Treat category as a order factor for plotting consistency
  
  
  
  #Add the palette to the unique categories 
  # cat_pal <- data.frame(
  #   category = unique(add_change$category),
  #   color = palette[1:length(unique(add_change$category))]
  # )
  # 
  # #add colour column to data 
  # 
  # color_added <- left_join(
  #   add_change, 
  #   cat_pal
  # )
  
  
  return(add_change)  
  
}

