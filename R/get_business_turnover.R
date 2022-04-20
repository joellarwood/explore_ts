#' Get Business Turnover 
#'
#' Returns a tidy dataframe of business turnover from https://www.abs.gov.au/statistics/economy/business-indicators/monthly-business-turnover-indicator/latest-release#data-download. readabs::read_abs_local used to handle data tidying 
#' 
#' 

get_business_turnover <- function(){
  
  #set temp file
  tmp_file <- tempfile()
  
  #download all data to the temp file
  download.file(
    url = "https://www.abs.gov.au/statistics/economy/business-indicators/monthly-business-turnover-indicator/sep-2021/56810_All_time_series_spreadsheets.zip",
    destfile = tmp_file
  )

  #Unzip file to temp dir 
  tmp_dir <- tempdir()
  
  unzip(tmp_file, exdir = tmp_dir)
  
  #read in data using readabs
  stp_turnover <- readabs::read_abs_local(
    path = tmp_dir
  ) 
  
  #make a column identifying the industry 
  stp_turnover$industry <- stringr::str_extract(
    stp_turnover$series,
    pattern = ";\\s(.*)\\s; "
  )
  
  #remove leftover whitespace and ; 
  stp_turnover$industry <- gsub(
    pattern = "\\s*;\\s*",
    replacement = "",
    stp_turnover$industry
  )
  
  return(stp_turnover)
  
  }