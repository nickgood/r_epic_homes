# Libraries
library(httr)
library(jsonlite)
library(dplyr)
library(purrr)
library(tidyr)
library(openair)
library(ggplot2)
library(lubridate)
library(readr)
library(stringr)
library(googlesheets4)
library(hrbrthemes)
library(kableExtra)
library(pracma)
library(padr)

#_______________________________________________________________________________
# clean names
clean_names <- function(x){
  out <- gsub(" ", "_", tolower(x))
  out <- gsub("\\(|\\)", "", out)
  out
}
#_______________________________________________________________________________

#_______________________________________________________________________________

#_______________________________________________________________________________
# read 5 minute data

d_5min <- function(date_start, date_end,
                   tkn = "",
                   org_id = "2059",
                   device_type = "awair-omni",
                   device_id = "11431", ...){
# pause
 Sys.sleep(2)
# url  
  url <- paste0("http://developer-apis.awair.is/v1/orgs/",
                org_id, "/",
                "devices/", device_type, "/", device_id, "/",
                "air-data/5-min-avg?",
                "from=", date_start,
                "&to=", date_end)
# GET data
  result <- GET(url, add_headers(Authorization = paste("Bearer", tkn, sep = " "))) %>%
    content() %>%
    as_tibble()

if(length(result$data) > 0){
# clean data
  out <- as_tibble(result) %>%
  unnest_wider(data) %>%
  select(-indices) %>%
  mutate(df = map(sensors, ~ .x %>% map_df(magrittr::extract, c("comp", "value")))) %>%
  unnest(df) %>%
  select(-sensors) %>%
  pivot_wider(names_from = "comp", values_from = "value") %>%
  mutate(datetime_local = format(as.POSIXct(strptime(timestamp,format = "%FT%H:%M:%S",tz = "GMT")),
                           tz = "US/Mountain",
                           usetz = TRUE))
}else{NA}

}
#_______________________________________________________________________________

#_______________________________________________________________________________
# upas time to posixct
upas_to_datetime <- function(x){
  as.POSIXct(strptime(sub("T"," ", x),
             format = "%Y-%m-%d %T",
             tz = "US/Mountain"))
}
#_______________________________________________________________________________

#_______________________________________________________________________________
bc_calc <- function(ref, sample, flow, area, dur){
  # area = m^2
  # flow = L/min
  # dur = seconds
  q <- flow / (60*1000)   # L/min to m^3/s
  rho_atn <- 12.5         # m^2/g
  d_atn = 100 * log(ref / sample)
  b_atn = (area * d_atn) / (q * dur)
  vol = q * dur 
  r = exp(-d_atn / 100) * 0.88 + 0.12
  bc_conc = 1e4 * b_atn / (rho_atn * r)
  bc_conc
}
#_______________________________________________________________________________

#_______________________________________________________________________________
read_omni_rds_file <- function(file){
  f <- read_rds(file)
  if(length(f$data) > 0){
    as_tibble(f) %>%
      unnest_wider(data) %>%
      select(-indices) %>%
      mutate(df = map(sensors, ~ .x %>% map_df(magrittr::extract, c("comp", "value")))) %>%
      unnest(df) %>%
      select(-sensors, -score) %>%
      pivot_wider(names_from = "comp", values_from = "value") %>%
      mutate(datetime = format(as.POSIXct(strptime(timestamp,format = "%FT%H:%M:%S",tz = "GMT")),
                               tz = "US/Mountain",
                               usetz = TRUE)) %>%
      mutate(filename = file)
  }else{
    tibble(timestamp = NA_character_,
           co2 = NA_real_,
           voc = NA_real_,
           temp = NA_real_,
           lux = NA_real_,
           humid = NA_real_,
           pm25 = NA_real_,
           score = NA_real_,
           spl_a  = NA_real_,    
           datetime = NA_character_,
           filename = file)
  }
}
#_______________________________________________________________________________

#_______________________________________________________________________________
read_omni_rds_files <- function(files){
  map(files, read_omni_rds_file) %>%
    bind_rows()
}
#_______________________________________________________________________________

#_______________________________________________________________________________

pm25_mean <- function(start, end, df){
  df %>% filter(datetime >= start & datetime <= end) %>%
    pull(pm25) %>%
    mean(., na.rm = TRUE)
}
#_______________________________________________________________________________


##______________________________________________________________________________
## filter sense data by job id and device name
filter_sense <- function(x, id, device_name){
  x %>% 
    filter(sense_device_name == device_name) %>%
    filter(job_id == id)
}
##______________________________________________________________________________

##______________________________________________________________________________
## pad sense time series @ 1 minute
pad_sense <- function(x){
  x %>% 
    select(datetime, avg_wattage, consumption_kwh) %>%
    pad(interval = "min") %>%
    mutate(avg_wattage = if_else(is.na(avg_wattage), 0, avg_wattage),
           consumption_kwh = if_else(is.na(consumption_kwh), 0, consumption_kwh))
}
##______________________________________________________________________________

##______________________________________________________________________________
## find peaks
findpeaks_sense <- function(x){
  # find peaks in avg wattage
  peaks_w <- findpeaks(x %>% pull(avg_wattage), 
                       nups = 2, ndowns = 2,
                       zero = "+",
                       minpeakheight = -Inf, minpeakdistance = 1,
                       threshold = 0, npeaks = 0, sortstr = FALSE) %>%
    as_tibble() %>%
    rename(peak_height = "V1", row = "V2") %>%
    select(peak_height, row)
}
##______________________________________________________________________________