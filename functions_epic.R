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
#library(kableExtra)
library(pracma)
library(padr)
library(magrittr)
library(gridExtra)
library(tufte)

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
      mutate(filename = file)
  }else{
    tibble(timestamp = NA_character_,
           co2 = NA_real_,
           voc = NA_real_,
           temp = NA_real_,
           lux = NA_real_,
           humid = NA_real_,
           pm25 = NA_real_,
           pm10_est = NA_real_,
           score = NA_real_,
           spl_a  = NA_real_,
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
## load 5 min omni data
get_5min <- function(tkn, org_id, device_id, start, end, home_id = "NA", location = "NA"){
  # pause
  Sys.sleep(1)
  # build url
  url <- paste0("https://developer-apis.awair.is/v1/orgs/",
                org_id,
                "/devices/awair-omni/",
                device_id,
                "/air-data/5-min-avg?from=",
                start,
                "&to=",
                end)
  # get data
  data <- content(GET(url, add_headers("X-Api-Key" = tkn)))
  # write to file
  file_name <- paste("../output/omni_raw_5min/", home_id, "/", 
                     org_id, "_", device_id, "_",
                     home_id, "_", location, "_",
                     start, "_", end, sep = "")
  write_rds(data, paste0(file_name, ".rds"))
  # print url
  print(url)
  # return url
  url
}
##______________________________________________________________________________

##______________________________________________________________________________
## load 15 min omni data
get_15min <- function(tkn, org_id, device_id, start, end, home_id = "NA", location = "NA"){
  # pause
  Sys.sleep(6)
  # build url
  url <- paste0("https://developer-apis.awair.is/v1/orgs/",
                org_id,
                "/devices/awair-omni/",
                device_id,
                "/air-data/15-min-avg?from=",
                start,
                "&to=",
                end)
  # get data
  data <- content(GET(url, add_headers("X-Api-Key" = tkn)))
  # write to file
  file_name <- paste("../output/omni_raw_15min/", home_id, "/", 
                     org_id, "_", device_id, "_",
                     home_id, "_", location, "_",
                     start, "_", end, sep = "")
  write_rds(data, paste0(file_name, ".rds"))
  # print url
  print(url)
  # return url
  url
  #
}
##______________________________________________________________________________

##______________________________________________________________________________
## clean omni data file
clean_omni_data <- function(x){
  if(length(x$data > 0)){
    data_long <- as_tibble(x) %>%
      unnest_wider(data) %>%
      select(-indices) %>%
      mutate(df = map(sensors, ~ .x %>% map_df(magrittr::extract, c("comp", "value")))) %>%
      unnest(df) %>%
      select(-sensors, -score) %>%
      pivot_wider(names_from = "comp", values_from = "value") %>%
      mutate(datetime = format(as.POSIXct(strptime(timestamp,format = "%FT%H:%M:%S",tz = "GMT")),
                               tz = "US/Mountain",
                               usetz = TRUE))
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
           filename = NA_character_)
  }
}
##______________________________________________________________________________

##______________________________________________________________________________
## get 15 min files
get_15min_files <- function(id_home){
  list.files(paste0("../output/omni_raw_15min/", id_home), full.names = TRUE)
}
##______________________________________________________________________________
## summarize omni
summarise_omni_1hour <- function(data){
  data %>% filter(!is.na(timestamp)) %>%
    mutate(datetime = format(as.POSIXct(strptime(timestamp,
                             format = "%FT%H:%M:%S.000Z",tz = "GMT")),
                             tz = "US/Mountain",
                             usetz = TRUE),
           filename = basename(filename),
           date = as.Date(datetime),
           hour = hour(datetime),
           location = sub("^.*_.*_.*_(.*)_.*_.*.rds$", "\\1", filename)) %>%
    distinct() %>%
    select(location, datetime, co2, humid, lux, pm25, pm10_est,
           spl_a, temp, voc, date, hour, timestamp, filename) %>%
    group_by(location, date, hour) %>%
    summarise(datetime = min(datetime),
              co2 = mean(co2, na.rm =TRUE), 
              humid = mean(humid, na.rm =TRUE), 
              lux = mean(lux, na.rm =TRUE), 
              pm25 = mean(pm25, na.rm =TRUE), 
              pm10_est = mean(pm10_est, na.rm =TRUE), 
              spl_a = mean(spl_a, na.rm =TRUE),
              temp = mean(temp, na.rm = TRUE),
              voc = mean(voc, na.rm =TRUE), .groups = "drop") %>%
    select(-hour)
}
##______________________________________________________________________________

##______________________________________________________________________________
## month to season
# calculates meteorological season from month of year
month_to_season <- function(x){
  season <- NA_character_
  ifelse(x == 3 | x == 4 | x == 5, season <- "spring", season)
  ifelse(x == 6 | x == 7 | x == 8, season <- "summer", season)
  ifelse(x == 9 | x == 10 | x == 11, season <- "fall", season)
  ifelse(x == 1 | x == 2 | x == 12, season <- "winter", season)
  return(season)
}
##______________________________________________________________________________

##______________________________________________________________________________
## hours of season
hours_of_season <- function(sn, date_start, date_end){
  tibble(datetime = seq(as.POSIXct(date_start, tz = "US/Mountain"),
                             as.POSIXct(date_end, tz = "US/Mountain"),
                             by = 60 * 60)) %>%
    mutate(season = factor(quarter(datetime, fiscal_start = 3),
                           levels = c(seq(1, 4, 1)),
                           labels = c("spring", "summer", "fall", "winter"))) %>%
    group_by(season) %>%
    count() %>%
    right_join(tibble(season = c("spring", "summer", "fall", "winter")), by = "season") %>%
    mutate(n = if_else(is.na(n), as.integer(0), n)) %>%
    filter(season == sn) %>%
    pull(n)
}
##______________________________________________________________________________

