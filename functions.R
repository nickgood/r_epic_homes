# load 5-min data for given date

omni_read_5min <- function(date_start, date_end, device_id,
  tkn = "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJ1c2VyX2lkIjoiNjEyNzIifQ.NG3rGE-ANLX110LgLzHVvKns-owboJHtJjCLDndrqWk",
  org_id = "2059",
  device_type = "awair-omni",
  ...){
# pause
 Sys.sleep(2)
# url  
  url <- paste0("http://developer-apis.awair.is/v1/orgs/",
                org_id, "/",
                "devices/", device_type, "/", device_id, "/",
                "air-data/5-min-avg?",
                "from=", date_start,
                "&to=", date_end)
# get data
  result <- GET(url,
    add_headers(Authorization = paste("Bearer", tkn, sep = " "))) %>%
    content() %>%
    as_tibble()

if(length(result$data) > 0){
# clean data
  out <- as_tibble(result) %>%
  unnest_wider(data) %>%
  select(-indices) %>%
  mutate(df = map(sensors, ~ .x %>% 
                  map_df(magrittr::extract, c("comp", "value")))) %>%
  unnest(df) %>%
  select(-sensors) %>%
  pivot_wider(names_from = "comp", values_from = "value") %>%
  mutate(datetime_local = format(as.POSIXct(strptime(timestamp,
                                format = "%FT%H:%M:%S",tz = "GMT")),
                                tz = "US/Mountain",
                                usetz = TRUE))
}else{NA}

}
#_______________________________________________________________________________