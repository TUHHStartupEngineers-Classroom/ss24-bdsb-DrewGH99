library(tidyverse)
library(jsonlite)

resp_cb <- GET("http://api.citybik.es/v2/networks")

resp_cb %>% 
       .$content %>% 
       rawToChar() %>% 
       fromJSON()

resp_cb2 <- GET("http://api.citybik.es/v2/networks/v2/networks/stadtrad-hamburg")

station_data <- resp_cb2 %>% 
  .$content %>% 
  rawToChar() %>% 
  fromJSON()

bike_list <- station_data[["network"]][["stations"]]
bike_list_tbl <- as_tibble(bike_list)

bike_list_tidy <- bike_list_tbl%>%
  select(name, free_bikes, timestamp, latitude, longitude)

#bike_list_tidy %>%
 # mutate(date2 = year(timestamp))
  