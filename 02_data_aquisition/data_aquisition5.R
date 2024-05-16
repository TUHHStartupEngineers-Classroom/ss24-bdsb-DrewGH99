
library(rvest)
library(tidyverse)
library(purrr)
library(xopen)
library(knitr)

url_home        <- "https://www.rosebikes.com"
html_home <- read_html(url_home)

#scrape main navigation elements
bike_categories_chr2  <- html_home %>% 
      html_element("main-navigation.header-main-navigation__main-navigation") %>% 
  html_elements("a") %>% 
  html_attr("href")

#create urls out of elements
site_main_urls <- bike_categories_chr2 |> str_c("https://www.rosebikes.com", ... = _)

#access url for bikes
site_category_url <- site_maine_urls[1]
site_category_html <- read_html(site_category_url)

#scrape bike types
bike_cats <- site_category_html %>%
  html_element("catalog-navigation") %>% 
  html_elements("a") %>% 
  html_attr("href")

#create urls for bike types
bike_cats_subcats <- bike_cats |> str_c(url_home, ... = _)

#access urls for bike types
site_subcategory_url <- bike_cats_subcats[2]
site_subcategory_html <- read_html(site_subcategory_url)

#scrape bike sub-types
bike_subcats <- site_subcategory_html %>%
  html_element("catalog-navigation") %>% 
  html_elements("a") %>% 
  html_attr("href")

#create urls for bike sub-types
bike_cats_sub_subcats <- bike_subcats |> str_c(url_home, ... = _)

#access bike sub-types
site_sub_subcategory_url <- bike_cats_sub_subcats[3]
site_sub_subcategory_html <- read_html(site_sub_subcategory_url)

#scrape bike models
model_name <- site_sub_subcategory_html %>% 
  html_elements("h4") %>%
  html_text2()

model_price  <- site_sub_subcategory_html %>%
       html_elements(".catalog-category-bikes__price-title") %>%
   html_text2() |>
  str_remove("from €")

#make tibble and plot
bike_data <- tibble(model   = model_name,
                    price = model_price) %>%
  kable(caption = "Rose Bikes Endurance Model Pricing" )

