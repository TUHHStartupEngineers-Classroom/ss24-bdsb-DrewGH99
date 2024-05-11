#Intro Challenge 1 ----

library("magrittr")

library(readxl)

library(readr)

bikes_tbl      <- read_excel(path = "00_data/01_raw_data/01_bike_sales/01_raw_data/bikes.xlsx")
orderlines_tbl <- read_excel("00_data/01_raw_data/01_bike_sales/01_raw_data/orderlines.xlsx")


bikeshops_tbl  <- read_excel("00_data/01_raw_data/01_bike_sales/01_raw_data/bikeshops.xlsx")

left_join(orderlines_tbl, bikes_tbl, by = c("product.id" = "bike.id"))

# Chaining commands with the pipe and assigning it to order_items_joined_tbl
bike_orderlines_joined_tbl <- orderlines_tbl %>%
  left_join(bikes_tbl, by = c("product.id" = "bike.id")) %>%
  left_join(bikeshops_tbl, by = c("customer.id" = "bikeshop.id"))

bike_orderlines_wrangled_tbl <- bike_orderlines_joined_tbl %>%
  # 5.1 Separate category name
  separate(col    = category,
           into   = c("category.1", "category.2", "category.3"),
           sep    = " - ") %>%
  
  # 5.2 Add the total price (price * quantity) 
  # Add a column to a tibble that uses a formula-style calculation of other columns
  mutate(total.price = price * quantity) %>%
  
  # 5.3 Optional: Reorganize. Using select to grab or remove unnecessary columns
  # 5.3.1 by exact column name
  select(-...1, -gender) %>%
  
  # 5.3.2 by a pattern
  # You can use the select_helpers to define patterns. 
  # Type ?ends_with and click on Select helpers in the documentation
  select(-ends_with(".id")) %>%
  
  # 5.3.3 Actually we need the column "order.id". Let's bind it back to the data
  bind_cols(bike_orderlines_joined_tbl %>% select(order.id)) %>% 
  
  # 5.3.4 You can reorder the data by selecting the columns in your desired order.
  # You can use select_helpers like contains() or everything()
  select(order.id, contains("order"), contains("model"), contains("category"),
         price, quantity, total.price,
         everything()) %>%
  
  # 5.4 Rename columns because we actually wanted underscores instead of the dots
  # (one at the time vs. multiple at once)
  rename(bikeshop = name) %>%
  set_names(names(.) %>% str_replace_all("\\.", "_"))



#create variable for table
sales_by_location_tbl <- bike_orderlines_wrangled_tbl %>%
  
  # separate state and city variables into different columns
  separate(col   = location,
           into  = c("city", "state"),
           sep   = ", ") %>%
 
  #select columns
  select(state, city, total_price) %>%
  
  #Group data by state and sum price data into the 'sales' variable
    group_by(state) %>% 
      summarize(sales = sum(total_price)) %>%

#add column that formats sales data as a currency
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                   decimal.mark = ",", 
                                   prefix = "", 
                                   suffix = " €")) %>%
  
  #Create canvas with x-axis and y-axis values
  ggplot(aes(x = state, y = sales)) +
  
  #Create bar plot with color fill
  geom_col(fill = "#9EB6F3") + 
  
  #Add labels to bars
  geom_label(aes(label = sales_text)) + 
  
  #Scale y-axis
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  
  #Modify labels and plot title
  labs(
    title    = "Revenue by Location",
    subtitle = "",
    x = "", 
    y = "Revenue"
  ) +

  #rotate axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#display plot
sales_by_location_tbl



#intro challenge 2 ----

#create variable for table
sales_by_location_and_year_tbl <- bike_orderlines_wrangled_tbl %>%
  
  # separate state and city
  separate(col   = location,
           into  = c("city", "state"),
           sep   = ", ") %>%
  
  select(state, city, total_price, order_date) %>% ##select columns
  mutate(year = year(order_date)) %>% #add column for the year that takes the year value from the order date
  group_by(year, state) %>% #group by year and state
  summarize(sales = sum(total_price)) %>% #sum sales for each year and state
  ungroup() %>% #ungroup

    mutate(sales_text = scales::dollar(sales, big.mark = ".", #add column that formats sales data as a currency
                                       decimal.mark = ",", 
                                       prefix = "", 
                                       suffix = " €")) %>%
  
  ggplot(aes(x = year, y = sales, fill = state)) + #Create canvas with x-axis, y-axis, and fill values
  
  geom_col() + #create bar plot
  
  geom_smooth(method = "lm", se = FALSE) + # Add a trendline
  
  facet_wrap(~ state) + #use face wrap to display multiple plots based on state
  
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".",    ##Scale y-axis
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  labs(
    title = "Sales by year and location",
    subtitle = "",
    fill = "Location" 
  )

sales_by_location_and_year_tbl #display plot