library(tidyverse)
price <- airbnb_price
room_type <- airbnb_room_type
last_review <- airbnb_last_review

#Total num of observations in each neighbourhood
num_obs_nbhood <- price %>% 
  group_by(nbhood_full) %>% 
  summarise(
    num = n()
  )
num_obs_nbhood

#separate last_review dataset
last_review_separated <- separate(
  data = last_review, 
  col = "listing_id\thost_name\tlast_review",
  into = c("listing_id", "host_name", "last_review") ,
  sep = "\t",
  remove = TRUE,
)

#converting listing_id into numeric
last_review_separated$listing_id <- as.integer(last_review_separated$listing_id)

#combing individual dataset into one
data_joined <- inner_join(last_review_separated,airbnb_price,room_type,by = "listing_id")
final_joined_data <- inner_join(data_joined,room_type, by = "listing_id")
view(final_joined_data)

#formating the price column

final_joined_data <- final_joined_data %>% 
  separate(
    col = price,
    into = c("Price","Curency"),
    sep = " ",
    remove = TRUE
  )
final_joined_data <- final_joined_data %>% 
  select(-Curency)
final_joined_data$Price <- as.numeric(final_joined_data$Price)


# Average price per type of room
df <- final_joined_data %>% 
  select(Price,room_type) %>% 
  group_by(room_type) %>% 
  summarise(
    Average_price = mean(Price)
  )

#prices of rooms vary each day, find the average price of rooms by date of booking
 avg_price_booking <- final_joined_data %>% 
   select(last_review,Price) %>% 
   group_by(last_review) %>% 
   summarise(
     Avg_price_onDay = mean(Price)
   )
#prices of rooms vary each day,
#find the average price of the different type of rooms by date of bookings,
#pivoted wider, where the names names_from argument is set to room_type 
 avg_price_roomtype <- final_joined_data %>% 
   select(room_type,last_review,Price) %>% 
   group_by(room_type,last_review) %>% 
   summarise(
     Avg_price_byroomType = mean(final_joined_data$Price)
   )
 
ds <-  pivot_wider(
   data = avg_price_roomtype,
   names_from = "room_type",
   values_from = "Avg_price_byroomType"
 )

#average price of rooms by type of neighbourhood
avg_price_nbhood <- final_joined_data %>% 
  select(nbhood_full,Price) %>% 
  group_by(nbhood_full) %>% 
  summarise(
    avg_Price_nbhood = mean(final_joined_data$Price)
  )

 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 