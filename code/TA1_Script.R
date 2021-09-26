#CARGAR DATOS
install.packages("openxlsx", dependencies = TRUE)
library(openxlsx)
hotel_bookings <- read.xlsx("data/hotel_bookings_miss.xlsx",1)

#INSPECCIONAR DATOS
View(hotel_bookings)
names(hotel_bookings)
str(hotel_bookings)
summary(hotel_bookings)


#PRE-PROCESAMIENTO
hotel_bookings.clear <- na.omit(hotel_bookings)

library(ggplot2)
library(scales)

outliersReplace <- function(data){
  quantiles <- quantile(data,c(0.04,0.96))
  data[data < quantiles[1]] <- mean(data)
  data[data > quantiles[2]] <- median(data)
  data         
}

#lead_time
lead_time <- outliersReplace(hotel_bookings.clear$lead_time)

par(mfrow = c(1,2))
boxplot(hotel_bookings.clear$lead_time, main = "lead_time Sin reemplazo de outliers con R")
boxplot(lead_time, main = " lead_time Con reemplazo de outliers con R")

# boxplot(hotel_bookings.clear$stays_in_weekend_nights, col = "gray", main = "Lead time")

#stays_in_weekend_nights
stays_in_weekend_nights <- outliersReplace(hotel_bookings.clear$stays_in_weekend_nights)

par(mfrow = c(1,2))
boxplot(hotel_bookings.clear$stays_in_weekend_nights, main = "stays_in_weekend_nights Sin reemplazo de outliers con R")
boxplot(hotel_bookings.stays_in_weekend_nights, main = " stays_in_weekend_nights Con reemplazo de outliers con R")


#stays_in_week_nights
stays_in_week_nights <- outliersReplace(hotel_bookings.clear$stays_in_week_nights)

par(mfrow = c(1,2))
boxplot(hotel_bookings.clear$stays_in_week_nights, main = "stays_in_week_nights Sin reemplazo de outliers con R")
boxplot(stays_in_week_nights, main = " stays_in_week_nights Con reemplazo de outliers con R")

#adults
adults <- outliersReplace(hotel_bookings.clear$adults)

par(mfrow = c(1,2))
boxplot(hotel_bookings.clear$adults, main = "adults Sin reemplazo de outliers con R")
boxplot(adults, main = " adults Con reemplazo de outliers con R")

#children
children <- outliersReplace(hotel_bookings.clear$children)

par(mfrow = c(1,2))
boxplot(hotel_bookings.clear$children, main = "children Sin reemplazo de outliers con R")
boxplot(children, main = " children Con reemplazo de outliers con R")

#babies
babies <- outliersReplace(hotel_bookings.clear$babies)

par(mfrow = c(1,2))
boxplot(hotel_bookings.clear$babies, main = "babies Sin reemplazo de outliers con R")
boxplot(babies, main = " babies Con reemplazo de outliers con R")

#previous_cancellations
previous_cancellations <- outliersReplace(hotel_bookings.clear$previous_cancellations)

par(mfrow = c(1,2))
boxplot(hotel_bookings.clear$previous_cancellations, main = "previous_cancellations Sin reemplazo de outliers con R")
boxplot(previous_cancellations, main = " previous_cancellations Con reemplazo de outliers con R")

#previous_bookings_not_canceled
previous_bookings_not_canceled <- outliersReplace(hotel_bookings.clear$previous_bookings_not_canceled)

par(mfrow = c(1,2))
boxplot(hotel_bookings.clear$previous_bookings_not_canceled, main = "previous_bookings_not_canceled Sin reemplazo de outliers con R")
boxplot(previous_bookings_not_canceled, main = " previous_bookings_not_canceled Con reemplazo de outliers con R")

#booking_changes
booking_changes <- outliersReplace(hotel_bookings.clear$booking_changes)

par(mfrow = c(1,2))
boxplot(hotel_bookings.clear$booking_changes, main = "booking_changes Sin reemplazo de outliers con R")
boxplot(booking_changes, main = " booking_changes Con reemplazo de outliers con R")

#days_in_waiting_list
days_in_waiting_list <- outliersReplace(hotel_bookings.clear$days_in_waiting_list)

par(mfrow = c(1,2))
boxplot(hotel_bookings.clear$days_in_waiting_list, main = "days_in_waiting_list Sin reemplazo de outliers con R")
boxplot(days_in_waiting_list, main = " days_in_waiting_list Con reemplazo de outliers con R")

#adr
adr <- outliersReplace(hotel_bookings.clear$adr)

par(mfrow = c(1,2))
boxplot(hotel_bookings.clear$adr, main = "adr Sin reemplazo de outliers con R")
boxplot(adr, main = " adr Con reemplazo de outliers con R")

#required_car_parking_spaces
required_car_parking_spaces <- outliersReplace(hotel_bookings.clear$required_car_parking_spaces)

par(mfrow = c(1,2))
boxplot(hotel_bookings.clear$required_car_parking_spaces, main = "required_car_parking_spaces Sin reemplazo de outliers con R")
boxplot(required_car_parking_spaces, main = " required_car_parking_spaces Con reemplazo de outliers con R")

#total_of_special_requests
total_of_special_requests <- outliersReplace(hotel_bookings.clear$total_of_special_requests)

par(mfrow = c(1,2))
boxplot(hotel_bookings.clear$total_of_special_requests, main = "total_of_special_requests Sin reemplazo de outliers con R")
boxplot(total_of_special_requests, main = " total_of_special_requests Con reemplazo de outliers con R")
