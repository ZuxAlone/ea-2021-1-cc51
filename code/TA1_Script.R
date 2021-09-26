#CARGAR DATOS
hotel_bookings <- read.csv("../data/hotel_bookings_miss.csv", header=TRUE, stringsAsFactors=FALSE, sep=";")


#INSPECCIONAR DATOS

#Operaciones para visualizar datos
View(hotel_bookings)
names(hotel_bookings)
str(hotel_bookings)
summary(hotel_bookings)

#Función para calcular los valores NA de cada columna del Dataframe
sin_valor <- function(x) {
  sum = 0
  for(i in 1:ncol(x)) {
    cat("En la columna ", colnames(x[i]), "total de valores en blanco: ", colSums(is.na(x[i])),"\n")
  }
}

#Función para calcular los valores en blanco de cada columna del Dataframe
en_blanco <- function(x) {
  sum = 0
  for(i in 1:ncol(x)) {
    cat("En la columna ", colnames(x[i]), "total de valores en blanco: ", colSums(x[i]==""),"\n")
  }
}


#Observamos la cantidad de valores NA y en blanco dentro de todo el dataframe
sin_valor(hotel_bookings)
en_blanco(hotel_bookings)

#Realizamos una cuenta de las filas duplicada dentro de todo el dataframe
nrow(hotel_bookings[duplicated(hotel_bookings),])


#PRE-PROCESAMIENTO
library(ggplot2)

hotel_bookings_unique <- unique(hotel_bookings)           #Nos deshacemos de los valores duplicados
hotel_bookings_clear <- na.omit(hotel_bookings_unique)    #Omitimos los valores NA del dataframe

reemplazar_atipicos <- function(data){
  quantiles <- quantile(data,c(0.04,0.96))
  data[data < quantiles[1]] <- mean(data)
  data[data > quantiles[2]] <- median(data)
  data         
}

#lead_time
lead_time <- reemplazar_atipicos(hotel_bookings_clear$lead_time)

par(mfrow = c(1,2))
boxplot(hotel_bookings_clear$lead_time, main = "lead_time con valores atípicos")
boxplot(lead_time, main = " lead_time sin valores atípicos")

#boxplot(hotel_bookings_clear$stays_in_weekend_nights, col = "gray", main = "Lead time")

#stays_in_weekend_nights
stays_in_weekend_nights <- reemplazar_atipicos(hotel_bookings_clear$stays_in_weekend_nights)

par(mfrow = c(1,2))
boxplot(hotel_bookings_clear$stays_in_weekend_nights, main = "stays_in_weekend_nights con valores atípicos")
boxplot(stays_in_weekend_nights, main = " stays_in_weekend_nights sin valores atípicos")


#stays_in_week_nights
stays_in_week_nights <- reemplazar_atipicos(hotel_bookings_clear$stays_in_week_nights)

par(mfrow = c(1,2))
boxplot(hotel_bookings_clear$stays_in_week_nights, main = "stays_in_week_nights con valores atípicos")
boxplot(stays_in_week_nights, main = " stays_in_week_nights sin valores atípicos")

#adults
adults <- reemplazar_atipicos(hotel_bookings_clear$adults)

par(mfrow = c(1,2))
boxplot(hotel_bookings_clear$adults, main = "adults con valores atípicos")
boxplot(adults, main = " adults sin valores atípicos")

#children
children <- reemplazar_atipicos(hotel_bookings_clear$children)

par(mfrow = c(1,2))
boxplot(hotel_bookings_clear$children, main = "children con valores atípicos")
boxplot(children, main = " children sin valores atípicos")

#babies
babies <- reemplazar_atipicos(hotel_bookings_clear$babies)

par(mfrow = c(1,2))
boxplot(hotel_bookings_clear$babies, main = "babies con valores atípicos")
boxplot(babies, main = " babies sin valores atípicos")

#previous_cancellations
previous_cancellations <- reemplazar_atipicos(hotel_bookings_clear$previous_cancellations)

par(mfrow = c(1,2))
boxplot(hotel_bookings_clear$previous_cancellations, main = "previous_cancellations con valores atípicos")
boxplot(previous_cancellations, main = " previous_cancellations sin valores atípicos")

#previous_bookings_not_canceled
previous_bookings_not_canceled <- reemplazar_atipicos(hotel_bookings_clear$previous_bookings_not_canceled)

par(mfrow = c(1,2))
boxplot(hotel_bookings_clear$previous_bookings_not_canceled, main = "previous_bookings_not_canceled con valores atípicos")
boxplot(previous_bookings_not_canceled, main = " previous_bookings_not_canceled sin valores atípicos")

#booking_changes
booking_changes <- reemplazar_atipicos(hotel_bookings_clear$booking_changes)

par(mfrow = c(1,2))
boxplot(hotel_bookings_clear$booking_changes, main = "booking_changes con valores atípicos")
boxplot(booking_changes, main = " booking_changes sin valores atípicos")

#days_in_waiting_list
days_in_waiting_list <- reemplazar_atipicos(hotel_bookings_clear$days_in_waiting_list)

par(mfrow = c(1,2))
boxplot(hotel_bookings_clear$days_in_waiting_list, main = "days_in_waiting_list con valores atípicos")
boxplot(days_in_waiting_list, main = " days_in_waiting_list sin valores atípicos")

#adr
adr <- reemplazar_atipicos(hotel_bookings_clear$adr)

par(mfrow = c(1,2))
boxplot(hotel_bookings_clear$adr, main = "adr con valores atípicos")
boxplot(adr, main = " adr sin valores atípicos")

#required_car_parking_spaces
required_car_parking_spaces <- reemplazar_atipicos(hotel_bookings_clear$required_car_parking_spaces)

par(mfrow = c(1,2))
boxplot(hotel_bookings_clear$required_car_parking_spaces, main = "required_car_parking_spaces con valores atípicos")
boxplot(required_car_parking_spaces, main = " required_car_parking_spaces sin valores atípicos")

#total_of_special_requests
total_of_special_requests <- reemplazar_atipicos(hotel_bookings_clear$total_of_special_requests)

par(mfrow = c(1,2))
boxplot(hotel_bookings_clear$total_of_special_requests, main = "total_of_special_requests con valores atípicos")
boxplot(total_of_special_requests, main = " total_of_special_requests sin valores atípicos")
