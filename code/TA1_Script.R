#CARGAR DATOS
hotel_bookings <- read.csv("../data/hotel_bookings_miss.csv", header=TRUE, stringsAsFactors=FALSE, sep=";")


#INSPECCIONAR DATOS

#Operaciones para visualizar datos
names(hotel_bookings)
View(hotel_bookings)
str(hotel_bookings)
summary(hotel_bookings)

#Función para calcular los valores NA de cada columna del Dataframe
sin_valor <- function(x) {
  sum = 0
  for(i in 1:ncol(x)) {
    sum = sum + colSums(is.na(x[i]))
  }
  cat("Existen", sum, "valores NA")
}

#Función para calcular los valores en blanco de cada columna del Dataframe
en_blanco <- function(x) {
  sum = 0
  for(i in 1:ncol(x)) {
    sum = sum + colSums(x[i]=="")
  }
  cat("Existen", sum, "valores en blanco")
}


#Observamos la cantidad de valores NA y en blanco dentro de todo el dataframe
sin_valor(hotel_bookings)
en_blanco(hotel_bookings)
sapply(hotel_bookings,anyNA)

#PRE-PROCESAMIENTO
library(ggplot2)

hotel_bookings_clear <- na.omit(hotel_bookings)    #Omitimos los valores NA del dataframe

#Realizamos una cuenta de las filas duplicada dentro de todo el dataframe
nrow(hotel_bookings_clear[duplicated(hotel_bookings_clear),])

hotel_bookings_clear <- unique(hotel_bookings_clear)           #Nos deshacemos de los valores duplicados


#Funcione utilizadas para reemplazar los valores atípicos
reemplazar_atipicos <- function(data){
  quantiles <- quantile(data,c(0.04,0.96))
  data[data < quantiles[1]] <- mean(data)
  data[data > quantiles[2]] <- median(data)
  data         
}

reemplazar_atipicos2 <- function(data, max_value, min_value) {
  data[data < min_value] <- mean(data)
  data[data > max_value] <- median(data)
  data
}

#Proceso de eliminación de valores atípicos

#lead_time
lead_time <- reemplazar_atipicos(hotel_bookings_clear$lead_time)

par(mfrow = c(1,2))
boxplot(hotel_bookings_clear$lead_time, main = "lead_time con valores atipicos")
boxplot(lead_time, main = " lead_time sin valores at?ipicos")


#stays_in_weekend_nights
stays_in_weekend_nights <- reemplazar_atipicos(hotel_bookings_clear$stays_in_weekend_nights)

par(mfrow = c(1,2))
boxplot(hotel_bookings_clear$stays_in_weekend_nights, main = "stays_in_weekend_nights con valores atipicos")
boxplot(stays_in_weekend_nights, main = " stays_in_weekend_nights sin valores atipicos")


#stays_in_week_nights
stays_in_week_nights <- reemplazar_atipicos(hotel_bookings_clear$stays_in_week_nights)

par(mfrow = c(1,2))
boxplot(hotel_bookings_clear$stays_in_week_nights, main = "stays_in_week_nights con valores atipicos")
boxplot(stays_in_week_nights, main = " stays_in_week_nights sin valores atipicos")


#adults
adults <- reemplazar_atipicos2(hotel_bookings_clear$adults,10,1)

par(mfrow = c(1,2))
boxplot(hotel_bookings_clear$adults, main = "adults con valores atipicos")
boxplot(adults, main = " adults sin valores atipicos")


#children
children <- reemplazar_atipicos2(hotel_bookings_clear$children, 3, 0)

par(mfrow = c(1, 2))
boxplot(hotel_bookings_clear$children, main = "children con valores atipicos")
boxplot(children, main = " children sin valores atipicos")


#babies
babies <- reemplazar_atipicos2(hotel_bookings_clear$babies, 3, 0)

par(mfrow = c(1,2))
boxplot(hotel_bookings_clear$babies, main = "babies con valores atipicos")
boxplot(babies, main = " babies sin valores atipicos")


#previous_cancellations
previous_cancellations <- reemplazar_atipicos2(hotel_bookings_clear$previous_cancellations, 5, 0)

par(mfrow = c(1,2))
boxplot(hotel_bookings_clear$previous_cancellations, main = "previous_cancellations con valores atipicos")
boxplot(previous_cancellations, main = " previous_cancellations sin valores atipicos")


#previous_bookings_not_canceled
previous_bookings_not_canceled <- reemplazar_atipicos(hotel_bookings_clear$previous_bookings_not_canceled)

par(mfrow = c(1,2))
boxplot(hotel_bookings_clear$previous_bookings_not_canceled, main = "previous_bookings_not_canceled con valores atipicos")
boxplot(previous_bookings_not_canceled, main = " previous_bookings_not_canceled sin valores atipicos")


#booking_changes
booking_changes <- reemplazar_atipicos(hotel_bookings_clear$booking_changes)

par(mfrow = c(1,2))
boxplot(hotel_bookings_clear$booking_changes, main = "booking_changes con valores atipicos")
boxplot(booking_changes, main = " booking_changes sin valores atipicos")


#days_in_waiting_list
days_in_waiting_list <- reemplazar_atipicos2(hotel_bookings_clear$days_in_waiting_list, 60, 0)

par(mfrow = c(1,2))
boxplot(hotel_bookings_clear$days_in_waiting_list, main = "days_in_waiting_list con valores atipicos")
boxplot(days_in_waiting_list, main = " days_in_waiting_list sin valores atipicos")


#adr
adr <- reemplazar_atipicos(hotel_bookings_clear$adr)

par(mfrow = c(1,2))
boxplot(hotel_bookings_clear$adr, main = "adr con valores atipicos")
boxplot(adr, main = " adr sin valores atipicos")


#required_car_parking_spaces
required_car_parking_spaces <- reemplazar_atipicos2(hotel_bookings_clear$required_car_parking_spaces,3,0)

par(mfrow = c(1,2))
boxplot(hotel_bookings_clear$required_car_parking_spaces, main = "required_car_parking_spaces con valores atipicos")
boxplot(required_car_parking_spaces, main = " required_car_parking_spaces sin valores atipicos")


#Reemplazo de datos en el dataset limpio con observaciones sin valores atípicos

hotel_bookings_clear$lead_time <- lead_time
hotel_bookings_clear$stays_in_weekend_nights <- stays_in_weekend_nights
hotel_bookings_clear$stays_in_week_nights <- stays_in_week_nights
hotel_bookings_clear$adults <- adults
hotel_bookings_clear$children <- children
hotel_bookings_clear$babies <- babies
hotel_bookings_clear$previous_cancellations <- previous_cancellations
hotel_bookings_clear$previous_bookings_not_canceled <- previous_bookings_not_canceled
hotel_bookings_clear$booking_changes <- booking_changes
hotel_bookings_clear$days_in_waiting_list <- days_in_waiting_list
hotel_bookings_clear$adr <- adr
hotel_bookings_clear$required_car_parking_spaces <- required_car_parking_spaces


#Creación del archivo csv con el dataset pre-procesado y listo para su análisis
write.csv(hotel_bookings_clear, "../data/hotel_bookings_clear.csv", na = "NA", row.names = FALSE)
