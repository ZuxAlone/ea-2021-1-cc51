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

hotel_clear <- read.csv("../data/hotel_bookings_clear.csv", header=TRUE, stringsAsFactors=FALSE)


#VISUALIZACIÓN DE DATOS

#Parte a
tablaA <- table(hotel_clear$hotel)
tablaA
barplot(tablaA, col = c("green", "yellow"), ylim = c(0, 60000),
        xlab = "Tipo de Hotel", ylab = "Numero de reservas", main="Hoteles de preferencia")


#Parte b
cityHotel_table <- table(hotel_clear$arrival_date_year[hotel_clear$hotel == "City Hotel"])
resortHotel_table <- table(hotel_clear$arrival_date_year[hotel_clear$hotel == "Resort Hotel"])
cityHotel_table
resortHotel_table
plot(cityHotel_table, type = "o", col = "red", xlab = "Año", ylab = "Número de reservas",
    main = "Demanda de hoteles a través de los años")
lines(resortHotel_table, type = "o", col = "blue")
legend("topleft", legend=c("City Hotel", "Resort Hotel"), col=c("red", "blue"), lty=1, cex=1)


#Parte c
res_hotel_mes <- table(hotel_clear$hotel, hotel_clear$arrival_date_month)
res_hotel_mes
vec_res_city <- c(sum(res_hotel_mes[1,"January"], res_hotel_mes[1,"February"], res_hotel_mes[1,"March"], res_hotel_mes[1,"April"]), 
                  sum(res_hotel_mes[1, "May"], res_hotel_mes[1, "June"], res_hotel_mes[1, "July"], res_hotel_mes[1, "August"]),
                  sum(res_hotel_mes[1, "September"], res_hotel_mes[1, "October"], res_hotel_mes[1, "November"], res_hotel_mes[1, "December"]))
vec_res_reso <- c(sum(res_hotel_mes[2,"January"], res_hotel_mes[2,"February"], res_hotel_mes[2,"March"], res_hotel_mes[2,"April"]), 
                  sum(res_hotel_mes[2, "May"], res_hotel_mes[2, "June"], res_hotel_mes[2, "July"], res_hotel_mes[2, "August"]),
                  sum(res_hotel_mes[2, "September"], res_hotel_mes[2, "October"], res_hotel_mes[2, "November"], res_hotel_mes[2, "December"]))

barplot(matrix(c(vec_res_city, vec_res_reso), nrow=2, byrow=T), col = c("cadetblue3", "cadetblue1"), ylim = c(0, 25000),
        xlab = "Temporadas", ylab = "Número de reservas",legend = c("City Hotel","Resort Hotel"), beside = TRUE, 
        main="Temporadas de reserva por tipo de Hotel", names = c("Enero-Abril", "Mayo-Agosto", "Septiembre-Diciembre"))


#Parte d
vec_mes_city <- c(res_hotel_mes[1,"January"], res_hotel_mes[1,"February"], res_hotel_mes[1,"March"], res_hotel_mes[1,"April"],
                  res_hotel_mes[1,"May"], res_hotel_mes[1,"June"], res_hotel_mes[1,"July"], res_hotel_mes[1,"August"],
                  res_hotel_mes[1,"September"], res_hotel_mes[1,"October"], res_hotel_mes[1,"November"], res_hotel_mes[1,"December"])
vec_mes_reso <- c(res_hotel_mes[2,"January"], res_hotel_mes[2,"February"], res_hotel_mes[2,"March"], res_hotel_mes[2,"April"],
                  res_hotel_mes[2,"May"], res_hotel_mes[2,"June"], res_hotel_mes[2,"July"], res_hotel_mes[2,"August"],
                  res_hotel_mes[2,"September"], res_hotel_mes[2,"October"], res_hotel_mes[2,"November"], res_hotel_mes[2,"December"])

barplot(matrix(c(vec_mes_city, vec_mes_reso), nrow=2, byrow=T), col = c("chartreuse", "chartreuse3"), ylim=c(0,7000),
        xlab = "Meses", ylab ="Número de reservas", legend = c("City Hotel","Resort Hotel"),
        beside = TRUE, main="Cantidad de reservas al mes por tipo de hotel", 
        names=c("En", "Fe", "Ma", "Ab", "May", "Ju", "Jul", "Ag", "Se", "Oc", "No", "Di"))


#Parte e
peque <- table(hotel_clear$hotel, hotel_clear$children, hotel_clear$babies)
peque
vec_peque_city <- c(peque[1,1,1], sum(peque[1,,], -peque[1,1,1]))
vec_peque_reso <- c(peque[2,1,1], sum(peque[2,,], -peque[2,1,1]))

barplot(matrix(c(vec_peque_city, vec_peque_reso), nrow=2, byrow=T), col=c("mediumorchid1","mediumorchid4"), ylim=c(0,50000),
        ylab="Número de reservas", legend=c("City Hotel", "Resort Hotel"), 
        beside=TRUE, main="Reservas con o sin pequeños por tipo de Hotel", names=c("Sin pequeños", "Con pequeños"))


#Parte f
needed_parking <- table(hotel_clear$hotel, hotel_clear$required_car_parking_spaces)
needed_parking
barplot(needed_parking, col=c("darkslategray1","brown1"), beside= TRUE,legend = c("City Hotel","Resort Hotel"), ylim=c(0,55000),
        ylab = "Número de reservas", main = "Numero de reservas por cantidad de parking necesario por tipo de hotel", 
        names= c("0 parking", "1 parking", "2 parking", "3 parking"))


#Parte g
status_mes <- table(hotel_clear$hotel, hotel_clear$reservation_status, hotel_clear$arrival_date_month)
status_mes
vec_can_city <- c(status_mes[1, 1, "January"], status_mes[1, 1, "February"], status_mes[1, 1, "March"],
                  status_mes[1, 1, "April"], status_mes[1, 1, "May"], status_mes[1, 1, "June"],
                  status_mes[1, 1, "July"], status_mes[1, 1, "August"], status_mes[1, 1, "September"],
                  status_mes[1, 1, "October"], status_mes[1, 1, "November"], status_mes[1, 1, "December"])
vec_can_reso <- c(status_mes[2, 1, "January"], status_mes[2, 1, "February"], status_mes[2, 1, "March"],
                  status_mes[2, 1, "April"], status_mes[2, 1, "May"], status_mes[2, 1, "June"],
                  status_mes[2, 1, "July"], status_mes[2, 1, "August"], status_mes[2, 1, "September"],
                  status_mes[2, 1, "October"], status_mes[2, 1, "November"], status_mes[2, 1, "December"])
barplot(matrix(c(vec_can_city, vec_can_reso), nrow=2, byrow=T), col = c("gold", "gold3"), legend = c("City Hotel", "Resort Hotel"),
        xlab = "Meses", ylab = "Número de reservas", beside = TRUE, main="Cantidad de reservas canceladas por mes",
        names=c("En", "Fe", "Ma", "Ab", "May", "Ju", "Jul", "Ag", "Se", "Oc", "No", "Di"))
