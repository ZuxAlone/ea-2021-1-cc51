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



#Parte a
table(hotel_clear$hotel)
barplot(table(hotel_clear$hotel), col = c("green", "yellow"), main="Hoteles de preferencia")


#Parte b
cityHotel_table <- table(hotel_clear$arrival_date_year[hotel_clear$hotel == "City Hotel"])
resortHotel_table <- table(hotel_clear$arrival_date_year[hotel_clear$hotel == "Resort Hotel"])

plot(cityHotel_table, type = "o", col = "red", xlab = "Año", ylab = "Número de reservas",
    main = "Demanda de hoteles a través de los años")
lines(resortHotel_table, type = "o", col = "blue")
legend("topleft", legend=c("City Hotel", "Resort Hotel"), col=c("red", "blue"), lty=1, cex=1)


#Parte c
month_reservas <- c(hotel_clear[5]) #Equivale a la columna "arrival_date_month"
hotel_nombre <- c(hotel_clear[1])

hotel_reservas_mes = data.frame(hotel_nombre,month_reservas)

grafica_hotel_reservas<-ggplot(hotel_reservas_mes, aes(x=factor(arrival_date_month)))+
  geom_bar(aes(fill=factor(hotel)),position = "dodge")

grafica_hotel_reservas

vec_quarts <- c(0, 0, 0)
vec_quarts[1] <- sum(colSums(month_reservas=="January"), colSums(month_reservas=="February"),
                     colSums(month_reservas=="March"), colSums(month_reservas=="April"))
vec_quarts[2] <- sum(colSums(month_reservas=="May"), colSums(month_reservas=="June"),
                     colSums(month_reservas=="July"), colSums(month_reservas=="August"))
vec_quarts[3] <- sum(colSums(month_reservas=="September"), colSums(month_reservas=="October"),
                     colSums(month_reservas=="November"), colSums(month_reservas=="December"))

barplot(vec_quarts, col = c("cadetblue2", "cadetblue3", "cadetblue1"), main="Temporadas de Reserva",
        names = c("Enero-Abril", "Mayo-Agosto", "Septiembre-Diciembre"))


#Parte d
vec_months <- c(colSums(month_reservas=="January"), colSums(month_reservas=="February"),
                colSums(month_reservas=="March"), colSums(month_reservas=="April"),
                colSums(month_reservas=="May"), colSums(month_reservas=="June"),
                colSums(month_reservas=="July"), colSums(month_reservas=="August"),
                colSums(month_reservas=="September"), colSums(month_reservas=="October"),
                colSums(month_reservas=="November"), colSums(month_reservas=="December"))
barplot(vec_months, col = c("chartreuse", "chartreuse1", "chartreuse2", "chartreuse3"),
        main="Cantidad de reservas por mes", names=c("En", "Fe", "Ma", "Ab", "May", "Ju", "Jul", "Ag", "Se", "Oc", "No", "Di"))


#Parte e


#Parte f
needed_parking <- table(hotel_clear$hotel, hotel_clear$required_car_parking_spaces)
barplot(needed_parking, col=c("darkslategray1","brown1"), beside= TRUE,legend = c("City Hotel","Resort Hotel"),
        main = "Numero de reservas por cantidad de parking necesario por tipo de hotel", 
        names= c("0 parking", "1 parking", "2 parking", "3 parking"))


#Parte g
status_reservas <- hotel_clear[31]
status_month <- table(hotel_clear$reservation_status, hotel_clear$arrival_date_month)
vec_cancelled <- c(status_month[1, "January"], status_month[1, "February"], status_month[1, "March"],
                status_month[1, "April"], status_month[1, "May"], status_month[1, "June"],
                status_month[1, "July"], status_month[1, "August"], status_month[1, "September"],
                status_month[1, "October"], status_month[1, "November"], status_month[1, "December"])
barplot(vec_cancelled, col = c("gold", "gold1", "gold2", "gold3"),
        main="Cantidad de reservas canceladas por mes",
        names=c("En", "Fe", "Ma", "Ab", "May", "Ju", "Jul", "Ag", "Se", "Oc", "No", "Di"))
