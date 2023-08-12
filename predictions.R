# Importojm librarit e nevojshme
library(dplyr) #Build-in pakete
library(ggplot2) #Build-in pakete
library(tidyr) #Duhet me instalu se pari ne R si pakete
library(caret) #Duhet me instalu se pari ne R si pakete

# Lexojme dataset-et per secilin vit (1988 deri 1992)
flight_data_1988 <- read.csv("1988.csv")
flight_data_1989 <- read.csv("1989.csv")
flight_data_1990 <- read.csv("1990.csv")
flight_data_1991 <- read.csv("1991.csv")
flight_data_1992 <- read.csv("1992.csv")
flight_data_1993 <- read.csv("1993.csv")

# I kombinojme te gjithe datasetet ne nje dataset te vetem
flight_data <- rbind(flight_data_1988, flight_data_1989, flight_data_1990, flight_data_1991, flight_data_1992)


#MODELI 1

# Konvertojm informatat per date ne formatin e duhur
flight_data$Date <- as.Date(paste(flight_data$Year, flight_data$Month, flight_data$DayofMonth, sep = "-"))

# Kalkulojme numrin e fluturimeve per dite, duke numeruar vetem fluturimet qe nuk jane anuluar
flight_data <- flight_data %>%
  filter(Cancelled == 0) %>%
  group_by(Date) %>%
  mutate(NumFlights = n())

# I asocojme te dhenat per training set me datasetin flight_data
set.seed(123)  # For reproducibility
training_indices <- flight_data$Year %in% c(1988, 1989, 1990, 1991, 1992)
train_data <- flight_data[training_indices, ]

# Ndertojme modelin e regresionit linear te shumefisht per te parashikuar Numrin e fluturimeve duke perdorur vitin, muajin, diten e muajit dhe diten e javes si ndryshore te pavarura
model <- lm(NumFlights ~ Year + Month + DayofMonth + DayOfWeek, data = train_data)

# Vizualizojme training data dhe modelin
print(ggplot(train_data, aes(x = Date, y = NumFlights)) +
  geom_point(color = "blue", alpha = 0.1) +   # Scatter plot i pikave te training
  geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 1) +   # Drejteza e bazuar ne model
  labs(x = "Data", y = "Numri i fluturimeve", title = "Training Data dhe Modeli") +
  theme_minimal())


# Printojme permbledhjen e modelit
summary(model)

#Pjesa d

test_data <- flight_data_1993

test_data$Date <- as.Date(paste(test_data$Year, test_data$Month, test_data$DayofMonth, sep = "-"))

test_data <- test_data %>%
  filter(Cancelled == 0) %>%
  group_by(Date) %>%
  mutate(NumFlights93 = n())

predictions <- predict(model, newdata = test_data)

rmse <- sqrt(mean((test_data$NumFlights93 - predictions)^2))
mae <- mean(abs(test_data$NumFlights93 - predictions))

print(paste("Root Mean Squared Error (RMSE):", rmse))
print(paste("Mean Absolute Error (MAE):", mae))


ggplot() +
  geom_line(data = test_data, aes(x = Date, y = NumFlights93), color = "blue") +
  geom_line(data = test_data, aes(x = Date, y = predictions), color = "red") +
  labs(x = "Data", y = "Numri i fluturimeve", title = "Numri i fluturimeve reale dhe parashikimet per vitin 1993") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(color = "none")

#Modeli2

library(randomForest)


flight_data_1992$Date <- as.Date(paste(flight_data_1992$Year, flight_data_1992$Month, flight_data_1992$DayofMonth, sep = "-"))

flight_data_1992 <- flight_data_1992 %>%
  filter(Cancelled == 0) %>%
  group_by(Date) %>%
  mutate(NumFlights = n())

set.seed(123) # For reproducibility
train_data <- flight_data_1992 %>%
  sample_frac(0.1)

model2 <- randomForest(NumFlights ~  Month + DayofMonth, data = train_data)

# Printojme permbledhjen e modelit
print(model2)

predictions2 <- predict(model2, newdata = test_data)

rmse <- sqrt(mean((test_data$NumFlights93 - predictions2)^2))
mae <- mean(abs(test_data$NumFlights93 - predictions2))

print(paste("Root Mean Squared Error (RMSE):", rmse))
print(paste("Mean Absolute Error (MAE):", mae))


ggplot() +
  geom_line(data = test_data, aes(x = Date, y = NumFlights93), color = "blue") +
  geom_line(data = test_data, aes(x = Date, y = predictions2), color = "red") +
  labs(x = "Data", y = "Numri i fluturimeve", title = "Numri i fluturimeve reale dhe parashikimet per vitin 1993") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(color = "none")
