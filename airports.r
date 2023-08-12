library(dplyr)
library(ggplot2)

#---------------2ai---------------

# Leximi i CSV file airports.csv
df <- read.csv("airports.csv")

# Shfaqja e head dhe tail te dataframe-it
head(df)
tail(df)

# Filtrimi i te dhenave duke perfshire vetem aeroportet ne USA dhe numerimi i aeroporteve per secilin shtet
num_airports <- df %>%
  filter(country == "USA") %>%
  count(state, name = "NumAirports") %>%
  arrange(desc(NumAirports)) %>%
  top_n(10)

# Krijimi i bar plot
ggplot(num_airports, aes(x = reorder(state, -NumAirports), y = NumAirports)) +
  geom_bar(stat = "identity", fill = "red", width = 0.5) +
  geom_text(aes(label = NumAirports), vjust = -0.5) +
  labs(title = "Numri i aeroporteve per top 10 shtetet ne USA",
       x = "Shteti",
       y = "Numri i aeroporteve") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

#---------------2aii---------------

#Filtrojme te dhenat duke perfshire vetem ato ku shteti eshte NY dhe atributi iata nuk eshte null
filtered_df <- df %>%
  filter(state == "NY" & !is.na(iata))

#Ndajme vetem vlerat unike te iata
distinct_iata <- filtered_df %>%
  distinct(iata)

#Numerojme keto te dhena
ny_commercial_airports <- distinct_iata %>%
  count()
print(ny_commercial_airports)

#---------------2aiii---------------

#Vendosim inputin per perdoruesin
state_input <- readline("Sheno emrin e shtetit: ")

#Marrim te dhenat per shtetin te cilin e ka vendosur perdoruesi
find_commercial_airports <- function(df, state_input) {
  
  filtered_df <- df %>%
    filter(state == state_input & !is.na(iata))
  
  commercial_airports <- filtered_df %>%
    distinct(iata, .keep_all = TRUE)
  return(commercial_airports)
}

#Printojme emrat e aeroporteve
result <- find_commercial_airports(df, state_input)
print(result$airport)

#---------------2b---------------

# Leximi i CSV file 1990.csv dhe caktimi i saj te nje variabel
csv_file <- "1990.csv"
flight_df <- read.csv(csv_file)
head(flight_df)

#Filtrojme te dhenat duke i marre ato qe vleren diverted e kane 0 dhe i grupojme sipas dyshes origjine dhe destinacion
grouped_flights <- flight_df %>%
  filter(Diverted == 0, Cancelled==0) %>%
  group_by(Origin, Dest) %>%
  summarise(count = n())

#Shfaqim numrin e linjave te mundshme
print(nrow(grouped_flights))

#Rendisim te dhenat ne rend rrites dhe shaqim top 5 sipas numrit te fluturimeve
flights_sorted <- grouped_flights %>% arrange(desc(count))
top_5_flights <- head(flights_sorted, 5)
print(top_5_flights)

#---------------3a---------------
# Leximi i CSV file 1995.csv dhe caktimi i saj te nje variabel
csv_1995 <- "1995.csv"
flight_df_ <- read.csv(csv_1995)

#Filtrojme te dhenat duke i larguar ato qe e kane emrin e aeroplanit Unknown (te panjohur)
grouped_airplane_distance <- flight_df_ %>%
  filter(TailNum != "UNKNOW") %>%
  #Llogaritim distancen e fluturimit per sipas aeroportit
  group_by(TailNum) %>%
  summarize(TotalDistance = sum(Distance, na.rm = TRUE)) %>%
  arrange(desc(TotalDistance))

#Printojme rezultatet e fituara
print(grouped_airplane_distance)
grouped_airplane_distance_top_10 <- grouped_airplane_distance %>% top_n(10)

#Krijojme nje bar plot per top 10 aeroplanet sipas distancave te fluturimit
ggplot(data = grouped_airplane_distance_top_10, aes(x = reorder(TailNum, -TotalDistance), y = TotalDistance)) +
  geom_bar(stat = "identity", fill = "red", width = 0.5) +
  geom_text(aes(label = TotalDistance), vjust = -0.5) +
  labs(title = "Top 10 Aeroplanet sipas distancave te fluturimit te kaluara",
       x = "Aeroplani",
       y = "Totali i distances") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), plot.title = element_text(hjust = 0.5))  

#Shfaqim top 5 dhe bottom 5 aeroplanet sipas distancave te fluturimit

top_5_tail_num <- head(grouped_airplane_distance$TailNum, 5)
bottom_5_tail_num <- tail(grouped_airplane_distance$TailNum, 5)
print(top_5_tail_num)
print(bottom_5_tail_num)

#---------------3b---------------

# Lexojme "plane-data.csv" file
plane_data <- read.csv("plane-data.csv")

# Filtrojme te dhenat e dataset-it plane_data bazuar ne top 5 aeroplanet me distancat me te gjata
additional_info <- plane_data[plane_data$tailnum %in% top_5_tail_num,]

# Printojme te dhenat shtese per keta 5 aeroplana
message("Informatat shtese per 5 aeroplanet me distancat me te gjata te fluturimit:")
print(additional_info)


#---------------3c---------------

# Filtrojme dataset-in per fluturimet nga "SEA"
sea_flights <- flight_df %>% filter(Origin == "SEA")

# Grupojme datasetin e filtruar sipas airoportit destinues dhe numrit te fluturimeve
flight_counts <- sea_flights %>%
  group_by(Dest) %>%
  summarise(NumFlights = n())

# Filtrojme dataset-in duke marre aeroportet me se paku 10 fluturime
at_least_10_flights <- flight_counts %>% filter(NumFlights >= 10)

# Krijojme nje bar plot
ggplot(at_least_10_flights, aes(x = reorder(Dest, -NumFlights), y = NumFlights)) +
  geom_bar(stat = "identity", fill = "red", width = 0.5) +
  geom_text(aes(label = NumFlights), vjust = 0.5, angle=90, hjust =-0.1) +
  labs(title = "Numri i fluturimeve nga SEA ne aeroporte tjera qe kane pasur se paku 10 fluturime",
       x = "Destinacioni",
       y = "Numri i fluturimeve") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), plot.title = element_text(hjust = 0.5))+  
  ylim(0, 10500)

