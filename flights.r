library(dplyr) # një paketë për ta bërë më të lehtë manipulimin e të dhënave
library(ggplot2) # një paketë per grafe për krijimin e plots komplekse

#---------------1ai---------------

# Leximi i CSV file 1990.csv dhe caktimi i saj te nje variabel
csv_file <- "1990.csv"
df <- read.csv(csv_file)

# Shfaqja e top 5 dhe bottom 5 rreshtave te dataframe-it
head(df)
tail(df)

# Numerimi i rreshtave te dataframe-it
num_rows <- nrow(df)
message("Numri i rreshtave eshte: ", num_rows)

# Shfaqja e emrave te kolonave te dataframe-it
column_names <- names(df)
formatted_cols <- paste(column_names, collapse = ", ")
message("Emrat e kolonave te dataframe-it jane: ", formatted_cols)

#---------------1aii---------------

# Kontrollimi i vlerave null apo string te zbrazet ne cdo kolon
null_or_empty_counts <- sapply(df, function(column) {
  sum(is.na(column) | column == "")
})
message("Matrica e vlerave null apo string te zbrazet eshte:")
print(null_or_empty_counts)

#---------------1bi---------------

#Totali i fluturimeve me origjine 'SEA'
sea_flights <- sum(df$Origin == "SEA")
message("Numri i fluturimeve me origjine 'SEA' eshte: ", sea_flights)

#---------------1bii---------------

# Filtrimi i rreshtave me origjine 'SEA' dhe destinacion 'PHX', dhe DepDelay me i madh se 30 min
num_flights <- df %>%
  filter(Origin == "SEA", Dest == "PHX", DepDelay >= 30) %>%
  # Grupimi i numrit te fluturimeve te filtruara sipas muajit
  group_by(Month) %>%
  summarise(NumFlights = n())

# Krijimi i bar plot
ggplot(num_flights, aes(x = Month, y = NumFlights)) +
  geom_bar(stat = "identity", fill = "red", width=0.5) +
  geom_text(aes(label = NumFlights), vjust = -0.5) +
  labs(title = "Numri i fluturimeve nga SEA ne PHX",
       x = "Muaji",
       y = "Numri i fluturimeve") +
  scale_x_continuous(breaks = 1:12, labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

#---------------1ci---------------
  
# Kalkulimi i mesatares se voneses per cdo airoport
avg_dep_delay <- df %>%
  group_by(Origin) %>%
  summarize(AvgDepDelay = mean(DepDelay, na.rm = TRUE))

# Renditja e te dhenave ne rend rrites sipas mesatares se voneses te nisjes
data_sorted <- avg_dep_delay %>% arrange(desc(AvgDepDelay))

# Marrim te dhenat per top 5 vlerat e mesatareve te vonesave te nisjes
top_5_dep_delay <- head(data_sorted$AvgDepDelay, 5)

# Marrim airoportet korresponduese per top 5 e mesiperme
top_5_airports <- data_sorted %>%
  filter(AvgDepDelay %in% top_5_dep_delay) %>%
  select(Origin)

# Printojme top 5 vlerat e mesatareve te vonesave te nisjes dhe airoportet korresponduese te tyre
message("Top 5 vlerat e mesatareve te vonesave te nisjes dhe airoportet korresponduese te tyre jane:")
for (i in 1:5) {
  message(paste("Vonesa mesatare e nisjes:", round(top_5_dep_delay[i], 2), "minuta, Airoporti:", top_5_airports$Origin[i]))
}

#---------------1cii---------------

# Identifikojme rreshtin me vleren me te larte te voneses se ardhjes
max_delay_row <- df[which.max(df$ArrDelay), c("Year", "Month", "DayofMonth", "ArrDelay")]

# Konvertojme numrin e minutave ne ore dhe minuta
minutes <- max_delay_row$ArrDelay
hours <- floor(minutes / 60)
minutes <- minutes %% 60

# Printojme diten me vonesen ne ardhje me te larte
message("Dita me vonesen me te larte ne ardhje eshte: ", max_delay_row$Year,"/", max_delay_row$Month, "/", max_delay_row$DayofMonth, " me nje vonese prej: ", hours, " ore dhe ", minutes, " minuta.")
