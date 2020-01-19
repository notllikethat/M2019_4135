weather <- readRDS(file = "weather.rds")

library(tidyr)

# Numbers of days are stored in X1 - X31 variables, so, firstly we change their names
names(weather)[5:35] <- 1:31

# Then we use `library(tidyr)` to convert our data into more understandable form via gathering it
weather <- gather(weather, day, some_data, 5:35, na.rm = T)

# Data would be more understandable, if we gather dates in one column
weather <- unite(weather, date, year, month, day, sep = '-')
weather$date <- as.Date(weather$date) 

# Then we spread the dataset by all measures
weather <- pivot_wider(weather, id_cols = date, names_from = measure, values_from = some_data)


# We also want to avoid spare spaces and NA's in the columns. To do that we:

# 1. Replace empty spaces in "Events" column with "No event"
weather$Events[weather$Events == ""] <- "No event"

# 2. Replace "T" in PrecipitationIn column with 0.00
weather$PrecipitationIn[weather$PrecipitationIn == "T"] <- replace(weather$PrecipitationIn, 
                                                                   weather$PrecipitationIn == "T", 
                                                                   format(0, nsmall = 2))

# 3. Replace NA's in numeric columns with mean values
for (i in c(2:19, 21, 23)){
  weather[ , i] <- replace(weather[ , i], 
                           which(is.character(weather[ , i])), 
                           as.numeric(weather[ , i], na.rm=TRUE))}

for (i in c(2:19, 21, 23)){
  weather[ , i] <- replace(weather[ , i], 
                           which(is.na(weather[ , i])), 
                           mean(weather[ , i], na.rm=TRUE))}


# Then we change data type to numeric in 'numeric' columns
weather[ , c(2:21, 23)] <- sapply(weather[ , c(2:21, 23)], as.numeric) 

str(weather)
