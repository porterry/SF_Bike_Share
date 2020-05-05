library(tidyverse)
library(lubridate)
library(caret)
#library(repr)

#Load Data
trip <- read.csv("trip.csv")
weather <- read.csv("weather.csv")
station <- read.csv("station.csv")

##Prepare data
#Filter out customers
#trip <- filter(trip, subscription_type == "Subscriber")

#Date Prep
weather$date <- mdy(weather$date)
trip$start_date <- mdy_hm(trip$start_date)
trip$end_date <- mdy_hm(trip$end_date)
trip$date <- trip$start_date
trip$date <- as.Date(trip$date)

#Make a weekend vs. weekday variable
#trip$weekend <- as.factor(wday(trip$date))
#trip$weekend <- (trip$weekend == 1 | trip$weekend == 7)
#trip$weekend <- factor(trip$weekend, labels = c("Weekday", "Weekend"))

#Warning, holidays will still be adding some errors to this dataset 

#Filter out cities except San Francisco
#trip$date <- as.Date(trip$start_date)
trip$id2 <- trip$id
trip$id <- trip$start_station_id 
trip <- left_join(trip, station, by = c ("id"))

#trip <- filter(trip, city == "San Francisco")

#Create counts by date 
dailyrides <- as.data.frame(table(trip$date, trip$city))
colnames(dailyrides) <- c("date","city", "ridecount")
dailyrides$date <- as.Date(dailyrides$date)

dailyrides$weekend <- as.factor(wday(dailyrides$date))
dailyrides$weekend <- (dailyrides$weekend == 1 | dailyrides$weekend == 7)
dailyrides$weekend <- factor(dailyrides$weekend, labels = c("Weekday", "Weekend"))

dailyrides <- filter(dailyrides, weekend == "Weekday")

table(dailyrides$city)

#Add weather info
#Get "city" variable into weather df
zip_code <- unique(weather$zip_code)
city <- c ("San Francisco", "Redwood City", "Palo Alto", "Mountain View", "San Jose")
index <- cbind(city, zip_code)   
weather <- merge(weather, index, by = "zip_code")

weather2 <- weather %>%
  select(zip_code, date ,mean_temperature_f, 
         mean_humidity, mean_dew_point_f, mean_wind_speed_mph,
         events, city)

table(weather2$events)
weather2$events <- factor(weather2$events)
weather2$rain <- ifelse(unclass(weather2$events) > 2
                          , c("rain"), c("no rain"))

dailyrides$rain <- factor(dailyrides$rain)

#Merge to ride count df
dailyrides <- left_join(dailyrides, weather2, by = c("date", "city"))

#Completeness of the data
sapply(dailyrides, function(x) {sum(is.na(x))})

dailyrides <- dailyrides %>%
  mutate(mean_temperature_f = ifelse(is.na(mean_temperature_f), 
                                     mean(mean_temperature_f, na.rm = TRUE), mean_temperature_f),
         mean_humidity = ifelse(is.na(mean_humidity), 
                                mean(mean_humidity, na.rm = TRUE), mean_humidity),
         mean_dew_point_f = ifelse(is.na(mean_dew_point_f), 
                                   mean(mean_dew_point_f, na.rm = TRUE), mean_dew_point_f),
         mean_wind_speed_mph = ifelse(is.na(mean_wind_speed_mph), 
                                      mean(mean_wind_speed_mph, na.rm = TRUE), mean_wind_speed_mph))

dailyrides <- filter(dailyrides, city == "San Francisco")

#Graphs
dailyrides %>% ggplot(aes(mean_temperature_f, ridecount)) +
  geom_point(color = "red") + 
  geom_smooth(method = "lm") + 
  theme_light() +
  ylab("Number of bicycle rides") +
  xlab("Mean Temperature") +
  ggtitle("Locals in San Francisco")

dailyrides %>% ggplot(aes(mean_humidity, ridecount)) +
  geom_point(color = "red") + 
  geom_smooth(method = "lm") + 
  theme_light() +
  ylab("Number of bicycle rides") +
  xlab("Mean Humidity") +
  ggtitle("Locals in San Francisco")

dailyrides %>% ggplot(aes(mean_dew_point_f, ridecount)) +
  geom_point(color = "red") + 
  geom_smooth(method = "lm") + 
  theme_light() +
  ylab("Number of bicycle rides") +
  xlab("Mean Dew Point") +
  ggtitle("Locals in San Francisco")

dailyrides %>% ggplot(aes(mean_wind_speed_mph, ridecount)) +
  geom_point(color = "red") + 
  geom_smooth(method = "lm") + 
  theme_light() +
  ylab("Number of bicycle rides") +
  xlab("Mean Wind Speed") +
  ggtitle("Locals in San Francisco")

dailyrides %>% ggplot(aes(rain, ridecount)) +
  geom_boxplot(color = "red", fill = "blue") +
  theme_light() + 
  ylab("Number of bicycle rides") +
  ggtitle("Locals in San Francisco")

#Make train and test sets
index <- createDataPartition(dailyrides$ridecount, times = 1, p = 0.5, list = FALSE)
train_data <- dailyrides[index,]
test_data <- dailyrides[-index,]


#Linear Regression
set.seed(1234)
ctrl <- trainControl(method = "repeatedcv", repeats = 3)

lm.fit <- train(ridecount ~ rain + mean_temperature_f + mean_humidity +
                  mean_wind_speed_mph + mean_dew_point_f,
                data = train_data,
                method = "lm",
                trControl = ctrl)

summary(lm.fit)

pred.lm <- predict(lm.fit, test_data)
Lm.RMSE <- RMSE(pred.lm, test_data$ridecount) #223.76

#confusionMatrix(pred.lm, test_data$ridecount)
#confusionMatrix(predict(lm.fit, test_set_smarket),test_set_smarket$Direction)$overall["Accuracy"] #52 Accuracy` `

#train_nnet$bestTune
#confusionMatrix(predict(lm.fit, test_data), as.numeric(test_data$ridecount))


#Partial Least Squares
library(pls)
pls.fit <- train(ridecount ~ rain + mean_temperature_f + mean_humidity +
                   mean_wind_speed_mph + mean_dew_point_f, 
                 data = train_data, 
                 method = "pls",
                 trControl = ctrl, 
                 preProc = c("center", "scale"),
                 tuneLength = 30)
                 #tuneGrid = data.frame(ncomp=13))

pls.fit
pred.pls <- predict(pls.fit, test_data)
Pls.RMSE <- RMSE(pred.pls, test_data$ridecount) #224.03

#Elastic Net
library(elasticnet)
enetGrid <- expand.grid(.lambda = c(0,0.01, .1), .fraction = seq(.05, 1, length = 20))

enet.fit <- train(ridecount ~ rain + mean_temperature_f + mean_humidity +
                    mean_wind_speed_mph + mean_dew_point_f, 
                  data = train_data, 
                  method = "enet", 
                  trControl = ctrl, 
                  preProc = c("center", "scale"), 
                  tuneGrid = enetGrid) 

enet.fit
pred.enet <- predict(enet.fit, test_data)
Enet.RMSE <- RMSE(pred.enet, test_data$ridecount) #223.69

#sqrt(mean((pred.enet - test$fat)^2))

#Neural Network
nnGrid <- expand.grid(.decay = c(0,0.01,.1), .size = c(1:10)) 

nn.fit <- train(ridecount ~ rain + mean_temperature_f + mean_humidity +
                  mean_wind_speed_mph + mean_dew_point_f, 
                data = train_data , 
                method = "nnet",
                trControl = ctrl, 
                preProc = c("center", "scale"), 
                tuneGrid = nnGrid)

pred.nn <- predict(nn.fit, test_data)
NN.RMSE <- RMSE(pred.nn, test_data$ridecount) #1042.27

#MARS
library(earth)
marsGrid <- expand.grid(.degree = 1:2, .nprune = 2:34)

mars.fit <- train(ridecount ~ rain + mean_temperature_f + mean_humidity +
                    mean_wind_speed_mph + mean_dew_point_f, 
                  data = train_data , 
                  method = "earth",
                  tuneGrid = marsGrid,
                  trControl = ctrl)

mars.fit
pred.mars <- predict(mars.fit, test_data)
Mars.RMSE <- RMSE(pred.mars, test_data$ridecount) #224.93

#SVM
library(kernlab)
svmGrid <- expand.grid(.sigma = .10204, .C = 8:16)

svm.fit <- train(ridecount ~ rain + mean_temperature_f + mean_humidity +
                   mean_wind_speed_mph + mean_dew_point_f, 
                 data =train_data , 
                 method = "svmRadial",
                 trControl = ctrl,
                 preProc = c("center", "scale"),
                 tuneLength = 20)
                 #tuneGrid = svmGrid)

svm.fit
svm.fit$finalModel

pred.svm <- predict(svm.fit, test_data)
Svm.RMSE <- RMSE(pred.svm, test_data$ridecount) #5.3322

#Knn
#Best Model
knn.fit <- train(ridecount ~ rain + mean_temperature_f + mean_humidity +
                   mean_wind_speed_mph + mean_dew_point_f, 
                 data = train_data , 
                 method = "knn",
                 preProc = c("center", "scale"),
                 tuneGrid = data.frame(k=seq(1,101,2)),
                 trControl = ctrl)

knn.fit$finalModel #11
pred.knn <- predict(knn.fit, test_data)
Knn.RMSE <- RMSE(pred.knn, test_data$ridecount) #215.14

