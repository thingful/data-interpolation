##################################################################################
############################# Thingful Experiment ################################

#### Loading all the libraries needed for the experiment
library(ggplot2)
library(reshape2)
library(rpart)
library(caret)
library(data.table)
library(randomForest)

set.seed(12321)

setwd("~/Admin/Work/Thingful/R Files") ## <- Change according to your directory

##################################################################################
########################### Processing of the data ###############################

thing_data <- read.csv(file = "Things_data.csv")

## Plot to get an idea of spread

p <- qplot(longitude, latitude, data = thing_data)
p + theme(panel.grid.minor = element_line(colour = "red"), 
          panel.grid.major = element_line(colour = "red"))

#### Set these variables according to analysis of above plot

min.lat <- 51.45    
max.lat <- 51.6     

min.lng <- -0.20    
max.lng <- 0        

resolution.lat <- 6  
resolution.lng <- 6  

########################

new_thing_data <- thing_data[which(thing_data$latitude >= min.lat & thing_data$latitude < max.lat & 
                                     thing_data$longitude >= min.lng & thing_data$longitude < max.lng),]

## Plotting the new data to get an idea of where the gaps are

p <- qplot(longitude, latitude, data = new_thing_data, xlim = c(min.lng, max.lng),
           ylim = c(min.lat, max.lat))
p + theme(panel.grid.minor = element_line(colour = "red"), 
          panel.grid.major = element_line(colour = "red"))

#### Drop unnecessary columns

dat1 <- new_thing_data[,c(2,3,4,5)]

#### More Cleaning

dat1[dat1$id == "temp",3] <- "temperature"
dat1[dat1$id == "no2",3] <- "Nitrogen Dioxide"
dat1[dat1$id == "hum",3] <- "screen_relative_humidity"

coordinates <- as.data.table(cbind(dat1$longitude, dat1$latitude))

##################################################################################
#################### Build centroids to cluster data around ######################

distance.lat <- max.lat - min.lat
distance.lng <- max.lng - min.lng

centers <- data.frame()

dist.lat <- distance.lat/resolution.lat
dist.lng <- distance.lng/resolution.lng

centers <- data.frame()

centers.lng <- data.frame()
centers.lat <- data.frame()

centers.lng[1,1] <- min.lng + (dist.lng/2)
centers.lat[1,1] <- min.lat + (dist.lat/2)

for (j in 2:resolution.lng){
  centers.lng[j,1] <- centers.lng[j-1,1] + dist.lng
}

for (i in 2:resolution.lat){
  centers.lat[i,1] <- centers.lat[i-1,1] + dist.lat
}

t = 1
for (i in 1:resolution.lng){
  
  for (j in 1:resolution.lat){
    print(t)
    centers[t,1] <- centers.lat[j,1]
    centers[t,2] <- centers.lng[i,1]
    t = t + 1
  }
  
  t = i*(resolution.lat)+1
  
}

##################################################################################
######## Plot centers and add as dummy variables to data to vizualize ############

dat_test <- dat1
centers[,3] <- "center"
centers[,4] <- 1
colnames(centers) <- c("latitude", "longitude", "id", "value")

dat_test <- rbind(dat_test, centers)

p <- qplot(longitude, latitude, data = dat_test, xlim = c(min.lng, max.lng),
           ylim = c(min.lat, max.lat))
p + theme(panel.grid.minor = element_line(colour = "red"), 
          panel.grid.major = element_line(colour = "red"))

##################################################################################
################# Perform clustering algorithm to data ###########################

coordinates <- as.data.table(cbind(dat_test$latitude, dat_test$longitude))

centers_1 <- centers[,c(1,2)]

clusters <- kmeans(coordinates, centers_1, iter.max = 1)

coordinates1 <- coordinates
coordinates1$cluster <- clusters$cluster

cluster <- clusters$cluster

dat2 <- cbind(dat_test, cluster)

##################################################################################
############ Give sensors uniq ids based on location - df.wide3 ##################

#### Flatten out the data with dcast to get "id" as variables

df.wide_uniq <- dcast(dat2, cluster ~ id, mean,  value.var = "value")

#### Some minor cleaning

colnames(df.wide_uniq)[12] <- "Nitrogen.Dioxide"
colnames(df.wide_uniq)[2] <- "Air.Quality.Index"
colnames(df.wide_uniq)[16] <- "PM10.Particulate"
colnames(df.wide_uniq)[17] <- "PM2.5.Particulate"
colnames(df.wide_uniq)[23] <- "Sulphur.Dioxide"

####

df.wide.centers <- cbind(df.wide_uniq, centers[,c(1,2)])

p2 <- ggplot(df.wide.centers, aes(x = longitude, y =latitude)) 
p2 + geom_tile(aes(fill = temperature)) + scale_fill_gradient(name = "Heat Map with Missing values",
                                                              low = "white", high = "orange")

name.col <- as.data.frame(as.data.table(dimnames(df.wide_uniq)[2]))

##################################################################################
############################## Filling in df.wide ################################

df.wide_test <- df.wide

for (i in 1:nrow(df.wide_test)){
  
  for (j in 2:length(df.wide_test)){
    
    if (is.na(df.wide_test[i,j])){
      index <- (df.wide_test[i,1])
      df.wide_test[i,j] <- df.wide_uniq[index,j]
      
    }
  }
}

##################################################################################
####### Examine missing data to determine what variables to keep/drop ############

#### For uniq variables

missing.data <- data.frame(2)

for (i in 1:length(df.wide_uniq)){
  missing.data[i,1] <- name.col[i,]
  missing.data[i,2] <- sum(is.na(df.wide_uniq[,i]))
}

colnames(missing.data) <- c("id", "no. missing")

#### For aggregated data

missing.data.test <- data.frame(2)

for (i in 1:length(df.wide)){
  missing.data.test[i,1] <- name.col[i,]
  missing.data.test[i,2] <- sum(is.na(df.wide_test[,i]))
}

colnames(missing.data.test) <- c("id", "no. missing")

##################################################################################
############################# Machine Learning ###################################

############################# rPart - df.wde_test ################################

df.wide_test2 <- df.wide_test[!is.na(df.wide_test$temperature),]
df.wide_test2$temperature <- round(df.wide_test2$temperature)
df.wide_test2$temperature <- as.factor(df.wide_test2$temperature)

inTrain <- createDataPartition(df.wide_test2$temperature, p = 0.6, list = FALSE)
train_model <- df.wide_test2[inTrain,]
train_validate <- df.wide_test2[-inTrain,]

## Using Rpart

startTime <- Sys.time()

modFit_rp <- rpart(temperature ~ ., data = train_model, method = "class")

runTime <- Sys.time() - startTime
runTime

pred <- predict(modFit_rp, train_validate, type = "class")
confusionMatrix(pred, train_validate$temperature)

############################# randomForest - df.wde_test #########################

df.wide_test2.impute <- rfImpute(temperature ~ .,df.wide_test2)

inTrain <- createDataPartition(df.wide_test2.impute$temperature, p = 0.6, list = FALSE)
train_model <- df.wide_test2.impute[inTrain,]
train_validate <- df.wide_test2.impute[-inTrain,]

startTime <- Sys.time()

modFit_rf <- randomForest(temperature ~ ., data = train_model)

runTime <- Sys.time() - startTime
runTime

pred2 <- predict(modFit_rf, train_validate, type = "class")
confusionMatrix(pred2, train_validate$temperature)

################################ Predict Final result ############################

Final_pred <- predict(modFit_rf, df.wide_uniq, type = "class") ### <- Not Working


