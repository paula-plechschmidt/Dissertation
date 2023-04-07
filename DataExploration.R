# Data exploration

## How many races have a horse that dropped out
racesWithDrop <- 0

for (i in 1:1000){
  print(i)
  print(length(unique(data[[i]]$runnerStatus)))
  if (length(unique(data[[i]]$runnerStatus)) != 1){
    racesWithDrop = racesWithDrop + 1
  }
}

## How many races have a market that has closed
racesWithClosedMarket <- 0

for (i in 1:1000){
  print(i)
  print(unique(data[[i]]$marketStatus))
  if (length(unique(data[[i]]$marketStatus)) != 1){
    racesWithClosedMarket = racesWithClosedMarket + 1
  }
}


## How many races have data points where the race is in play
racesInPlay <- 0

for (i in 1:1000){
  print(i)
  print(unique(data[[i]]$inplay))
  if (length(unique(data[[i]]$inplay)) != 1){
    racesInPlay = racesInPlay + 1
  }
}

data <- readRDS("~/Documents/Year 4/Project/myrepo/smallData_hc_1k.rds")

## Data cleaning
cleanData <- list()
drop <- c("runnerStatus", "adjustmentFactor", "marketStatus",
          "inplay", "marketName", "distFurl", "distRecordTime",
          "winnerFlag", "SP_nearPrice", "SP_farPrice", "SP_actualSP")
for (i in 1:1000){
  print(i)
  new_df <- data[[i]][,!names(data[[i]]) %in% drop] #removing unnecessary columns
  new_df <- new_df[!(new_df$timeToStart > 0),] #remove rows with timeToStart > 0
  if (!any(is.na(new_df))){
    cleanData <- append(cleanData, list(na.omit(new_df)))
  }
}
saveRDS(cleanData, file = "cleanDataNoNA.rds")


## Add a column that represents the variability of each of the horses backing and laying price
cleanDataVolatility <- list()
for (i in 1:1000){
  selectionId <- unique(cleanData[[i]]$selectionId)
  new_df <- cbind(cleanData[[i]], bckStandardDeviation=NA, layStandardDeviation=NA)
  for (id in selectionId){
    bckStandardDev <- sd((cleanData[[i]][(cleanData[[i]]$selectionId == id),])$bckPrc1)
    layStandardDev <- sd((cleanData[[i]][(cleanData[[i]]$selectionId == id),])$bckPrc1)
    new_df <- within(new_df, bckStandardDeviation[selectionId == id] <- bckStandardDev)
    new_df <- within(new_df, layStandardDeviation[selectionId == id] <- layStandardDev)
  }
  cleanDataVolatility <- append(cleanDataVolatility, list(new_df))
}
saveRDS(cleanDataVolatility, file = "cleanDataWithStdDev.rds")


#Importing the cleaned data as data
data <- readRDS("~/Documents/Year 4/Project/myrepo/cleanDataWithStdDev.rds")

### VOLATILITY VS ODDS ###
## Plot the relationship between volatility and odds
plot(data[[2]]$bckPrc1, data[[2]]$bckStandardDeviation,
     main = "Backing Odds vs. Standard Deviation of Backing Odds", 
     xlab = "Backing Odds", ylab = "Volatility")
plot(data[[3]]$bckPrc1, data[[3]]$bckStandardDeviation,
     main = "Backing Odds vs. Standard Deviation of Backing Odds", 
     xlab = "Backing Odds", ylab = "Volatility")
plot(data[[4]]$bckPrc1, data[[4]]$bckStandardDeviation,
     main = "Backing Odds vs. Standard Deviation of Backing Odds", 
     xlab = "Backing Odds", ylab = "Volatility")


### VOLUME VS ODDS ###
library(dplyr)
library(zoo)

relOdds <- c()
pctVol <- c()
for (i in 1:1000){
  minTick <- min(data[[i]]$timeToStart) #gets the smallest time before start (sometimes is 0, sometimes -1)
  df_time0 <- data[[i]][(data[[i]]$timeToStart == minTick),]  #getting only the rows where time is 0, i.e. right before start of race
  #if statement to handle when the total matched is given as 0
  if (unique(df_time0$totalMatched) != 0){
    minBckPrc1 <- min(df_time0$bckPrc1)
    relOdds <- c(relOdds, df_time0$bckPrc1 - minBckPrc1)
    pctVol <- c(pctVol, df_time0$bckSz1/unique(df_time0$totalMatched))
  }
}

plot(relOdds, relVol)
oddsVsVol <- data.frame(relOdds, relVol)
ggplot(oddsVsVol, aes(x=relOdds, y=relVol)) + 
  geom_point() + 
  labs(title="Relative Odds vs. Relative Volume",x="Odds", y = "Volume") +
  xlim(0,10) + ylim(0,1)


### REVERSION TO STARTING VALUE ### Not done yet
#Look at each of the least favorites
#Count the number of times the odds passed through the starting value

for (i in 1:1000){
  time <- min(data[[i]]$timeToStart)
  id <- underdog(data[[i]], time)[1]
  srtOdd <- subset(data[[i]], timeToStart==time & selectionId==id)$bckPrc1
  
  
}

time <- min(data[[2]]$timeToStart)
id <- underdog(data[[2]], timeToStart)[1]
srtOdd <- data[[2]][(data[[2]]$timeToStart == timeToStart) && (data[[2]]$selectionId == id),]

subset(data[[2]], timeToStart==time & selectionId==id)$bckPrc1



### INCREASE IN ODDS OF THE UNDERDOG ###
diffBckOdds <- c()
diffLayBckOdds <- c()
for (i in 1:1000){
  print(i)
  startTime <- min(data[[i]]$timeToStart)
  endTime <- max(data[[i]]$timeToStart)
  undDog <- underdog(data[[i]], time)
  startBckOdds <- subset(data[[i]], selectionId==undDog & timeToStart==startTime)$bckPrc1
  startLayOdds <- subset(data[[i]], selectionId==undDog & timeToStart==startTime)$layPrc1
  endOdds <- subset(data[[i]], selectionId==undDog & timeToStart==endTime)$bckPrc1
  diffBckOdds <- c(diffBckOdds, endOdds - startBckOdds)
  diffLayBckOdds <- c(diffLayBckOdds, endOdds - startLayOdds)
}

ggplot(data.frame(diffBckOdds), aes(x=factor(0), y=diffBckOdds)) + geom_boxplot() +
  xlab("") + ylab("Final Odds - Initial Odds") + ggtitle("Distribution of Difference in Odds")

ggplot(data.frame(diffLayBckOdds), aes(x=factor(0), y=diffLayBckOdds)) + geom_boxplot() +
  xlab("") + ylab("Final Backing Odds - Initial Laying Odds") + ggtitle("Distribution of Difference in Backing and Laying Odds")



### RELATIONSHIP BETWEEN 1ST AND 2ND FAVOURITE HORSE ###
df_first <- subset(data[[3]], selectionId == favourite(data[[3]], min(data[[3]]$timeToStart)))
df_second <- subset(data[[3]], selectionId == secondFavourite(data[[3]], min(data[[3]]$timeToStart)))

cor(df_first$bckPrc1, df_second$bckPrc1)

correlations <- c()
for (i in 1:1000){
  print(i)
  first <- favourite(data[[i]], min(data[[i]]$timeToStart))
  second <- secondFavourite(data[[i]], min(data[[i]]$timeToStart))
                    
  if(length(first) != 1){
    df_first <- subset(data[[i]], selectionId == first[1])
    df_second <- subset(data[[i]], selectionId == first[2])
    correlations <- c(correlations, cor(df_first$bckPrc1, df_second$bckPrc1))
  } else {
    df_first <- subset(data[[i]], selectionId == first)
    df_second <- subset(data[[i]], selectionId == second[1])
    correlations <- c(correlations, cor(df_first$bckPrc1, df_second$bckPrc1))
  }
}

ggplot(data.frame(correlations), aes(x=factor(0), y=correlations)) + geom_boxplot() +
  xlab("") + ylab("Correlation") + ggtitle("Correlation between First and Second Favourite Horse's Backing Prices")


### VOL VS SPREAD ###
spread <- c()
vol <- c()
for (i in 1:1000){
  selectionId <- unique(data[[i]]$selectionId)
  for (id in selectionId){
    df_id <- subset(data[[i]], selectionId == id)
    spread <- c(spread, mean(df_id$layPrc1 - df_id$bckPrc1))
    vol <- c(vol, df_id$bckStandardDeviation[1])
  }
}

ggplot(data.frame(spread, vol), aes(x=vol, y=spread)) + geom_point() + xlim(0,220) +   
  xlab("Standard Deviation") + ylab("Average Spread") + ggtitle("Volatility vs. Spread")





