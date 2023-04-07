library(tidyverse)
library(ggplot2)
require(gridExtra)

## Idea: If price starts to drop then the price will continue dropping
# All I am doing here is tracking the favourites' price movements

data <- readRDS("~/Documents/Year 4/Project/myrepo/cleanDataWithStdDev.rds")

getBckPrc <- function(df, time){
  return(df[df$timeToStart == time,]$bckPrc1)
}

momentum <- function(df){
  #only looking at the favourite
  fav <- favourite(df, min(df$timeToStart))[1]
  df_fav <- df[df$selectionId == fav,]
  columns <- c("StartingOdds", "EndingOdds", "Strategy", "TradeProfit")
  trade_df <- data.frame(matrix(nrow=0, ncol=length(columns)))
  colnames(trade_df) <- columns
  
  bckPrices <- df_fav$bckPrc1[1:4] #last four back prices
  layPrices <- df_fav$layPrc1[1:4] #last four lay prices
  startOdds <- 0 #starting odds set to 0
  finalOdds <- 0 #starting odds set to 0
  backThenLay <- FALSE #to indicate chosen strategy
  tradeActive <- FALSE #to indicate if we already have a trade running
  profit <- 0 #total profit
  importantTimesStart <- c()
  importantTimesEnd <- c()
  
  #print(bckPrices)
  #print(layPrices)
  
  for (time in df_fav$timeToStart){
    rowData <- subset(df_fav, timeToStart == time)

    #Checking if price has changed and adding to the odds tracker
    if(bckPrices[4] != rowData$bckPrc1[1]){
      bckPrices <- c(bckPrices, rowData$bckPrc1)
      bckPrices <- bckPrices[-1]
    }
    if(layPrices[4] != rowData$layPrc1[1]){
      layPrices <- c(layPrices, rowData$layPrc1)
      layPrices <- layPrices[-1]
    }
    
    #If prices have decreased three times consecutively and we have no active trade we "place" a bet
    if((bckPrices[2] > bckPrices[3]) & (bckPrices[1] > bckPrices[2]) &
       (bckPrices[3] > bckPrices[4]) & !tradeActive){
      #print("")
      #print("Starting Backing Prices")
      #print(bckPrices)
      #print(rowData$callTime_GMT)
      startOdds <- bckPrices[4] #Set the starting odds
      backThenLay <- TRUE #Indicate the strategy
      importantTimesStart <- c(importantTimesStart, rowData$callTime_GMT)
      tradeActive <- TRUE #Announce the trade is active
    }
    #If prices have increased three times consecutively and we have no active trade we "place" a bet
    else if((layPrices[2] < layPrices[3]) & (layPrices[1] < layPrices[2]) & 
       (layPrices[3] < layPrices[4]) & !tradeActive){
      #print("")
      #print("Starting Laying Prices")
      #print(layPrices)
      #print(rowData$callTime_GMT)
      startOdds <- layPrices[4] #set starting odds
      backThenLay <- FALSE #indicate strategy
      importantTimesStart <- c(importantTimesStart, rowData$callTime_GMT)
      tradeActive <- TRUE #announce active trade
    }
    
    #If prices have decreased twice consecutively and we are in a lay then back strategy OR
    #if prices have increased twice consecutively and we are in a back then lay strategy
    #if race is about to start
    #we exit the trade
    else if((tradeActive & backThenLay & (layPrices[3] < layPrices[4]) &
       (layPrices[2] < layPrices[3])) ||
       (tradeActive & !backThenLay & (bckPrices[3] > bckPrices[4]) &
       (bckPrices[2] > bckPrices[3])) ||
       (tradeActive & time == 0)){
      
      if(backThenLay){
        finalOdds <- layPrices[4]
      } else {
        finalOdds <- bckPrices[4]
      }
      tradeProfit <- tradeProfit(10, startOdds, finalOdds, backThenLay)
      
      #print("Final Prices")
      #print(bckPrices)
      #print(layPrices)
      #print(startOdds)
      #print(finalOdds)
      #print(backThenLay)
      #print(tradeProfit)
      
      profit <- profit + tradeProfit #calculate theoretical profit
      startOdds <- 0 #reset odds
      endOdds <- 0 #reset odds
      importantTimesEnd <- c(importantTimesEnd, rowData$callTime_GMT)
      tradeActive <- FALSE #indicate trade has completed
    }
  }
  
  pricePlot <- plotLayBckPriceFavourite(df)
  for (time in importantTimesStart){
    pricePlot <- pricePlot + geom_vline(xintercept = time, color = "green")
  }
  for (time in importantTimesEnd){
    pricePlot <- pricePlot + geom_vline(xintercept = time, color = "red")
  }
  plot(pricePlot)
  return(profit)
}
# loop through all ticks
# when price changes - add new price to the end and the oldest price gets
# deleted from the front

# if tradeActive is false, each time price changes check if previous two were 
#   both higher/lower and if no trade is in process
#   if higher then set backThenLay to true, if not to false
# if conditions are met, save the odds to the starting odds and set tradeActive
#  to true.

# keep updating prices each time they move
# if backThenLay, look at the laying prices and check if that they haven't 
#   increased twice in a row
# if !backThenLay, look at the backing prices and check that they haven't 
#   decreased twice in a row
# if they have done either of these things, we "sell" and save the odds as the
#   final odds, set tradeActive to false and calculate the profit

profits <- c()

for (i in 1:1000){
  profits <- c(profits, momentum(data[[i]]))
}

ggplot(data.frame(profits), aes(x=profits)) + geom_histogram() + xlim(-20,10) +
  ggtitle("Histogram of Profits with Initial Trading Method")
write.csv(profits,file="profitsFirstMomentumRun.csv",row.names=F)

contrarian <- function(df){
  #only looking at the favourite
  fav <- favourite(df, min(df$timeToStart))[1]
  df_fav <- df[df$selectionId == fav,]
  columns <- c("StartingOdds", "EndingOdds", "Strategy", "TradeProfit")
  trade_df <- data.frame(matrix(nrow=0, ncol=length(columns)))
  colnames(trade_df) <- columns
  
  bckPrices <- df_fav$bckPrc1[1:4] #last four back prices
  layPrices <- df_fav$layPrc1[1:4] #last four lay prices
  startOdds <- 0 #starting odds set to 0
  finalOdds <- 0 #starting odds set to 0
  backThenLay <- FALSE #to indicate chosen strategy
  tradeActive <- FALSE #to indicate if we already have a trade running
  profit <- 0 #total profit
  
  for (time in df_fav$timeToStart){
    rowData <- subset(df_fav, timeToStart == time)
    
    #Checking if price has changed and adding to the odds tracker
    if(bckPrices[4] != rowData$bckPrc1[1]){
      bckPrices <- c(bckPrices, rowData$bckPrc1)
      bckPrices <- bckPrices[-1]
    }
    if(layPrices[4] != rowData$layPrc1[1]){
      layPrices <- c(layPrices, rowData$layPrc1)
      layPrices <- layPrices[-1]
    }
    
    #If prices have decreased three times consecutively and we have no active trade we "place" a bet
    if((bckPrices[1] < bckPrices[2]) & (bckPrices[2] < bckPrices[3]) &
       (bckPrices[3] < bckPrices[4]) & !tradeActive){
      print("")
      print("Starting Backing Prices")
      print(bckPrices)
      print(rowData$callTime_GMT)
      startOdds <- bckPrices[4] #Set the starting odds
      backThenLay <- TRUE #Indicate the strategy
      tradeActive <- TRUE #Announce the trade is active
    }
    #If prices have increased three times consecutively and we have no active trade we "place" a bet
    else if((layPrices[1] > layPrices[2]) & (layPrices[2] > layPrices[3]) &
            (layPrices[3] > layPrices[4]) & !tradeActive){
      print("")
      print("Starting Laying Prices")
      print(layPrices)
      print(rowData$callTime_GMT)
      startOdds <- layPrices[4] #set starting odds
      backThenLay <- FALSE #indicate strategy
      tradeActive <- TRUE #announce active trade
    }
    
    #If prices have decreased twice consecutively and we are in a lay then back strategy OR
    #if prices have increased twice consecutively and we are in a back then lay strategy
    #we exit the trade
    else if((tradeActive & backThenLay & (layPrices[2] < layPrices[3]) &
             (layPrices[3] < layPrices[4])) ||
            (tradeActive & !backThenLay & (bckPrices[2] > bckPrices[3]) &
             (bckPrices[3] > bckPrices[4]))){
      
      if(backThenLay){
        finalOdds <- layPrices[4]
      } else {
        finalOdds <- bckPrices[4]
      }
      tradeProfit <- tradeProfit(10, startOdds, finalOdds, backThenLay)
      
      print("Final Prices")
      print(bckPrices)
      print(layPrices)
      print(startOdds)
      print(finalOdds)
      print(backThenLay)
      print(tradeProfit)
      
      profit <- profit + tradeProfit #calculate theoretical profit
      startOdds <- 0 #reset odds
      endOdds <- 0 #reset odds
      tradeActive <- FALSE #indicate trade has completed
    }
  }
  return(profit)
}

