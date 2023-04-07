## MAKES IT LESS SENSITIVE - NOW NEEDS 3 TICKS TO JUMP OUT OF POSITION ##

data <- readRDS("~/Documents/Year 4/Project/myrepo/cleanDataNoNA.rds")

momentum <- function(df, leastFavourite = FALSE){
  #only looking at the favourite
  fav <- favourite(df, min(df$timeToStart))[1]
  if (leastFavourite){
    fav <- underdog(df, min(df$timeToStart))[1]
  }
  df_fav <- df[df$selectionId == fav,]
  rownames(df_fav) <- NULL #To reindex the subsetted dataframe
  
  if(any(is.na(df_fav))){
    return(0)
  }
  
  bckPrices <- df_fav$bckPrc1[1:4] #last four back prices
  layPrices <- df_fav$layPrc1[1:4] #last four lay prices
  startOdds <- 0 #starting odds set to 0
  finalOdds <- 0 #starting odds set to 0
  backThenLay <- FALSE #to indicate chosen strategy
  tradeActive <- FALSE #to indicate if we already have a trade running
  profit <- 0 #total profit
  importantTimesStart <- c()
  importantTimesEnd <- c()
  startTime <- NULL
  
  for (i in 1:nrow(df_fav)){
    rowData <- df_fav[i,]
    
    #Checking if price has changed and adding to the odds tracker
    if(bckPrices[4] != rowData$bckPrc1){
      bckPrices <- c(bckPrices, rowData$bckPrc1)
      bckPrices <- bckPrices[-1]
    }
    if(layPrices[4] != rowData$layPrc1){
      layPrices <- c(layPrices, rowData$layPrc1)
      layPrices <- layPrices[-1]
    }
    
    #If prices have decreased three times consecutively and we have no active trade we "place" a bet
    if((bckPrices[2] > bckPrices[3]) & (bckPrices[1] > bckPrices[2]) &
       (bckPrices[3] > bckPrices[4]) & !tradeActive){
      'print("")
      print("")
      print("")
      print("Starting Backing Prices")
      print(bckPrices)
      print(rowData$callTime_GMT)'
      startOdds <- bckPrices[4] #Set the starting odds
      startTime <- rowData$callTime_GMT
      backThenLay <- TRUE #Indicate the strategy
      tradeActive <- TRUE #Announce the trade is active
      importantTimesStart <- c(importantTimesStart, rowData$callTime_GMT)
    }
    #If prices have increased three times consecutively and we have no active trade we "place" a bet
    else if((layPrices[2] < layPrices[3]) & (layPrices[1] < layPrices[2]) & 
            (layPrices[3] < layPrices[4]) & !tradeActive){
      'print("")
      print("")
      print("")
      print("Starting Laying Prices")
      print(layPrices)
      print(rowData$callTime_GMT)'
      startOdds <- layPrices[4] #set starting odds
      startTime <- rowData$callTime_GMT
      backThenLay <- FALSE #indicate strategy
      tradeActive <- TRUE #announce active trade
      importantTimesStart <- c(importantTimesStart, rowData$callTime_GMT)
    }
    
    #If prices have decreased thrice consecutively and we are in a lay then back strategy OR
    #if prices have increased thrice consecutively and we are in a back then lay strategy
    #we exit the trade
    else if((tradeActive & backThenLay & (layPrices[1] < layPrices[2]) &
             (layPrices[2] < layPrices[3]) & (layPrices[3] < layPrices[4])) ||
            (tradeActive & !backThenLay & (bckPrices[1] > bckPrices[2]) & 
             (bckPrices[2] > bckPrices[3]) & (bckPrices[3] > bckPrices[4])) ||
            (i == nrow(df_fav))){
      
      if(backThenLay){
        finalOdds <- layPrices[4]
      } else {
        finalOdds <- bckPrices[4]
      }
      tradeProfit <- tradeProfit(10, startOdds, finalOdds, backThenLay)
      
      '
      print("----------------------")
      print("Final Prices")
      print(bckPrices)
      print(layPrices)
      print(startTime)
      print(rowData$callTime_GMT)
      print(startOdds)
      print(finalOdds)
      print(backThenLay)
      print(tradeProfit)
      '
      
      profit <- profit + tradeProfit #calculate theoretical profit
      #print(cat("Total Profit: ", profit))
      startOdds <- 0 #reset odds
      endOdds <- 0 #reset odds
      tradeActive <- FALSE #indicate trade has completed
      importantTimesEnd <- c(importantTimesEnd, rowData$callTime_GMT)
    }
  }
  '
  pricePlot <- plotLayBckPriceFavourite(df)
  for (time in importantTimesStart){
    pricePlot <- pricePlot + geom_vline(xintercept = time, color = "green")
  }
  for (time in importantTimesEnd){
    pricePlot <- pricePlot + geom_vline(xintercept = time, color = "red")
  }
  plot(pricePlot)
  '
  return(profit)
}

profits <- c()
for (i in 1:800){
  print(i)
  profits <- c(profits, momentum(data[[i]], TRUE))
  print("----------------------------------------------------")
}

mean(profits)
ggplot(data.frame(profits), aes(x=profits)) + geom_histogram() +
  ggtitle("Histogram of Profits with Second Trading Method for the Least Favourite Horse") + xlab("Profits")
write.csv(profits,file="profitsFourthMomentumStrategy_underdog.csv",row.names=F)



fav <- underdog(data[[199]], min(data[[199]]$timeToStart))[1]
df_fav <- data[[199]][data[[199]]$selectionId == fav,]
rownames(df_fav) <- NULL


profitsFav <- read.csv("profitsFourthMomentumRun.csv")
profitsFav <- unlist(profitsFav$x)
profitsUnderdog <- profits

posFav <- sum(profitsFav > 0)/length(profitsFav)
posUnderdog <- sum(profitsUnderdog > 0)/length(profitsUnderdog)
