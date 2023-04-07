## GOING TO TERMINATE TRADING ON A RACE WHEN A PROFIT HAS BEEN ACHIEVED ##

momentum <- function(df){
  #only looking at the favourite
  fav <- favourite(df, min(df$timeToStart))[1]
  df_fav <- df[df$selectionId == fav,]
  rownames(df_fav) <- NULL #To reindex the subsetted dataframe
  
  bckPrices <- df_fav$bckPrc1[1:4] #last four back prices
  layPrices <- df_fav$layPrc1[1:4] #last four lay prices
  startOdds <- 0 #starting odds set to 0
  finalOdds <- 0 #starting odds set to 0
  backThenLay <- FALSE #to indicate chosen strategy
  tradeActive <- FALSE #to indicate if we already have a trade running
  profit <- 0 #total profit
  importantTimesStart <- c()
  importantTimesEnd <- c()
  
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
      print("start backing")
      startOdds <- bckPrices[4] #Set the starting odds
      backThenLay <- TRUE #Indicate the strategy
      tradeActive <- TRUE #Announce the trade is active
      importantTimesStart <- c(importantTimesStart, rowData$callTime_GMT)
    }
    #If prices have increased three times consecutively and we have no active trade we "place" a bet
    else if((layPrices[2] < layPrices[3]) & (layPrices[1] < layPrices[2]) & 
            (layPrices[3] < layPrices[4]) & !tradeActive){
      print("start laying")
      startOdds <- layPrices[4] #set starting odds
      backThenLay <- FALSE #indicate strategy
      tradeActive <- TRUE #announce active trade
      importantTimesStart <- c(importantTimesStart, rowData$callTime_GMT)
    }
    
    #If prices have decreased twice consecutively and we are in a lay then back strategy OR
    #if prices have increased twice consecutively and we are in a back then lay strategy
    #we exit the trade
    else if((tradeActive & backThenLay & (layPrices[1] < layPrices[2]) &
             (layPrices[2] < layPrices[3]) & (layPrices[3] < layPrices[4])) ||
            (tradeActive & !backThenLay & (bckPrices[1] > bckPrices[2]) & 
             (bckPrices[2] > bckPrices[3]) & (bckPrices[3] > bckPrices[4])) ||
            (i == nrow(df_fav))){
      
      print("end")
      importantTimesEnd <- c(importantTimesEnd, rowData$callTime_GMT)
      if(backThenLay){
        finalOdds <- layPrices[4]
      } else {
        finalOdds <- bckPrices[4]
      }
      print(startOdds)
      print(finalOdds)
      tradeProfit <- tradeProfit(10, startOdds, finalOdds, backThenLay)
      profit <- profit + tradeProfit #calculate theoretical profit
      if(profit > 0){
        break
      }
      startOdds <- 0 #reset odds
      endOdds <- 0 #reset odds
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

profits4 <- c()
for (i in 1:800){
  print(i)
  profits4 <- c(profits4, momentum(data[[i]]))
}

mean(profits4)
ggplot(data.frame(profits4), aes(x=profits4)) + geom_histogram() +
  ggtitle("Histogram of Profits with Third Trading Method") + xlab("Profits")
write.csv(profits4,file="profitsFourhtMomentumRun.csv",row.names=F)







