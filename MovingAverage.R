# Simple Moving Average
sma <- function(k,x) {      # k is the span, x is the data vector
  N <- length(x)
  out <- rep(NA,k-1)
  for(i in k:N) {
    out <- c(out,mean(x[(i-k+1):i]))
  }
  return(out)
}


## Trading Idea

# Calculate moving average at each point in time and save
# Calculate the average spread at each point and update
# If odds have moved by more than the average spread in the last minute
# (check assumptions on minute/optimise for ideal amount of time) we trade
# We keep the trade going 

movingAverageMomentum <- function(df, k){
  #only looking at the favourite
  fav <- favourite(df, min(df$timeToStart))[1]
  df_fav <- df[df$selectionId == fav,]
  rownames(df_fav) <- NULL #To reindex the subsetted dataframe
  df_fav$timeToStart <- as.numeric(df_fav$timeToStart)
  
  #creating matrix to save the moving average
  df_ma <- data.frame(matrix(ncol = 4, nrow = 0))
  names <- c("index", "timeToStart", "bckMa", "layMa")
  
  #variables to save
  marketOpen <- 0
  averageSpread <- 0
  tradeActive <- FALSE
  tradeStart <- 0
  backThenLay <- FALSE
  startOdds <- 0
  currentBckOdds <- 0
  currentLayOdds <- 0
  endOdds <- 0
  profit <- 0
  tradesOpened <- 0
  tradesClosed <- 0
  
  maxDiffOdds <- 0
  bestOdds <- 0
  
  count <- 0
  
  startBckTradeTime <- c()
  endBckTradeTime <- c()
  startLayTradeTime <- c()
  endLayTradeTime <- c()
  
  #looping through all  the times in the race
  for (time in df_fav$timeToStart){
    #Calculating moving average for each time
    if(count == k+1){
      marketOpen <- time
    }
    newRow <- c()
    if (count > k){
      #adding information to the moving average dataframe
      newRow <- c(newRow, count)
      newRow <- c(newRow, time)
      newRow <- c(newRow, mean(df_fav$bckPrc1[(count-k+1):count]))
      newRow <- c(newRow, mean(df_fav$layPrc1[(count-k+1):count]))
      df_ma <- rbind(df_ma, newRow)
      colnames(df_ma) <- names
      
      #calculating the average spread
      averageSpread <- (averageSpread + (df_fav[count,]$layPrc1 - df_fav[count,]$bckPrc1))/count
      
      #adjusting times to make due with duplicate/missing times
      timeMin60 <- time - 60
      marketOpenPlus60 <- marketOpen
      timeMin30 <- time - 30
      while(!(timeMin60 %in% df_ma$timeToStart)){
        timeMin60 <- timeMin60 + 1
      }
      while(!(marketOpen %in% df_ma$timeToStart)){
        marketOpenPlus60 <- marketOpenPlus60 + 1
      }
      while(!(timeMin30 %in% df_ma$timeToStart)){
        timeMin30 <- timeMin30 + 1
      }
      
      
      ##############
      ## STRATEGY ##
      ##############
      
      
      ######################
      ## trade not active ##
      ######################
      #If we do not have an active trade..
      if(!tradeActive & (time >= marketOpen + 60)){
        #.. and the moving average of the backing prices is decreasing over a minute
        if((newRow[3] < (df_ma[df_ma$timeToStart == (timeMin60),]$bckMa[1] - averageSpread))){
          #we enter a trade
          print("---------------------------------------------------------------")
          print("Starting trade by backing")
          startOdds <- df_fav$bckPrc1[count]
          print(startOdds)
          print(df_fav[df_fav$timeToStart == time,]$callTime_GMT)
          backThenLay <- TRUE
          tradeStart <- newRow[2]
          tradesOpened <- tradesOpened + 1
          bestOdds <- newRow[4]
          maxDiffOdds <- newRow[3] - newRow[4]
          startBckTradeTime <- c(startBckTradeTime, df_fav[df_fav$timeToStart == time,]$callTime_GMT)
          tradeActive <- TRUE
        }
        #.. and the moving average of the laying prices is increasing over a minute
        else if((newRow[4] > (df_ma[df_ma$timeToStart == (timeMin60),]$layMa[1] + averageSpread))){
          #we enter a trade
          print("--------------------------------------------------------------- ")
          print("Starting trade by laying")
          startOdds <- df_fav$bckPrc1[count]
          print(startOdds)
          print(df_fav[df_fav$timeToStart == time,]$callTime_GMT)
          backThenLay <- FALSE
          tradeStart <- newRow[2]
          tradesOpened <- tradesOpened + 1
          bestOdds <- newRow[3]
          maxDiffOdds <- newRow[3] - newRow[4]
          startLayTradeTime <- c(startLayTradeTime, df_fav[df_fav$timeToStart == time,]$callTime_GMT)
          tradeActive <- TRUE
        }
      }
      
      ##################
      ## trade active ##
      ##################
      #If we have an active trade..
      else if(tradeActive){
        currentBckOdds <- newRow[3]
        currentLayOdds <- newRow[4]
        
        #saving the largest possible profit we could have achieved and the corresponding odds
        # To handle if there are missing times
        tradeStart30 <- tradeStart + 30
        if(!(tradeStart30 %in% df_ma$timeToStart)){
          tradeStart30 <- tradeStart30 + 1
        }
        
        #.. and if we backed first
        if(backThenLay){
          # we quit the trade if we have not covered the spread in the first 30 seconds
          if((time == tradeStart30) & (newRow[4] < df_ma[df_ma$timeToStart == tradeStart,]$bckMa[1])){
            print("Spread not covered so we lay!")
            print(df_fav[df_fav$timeToStart == time,]$callTime_GMT)
            endOdds <- df_fav$layPrc1[count]
            print(endOdds)
            profit <- profit + tradeProfit(10, startOdds, endOdds, backThenLay)
            print(tradeProfit(10, startOdds, endOdds, backThenLay))
            maxDiffOdds <- 0
            bestOdds <- 0
            tradesClosed <- tradesClosed + 1
            endLayTradeTime <- c(endLayTradeTime, df_fav[df_fav$timeToStart == time,]$callTime_GMT)
            tradeActive <- FALSE
          }
          
          possibleProfit <- startOdds - currentLayOdds
          if (possibleProfit > maxDiffOdds){
            maxDiffOdds <- possibleProfit
            bestOdds <- currentLayOdds
          }
          # if we are more than 30 seconds into the trade and there has been a loss
          # larger than 25% of the maximum profit we could have achieved we quit the trade
          if (((currentLayOdds - bestOdds) >= 0.25*maxDiffOdds) &
              (time > tradeStart30)){
            print("Loss too large so we lay!")
            print(df_fav[df_fav$timeToStart == time,]$callTime_GMT)
            endOdds <- df_fav$layPrc1[count]
            print(endOdds)
            profit <- profit + tradeProfit(10, startOdds, endOdds, backThenLay)
            print(tradeProfit(10, startOdds, endOdds, backThenLay))
            maxDiffOdds <- 0
            bestOdds <- 0
            tradesClosed <- tradesClosed + 1
            endLayTradeTime <- c(endLayTradeTime, df_fav[df_fav$timeToStart == time,]$callTime_GMT)
            tradeActive <- FALSE
          }
        } 
        #.. and if we layed first
        else {
          # we quit the trade if we have not covered the spread in the first 30 seconds
          if((time == tradeStart30) & (newRow[3] > df_ma[df_ma$timeToStart == tradeStart,]$layMa[1])){
            print("Spread not covered so we back!")
            print(df_fav[df_fav$timeToStart == time,]$callTime_GMT)
            endOdds <- df_fav$bckPrc1[count]
            print(endOdds)
            profit <- profit + tradeProfit(10, startOdds, endOdds, backThenLay)
            print(tradeProfit(10, startOdds, endOdds, backThenLay))
            maxDiffOdds <- 0
            bestOdds <- 0
            tradesClosed <- tradesClosed + 1
            endBckTradeTime <- c(endBckTradeTime, df_fav[df_fav$timeToStart == time,]$callTime_GMT)
            tradeActive <- FALSE
          }
          
          possibleProfit <- currentBckOdds - startOdds
          if (possibleProfit > maxDiffOdds){
            maxDiffOdds <- possibleProfit
            bestOdds <- currentBckOdds
          }
          # if we are more than 30 seconds into the trade and there has been a loss
          # larger than 25% of the maximum profit we could have achieved we quit the trade
          if (((bestOdds - currentBckOdds) > 0.25*maxDiffOdds) &
              (time > tradeStart30)){
            print("Loss to large so we back!")
            print(df_fav[df_fav$timeToStart == time,]$callTime_GMT)
            endOdds <- df_fav$bckPrc1[count]
            print(endOdds)
            profit <- profit + tradeProfit(10, startOdds, endOdds, backThenLay)
            print(tradeProfit(10, startOdds, endOdds, backThenLay))
            maxDiffOdds <- 0
            bestOdds <- 0
            tradesClosed <- tradesClosed + 1
            endBckTradeTime <- c(endBckTradeTime, df_fav[df_fav$timeToStart == time,]$callTime_GMT)
            tradeActive <- FALSE
          }
        }
      }
    }
    count <- count + 1
  }
  
  print(tradesClosed)
  print((length(endBckTradeTime) + length(endLayTradeTime)))
  if(tradesClosed == (length(endBckTradeTime) + length(endLayTradeTime))){
    print("Yay")
  }
  
  #plotting for visualisation
  pricePlot <- plotLayBckPriceFavourite(df)
  pricePlot <- pricePlot + geom_line(data = df_ma, aes(x = df_fav$callTime_GMT[index], y = bckMa)) + 
    geom_line(data = df_ma, aes(x = df_fav$callTime_GMT[index], y = layMa))
  
  for(time in startBckTradeTime){
    pricePlot <- pricePlot + geom_vline(xintercept=time, colour="chartreuse4")
  }
  for(time in startLayTradeTime){
    pricePlot <- pricePlot + geom_vline(xintercept=time, colour="violetRed4")
  }
  for(time in endBckTradeTime){
    pricePlot <- pricePlot + geom_vline(xintercept=time, colour="violetRed2")
  }
  for(time in endLayTradeTime){
    pricePlot <- pricePlot + geom_vline(xintercept=time, colour="chartreuse2")
  }
  
  plot(pricePlot)
  #print(tradesOpened)
  #print(tradesClosed)
  return(profit)
}



profits6 <- c()
for (i in 1:800){
  print(i)
  profits6 <- c(profits6, movingAverageMomentum(data[[i]], 20))
  print(mean(profits6))
}

mean()
ggplot(data.frame(profits6), aes(x=profits6)) + geom_histogram() +
  ggtitle("Histogram of Profits with Moving Average Trading Method") + 
  xlab("Profits") + xlim(-20,10)
write.csv(profits4,file="profitsMAMomentumRun.csv",row.names=F)



df_ma <- data.frame(matrix(ncol = 4, nrow = 0))
names <- c("timeToStart", "time", "bckMa", "layMa")
colnames(df_ma) <- names


newRow <- c(df_fav$callTime_GMT[5], "b", 4, 8) 
df_ma <- rbind(df_ma, newRow)
colnames(df_ma) <- names

