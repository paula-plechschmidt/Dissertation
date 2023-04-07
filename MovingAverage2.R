
movingAverageMomentum2 <- function(df, k){
  
  # Get the favourites data
  fav <- favourite(df, min(df$timeToStart))[1] #gives the selection id of the favourite
  df_fav <- df[df$selectionId == fav,] #returns only the rows of the favourite
  rownames(df_fav) <- NULL #reindexes subsetted dataframe
  df_fav$timeToStart <- as.numeric(df_fav$timeToStart) #reformat the time to start
  
  # Matrix for Moving Average
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
  
  maxDiffOdds <- 0
  bestOdds <- 0
  
  count <- 0
  
  spread <- c()
  startBckTradeTime <- c()
  endBckTradeTime <- c()
  startLayTradeTime <- c()
  endLayTradeTime <- c()
  
  # Loop through all the times in the race
  for (time in df_fav$timeToStart){
    
    # Calculating moving average for each time
    #starting to calculate when we have more than k existing time points
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
      spread <- c(spread, (df_fav[count,]$layPrc1 - df_fav[count,]$bckPrc1))
      averageSpread <- mean(spread)
      #averageSpread <- (averageSpread*(count-1) + (df_fav[count,]$layPrc1 - df_fav[count,]$bckPrc1))/count
      
      # Adjusting times to make due with duplicate/missing times
      # 60 seconds before current time
      timeMin60 <- time - 60
      # 30 seconds before current time
      timeMin30 <- time - 30
      # 1 minute past the market open
      marketOpenPlus60 <- marketOpen
      
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
        #... and the moving average of the backing prices is decreasing over a minute
        if((newRow[3] < (df_ma[df_ma$timeToStart == timeMin60,]$bckMa[1] - averageSpread))){
          #...we enter a trade
          print("---------------------------------------------------------------")
          print("Starting trade by backing")
          print(time)
          print(paste0("average spread ", averageSpread))
          startOdds <- df_fav$bckPrc1[count] #saving actual (not moving average) starting odds
          print(startOdds)
          print(df_fav[df_fav$timeToStart == time,]$callTime_GMT)
          backThenLay <- TRUE #seting the trading method
          tradeStart <- newRow[2] #saving the time at which the trade started
          bestOdds <- newRow[4] #savng best laying odds we can obtain to close trade
          maxDiffOdds <- newRow[3] - newRow[4] #saving the spread we are able to achieve
          startBckTradeTime <- c(startBckTradeTime, df_fav[df_fav$timeToStart == time,]$callTime_GMT) #adding this trade to the times trades have been started
          tradeActive <- TRUE #setting trade to active
        }
        #... and the moving average of the laying prices is increasing over a minute
        else if((newRow[4] > (df_ma[df_ma$timeToStart == (timeMin60),]$layMa[1] + averageSpread))){
          #...we enter a trade
          print("--------------------------------------------------------------- ")
          print("Starting trade by laying")
          print(paste0("average spread ", averageSpread))
          print(time)
          startOdds <- df_fav$layPrc1[count]
          print(startOdds)
          print(df_fav[df_fav$timeToStart == time,]$callTime_GMT)
          backThenLay <- FALSE
          tradeStart <- newRow[2]
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
        currentBckOdds <- newRow[3] #MA backing odds at point in time
        currentLayOdds <- newRow[4] #MA laying odds at point in time
        
        #To handle if there are missing times
        tradeStart30 <- tradeStart + 30
        if(!(tradeStart30 %in% df_ma$timeToStart)){
          tradeStart30 <- tradeStart30 + 1
        }
        tradeStart60 <- tradeStart + 60
        if(!(tradeStart60 %in% df_ma$timeToStart)){
          tradeStart60 <- tradeStart60 + 1
        }
        
        #.. and if we backed first
        if(backThenLay){
          # we quit the trade if the laying price has increased by more than the average spread during the first 30 sec
          if((time <= tradeStart60) & (newRow[4] > df_ma[df_ma$timeToStart == tradeStart,]$layMa[1] + averageSpread)){
            print('laying price increased more than average spread in first 30 seconds')
            print(df_fav[df_fav$timeToStart == time,]$callTime_GMT)
            endOdds <- df_fav$layPrc1[count]
            print(endOdds)
            profit <- profit + tradeProfit(10, startOdds, endOdds, backThenLay)
            print(tradeProfit(10, startOdds, endOdds, backThenLay))
            maxDiffOdds <- 0
            bestOdds <- 0
            endLayTradeTime <- c(endLayTradeTime, df_fav[df_fav$timeToStart == time,]$callTime_GMT)
            tradeActive <- FALSE
          }
          # we quit the trade if we have not covered the spread in the first 30 seconds
          '
          if((time == tradeStart60) & (newRow[4] > df_ma[df_ma$timeToStart == tradeStart,]$bckMa[1])){
            print("Spread not covered so we lay!")
            print(df_fav[df_fav$timeToStart == time,]$callTime_GMT)
            endOdds <- df_fav$layPrc1[count]
            print(endOdds)
            profit <- profit + tradeProfit(10, startOdds, endOdds, backThenLay)
            print(tradeProfit(10, startOdds, endOdds, backThenLay))
            maxDiffOdds <- 0
            bestOdds <- 0
            endLayTradeTime <- c(endLayTradeTime, df_fav[df_fav$timeToStart == time,]$callTime_GMT)
            tradeActive <- FALSE
          }'
          
          possibleProfit <- startOdds - currentLayOdds
          if (possibleProfit > maxDiffOdds){
            maxDiffOdds <- possibleProfit
            bestOdds <- currentLayOdds
          }
          # if we are more than 30 seconds into the trade and there has been a loss
          # larger than 25% of the maximum profit we could have achieved we quit the trade
          if (((currentLayOdds - bestOdds) >= 0.25*maxDiffOdds) &
              (time > tradeStart60)){
            print("Loss too large so we lay!")
            print(df_fav[df_fav$timeToStart == time,]$callTime_GMT)
            endOdds <- df_fav$layPrc1[count]
            print(endOdds)
            profit <- profit + tradeProfit(10, startOdds, endOdds, backThenLay)
            print(tradeProfit(10, startOdds, endOdds, backThenLay))
            maxDiffOdds <- 0
            bestOdds <- 0
            endLayTradeTime <- c(endLayTradeTime, df_fav[df_fav$timeToStart == time,]$callTime_GMT)
            tradeActive <- FALSE
          }
        } 
        #.. and if we layed first
        else {
          # we quit the trade if the backing price has decreased by more than the average spread during the first 30 sec
          if((time <= tradeStart60) & (newRow[3] < df_ma[df_ma$timeToStart == tradeStart,]$bckMa[1] - averageSpread)){
            print('laying price increased more than average spread in first 30 seconds')
            print(df_fav[df_fav$timeToStart == time,]$callTime_GMT)
            endOdds <- df_fav$bckPrc1[count]
            print(endOdds)
            profit <- profit + tradeProfit(10, startOdds, endOdds, backThenLay)
            print(tradeProfit(10, startOdds, endOdds, backThenLay))
            maxDiffOdds <- 0
            bestOdds <- 0
            endLayTradeTime <- c(endLayTradeTime, df_fav[df_fav$timeToStart == time,]$callTime_GMT)
            tradeActive <- FALSE
          }
          # we quit the trade if we have not covered the spread in the first 30 seconds
          '
          if((time == tradeStart60) & (newRow[3] < df_ma[df_ma$timeToStart == tradeStart,]$layMa[1])){
            print("Spread not covered so we back!")
            print(df_fav[df_fav$timeToStart == time,]$callTime_GMT)
            endOdds <- df_fav$bckPrc1[count]
            print(endOdds)
            profit <- profit + tradeProfit(10, startOdds, endOdds, backThenLay)
            print(tradeProfit(10, startOdds, endOdds, backThenLay))
            maxDiffOdds <- 0
            bestOdds <- 0
            endBckTradeTime <- c(endBckTradeTime, df_fav[df_fav$timeToStart == time,]$callTime_GMT)
            tradeActive <- FALSE
          }'
          
          print("***************")
          possibleProfit <- currentBckOdds - startOdds
          print(possibleProfit)
          if (possibleProfit > maxDiffOdds){
            maxDiffOdds <- possibleProfit
            bestOdds <- currentBckOdds
          }
          print(maxDiffOdds)
          print(maxDiffOdds*0.25)
          print(paste0(bestOdds, " - ", currentBckOdds, " = ", bestOdds - currentBckOdds))
          print("****************")
          # if we are more than 30 seconds into the trade and there has been a loss
          # larger than 25% of the maximum profit we could have achieved we quit the trade
          if (((bestOdds - currentBckOdds) > 0.25*maxDiffOdds) &
              (time > tradeStart60)){
            print("Loss to large so we back!")
            print(df_fav[df_fav$timeToStart == time,]$callTime_GMT)
            endOdds <- df_fav$bckPrc1[count]
            print(endOdds)
            profit <- profit + tradeProfit(10, startOdds, endOdds, backThenLay)
            print(tradeProfit(10, startOdds, endOdds, backThenLay))
            maxDiffOdds <- 0
            bestOdds <- 0
            endBckTradeTime <- c(endBckTradeTime, df_fav[df_fav$timeToStart == time,]$callTime_GMT)
            tradeActive <- FALSE
          }
        }
      }
    }
    count <- count + 1
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
  return(profit)
}
