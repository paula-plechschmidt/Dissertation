## Moving average strategy that corresponds to the trade logic of strategy 3

movingAverageMomentum3 <- function(df, k){
  
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
  endOdds <- 0
  profit <- 0
  
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
          #print("---------------------------------------------------------------")
          #print("Starting trade by backing")
          #print(time)
          #print(paste0("average spread ", averageSpread))
          startOdds <- df_fav$bckPrc1[count] #saving actual (not moving average) starting odds
          #print(startOdds)
          #print(df_fav[df_fav$timeToStart == time,]$callTime_GMT)
          backThenLay <- TRUE #seting the trading method
          tradeStart <- newRow[2] #saving the time at which the trade started
          bestOdds <- newRow[4] #savng best laying odds we can obtain to close trade
          startBckTradeTime <- c(startBckTradeTime, df_fav[df_fav$timeToStart == time,]$callTime_GMT) #adding this trade to the times trades have been started
          tradeActive <- TRUE #setting trade to active
        }
        #... and the moving average of the laying prices is increasing over a minute
        else if((newRow[4] > (df_ma[df_ma$timeToStart == (timeMin60),]$layMa[1] + averageSpread))){
          #...we enter a trade
          #print("--------------------------------------------------------------- ")
          #print("Starting trade by laying")
          #print(paste0("average spread ", averageSpread))
          #print(time)
          startOdds <- df_fav$layPrc1[count]
          #print(startOdds)
          #print(df_fav[df_fav$timeToStart == time,]$callTime_GMT)
          backThenLay <- FALSE
          tradeStart <- newRow[2]
          bestOdds <- newRow[3]
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
        
        ### BACK THEN LAY ###
        if(backThenLay){
          if (time == max(df_fav$timeToStart)){
            #print("End of pre-race betting!")
            #print(df_fav[df_fav$timeToStart == time,]$callTime_GMT)
            endOdds <- df_fav$layPrc1[count]
            #print(endOdds)
            profit <- profit + tradeProfit(10, startOdds, endOdds, backThenLay)
            #print(tradeProfit(10, startOdds, endOdds, backThenLay))
            bestOdds <- 0
            endLayTradeTime <- c(endLayTradeTime, df_fav[df_fav$timeToStart == time,]$callTime_GMT)
            tradeActive <- FALSE
          }
          # If we covered the spread (i.e the current laying odds are below the initial backing odds)
          spreadCovered = currentLayOdds < startOdds
          if(spreadCovered){
            # If the current laying odds are smaller than the best laying odds saved, we update them
            if(currentLayOdds < bestOdds){
              bestOdds <- currentLayOdds
            }
            # We let it run for 30 seconds and then (maybe?? not for now)
            # Calculate diff1 = initial backing odds - current laying odds
            # Calculate diff2 = initial backing odds - best odds
            # If diff1 < 0.75*diff2 we end the trade
            diff1 <- startOdds - currentLayOdds
            #print(paste0("diff1 ", diff1))
            diff2 <- startOdds - bestOdds
            #print(paste0("diff2 ", diff2))
            if(diff1 < 0.7*diff2){
              #print("Loss too large so we lay!")
              #print(df_fav[df_fav$timeToStart == time,]$callTime_GMT)
              endOdds <- df_fav$layPrc1[count]
              #print(endOdds)
              profit <- profit + tradeProfit(10, startOdds, endOdds, backThenLay)
              #print(tradeProfit(10, startOdds, endOdds, backThenLay))
              bestOdds <- 0
              endLayTradeTime <- c(endLayTradeTime, df_fav[df_fav$timeToStart == time,]$callTime_GMT)
              tradeActive <- FALSE
            }
          }
          # If we didn't cover the spread
          else{
            # If the current laying odds are larger than the initial laying odds + average spread
            if(currentLayOdds > df_ma[df_ma$timeToStart == tradeStart,]$layMa[1] + averageSpread){
              #print("Odds too far out of line")
              #print(df_fav[df_fav$timeToStart == time,]$callTime_GMT)
              endOdds <- df_fav$layPrc1[count]
              #print(endOdds)
              profit <- profit + tradeProfit(10, startOdds, endOdds, backThenLay)
              #print(tradeProfit(10, startOdds, endOdds, backThenLay))
              bestOdds <- 0
              endLayTradeTime <- c(endLayTradeTime, df_fav[df_fav$timeToStart == time,]$callTime_GMT)
              tradeActive <- FALSE
            }
            # we end the trade
          }
        }
        ### LAY THEN BACK ###
        else {
          if (time == max(df_fav$timeToStart)){
            #print("End of pre-race betting!")
            #print(df_fav[df_fav$timeToStart == time,]$callTime_GMT)
            endOdds <- df_fav$bckPrc1[count]
            #print(endOdds)
            profit <- profit + tradeProfit(10, startOdds, endOdds, backThenLay)
            #print(tradeProfit(10, startOdds, endOdds, backThenLay))
            bestOdds <- 0
            endBckTradeTime <- c(endBckTradeTime, df_fav[df_fav$timeToStart == time,]$callTime_GMT)
            tradeActive <- FALSE
          }
          # If we covered the spread (i.e. the current backing odds are above the initial laying odds)
          spreadCovered = currentBckOdds > startOdds
          if (spreadCovered){
            # If the current backing odds are larger than the best odds then we update them 
            if (currentBckOdds > bestOdds){
              bestOdds <- currentBckOdds
            }
            # calculate diff1 = current backing odds - initial laying odds
            # Calculate diff2 = best odds - initial laying odds
            # If diff1 < 0.75*diff2 we end the trade
            diff1 = currentBckOdds - startOdds
            #print(paste0("diff1 ", diff1))
            diff2 = bestOdds - startOdds
            #print(paste0("diff2 ", diff2))
            if (diff1 < 0.7*diff2){
              #print("Loss to large so we back!")
              #print(df_fav[df_fav$timeToStart == time,]$callTime_GMT)
              endOdds <- df_fav$bckPrc1[count]
              #print(endOdds)
              profit <- profit + tradeProfit(10, startOdds, endOdds, backThenLay)
              #print(tradeProfit(10, startOdds, endOdds, backThenLay))
              bestOdds <- 0
              endBckTradeTime <- c(endBckTradeTime, df_fav[df_fav$timeToStart == time,]$callTime_GMT)
              tradeActive <- FALSE
            }
          }
          else{
            #If the current backing odds are smaller than the initial backing odds - average spread
            if(currentBckOdds < df_ma[df_ma$timeToStart == tradeStart,]$bckMa[1] - averageSpread){
              #print("Odds too far out of line")
              #print(df_fav[df_fav$timeToStart == time,]$callTime_GMT)
              endOdds <- df_fav$bckPrc1[count]
              #print(endOdds)
              profit <- profit + tradeProfit(10, startOdds, endOdds, backThenLay)
              #print(tradeProfit(10, startOdds, endOdds, backThenLay))
              bestOdds <- 0
              endBckTradeTime <- c(endBckTradeTime, df_fav[df_fav$timeToStart == time,]$callTime_GMT)
              tradeActive <- FALSE
            }
          }
        }
      }
    }
    count <- count + 1
  }
  
  '
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
  '
  return(profit)
}


#With 0.7 as reversal criteria
profitsMA1 <- c()
for (i in 692:700){
  print(i)
  profitsMA1 <- c(profitsMA1, movingAverageMomentum3(data[[i]], 20))
  print(mean(profitsMA1))
}
ggplot(data.frame(profitsMA1), aes(x=profitsMA1)) + geom_histogram() +
  ggtitle("Histogram of Profits with Adjusted Moving Average Trading Method") + 
  xlab("Profits") + xlim(-15,10)



