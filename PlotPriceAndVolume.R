library(ggplot2)
require(gridExtra)

# Functions to plot the back and lay prices and volumes for each of the horses
plotBckPriceAndVolume <- function(df){
  df$selectionId <- as.factor(df$selectionId)
  
  bckOddsPlot <- ggplot(data=df, aes(x=callTime_GMT, y=bckPrc1, group=selectionId)) +
    geom_line(aes(color=selectionId)) + xlab("time") + ylab("Odds")
  bckVolumesPlot <-  ggplot(data=df, aes(x=callTime_GMT, y=bckSz1, group=selectionId)) +
    geom_line(aes(color=selectionId)) + xlab("time") + ylab("Volume")
  
  grid.arrange(bckOddsPlot, bckVolumesPlot, ncol=2)
}

plotLayPriceAndVolume <- function(df){
  df$selectionId <- as.factor(df$selectionId)
  
  layOddsPlot <- ggplot(data=df, aes(x=callTime_GMT, y=layPrc1, group=selectionId)) +
    geom_line(aes(color=selectionId))
  layVolumesPlot <-  ggplot(data=df, aes(x=callTime_GMT, y=laySz1, group=selectionId)) +
    geom_line(aes(color=selectionId))
  
  grid.arrange(layOddsPlot, layVolumesPlot, ncol=2)
}


plotLayBckPriceFavourite <- function(df){
  fav <- favourite(df, min(df$timeToStart))[1]
  df_fav <- df[df$selectionId == fav,]
  
  return(ggplot() + geom_line(data=df_fav, aes(x=callTime_GMT, y=layPrc1), color = "blue") +
    geom_line(data=df_fav, aes(x=callTime_GMT, y=bckPrc1), color = "orange") + xlab("Dates") + ylab("Odds"))
}


plotOddsSMAFavourite <- function(df, k){
  fav <- favourite(df, min(df$timeToStart))[1]
  df_fav <- df[df$selectionId == fav,]
  df_fav$bckMovingAverage <- sma(k, df_fav$bckPrc1)
  df_fav$layMovingAverage <- sma(k, df_fav$layPrc1)

  plot(ggplot() + geom_line(data=df_fav, aes(x=callTime_GMT, y=layPrc1), color = "blue") +
         geom_line(data=df_fav, aes(x=callTime_GMT, y=bckPrc1), color = "orange") + 
         geom_line(data=df_fav, aes(x=callTime_GMT, y=bckMovingAverage), color = "orangered3") +
         geom_line(data=df_fav, aes(x=callTime_GMT, y=layMovingAverage), color = "turquoise") +
         xlab("Dates") + ylab("Odds")) + ggtitle("Moving average for backing and laying odds of the favourite horse")
}

