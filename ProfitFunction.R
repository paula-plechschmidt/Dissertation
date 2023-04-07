secondStake <- function(stake, prevOdds, crntOdds){
  return ((prevOdds/crntOdds)*stake)
}

tradeProfit <- function(stake, prevOdds, crntOdds, backThenLay){
  secondStake <- secondStake(stake, prevOdds, crntOdds)
  if(backThenLay){
    return(secondStake - stake)
  }else{
    return(stake - secondStake)
  }
}

#choose favorite at given time
#for start of time us min(df$timeToStart)
favourite <- function(df, time){
  startData <- df[df$timeToStart == time,]
  minOdds <- min(startData$bckPrc1)
  #Check if we are returning two, if yes take the first
  return(startData[startData$bckPrc1 == minOdds,]$selectionId)
}


underdog <- function(df, time){
  startData <- df[df$timeToStart == time,]
  maxOdds <- max(startData$bckPrc1)
  #Check if we are returning two, if yes take the first
  return(startData[startData$bckPrc1 == maxOdds,]$selectionId)
}

secondFavourite <- function(df, time){
  startData <- df[df$timeToStart == time,]
  bckPrc1 <- df[df$timeToStart == time,]$bckPrc1
  n <- length(bckPrc1)
  odds <- sort(bckPrc1,partial=n-1)[n-1]
  return(startData[startData$bckPrc1 == odds,]$selectionId)
}


