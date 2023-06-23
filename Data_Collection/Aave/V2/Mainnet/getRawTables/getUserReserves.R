## First, let's check for the existence of borrowsAave.csv and see if we need to get all borrows or just the more recent borrows:
fileName <- "rawUserReserves.csv"
lastTimestamp = getMaxTimestamp(fileName)
newFile = FALSE

if(is.null(lastTimestamp)){
  lastTimestamp = 0
  newFile = TRUE
}

repeat{
  qry <- Query$new()
  query = str_squish(str_c('query { userReserves(first:1000 orderBy: lastUpdateTimestamp where: {lastUpdateTimestamp_gt:',lastTimestamp,'}){
                           id,
                           pool{id},
                           reserve{id},
                           user{id},
                           usageAsCollateralEnabledOnUser,
                           scaledATokenBalance,
                           currentATokenBalance,
                           scaledVariableDebt,
                           currentVariableDebt,
                           principalStableDebt,
                           currentStableDebt,
                           currentTotalDebt,
                           stableBorrowRate,
                           oldStableBorrowRate,
                           liquidityRate,
                           stableBorrowLastUpdateTimestamp,
                           variableBorrowIndex,
                           aTokenincentivesUserIndex,
                           vTokenincentivesUserIndex,
                           sTokenincentivesUserIndex,
                           aIncentivesLastUpdateTimestamp,
                           vIncentivesLastUpdateTimestamp,
                           sIncentivesLastUpdateTimestamp,
                           lastUpdateTimestamp
                           }}',sep = ''))
  
  qry$query("userReserves", query)
  
  response <- con$exec(qry$queries$userReserves) %>% fromJSON()
  
  if(!is.null(response$errors)){
    next
  }
  if(length(response$data$userReserves) == 0){
    break
  }
  
  responseData <- response$data$userReserves %>%
    rename(timestamp = lastUpdateTimestamp) %>%
    mutate(poolID = as.character(pool$id),
           reserveID = as.character(reserve$id),
           userID = as.character(user$id)) %>%
    dplyr::select(-pool, -reserve, -user)
  
  responseData[responseData == "NULL"] <- NA
  
  lastTimestamp = max(responseData$timestamp)
  
  write_csv(responseData, paste0(dataPath, fileName), append = !newFile)
  
  newFile = FALSE
  
  if(length(responseData$id) < 1000){
    break
  } 
}