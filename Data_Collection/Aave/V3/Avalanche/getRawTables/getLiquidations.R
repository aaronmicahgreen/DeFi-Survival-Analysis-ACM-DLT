## First, let's check for the existence of borrowsAave.csv and see if we need to get all borrows or just the more recent borrows:
fileName <- "rawLiquidations.csv"
lastTimestamp = getMaxTimestamp(fileName)
newFile = FALSE

if(is.null(lastTimestamp)){
  lastTimestamp = 0
  newFile = TRUE
}

repeat{
  qry <- Query$new()
  query = str_squish(str_c('query { liquidationCalls(first:1000 orderBy: timestamp where: {timestamp_gt:',lastTimestamp,'}){
                           id, timestamp, principalAmount, collateralAmount,
                           user{id},
                           liquidator,
                           principalReserve{id},
                           collateralReserve{id},
                           pool{id}
                           }}',sep = ''))
  
  qry$query("liquidations", query)
  
  response <- con$exec(qry$queries$liquidations) %>% fromJSON()
  
  if(!is.null(response$errors)){
    next
  }
  
  if(length(response$data$liquidationCalls) == 0){
    break
  }
  
  responseData <- response$data$liquidationCalls %>%
    mutate(id = as.character(id),
           userID = as.character(user$id), 
           collateralReserveID = as.character(collateralReserve$id), 
           principalReserveID = as.character(principalReserve$id), 
           poolID = as.character(pool$id)) %>%
    select(-user, -pool, -collateralReserve, -principalReserve)
  
  responseData[responseData == "NULL"] <- NA
  
  lastTimestamp = max(responseData$timestamp)
  
  write_csv(responseData, paste0(dataPath, fileName), append = !newFile)
  
  newFile = FALSE
  
  if(length(responseData$id) < 1000){
    break
  } 
}