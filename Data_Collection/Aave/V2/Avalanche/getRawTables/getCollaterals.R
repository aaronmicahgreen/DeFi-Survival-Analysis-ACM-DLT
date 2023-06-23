fileName <- "rawCollaterals.csv"
lastTimestamp = getMaxTimestamp(fileName)
newFile = FALSE

if(is.null(lastTimestamp)){
  lastTimestamp = 0
  newFile = TRUE
}

repeat{
  qry <- Query$new()
  query = str_squish(str_c('query { usageAsCollaterals(first:1000 orderBy: timestamp where: {timestamp_gt:',lastTimestamp,'}){
                           id,
                           user{id},
                           reserve{id},
                           pool{id},
                           timestamp, 
                           fromState,
                           toState
                           }}',sep = ''))
  
  qry$query("collaterals", query)
  
  response <- con$exec(qry$queries$collaterals) %>% fromJSON()
  
  if(!is.null(response$errors)){
    next
  }
  if(length(response$data$usageAsCollaterals) == 0){
    break
  }
  
  responseData <- response$data$usageAsCollaterals %>%
    mutate(userID = as.character(user$id),
           reserveID = as.character(reserve$id), 
           poolID = as.character(pool$id)) %>%
    select(-user,  -reserve, -pool)
  
  responseData[responseData == "NULL"] <- NA
  
  lastTimestamp = max(responseData$timestamp)
  
  write_csv(responseData, paste0(dataPath, fileName), append = !newFile)
  
  newFile = FALSE
  
  if(length(responseData$id) < 1000){
    break
  } 
}