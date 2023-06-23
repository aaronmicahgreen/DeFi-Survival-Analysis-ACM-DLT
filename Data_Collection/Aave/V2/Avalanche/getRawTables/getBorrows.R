## First, let's check for the existence of borrowsAave.csv and see if we need to get all borrows or just the more recent borrows:
fileName <- "rawBorrows.csv"
lastTimestamp = getMaxTimestamp(fileName)
newFile = FALSE

if(is.null(lastTimestamp)){
  lastTimestamp = 0
  newFile = TRUE
}

repeat{
  qry <- Query$new()
  query = str_squish(str_c('query { borrows(first:1000 orderBy: timestamp where: {timestamp_gt:',lastTimestamp,'}){
                           id,
                           user{id},
                           caller{id},
                           reserve{id},
                           pool{id},
                           timestamp, 
                           amount, 
                           borrowRate, 
                           borrowRateMode,
                           stableTokenDebt,
                           variableTokenDebt
                           }}',sep = ''))
  
  qry$query("borrows", query)
  
  response <- con$exec(qry$queries$borrows) %>% fromJSON()
  
  if(!is.null(response$errors)){
    next
  }
  if(length(response$data$borrows) == 0){
    break
  }
  
  responseData <- response$data$borrows %>%
    mutate(userID = as.character(user$id), 
           onBehalfOfID = as.character(caller$id), 
           reserveID = as.character(reserve$id), 
           poolID = as.character(pool$id)) %>%
    select(-user, -caller, -reserve, -pool)
  
  responseData[responseData == "NULL"] <- NA
  
  lastTimestamp = max(responseData$timestamp)
  
  write_csv(responseData, paste0(dataPath, fileName), append = !newFile)
  
  newFile = FALSE
  
  if(length(responseData$id) < 1000){
    break
  } 
}