## First, let's check for the existence of repaysAave.csv and see if we need to get all borrows or just the more recent borrows:
fileName <- "rawRepays.csv"
lastTimestamp = getMaxTimestamp(fileName)
newFile = FALSE

if(is.null(lastTimestamp)){
  lastTimestamp = 0
  newFile = TRUE
}

repeat{
  qry <- Query$new()
  query = str_squish(str_c('query { repays(first:1000 orderBy: timestamp where: {timestamp_gt:',lastTimestamp,'}){
                           id, timestamp, amount,
                           user{id},
                           repayer{id},
                           reserve{id},
                           pool{id}
                           }}',sep = ''))
  
  qry$query("repays", query)
  
  response <- con$exec(qry$queries$repays) %>% fromJSON()
  
  if(!is.null(response$errors)){
    next
  }
  if(length(response$data$repays) == 0){
    break
  }
  responseData <- response$data$repays %>%
    mutate(id = as.character(id),
           userID = as.character(user$id), 
           onBehalfOfID = as.character(repayer$id), 
           reserveID = as.character(reserve$id), 
           poolID = as.character(pool$id)) %>%
    select(-user, -repayer, -reserve, -pool)
  
  responseData[responseData == "NULL"] <- NA
  
  lastTimestamp = max(responseData$timestamp)
  
  write_csv(responseData, paste0(dataPath, fileName), append = !newFile)
  
  newFile = FALSE
  
  if(length(responseData$id) < 1000){
    break
  } 
}