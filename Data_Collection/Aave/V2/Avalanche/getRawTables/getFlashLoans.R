## First, let's check for the existence of borrowsAave.csv and see if we need to get all borrows or just the more recent borrows:
fileName <- "rawFlashLoans.csv"
lastTimestamp = getMaxTimestamp(fileName)
newFile = FALSE

if(is.null(lastTimestamp)){
  lastTimestamp = 0
  newFile = TRUE
}

repeat{
  qry <- Query$new()
  query = str_squish(str_c('query { flashLoans(first:1000 orderBy: timestamp where: {timestamp_gt:',lastTimestamp,'}){
                           id,
                           pool{id},
                           reserve{id},
                           target,
                           amount,
                           totalFee,
                           initiator,
                           timestamp
                           }}',sep = ''))
  
  qry$query("flashLoans", query)
  
  response <- con$exec(qry$queries$flashLoans) %>% fromJSON()
  
  if(!is.null(response$errors)){
    next
  }
  if(length(response$data$flashLoans) == 0){
    break
  }
  responseData <- response$data$flashLoans %>%
    mutate(reserveID = as.character(reserve$id), 
           poolID = as.character(pool$id)) %>%
    select(-reserve, -pool)
  
  responseData[responseData == "NULL"] <- NA
  
  lastTimestamp = max(responseData$timestamp)
  
  write_csv(responseData, paste0(dataPath, fileName), append = !newFile)
  
  newFile = FALSE
  
  if(length(responseData$id) < 1000){
    break
  } 
}