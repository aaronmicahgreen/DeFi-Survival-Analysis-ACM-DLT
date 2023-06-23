## First, let's check for the existence of borrowsAave.csv and see if we need to get all borrows or just the more recent borrows:
fileName <- "rawSwaps.csv"
lastTimestamp = getMaxTimestamp(fileName)
newFile = FALSE

if(is.null(lastTimestamp)){
  lastTimestamp = 0
  newFile = TRUE
}

repeat{
  qry <- Query$new()
  query = str_squish(str_c('query { swaps(first:1000 orderBy: timestamp where: {timestamp_gt:',lastTimestamp,'}){
                           id,
                           user{id},
                           reserve{id},
                           pool{id},
                           timestamp,
                           borrowRateModeTo,
                           borrowRateModeFrom,
                           stableBorrowRate,
                           variableBorrowRate
                           }}',sep = ''))
  
  qry$query("swaps", query)
  
  response <- con$exec(qry$queries$swaps) %>% fromJSON()
  
  if(!is.null(response$errors)){
    next
  }
  
  if(length(response$data$swaps) == 0){
    break
  }
  
  responseData <- response$data$swaps %>%
    mutate(userID = as.character(user$id), 
           reserveID = as.character(reserve$id), 
           poolID = as.character(pool$id)) %>%
<<<<<<< HEAD
    dplyr::select(-user, -reserve, -pool)
=======
    select(-user, -reserve, -pool)
>>>>>>> 46db68c5bd8a0f571a4ebdf5b28bf1adad7a986f
  
  responseData[responseData == "NULL"] <- NA
  
  lastTimestamp = max(responseData$timestamp)
  
  write_csv(responseData, paste0(dataPath, fileName), append = !newFile)
  
  newFile = FALSE
  
  if(length(responseData$id) < 1000){
    break
  } 
}