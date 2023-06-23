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
                           id,
                           user{id},
                           repayer{id},
                           reserve{id},
                           pool{id},
                           timestamp, 
                           amount
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
    mutate(userID = as.character(user$id), 
           onBehalfOfID = as.character(repayer$id), 
           reserveID = as.character(reserve$id), 
           poolID = as.character(pool$id)) %>%
<<<<<<< HEAD
    dplyr::select(-user, -repayer, -reserve, -pool)
=======
    select(-user, -repayer, -reserve, -pool)
>>>>>>> 46db68c5bd8a0f571a4ebdf5b28bf1adad7a986f
  
  responseData[responseData == "NULL"] <- NA
  
  lastTimestamp = max(responseData$timestamp)
  
  write_csv(responseData, paste0(dataPath, fileName), append = !newFile)
  
  newFile = FALSE
  
  if(length(responseData$id) < 1000){
    break
  } 
}