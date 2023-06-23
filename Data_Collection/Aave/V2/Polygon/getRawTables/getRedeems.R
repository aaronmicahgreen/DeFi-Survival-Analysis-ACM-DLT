fileName <- "rawRedeems.csv"
lastTimestamp = getMaxTimestamp(fileName)
newFile = FALSE

if(is.null(lastTimestamp)){
  lastTimestamp = 0
  newFile = TRUE
}

repeat{
  qry <- Query$new()
  query = str_squish(str_c('query { redeemUnderlyings(first:1000 orderBy: timestamp where: {timestamp_gt:',lastTimestamp,'}){
                           id,
                           user{id},
                           to{id},
                           reserve{id},
                           pool{id},
                           timestamp, 
                           amount
                           }}',sep = ''))
  
  qry$query("redeems", query)
  
  response <- con$exec(qry$queries$redeems) %>% fromJSON()
  
  if(!is.null(response$errors)){
    next
  }
  if(length(response$data$redeemUnderlyings) == 0){
    break
  }
  
  responseData <- response$data$redeemUnderlyings %>%
    mutate(userID = as.character(user$id), 
           onBehalfOfID = as.character(to$id), 
           reserveID = as.character(reserve$id), 
           poolID = as.character(pool$id)) %>%
<<<<<<< HEAD
    dplyr::select(-user, -to, -reserve, -pool)
=======
    select(-user, -to, -reserve, -pool)
>>>>>>> 46db68c5bd8a0f571a4ebdf5b28bf1adad7a986f
  
  responseData[responseData == "NULL"] <- NA
  
  lastTimestamp = max(responseData$timestamp)
  
  write_csv(responseData, paste0(dataPath, fileName), append = !newFile)
  
  newFile = FALSE
  
  if(length(responseData$id) < 1000){
    break
  } 
}