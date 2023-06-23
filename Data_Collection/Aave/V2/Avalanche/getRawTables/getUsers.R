fileName <- "rawUsers.csv"
newFile = TRUE
lastID = 0
repeat{
  qry <- Query$new()
  query = str_squish(str_c('query { users(first:1000 orderBy: id where: {id_gt:',lastID,'}){
                           id,
                           borrowedReservesCount,
                           unclaimedRewards,
                           lifetimeRewards,
                           incentivesLastUpdated
                           }}',sep = ''))
  
  qry$query("users", query)
  
  response <- con$exec(qry$queries$users) %>% fromJSON()
  
  if(!is.null(response$errors)){
    next
  }
  if(length(response$data$users) == 0){
    break
  }
  
  responseData <- response$data$users %>%
    mutate(id = as.character(id))
  
  responseData[responseData == "NULL"] <- NA
  
  lastID <- paste0("\"", as.character(tail(responseData$id, 1)), "\"")
  
  write_csv(responseData, paste0(dataPath, fileName), append = !newFile)
  
  newFile = FALSE
  
  if(length(responseData$id) < 1000){
    break
  } 
}