fileName <- "rawPriceHistoryItems.csv"
lastTimestamp = getMaxTimestamp(fileName)
newFile = FALSE

if(is.null(lastTimestamp)){
  lastTimestamp = 0
  newFile = TRUE
}

repeat{
  qry <- Query$new()
  query = str_squish(str_c('query { priceHistoryItems(first:1000 orderBy: timestamp where: {timestamp_gt:',lastTimestamp,'}){
                           id,
                           asset{id},
                           price,
                           timestamp
                           }}',sep = ''))
  
  qry$query("priceHistoryItems", query)
  
  response <- con$exec(qry$queries$priceHistoryItems) %>% fromJSON()
  
  if(!is.null(response$errors)){
    next
  }
  if(length(response$data$priceHistoryItems) == 0){
    break
  }
  responseData <- response$data$priceHistoryItems %>%
    mutate(id = as.character(id),
           assetID = as.character(asset$id)) %>%
    select(-asset)
  
  responseData[responseData == "NULL"] <- NA
  
  lastTimestamp = max(responseData$timestamp)
  
  write_csv(responseData, paste0(dataPath, fileName), append = !newFile)
  
  newFile = FALSE
  
  if(length(responseData$id) < 1000){
    break
  } 
}