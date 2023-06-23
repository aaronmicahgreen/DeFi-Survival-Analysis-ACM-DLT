# The files for each transaction type:
borrowsFile = "rawBorrows.csv"
collateralsFile = "rawCollaterals.csv"
depositsFile = "rawDeposits.csv"
liquidationsFile = "rawLiquidations.csv"
redeemsFile = "rawRedeems.csv"
repaysFile = "rawRepays.csv"
swapsFile = "rawSwaps.csv"
flashLoansFile = "rawFlashLoans.csv"

# Related files to include reserve information:
reservesFile = "rawReserves.csv"
reserveParamsHistoryFile = "rawReserveParamsHistory.csv"

# Load the raw transaction tables:
rawBorrows <- read_csv(paste(rawDataPath, borrowsFile, sep=""))
rawCollaterals <- read_csv(paste(rawDataPath, collateralsFile, sep = ""))
rawDeposits <- read_csv(paste(rawDataPath, depositsFile, sep=""))
rawLiquidations <- read_csv(paste(rawDataPath, liquidationsFile, sep=""))
rawRedeems <- read_csv(paste(rawDataPath, redeemsFile, sep=""))
rawRepays <- read_csv(paste(rawDataPath, repaysFile, sep=""))
rawFlashLoans <- read_csv(paste0(rawDataPath, flashLoansFile))

# Load the raw reserve information:
rawReserveInfo <- read_csv(paste0(rawDataPath, reservesFile))
rawReserveParamsHistory <- read_csv(paste0(rawDataPath, reserveParamsHistoryFile))

# We will add the names of the deployed contracts to the aliases:
deployedContracts <- read_csv(paste0(rawDataPath, "deployedContracts.csv"))

deployedContracts <- deployedContracts %>%
  rename(alias = name,
         id = address)

aliases <- userAliases %>%
  select(id, alias) %>%
  bind_rows(deployedContracts)

reserveInfo <- rawReserveInfo %>%
  select(id,
         symbol,
         decimals)

reserveParamsHistory <- rawReserveParamsHistory %>%
  mutate(txID = str_sub(id, start = 1, end = 66)) %>%
  left_join(reserveInfo, by = c("reserveID" = "id"))

borrows <- rawBorrows %>%
  mutate(type = "borrow",
         id = str_extract(id, "0x\\w+")) %>%
  left_join(reserveParamsHistory, by = c("id" = "txID", "timestamp", "reserveID")) %>%
  mutate(amount = amount / (10^decimals)) %>%
  mutate(amountUSD = amount * priceInUsd) %>%
  mutate(priceInEth = priceInEth / (10^18)) %>%
  mutate(amountETH = amount * priceInEth) %>%
  mutate(reserve = symbol) %>%
  mutate(user = userID) %>%
  mutate(onBehalfOf = onBehalfOfID) %>%
  mutate(borrowRate = borrowRate / (10^25)) %>%
  mutate(pool = poolID) %>%
  left_join(aliases, by = c("user" = "id")) %>%
  rename(userAlias = alias) %>%
  left_join(aliases, by = c("onBehalfOf" = "id")) %>%
  rename(onBehalfOfAlias = alias) %>%
  select(id, type, timestamp, user, userAlias, onBehalfOf, onBehalfOfAlias, pool, reserve, amount, amountUSD, amountETH, borrowRate, borrowRateMode) %>%
  drop_na() %>%
  distinct()

collaterals <- rawCollaterals %>%
  mutate(type = "collateral",
         id = str_extract(id, "0x\\w+")) %>%
  mutate(user = userID, pool = poolID) %>%
  left_join(reserveParamsHistory, by = c("id" = "txID", "timestamp", "reserveID")) %>%
  mutate(reserve = symbol) %>%
  left_join(aliases, by = c("user" = "id")) %>%
  rename(userAlias = alias) %>%
  select(id, timestamp, user, userAlias, pool, reserve, fromState, toState, type) %>%
  drop_na() %>%
  distinct()

deposits <- rawDeposits %>%
  mutate(type = "deposit",
         id = str_extract(id, "0x\\w+")) %>%
  mutate(user = userID, pool = poolID) %>%
  left_join(reserveParamsHistory, by = c("id" = "txID", "timestamp", "reserveID")) %>%
  mutate(reserve = symbol) %>%
  mutate(amount = amount / (10^decimals)) %>%
  mutate(amountUSD = amount * priceInUsd) %>%
  mutate(priceInEth = priceInEth / (10^18)) %>%
  mutate(amountETH = amount * priceInEth) %>%
  left_join(aliases, by = c("user" = "id")) %>%
  rename(userAlias = alias) %>%
  left_join(aliases, by = c("onBehalfOf" = "id")) %>%
  rename(onBehalfOfAlias = alias) %>%
  select(id, timestamp, type, amount, amountUSD, amountETH, reserve, user, userAlias, onBehalfOf, onBehalfOfAlias, pool) %>%
  drop_na() %>%
  distinct()

liquidations <- rawLiquidations %>%
  mutate(type = "liquidation",
         id = str_extract(id, "0x\\w+"),
         user = userID, pool = poolID) %>%
  left_join(reserveParamsHistory, by = c("id" = "txID", "principalReserveID" = "reserveID", "timestamp")) %>%
  mutate(principalAmount = principalAmount / 10^decimals,
         principalAmountUSD = priceInUsd * principalAmount,
         principalAmountETH = priceInEth * principalAmount / 10^18) %>%
  select(id, timestamp, type, user, liquidator, pool, 
         principalAmount, principalReserve = symbol, principalAmountUSD, principalAmountETH, 
         collateralAmount, collateralReserveID) %>%
  left_join(reserveParamsHistory, by = c("id" = "txID", "collateralReserveID" = "reserveID", "timestamp")) %>%
  mutate(collateralAmount = collateralAmount / 10^decimals,
         collateralAmountUSD = priceInUsd * collateralAmount,
         collateralAmountETH = priceInEth * collateralAmount / 10^18) %>%
  select(id, timestamp, type, user, liquidator, pool,
         principalAmount, principalReserve, principalAmountUSD, principalAmountETH,
         collateralAmount, collateralReserve = symbol, collateralAmountUSD, collateralAmountETH) %>%
  left_join(aliases, by = c("user" = "id")) %>%
  rename(userAlias = alias) %>%
  left_join(aliases, by = c("liquidator" = "id")) %>%
  rename(liquidatorAlias = alias) %>%
  distinct()

redeems <- rawRedeems %>%
  mutate(type = "withdraw",
         id = str_extract(id, "0x\\w+")) %>%
  mutate(user = userID, pool = poolID, onBehalfOf = onBehalfOfID) %>%
  left_join(reserveParamsHistory, by = c("id" = "txID", "timestamp", "reserveID")) %>%
  mutate(reserve = symbol) %>%
  mutate(amount = amount / (10^decimals)) %>%
  mutate(amountUSD = amount * priceInUsd) %>%
  mutate(priceInEth = priceInEth / (10^18)) %>%
  mutate(amountETH = amount * priceInEth) %>%
  left_join(aliases, by = c("user" = "id")) %>%
  rename(userAlias = alias) %>%
  left_join(aliases, by = c("onBehalfOf" = "id")) %>%
  rename(onBehalfOfAlias = alias) %>%
  select(id,timestamp, type, amount, amountUSD, amountETH, reserve, user, userAlias, onBehalfOf, onBehalfOfAlias, priceInUsd, pool) %>%
  drop_na() %>%
  distinct()

repays <- rawRepays %>%
  mutate(type = "repay",
         id = str_extract(id, "0x\\w+"))%>%
  mutate(user = userID, pool = poolID, onBehalfOf = onBehalfOfID) %>%
  left_join(reserveParamsHistory, by = c("id" = "txID", "timestamp", "reserveID")) %>%
  mutate(reserve = symbol) %>%
  mutate(amount = amount / (10^decimals)) %>%
  mutate(amountUSD = amount * priceInUsd) %>%
  mutate(priceInEth = priceInEth / (10^18)) %>%
  mutate(amountETH = amount * priceInEth) %>%
  left_join(aliases, by = c("user" = "id")) %>%
  rename(userAlias = alias) %>%
  left_join(aliases, by = c("onBehalfOf" = "id")) %>%
  rename(onBehalfOfAlias = alias) %>%
  select(id, timestamp, type, amount, amountUSD, amountETH, reserve, user, userAlias, onBehalfOf, onBehalfOfAlias, priceInUsd, pool) %>%
  drop_na() %>%
  distinct()

flashLoans <- rawFlashLoans %>%
  mutate(type = "flashLoan",
         id = str_extract(id, "0x\\w+")) %>%
  mutate(pool = poolID) %>%
  left_join(reserveParamsHistory, by = c("id" = "txID", "timestamp", "reserveID")) %>%
  mutate(reserve = symbol) %>%
  select(id, timestamp, type, reserve, target, pool, amount, totalFee) %>%
  drop_na() %>%
  distinct()

cleanedTransactions <- borrows %>%
  bind_rows(collaterals) %>%
  bind_rows(deposits) %>%
  bind_rows(liquidations) %>%
  bind_rows(redeems) %>%
  bind_rows(repays) %>%
  bind_rows(flashLoans)





cleanedTransactions <- cleanedTransactions %>%
  mutate(userAlias = case_when(is.na(userAlias) & !is.na(user) ~ "Smart Contract",
                               TRUE ~ userAlias),
         onBehalfOfAlias = case_when(is.na(onBehalfOfAlias) & !is.na(onBehalfOf) ~ "Smart Contract",
                                     TRUE ~ onBehalfOfAlias),
         liquidatorAlias = case_when(is.na(liquidatorAlias) & !is.na(liquidator) ~ "Smart Contract",
                                     TRUE ~ liquidatorAlias),
         version = "V2",
         deployment = "Avalanche") 

saveRDS(cleanedTransactions, paste0(dataPath, "transactions.rds"))
