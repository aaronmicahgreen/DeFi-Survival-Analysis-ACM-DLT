library(randomNames)

rawUsers <- read_csv(paste0(rawDataPath, "rawUsers.csv"))
aliases = NULL
set.seed(69420) # Dank seeding

while(length(aliases[,1]) < length(rawUsers$id)){
  alias <- randomNames(1000, name.order = "first.last", name.sep = " ", sample.with.replacement = FALSE)
  aliases <- aliases %>%
    bind_rows(data.frame(alias)) %>%
    distinct()
}

aliases <- aliases %>%
  head(length(rawUsers$id))

userAliases <- bind_cols(rawUsers, aliases) %>%
  mutate(version = "V3",
         deployment = "Harmony")

write_csv(userAliases, paste0(dataPath, "users.csv"))
