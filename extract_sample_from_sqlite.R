library(RSQLite)
library(dplyr)


con <- dbConnect(drv = RSQLite::SQLite(), dbname="cpidapandemia.sqlite")

tables <- dbListTables(con)


df_full <- dbGetQuery(conn = con, 
                      statement = paste0("SELECT DISTINCT text",
                                         " FROM ", tables[[1]], 
                                         " WHERE retweet_name IS NULL",
                                         " ORDER BY RANDOM()",
                                         " LIMIT 4000000")) %>% 
  as_tibble()



saveRDS(df_full, "cpi_da_pandemia_sample4mi.rds")
