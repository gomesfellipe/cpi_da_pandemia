# Import data -------------------------------------------------------------

library(RSQLite)
library(dplyr)

con <- dbConnect(drv = RSQLite::SQLite(), dbname="cpidapandemia.sqlite")

tables <- dbListTables(con)

# Carregar uma "amostra" de 4.000.000 tweets
df <- dbGetQuery(conn = con, 
                 statement = paste0("SELECT DISTINCT text",
                                    " FROM ", tables[[1]], 
                                    " WHERE retweet_name IS NULL",
                                    " ORDER BY RANDOM()",
                                    " LIMIT 4000000")) %>% 
  as_tibble()


# Salvar cache
saveRDS(df, "cpi_da_pandemia_sample4mi.rds")





