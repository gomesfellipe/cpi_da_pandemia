
# Carregar pacotes --------------------------------------------------------

library(RSQLite)
library(tidyverse)

# Import data -------------------------------------------------------------

# conectar ao banco
con <- dbConnect(drv = RSQLite::SQLite(), dbname="cpidapandemia.sqlite")

# lista de tabelas presentes no banco
tables <- dbListTables(con)

# Carregar uma "amostra" de 4.000.000 tweets
df <- dbGetQuery(conn = con, 
                 statement = paste0("SELECT DISTINCT text",
                                    " FROM ", tables[[1]], 
                                    " WHERE retweet_name IS NULL",
                                    " ORDER BY RANDOM()",
                                    " LIMIT 4000000")) %>% 
  as_tibble()

# Prepare data ------------------------------------------------------------

# Carregar cache
# df <- read_rds('cpi_da_pandemia_sample4mi.rds')

# Preparar textos
df <- df %>% 
  transmute(tweet = text %>% 
              str_to_lower() %>% 
              abjutils::rm_accent() %>% 
              str_remove_all("(f|ht)(tp)(s?)(://)(.*)[.|/](.*)") %>% # links
              str_remove_all("https://") %>% # links
              str_remove_all("http://") %>% # links
              str_remove_all('[0-9]') %>% 
              str_remove_all("[^\x01-\x7F]") %>% # remover unicode
              #str_remove_all("@\\w+") %>% # remover @...
              str_remove_all("#\\w+") %>% # remover #...
              str_replace_all("(.*?)($|(@|#)|[^[:punct:]]+?)(.*?)", "\\2") %>% # remover caracter especial menos @ ou #
              str_remove_all("[[:cntrl:]]") %>% 
              str_remove_all("kkk+") %>% # risadas
              str_trim() %>% 
              str_squish(),
            id = 1:nrow(.)
  )

# corrigir alguns caracteres
df <- df %>%
  mutate(tweet = tweet %>%
           str_replace_all("$", "s") %>% 
           str_replace_all("\+", " ") %>% 
           str_replace_all("@", "a")
  )

# Salvar cache com dados minimamente tratados
saveRDS(df, "cpi_da_pandemia_sample4mi.rds")
