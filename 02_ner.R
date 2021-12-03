
# Carregar pacote ---------------------------------------------------------

library(tidyverse)
library(spacyr)

# Import data -------------------------------------------------------------

df <- readRDS("cpi_da_pandemia_sample4mi.rds")

# NER ---------------------------------------------------------------------
# spacyr::spacy_install()
# spacy_download_langmodel("pt_core_news_sm")

# Inicializar modelo de reconhecimento de entidades
spacy_initialize(model="pt_core_news_sm")

# Aplicar modelo
entities <- spacy_extract_entity(df$tweet)

# obter pessoas e organizacoes
filtered_entities <- entities %>% 
  filter(ent_type == "ORG" | ent_type == "PER" )

# Salvar cache
saveRDS(filtered_entities, "filtered_entities.rds")