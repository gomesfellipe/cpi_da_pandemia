

library(tidyverse)
library(tidytext)

library(igraph)
library(ggraph)
library(tidygraph)

df <- read_rds('cpi_da_pandemia_sample4mi.rds')

# stopwords
# sw <- paste0(c('us$'), collapse = "|")

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

# corrigir entidades
df <- df %>%
 mutate(tweet = tweet %>%
          str_replace_all("$", "s"),
          str_replace_all("\+", " "),
          str_replace_all("@", "a")
        )

# AED ---------------------------------------------------------------------


# # tokenizar
# tidy_tweets <- df %>% 
#   unnest_tokens(word, tweet)
# 
# # remover stopwords
# extra_stopwords=c("nao", "pra")
# 
# tidy_tweets <- tidy_tweets %>%
#   anti_join(tibble(word=c(tm::stopwords('pt'), extra_stopwords)))

# NER ---------------------------------------------------------------------
# spacyr::spacy_install()
# spacy_download_langmodel("pt_core_news_sm")

library(spacyr)
spacy_initialize(model="pt_core_news_sm")
entities <- spacy_extract_entity(df$tweet)

# group entities by document
filtered_entities <- subset(entities, entities["ent_type"] == "ORG" |
                              entities["ent_type"] == "PER" )
filtered_entities %>% count(text, sort = T) %>% head(50)

saveRDS(filtered_entities, "filtered_entities.rds")

edges <- 
  filtered_entities %>%
  group_by(doc_id) %>%
  summarise(entities = paste(text, collapse = ",")) # remove duplicated for the same document
edges <- lapply(str_split(edges$entities, ","),
                function(t){unique(unlist(t))}) # Auxiliary functions for creating adjancnt
edges <- edges[map_dbl(edges, length) != 1]
get_adjacent_list <- function(edge_list) {
  
  if(length(edge_list) == 1){
    adjacent_matrix <- edge_list
  }else{
    adjacent_matrix <- gtools::combinations(length(edge_list),
                                    2, edge_list)  
  }
  
  return(adjacent_matrix)
}
adjacent_matrix <- edges %>%
  lapply(get_adjacent_list) %>%
  reduce(rbind)


df <- as_tibble(adjacent_matrix) %>% `colnames<-`(c('item1', 'item2'))
df <- df %>% 
  mutate_all(~case_when(.x == "Fundação Getulio Vargas" ~ "FGV",
                        .x == "Organização para Cooperação e Desenvolvimento Econômico" ~ "OCDE",
                        .x == "Organização para a Cooperação e Desenvolvimento Econômico" ~ "OCDE",
                        .x == "Fundo Monetário Internacional" ~ "FMI",
                        .x == "Paulo Guedes" ~ "Guedes",
                        .x == "Donald Trump" ~ "Trump",
                        TRUE ~ .x)) %>% 
  filter_all(~!.x %in% c("Assine", "Isabel Seta", "Google Podcasts", "Mônica Mariotti", "Thiago Kaczuroski", "Luiz Felipe Silva", "Spotify", "Giovanni Reginato", "Focus"))
weighted_edgelist <- df %>%
  filter_at(1:2, ~.x != "G1") %>% 
  group_by(item1, item2) %>%
  summarise(n=n()) %>% 
  filter(n>100)
# news_graph <- weighted_edgelist %>%  graph_from_data_frame(directed = F)
# write_graph(news_graph, 'news_graph.graphml', 'graphml')
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
subt <- weighted_edgelist
vert <- subt %>% gather(item, word, item1, item2) %>%
  group_by(word) %>% summarise(n = sum(n))
# Obter componentes para colorir os clusters do grafo
tidy_graph_components <- 
  subt  %>%
  select(item1, item2) %>% 
  as.matrix() %>%
  graph.edgelist(directed = FALSE)  %>%
  as_tbl_graph() %>% 
  activate("edges") %>% 
  mutate(weight = subt$n) %>% 
  activate("nodes") %>% 
  # ttps://tidygraph.data-imaginist.com/reference/group_graph.html # tipos de agrupamentos
  mutate(component = as.factor(tidygraph::group_edge_betweenness()))
# Atualizar vertice para incluir componentes
vert <- vert %>% 
  left_join( as.data.frame(activate(tidy_graph_components, "nodes")) %>% 
               rename(word = name))
set.seed(314)
subt %>%
  graph_from_data_frame(vertices = vert) %>%
  # https://www.data-imaginist.com/2017/ggraph-introduction-layouts/ # layouts
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches'), color = "#D9D9D9A0") +
  geom_node_point() + 
  geom_node_text(aes(label = name, size = n, alpha = n, color = component),# color = "#EAFF00",
                 repel = TRUE, point.padding = unit(0.2, "lines"),
                 show.legend = F) +
  scale_size(range = c(2,10)) +
  scale_alpha(range = c(0.5,1))+ 
  theme_dark() + 
  theme(
    panel.background = element_rect(fill = "#2D2D2D"),
    legend.key = element_rect(fill = "#2D2D2D")
  ) +
  theme_graph(background = "black")

