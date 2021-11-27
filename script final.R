library(dplyr)
library(purrr)
library(stringr)
library(tidyr)
library(furrr)

filtered_entities <- readRDS("filtered_entities.rds")

filtered_entities <- as_tibble(filtered_entities) 

# Criar lista de arestas (edges or links)
edges <- 
  filtered_entities %>%
  group_by(doc_id) %>%
  summarise(entities = paste(text, collapse = ",")) %>% 
  mutate(entities = str_replace_all(entities, "^(@\\w+)\\s| (@\\w+)", "\\1,\\2")) %>% 
  pull(entities) %>% 
  str_split(",") %>% 
  map(~unique(unlist(.x))) %>% 
  .[map_dbl(., length) != 1]

get_adjacent_list <- function(edge_list) {
  gtools::combinations(length(edge_list), 2, edge_list)  
}

plan(multisession, workers = 4)

tictoc::tic()
adjacent_matrix <-
  future_map_dfr(edges, ~ 
                   as.data.frame(get_adjacent_list(.x))) %>% 
  as_tibble() %>% 
  set_names(c('item1', 'item2'))
tictoc::toc()

adjacent_matrix <- adjacent_matrix %>% 
  mutate_all(~case_when(
    str_detect(.x, "(^(luciano\\s)?hang( bobo da corte)?|^@lucianohangbr|luciano)") ~ "@lucianohang",
    str_detect(.x, "^omar azis$") ~ "@omarazizsenador",
    str_detect(.x, "^(sen(ador)?\\s?)?(omar(\\saziz)?)$") ~ "@omarazizsenador",
    str_detect(.x, "^lula$") ~ "@lulaoficial",
    str_detect(.x, "^randolfe( rodrigues)?") ~ "@randolfeap",
    str_detect(.x, "^renancalheiross$") ~ "@renancalheiros",
    str_detect(.x, "^renan( calheiros)?") ~ "@renancalheiros",
    str_detect(.x, "^(sen(ador)?\\s?)?otto( alencar)?") ~ "@ottoalencar",
    str_detect(.x, "^katia( abreu)?") ~ "@katiaabreu",
    str_detect(.x, "^(ricardo\\s)?lewandowski") ~ "ricardolewandowski",
    str_detect(.x, "^(jair)?\\s?(m(essias)?)?\\s?(bolsonaro)") ~ "@jairbolsonaro",
    str_detect(.x, "^flavio(\\sbolsonaro)?") ~ "@flaviobolsonaro",
    str_detect(.x, "^(carlos(\\sbolsonaro)?|carluxo)") ~ "@carlosbolsonaro",
    str_detect(.x, "^carlos eduardo(\\sbolsonaro)?") ~ "@bolsonarosp",
    str_detect(.x, "^(augusto nunes)") ~ "@augustosnunes",
    str_detect(.x, "^((dra )?mayra(\\spinheiro)?)") ~ "@dramayraoficial",
    str_detect(.x, "^((dra )?(nise )?yamaguchi)") ~ "@nise_dra",
    str_detect(.x, "^(ministro )?(paulo )?guedes") ~ "@pauloguedesreal",
    str_detect(.x, "^(ministro\\s)?(marcelo\\s)?queiroga") ~ "@mqueiroga2",
    str_detect(.x, "^(sen(adora)?\\s)?(simone\\s)?tebet") ~ "@simonetebetms",
    str_detect(.x, "^(sen(ador)?\\s)?(alessandro\\s)?vieira") ~ "@sen_alessandro",
    str_detect(.x, "^(sen(ador)?\\s)?(fabiano\\s)?contarato") ~ "@contaratosenado",
    str_detect(.x, "^(sen(ador)?\\s)?humberto (costa|dracula)") ~ "@senadorhumberto",
    str_detect(.x, "^(dep(utado)?\\s)?ricardo barros") ~ "@ricardobarrospp",
    str_detect(.x, "^(onix\\s)?lorenzoni") ~ "@onyxlorenzoni",
    str_detect(.x, "^onix(\\slorenzoni)?") ~ "@onyxlorenzoni",
    str_detect(.x, "^(rodrigo\\s)?maia") ~ "@rodrigomaia",
    str_detect(.x, "^(davi\\s)?alcolumbre") ~ "@davialcolumbre",
    str_detect(.x, "^(min(istro)?\\s)?(sergio\\s)?moro") ~ "@sf_moro",
    str_detect(.x, "^(gilmar\\s)?mendes") ~ "@gilmarmendes",
    str_detect(.x, "^(rodrigo\\s)?(soares\\s)?pacheco") ~ "@rpsenador",
    str_detect(.x, "^luis miranda") ~ "@luismirandausa",
    str_detect(.x, "^luis ricardo(\\smiranda)?") ~ "luisricardomiranda",
    str_detect(.x, "^(the\\s)?intercept(\\sbrasil)?") ~ "@theinterceptbr",
    str_detect(.x, "^(fabio\\s)?wajngarten") ~ "@fabiowoficial",
    str_detect(.x, "^(alexandre\\s)?frota") ~ "@alefrota77",
    str_detect(.x, "^(marcos\\s)?rogerio") ~ "@marcosrogerio",
    str_detect(.x, "^(eduardo\\s)?girao") ~ "@edugiraooficial",
    str_detect(.x, "^jorginho(\\smello)?") ~ "@jorginhomello",
    str_detect(.x, "^eduardo braga") ~ "@eduardobraga_am",
    str_detect(.x, "^(carla\\s)?zambelli") ~ "@carlazambelli38",
    str_detect(.x, "^(eduardo\\s)?pazuello") ~ "pazuello",
    str_detect(.x, "^michelle bolsonaro") ~ "@mibolsonaro",
    str_detect(.x, "^(luiz\\s)?(henrique\\s)?mandetta") ~ "@mandetta",
    str_detect(.x, "^(nelson\\s)?teich") ~ "@teichnelson",
    str_detect(.x, "^(augusto\\s)?aras") ~ "augustoaras",
    str_detect(.x, "^(arthur\\s)?lira") ~ "@arthurlira_",
    str_detect(.x, "^andre mendonca") ~ "@amendoncaagu",
    str_detect(.x, "^(bia\\s)?kicis") ~ "@biakicis",
    str_detect(.x, "^(fernando\\s)?bezerra") ~ "@fbezerracoelho",
    str_detect(.x, "^jaques wagner") ~ "@jaqueswagner",
    str_detect(.x, "^'(alagoas\\s|sen(ador)?\\s?)?jader(\\sbarbalho)?'") ~ "@jader_barbalho",
    str_detect(.x, "^(gen(eral)?\\s)?(braga|neto|braga neto)") ~ "genbraganeto",
    str_detect(.x, "^(gen(eral)?\\s)?braga(\\sneto)?") ~ "genbraganeto",
    str_detect(.x, "^ernesto(\\saraujo)?") ~ "@ernestofaraujo",
    str_detect(.x, "^(cor(onel\\s?)?)?helcio(\\s?bruno)?") ~ "corhelciobruno",
    str_detect(.x, "^(carlos\\s)?wizard(\\smartins)?") ~ "@carloswmartins",
    str_detect(.x, "^(luís\\s)?(roberto\\s)?barroso") ~ "@lrobertobarroso",
    str_detect(.x, "^osmar(\\sterra)?") ~ "@osmarterra",
    str_detect(.x, "^rui costa") ~ "@costa_rui",
    str_detect(.x, "^roberto(\\sferreira|\\sdias|\\sferreira\\sdias)") ~ "roberto dias",
    str_detect(.x, "^fib(\\s?bank)?") ~ "fibbank",
    str_detect(.x, "^(rodrigo\\sariel\\s)?zimmermann") ~ "zimmermann",
    str_detect(.x, "^(wilson\\s)?witzel|wilson") ~ "@wilsonwitzel",
    str_detect(.x, "^filipe (g\\s)?martins") ~ "@filgmartin",
    str_detect(.x, "^jurema(\\swerneck)?") ~ "@juremawerneck",
    str_detect(.x, "^(pedro\\s)?(curi\\s)?hallal") ~ "@pedrohallal",
    str_detect(.x, "^eliziane(\\sgama)?") ~ "@elizianegama",
    str_detect(.x, "^alexandre (de\\s)?moraes") ~ "@alexandre",
    str_detect(.x, "^consorcio(\\snordeste)?") ~ "consorcio nordeste",
    str_detect(.x, "^(prevent|senior|prevent senior)") ~ "@preventsenior",
    str_detect(.x, "^(francisco\\s?)?(maximiano(\\smedrades)?|(maximiano\\s?)?medrades)") ~ "franciscomaximiano",
    str_detect(.x, "g[rl]obro tv") ~ "globo",
    str_detect(.x, "reinaldo(\\s?azevedo)?") ~ "@reinaldoazevedo",
    str_detect(.x, "(lava\\s?)?jato") ~ "lava jato",
    str_detect(.x, "^(@?BolsonaroSP|eduardo(\\s?bolsonaro)?)") ~ "@bolsonarosp",
    TRUE ~.x
  ))

entities_to_drop <- c('ajuda leia', 'whindersson', 'mao', 'tao', 'hbo', 'hein', 'covid senador', 
                      'iyikigeldinbaba marcos rogerio loki', 'army doe', 'sylvie golden', 'opniao',
                      'covid beira', 'votacao', 'hello', 'our governants brazilians diethe senators from',
                      'papa ragnarol btob phoebe pitty', 'enem grand', 'gabriel silva', 'ben platt randolfe evan hensen twenty',
                      'annabelle lucas neto', 'avon tao on deolane', 'milhoes', 'bombasem acareacao',
                      'adnet hoseok benzema', 'omissao', 'omisso', 'internacional lula', 'mentiu queiroga',
                      'crimeou naoinquerito', 'provasdissimina fake news', 'informacoes sigilosa recebe instrucoes',
                      'juliette', 'google','avon tao on deolane', 'convoque o carlos','prefeitosato gov',
                      "jennifer aniston louis fetus salles flavio bolsonaro dua lipa ross phoebe my", 'senador',
                      "rihanna marina dilan", 'pagtohouve', 'corrupcaohouve', 'brasilhouve','favorpode',
                      "juliette milhoes",'exmo', 'us$', 'ninguemteich', 'cloroquinateich naocalheiros',
                      "lamar ana maria braga", 'canalhamandeltadoriaciro gomesunese', 'lula ladraorodrigomaia', 'julgalos stf', 'foro',
                      'branchu dcupruaho perigo', 'kajurus', 'xing ling', '@brazilfight', 'humilhad', 'senadora',
                      'francisco eduardo cardoso alves', 'ricardo ariel zimerman', 'impeachementnao', 'ladroes bandidosnao',
                      'fake', '@leonicemariana', '@terrabrasilnot', 'lula ladraorodrigo maia', 'amp cia',# 'omar aziz psdam',
                      'canalhamandettadoriaciro gomesunemse', 'jurista ives', 'martins senador defende',
                      '@mipasquarelle', '@christinafonse', 'barrar renan', 'alo', 'sen', 'senhor', 'sr', 'fausto',
                      'loki', 'vc', 'modric superman', 'arao felipe', 'nao')

weighted_edgelist <- adjacent_matrix %>%
  filter_at(1:2, ~ !.x %in% entities_to_drop) %>% 
  group_by(item1, item2) %>%
  summarise(n=n()) %>% 
  ungroup() %>% 
  filter(n>100)

weighted_edgelist %>% 
  arrange(n) %>% View()


# Grafico -----------------------------------------------------------------
library(igraph)
library(ggraph)
library(tidygraph)

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





