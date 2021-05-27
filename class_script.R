library(readr)
library(tidygraph)
library(ggraph)
library(dplyr)
library(tidyr)
library(tibble)
library(igraph)

# --- Download the Data --- #

url <- "https://github.com/rfordatascience/tidytuesday/raw/master/tidytuesday_tweets/data.rds"
out_file <- "data/tt_tweets.rds"
download.file(url, destfile = out_file, mode = "wb")

# --- Load the data into R --- #

tweets <- read_rds('data/tt_tweets.rds')

# --- Construct a Mentions Network --- #

connections <- 
    tweets %>% 
    filter(mentions_screen_name != 'NA') %>%
    select(from = screen_name, to = mentions_screen_name)

connections <-
    connections %>% 
    unnest_longer(to) %>% 
    filter(from != to)

# --- Sample the network --- #

set.seed(1234567890)

tweet_authors <- 
    as_tibble(
        unique(connections$from)
    )

seed_users <- sample_n(tweet_authors, 250)

connections_sample <-
    connections %>% 
    filter(from %in% seed_users$value)

first_step <- unique(connections_sample$to)

smple_users <- unique(c(seed_users$value, first_step))

edgelist <- 
    connections %>%
    filter(from %in% smple_users,
           to %in% smple_users) %>%
    distinct()

# --- Create a Network Object --- #

tg <- 
    as_tbl_graph(edgelist) %>%
    convert(to_undirected) %>%
    convert(to_simple)

# --- Properties of a Network --- #

# number of nodes (individuals)
gorder(tg)

# how many connections are there?
gsize(tg)

# what is the max number of connections in this network?
max_connections <- 0.5 * (gorder(tg) * (gorder(tg) - 1))
print(max_connections)

# how dense is my network?
gsize(tg) / max_connections

# --> that's quite sparse, approx 0.5 of one percent of all connections
# alternatively, just do this
edge_density(tg)

# probability that adjacent nodes are connected
transitivity(tg, type = 'undirected')

# --- Node Influentiality --- #
# different metrics we can use ..

# degree: number connections of a node
# betweeness: measures shortest paths between people
# eigenvector ('prestige'): influential when connected to more connected nodes
tg <-
    tg %>%
    activate(nodes) %>%
    mutate(degree      = centrality_degree(),
           betweenness = centrality_betweenness(),
           eigen       = centrality_eigen(),
           pagerank    = centrality_pagerank() 
           )

centrality_table <- 
    tg %>%
    activate(nodes) %>%
    as_tibble()

# in the lab ... you'll look at how these rankings are similar 
# across different centrality measures

# --- Community Detection --- #
# ran community detection  using 'infomap' algorithm
tg <-
    tg %>%
    activate(nodes) %>%
    mutate(grp_info = group_louvain())

# how many communities to I find?
tg %>%
    activate(nodes) %>%
    as_tibble() %>%
    summarise(max_grp = max(grp_info))

# how large are these groups?
grp_size <-
    tg %>%
    activate(nodes) %>%
    as_tibble() %>%
    group_by(grp_info) %>%
    count()

tg_plot <- 
    tg %>%
    activate(nodes) %>%
    filter(grp_info %in% c(1, 2, 3, 4, 5)) 

tg_plot %>%
    ggraph(layout = 'fr') +
    geom_node_point(aes(color = as.factor(grp_info))) +
    geom_edge_link(alpha = 0.2) +
    theme_void()

#' summary:
#' - sampled a network starting with a set of seed users, then i expanded (by one)
#' - summary stats of a network ... size, density and so on
#' - measure influence based on node connectedness
#' - can i split a network into multiple communities
#' - (lab) within each community can i look for influential nodes
    
    
    
    
    


