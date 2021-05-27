library(readr)
library(tidygraph)
library(ggraph)
library(dplyr)
library(tidyr)
library(tibble)
library(igraph)

url <- "https://github.com/rfordatascience/tidytuesday/raw/master/tidytuesday_tweets/data.rds"
out_file <- "data/tt_tweets.rds"
download.file(url, destfile = out_file, mode = "wb")
