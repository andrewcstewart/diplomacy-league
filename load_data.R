# library(DBI)
library(tidyjson)
library(tidyverse)
library(jsonlite)
library(glue)
library(DBI)
library(dbplyr)
library(ggplot2)

con <- DBI::dbConnect(odbc:: odbc(), 
                driver = "SQLite Driver",
                database = "test.sqlite")
db <- src_dbi(con)

game_files <- list.files(path="data", pattern = "*.json", full.names = TRUE)

# 1 ) get player -> country table

parse_players <- function(game_data_path) {
  .url <- fromJSON(game_data_path) %>%
    pull(url) %>%
    .[1]
  .url %>%
    read_html() %>%
    html_nodes("table") %>%
    .[[3]] %>%
    html_table(fill=TRUE) %>%
    rename(Winner = 2) %>%
    mutate(Winner = ifelse(Winner == "Winner!", TRUE, FALSE)) %>%
    separate(Player, sep="#", into=c("player.name","player.id")) %>%
    rename(winner=Winner, country=Country) %>%  
    mutate(game_url = urltools::path(.url)) %>% 
    separate(game_url, sep="/", into=c("junk","game.name","game.id","game.year","game.season"), fill='right') %>%
    select(starts_with("game"), starts_with("player"), country, winner)
}

game_files %>%
  purrr::map(parse_players) %>%
  reduce(rbind) %>% 
  copy_to(dest=db, 'players', overwrite=TRUE, temporary=FALSE)
# 
# db %>% 
#   tbl("players") %>%
#   collect() %>%
#   group_by(name) %>%
#   summarize(wins = sum(winner)) %>%
#   ggplot(aes(x=name, y=wins)) +
#   geom_col()


# 2) get territory counts per game year
parse_territories <- function(game_data_path) {
  read_json(game_data_path) %>%
    as.tbl_json() %>%
    gather_array() %>%
    spread_values(url = jstring("url")) %>%
    enter_object('territories') %>% 
    gather_keys('territory') %>%
    append_values_string('player') %>%
    group_by(url,player) %>%
    summarise(territory.count = n()) %>%
    mutate(game_url = path(url)) %>%
    separate(game_url, sep="/", into=c("junk","game.name","game.id","game.year","game.season"), fill='right') %>%
    ungroup() %>%
    select(starts_with("game"),player,territory.count)
}

game_files %>%
  purrr::map(parse_territories) %>%
  reduce(rbind) %>%
  copy_to(dest=db, 'territories', overwrite=TRUE, temporary=FALSE)

# 3) unit counts per game year
parse_units <- function(game_data_path) {
  read_json(game_data_path) %>%
    as.tbl_json() %>%
    gather_array() %>%
    spread_values(url = jstring("url")) %>%
    enter_object('unitsByPlayer') %>%
    gather_keys('player') %>%
    json_lengths('units') %>%
    mutate(game_url = path(url)) %>%
    separate(game_url, sep="/", into=c("junk","game.name","game.id","game.year","game.season"), fill='right') %>%
    select(starts_with("game"),player,units)    
}

game_files %>%
  purrr::map(parse_units) %>%
  reduce(rbind) %>% 
  copy_to(dest=db, 'units', overwrite=TRUE, temporary=FALSE)

# orders by game year 
parse_orders <- function(game_data_path) {
  read_json(game_data_path) %>%
    as.tbl_json() %>%
    gather_array() %>%
    spread_values(url = jstring("url")) %>%
    enter_object('orders') %>%
    tidyjson::gather_keys(column.name = 'country') %>%
    tidyjson::gather_keys(column.name = 'territory') %>% 
    tidyjson::spread_values(to = jstring("to"),
                            from = jstring("from"),
                            type = jstring("type"),
                            result = jstring("result"),
                            result_reason = jstring("result_reason")) %>%
    mutate(game_url = path(url)) %>%
    separate(game_url, sep="/", into=c("junk","game.name","game.id","game.year","game.season"), fill='right') %>%
    select(starts_with("game"),country,territory,type,from,to,result,result_reason)
}

game_files %>%
  purrr::map(parse_orders) %>%
  reduce(rbind) %>%
  copy_to(dest=db, 'orders', overwrite=TRUE, temporary=FALSE)

# retreats
parse_retreats <- function(game_data_path) {
  read_json(game_data_path) %>%
    as.tbl_json() %>%
    gather_array() %>%
    spread_values(url = jstring("url")) %>%
    enter_object('orders') %>%
    tidyjson::gather_keys(column.name = 'country') %>%
    tidyjson::gather_keys(column.name = 'territory') %>% 
    enter_object("retreat") %>%
    tidyjson::spread_values(to = jstring("to"),
                            type = jstring("type"),
                            result = jstring("result")) %>%
    mutate(game_url = path(url)) %>%
    separate(game_url, sep="/", into=c("junk","game.name","game.id","game.year","game.season"), fill='right') %>%
    select(starts_with("game"),country,territory,type,to,result)    
}

game_files %>%
  purrr::map(parse_retreats) %>%
  reduce(rbind) %>%
  copy_to(dest=db, 'retreats', overwrite=TRUE, temporary=FALSE)


# library(ggplot2)
# df %>%
#   ungroup() %>%
#   mutate(year = as.integer(str_match(url, "19\\d\\d")),
#          turn = str_match(url, "\\w+$")) %>%
#   filter(!is.na(year)) %>%
#   filter(turn=="winter") %>%
#   ggplot(aes(x=year, y=terr_count, color=player)) +
#   geom_line()

# league.players
# * player.name
# * player.account.name
# * player.account.id


bind_rows(
  list(player.name="Andrew", player.id="9196"),
  list(player.name="Ben", player.id="1390"),
  list(player.name="Sam", player.id="670"),
  list(player.name="Mark", player.id="8509"),
  list(player.name="Tom", player.id="3730"),
  list(player.name="Justin", player.id="4951"),
  list(player.name="Alex", player.id="6766")) %>%
  copy_to(dest=db, 'league.players', overwrite=TRUE, temporary=FALSE)

bind_rows(
  list(game.id="5127116224462848", league.game=FALSE),
  list(game.id="5636021098643456", league.game=FALSE),
  list(game.id="5668161056145408", league.game=FALSE),
  list(game.id="5694328282808320", league.game=FALSE),
  list(game.id="5841968584720384", league.game=FALSE),
  list(game.id="6298332016672768", league.game=FALSE),
  list(game.id="6329975150477312", league.game=FALSE))