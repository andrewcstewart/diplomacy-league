library(httr)
library(rvest)
library(urltools)
library(V8)
library(jsonlite)
library(tidyjson)
library(glue)
ctx <- v8()

### FUNCTIONS

# parse game url
parse_game_link <- function(.url) {
  base_url <- urltools::url_parse(.url)  %>% mutate(path="") %>% url_compose()
  url_parsed <- path(.url) %>% str_split("/") %>% unlist()
  game_name <- url_parsed[2]
  game_id <- url_parsed[3]
  game_path <- url_parsed[1:3] %>% paste0(collapse = '/')
  history_path <- paste0(game_path, "/ajax/history")
  list(
    game_id = game_id,
    game_name = game_name,
    base_url = base_url,
    game_url = url_absolute(game_path, base_url),
    history_url = url_absolute(history_path, base_url)
  )
}

# parse game data for year
parse_game_year <- function(url) {
  ctx <- v8()
  
  read_html(url)  %>%
    html_text(trim=TRUE) %>%
    str_extract_all("\\w*var.*;", simplify = FALSE) %>%
    map(ctx$eval)
  
  list(
    url = url,
    territories = ctx$get('territories'),
    unitsByPlayer = ctx$get('unitsByPlayer'),
    orders = ctx$get('orders')
  )
}

scrape_game <- function(game_url) {
  game <- parse_game_link(game_url)
  game_data_path <- file.path("data",paste0(game$game_id,".json"))
  
  # get full history
  read_html(game$history_url) %>%
    html_nodes("a")  %>%
    html_attr("href") %>%
    map_chr(~ url_absolute(.x , game$base_url)) %>%
    map(parse_game_year) %>%
    write_json(game_data_path,auto_unbox=TRUE, pretty = TRUE)
}

### PARSING


# get all game links
# https://www.backstabbr.com/game/Vercingetorix/5393551035203584  # EARLY DRAW
game1 <- "https://www.backstabbr.com/game/THE-MEAT-GRINDER/6298332016672768"
game2 <- "https://www.backstabbr.com/game/The-Defenestrations-of-Sa/5668161056145408"
game3 <- "https://www.backstabbr.com/game/Milo-Minderbinder/5636021098643456"
game4 <- "https://www.backstabbr.com/game/Major-Major-Major-Major/5694328282808320"
game5 <- "https://www.backstabbr.com/game/Lajos-Kossuth-Chute/5127116224462848"
game6 <- "https://www.backstabbr.com/game/IM-SORRY-JUSTIN/6329975150477312"
game7 <- "https://www.backstabbr.com/game/Diplo-Diplo/5841968584720384"

scrape_game(game1)
# scrape_game(game2)
scrape_game(game3)
scrape_game(game4)
scrape_game(game5)
scrape_game(game6)
scrape_game(game7)

# game <- parse_game_link("https://www.backstabbr.com/game/The-Defenestrations-of-Sa/5668161056145408")
# game <- parse_game_link(game1)
# game_data_path <- file.path("data",paste0(game$game_id,".json"))
# 
# 
# # get full history
# data <- read_html(game$history_url) %>%
#   html_nodes("a")  %>%
#   html_attr("href") %>%
#   map_chr(~ url_absolute(.x , game$base_url)) %>%
#   map(parse_game_year)
# 
# # save game data json
# data %>%
#     write_json(game_data_path,auto_unbox=TRUE, pretty = TRUE)
# 


