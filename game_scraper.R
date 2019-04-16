library(httr)
library(rvest)
library(urltools)
library(V8)
ctx <- v8()

# 1 ) get player -> country table

players.df <- read_html("https://www.backstabbr.com/game/The-Defenestrations-of-Sa/5668161056145408") %>%
  html_nodes("table") %>%
  .[[3]] %>%
  html_table(fill=TRUE) %>%
  rename(Winner = 2) %>%
  mutate(Winner = ifelse(Winner == "Winner!", TRUE, FALSE))

# 2 ) get game history

# read_html("https://www.backstabbr.com/game/The-Defenestrations-of-Sa/5668161056145408/1913/fall")  %>%
#   html_text(trim=TRUE) %>%
#   str_extract_all("\\w*var.*;", simplify = FALSE) %>%
#   map(ctx$eval)
# 
# ctx$get('territories')
# ctx$get('unitsByPlayer')
# ctx$get('orders')
# 
# ctx$get('orders') %>% toJSON()


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


read_html("https://www.backstabbr.com/game/The-Defenestrations-of-Sa/5668161056145408/1913/fall")  %>%
  # html_text(trim=TRUE) %>%
  # html_attrs()
  # html_nodes("a")  %>% 
  html_nodes(xpath ='//*[@id="game-history-controls"]') %>%
  html_nodes("a")  %>%
  html_attr("href") %>%
  .[1]

get_url <- function(path) {
  url_absolute(path, base_url)
}

# get full history
history_url <- "https://www.backstabbr.com//game/The-Defenestrations-of-Sa/5668161056145408/ajax/history"
base_url <- url_parse(history_url) %>% mutate(path="") %>% url_compose()
data <- read_html("https://www.backstabbr.com//game/The-Defenestrations-of-Sa/5668161056145408/ajax/history") %>%
  html_nodes("a")  %>%
  html_attr("href") %>% 
  map_chr(get_url) %>%
  # reduce(list)
  map(parse_game_year)

data %>% 
  write_json("data2.json", auto_unbox=TRUE, pretty = TRUE)

df <- read_json("data2.json") %>%
  as.tbl_json() %>%
  gather_array() %>%
  spread_values(url = jstring("url")) %>%
  enter_object('territories') %>% 
  gather_keys('territory') %>%
  append_values_string('player') %>%
  group_by(url,player) %>%
  summarise(terr_count = n())


library(DBI)
library(dbplyr)
# Create an ephemeral in-memory RSQLite database
# db <- dbConnect(RSQLite::SQLite(), "game.sqlite")
db <- dbConnect(odbc:: odbc(), 
                driver = "SQLite Driver",
                database = "test.sqlite")

dbWriteTable(conn = db, 
             name = "territories",
             df, 
             overwrite=FALSE,
             append=TRUE,
             row.names=FALSE)
db %>%
  src_dbi() %>%
  copy_to(df, 'territories2', overwrite=TRUE, temporary=FALSE)

db %>% 
  src_dbi() %>%
  copy_to(players.df, 'players', overwrite=TRUE, temporary=FALSE)

db %>% 
  src_dbi() %>%
  tbl("players")

library(ggplot2)
df %>% 
  ungroup() %>%
  mutate(year = as.integer(str_match(url, "19\\d\\d")),
         turn = str_match(url, "\\w+$")) %>%
  filter(!is.na(year)) %>%
  filter(turn=="winter") %>%
  ggplot(aes(x=year, y=terr_count, color=player)) +
  geom_line()


read_json("data2.json") %>%
  as.tbl_json() %>%
  gather_array() %>%
  spread_values(url = jstring("url")) %>%
  enter_object('unitsByPlayer') %>% 
  gather_keys('player') %>%
  json_lengths('units')
  
