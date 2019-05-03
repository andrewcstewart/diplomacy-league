# diplomacy-league

This repository contains code for scraping diplomacy game data from www.backstabr.com and generating an interactive dashboard to track league rankings and stats.

## Workflow

 * Scrape JSON game state data from the website.
 * Parse the JSON into SQLite tables.
 * Perform additional transformations on the data.
 * Generate the dashboard.
 
## Data

### JSON data

### DB tables

League tables

 * players
  * player name
  * account name
  
Game tables
 
* games
  - game.id
  - game.data

* game.players
  - game.id
  - player.id
  - country
  - winner
  
* 

