# library(DBI)
library(tidyverse)
library(dbplyr)
library(ggplot2)

db <- DBI::dbConnect(odbc:: odbc(), 
                driver = "SQLite Driver",
                database = "test.sqlite")

