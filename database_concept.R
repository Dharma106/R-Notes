library(RODBC)
library(dplyr)


dbconnection <- odbcDriverConnect('driver={SQL Server};
                    server= "";
                    database="";
                    trusted_connection=true')

data <- sqlFetch(dbconnection, "access.vwTradeFlow")
# or
trade_flow_data <- sqlQuery(dbconnection, "SELECT * FROM access.vwTradeFlow")


# to work with dplyr or to use database with "dplyr", install "dbplyr".
# one also needs to install a "DBI" backend package. "DBI" package provides a 
# common interface that allows "dplyr" to work with many database using the 
# same code. "DBI" is automatically installed with "dbplyr", but one need to
# install a specific backend for the database that he/she wants to connect to.

# five commonly used backends are:
  # "RMySQL" connects MySQL and MariaDB
  # "RPostgreSQL" connects to Postgres and Redshift.
  # "RSQLite" embeds a SQLite database
  # "odbc" connects to many commercial database via the open dataabase 
  #  connectivity protocl.
  # "bigrquery" connects to Google's BigQuery.

# "SQLite" is a great way to get started with database because it's completely 
# embedded inside
# an R package. Unlike most other systems, we don't need to set up a seperate 
# database server.

# "DBI" understanding ####
library(DBI)
con <- dbConnect(RSQLite::SQLite(), ":memory:")
# dbConnect() function varies from database to database, but the first argument
# is always the database backend. As an illustration, different backend driver
# for different type of db
#   RSQLite::SQLite() for RSQLite,
#   RMySQL::MySQL() for RMySQL
#   RPostgreSQL::PostgreSQL() for RPostgreSQL
#   odbcL::odbc() for odbc
#   bigrquery::bigquery() for BigQuery

# SQLite only needs one argument i.e. the path to the database. 
# Here the special string, ":memory:" is used which casue SQLite to create a
# temporary in-memory database.

# A easy, quick and dirty way to copy data to a database is through
# "copy_to()" function where one has to provide the connection object, 
# name of the table to call from the server and specify the name it has to save
# or it will save with the save table name loaded from server. creating a right 
# indexes is key to good database performance

copy_to(dest = con, nycflights13::flights, "flights_temp",
        temporary = FALSE, indexes = list(
          c("year", "month", "day",
            "carrier","tailnum", "dest")))

flights_db <- tbl(con, "flights_temp")
# on print the flights_db it mostly looks like a regular table but the main 
# difference is that it's a remote source in a SQLite datebase.

# we can use the usual function uses of dplyr package on the database pulled out. 
flights_db %>% select(year:day, dep_delay, arr_delay)
flights_db %>% filter(dep_delay > 200)
flights_db %>% group_by(dest) %>%
  summarise(delay = mean(dep_time, na.rm = TRUE))

# When working with databases, dplyr tries to be as lazy as possible:
  # 1. It never pulls data into R unless you explicitly ask for it.
  # 2. It delays doing any work until the last possible moment: it collects 
  # together everything you want to do and then sends it to the database in one step.

tailnum_delay_db <- flight_tbl %>%
  group_by(tailnum) %>%
  summarise(delay = mean(arr_delay), n = n()) %>%
  arrange(desc(delay)) %>%
  filter(n > 100)
  
# behind this code, dplyr is translating R code into SQL.
# the sql query can be seen through show_query().
tailnum_delay_db %>% show_query()

# once we figured out what data is required one can use collect() to pull
# all the data down into a tibble.
tailnum_delay <- collect(tailnum_delay_db)
# there is no way to determine how many rows a query will return unless we 
# actually run it, so nrow() taking sql_query written as dplyr format will show 
# NA
nrow(tailnum_delay_db)



# 
dbListTables(con)
dbWriteTable(con, "mtcars", mtcars)
dbListTables(con)

dbListFields(con, "mtcars")
dbReadTable(con, "mtcars")

# fetch all the result using sql
res <- dbSendQuery(con, "SELECT * FROM mtcars WHERE cyl = 4")
dbFetch(res)
