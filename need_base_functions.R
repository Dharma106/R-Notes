library(XLConnect)
library(readxl)
library(tidyverse)
options(java.parameters = "-Xmx1024m")

# Function to filter original data based on crop year and Rank.
data_by_cy <- function(raw_data, year){
  
  # If user has not given year then it will ask user inputs.
  if(missing(year)){
    year <- readline("Enter year in the format 2017-18 for 2017: ")
    num_year <- strsplit(year, split = "-")
    num_year <- as.numeric(num_year[[1]])
    pre_year <- paste(num_year[1]-1, num_year[2]-1, sep = "-")
    next_year <- paste(num_year[1]+1, num_year[2]+1, sep = "-")
    cy <- c(pre_year, year, next_year)
  }
  
  # To get the corresponding column number of the data.
  cy_col_num <- which(names(raw_data) == "Crop Year")
  rank_col_num <- which(names(raw_data) == "Rank")
  prod_col_num <- which(names(raw_data) == "Production (K Bag)")
  
  # Compiling data based on the year
  
  # Data for one year back from the input year.
  pre_year_data <- filter(raw_data, raw_data[, cy_col_num] == cy[1] & 
                            raw_data[, rank_col_num] > 0)
  pre_year_data <- arrange(pre_year_data, Rank)
  pre_year_data[, "Cummulative_Production"] <- 
    cumsum(pre_year_data[, prod_col_num])
  
  # Data for the input year.
  cur_year_data <- filter(raw_data, raw_data[, cy_col_num] == cy[2] & 
                            raw_data[, rank_col_num] > 0)
  cur_year_data <- arrange(cur_year_data, Rank)
  cur_year_data[,"Cummulative_Production"] <- 
    cumsum(cur_year_data[, prod_col_num])
  
  # Data for one year ahead from the input year.
  next_year_data <- filter(raw_data, raw_data[, cy_col_num] == cy[3] & 
                             raw_data[, rank_col_num] > 0)
  next_year_data <- arrange(next_year_data, Rank)
  next_year_data[,"Cummulative_Production"] <- 
    cumsum(next_year_data[, prod_col_num])
  
  # row binding the three years data in one dataframe. 
  output_data <- rbind(pre_year_data, cur_year_data, next_year_data)
  return(output_data)
}


# Function to extract data for three crop year in a generalized way
insertRow <- function(raw_data, r, prod_split, tc_unit_name, start_row = 1){
  # tc_unit_name stands for total cost unit name.
  prod_col_num <- which(names(raw_data) == "Production (K Bag)")
  tc_col_num <- which(names(raw_data) == tc_unit_name)
  cum_prod_num <- which(names(raw_data) == "Cummulative_Production")
  avg_COP_col_num <- which(names(raw_data) == "Avg_COP")
  
  if(r == 1){
    raw_data[seq(r+1, nrow(raw_data)+1), ] <- 
      raw_data[seq(r, nrow(raw_data)), ]
    raw_data[r, prod_col_num] <- prod_split 
    raw_data[r+1, prod_col_num] <- raw_data[r+1, prod_col_num]- 
      raw_data[r, prod_col_num]
    raw_data[, cum_prod_num] <- cumsum(raw_data[, prod_col_num])
    # raw_data$Cummulative_Production <- cumsum(raw_data$`Production (K Bag)`)
    x = raw_data[start_row:r, tc_col_num]
    w = raw_data[start_row:r, prod_col_num]
    raw_data[start_row:r, avg_COP_col_num] <- sum(x*w)/sum(w)
  } else {
    raw_data[seq(r+1, nrow(raw_data)+1), ] <- 
      raw_data[seq(r, nrow(raw_data)), ]
    raw_data[r, prod_col_num] <- prod_split - 
      raw_data[r-1, cum_prod_num]
    raw_data[r+1, prod_col_num] <- raw_data[r+1, prod_col_num]- 
      raw_data[r, prod_col_num]
    raw_data[, cum_prod_num] <- cumsum(raw_data[, prod_col_num])
    # raw_data$Cummulative_Production <- cumsum(raw_data$`Production (K Bag)`)
    x = raw_data[start_row:r, tc_col_num]
    w = raw_data[start_row:r, prod_col_num]
    raw_data[start_row:r, avg_COP_col_num] <- sum(x*w)/sum(w)
  }
  # assign('raw_data',raw_data, envir = .GlobalEnv)
  # assingin data of runtime environment to Global environment
  return(raw_data)
}


# Function to extract data for three crop year based on total cost unit 
# provided by user
avg_cop_data <- function(data, tc_unit_name, cy){
  avg_cop_by_cy <- vector("list", length(cy))
  cy_col_num <- which(names(data) == "Crop Year")
  prod_col_num <- which(names(data) == "Production (K Bag)")
  tc_col_num <- which(names(data) == tc_unit_name)
  cum_prod_num <- which(names(data) == "Cummulative_Production")
  for(i in seq_along(cy)){
    existingDF <- filter(data, data[, cy_col_num] == cy[i])
    existingDF$Avg_COP <- NA
    avg_COP_col_num <- which(names(existingDF) == "Avg_COP")
    tot_prod <- as.numeric(existingDF[nrow(existingDF), cum_prod_num])
    prod_split <- c(0.25, 0.50, 0.75) * tot_prod
    start_row = 1
    for (j in 1:length(prod_split)) {
      get_row_num <- min(which(existingDF[, cum_prod_num] > prod_split[j]))
      prod_part <- prod_split[j]
      existingDF <- insertRow(existingDF, get_row_num, 
                              prod_part, tc_unit_name, start_row)
      start_row <- get_row_num + 1
    }
    
    existingDF <- existingDF
    row_lst_prt <- get_row_num + 1
    x = existingDF[row_lst_prt:nrow(existingDF), tc_col_num]
    y = existingDF[row_lst_prt:nrow(existingDF), prod_col_num]
    existingDF[row_lst_prt:nrow(existingDF), avg_COP_col_num] <- sum(x*y)/sum(y)
    existingDF1 <- existingDF
    existingDF1[nrow(existingDF1)+1, ] <- 0
    existingDF1[2:nrow(existingDF1),] <- existingDF
    existingDF1[1, cum_prod_num] <- 0
    avg_cop_by_cy[[i]] <- existingDF1
  }
  data_avg_cop <- do.call("rbind", avg_cop_by_cy)
  return(data_avg_cop)
}


