
# Function to filter original data based on crop year and Rank.
data_by_cy <- function(rawDF, filter_by_cy =`Crop Year`, cy){  
  cur_year_data <- filter(rawDF, filter_by_cy == cy[1])
  cur_year_data <- filter(cur_year_data, Rank > 0)
  cur_year_data <- arrange(cur_year_data, Rank)
  cur_year_data$Cummulative_Production <-
    cumsum(cur_year_data$`Production (MT)`)
  
  pre_year_data <- filter(rawDF, filter_by_cy == cy[2])
  pre_year_data <- filter(pre_year_data, Rank > 0)
  pre_year_data <- arrange(pre_year_data, Rank)
  pre_year_data$Cummulative_Production <- 
    cumsum(pre_year_data$`Production (MT)`)
  
  next_year_data <- filter(rawDF, filter_by_cy == cy[3])
  next_year_data <- filter(next_year_data, Rank > 0)
  next_year_data <- arrange(next_year_data, Rank)
  next_year_data$Cummulative_Production <- 
    cumsum(next_year_data$`Production (MT)`)
  
  analysis_data <- rbind(cur_year_data, pre_year_data, next_year_data)
  return(analysis_data)  
}


# function to insert row in the data with doing change in Production
# and creating corresponding average COP.
insertRow <- function(existingDF, r, prod_split, start_row = 1){
  existingDF[seq(r+1, nrow(existingDF)+1), ] <- 
    existingDF[seq(r, nrow(existingDF)), ]
  existingDF[r, "Production (MT)"] <- prod_split - 
    existingDF[r-1, "Cummulative_Production"]
  existingDF[r+1, "Production (MT)"] <- existingDF[r+1, "Production (MT)"]- 
    existingDF[r, "Production (MT)"]
  existingDF$Cummulative_Production <- cumsum(existingDF$`Production (MT)`)
  existingDF[start_row:r, "Avg_COP"] <- 
    weighted.mean(x = existingDF$`Total cost (Cts/lb)`[start_row:r], 
                  w = existingDF$`Production (MT)`[start_row:r])
  # start_row <- r + 1
  # assign('existingDF',existingDF, envir = .GlobalEnv)
  # assign('start_row', start_row, envir = .GlobalEnv)
  # assingin data of runtime environment to Global environment
  return(existingDF)
}
  

# Function to modify data with adding rows using insertRow function created above for three year, based on the some predefined arguments.

avg_cop_data <- function(data, cy){
  avg_cop_by_cy <- vector("list", length(cy))  
  cy_col_num <- which(names(data) == "Crop Year")
  prod_col_num <- which(names(data) == "Production (MT)")
  tc_col_num <- which(names(data) == "Total cost (Cts/lb)")
  cum_prod_num <- which(names(data) == "Cummulative_Production")
  for(i in seq_along(cy)){
    existingDF <- filter(data, data[, cy_col_num] == cy[i])
    existingDF$Avg_COP <- NA
    avg_COP_col_num <- which(names(existingDF) == "Avg_COP")
    tot_prod <- as.numeric(existingDF[nrow(existingDF), cum_prod_num])
    prod_split <- c(0.25, 0.50, 0.75) * tot_prod
    start_row = 1
    for (j in 1:length(prod_split)) {
      get_row_num <- 
        min(which(existingDF[, cum_prod_num] > prod_split[j]))
      prod_part <- prod_split[j]
      existingDF <- insertRow(existingDF, get_row_num, prod_part, start_row)
      start_row <- get_row_num + 1
    }    
    existingDF <- existingDF #Check this redundancy
    row_lst_prt <- get_row_num + 1
    x = existingDF[row_lst_prt:nrow(existingDF), tc_col_num]
    y = existingDF[row_lst_prt:nrow(existingDF), prod_col_num]
    existingDF[row_lst_prt:nrow(existingDF), avg_COP_col_num] <- sum(x*y)/sum(y)
    avg_cop_by_cy[[i]] <- existingDF
  }
  data_avg_cop <- do.call("rbind", avg_cop_by_cy)
  return(data_avg_cop)
}



#Function for reading one crop year data
avg_cop_data_version2 <- function(data, cy){
  cy_col_num <- which(names(data) == "Crop Year")
  prod_col_num <- which(names(data) == "Production (MT)")
  tc_col_num <- which(names(data) == "Total cost (Cts/lb)")
  cum_prod_num <- which(names(data) == "Cummulative_Production")
  existingDF <- filter(data, data[, cy_col_num] == cy[1])
  existingDF$Avg_COP <- NA
  avg_COP_col_num <- which(names(existingDF) == "Avg_COP")
  tot_prod <- as.numeric(existingDF[nrow(existingDF), cum_prod_num])
  prod_split <- c(0.25, 0.50, 0.75) * tot_prod
  start_row = 1
  for (i in 1:length(prod_split)) {
    get_row_num <- 
      min(which(existingDF[, cum_prod_num] > prod_split[i]))
    prod_part <- prod_split[i]
    existingDF <- insertRow(existingDF, get_row_num, prod_part, start_row)
    start_row <- get_row_num + 1
  }
  existingDF <- existingDF
  row_lst_prt <- get_row_num + 1
  x = existingDF[row_lst_prt:nrow(existingDF), tc_col_num]
  y = existingDF[row_lst_prt:nrow(existingDF), prod_col_num]
  existingDF[row_lst_prt:nrow(existingDF), avg_COP_col_num] <- sum(x*y)/sum(y)
  return(existingDF)
}
