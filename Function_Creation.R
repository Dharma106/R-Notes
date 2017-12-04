# Function 1
# Function to filter original data based on crop year and Rank.
data_by_cy <- function(rawDF, cy){
  cy_col_num <- which(names(rawDF) == "Crop Year")
  rank_col_num <- which(names(rawDF) == "Rank")
  prod_col_num <- which(names(data) == "Production (MT)")
  
  cur_year_data <- filter(rawDF, rawDF[, cy_col_num] == cy[1] & 
                            rawDF[, rank_col_num] > 0)
  cur_year_data <- arrange(cur_year_data, Rank)
  cur_year_data[,"Cummulative_Production"] <- 
    cumsum(cur_year_data[, prod_col_num])
  pre_year_data <- filter(rawDF, rawDF[, cy_col_num] == cy[2] & 
                            rawDF[, rank_col_num] > 0)
  pre_year_data <- arrange(pre_year_data, Rank)
  pre_year_data[, "Cummulative_Production"] <- 
    cumsum(pre_year_data[, prod_col_num])
  next_year_data <- filter(rawDF, rawDF[, cy_col_num] == cy[3] & 
                             rawDF[, rank_col_num] > 0)
  next_year_data <- arrange(next_year_data, Rank)
  next_year_data[,"Cummulative_Production"] <- 
    cumsum(next_year_data[, prod_col_num])
  analysis_data <- rbind(cur_year_data, pre_year_data, next_year_data)
  return(analysis_data)
}

# Function to extract data for three crop year in a generalized way
insertRow <- function(existingDF, r, prod_split, tc_unit_name, start_row = 1){
  prod_col_num <- which(names(existingDF) == "Production (MT)")
  tc_col_num <- which(names(data) == tc_unit_name)
  cum_prod_num <- which(names(data) == "Cummulative_Production")
  avg_COP_col_num <- which(names(existingDF) == "Avg_COP")
  
  existingDF[seq(r+1, nrow(existingDF)+1), ] <- 
    existingDF[seq(r, nrow(existingDF)), ]
  existingDF[r, prod_col_num] <- prod_split - 
    existingDF[r-1, cum_prod_num]
  existingDF[r+1, prod_col_num] <- existingDF[r+1, prod_col_num]- 
    existingDF[r, prod_col_num]
  existingDF[, cum_prod_num] <- cumsum(existingDF[, prod_col_num])
  # existingDF$Cummulative_Production <- cumsum(existingDF$`Production (MT)`)
  x = existingDF[start_row:r, tc_col_num]
  w = existingDF[start_row:r, prod_col_num]
  existingDF[start_row:r, avg_COP_col_num] <- sum(x*w)/sum(w)
  # assign('existingDF',existingDF, envir = .GlobalEnv)
  # assingin data of runtime environment to Global environment
  return(existingDF)
}



# Function to extract data for three crop year based on total cost unit 
# provided by user
avg_cop_data <- function(data, tc_unit_name, cy){
  avg_cop_by_cy <- vector("list", length(cy))
  cy_col_num <- which(names(data) == "Crop Year")
  prod_col_num <- which(names(data) == "Production (MT)")
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
    avg_cop_by_cy[[i]] <- existingDF
  }
  data_avg_cop <- do.call("rbind", avg_cop_by_cy)
  return(data_avg_cop)
}


# for one specific purpose function
# *******************
          # ******************
                      # ******************
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







