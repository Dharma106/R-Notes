# To install packages which are not there in the system
req_pkgs <- c("ggplot2", "readxl", "reshape2", "grid")
# if packages exists it will evalute to character(0).
pkgs_avail <- req_pkgs[!(req_pkgs %in% installed.packages())]

if(length(pkgs_avail)){
  install.packages(pkgs_avail)
}

library(readxl)
library(reshape2)
library(ggplot2)
library(grid)


cost_component_graph <- function(data, year, title_name){
  # Exit function if mentioned variable is missing.
  if(missing(data)){
    stop("Data is missing with no default")
  }
  # Counting total rows & Read variable names
  total_row <- nrow(data)
  col_names <- colnames(data)
  # Exit function if mentioned variable is missing.
  if((is.null(which(col_names == "Inputs"))))
    stop("Input Component Column is missing")
  # To check whether user has provided the arguments or not
  if(missing(year)){
    year <- readline("Enter year in the format 2001 for 2001-02: ")
  }
  year <- as.integer(year)
  cur_year <- paste(year, substr(year+1, 3, 4), sep = "-")
  pre_year <- paste(year-1, substr(year, 3, 4), sep = "-")
  nxt_year <- paste(year+1, substr(year+2, 3, 4), sep = "-")
  for_year_col <- c(pre_year,cur_year,nxt_year)
 
  # To check the created variables in the data set.
  req_crop_yr_var <- match(for_year_col, col_names)
  if(all(is.na(req_crop_yr_var)== TRUE))
    stop("Mentioned year variable is missing")
  if(any(is.na(req_crop_yr_var) == TRUE))
    req_crop_yr_var <- as.integer(na.omit(req_crop_yr_var))
  
  # To find the cloumn number for specified variables.
  input_col_num <- which(col_names == "Inputs")
  tot_cst_row_num <- grep("Total Cost",
                          x = data$Inputs)
  # If vairable for division is not found stop
  if(length(tot_cst_row_num) == 0){
    stop("Required Denominator for % calcuation could not be found")
  }
  # when two location found for divisor variable 
  if(length(tot_cst_row_num) == 2){
    num1 <- tot_cst_row_num[1]
    num2 <- tot_cst_row_num[2]
    tot_cst_row_num <- c(num1, num2)
  } else{
    num1 <- tot_cst_row_num
  }
  
  # Defining new data set for percentage calcuation
  per_contribution <- data[1:num1,c(input_col_num,req_crop_yr_var)]
  for(row in 1:num1){
    per_contribution[row, -c(input_col_num)] <- 
      per_contribution[row, -c(input_col_num)]/per_contribution[num1,-c(input_col_num)]
  }
  # Arranging data in order of direct cost and indirect cost
  order_seq <- c(10:1,11)
  per_contribution$Inputs <- factor(per_contribution$Inputs, 
                                    levels = per_contribution$Inputs[order_seq])
  # Using melt function from reshape2 to reorgainse data
  # by year variable in one column 
  data_for_graphs <- 
    melt(per_contribution[1:(num1-1),],
         id.vars = input_col_num)
  g <- ggplot(data_for_graphs, aes(x = variable, y = value, fill = Inputs)) +
    geom_bar(position = "fill", stat = "identity", width = 0.4)
  g <- g + scale_y_continuous(labels = scales::percent, 
                              breaks = scales::pretty_breaks(10))
  # storing data name as character for title
  if(missing(title_name)){
    title_name <- deparse(substitute(data))
  }
  g <- g + ggtitle(paste(title_name,"- Total Cost with % share of each component"))
  g <- g + labs(x= NULL, y= NULL)
  shades_of_blue <- c("lightblue2","steelblue2","steelblue3",
                      "dodgerblue3","dodgerblue4")
  shades_of_orange <- c("grey88", "bisque3", "lightsalmon",
                        "darkorange3", "darkorange4")
  color_name <- c(shades_of_orange, shades_of_blue)
  g <- g + scale_fill_manual(values = color_name)
  g <- g +theme(
    # panel.background = element_rect(linetype = "solid", fill =  "grey99" ),
    panel.background = element_blank(),
    panel.grid.major.y = element_line(linetype = "solid", color = "grey75"),
    panel.grid.minor.y = element_blank(), 
    panel.grid.major.x = element_blank(),
    # axis.line.y = element_line(linetype = 1),
    axis.text = element_text(face = "bold", colour = "black")
  )
  
  # For annotation to be plotted above the bar
  data[, req_crop_yr_var] <- sapply(data[,req_crop_yr_var], 
                                    sprintf, fmt = "%.0f", 
                                    USE.NAMES = TRUE)
  data <- as.data.frame(data)
  tot_cst_unit <- sapply(tot_cst_row_num,function(x) 
    substr(data[x,1],
           start =13,
           stop = nchar(trimws(data[x,1]))-1))
  tot_cst_value <- lapply(req_crop_yr_var, function(x) data[tot_cst_row_num, x])
  final_label <- sapply(tot_cst_value, function(x) paste(x, tot_cst_unit))
  if(!is.null(dim(final_label))){
    top_label <- paste(final_label[1,], final_label[2,], sep = "\n")
  }else{
    top_label <- final_label
  }
  g <- g + annotate("text", x = col_names[req_crop_yr_var], 
               y = 1.04, label = top_label, size = 3)
  print(g)
  note <- paste("Note:","Direct cost in shades of blue",
                "Indirect cost in shades of orange", 
                sep = "\n", collapse = "\n")
  # Creating text on empty space of graph using grid.text from grid packages
  grid.text(note, x = 0.7, y = 0.1, gp = gpar(cex = .7), just = "left")
  # Storing the final graph to an R object
  g <- recordPlot()
  return(g)
}

