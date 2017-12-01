# Initializing packages to read pdf contents and writing back the required content ("Using keywords") to excel 

library(tabulizer)
library(pdftools)
library(pdfsearch)
library(plyr)
library(XLConnect)

# To unzip the zip file.
unzip("OUTWARD Sept 2017.zip")

# storing directory location as variable
directory <- paste0(getwd(), c("/OUTWARD"))

# Reading all the folders of unzipped file as an object.
read_folder <- list.files(directory)

# Creating a temp dataframe with intial value as 0. 
temp_data <- data.frame("X1" = "0", "X2"="0", "X3"="0", 
                        "X4"="0", "X5"="0", "X6"="0", 
                        "X7"="0", "X8"="0")

# Loop to read all the pdf files of all the sub-folders of individual folder.
# First loop to read subfolder of folder listed in read folder object 
for (i in 1 : length(read_folder)) {
  sub_dir <- paste0(directory, "/", read_folder[i])
  read_sub_folder <- list.files(sub_dir)
  
  # To read the pdf files of each sub folder.
  for (i in 1:length(read_sub_folder)) {
    sub_dir_pdf <- paste0(sub_dir, "/", read_sub_folder[i])
    read_pdf <- pdf_text(sub_dir_pdf)
    keyword_location <- keyword_search(read_pdf, keyword = "COFFEE")
    page_num <- keyword_location$page_num
    page_num <- unique(page_num)
    
    # Condition for extracting data only if the page_num is non-zero (i.e. read contents only when related keywords are there).
    if (length(page_num) != 0) {
      pdf_data <- vector("list", length(page_num))
      for (i in 1:length(page_num)) {
        pdf_data[[i]] <- extract_tables(sub_dir_pdf, 
                                        pages = page_num[i],
                                        guess = FALSE)
        pdf_data[[i]] <- as.data.frame(pdf_data[[i]])
        pdf_data[[i]] <- pdf_data[[i]][-c(1:6),]
      }
      export_data <- do.call("rbind.fill", pdf_data)
      append_data <- rbind.fill(temp_data, export_data)
      temp_data <- append_data
    } 
    next
  }
  # temp_data <- append_data
}

# Creating workbook and saving extreted content to sheet.
wb <- loadWorkbook("Indo_Custom_PDF_data.xlsx", create = TRUE)
createSheet(wb, name = "Indo_Custom_Extract")
setColumnWidth(wb,sheet = "Indo_Custom_Extract", width = 40)
setRowHeight(wb, sheet = "Indo_Custom_Extract", height = 15)
writeWorksheet(object = wb, data = temp_data, sheet = "Indo_Custom_Extract")
saveWorkbook(wb)


