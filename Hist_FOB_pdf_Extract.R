library(XLConnect)
library(tabulizer)
library(pdfsearch)
library(pdftools)
library(readr)
library(readxl)

options(java.parameters = "-Xmx1024m")

default_dir <- getwd()
# Set you working directory 
setwd("C:/users/documents")

pdf_file_names <- list.files(pattern = ".pdf")
data <- vector("list", length(pdf_file_names))
tot_pdf_file = length(pdf_file_names)
not_used_pdf <- vector()


for(i in 1:tot_pdf_file){
  pdf_name = pdf_file_names[i]
  raw_data = extract_tables(pdf_name)
  if(length(raw_data) != 0){
    len = length(raw_data)
    if(nrow(raw_data[[len]]) == 12){
      pdf_content <- extract_text(pdf_name)
      pg_num_for_date <- keyword_search(pdf_content, 
                                        keyword = "www.coffeeagents.com")
      date_content <- read_delim(pdf_content, delim = "\t",
                                 skip = pg_num_for_date[[3]],
                                 n_max = 2)
      
      colnames(date_content) <- "Date"
      date_content$Date <- parse_character(date_content$Date)
      pdf_date <- as.character(na.exclude(date_content$Date))
      pdf_date <- trimws(pdf_date)
      pdf_date <- pdf_date[1]
      for_date = substr(pdf_date,
                        start = nchar(pdf_date)-10,
                        stop = nchar(pdf_date))
      for_date = trimws(for_date)
      
      data[[i]] = as.data.frame(raw_data[[len]])
      data[[i]] = data[[i]][-1, ]
      data[[i]][, "Date"] <- for_date
      data[[i]][, "File_Name"] <- pdf_name
    } else{
      not_used_pdf[i] = pdf_name
      next
    } 
    
  } else {
    not_used_pdf[i] = pdf_name
    next
  }
}

# PDF file for which data could not be extracted
not_used_pdf <- na.omit(not_used_pdf)
not_used_pdf <- as.data.frame(not_used_pdf)
not_used_pdf <- as.character(not_used_pdf[, 1])

# Binding list type data into one file
FOB_raw_data <- dplyr::bind_rows(data)
final_data <- FOB_raw_data[, - ncol(FOB_raw_data)]
colnames(final_data) <- c("Origin", "Shipping Month",
                          "Exchange", "Diff. c/lb", "Date", "File_Name")
# final_data <- final_data[, c(5, 1:4)]

# Writing data to an excel file
wb <- loadWorkbook("FOB_differentials.xlsx", create = TRUE)
createSheet(wb, name = "FOB_differentials")
createSheet(wb, name = "Raw_Data")
csHeader <- createCellStyle(wb, name = "header")
setFillPattern(csHeader, fill = XLC$FILL.SOLID_FOREGROUND)
setFillForegroundColor(csHeader, color = XLC$COLOR.GREY_25_PERCENT)
writeWorksheet(wb, data = FOB_raw_data, sheet = "Raw_Data")
writeWorksheet(wb, data = final_data, sheet = "FOB_differentials")
saveWorkbook(wb)
