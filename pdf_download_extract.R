# This Code is to download PDF file from website and save it to specify the loaction.
# Then it will extract required table using specified keyword and will save to excel file.

extract_table_from_pdf_version_5 <- function(){
  
  # Checking required packages availability in system
  # NOTE XLConnect package requires availabilty of Java in the system.
  
  # Allcoating 1 GB space to java for smother handling 
  options(java.parameters = "-Xmx1024m")
  
  lib1 <- library(XLConnect, logical.return = TRUE)
  if(lib1 == TRUE) {
    print("XLConnect package is pre-installed")
  } else {
    install.packages("XLConnect")
    library(XLConnect)
  }
  
  # Checking pdftools, pdfsearch & readr package availability in system
  lib2 <- library(pdftools, logical.return = TRUE)
  if(lib2 == FALSE){
    install.packages("pdftools")
    library(pdftools)
  } else {
    print("pdftools package is pre-installed")
  }
  
  lib3 <- library(readr, logical.return = TRUE)
  if(lib3 == FALSE){
    install.packages("readr")
    library(readr)
  } else {
    print("readr package is pre-installed")
  }
  
  lib4 <- library(pdfsearch, logical.return = TRUE)
  if(lib4 == FALSE){
    install.packages("pdfsearch")
    library(pdfsearch)
  } else {
    print("pdfsearch package is pre-installed")
  }
  
  default_dir <- getwd()
  date <- readline(prompt = "Provide Date (YYYYMMDD) for dwonloading pdf: ")
  date <- as.integer(date)
  url <- paste0("http://bvmf.bmfbovespa.com.br/download/BOLETINSDIARIOS/bd_00_",date,".pdf")
  name_for_pdf = paste0(date,"_BMFstocks.pdf")
  name_for_page = paste0(date,"_")
  
  # Setting Working directory
  setwd(default_dir)
  
  # Download PDF file
  download.file(url, mode = "wb", destfile = name_for_pdf)
  
  # Reading downloaded pdf file
  coffee_data <- pdf_text(name_for_pdf)  
  
  find_page_number <- keyword_search(coffee_data,
                                     keyword = "Distribuição dos locais de formação de lotes café tipo 4/5")
  display_page_number <- find_page_number[[2]]
  page_num <- display_page_number[1]
  page7_contents <- coffee_data[[page_num]]
  page8_contents <- coffee_data[[page_num +1]]  
  
  # Part 1 : Data extraction from page 7
  page_7_part1_content <- read_delim(page7_contents, delim = "\t", 
                                     skip = 29)
  colnames(page_7_part1_content) <- c("UF", "Munic?pio",
                                      "Participante Deposit?rio do Agroneg?cio",
                                      "Lotes", "%")
  page_7_part1_content <- page_7_part1_content[-nrow(page_7_part1_content),]
  
  # page_7_part2_content <- read_delim(page8_contents, delim = "\t", 
  #                                    skip = 2, n_max = 9)
  # colnames(page_7_part2_content) <- c("UF", "Munic?pio",
  #                                     "Participante Deposit?rio do Agroneg?cio",
  #                                     "Lotes", "%")
  
  # Combining table extracted from page 7 and portion of page 8
  # page_7_data <- rbind(page_7_part1_content, page_7_part2_content)
  page_7_data <- page_7_part1_content
  page_7_data$`Munic?pio` <- parse_character(page_7_data$`Munic?pio`)
  
  page_7_data$`Participante Deposit?rio do Agroneg?cio` <- 
    parse_character(page_7_data$`Participante Deposit?rio do Agroneg?cio`)
  
  page_7_data$Lotes <- parse_number(page_7_data$Lotes)
  page_7_data$`%` <- parse_number(
    page_7_data$`%`, locale = locale(decimal_mark = ","))
  page_7_data <- as.data.frame(page_7_data)
  # write.csv(page_7_data, file = "page7.csv", row.names = FALSE )  
  
  # Part 2: Data extraction from page 7
  page_8_data <- read_delim(file = page8_contents,
                            delim = "\t", 
                            escape_backslash = TRUE,
                            skip = 5,
                            n_max = 55,
                            guess_max = 2)
  column_names <- colnames(page_8_data)
  temp_col_names <- c("X1","X2","X3","X4")
  colnames(page_8_data) <- temp_col_names
  
  # Reading individual columns and parsing each column according
  column1 <- page_8_data$X2[-1]
  column2 <- parse_number(page_8_data$X3[-1],
                          locale = locale(decimal_mark = ","))
  column3 <- parse_number(page_8_data$X4[-1],
                          locale = locale(decimal_mark = ",")) 
  
  page_8_data_2 <- cbind(column1, column2, column3)
  page8_na_removed <- na.omit(page_8_data_2[,])
  # page8_na_removed <- na.omit(page_8_data_2[1:length(page_8_data_2[,1]),])
  
  # to remove attributes display of na.omit funtion
  page_8_data_2 <- rbind(page8_na_removed) 
  page_8 <- parse_number(page_8_data_2) # making it numeric value
  # So it will become object type, then we need to convert to martix form.
  page_8_data_2 <- matrix(page_8, nrow = length(page_8_data_2[,1]),
                          ncol = 3, byrow = FALSE)
  page_8_data_2 <- subset(page_8_data_2, page_8_data_2[,3] <= 32.5)
  page_8_data_2 <- subset(page_8_data_2, page_8_data_2[,2] != 0.00)
  colnames(page_8_data_2) <- c("Lotes", "(%)", column_names[4])
  page_8_data_2 <- as.data.frame(page_8_data_2)
  not_req_row <- which(page_8_data_2[, 3] == 0.1)
  if(length(not_req_row) != 0){
    page_8_data_2 <- page_8_data_2[-c(not_req_row), ]
  }
  # page_8_data_2 <- page_8_data_2[-nrow(page_8_data_2), ]
  # page_8_data_2 <- page_8_data_2[-6, ]
  
  # Linking to Excel Sheet and writing the data
  
  # writeWorksheetToFile(file = "BMFstocks.xlsx",
  #                      data = page_7_data, 
  #                      sheet = paste0(name_for_page,"page7"),
  #                      styleAction = XLC$STYLE_ACTION.PREDEFINED)
  # 
  # writeWorksheetToFile(file = "BMFstocks.xlsx", 
  #                      data = page_8_data_2, 
  #                      sheet = paste0(name_for_page, "page8"),
  #                      clearSheets = FALSE, 
  #                      styleAction = XLC$STYLE_ACTION.PREDEFINED)
  
  wb <- loadWorkbook(filename = "BMFstocks.xlsx", create = TRUE)
  createSheet(wb, name = paste(name_for_page, "page7"))
  setStyleAction(wb, type = XLC$STYLE_ACTION.PREDEFINED)
  writeWorksheet(wb, data = page_7_data,
                 sheet = paste(name_for_page, "page7"))

  createSheet(wb, name = paste(name_for_page, "page8"))
  writeWorksheet(wb, data = page_8_data_2,
                 sheet = paste(name_for_page, "page8"))
  saveWorkbook(wb)  
  
  # total lotes of Page 7 and Page 8 should match.
  check <- sum(page_8_data_2[,1]) == 
    sum(page_7_data$Lotes[1:length(page_7_data$Lotes)-1])
  
  lotes_diff <- sum(page_7_data$Lotes[1:length(page_7_data$Lotes)-1]) -
    sum(page_8_data_2[,1])
  
  if(check == FALSE){
    cat("Total lotes of Page7 doesn't match to
        Total lotes of Page8\nReview   BMFstocks.xlsx file\n\n")
    cat(c("There is difference of", lotes_diff, "lotes"))
  }  
  setwd(default_dir)  
}
