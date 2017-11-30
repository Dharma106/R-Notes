# This code is to read, extract & save data corresponding to Robusta & Arabica tab of all country excel file.

library(XLConnect)
library(tidyverse)
options(java.parameters = "-Xmx1024m")

default_dir <- getwd()
setwd("R:/LTPM/COP- Database")

# Reading individual country excel file with coffee type.
Brazil_Robusta <- read_xlsx("Brazil.xlsx", sheet = "Robusta")
Brazil_Arabica <- read_xlsx("Brazil.xlsx", sheet = "Arabica")
China_Arabica <- read_xlsx("China.xlsx", sheet = "Arabica")
Colombia_Arabica <- read_xlsx("Colombia.xlsx", sheet = "Arabica")
Ethiopia_Arabica <- read_xlsx("Ethiopia_Arabica.xlsx", sheet = "Arabica") 
Guatemala_Arabica <- read_xlsx("Guatemala.xlsx", sheet = "Arabica")
Honduras_Arabica <- read_xlsx("Honduras.xlsx", sheet = "Arabica")
India_Arabica <- read_xlsx("India.xlsx", sheet = "Arabica")
India_Robusta <- read_xlsx("India.xlsx", sheet = "Robusta")
Indonesia_Robusta <- read_xlsx("Indonesia Robusta.xlsx", sheet = "Robusta")
Mexico_Arabica <- read_xlsx("Mexico.xlsx", sheet = "Arabica")
Peru_Arabica <- read_xlsx("Peru.xlsx", sheet = "Arabica")
Thailand_Arabica <- read_xlsx("Thailand.xlsx", sheet = "Robusta")
Uganda_Robusta <- read_xlsx("Uganda.xlsx", sheet = "Robusta")
Vietnam_Robusta <- read_xlsx("Vietnam_Robusta.xlsx", sheet = "Robusta")
Vietnam_Arabica <- read_xlsx("Vietnam_Arabica.xlsx", sheet = "Arabica")

# Defining name for backup COP file
COP_Mon_back_name <- paste0("COP for ALl country (",
                            format(Sys.Date()-30, "%b-%Y"),") ",
                            format(Sys.Date(),"%d-%b"), ".xlsx")

# sheet_name <- c("Brazil_Robusta", "Brazil_Arabica", "China_Arabica", 
#                 "Colombia_Arabica", "Ethiopia_Arabica", "Guatemala_Arabica",
#                 "Honduras_Arabica", "India_Robusta", "India_Arabica",
#                 "Indonesia_Robusta", "Mexico_Arabica", "Peru_Arabica",
#                 "Thailand_Arabica", "Uganda_Robusta",
#                 "Vietnam_Robusta", "Vietnam_Arabica")

# To get the name of active data.frame in current environment and assinging
# name of sheet for creating a common excel file.
sheet_name <- names(which(unlist(eapply(.GlobalEnv, is.data.frame))))
df_list <- lapply(sheet_name, function(x) get(x))

#Creating excel file with COP_Mon_back_name
wb <- loadWorkbook(COP_Mon_back_name, create = TRUE)
createSheet(wb, name = sheet_name)
header_style <- createCellStyle(object = wb, name = "header")
setFillPattern(header_style, fill = XLC$FILL.SOLID_FOREGROUND)
setFillForegroundColor(header_style, color = XLC$COLOR.BLUE_GREY)

# sapply(df_list, function(x) writeWorksheet(wb, x, sheet = sheet_name))

for (i in 1:length(sheet_name)){
  createSheet(wb, name = sheet_name[i])
  writeWorksheet(wb, data = df_list[[i]], sheet = sheet_name[i])
  setCellStyle(wb, sheet = sheet_name[i], 
               row = 1, col = ncol(df_list[[i]]),
               cellstyle = header_style) 
}
saveWorkbook(wb, file = "R:/LTPM/COP- Database/Monthly Backup" )




#This needs to be run where there is a space crunch for Java.

# createSheet(wb, name = sheet_name[1])
# writeWorksheet(wb, data = df_list[[1]], sheet = sheet_name[1])
# setCellStyle(wb, sheet = sheet_name[1], row = 1, col = ncol(df_list[[1]]), cellstyle = header_style) 
# 
# createSheet(wb, name = sheet_name[2])
# writeWorksheet(wb, data = df_list[[2]], sheet = sheet_name[2])
# setCellStyle(wb, sheet = sheet_name[2], row = 1, col = ncol(df_list[[2]]), cellstyle = header_style) 
# 
# createSheet(wb, name = sheet_name[3])
# writeWorksheet(wb, data = df_list[[3]], sheet = sheet_name[3])
# setCellStyle(wb, sheet = sheet_name[3], row = 1, col = ncol(df_list[[3]]), cellstyle = header_style) 
# 
# createSheet(wb, name = sheet_name[4])
# writeWorksheet(wb, data = df_list[[4]], sheet = sheet_name[4])
# setCellStyle(wb, sheet = sheet_name[4], row = 1, col = ncol(df_list[[4]]), cellstyle = header_style) 
# 
# createSheet(wb, name = sheet_name[5])
# writeWorksheet(wb, data = df_list[[5]], sheet = sheet_name[5])
# setCellStyle(wb, sheet = sheet_name[5], row = 1, col = ncol(df_list[[5]]), cellstyle = header_style) 
# 
# createSheet(wb, name = sheet_name[6])
# writeWorksheet(wb, data = df_list[[6]], sheet = sheet_name[6])
# setCellStyle(wb, sheet = sheet_name[6], row = 1, col = ncol(df_list[[6]]), cellstyle = header_style) 
# 
# createSheet(wb, name = sheet_name[7])
# writeWorksheet(wb, data = df_list[[7]], sheet = sheet_name[7])
# setCellStyle(wb, sheet = sheet_name[7], row = 1, col = ncol(df_list[[7]]), cellstyle = header_style) 
# 
# createSheet(wb, name = sheet_name[8])
# writeWorksheet(wb, data = df_list[[8]], sheet = sheet_name[8])
# setCellStyle(wb, sheet = sheet_name[8], row = 1, col = ncol(df_list[[8]]), cellstyle = header_style)
# 
# createSheet(wb, name = sheet_name[9])
# writeWorksheet(wb, data = df_list[[9]], sheet = sheet_name[9])
# setCellStyle(wb, sheet = sheet_name[9], row = 1, col = ncol(df_list[[9]]), cellstyle = header_style)
# 
# createSheet(wb, name = sheet_name[10])
# writeWorksheet(wb, data = df_list[[10]], sheet = sheet_name[10])
# setCellStyle(wb, sheet = sheet_name[10], row = 1, col = ncol(df_list[[10]]), cellstyle = header_style)
# 
# createSheet(wb, name = sheet_name[11])
# writeWorksheet(wb, data = df_list[[11]], sheet = sheet_name[11])
# setCellStyle(wb, sheet = sheet_name[11], row = 1, col = ncol(df_list[[11]]), cellstyle = header_style)
# 
# createSheet(wb, name = sheet_name[12])
# writeWorksheet(wb, data = df_list[[12]], sheet = sheet_name[12])
# setCellStyle(wb, sheet = sheet_name[12], row = 1, col = ncol(df_list[[12]]), cellstyle = header_style)
# 
# createSheet(wb, name = sheet_name[13])
# writeWorksheet(wb, data = df_list[[13]], sheet = sheet_name[13])
# setCellStyle(wb, sheet = sheet_name[13], row = 1, col = ncol(df_list[[13]]), cellstyle = header_style)
# 
# createSheet(wb, name = sheet_name[14])
# writeWorksheet(wb, data = df_list[[14]], sheet = sheet_name[14])
# setCellStyle(wb, sheet = sheet_name[14], row = 1, col = ncol(df_list[[14]]), cellstyle = header_style)
# 
# createSheet(wb, name = sheet_name[15])
# writeWorksheet(wb, data = df_list[[15]], sheet = sheet_name[15])
# setCellStyle(wb, sheet = sheet_name[15], row = 1, col = ncol(df_list[[15]]), cellstyle = header_style)
# 
# createSheet(wb, name = sheet_name[16])
# writeWorksheet(wb, data = df_list[[16]], sheet = sheet_name[16])
# setCellStyle(wb, sheet = sheet_name[16], row = 1, col = ncol(df_list[[16]]), cellstyle = header_style)
# 
# saveWorkbook(wb)
