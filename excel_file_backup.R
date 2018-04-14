# This code is to read, extract & save data corresponding to Robusta & Arabica tab of all country excel file.
req_pkg <- c("XLConnect", "readxl")
pkg_avail <- req_pkg[!(req_pkg %in% installed.packages())]
if(length(pkg_avail)){
  install.packages(pkg_avail)
}
# Calling required library functions in working environments
library(XLConnect)
library(readxl)
# Allocating 1 gb space to java handler
options(java.parameters = "-Xmx1024m")

# Reading individual country excel file with coffee type. 
# Condsidering these file exists in the system
B_R <- read_xlsx("B.xlsx", sheet = "R")
B_A <- read_xlsx("B.xlsx", sheet = "A")
C_A <- read_xlsx("C.xlsx", sheet = "A")
C_A <- read_xlsx("C.xlsx", sheet = "A")
E_A <- read_xlsx("E.xlsx", sheet = "A") 
G_A <- read_xlsx("G.xlsx", sheet = "A")
H_A <- read_xlsx("H.xlsx", sheet = "A")
I_A <- read_xlsx("I.xlsx", sheet = "A")
I_R <- read_xlsx("I.xlsx", sheet = "R")
IA_R <- read_xlsx("IA.xlsx", sheet = "R")
M_A <- read_xlsx("M.xlsx", sheet = "A")
P_A <- read_xlsx("P.xlsx", sheet = "A")
T_A <- read_xlsx("T.xlsx", sheet = "R")
U_R <- read_xlsx("U.xlsx", sheet = "R")
V_R <- read_xlsx("V_R.xlsx", sheet = "R")
V_A <- read_xlsx("V_A.xlsx", sheet = "A")

# Defining name for backup file 
monthly_backup_name <- paste0("Backup data for ALl country (",
                            format(Sys.Date()-30, "%b-%Y"),") ",
                            format(Sys.Date(),"%d-%b"), ".xlsx")

# To get the name of active data.frame in current environment and assinging
# data name as the sheet name for creating a excel file.
sheet_name <- names(which(unlist(eapply(.GlobalEnv, is.data.frame))))
df_list <- lapply(sheet_name, function(x) get(x))

#Creating excel file with file name as "monthly_backup_name"
wb <- loadWorkbook(monthly_backup_name, create = TRUE)
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
saveWorkbook(wb, file = "C:/user/documents/Backup")

