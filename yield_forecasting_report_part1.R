
library(excel.link)
library(forecast)
library(tseries)
library(rmarkdown)

options(digits = 4)

# This code is for time series modelling and forecasting.
# Part 1
  # Loading data and arranging for the analysis.

coffee_type <- readline(prompt = "Specify type of coffee (Robusta \\ Arabica) for analysis : ")
coffee_yield_country_wise <- xl.read.file("Y:\\coffee\\Trainee\\Analaysis & Modelling\\Forecasting Model\\LTPM Coffee\\Coffee_Yield_Country_Wise.xlsx",
                                          xl.sheet = coffee_type)
options(digits = 4)
display_country_number <- data.frame(
  Country_Name = colnames(coffee_yield_country_wise)[3:32])
print(display_country_number)

# Reading input of Country number displayed on the screen from user.
country_name_num <- readline(
  "Provide Country Name Number for analysis from displayed list:  ")
country_name_num <- as.integer(country_name_num)


crop_year <- readline(
  "Till which Year (e.g. 2015 for 15/16 crop year) data to be considered: ")
crop_year <- as.integer(crop_year)

country_name = colnames(coffee_yield_country_wise)[3:32]
year <- crop_year %% 1000 + 1

country <- country_name[country_name_num]

# Training data for modelling purpose
yield_training_data <- 
  coffee_yield_country_wise[1:year,
                            c(1, 2, 2 + country_name_num)]

# Test data for forecasting purpose
yield_test_data <- 
  coffee_yield_country_wise[c(year+1, year+2),
                            c(1, 2, 2 + country_name_num)]

no_of_obs <- length(yield_training_data[, 3])


# Part 2 
  ## Graphical Analysis
plot(yield_training_data[, 3], 
     main = paste0(country," ", coffee_type, " Yield (MT/ha)"),
     xlab = "Crop-Year", 
     ylab = "Yield(MT/ha)",
     type = "o",
     xaxt = "n")
axis(side = 1,
     at = 1: no_of_obs, 
     labels = yield_training_data[,1],
     las = 3,
     col.axis = "blue")
axis(side = 2, col.axis = "blue")

  ## Basic Statistical Summary
summary(yield_training_data[,3])

  ## Linear Regression Model
Yield <- yield_training_data[, 3]
linear_fit <- lm(Yield ~ Year, 
                 data = yield_training_data)

summary(linear_fit)
linear_predict <- data.frame(
  Crop_Year = yield_test_data[,1],
  yield = predict.lm(linear_fit,
                     newdata = yield_test_data,
                     interval = "prediction",
                     level = 0.80))

qqnorm(linear_fit$residuals)
qqline(linear_fit$residuals)
shapiro.test(linear_fit$residuals)


## ExponenTial Smoothing (ETS) Model

ets_fit <- ets(yield_training_data[,3])
summary(ets_fit)

plot(forecast(ets_fit, h = 2), 
     ylab = "Yield (MT/ha)",
     xlab = "Crop-Year",
     main = paste0(country," ", coffee_type,
                   ": Yield Forecast using ExponenTial Smoothing (ETS) model"),
     type = "o",
     xaxt = "n")

axis(side = 1,
     at = 1: no_of_obs, 
     labels = yield_training_data[,1],
     las = 3,
     col.axis = "blue")
axis(side = 2,
     col.axis = "blue")

lines(ets_fit$fitted,
      col = "red",
      type = "o")
lines(linear_fit$fitted.values,
      col = "blue",
      type = "o")
legend("topleft",
       lty= 1,
       col=c("black","blue","red"), 
       legend = c("Original Data", "Linear Regression Fit",
                  "ExponenTial Smoothing Fit"), 
       bty = "n",
       cex = .7)

qqnorm(ets_fit$residuals)
qqline(ets_fit$residuals)
shapiro.test(ets_fit$residuals)
forecast(ets_fit, h = 2)


## Auto Regressive Integrate Moving Average (ARIMA) Model

# adf.test(yield_training_data[, 3])
# kpss.test(yield_training_data[, 3])

arima_fit <- auto.arima(yield_training_data[, 3],
                        seasonal = FALSE)
summary(arima_fit)
plot(forecast(arima_fit, h = 2),
     ylab = "Yield (MT/ha)",
     xlab = "Crop-Year",
     main = paste0(country," ", coffee_type,": Yield Forecast using ARIMA Model"),
     type = "o",
     xaxt = "n")
axis(side = 1,
     at = 1: no_of_obs, 
     labels = yield_training_data[,1],
     las = 3,
     col.axis = "blue")
axis(side = 2,
     col.axis = "blue")

lines(arima_fit$fitted,
      col = "blue",
      type = "o")
lines(linear_fit$fitted.values,
      col = "red",
      type = "o")

legend("topleft", 
       lty= 1, 
       col=c("black","blue","red"), 
       legend = c("Yield data", "ARIMA Model","Linear Regression Fit"),
       bty = "n",
       cex = .7)

qqnorm(arima_fit$residuals)
qqline(arima_fit$residuals)
shapiro.test(arima_fit$residuals)

forecast(arima_fit, h = 2)


# To generate the word document report of the analysis.
render(input = "coffee_report.Rmd", 
       output_format = "word_document",
       output_file = paste0(country,"_", coffee_type,"_Modelling_Report.docx"),
       output_dir = "Y:/coffee/Trainee/Analaysis & Modelling/Forecasting Model/LTPM Coffee", quiet = TRUE)
