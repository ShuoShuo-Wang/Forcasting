library(forecast)

dat <- read.csv("weekly_sales.csv")

dat_ts <- ts(dat$Demand)

#8. simple exponential smoothing method
ses_var <- ses(dat_ts, alpha = 0.3)
ses_var
 
#9. RMSE of simple exponential smoothing method 
sqrt(ses_var$model$mse)

#10. Holt's method 
holt_var <- holt(dat_ts, alpha = 0.25, beta = 0.15)

#11. winter's method
dat_hw_ts <- ts(dat$Demand, frequency = 4)
hw_var <- hw(dat_hw_ts, alpha = 0.2, beta = 0.1, gamma = 0.1)

#12. Letting R choose the arguments in all three methods
options(scipen = 10)

ses_var_auto <- ses(dat_ts)
ses_var_auto$model$par

holt_var_auto <- holt(dat_ts)
holt_var_auto$model$par

hw_var_auto <- hw(dat_hw_ts)
hw_var_auto$model$par

# performance evaluation
sqrt(ses_var_auto$model$mse)
sqrt(holt_var_auto$model$mse)
sqrt(hw_var_auto$model$mse)
