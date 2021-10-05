housing_prices <- read.csv("housing_prices_by_state.csv")
n <- nrow(housing_prices)
p<- ncol(housing_prices)
new_prices <- data.frame()
names_5 <- names(housing_prices)[1:5]
month_countdown <- 12:1
months_reverse <- paste("month-", month_countdown, sep = "")
month_countup <- c(6, 12)
months_forward <- paste("month+", month_countup, sep = "")
names <- c(names_5, months_reverse, months_forward)
for(i in 1:n){
  print(i)
  for(j in 6:(p-24)){
    #print(j)
    bound<-cbind(housing_prices[i, 1:5], housing_prices[i, j:(j+11)], 
                 housing_prices[i,(j+16)], housing_prices[i, (j+24)])
    #print(bound)
    new_df <- data.frame(bound)
    #print(new_df)
    new_df <- unname(new_df)
    #print("unnamed")
    colnames(new_df) <- names
    #print(new_df)
    new_prices <- rbind(new_prices, new_df)
  }
}
write.csv(new_prices, "one_year_data_six_months_one_year_prediction.csv")
