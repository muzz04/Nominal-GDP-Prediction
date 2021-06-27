 install.packages("tidyr")
 install.packages("ggplot2")
 install.packages("devtools")
 install.packages("caret")
 install.packages("dplyr")

library(dplyr)
library(ggplot2)
library(reshape2)
library(tidyr)
library(caret)
 rm(list = ls())

#loading data into dataframes
gdp_factors_df <- read.csv("gdp_factors.csv")
gdp_df <- read.csv("gdp_yearly.csv")

#summary
summary(gdp_factors_df)
summary(gdp_df)

#No of rows in Data frame
nrow(gdp_factors_df)
#396

nrow(gdp_df)
#58

#Converting date to its corresponding year
gdp_factors_df$date <- as.Date(gdp_factors_df$date,"%Y-%m-%d") 
gdp_factors_df$date <- as.numeric(format(gdp_factors_df$date,"%Y"))


#Grouping by year
yearly_df <- gdp_factors_df %>%
  group_by(date) %>%
    summarise(house_completed = sum(house_completed)/ 1000,
              air_passengers = sum(air_passengers)/1000,
              manufacuting_sales = sum(manufacuting_sales) /1000,
              total_reserve = sum(total_reserve)/1000,
              low_income_per = mean(low_income_per,na.rm = T),
              fdi = mean(fdi,na.rm = T)/1000,
              male_employement_per = mean(male_employement_per,na.rm = T),
              male_unemployment_per = mean(unemployment_male_per,na.rm = T)
              )
  
  nrow(yearly_df)
  #33
  
  #summary
  summary(yearly_df)
  
  # There are 22 NA's in manufacturing out of 33 so droping it
  yearly_df$manufacuting_sales <- NULL
  
  #changing column name to year
  colnames(yearly_df)[1] <- ("year")
  
  #filtering data for year after 1986
  gdp_df <- gdp_df %>%
    filter(year >= 1987)
  
  #removing rows having na values
  yearly_df <- yearly_df %>%
    na.omit(yearly_df)
  
  #final dataset for factors mapped yearly
  yearly_df
  
  nrow(yearly_df)
  #29 rows
  
  
  ################# Univariate Analysis of Factors ####################
  
   # 1 fdi 
  
  data = yearly_df$fdi
  
  #detecting outliers from boxplot
  boxplot(data,ylab= "Values")
  
  #Normal distribution Shapiro test 
  shapiro.test(data)
  
  #p-value < 0.05 so data is not normally distributed
  
  #Performing square root transformation
  data = sqrt(data)
  
  #Checking again Shapiro test
  shapiro.test(data)
  # p value = 0.05562, which is > 0.05
  
  #NormalDistribution Histogram of transformed FDI data
  hist(data, main="FDI Distribution", 
       xlab="FDI", 
       border="black", 
       col="gray" )
  
  #assigning transformed data to FDI
  yearly_df$fdi = data
  
  
  # 2 house_completed
  
  data = yearly_df$house_completed

  #Outlier detection
  boxplot(data,ylab="Values")
  
  #Normal distribution shapiro test
  shapiro.test(data)
  
  #summary of house_completed data point
  summary(data)
  
  #Distribution Histogram
  hist(data,main="Houses completed", 
       xlab="FDI", 
       border="black", 
       col="gray")
  
  #removing left skew by subtracting max value from data
  data = 191.60 - data + 1
  
  #Performing square root transformation
  data = sqrt(data)
  
  #Normal distributed Histogram
  hist(data,main="Houses Completed", 
       xlab="FDI", 
       border="black", 
       col="gray")
  
  #Assigning values to house_completed
  yearly_df$house_completed = data
  
  # 3 air passenger
  
  data = yearly_df$air_passengers
  
  #Outlier identification
  boxplot(data)
  
  #Accessing outlier data
  out = boxplot.stats((data))$out
  
  #Oulier data
  out
  
  #Replacing outlier data with min data
  data[data %in% out] = min(data)  
  
  #summary of transformed data
  summary(data)
  
  #Plotting Distribution
  hist(data,main="Air passengers", 
       xlab="FDI", 
       border="black", 
       col="gray")
  
  
  
  #Removing right skewness of data using min value
  data = data - 19.13 + 1
  

  #Performing Square root transformation
  data = sqrt(data) 
  
  #Distribution histogram
  hist(data,main="Air passengers", 
       xlab="FDI", 
       border="black", 
       col="gray")
  
  #Normal distribution shapiro test
  shapiro.test(data)
  
  #assigning data to air passengers
  yearly_df$air_passengers = data
  
  #4 low_income_per

  data = yearly_df$low_income_per
  
  #Outlier identification
  boxplot(data)
  
  #Plotting Distribution
  hist(data,main="Low income %", 
       xlab="FDI", 
       border="black", 
       col="gray")
  
  #Checking if data is normally distributed
  shapiro.test(data)
  # p value = 0.481, which is > 0.05 hence low_income_per is equally distributed
  
  
  #5 total_reserve
  
  data = yearly_df$total_reserve
  
  
  boxplot(data)
  
  #Normal distribution shapiro test
  shapiro.test(data)
  # p value less than 0.05, hence data is not normally distributed
  
  summary(data)
  
  #Distribution Histogram plot
  hist(data,main="Total reserve", 
       xlab="FDI", 
       border="black", 
       col="gray")
  
  #removing skewness with minimum value
  data = data - 145.3 + 1  
  
  #Performing Square root transformation
  data = sqrt(data)
  
  shapiro.test(data)
  # p value > 0.05
  
  #Distribution of total_reserve data
  hist(data,main="Total reserve", 
       xlab="FDI", 
       border="black", 
       col="gray")
  
  yearly_df$total_reserve = data
  
  #6 employment
  
  data = yearly_df$male_employement_per
  boxplot(data)
  
  #Normal distribution shapiro test
  shapiro.test(data)
  
  #Distribution histogram
  hist(data,main="Male employment", 
       xlab="FDI", 
       border="black", 
       col="gray")
  
  summary(data)
  
  #removing skewness using max value
  data = 33.64 - data + 1
  
  shapiro.test(data)
  
  data = sqrt(data)
  
  #Log transformation of data
  data = log(data)   
  
  shapiro.test(data)
  #p value > 0.05
  
  hist(data,main="Male employment %", 
       xlab="FDI", 
       border="black", 
       col="gray")
  
  #assigning transformed values to male_employment_per
  yearly_df$male_employement_per = data
  
  #7 unemployment
  
  data = yearly_df$male_unemployment_per
  
  
  boxplot(data)
  
  #Normal distribution shapiro test
  shapiro.test(data)
  
  
  summary(data)
  
  #Distribution histogram
  hist(data,main="Male unemployment %", 
       xlab="FDI", 
       border="black", 
       col="gray")
  
  #removing skewness using min value
  data = data - 6.135 + 1 
  
  #Performing log transformation
  data = log(data)
  

  hist(data,main="Male unemployment %", 
       xlab="FDI", 
       border="black", 
       col="gray")
  

  shapiro.test(data)
  # p value > 0.05
  
  #assigning transformed values to male_unemployement_per
  yearly_df$male_unemployment_per = data
  

  #merging with GDP data on year
  merged_df <- merge(gdp_df,yearly_df,by = "year",all.y = T)
  
  #tranforming merged data to year wise
  melted_data <- melt(merged_df,id.vars = "year")
  
  #Plot to show yearly trend of each variable
  ggplot(melted_data, aes(year,value)) +  
    geom_point() +
    facet_wrap(.~variable,scales = "free_y",ncol = 2)+
    theme_bw() + 
    scale_x_continuous(breaks = seq(1987,2019,5))+
    labs(title = "Yearly trend of variables")+
    ylab("Values")+
    xlab("Year")
  
  
  #Calculating correlation between variables
  corr_mat <- cor(merged_df[,-c(1,2)]) #skipping first two columns
  
  corr_mat
  
  # Heat map showing intensity of correlation 
  qplot(x = Var1, y = Var2,
        data = melt(corr_mat, use = "p"),
        fill = value,
        geom = "tile") +
    scale_fill_gradient2(limits = c(-1, 1))+
    xlab("Variable")+
    ylab("Variable")
  
  #Getting index from correlation matrix to identify variables with high correlation
  index <- findCorrelation(corr_mat,.92)
  
  #Getting names from index 
  to_be_removed <- colnames(corr_mat)[index]
  
  to_be_removed
  
  #Droping columns which are correlated
  merged_df <- merged_df[!names(merged_df) %in% to_be_removed]
  
  #Filtering data to make data for training linear regression model, taking data of years < 2017
  data_model <- merged_df %>%
    filter(year < 2017)

  #building linear regression model, removing first column (year values)
  lmMod <- lm(gdp ~ ., data=data_model[,-c(1)])
  
  #Summary of model
  summary (lmMod)
  
  #Coefficients of variables
  summary(lmMod)$coef
  
  #Variance table
  anova(lmMod)
  
  #Getting testing data
  predict_data = merged_df %>%
   filter(year == 2017 | year == 2018)
  
  #Assigning GDP value of year 2018
  predict_data[predict_data$year == 2018,"gdp"] <- 1.712
    
  #Predicting data using model
  predictions <- lmMod %>% 
    predict(predict_data[,-c(1,2)])
  
  predictions
  
  prediction <- c(predictions[1],predictions[2])
  actual <- predict_data$gdp
  
  # Root mean square error
  
  RMSE(prediction,actual)
  
  #predicted value of GDP of year 2017
  
  prediction[1]
  # actual GDP value is 1.64
  
  #predicted value of GDP of year 2018
  
  prediction[2]
  # actual GDP value is 1.712
  
  
  