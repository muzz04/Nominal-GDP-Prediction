
library(dplyr)
library(readxl)
library(tidyr)
library(ggplot2)
library(scales)
library(stargazer)
library(purrr)
library(cluster)
library(dendextend)

rm(list = ls())

#loading the gdp excel file
gdp<- read_excel("GDP_value.xlsx")

#loading the indicators excel file
gdp_byindustry <- read_excel("Real Gdp By Industry.xlsx")


#summary
summary(gdp)
summary(gdp_byindustry)

#filtering data for year after 2001
gdp <- gdp %>%
  filter(Year >= 2001)

#merging the gdp and gdp_byindustry data
data_merged <- merge(gdp,gdp_byindustry,by = "Year",all.y = T)

#Analysis of the different factors

# 1. Agriculture_forestry_fishing_hunting

data_val = gdp_byindustry$Agriculture_forestry_fishing_hunting

#detecting the outliers present using boxplot
boxplot(data_val,ylab= "Values")

# Doing Normal distribution by Shapiro test 
shapiro.test(data_val)

# Since value is > 0.05 so data is normalized and we
# are considering this variable for analysis.

#summary of data
summary(data_val)

#Histogram of data
hist(data_val, main="Agriculture_forestry_fishing_hunting Distribution", 
     xlab="Agriculture_forestry_fishing_hunting", 
     border="black", 
     col="lightblue" )

# 2. Mining_oil_gas_extraction

data_val = gdp_byindustry$Mining_oil_gas_extraction

#detecting the outliers present using boxplot
boxplot(data_val,ylab= "Values")

# Doing Normal distribution by Shapiro test 
shapiro.test(data_val)

# Since value is > 0.05 so data is normalized and we
# are considering this variable for analysis

#summary of data
summary(data_val)

#Histogram of data
hist(data_val, main="Mining_oil_gas_extraction Distribution", 
     xlab="Mining_oil_gas_extraction", 
     border="black", 
     col="lightblue" )

# 3. Utilities

data_val = gdp_byindustry$Utilities

#detecting the outliers present using boxplot
boxplot(data_val,ylab= "Values")

# Doing Normal distribution by Shapiro test 
shapiro.test(data_val)

# Since value is > 0.05 so data is normalized and we
# are considering this variable for analysis

#summary of data
summary(data_val)

#Histogram of data
hist(data_val, main="Utilities", 
     xlab="Utilities", 
     border="black", 
     col="lightblue" )


# 4. Construction

data_val = gdp_byindustry$Construction

#detecting the outliers present using boxplot
boxplot(data_val,ylab= "Values")

# Doing Normal distribution by Shapiro test 
shapiro.test(data_val)

# Since value if > 0.05 so data is normalized and we
# are considering this variable for analysis

#summary of data
summary(data_val)

#Histogram of data
hist(data_val, main="Construction", 
     xlab="Construction", 
     border="black", 
     col="lightblue" )

# 5. Manufacturing

data_val = gdp_byindustry$Manufacturing

#detecting the outliers present using boxplot
boxplot(data_val,ylab= "Values")

# Doing Normal distribution by Shapiro test 
shapiro.test(data_val)

# Since value if > 0.05 so data is normalized and we
# are considering this variable for analysis

#summary of data
summary(data_val)

#Histogram of data
hist(data_val, main="Manufacturing", 
     xlab="Manufacturing", 
     border="black", 
     col="lightblue" )



# 6. Wholesale_trade

data_val = gdp_byindustry$Wholesale_trade

#detecting the outliers present using boxplot
boxplot(data_val,ylab= "Values")

# Doing Normal distribution by Shapiro test 
shapiro.test(data_val)

# Since value if > 0.05 so data is normalized and we
# are considering this variable for analysis

#summary of data
summary(data_val)

#Histogram of data
hist(data_val, main="Wholesale_trade", 
     xlab="Wholesale_trade", 
     border="black", 
     col="lightblue" )


# 7. Retail_trade

data_val = gdp_byindustry$Retail_trade

#detecting the outliers present using boxplot
boxplot(data_val,ylab= "Values")

# Doing Normal distribution by Shapiro test 
shapiro.test(data_val)

# Since value if > 0.05 so data is normalized and we
# are considering this variable for analysis

#summary of data
summary(data_val)

#Histogram of data
hist(data_val, main="Retail_trade", 
     xlab="Retail_trade", 
     border="black", 
     col="lightblue" )


# 8. Transportation_warehousing

data_val = gdp_byindustry$Transportation_warehousing

#detecting the outliers present using boxplot
boxplot(data_val,ylab= "Values")

# Doing Normal distribution by Shapiro test 
shapiro.test(data_val)

# Since value if > 0.05 so data is normalized and we
# are considering this variable for analysis

#summary of data
summary(data_val)

#Histogram of data
hist(data_val, main="Transportation_warehousing", 
     xlab="Transportation_warehousing", 
     border="black", 
     col="lightblue" )

# 9. Information_cultural_industries 

data_val = gdp_byindustry$Information_cultural_industries

#detecting the outliers present using boxplot
boxplot(data_val,ylab= "Values")

# Doing Normal distribution by Shapiro test 
shapiro.test(data_val)

# Since value if > 0.05 so data is normalized and we
# are considering this variable for analysis

#summary of data
summary(data_val)

#Histogram of data
hist(data_val, main="Information_cultural_industries", 
     xlab="Information_cultural_industries", 
     border="black", 
     col="lightblue" )

# 10. Finance_insurance_realestate_renting_leasing

data_val = gdp_byindustry$Finance_insurance_realestate_renting_leasing

#detecting the outliers present using boxplot
boxplot(data_val,ylab= "Values")

# Doing Normal distribution by Shapiro test 
shapiro.test(data_val)

# Since value if > 0.05 so data is normalized and we
# are considering this variable for analysis

#summary of data
summary(data_val)

#Histogram of data
hist(data_val, main="Finance_insurance_realestate_renting_leasing", 
     xlab="Finance_insurance_realestate_renting_leasing", 
     border="black", 
     col="lightblue" )

# 11. Professional_scientific_technicalservices

data_val = gdp_byindustry$Professional_scientific_technicalservices

#detecting the outliers present using boxplot
boxplot(data_val,ylab= "Values")

# Doing Normal distribution by Shapiro test 
shapiro.test(data_val)

# Since value if > 0.05 so data is normalized and we
# are considering this variable for analysis

#summary of data
summary(data_val)

#Histogram of data
hist(data_val, main="Professional_scientific_technicalservices", 
     xlab="Professional_scientific_technicalservices", 
     border="black", 
     col="lightblue" )

# 12. Administrative_support_wastemanagement_remediationservices

data_val = gdp_byindustry$Administrative_support_wastemanagement_remediationservices

#detecting the outliers present using boxplot
boxplot(data_val,ylab= "Values")

# Doing Normal distribution by Shapiro test 
shapiro.test(data_val)

# Since value if > 0.05 so data is normalized and we
# are considering this variable for analysis

#summary of data
summary(data_val)

#Histogram of data
hist(data_val, main="Administrative_support_wastemanagement_remediationservices", 
     xlab="Administrative_support_wastemanagement_remediationservices", 
     border="black", 
     col="lightblue" )

# 13. Arts_entertainment_recreation

data_val = gdp_byindustry$Arts_entertainment_recreation

#detecting the outliers present using boxplot
boxplot(data_val,ylab= "Values")

# Doing Normal distribution by Shapiro test 
shapiro.test(data_val)

# Since value if > 0.05 so data is normalized and we
# are considering this variable for analysis

#summary of data
summary(data_val)

#Histogram of data
hist(data_val, main="Arts_entertainment_recreation", 
     xlab="Arts_entertainment_recreation", 
     border="black", 
     col="lightblue" )

# 14. Accommodation_food_services

data_val = gdp_byindustry$Accommodation_food_services

#detecting the outliers present using boxplot
boxplot(data_val,ylab= "Values")

# Doing Normal distribution by Shapiro test 
shapiro.test(data_val)

#p-value < 0.05 so the data is not normally distributed.

#summary of data
summary(data_val)

#Histogram of data
hist(data_val, main="Accommodation_food_services", 
     xlab="Accommodation_food_services", 
     border="black", 
     col="lightblue" )

# Removing the right skewness by subtracting from minimum value
data_val = data_val - 88.64 + 1

#doing square root transformation
data_val = sqrt(data_val)

#Checking again the Shapiro test if the tranformation gives value > 0.05
shapiro.test(data_val)

#Doing Normal distributed Histogram
hist(data_val, main="Accommodation_food_services", 
     xlab="Accommodation_food_services", 
     border="black", 
     col="lightblue" )

#Assigning the data back
gdp_byindustry$Accommodation_food_services = data_val

# 15. Other_private_services

data_val = gdp_byindustry$Other_private_services

#detecting the outliers present using boxplot
boxplot(data_val,ylab= "Values")

# Doing Normal distribution by Shapiro test 
shapiro.test(data_val)

# Since value is > 0.05 so data is normalized and we
# are considering this variable for analysis

#summary of data
summary(data_val)

#Histogram of data
hist(data_val, main="Other_private_services", 
     xlab="Other_private_services", 
     border="black", 
     col="lightblue" )



#16. Male Employment rate

data_val = gdp_byindustry$Employment_rate_male

#detecting the outliers present using boxplot
boxplot(data_val,ylab= "Values")

# Doing Normal distribution by Shapiro test 
shapiro.test(data_val)

#p-value < 0.05 so the data is not normally distributed.

#summary of data
summary(data_val)

#Histogram of data
hist(data_val, main="Employment_rate_male", 
     xlab="Employment_rate_male", 
     border="black", 
     col="lightblue" )

# Removing the right skewness by subtracting from minimum value
data_val = data_val - 29.73 + 1

#doing square root transformation
data_val = sqrt(data_val)

#Checking again the Shapiro test if the tranformation gives value > 0.05
shapiro.test(data_val)

#Doing Normal distributed Histogram
hist(data_val, main="Employment_rate_male", 
     xlab="Employment_rate_male", 
     border="black", 
     col="lightblue" )

#Assigning the data back
gdp_byindustry$Employment_rate_male = data_val


# 17. Female employment rate

data_val = gdp_byindustry$Employment_female

#detecting the outliers present using boxplot
boxplot(data_val,ylab= "Values")

# Doing Normal distribution by Shapiro test 
shapiro.test(data_val)

#p-value < 0.05 so the data is not normally distributed.

#summary of data
summary(data_val)

#Histogram of data
hist(data_val, main="employment_female", 
     xlab="employment_female", 
     border="black", 
     col="lightblue" )

# Removing the right skewness by subtracting from minimum value
data_val = data_val - 8.527 + 1

#doing square root transformation
data_val = sqrt(data_val)

#Checking again the Shapiro test if the tranformation gives value > 0.05
shapiro.test(data_val)

#Doing Normal distributed Histogram
hist(data_val, main="Employment_rate_male", 
     xlab="Employment_rate_male", 
     border="black", 
     col="lightblue" )

#Assigning the data back
gdp_byindustry$Employment_female = data_val

# 18. exports

data_val = gdp_byindustry$Exports

#detecting the outliers present using boxplot
boxplot(data_val,ylab= "Values")

# Doing Normal distribution by Shapiro test 
shapiro.test(data_val)

# Since value is > 0.05 so data is normalized and we
# are considering this variable for analysis.

#summary of data
summary(data_val)

#Histogram of data
hist(data_val, main="exports", 
     xlab="exports", 
     border="black", 
     col="lightblue" )

# 19. Imports

data_val = gdp_byindustry$imports

#detecting the outliers present using boxplot
boxplot(data_val,ylab= "Values")

#Since outlier is present we need to get the value of it
outlier = boxplot.stats((data_val))$out

#Replacing the outlier with minimum value 
data_val[data_val %in% outlier] = min(data_val)

# Doing Normal distribution by Shapiro test 
shapiro.test(data_val)

# Since value is > 0.05 so data is normalized and we
# are considering this variable for analysis.

#summary of data
summary(data_val)

#Histogram of data
hist(data_val, main="imports", 
     xlab="imports", 
     border="black", 
     col="lightblue" )

gdp_byindustry$imports = data_val


# 20. Inflation

data_val = gdp_byindustry$Inflation

#detecting the outliers present using boxplot
boxplot(data_val,ylab= "Values")

# Doing Normal distribution by Shapiro test 
shapiro.test(data_val)

# Since value is > 0.05 so data is normalized and we
# are considering this variable for analysis.

#summary of data
summary(data_val)

#Histogram of data
hist(data_val, main="inflation", 
     xlab="inflation", 
     border="black", 
     col="lightblue" )

# 21. Unemployment rate of male

data_val = gdp_byindustry$`Unemployment male`

#detecting the outliers present using boxplot
boxplot(data_val,ylab= "Values")

#Since outlier is present we need to get the value of it
outlier = boxplot.stats((data_val))$out

#Replacing the outlier with minimum value 
data_val[data_val %in% outlier] = min(data_val)  


# Doing Normal distribution by Shapiro test 
shapiro.test(data_val)

# Since value is > 0.05 so data is normalized and we
# are considering this variable for analysis.

#summary of data
summary(data_val)

#Histogram of data
hist(data_val, main="Unemployment male", 
     xlab="Unemployment male", 
     border="black", 
     col="lightblue" )

gdp_byindustry$`Unemployment male` = data_val

#Calculating the correlation between the variables
# Skipping the first two columns as it is year and gdp
corelation_matrix <- cor(data_merged[,-c(1,2)]) 

# Intensity of correlation 
qplot(x = Var1, y = Var2,
      data = melt(corelation_matrix, use = "p"),
      fill = value,
      geom = "tile") +
  scale_fill_gradient2(limits = c(-1, 1))+
  xlab("Variable")+
  ylab("Variable")

#Getting index from correlation matrix to identify variables with high correlation
index <- findCorrelation(corelation_matrix,.90)

#Getting the vraible names with high corelation 
to_be_removed <- colnames(corelation_matrix)[index]

#removing the columns which are highly correlated
data_merged <- data_merged[!names(data_merged) %in% to_be_removed]

# after removing the variable get the corelating matrix
corelation_matrix <- cor(data_merged[,-c(1,2)])

# Intensity of correlation after removing the variables
qplot(x = Var1, y = Var2,
      data = melt(corelation_matrix, use = "p"),
      fill = value,
      geom = "tile") +
  scale_fill_gradient2(limits = c(-1, 1))+
  xlab("Variable")+
  ylab("Variable")


# preparing a dataframe with values from 2001-2016
train_data <-  data.frame("GDP" = data_merged$gdp,
                          "mining" = data_merged$Mining_oil_gas_extraction,
                          "Manufacture" = data_merged$Manufacturing,
                          "arts" = data_merged$Arts_entertainment_recreation,
                          "accommodation" = data_merged$Accommodation_food_services,
                          "Emp_male" = data_merged$Employment_rate_male,
                          "Exports" = data_merged$Exports,
                          "Imports" = data_merged$imports,
                          "Inflation" = data_merged$Inflation,
                          "Unemp_male" = data_merged$`Unemployment male`)

# manually giving the variable values for year 2017-2018 for prediction 
mining <- c(118.331,124.485)
Manufacture <- c(107.01,109.702)
arts <- c(108.254,110.479)
accommodation <- c(119.845,122.657)
Emp_male <- c(29.5170002,29.41300011)
Exports <- c(31.46312002,32.13092076)
Imports <- c(33.67951203,34.09305044)
Inflation <- c(1.596884129,2.268225672)
Unemp_male <- c(6.78000021,6.135000229)

# dataframe with values for 2017 and 2018
test_data <- data.frame(mining,Manufacture,arts,accommodation,Emp_male,Exports,Imports,Inflation,Unemp_male)

# Simple Linear Regression model 
linear_model <- lm(GDP ~ mining+Manufacture+arts+accommodation+Emp_male+Exports+Imports+Inflation+Unemp_male, data = train_data)

# asking the model to predict GDP for year 2017 and 2018
predict(linear_model, test_data)

# checking how the model would have predict GDP for year 2001-2016
predict(linear_model, train_data)

# Coefficient of Independent Variables
coef(linear_model)

# Model Summary
summary(linear_model)
