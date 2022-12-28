
## Scenario
## You are a data analyst working for Turtle Games, a game manufacturer and 
## retailer. They manufacture and sell their own products, along with sourcing
## and selling products manufactured by other companies. Their product range 
## includes books, board games, video games and toys. They have a global 
## customer base and have a business objective of improving overall sales 
##performance by utilising customer trends. 

## In particular, Turtle Games wants to understand:
## - how customers accumulate loyalty points 
## - how useful are remuneration and spending scores data 
## - can social data (e.g. customer reviews) be used in marketing 
##     campaigns 
## - what is the impact on sales per product 
## - the reliability of the data (e.g. normal distribution, Skewness, Kurtosis)
## - if there is any possible relationship(s) in sales between North America,
##     Europe, and global sales .

################################################################################


###############################################################################

# 1. Load and explore the data

# Install and import Tidyverse.
library(ggplot2)
library(tidyverse)
library(bbplot)
library(DataExplorer)


# Import the data set.
tsales <- read.csv(choose.files(), header=TRUE)

# Print the data frame.
View(tsales)
str(tsales)
summary(tsales)
DataExplorer::create_report(tsales)

# Check for NA 
is.na(tsales)
apply(is.na(tsales), 2, which)
#(Column Year which contains the NA will be removed in following step)

#Check for duplicates 


#Found 0 duplicates


#Check Values per Column

table(tsales$Genre)
table(tsales$Platform)
table(tsales$Publisher)
table(tsales$Product)

#Count (Filter all products with more than 2 sales )
tsales %>% count(Product, name = "CountByProd", sort=TRUE) %>% 
  filter (CountByProd>2)

# Top 10 Platforms.
tsalesp <- tsales %>% group_by(Platform) %>%
  summarise(NA_Sales_sum=sum(NA_Sales),
            EU_Sales_sum=sum(EU_Sales),
            Global_Sales_sum=sum(Global_Sales),
            .groups='drop')%>%
            arrange(desc(Global_Sales_sum)) %>%
            top_n(10)

#Plot DF
tsalesp
ggplot(data=tsalesp, aes(x=reorder(Platform,Global_Sales_sum), 
                         y=Global_Sales_sum,fill=Platform))+
      geom_bar(stat = "identity")+
      labs(title= "Top 10 Platforms bss Global Sales figures",
      x="Platforms",
      y="Global Sales (in mil GBP)")


# Top 10 Genres.
tsalesg <- tsales %>% group_by(Genre) %>%
  summarise(NA_Sales_sum=sum(NA_Sales),
            EU_Sales_sum=sum(EU_Sales),
            Global_Sales_sum=sum(Global_Sales),
            .groups='drop') %>%
            arrange(desc(Global_Sales_sum))%>%
            top_n(10)          

# Plot DF
tsalesg
ggplot(data=tsalesg, aes(x=reorder(Genre,Global_Sales_sum), 
                         y=Global_Sales_sum,fill=Genre))+
  geom_bar(stat = "identity")+
  labs(title= "Top 10 Game Genres bss Global Sales figures",
       x="Genres",
       y="Global Sales (in mil GBP)")


# Top 10 Publishers.
tsalespub <- tsales %>% group_by(Publisher) %>%
  summarise(NA_Sales_sum=sum(NA_Sales),
            EU_Sales_sum=sum(EU_Sales),
            Global_Sales_sum=sum(Global_Sales),
            .groups='drop') %>%
  arrange(desc(Global_Sales_sum))%>%
  top_n(10)          

# Plot DF
tsalespub
ggplot(data=tsalespub, aes(x=reorder(Publisher,Global_Sales_sum), 
                         y=Global_Sales_sum,fill=Publisher))+
  geom_bar(stat = "identity")+
  labs(title= "Top 10 Publisher bss Global Sales figures",
       x="Publisher",
       y="Global Sales (in mil GBP)")+
      scale_x_discrete(guide = guide_axis(n.dodge = 2))

# Top 10 Products.
tsalesprod <- tsales %>% group_by(Product) %>%
  summarise(NA_Sales_sum=sum(NA_Sales),
            EU_Sales_sum=sum(EU_Sales),
            Global_Sales_sum=sum(Global_Sales),
            .groups='drop') %>%
  arrange(desc(Global_Sales_sum))%>%
  top_n(10)

# Create a new data frame from a subset of the sales data frame.
# Remove unnecessary columns. 
tsales2 <- select(tsales, -Ranking, -Year, -Genre, -Publisher)
head(tsales2)

# View the data frame.
View(tsales2)

# View the descriptive statistics.
summary(tsales2)

################################################################################

# 2. Review plots to determine insights into the data set.

## 2a) Scatterplots
# Create scatterplots.
qplot(Product, NA_Sales, colour=I('red'), data=tsales2, geom=c('point', 'jitter'))+
        labs(title= "Relationship between product and NA sales",
        subtitle="Categorical display where each plot corresponds to one product ID",
        x="Products",
        y="NA Sales", bbc_style())


#Qplot for EU
qplot(Product, EU_Sales, colour=I('green'), data=tsales2, geom=c('point', 'jitter'))+
        labs(title= "Relationship between product and EU sales",
        subtitle="Categorical display where each plot corresponds to one product ID",
        x="Products",
        y="NA Sales", bbc_style())


#Qplot for Global
qplot(Product, Global_Sales, colour=I('blue'), data=tsales2, geom=c('point', 'jitter'))+
        labs(title= "Relationship between product and Global Sales",
        subtitle="Categorical display where each plot corresponds to one product ID",
        x="Products",
        y="Global Sales") 
#An quick observation is that products id follow similar pattern of sales
#(ie corresponding always to sales figures) across both EU and NA sales as well
# as seen in Global Sales combined

## 2b) Histograms
# Create histograms.
#The histogram and density plots are used to display the distribution of data.


qplot(NA_Sales, bins=25, data=tsales2, color = I("blue"), main='Histogram NA Sales')
qplot(EU_Sales, bins=25, data=tsales2, color = I("blue"), main='Histogram EU Sales')
qplot(Global_Sales, bins=25, data=tsales2, color = I("blue"), main='Histogram Global Sales')

#Right (or Positively) skewed distribution detected for all 3 histograms.
#Suggestion for data transformation tools may be employed to make the skewed data closer to a normal distribution

qplot(Product, bins=25, data=tsales2, color = I("blue"), main='Histogram Global Sales')

## 2c) Boxplots
# Create boxplots.
qplot(NA_Sales, colour=I('red'), data=tsales2,
      geom='boxplot')+
  labs(title= "Boxplot of NA sales ", bbc_style())

qplot(EU_Sales, colour=I('green'), data=tsales2,
      geom='boxplot')+
  labs(title= "Boxplot of EU Sales", bbc_style())

qplot(Global_Sales, colour=I('blue'), data=tsales2,
      geom='boxplot')+
  labs(title= "Boxplot of  Global Sales ", bbc_style())

#As was expected from right skewed histogram boxerplot find extreme values far 
#from the peak on the high end more frequently than on the low.
#Also practically expected to have right skewed distribution as sales data can
#never be less than zero but they can have unusually large values

###############################################################################
######START OF DATA WRANGLING FOR REVIEWS DF TO GET DEMO INFO######
#Check data on reviews
treviews <- read.csv(choose.files(), header=TRUE)
DataExplorer::create_report(treviews)

#Shape DF
treviews <- select(treviews, -language, -platform, -product, -review, -summary)
#Rename columns
colnames(treviews)<- c("gender","age","remuneration", 
                       "spending_score", "loyalty_points", "education")

# View new data frame.
head(treviews)

# Determine the unique values in each column.
table(treviews$gender)
table(treviews$education)

summary(treviews)

# Compare gender and education.
ggplot(treviews, aes(x=gender, fill=education)) + 
  geom_bar(position='dodge')

# Compare age, remuneration and gender.
ggplot(treviews, aes(x=age, y=remuneration, col=gender)) + 
  geom_smooth(se=FALSE)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 20))+
  geom_vline(xintercept=34, linetype = "longdash", color="red")+
  geom_vline(xintercept=40, linetype = "longdash", color="green")

# Compare gender, education and remuneration.
qplot(gender, education, colour=remuneration, 
      data=treviews, geom=c('point', 'jitter'))

# Compare gender, education and spending score.
qplot(gender, education, colour=spending_score, 
      data=treviews, geom=c('point', 'jitter'))

# Compare gender, education and loyalty points.
qplot(gender, education, colour=loyalty_points, 
      data=treviews, geom=c('point', 'jitter'))

# Compare gender, age and remuneration.
qplot(gender, age, colour=remuneration, 
      data=treviews, geom=c('point', 'jitter'))

# Compare gender, age and spending score.
qplot(gender, age, colour=spending_score, 
      data=treviews, geom=c('point', 'jitter'))

# Compare gender, age and loyalty points.
qplot(gender, age, colour=loyalty_points, 
      data=treviews, geom=c('point', 'jitter'))


######END OF DATA WRANGLING FOR REVIEWS DF TO GET DEMO INFO######


################################################################################

# 1. Load and explore the data
View (tsales2)
dim(tsales2)

# Check output: Determine the min, max, and mean values.
min(tsales2$NA_Sales) 
max(tsales2$NA_Sales) 
mean(tsales2$NA_Sales) 

min(tsales2$EU_Sales) 
max(tsales2$EU_Sales)
mean(tsales2$EU_Sales)

min(tsales2$Global_Sales) 
mean(tsales2$Global_Sales) 


# View the descriptive statistics.
summary(tsales2)


###############################################################################

# 2. Determine the impact on sales per product_id.

## 2a) Use the group_by and aggregate functions.
# Group data based on Product and determine the sum per Product.

tsales2_pro <- tsales2 %>% group_by(Product) %>% 
summarise(across(.cols = c(NA_Sales, EU_Sales, Global_Sales), list(sum = sum)))

# View the data frame.
View (tsales2_pro)

# Explore the data frame.
## 2b) Determine which plot is the best to compare game sales.
# Create scatterplots.
qplot(Global_Sales_sum, EU_Sales_sum, data=tsales2_pro,
      main='Scatterplot global sales vs EU sales grouped by product')
#the scatterplot shows a strong positive linear correlation between EU Sales and Global Sales

qplot(Global_Sales_sum, NA_Sales_sum, data=tsales2_pro,
      main='Scatterplot global sales vs NA sales grouped by product')
#NA sales show to be tightly also positive correlated with Global Sales

qplot(EU_Sales_sum, NA_Sales_sum, data=tsales2_pro,
      main='Scatterplot EU sales vs NA sales grouped by product')
#Positive strong correlation is found also between EU and NA sales

# Create histograms.
qplot(EU_Sales_sum, bins=25, color = I("blue"), data=tsales2_pro, 
      main='Histogram global sales grouped by product')
qplot(NA_Sales_sum, bins=25, color = I("blue"), data=tsales2_pro,
      main='Histogram global sales grouped by product')
qplot(Global_Sales_sum, bins=25, color = I("blue"), data=tsales2_pro, 
      main='Histogram global sales grouped by product')

##As with previous plotting of Histograms we still note the same right skew for all three histograms

# Create boxplots.
qplot(NA_Sales_sum, data = tsales2_pro, colour = I("green"), geom = "boxplot")
qplot(EU_Sales_sum, data = tsales2_pro, colour = I("green"), geom = "boxplot")
qplot(Global_Sales_sum, data = tsales2_pro, colour = I("green"), geom = "boxplot")

## Same with histograms there is always positive outliers as we are examining sales data
# and also upper quartile shows difference than rest(ie 75% fall below the upper quartile.).
#Also all boxes are comparatively shors meaning that data seems to be in agreement
###############################################################################


# 3. Determine the normality of the data set.

## 3a) Create Q-Q Plots
# Create Q-Q Plots.
qqnorm(tsales2_pro$EU_Sales_sum)
# Add a reference line:
qqline(tsales2_pro$EU_Sales_sum, col='blue')

qqnorm(tsales2_pro$NA_Sales_sum)
# Add a reference line:
qqline(tsales2_pro$NA_Sales_sum, col='blue')

qqnorm(tsales2_pro$Global_Sales_sum)
# Add a reference line:
qqline(tsales2_pro$Global_Sales_sum, col='blue')

#reference line along with shape of qqplot for Global Sales show that our values 
#do not follow a normal distribution (which is also is what positive skewed results confirmed)

## 3b) Perform Shapiro-Wilk test
# Install and import Moments.
library(moments)

# Perform Shapiro-Wilk test.
shapiro.test((tsales2_pro$EU_Sales_sum))
shapiro.test((tsales2_pro$NA_Sales_sum))
shapiro.test((tsales2_pro$Global_Sales_sum))

##All Shapiro-Wilk test show p-values below 0.05
#which is another confirmation that we can reject such a null hypothesis 
#and say that the sample has not been generated from a normal distribution

## 3c) Determine Skewness and Kurtosis
# Skewness and Kurtosis.
skewness(tsales2_pro$EU_Sales_sum)
skewness(tsales2_pro$NA_Sales_sum)
skewness(tsales2_pro$Global_Sales_sum)

#As observer above results show a handful of high observations 
#which raise the average above the median value. 

kurtosis(tsales2_pro$Global_Sales_sum)
kurtosis(tsales2_pro$EU_Sales_sum)
kurtosis(tsales2_pro$NA_Sales_sum)

#All kurtosis results show a heavy tailed distribution
#meaning that our results have a big variance from normal distribution
#Evidence that data is heavy tailed and right skewed

## 3d) Determine correlation
# Determine correlation.

round(cor(tsales2_pro), digits=2)
#We can see from the results that there is strong positive correlation 
#between NA and Global Sales as well as between EU and Global Sales.
#A slightly less stronger but still significant positive correlation between
#EU and NA Sales

###############################################################################

# 4. Plot the data
# Create plots to gain insights into data.
# Choose the type of plot you think best suits the data set and what you want 
# to investigate. Explain your answer in your report.

# Top 10 Products.
tsalespro <- tsales2_pro %>% group_by(Product) %>%
  summarise(NA_Sales_sum=sum(NA_Sales_sum),
            EU_Sales_sum=sum(EU_Sales_sum),
            Global_Sales_sum=sum(Global_Sales_sum))%>%
  arrange(desc(Global_Sales_sum))%>%
  top_n(10)

#Reformat data to long format, one column for meauser and another for key variable
#telling us which measure to use in each row
tsalesprolong <- gather(tsalespro, key="measure", 
value="value", c("Global_Sales_sum", "NA_Sales_sum","EU_Sales_sum"))

tsalesprolong

ggplot(data=tsalesprolong, aes(x=reorder(Product, value),y=value, fill=Product))+
  geom_bar(stat='identity')+
  facet_wrap(~measure)+
  labs(title= "Top 10 Product bss EU, Global & NA Sales sum figures",
       x="Product IDs",
       y="Sales (in mil GBP)")+
      scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
        geom_text(aes(label=value), vjust=1.5, colour="white", size=2.5) 


###############################################################################

 # 5. Create plots to gain insights into the sales data.
##  - Compare all the sales data (columns) for any correlation(s).
##  - Add a trend line to the plots for ease of interpretation.
# Scatterplot global sales vs NA sales.

ggplot(data=tsales2_pro,mapping=aes(x=Global_Sales_sum, y=NA_Sales_sum)) +
  geom_point(color='black',
             alpha=0.75,
             size=2.5) +
  geom_smooth(method='lm', color='orange') +
  scale_x_continuous("Global sales") +
  scale_y_continuous("NA sales") +
  labs(title="Turtle Games global sales vs NA sales (Million GBP)")


# Scatterplot global sales vs Europe sales.

ggplot(data=tsales2_pro,mapping=aes(x=Global_Sales_sum, y=EU_Sales_sum)) +
  geom_point(color='black',
             alpha=0.75,
             size=2.5) +
  geom_smooth(method='lm', color='orange') +
  scale_x_continuous("Global sales") +
  scale_y_continuous("EU sales") +
  labs(title="Turtle Games global sales vs EU sales(Million GBP)")

# Scatterplot global sales vs Europe sales.

ggplot(data=tsales2_pro,mapping=aes(x=EU_Sales_sum, y=NA_Sales_sum)) +
  geom_point(color='black',
             alpha=0.75,
             size=2.5) +
  geom_smooth(method='lm', color='orange') +
  scale_x_continuous("EU sales") +
  scale_y_continuous("NA sales") +
  labs(title="Turtle Games EU sales vs NA sales(Million GBP)")

#scatterplots confirm once more the correlation table
#produced by running the corr command ie strong correlation beween 
#Global and NA /Global and EU. Slightly weaker correlated between 
#EU & NA_Sales

###############################################################################
###############################################################################
# 1. Load and explor the data
View(tsales2_pro)

# Determine a summary of the data frame.
summary(tsales2_pro)

# 2. Create a simple linear regression model
## 2a) Determine the correlation between columns
cor(tsales2_pro)
# Create a linear regression model on the original data.
model1 <- lm(NA_Sales_sum ~ Global_Sales_sum, data=tsales2_pro)
model2 <- lm(EU_Sales_sum ~ Global_Sales_sum, data=tsales2_pro)
model3 <- lm(EU_Sales_sum ~ NA_Sales_sum, data=tsales2_pro)

model1
summary(model1)
model2
summary(model2)
model3
summary(model3)

## 2b) Create a plot (simple linear regression)
# Basic visualisation.
plot(tsales2_pro$NA_Sales_sum, tsales2_pro$Global_Sales_sum)
abline(coefficients(model1))

plot(tsales2_pro$EU_Sales_sum, tsales2_pro$Global_Sales_sum)
abline(coefficients(model2))

plot(tsales2_pro$EU_Sales_sum, tsales2_pro$NA_Sales_sum)
abline(coefficients(model3))

###############################################################################

# 3. Create a multiple linear regression model
# Select only numeric columns.

names(tsales2_pro)
tsales3 <- subset(tsales2_pro, select=-c(Product))

str(tsales3)
summary(tsales3)

# Determine the correlation between the sales columns.
round(cor(tsales3), digits = 2)


# Multiple linear regression model.
modelA = lm(Global_Sales_sum~NA_Sales_sum+EU_Sales_sum, data=tsales3)
summary(modelA)

###############################################################################

# 4. Predictions based on given values
# Compare with observed values for a number of records.
View(tsales2_pro)

# A. NA_Sales_sum of 34.02 and EU_Sales_sum of 23.80.
NA_Sales_sum <- c(34.02)
EU_Sales_sum <- c(23.80)

tsales4 <- data.frame(NA_Sales_sum, EU_Sales_sum)

# Predicted Global_Sales value.
predict(modelA, newdata = tsales4)
# Predicted value 68.056 vs observation value 67.85: good .

# B. NA_Sales_sum of 3.93 and EU_Sales_sum of 1.56.
# Proceed with 3.94/1.28 with observed Global_sales value 8.36 as found in DF.
NA_Sales_sum <- c(3.94)
EU_Sales_sum <- c(1.28)

tsales5 <- data.frame(NA_Sales_sum, EU_Sales_sum)

# Predicted Global_Sales value.
predict(modelA, newdata = tsales5)
# Predicted value 7.03 vs observation value 8.36: average.

# C. NA_Sales_sum of 2.73 and EU_Sales_sum of 0.65, observed value 4.32.
NA_Sales_sum <- c(2.73)
EU_Sales_sum <- c(0.65)

tsales6 <- data.frame(NA_Sales_sum, EU_Sales_sum)

# Predicted Global_Sales value.
predict(modelA, newdata = tsales6)
# Predicted value 4.90 vs observation value 4.32: good.

# D. NA_Sales_sum of 2.26 and EU_Sales_sum of 0.97.
# Proceed with 2.27/2.30 with observed Global_sales value 5.60 as found in DF.
NA_Sales_sum <- c(2.27)
EU_Sales_sum <- c(2.30)

tsales7 <- data.frame(NA_Sales_sum, EU_Sales_sum)

# Predicted Global_Sales value.
predict(modelA, newdata = tsales7)
# Predicted value 6.36 vs observation value 5.60: average.

# E. NA_Sales_sum of 22.08 and EU_Sales_sum of 0.52, Global sales 23.21.
NA_Sales_sum <- c(22.08)
EU_Sales_sum <- c(0.52)

tsales8 <- data.frame(NA_Sales_sum, EU_Sales_sum)

# Predicted Global_Sales value.
predict(modelA, newdata = tsales8)
# Predicted value 26.62 vs observation value 23.21: average.