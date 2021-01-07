# Install the package
install.packages("lubridate")
# Load the packages
library(lubridate)
library(dplyr)

# Read data
data<-read.csv("Walmart_Store_sales.csv")
  data$Date <- dmy(data$Date)
str(data)


# Calculating store with maximum sales
t1 <- aggregate(data$Weekly_Sales, by=list(Category=data$Store), FUN=sum)
t1
t1[which.max(t1$x),]
barplot(t1$x, t1$Category)
# Store with max Std deviation and its coefficient of variation
d <- data %>%
  group_by(Store) %>%
  summarise(mean_sale = mean(Weekly_Sales),std_dev = sd(Weekly_Sales)) %>%
  mutate(coeff_of_variation = std_dev*100/mean_sale)
d
d[which.max(d$std_dev),]


data %>%
  group_by(Store) %>%
  summary()

# Stores with top 6 Q-o-Q Growth for q3 2012
q3 <-data %>%
filter(Date >= "2012-04-01" & Date < "2012-09-01")

q3_growth <- q3 %>%
  group_by(Store) %>%
  mutate(qoq = Weekly_Sales - lag(Weekly_Sales)) %>%
  summarise(qvar = var(qoq, na.rm = T))
View(q3_growth)
q3_res <- q3_growth[order(q3_growth$qvar),]
tail(q3_res)
