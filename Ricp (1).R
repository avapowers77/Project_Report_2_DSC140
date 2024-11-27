library(ggplot2)
library(readr)
library(corrplot)
library(caret)
library(class)

## BUSINESS DATA SET ## 
#Problem 1
library(readr)
business <- read_csv("RCodes/OA 11.7 - yelp_academic_dataset_business.json.csv")
print(business)

(business[1:5])

#Problem 2
ggplot(business) + geom_bar(aes(x=state), fill= "pink") + labs(x="State", y="Number of Businesses", title = "# of Businesses per State")

#Problem 3
contingency_table <- table(business$stars)
print(contingency_table)
pie(contingency_table, main="Pie Chart of Stars")

#Problem 4 
ggplot(business)+geom_boxplot(aes(y=stars))
ggplot(business, aes(x=review_count, y=stars, group=stars))+geom_boxplot(show.legend=FALSE)

#Problem 5
one_five <- subset(business, stars ==1 | stars==5)
View(one_five)

busi_cont <- table(one_five$stars)
print(busi_cont)
chisq.test(busi_cont)


## USER DATA ## 
#Problem 1
library(readr)
user <- read_csv("RCodes/OA 11.6 - yelp_academic_dataset_user.json.csv")
View(user)

colnames(user)

#Problem 2
numeric_only <- user[,sapply(user, is.numeric)]
#goes through y data frame and deletes non-numeric data columns

correlation_matrix <- cor(numeric_only)
View(correlation_matrix)

#Problem 3
linear_model <- lm(user$cool_votes ~ user$funny_votes) 
print(linear_model)

coefs <- coef(linear_model) 
print(coefs)
ggplot(user) + geom_point(aes(x=cool_votes, y=funny_votes)) +
  geom_smooth(aes(x=cool_votes, y=funny_votes), method = "lm", se=F) 

# Problem 4
linear_model <- lm(user$review_count ~ user$fans) 
print(linear_model)

ggplot(user) + geom_point(aes(x=review_count, y=fans)) +
  geom_smooth(aes(x=review_count, y=fans), method = "lm", se=F) 
#Not necessarily#

#Problem 5
linear_model <- lm(user$fans ~ user$average_stars) 
print(linear_model)

ggplot(user) + geom_point(aes(x=average_stars, y=fans)) +
  geom_smooth(aes(x=average_stars, y=fans), method = "lm", se=F) 

#Problem 6
user_cluster <- kmeans(user[3:4], 3)
print(table(user_cluster$cluster, user$fans))

user$cluster <- user_cluster$cluster
View(user)

ggplot(user) + geom_point(aes(x=review_count, y=average_stars))

user_x <- user[3:4]
wcss <- function(k){
  kmeans(user_x, centers=k)$tot.withinss
}

k_values <- 3:4

wcss_values <- sapply(k_values, wcss)

elbow_plot <- data.frame(k = k_values, wcss = wcss_values)
View(elbow_plot)

ggplot(elbow_plot, aes(x=k_values, y=wcss)) + geom_line() + geom_point()






