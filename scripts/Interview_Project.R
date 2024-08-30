# Prep -------------------------------------------------------------

rm(list = ls()) # Cleaning Rstudio

pacman::p_load(ggplot2, rio, tidyverse, skimr, caret, 
               rvest, magrittr, rstudioapi, stargazer, 
               boot, readxl, knitr, kableExtra,
               glmnet, sf, tmaptools, leaflet,
               tokenizers, stopwords, SnowballC,
               stringi, dplyr, stringr, sp, hunspell,
               car, mice, glmnet, data.table, mice, openxlsx, pROC) # Packages

#Setting the directory
path_script<-rstudioapi::getActiveDocumentContext()$path
path_folder<-dirname(path_script) 
setwd(path_folder)
getwd()


# Creating Dataset --------------------------------------------------------

set.seed(1111)

n_rows<-15000
n_customers<-8000
n_columns<-4
categories<-c("Standard", "Premier", "Suite", "Specialty Suite", "Presidential")
customer_ids<-1:n_customers

db <- data.frame(
  CustomerID = sample(customer_ids, n_rows, replace = TRUE),
  TransactionDate = sample(seq(as.Date('2022-01-01'), as.Date('2024-07-31'), by="day"), n_rows, replace = TRUE), #normally R formats dates YYYY-MM-DD
  Amount = round(rnorm(n_rows, mean=360, sd=75), 2) #normal distribution
)

high_p<- c(0.05,0.1,0.45,0.35,0.05)
low_p<-c(0.35,0.45,0.15,0.5,0)

assignment<- function(amount) {
  if (amount > 400) {
    sample(categories, 1, prob = high_p)
  } else {
    sample(categories, 1, prob = low_p)
  }
}

db$ProductCategory<-sapply(db$Amount, assignment)

head(db)

#Issues

set.seed(1234)
rate<-0.04

missing_amount<- sample(1:n_rows, size=round(n_rows*rate))
db$Amount[missing_amount] <- NA

missing_productc<-sample(1:n_rows,size=round(n_rows*rate))
db$ProductCategory[missing_productc]<-NA

date_error<-sample(1:n_rows,size=round(n_rows*rate))
db$TransactionDate[date_error] <- format(db$TransactionDate[date_error], "%d-%m-%Y")

dup_rate<-0.01
duplicate_index<-sample(1:n_rows,size=round(n_rows*dup_rate))
duplicates<-db[duplicate_index,]
db<-rbind(db,duplicates)

##to mantain the previous amount of rows
num_del<-nrow(duplicates)
delete_indices <- sample(1:nrow(db), size = num_del)
db <- db[-delete_indices, ]
head(db)

#Sava data as csv
write.csv(db,file="../stores/transaction_data.csv")

# Data Cleaning and Preparation -------------------------------------------------------------
db<- read.csv("../stores/transaction_data.csv")

#Examining the data
summary(db) #We see here that there are 602 values missing in Amount. Moreover, that the mean of Transaction Date and min look weird. Finally, that product category is character but might be categorical
skim(db) #Here we see that there are 599 missing in ProductCategory, with 5 different categories
table(db$ProductCategory) #we check and it's a categorical variable

#We check in more detail
ggplot(data = db, mapping = aes(x = Amount)) + 
  geom_histogram(fill = "goldenrod1", color = "goldenrod3", binwidth = 10) + 
  labs(title = "Distribution of Amount Spent by Transaction", y = "Count", x = "Amount") +
  theme_minimal()

ggsave("../views/Distribution_Amount.png", plot = last_plot(), width = 8, height = 6, dpi = 300)

ggplot(data = db, aes(x = reorder(ProductCategory, ProductCategory, function(x) -length(x)))) + 
  geom_bar(fill = "goldenrod1", color = "goldenrod3") + 
  labs(title = "Distribution of Product Categories", y = "Count", x = "Type of Room") +
  theme_minimal()

ggsave("../views/Count_Room_Type.png", plot = last_plot(), width = 8, height = 6, dpi = 300)

categories<-c("Standard", "Premier", "Suite", "Specialty Suite", "Presidential")

db$ProductCategory <- factor(db$ProductCategory, levels = categories) #so it will display in desired order
ggplot(data = db, aes(x = ProductCategory, y = Amount)) + 
  geom_boxplot(fill = "goldenrod1", color = "goldenrod4") + 
  labs(title = "Distribution of Amount Spent by Product Category", y = "Amount", x = "Product Category") +
  theme_minimal()
ggsave("../views/Amount_by_Product.png", plot = last_plot(), width = 8, height = 6, dpi = 300)

db$TransactionDate <- as.Date(db$TransactionDate)
db$Month_Yr <- format(as.Date(db$TransactionDate), "%Y-%m")
db$Yr<-format(as.Date(db$TransactionDate),"%Y")
setDT(db)
Month_Yr_counts <- db[, .N, by = Month_Yr]
Yr_counts<-db[,.N,by=Yr]

ggplot(Month_Yr_counts, aes(x = Month_Yr, y = N)) +
  geom_bar(stat = 'identity', fill = 'blue') +
  labs(x = 'Month-Year', y = 'Count') #here it's more clearly seen that there are issues with the dates

ggplot(Yr_counts, aes(x = Yr, y = N)) +
  geom_bar(stat = 'identity', fill = 'blue') +
  labs(x = 'Month-Year', y = 'Count') #here it's more clearly seen that there are issues with the dates

##Dates
suspected_errors <- as.numeric(db$Yr) < 1900
db$Month <- format(as.Date(db$TransactionDate), "%m")
db$Day <- format(as.Date(db$TransactionDate), "%d")

correct_year <- paste0("20", db$Day)  
correct_month<-paste(db$Month)
correct_day<- paste0(db$Yr)
correct_day<-substring(correct_day, nchar(correct_day)-1)
corrected_date <- paste0(correct_year, "-", correct_month, "-", correct_day)
corrected_date<-as.Date(corrected_date,format="%Y-%m-%d")

db$TransactionDate[suspected_errors] <- corrected_date[suspected_errors]

db$Month_Yr <- format(as.Date(db$TransactionDate), "%Y-%m")
Month_Yr_counts <- db[, .N, by = Month_Yr]
skim(db$TransactionDate)

ggplot(Month_Yr_counts, aes(x = Month_Yr, y = N)) +
  geom_bar(stat = 'identity', fill = 'goldenrod1', color = 'goldenrod3') +
  labs(x = 'Month-Year', y = 'Count', title = 'Distribution of Counts Over Time') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  #we check that the data makes more sense and now we see not many outliers, although still some values are very small
ggsave("../views/Count_Dates.png", plot = last_plot(), width = 8, height = 6, dpi = 300)


##Missing values- Amount (602) & Product Category (599)
#In case of the categorical variable, we could either maintain the NAs, as they aren't that many and consiering its a categorical value, or otherwise replace by the mode.

mice_db <- mice(db, m = 5, method = "pmm", seed = 1234) #we could also try different imputation methods such as cart
db <- complete(mice_db) 

skim(db) #we double check and see that there are no missing errors. we would also need to double check that the data has not varied significantly

#Duplicates
dup_ind<-duplicated(db[,c("CustomerID","Amount","TransactionDate")]) #this command will keep the first occurrence
db<-db[!dup_ind,] #we remove those duplicates

#Sava data as csv
write.csv(db,file="../stores/clean_data.csv")
#Note: depending on the type of data, we could have also evaluated outliers, in order to decide if we should remove or keep them.

# Exploratory Data Analysis (EDA) -----------------------------------------
db<- read.csv("../stores/clean_data.csv")

categories<-c("Standard", "Premier", "Suite", "Specialty Suite", "Presidential")


skim(db) 
db$TransactionDate <- as.Date(db$TransactionDate, format = "%Y-%m-%d")
db$Month_Yr <- format(db$TransactionDate, "%Y-%m")
db$Month<-format(db$TransactionDate,"%m")
db$Yr<-format(db$TransactionDate,"%Y")

db$ProductCategory <- factor(db$ProductCategory)

#Summary statistics for Amount
sum_stats<-skim(db[,c("TransactionDate","ProductCategory","Amount")])
stats_tbl<-as.data.frame(sum_stats)
write.xlsx(stats_tbl, file = "../document/stats_tbls.xlsx")

db$ProductCategory <- factor(db$ProductCategory, levels = categories)
two_way_table <- table(cut(db$Amount, 
                           breaks = c(0, 100, 200, 300, 400, 500,600,700), 
                           labels = c("0-100", "100-200", "200-300", "300-400", "400-500","500-600","600-700")),
                       db$ProductCategory)
print(two_way_table)
write.xlsx(two_way_table, file = "../document/twoway_tbls.xlsx")

#Distribution Amount

ggplot(data = db, mapping = aes(x = Amount)) +
  geom_histogram(aes(y = ..density..), 
                 binwidth = 15,
                 fill = 'bisque', 
                 color = 'bisque4') +
  stat_function(fun = dnorm, 
                args = list(mean = mean(db$Amount), 
                            sd = sd(db$Amount)), 
                color = 'darkgoldenrod', 
                size = 1) +
  labs(title = "Histogram of Amount Spent with Normal Distribution",
       x = "Amount Spent",
       y = "Density") +
  theme_minimal()
ggsave("../views/Amount2.png", plot = last_plot(), width = 8, height = 6, dpi = 300)


#Trends over time
db$Month_Yr <- factor(db$Month_Yr, levels = sort(unique(db$Month_Yr)))
db$Month_Yr_graph <- as.Date(paste0(db$Month_Yr, "-01"), format = "%Y-%m-%d")


ggplot(data = db, mapping = aes(x = Month_Yr_graph, y = Amount)) +
  geom_point(color = 'darkblue') +
  geom_smooth(method = 'lm', se = FALSE, color = 'brown4') +  # Add trend line
  labs(title = "Scatter Plot of Amount by Month and Year",
       x = "Month-Year",
       y = "Amount") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "3 months")
ggsave("../views/Amount_time.png", plot = last_plot(), width = 8, height = 6, dpi = 300)


#When we plot the data like this, we come to see that, either something happened in 2021, or otherwise, the data before 2021 is incorrect
#since we "fixed" the dates by adding 20 to the dates, this was a strong assumption and it could be reflecting incorrect years
#the recommended thing would be to create an identifier on the dates that were corrected, and see if they all fall in this range
#if so, then we would need to "fix" the issue in a different manner, or ultimately delete these observations as it could be skewing the data

categories<-c("Standard", "Premier", "Suite", "Specialty Suite", "Presidential")

db$ProductCategory <- factor(db$ProductCategory, levels = categories) #so it will display in desired order
ggplot(data=db, aes(x=ProductCategory, y=Amount)) +  geom_boxplot(fill='goldenrod1')
ggsave("../views/Amount_category.png", plot = last_plot(), width = 8, height = 6, dpi = 300)

#Here we see that, even though we would expect higher categories of rooms to have a higher amount spent, this isn't necessarily the case.
#this could be due to many factors, such as the market segments that are booking the rooms.
#Moreover, we know from that data that the suite that is being sold the most is the specialty suite,
#which is at a lower point than the normal suite, could be that we are doing a better job at pricing this room, or that we are not selling
#many suites, and instead using them as an upgrade path, compared to specialty suites.

summary_customers <- db %>%
  group_by(CustomerID) %>%
  summarize(
    NumberOfTransactions = n(),
    TotalAmount = sum(Amount, na.rm = TRUE),
    MedianAmount = median(Amount, na.rm = TRUE)
  )

summary(summary_customers)  #we see that the number of transactions go from 1-9. 
                            #We will create groups based on this although and the amount spent

transaction_thresholds <- c(1, 2, 5, 8, Inf)
transaction_labels <- c("1-2", "3-5", "6-8", "9+")

amount_thresholds <- c(0, 999, 2499, 4999, Inf)

# Create summary table with categorized variables
summary_customers <- db %>%
  group_by(CustomerID) %>%
  summarize(
    NumberOfTransactions = n(),
    TotalAmount = sum(Amount, na.rm = TRUE),
    MedianAmount = median(Amount, na.rm = TRUE)
  ) %>%
  mutate(
    # Assign transaction categories with the extra label for a single transaction
    transaction_cat = cut(
      NumberOfTransactions,
      breaks = transaction_thresholds,
      labels = transaction_labels,
      include.lowest = TRUE
    ),
    # Assign amount categories
    amount_cat = cut(
      TotalAmount,
      breaks = amount_thresholds,
      labels = c("$0-$999", "$1000-$2499", "$2500-$4999", "$5000+"),
      include.lowest = TRUE
    ),
    # Use case_when to combine the two categories into a final category
    final_category = case_when(
      NumberOfTransactions >= 9 | TotalAmount >= 5000 ~ "Platinum",
      NumberOfTransactions >= 6 | TotalAmount >= 2500 ~ "Gold",
      NumberOfTransactions >= 2 | TotalAmount >= 1000 ~ "Silver",
      TRUE ~ "Bronze"  # Default to Bronze if none of the above conditions are met
    )
  )

# Check the final result
print(summary_customers)

#summary for categories
category_summary <- summary_customers %>%
  group_by(final_category) %>%
  summarize(
    mean_transactions = mean(NumberOfTransactions, na.rm = TRUE),
    mean_amount_spent = mean(TotalAmount, na.rm = TRUE)
  )
print(category_summary)

summary_customers <- summary_customers %>%
  mutate(final_category = factor(final_category, levels = c("Bronze", "Silver", "Gold", "Platinum")))

ggplot(summary_customers, aes(x = final_category, y = NumberOfTransactions, fill = final_category)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, outlier.color = "darkblue", outlier.shape = 16) +
  theme_minimal() +
  labs(title = "Distribution of Number of Transactions by Category", 
       x = "Category", 
       y = "Number of Transactions") +
  scale_fill_brewer(palette = "Set3") +
  theme(legend.position = "none")
ggsave("../views/Category_Transaction.png", plot = last_plot(), width = 8, height = 6, dpi = 300)


ggplot(summary_customers, aes(x = TotalAmount)) +
  geom_histogram(binwidth = 50, fill = "lightblue", color = "black") +
  facet_wrap(~ final_category) +
  theme_minimal() +
  labs(title = "Histograms of Amount Spent by Category", 
       x = "Amount Spent ($)", 
       y = "Frequency") +
  theme(strip.text = element_text(size = 12))
ggsave("../views/Spent_Cat.png", plot = last_plot(), width = 8, height = 6, dpi = 300)

# Statistic Analysis ------------------------------------------------------

#H0:μ_1=μ_2 --> the means of Standard rooms and Suites are not statistically signifcant 
#H0:μ_1≠μ_2 

#to use the t-test we make the assumption that the data for each group should be normally distributed.
#We will first test this to decide with method to apply
#We cannot use the Shapiro test as we have more than 5000 observations in Specialty Suite- limitation of R
#We will therefore do a KS for a formal result

ks_test_results <- db %>%
  group_by(ProductCategory) %>%
  summarise(
    ks_test = list(ks.test(Amount, "pnorm", mean = mean(Amount), sd = sd(Amount))),
    p_value = sapply(ks_test, function(x) x$p.value)
  )

print(ks_test_results)
#The results for Standard and Suite reject the null hypothesis, meaning that there is enough statistical evidence to 
#say that the amounts for Standard and Premier do not follow a normal distribution
#we will therefore need to use a non-parametric method in the above hypothesis testing

#We will use the Mann-Whitney test
group_Standard <- db %>% filter(ProductCategory == "Standard") %>% pull(Amount)
group_Suite <- db %>% filter(ProductCategory == "Suite") %>% pull(Amount)

# Perform Mann-Whitney U test
mann_whitney_result <- wilcox.test(group_Standard, group_Suite)

# Print the result
print(mann_whitney_result)

#We reject the null hypothesis, and therefore say that the mean amount spent for Standard rooms and suites is not the same.

# Predictive Models Prep-------------------------------------------------------

#We will copy the customer category in the main db
summary_customers$HighValue <- ifelse(summary_customers$TotalAmount > 1500, 1, 0)
summary_customers$HighValue <- as.factor(summary_customers$HighValue)

cat<-summary_customers[,c("CustomerID","final_category","HighValue")]

db <- merge(db, cat, by = "CustomerID", all.x = TRUE)

#train-test
#30%test, 70% train
set.seed(1234)
id_train<-sample (1:nrow(db),size=0.7*nrow(db), replace=F) 
train<-db[id_train,]
test<-db[-id_train,]

skim(train) #we need to double check that HighValue is set as a factor variable
train$HighValue <- factor(train$HighValue, levels = c("0", "1"), labels = c("No", "Yes"))

#If we had more numerical values, we would start seeing correlations here (e.g. using a matrix)

#NOTE: Normally, we would need to standarize/normalize the variables. However, since we use the package Caret, we don't need to.

#Sava data as csv
write.csv(db,file="predictive_models_data.csv")
write.csv(test,file="test.csv")
write.csv(train,file="train.csv")

# TRAIN CONTROL -----------------------------------------------------------
db<- read.csv("predictive_models_data.csv")
test<-read.csv("test.csv")
train<-read.csv("train.csv")

ctrl<- trainControl(method = "cv", #cross validation
                    number = 10, #we will split data in 10
                    classProbs = TRUE, 
                    verbose=FALSE,
                    savePredictions = T)

# LOGIT 1 -------------------------------------------------------------------
set.seed(1234)
logit1 <- train(HighValue~Amount+ProductCategory+final_category, #formula, we could have broken down categorical variables into dummies and chose to keep all or subsection
                data = train,
                metric="Accuracy",
                method = "glmnet", #logistic regression with elastic net regularization
                trControl = ctrl,
                family= "binomial"
)

logit1 #once we run this model, we see an initial value for alpha and lambda that we can use in the tune grid to improve the performance of the model

#This shows it in a graphic manner
plot(logit1$results$lambda,
     logit1$results$Accuracy,
     xlab="lambda",
     ylab="Accuracy")

predictTest_logit1 <- data.frame(
  obs = train$HighValue,                    ## observed class labels
  predict(logit1, newdata = train, type = "prob"),   ## predicted class probabilities
  pred = predict(logit1, newdata = train, type = "raw")    ## predicted class labels
)

head(predictTest_logit1)

predictTest_logit1$obs <- factor(predictTest_logit1$obs, levels = c("No", "Yes"))
confusionMatrix(data = predictTest_logit1$pred, reference=predictTest_logit1$obs)
#We have an 87% accuracy when we check in the training data.
#We are classifying a lot of the valued customers as no, while doing a good job of not classifying the not not high value customers.
#Tradeoff between sesitivity and specificity. In this case our sensitivity is 1, but specificty 0.27

#We will fine tune the grid and also evaluate another cut as the default (0.5)

# GRID ------------------------------------------------------------------

#hyperparameters.
hyperparameter_grid <- expand.grid(alpha = seq(0, 0.15, 0.01), # iremos variando los valores
                                   lambda = seq(0.03, 0.04, 0.001)) # iremos variando los valores

colnames(hyperparameter_grid) <- c("alpha", "lambda")


# LOGIT2 ------------------------------------------------------------------

set.seed(1234)
logit2 <- train(HighValue~Amount+ProductCategory+final_category, #formula, we could have broken down categorical variables into dummies and chose to keep all or subsection
                data = train,
                metric="Accuracy",
                method = "glmnet", #logistic regression with elastic net regularization
                trControl = ctrl,
                tuneGrid = hyperparameter_grid,
                family= "binomial"
)

logit2 #once we run this model, we see an initial value for alpha and lambda that we can use in the tune grid to improve the performance of the model

#Tuning our grid didn't help improve our model's accuracy
plot(logit2$results$lambda,
     logit2$results$Accuracy,
     xlab="lambda",
     ylab="Accuracy")

predictTest_logit2 <- data.frame(
  obs = train$HighValue,                    ## observed class labels
  predict(logit2, newdata=train, type = "prob"),         ## predicted class probabilities
  pred = predict(logit2, newdata=train, type = "raw")    ## predicted class labels (esto luego lo sacamos porque vamos a variar el corte)
)

head(predictTest_logit2)

predictTest_logit2$obs <- factor(predictTest_logit2$obs, levels = c("No", "Yes"))
confusionMatrix(data = predictTest_logit2$pred, reference=predictTest_logit2$obs)

#Creating a new cut
roc_data <- roc(predictTest_logit2$obs, predictTest_logit2$Yes)
plot(roc_data, main = "ROC Curve", col = "purple", lwd = 2) #this shows our ROC curve. We are high in sensitivity and low in specificity.
mycoords <- coords(roc_data, "all")

#to see what the best cutoff would be if we want a balance between sensitivity and specificity
plot(mycoords$threshold, mycoords$sensitivity, type = "l", col = "red",
     xlab = "Cutoff", ylab = "Sensitivity", main = "Sensitivity vs. Cutoff")
lines(mycoords$threshold, mycoords$specificity, col = "blue")
legend("bottomright", legend = c("Sensitivity", "Specificity"), col = c("red", "blue"), lwd = 2)

#New Matrix
predicted_probabilities <- predictTest_logit2$Yes
new_cutoff<-0.18
predictTest_logit2$new_thres <- factor(ifelse(predicted_probabilities > new_cutoff, "Yes", "No"))

confusionMatrix(data = predictTest_logit2$new_thres, reference=predictTest_logit2$obs)
#Our accuracy has decreased to 78%, However, we are doing a better job at classifying high value guests


# PREDICTION LOGIT --------------------------------------------------------

# Logit1

test$logit1<- predict(logit1, newdata=test)

test$HighValue <- factor(test$HighValue, levels = c("0", "1"), labels = c("No", "Yes"))
confusionMatrix(data = test$logit1, reference = test$HighValue)
#We mantained the accuracy of 87%, but once again our specificity is very low.

#Logit2

#Remember in our train model we selected a cutoff of 0.18
test$logit2_prob <- predict(logit2, newdata = test, type = "prob")
test$logit2 <-factor(ifelse(test$logit2_prob$Yes > new_cutoff, "Yes", "No"),  levels = c("No", "Yes"))

#test$logit2 <- factor(test$logit2, levels = c("No", "Yes"), labels = c("0", "1"))
confusionMatrix(data = test$logit2, reference = test$HighValue)
#We get an accuracy of 78%, with 0.8391 in sensitivity and 0.4908 is specificity




# Forest ------------------------------------------------------------------

ctrl3<- trainControl(method = "cv",
                    number = 5,
                    classProbs = TRUE,
                    verbose=FALSE,
                    savePredictions = T)

set.seed(1234)

class_forest <- train(
  HighValue~Amount+ProductCategory+final_category,
  data=train,
  method = "ranger",
  trControl = ctrl3,
  tuneGrid=expand.grid(
    mtry = c(1,2,3,4,5,6,7,8), #cualquier subconjunto es bagging
    splitrule = "gini", #parta el arbol a traves de gini
    min.node.size = c(15,30,45,60)) #controlamos la profundidad del arbol por el numero de obs minimo
)

class_forest
#the values used for the model wer mtry=3, splitrule=gini and min.nod.size=30.
#Based on these results we could try to further finetune

predictTest_forest <- data.frame(
  obs = test$HighValue,                                    ## observed class labels
  predict(class_forest, newdata = test, type = "prob"),         ## predicted class probabilities
  pred = predict(class_forest, newdata = test, type = "raw")    ## predicted class labels
)

# Accuracy
mean(predictTest_forest$obs==predictTest_forest$pred)
#The accuracy is also 87%

# TREE --------------------------------------------------------------------

ctrl2<- trainControl(method = "cv",
                    number = 10,
                    classProbs = TRUE,
                    verbose=FALSE,
                    savePredictions = T)


set.seed(1234)

train$HighValue <- factor(train$HighValue, levels = c("No", "Yes"), labels = c("0", "1"))
train$ProductCategory <- as.factor(train$ProductCategory)
train$final_category <- as.factor(train$final_category)

train <- subset(train, select = -c(X, X.1, X.2))

class_tree <- train(HighValue~Amount+ProductCategory+final_category+Yr,
                       data = train, 
                       method = "rpart", #trees
                       trControl = ctrl2,
                       tuneLength=100) #100 alphas


class_arboles
#aqui conseguimos cual es el mejor cost complexitu prunning (cp)

predictTest_arbol <- data.frame(
  obs = test$Default,                                    ## observed class labels
  predict(class_arboles, newdata = test, type = "prob"),         ## predicted class probabilities
  pred = predict(class_arboles, newdata = test, type = "raw")    ## predicted class labels
)

head(predictTest_arbol)


# Accuracy
mean(predictTest_arbol$obs==predictTest_arbol$pred)

p_load("rpart.plot")
prp(class_arboles$finalModel, under = TRUE, branch.lty = 2, yesno = 2, faclen = 0, varlen=15,tweak=1.2,clip.facs= TRUE,box.palette = "Greens",compress=FALSE,ycompress = FALSE)

