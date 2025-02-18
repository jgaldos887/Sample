# Data Cleaning and Preparation


The data generated was intended to represent hotel bookings across various room categories and customer segments. Fictional data was created according to established guidelines. The offered products are categorized into five distinct types. Additionally, CustomerID serves as a unique identifier, Amount denotes the transaction value, and TransactionDate records the date of each transaction.

To clean the data, we began with an initial exploration using basic commands to obtain statistical summaries. We also created several graphs to understand the distribution of each variable. This process revealed issues with missing values. For date values, we decomposed the numbers into day, month, and year, which highlighted input errors. We assumed that any numbers below a certain threshold (1900) were erroneous and replaced by combining the value in Day + "20," given that other values were in the 2000s. However, this approach did not fully resolve the issue, as subsequent graphs still showed anomalies. To address this, we planned to flag the modified dates to track them in future graphs. If these flagged dates continued to deviate from the expected parameters and no additional data could be used for correction, we would need to assess their impact on our results and potentially exclude them from the analysis. Care must be taken to avoid bias, particularly if the excluded data is from a specific group and constitutes a significant portion of the dataset. An alternative approach would be to compare results with and without the excluded data to check for significant differences. In our case, since we hadn't directly worked with the Date variable, we opted to retain all the information.

To handle missing values, we employed the MICE package and the Predictive Mean Matching (PMM) method, which works for both continuous and categorical variables. We then removed any duplicate transactions.

At this stage, we could have evaluated outliers in each variable to decide whether to retain or exclude them. We also converted the different date variables (Yr, Month, Month-Yr) to the correct Date format and changed ProductCategory to a factor. Once these adjustments were made, we updated the statistical tables.

# Exploratory Data Analysis

The transaction amount followed a normal distribution, which deviates from our expectations. We anticipated a higher frequency of transactions with lower values, resulting in a right-skewed graph. To understand why the observed distribution differs from expectations, a detailed examination of the data is necessary. Factors to investigate include whether the data is from a specific segment, if it varies across different seasons (low vs. high) or years.

![Image 1](views/Amount2.png)

When plotting the data by date, we observe that prices appear to follow a consistent trend throughout the period, which is unexpected. Typically, rates should increase annually, unless influenced by a recession, in which case they might decline. A flat rate over time suggests potential issues with rate optimization. If the data is accurate, it implies that rates should be adjusted at least in line with yearly inflation; otherwise, we risk selling at below-market prices.

![Image 2](views/Amount_time.png)

Boxplots of the amount spent by product category reveal discrepancies in rates by room type. Generally, suite prices are higher than those of specialty suites, while standard, premier, and specialty suites are quite similar. This may be influenced by the different customer segments. To investigate this further, we need to compare the graphs for each main segment. However, the presidential suite aligns with expected pricing behavior.

![Image 3](views/Amount_category.png)

Customers are categorized into tiers: Bronze, Silver, Gold, and Platinum, based on their total amount spent or number of transactions.

Bronze clients typically make around 1 transaction with an average spend of $357. The distribution of transaction amounts is tightly clustered around this median value, indicating minimal variation in transaction values.

Silver clients average 2.7 transactions and spend $988. The transaction distribution is uneven, with a significant number of customers having fewer transactions than the median. The total amount spent shows considerable variation.

Gold clients average 6.4 transactions and spend $2,314. Like Silver clients, the distribution of transactions is uneven.

Platinum clients average 9 transactions and spend $2,889.

![Image 4](views/Category_Transaction.png)

![Image 5](views/Spent_Cat.png)


# Statistical Analysis

We want to determine if the mean amount spent on standard rooms and suites is the same. To address this, we formulate the following hypotheses:

- **Null Hypothesis (H₀):** $\( \mu_1 = \mu_2 \)$
- **Alternative Hypothesis (H₁):** $\( \mu_1 \neq \mu_2 \)$

Here, the null hypothesis states that the mean transaction amounts for standard rooms and suites are statistically equivalent.

To use the t-test, we must assume that the data for each group is normally distributed. To test this assumption, we will use the Kolmogorov-Smirnov test. We cannot use the Shapiro-Wilk test because it is limited to datasets with up to 5000 observations, whereas our Specialty Suite dataset exceeds this limit.

The results indicate that the null hypothesis is rejected for Standard and Suite products. This implies there is sufficient statistical evidence to conclude that the amounts spent on Standard and Suite products do not follow a normal distribution. Consequently, we must use a non-parametric method for hypothesis testing.

We applied the Mann-Whitney U test and obtained a p-value of \( < 2.2 \times 10^{-16} \). Therefore, we reject the null hypothesis and conclude that the mean amount spent on standard rooms and suites is significantly different.


# Predictive Modeling

We split our data into training (70%) and test (30%) datasets and employed two classification models: Logistic Regression (Logit) and Random Forest.

For the Logistic Regression model, we applied 10-fold cross-validation and used accuracy as the performance metric. Elastic Net regularization was utilized, and initially, we did not tune the hyperparameters (alpha and lambda).

To evaluate the model’s performance, we used a confusion matrix. The initial results showed an accuracy of 87%, with 100% sensitivity and 27% specificity. This indicates that while there were no false positives (i.e., non-high-value customers were not misclassified as high-value), the model had a high rate of false negatives (i.e., high-value customers were not correctly identified).

To improve performance, we refined the Logistic Regression model by tuning the hyperparameters and adjusting the classification threshold, which by default is 0.5. We selected a threshold of 0.18, resulting in an accuracy of 78%, with 84% sensitivity and 50% specificity. This adjustment led to 16% of non-high-value customers being misclassified as high-value, but improved the correct classification of high-value customers to 50%. If the goal is to minimize misclassification of non-high-value customers, the initial model is preferable. Conversely, if the aim is to better identify high-value customers even at the risk of misclassifying others, the refined model is more suitable.

It is important to note that with more data on each customer, the model's performance could potentially improve, as we are currently using only three variables to determine customer classification.

After developing the models, we tested them on the test dataset to ensure no overfitting. The accuracy remained consistent.

Finally, we applied a Random Forest model with 5-fold cross-validation using the ranger method, achieving an accuracy of 86.9%.

# Repository structure

The documents found in this repository are as follows:

- `document`: Contains some exported tables.
- `scripts`: Contains all the R scripts for the development of this project.
- `stores`: Contains all the data used and saved throught the code.
- `views`: Contains all the images generated and saved.
