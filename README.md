# Data Cleaning and Preparation


To clean the data, we proceeded to do an initial exploration of the data available. This was done using simple commands to see statistical summaries. Moreover, we also did several different graphs to understand the distribution of each of the variables.
This allowed us to see that there were some problems of missing values. In case of the date values, we broke down the numbers in day, month and year. This made it apparent that there had been an issue in how this data was inputted. We made the assumption that all numbers below a selected threshold (1900) were mistakes, and so we decided to paste the “Day” with the number “20”, as every other value was 2000+. This, nevertheless, did not completely fixed the error, as when we did a new graph, we could still see that the distribution was odd. To validate this, the best action plan would have been to put an identifier to the dates we were changing, so we could later track them when we did the new graph. If all of them still fell outside the rest of the parameters and there was no other data we could use, we would have to decide if this would affect our results and exclude these from our analysis. We would have to be careful though, as if all the data excluded was from a particular group, it could create a bias in our results. This, especially if they represented a big amount of values compared to the sample size. Another way to handle this, would be to run tests with both datasets and see if they vary significantly. In this case, as we have not worked directly with the variable Dates, we decided to keep all the information. 


To tackle missing values, we used the package MICE and PMM (Predictive Mean Matching) method. This method works for both continuous and categorical values, 
Finally, we proceeded to remove the duplicate transactions. 


On this note, we could have decided at this point to also evaluate outliers in each of the variables and decide whether we wanted to keep them or not. 
Once we did this, we checked once again our data; we run statistics tables and produced several graphs.
