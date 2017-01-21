## Kaggle Airbnb New User Bookings

In this project, I analyzed data on Airbnb datasets and predicted where a new user will make their first booking. You can download the datasets from the [Kaggle competition page](https://www.kaggle.com/c/airbnb-recruiting-new-user-bookings).

Using the data, I analyzed factors that correlated with destination country, and did some exploratory visualization and analysis.  I then cleaned the train and sessions datasets and created two models that predict where a new user will book their first travel experience. 

You can see the exploratory data analysis [here](https://erica-tan.github.io/Kaggle-Airbnb/).  

You can see the data pre-processing code in `Airbnb_session.R` and `Airbnb_data_cleaning.R` above and the model code in `Airbnb_regression.R` and `Airbnb_xgboost.R` above.

###Required Libraries
dplyr,data.table, caret, ROCR, xgboost
