# Machine-Learning_-Housing-Prices-Prediction
Apply ML algorithm to predict the prices of over 70,000+ houses in Edmonton.

## Objectives
1. Predict the "assessed_value" of a house using whatever features you can utilize, based on the provided dataset.

2. Predict the increase (1) or decrease (-1) of "assessed_value" of a house from year 2015 to year 2016.

## Specific strategies
1. Divide the provided dataset of 70,000+ houses into training data and testing data randomly. 10% of the data should be testing data. Train and tune your model on your training dataset ONLY. Report errors (relative errors or RMSE) on the held out testing data.
 
2. The 2016 assessed_value is unknown. First, assume the 2015 assessed_value is always known for all the houses and can be used as a feature in your prediction. Second, redo the task assuming the 2015 assessed_value is not known for houses in the test dataset and thus can NOT be used as a feature in your prediction.

## Additional information
1. The online houses information from 2012 - 2016 can be retrived on:
https://data.edmonton.ca/City-Administration/Property-Assessment-Data-2012-2016-/qi6a-xuwt/data

2. Note that in Edmonton Open Property Assessment Data, the GPS locations are not available, however, you can leverage the GPS information.

## Addtional Codes 
Filtering.R: the data preprocessing step, the categorical and numerical data are cleaned and extracted for the next step.
Prediction and Prediction RF: implementation of ML algorithm.


