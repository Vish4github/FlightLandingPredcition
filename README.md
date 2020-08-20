# Flight Landing Prediction

In this study I aim to understand what factors affect the landing distance of a commercial flight. 
The motivation behind this goal is to reduce the risk of landing overrun. The various stages delineated in this report aims to find the most important factors among a set of variables that impact the landing distance. We analyze data for two main makes, Boeing and Airbus. Data quality and data completeness are two main issues noticed during analysis, interfering with the model building and inferences derived. Since it’s a small data set, ignoring or removing observations with null values wasn’t a preferred methodology as it was aimed not to lose valuable information. The original dataset had some issues such as missing values, duplications and abnormalities with the variable values. These issues were accounted for in the best possible manner before moving on to the model building and inference. A simple linear model was made to fit the data using relevant predictors, after accounting for multicollinearity and other possible issues. Ground speed was found to the most important factor to be considered while trying to reduce the risk of landing overrun from the minimal data that was available. However, it was inferred that a simple linear model was not an adequate fit for the data and a log transformation was necessary.  
A larger and more complete data set would be beneficial in understanding the relationship between landing distance and other factors in much more comprehensive manner.