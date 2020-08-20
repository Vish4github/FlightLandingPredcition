# Flight Landing Prediction

In this study I aim to understand what factors affect the landing distance of a commercial flight. The motivation behind this goal is to reduce the risk of landing overrun. 

The various stages delineated in this report aims to find the most important factors among a set of variables that impact the landing distance. We analyze data for two main makes, Boeing and Airbus. Data quality and data completeness are two main issues noticed during analysis, interfering with the model building and inferences derived. 

Since it’s a small data set, ignoring or removing observations with null values wasn’t a preferred methodology as it was aimed not to lose valuable information. The original dataset had some issues such as missing values, duplications and abnormalities with the variable values. These issues were accounted for in the best possible manner before moving on to the model building and inference. A simple linear model was made to fit the data using relevant predictors, after accounting for multicollinearity and other possible issues. 

Ground speed was found to the most important factor to be considered while trying to reduce the risk of landing overrun from the minimal data that was available. However, it was inferred that a simple linear model was not an adequate fit for the data and a log transformation was necessary.  
A larger and more complete data set would be beneficial in understanding the relationship between landing distance and other factors in much more comprehensive manner.

## Motivation: To reduce the risk of landing overrun
## Goal: To study what factors and how they would impact the landing distance of a commercial flight
## Data: Landing data (landing distance and other parameters) from 800 commercial flights  

### Variable dictionary:

* Aircraft: The make of an aircraft (Boeing or Airbus). 
* Duration (in minutes): Flight duration between taking off and landing. The duration of a normal flight should always be greater than 40min. 
* No_pasg: The number of passengers in a flight. 
* Speed_ground (in miles per hour): The ground speed of an aircraft when passing over the threshold of the runway. If its value is less than 30MPH or greater than 140MPH, then the  landing would be considered as abnormal. 
* Speed_air (in miles per hour): The air speed of an aircraft when passing over the threshold of the runway. If its value is less than 30MPH or greater than 140MPH, then the landing would be considered as abnormal. 
* Height (in meters): The height of an aircraft when it is passing over the threshold of the runway. The landing aircraft is required to be at least 6 meters high at the threshold of the runway. 
* Pitch (in degrees): Pitch angle of an aircraft when it is passing over the threshold of the runway.
