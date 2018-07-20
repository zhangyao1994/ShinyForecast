# Load the data and format them once to save some time for the Shiny App

# Load library
library(feather)
library(tidyverse)

# Load data for 12-week forecast
CFG_fcast.joined <- read_feather('~/GitHub/ShinyPractice/Fcast-app/data/CFG_fcast.feather')
# Renaming the factors helps with the legend order.
Modelnames <- levels(as.factor(CFG_fcast.joined$Model))
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
NewModelNames <- c("3 ARIMA","1 Actual","9 Mean","8 MRP_Fcast","4 Prophet","7 TBATS","6 LinearModel","5 RandomForest","2 Xgboost")
=======
NewModelNames <- c("3 ARIMA","1 Truth","8 MRP_Fcast","4 Prophet","7 TBATS","6 LinearModel","5 RandomForest","2 Xgboost")
>>>>>>> parent of 433f6d3... Add the Model Combination Mean
=======
NewModelNames <- c("3 ARIMA","1 Truth","8 MRP_Fcast","4 Prophet","7 TBATS","6 LinearModel","5 RandomForest","2 Xgboost")
>>>>>>> parent of 433f6d3... Add the Model Combination Mean
=======
NewModelNames <- c("3 ARIMA","1 Truth","8 MRP_Fcast","4 Prophet","7 TBATS","6 LinearModel","5 RandomForest","2 Xgboost")
>>>>>>> parent of 433f6d3... Add the Model Combination Mean
len_Model <- length(Modelnames)
for (i_Model in 1:len_Model){
  CFG_fcast.joined$Model <- replace(CFG_fcast.joined$Model, which(Modelnames[i_Model]==CFG_fcast.joined$Model), NewModelNames[i_Model])
}
write_feather(CFG_fcast.joined,"~/GitHub/ShinyPractice/Fcast-app/data/CFG_fcast.feather")

# Load data for Cross-Validation forecast
All_fcast_CV <- read_feather('~/GitHub/ShinyPractice/Fcast-app/data/All_fcast_cv.feather')
Modelnames <- levels(as.factor(All_fcast_CV$Model))
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
NewModelNames <- c("3 ARIMA","1 Actual","4 Prophet","7 TBATS","6 LinearModel","5 RandomForest","2 Xgboost")
=======
NewModelNames <- c("3 ARIMA","4 Prophet","7 TBATS","6 LinearModel","5 RandomForest","2 Xgboost","1 Truth")
>>>>>>> parent of 433f6d3... Add the Model Combination Mean
=======
NewModelNames <- c("3 ARIMA","4 Prophet","7 TBATS","6 LinearModel","5 RandomForest","2 Xgboost","1 Truth")
>>>>>>> parent of 433f6d3... Add the Model Combination Mean
=======
NewModelNames <- c("3 ARIMA","4 Prophet","7 TBATS","6 LinearModel","5 RandomForest","2 Xgboost","1 Truth")
>>>>>>> parent of 433f6d3... Add the Model Combination Mean
len_Model <- length(Modelnames)
for (i_Model in 1:len_Model){
  All_fcast_CV$Model <- replace(All_fcast_CV$Model, which(Modelnames[i_Model]==All_fcast_CV$Model), NewModelNames[i_Model])
}
write_feather(All_fcast_CV,"~/GitHub/ShinyPractice/Fcast-app/data/All_fcast_cv.feather")

