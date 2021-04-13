#install.packages("devtools")
#devtools::install_github("ropensci/bikedata")
library("readr")
#shapper::install_shap()
library("DALEX")
library("tidyverse")
library("caret")
library("ggplot2")
library("xgboost")
library("SHAPforxgboost")
library("here")
library("shapr")

#dl_bikedata("London", data_dir = "C:/Users/lordv/Desktop", dates = NULL, quiet = FALSE)

dir_path <- 'C:/Users/lordv/Desktop/BikeData/'
file_pattern <- '.csv' 

### function to load and preprocess data
read_dir <- function(dir_path, file_name){
  print(file_name)
    read_csv(paste0(dir_path, file_name)) %>% 
    select(c(`Rental Id`, 
             Duration, 
             `Bike Id`, 
             `End Date`, 
             `EndStation Id`, 
             `EndStation Name`, 
             `Start Date`,
             `StartStation Id`,
             `StartStation Name`))  %>%
    select(-c(`EndStation Name`,`StartStation Name`,`Rental Id`,`Bike Id`)) %>%
    filter_all(all_vars(!is.na(.))) %>%
    mutate(hour = as.integer(substr(`Start Date`,12,13))) %>%
    mutate(day = weekdays(as.Date(substr(`Start Date`,1,10),"%d/%m/%Y"))) %>%
    mutate(year = substr(`Start Date`,7,10) )  %>%
    select(-c(`Start Date`,`End Date`))  %>%
    mutate(target = ifelse(`EndStation Id`==`StartStation Id`,1,0)) %>%
    select(-c(`EndStation Id`,`StartStation Id`)) %>%
    sample_frac(0.05)
}


df <- 
  list.files(dir_path, pattern = file_pattern) %>% 
  map_df(~ read_dir(dir_path, .))

df$day <- as.factor(df$day)
df$year <- as.integer(df$year)
df$target <- as.factor(df$target)
summary(df)


#Modeling
set.seed(3456)
trainIndex <- createDataPartition(df$target, p = .8, 
                                  list = FALSE, 
                                  times = 1)

dfTrain <- df[ trainIndex,]
dfTrain$year <- as.integer(as.character(dfTrain$year))
dfTest  <- df[-trainIndex,]
dfTest$year <- as.integer(as.character(dfTest$year))
rm(df); gc()


###XGBoost
x_var <- c("Duration", "hour", "day", "year")

Xtrain <- dfTrain[,x_var]
YTrain <- as.vector(dfTrain$target)
XTest <- dfTest[,x_var]
YTest <- as.vector(dfTest$target)

dummylist <- make_dummies(traindata = Xtrain, testdata = XTest)

x_train_dummy <- dummylist$train_dummies
x_test_dummy <- dummylist$test_dummies

model_cat <- xgboost::xgboost(
  data = x_train_dummy,
  label = as.integer(YTrain),
  nround = 40,
  verbose = TRUE
)

#model_cat$feature_list <- dummylist$feature_list

shap_values <- shap.values(xgb_model = model_cat, X_train = x_train_dummy)
shap_values$mean_shap_score
shap_values$shap_score

# To prepare the long-format data:
shap_long <- shap.prep(xgb_model = model_cat, X_train = x_train_dummy)
shap_long <- shap.prep(shap_contrib = shap_values$shap_score, X_train = x_train_dummy)
shap.plot.summary(sample_frac(shap_long,0.01))



fig_list <- lapply(names(shap_values$mean_shap_score)[1:4], 
                   shap.plot.dependence, data_long = shap_long)
gridExtra::grid.arrange(grobs = fig_list, ncol = 2)




