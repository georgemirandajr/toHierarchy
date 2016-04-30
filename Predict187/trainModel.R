
# R's randomForest() package will be used to classify murderers/non-murderers, including attempted murderers. This work is based on an original study by Berk, R et al (2009). For future reference, this analysis was performed using R version 3.2.3 (2015-12-10) on a Windows 32-bit machine running on Windows 7. 

# This documentation is intended to share the general methodology of engineering some of the variables; however, for the sake of brevity, it does not go through the entire process of creating all the variables used in the final data. The final dataset used in the model is provided in the aps.rds file. Although the train.rds and test.rds datasets are provided in order to reproduce results, others may choose to slice the data in ways that may lead to better models, which is why the full and final dataset is provided.


# The environment
packages <- c("dplyr", "Hmisc", "randomForest", "caret")
# dplyr - 0.4.3
# randomForest - 4.6-12
# caret - 6.0-68
# acs - 1.2  (not used in this code, but was used to create variables such as income and population)

sapply(packages, require, character.only = TRUE)


# The Data ----------------------------------------------------------------

# APS List
# Probationers that began their supervision between October 1, 2011 and September 30, 2013 are included in the data. For each case, a follow-up period of 2 years was established to ensure that all cases were followed for the same amount of time. This data comes from an internal system referred to as the APS (Adult Probation System) list, which contains records on all cases, including closed cases and new cases that have not yet been transferred from the state to the county. 

# It is also worth noting that the data consists solely of AB 109 probationers. These are individuals who served time in the state prison system and were identified to be supervised by local counties, as opposed to state parole officers.  

# The actual data have been anonymized for this repo. The data contains 16,694 observations and was split 60/40 into a training and testing set, respectively. 
# Load the data 
aps <- readRDS("aps.rds")
train <- readRDS("train.rds")
test <- readRDS("test.rds")

# First Dive into the Data

# We can create some tables that show the proportion of murders/attempted committed for certain variables. Particularly of interest are variables such as age, whether the person was white, and whether the person was male.
prop.table(table(aps$White, aps$Murder), 1)
prop.table(table(aps$Male, aps$Murder), 1)
prop.table(table(aps$Age, aps$Murder), 1)

# For each table, we can see that each of these variables indicate that 99% of the population have not committed or attempted murder. As Berk suggests, this highlights the difficulty of making "worthwhile distinctions between prospective murderers and the rest". He argues that this insight underscores the need to account for the costs of false negatives and false positives.

#------ Violent Offenses ------#
# The California Department of Corrections and Rehabilitation (CDCR) website provides a list of offenses defined as violent. Although some penal codes are provided on the site, many codes had to be deciphered because the website uses broad terms to describe some offenses. For instance, the website refers to mayhem and murder instead of specifically referring to PC203 and PC187, respectively. The 'Primary Charge' variable was used to develop a binary variable indicating whether or not the person's primary charge fell within the violent category.

violent_codes <- c("PC187(A)", "PC664/187", "PC664/187(A)", "PC664/211", "PC664/459", "PC192(A)", "PC203", "PC204", "PC205", "PC206", "PC261", "PC261(A)(2)", "PC262", "PC286", "PC288(A)", "PC288.5", "PC12022.7", "PC12022.8", "PC12022.9", "PC12022.3", "PC12022.5", "PC12022.53", "PC12022.55", "PC451(A)", "PC451(B)", "PC289(A)", "PC289(J)", "PC12308", "PC12309", "PC12310", "PC220", "PC215(A)", "PC264.1", "PC460(A)")

# The code below was used to create the variable, ViolentCase. It is shown for documentation purposes only. The 'aps' data already includes this feature.
aps <- aps %>% 
  dplyr::mutate(ViolentCase = ifelse(`PRIMARY CHARGE` %in% violent_codes, 1, 0))

##------------ Select data for modelling -------------##
# Some predictors might sound like a good idea to have in the model, but may not provide much influence on the predictions. To find out which variables will not have much impact I first decided to look at the variance of each one. If a variable has little to no variance, then it is likely not going to change the outcome variable. The caret package contains a function to do this "near zero variance" analysis.

nz <- caret::nearZeroVar(aps, saveMetrics = TRUE)

nz

rm(nz)

# After inspecting the results, most of the engineered variables (e.g. age, income, population) appear to have variance that could be useful in a model. The only engineered variables that showed near zero variance were RegisterSO (Registered Sex Offender), WeaponCase (Primary Charge is weapons-related), and DrugCase (Primary Charge is drug-related). ViolentCase and Gang are non-near zero variance, therefore they will be kept. Earlier contingency tables also suggested a relationship between these variables and the response variable. 

# Select the columns that contain the final predictors and the outcome variable, Murder.
data_predictors <- aps %>% 
  dplyr::select(Murder, Age, White, Male, Total_Pop, Black_Pop, Prop_Black, Income, Zip_Present, Gang, ViolentCase, Income_Log)

# Many of the predictor variables should be converted to factor class prior to modelling. 
toConvert <- c("Murder", "White", "Male", "Zip_Present", "Gang", "ViolentCase")
data_predictors[,toConvert] <- lapply(data_predictors[,toConvert], factor)


# Partition Data ----------------------------------------------------------

# Set the seed in order to reproduce results. 
set.seed(1001)

# The train and test sets were created using the following code. You can recreate them or just use the ones provided.
inTrain <- createDataPartition(data_predictors$Murder, 
                               p = 0.6, 
                               list = FALSE, 
                               times = 1)

train <- data_predictors[inTrain,]

test <- data_predictors[-inTrain,]

# Use randomForest to model the data

# First Model -------------------------------------------------------------

fit <- randomForest(Murder ~ Age + White + Male + Total_Pop + Black_Pop + Prop_Black + Income + Zip_Present + Gang + ViolentCase + Income_Log, 
                    data = train, 
                    importance = TRUE, 
                    ntree = 1500,
                    na.action = na.roughfix)

pred <- predict(fit, test)  # make predictions on unobserved data
cm <- caret::confusionMatrix(table(pred, test$Murder), positive = "1") 

# This first pass at modelling yielded 99% accuracy! 
# However, it failed in the most important way; it correctly predicted only 1 out of 51 murderers.

# Let's see what the ROC curve looks like. 
# Order the factor variables that contain the reference and predicted values. 
test$Murder<- ordered(test$Murder)  # response variable
pred <- ordered(pred)  # predictor variable


# Model 1 ROC -------------------------------------------------------------
# Create the ROC curve using pROC package.
ROC <- pROC::roc(response = test$Murder, predictor = pred, levels = levels(test$Murder))

# Plot the ROC curve and get the area under the curve. The area under the curve is 0.6944, which refers to
pROC::plot.roc(ROC, col = rgb(1, 0, 0, .5), lwd = 2)  # AUC = 0.5095


# Model 1 Variable Importance ---------------------------------------------

varImpPlot(fit)

# A plot of the mean decrease in model accuracy revealed that some variables were actually making the model worse than if the variables had not been included at all. These variables were White, Male, and Gang. The lack of accuracy by the Male variable made logical sense because most observations were male. The near zero variance test did not catch this variable because 90% of observations were male, making it far away enough from zero variance. And although about 18% of observations were white, the White variable actually took away from model accuracy and did not do well when splitting the data (refer to MeanDecreaseGini index). Then there's the Gang variable. At first glance, one would suspect that this is a good indicator of extreme violence such as murder; however, the data itself is suspect. Many probationers do not admit to being involved with a gang in order to avoid closer supervision, making this an underreported metric. Despite the fact that 35% of observations indicated gang affiliation, it did not fare well in determining accuracy or making distinctions in the outcome. 

# The Income_Log variable appeared to have nearly the exact effect; therefore, the Income_Log was removed from the model as well.   

# In summary, this first model was no better than randomly picking one out of the waiting room at a supervision office. The removal of some variables and parameter tuning later resulted in model improvement. The model could be improved by lowering the threshold at which the model makes predictions. By default, the model assigns 0 or 1 based whether the probability is higher than 50% that it is either 0 or 1 (majority vote). Lowering this threshold will generate many more false positives, but will minimize the number of instances where an individual is missclassified as being a non-murderer. To aide in making this business decision, it will be helpful to understand the probability distribution of all positive and negative 


# Final Model ------------------------------------------------------------

# The modelling process included many iterations of a fitted object; however, the final model parameters are detailed and explained in this section to cut to the chase.

# Re-sampling
# Because murderers are outnumbered in the data nearly 100:1, we must correct for this when sampling. The negative outcomes (non-murderers) need to be undersampled and the positive (murderer) outcomes should be oversampled. I achieve this with the sampsize argument and stratify the sampling by the outcome variable using the strata argument. 

# NA Values
# There are some NA values in the data, so I decided to make use of the na.action = na.roughfix argument. This uses the median of a variable to fill in missing values of that variable. For non-numeric variables, this argument uses the most frequently appearing value (mode) of a factor variable. 

# ntree
# I decided to grow the tree 1,500 times to ensure that each observation was used in the model several times. On average, each observation was used about 20 times in the model across 1,500 trees, while some were used as many as 745 times and others only 4 times. I kept track of this with the keep.inbag argument. 
# Voting
# I adjusted the cutoff for voting for any single classifier (default is a majority vote or 1/k). I lowered the threshold for murderer votes and raised the threshold for non-murderer votes. Later on during prediction, I raise the non-murderer threshold by 5% while leaving the murderer cutoff alone. I did this to allow more candidates into the pool of potential murderers and avoid false negatives. 

# Classwt
# The classwt argument was used to further inform the model of the class imbalance. Previous literature on this topic did not reveal a scientific way of determining the appropriate weight; however, Berk et al utilized a 10:1 weight on the basis that it was acceptable to probation administrators. When trying a 10:1 class weight, the results were not any different than without the weighting. After reflecting on the purpose of the weight, I decided to attempt a weighting that mirrored the imbalance of the data. Since there were about 129 non-murderers for every murderer, I used that as the weighting. After some tests, I pulled back on the weight and settled on 125:1. 

# mtry
# The default number of variables randomly selected at each split is the square root of all variables. Therefore, as the total number of variables increases, the greater the number of variables tried at each split. In this case, the square root of 7 variables is about 2. Because of the low number of variables present, I adjusted this parameter to randomly select 1 variable at each split.

# Fit a second model to the data. 
fit2 <- randomForest(Murder ~ Age + Total_Pop + Black_Pop + Prop_Black + Income + Zip_Present + ViolentCase, 
                    data = train, 
                    importance = TRUE, 
                    ntree = 1500,
                    mtry = 1,
                    cutoff = c(0.65, 0.30),
                    classwt = c(125, 1),
                    sampsize = c("0" = 100, "1" = 34),
                    strata = as.factor(train$Murder),
                    keep.inbag = TRUE,
                    na.action = na.roughfix)

fit2

test$pred <- predict(fit2, test, cutoff = c(0.70, 0.30))  # Predicted outcome

# Add probabilities to the test set for further analysis
prob <- predict(fit, test, type = "prob")  # Probability matrix for both outcomes
test$prob_0 <- prob[,1]  # Probability of not being a murderer
test$prob_1 <- prob[,2]  # Probability of being a murderer

cm2 <- caret::confusionMatrix(table(test$pred, test$Murder), positive = "1")  # Produce a confusion matrix

# Calculate some false positive/negative metrics based on the confusion matrix
FN <- cm2$table[3]  # False Negatives
FP <- cm2$table[2]  # False Positives

cond_pos <- sum(cm2$table[,2])  # All Positive cases
cond_neg <- sum(cm2$table[,1])  # All Negative cases

FNR <- FN / cond_pos  # 0.2941176  False Negatives / All Positives
FPR <- FP / cond_neg  # 0.1043149  False Positives / All Negatives


# Model 2 Variable Importance ---------------------------------------------

varImpPlot(fit2)

# When looking at the specificity measure, when the model predicts an individual will not commit murder, it is correct nearly 89% of the time. Since we are concerned with identifying those who are more likely to display murderous behavior and intervening before murder is attempted/committed, it is crucial to minimize missed opportunities (False Negatives). 

# Show "importance" of variables: higher value mean more important:
round(importance(fit2), 2)

# Partial Dependence Plots
partialPlot(fit2, train, x.var = Age, which.class = 1)
partialPlot(fit2, train, x.var = Black_Pop, which.class = 1)

# Model 2 ROC -------------------------------------------------------------

# Another way to evaluate the effectiveness of the model is to analyze a ROC curve. This is done by plotting the True Positive Rate (Sensitivity) against the False Positive Rate. The Area Under the Curve (AUC) gives us a value of about 0.7. 

# To plot an ROC curve, we must order the factor variables that contain the reference and predicted values. 
test$Murder<- ordered(test$Murder)  # response variable
test$pred <- ordered(test$pred)  # predictor variable

# Create the ROC curve using pROC package.
ROC <- pROC::roc(response = test$Murder, predictor = test$pred, levels = levels(test$Murder))

# Plot the ROC curve and get the area under the curve. The area under the curve is 0.6944, which refers to 
pROC::plot.roc(ROC, col = rgb(1, 0, 0, .5), lwd = 2)  # AUC = 0.8091


# LSCMI Model -------------------------------------------------------------

# LSCMI has been the standard in the Probation Department for quite some time and is backed by extensive research. It is a case management tool that is used to assess the risk and level of service needed by probationers. It is not intended to predict whether or not someone will commit or attempt to commit murder. Instead, LSCMI is used to predict broad criminogenic behavior. Comparisons between random forest and LSCMI must keep in mind the difference in their purpose. 

# Copy the aps data to a new df
LSCMI <- aps %>% select(Murder, `LSCMI SCORE`)

LSCMI$grp <- ifelse(LSCMI$`LSCMI SCORE` >= 0 & LSCMI$`LSCMI SCORE`<=10, "Low",
                    ifelse(LSCMI$`LSCMI SCORE` > 10 & LSCMI$`LSCMI SCORE` <= 20, "Med",
                           ifelse(LSCMI$`LSCMI SCORE` > 20 & LSCMI$`LSCMI SCORE` <= 30, "High", "Ultra High")
                           )
                    )

LSCMI$grp <- factor(LSCMI$grp, ordered = TRUE, levels = c("Low", "Med", "High", "Ultra High"))

table(LSCMI$grp, LSCMI$Murder)  

# This contingency table shows that if High or Ultra High were used as a predictive model, there would be a high number of false positives, despite being able to predict 68% of murders. This means that more resources would need to be poured in to preventative programs because the LSCMI model is unable to accurately distinguish murderers from non-murderers. More importantly, there is an extremely high rate of false negatives that would go unflagged (32% when combining Low and Medium). Compare that to the random forest model which has less than 1% false negative rate. In other words, the LSCMI score is unable to efficiently predict without overcompensating. Results for the LSCMI method are worse when raising the threshold to Ultra High risk as the basis for identifying murderers because only about 6% of murderers were identified as Ultra High risk. 

# Logistic Regression Model -----------------------------------------------

# Logistic regression was the baseline for Berk's work. He found that this method yielded very little predictive power largely because this model favored the majority class. Below is my attempt to reproduce this method with LA County data.

logit <- glm(formula = Murder ~ Age + Total_Pop + Black_Pop + Prop_Black + Income + Gang + Zip_Present + ViolentCase, family = "binomial", data = train)

test2 <- test  # copy the test set 

# Use this model to find the predicted probability of each record in the test set.
test2$MurderP <- predict(logit, newdata = test2, type = "response")

# Determine if there are any with a probability of over 50%
any(test2$MurderP>0.5)  # there are none with a 50% or higher probability of being a murderer

# How many had a probability of 25% or higher?
length(which(test2$MurderP>0.25))  # 8 observations (15% of total murderers in test data)





