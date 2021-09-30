#title: "Homework 3 lab 3"
#author: "KIM FAI CHAN"
#date: "9/27/2021"

setwd("~/Desktop/jeff's documents/Master of Economics in CCNY/ECONOMETRICS ECO B2000/homework 3 lab 3")
load("acs2017_ny_data.RData")

#For the k-nn classification, it's decided by personal Income Total, Housing Cost, the population was limited in NYC with the age between 21 and 65.
dat_NYC <- subset(acs2017_ny, (acs2017_ny$in_NYC == 1)&(acs2017_ny$AGE > 20) & (acs2017_ny$AGE < 66))
attach(dat_NYC)
borough_f <- factor((in_Bronx + 2*in_Manhattan + 3*in_StatenI + 4*in_Brooklyn + 5*in_Queens), levels=c(1,2,3,4,5),labels = c("Bronx","Manhattan","Staten Island","Brooklyn","Queens"))

norm_varb <- function(X_in) {
  (X_in - min(X_in, na.rm = TRUE))/( max(X_in, na.rm = TRUE) - min(X_in, na.rm = TRUE) )
}
is.na(OWNCOST) <- which(OWNCOST == 9999999)
housing_cost <- OWNCOST + RENT
norm_inc_tot <- norm_varb(INCTOT)
norm_housing_cost <- norm_varb(housing_cost)

data_use_prelim <- data.frame(norm_inc_tot,norm_housing_cost)
good_obs_data_use <- complete.cases(data_use_prelim,borough_f)
dat_use <- subset(data_use_prelim,good_obs_data_use)
y_use <- subset(borough_f,good_obs_data_use)

set.seed(12345)
NN_obs <- sum(good_obs_data_use == 1)
select1 <- (runif(NN_obs) < 0.8)
train_data <- subset(dat_use,select1)
test_data <- subset(dat_use,(!select1))
cl_data <- y_use[select1]
true_data <- y_use[!select1]
summary(cl_data)

#after summary then below info pops up:
#Bronx     Manhattan Staten Island      Brooklyn        Queens 
#4820          5164          1892         12395         10955 

prop.table(summary(cl_data))
#> prop.table(summary(cl_data))
#Bronx     Manhattan Staten Island      Brooklyn        Queens 
#0.13683075    0.14659626    0.05371033    0.35187078    0.31099188 

summary(train_data)
#norm_inc_tot     norm_housing_cost
#Min.   :0.00000   Min.   :0.00000  
#1st Qu.:0.01184   1st Qu.:0.02478  
#Median :0.02693   Median :0.96898  
#Mean   :0.04249   Mean   :0.58675  
#3rd Qu.:0.05219   3rd Qu.:0.97784  
#Max.   :1.00000   Max.   :1.00000 

require(class)
for (indx in seq(1, 9, by= 2)) {
  pred_borough <- knn(train_data, test_data, cl_data, k = indx, l = 0, prob = FALSE, use.all = TRUE)
  num_correct_labels <- sum(pred_borough == true_data)
  correct_rate <- num_correct_labels/length(true_data)
  print(c(indx,correct_rate))
}
#[1] 1.0000000 0.3457318
#[1] 3.0000000 0.3482731
#[1] 5.0000000 0.3595934
#[1] 7.0000000 0.3684879
#[1] 9.0000000 0.3726464

#The results are: The algorithm could be more accurate when k = 9. And higher the values of k, the higher the accurate. 
#It is clear that as k increases after 1, the quality of algorithm to correctly classify the observations increases. knowing more about variables allows us to refine the algorithm and the accuracy could become higher on classifying the test data. However, it seems more variables, the algorithm become more confused on toward difference direction.


cl_data_n <- as.numeric(cl_data)
model_ols1 <- lm(cl_data_n ~ train_data$norm_inc_tot + train_data$norm_housing_cost)
y_hat <- fitted.values(model_ols1)

mean(y_hat[cl_data_n == 1])
#[1] 3.485385
mean(y_hat[cl_data_n == 2])
#[1] 3.380373
mean(y_hat[cl_data_n == 3])
#[1] 3.762961
mean(y_hat[cl_data_n == 4])
#[1] 3.548343
mean(y_hat[cl_data_n == 5])
#[1] 3.635049

cl_data_n1 <- as.numeric(cl_data_n == 1)
model_ols_v1 <- lm(cl_data_n1 ~ train_data$norm_inc_tot + train_data$norm_housing_cost)
y_hat_v1 <- fitted.values(model_ols_v1)

mean(y_hat_v1[cl_data_n1 == 1])
#[1] 0.1575926
mean(y_hat_v1[cl_data_n1 == 0])
#[1] 0.1335395


#The trade off between classifying the training data and the test data comes from:  

# what point do I think a trade off between better classifying the training data and doing worse at classifying the test data?:  
# a. how deep we could understand on the data;
# b. do I have the knowledge to identify the key useful variables in minimum level?
# c. do I have the ability to identify which are un-useful observations?

# Finally, use adequate useful classified observations to train the algorithm and keep a large enough test set to test the algorithm.  

# However in my understanding this would much more depends on the bias of observer,
# and this is decided by the knowledge of the observer to the data set.


