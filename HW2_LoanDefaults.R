install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("rpart")

library(dplyr)
library(ggplot2)
library(tidyr)
library(rpart)

install.packages("data.table")
library(data.table)

install.packages("rpart.plot")
library(rpart.plot)

install.packages("magrittr")
library(magrittr)

library(tidyverse)
library(lubridate)

lcdf <- read_csv('/Users/pruthvinathjeripityvenkata/Desktop/UIC/Sem 3/IDS 572/HW/HW2/lcDataSampleFall22.csv')
lcdf <- as.data.frame(lcdf)

#----------------------------------------Data cleaning_________________________________________________________
lcdf$zip_code <- substr(lcdf$zip_code,1,3)
lcdf$zip_code <- as.numeric(lcdf$zip_code)

##
library(lubridate)
lcdf$earliest_cr_line<-paste(lcdf$earliest_cr_line, "-01", sep = "")
lcdf$earliest_cr_line<-parse_date_time(lcdf$earliest_cr_line,  "myd")

lcdf$last_credit_pull_d<-paste(lcdf$last_credit_pull_d, "-01", sep = "")
lcdf$last_credit_pull_d<-parse_date_time(lcdf$last_credit_pull_d,  "myd")

lcdf$verification_status <- ifelse(lcdf$verification_status == "Not Verified",0,1)

# ----------------------------------------------Exploration Data Analysis -----------------------------------------------
## QUESTION 2.a.i
loans <- lcdf %>% group_by(loan_status) %>% summarise(nLoans = n())
data_bar <- loans$nLoans
names(data_bar) <- loans$loan_status
barplot(data_bar, xlab = 'loan status', ylab='number of loans')

# Default rate calculate - Grade wise and Sub-Grade wise
defaults <- lcdf %>% group_by(grade) %>% summarise(nLoans = n(), defaults = sum(loan_status == "Charged Off"), default_rate = defaults/nLoans)
data_bar <- defaults$default_rate
names(data_bar) <- defaults$grade
barplot(data_bar, xlab = 'grade', ylab='default rate')

defaults2 <- lcdf %>% group_by(sub_grade) %>% summarise(nLoans = n(), defaults = sum(loan_status == "Charged Off"), default_rate = defaults/nLoans)
data_bar2 <- defaults2$default_rate
names(data_bar2) <- defaults2$sub_grade
barplot(data_bar2, xlab = 'sub-grade', ylab='default rate')

## QUESTION 2.a.ii
#Loans, loan amounts, interest rates
loan <- lcdf %>% group_by(grade) %>% summarise(nLoans = n(), loan_amt = sum(loan_amnt), avg_interest = mean(int_rate), 
                                               std = sd(int_rate), max = max(int_rate), min = min(int_rate))
interestRate_bar <- loan$max
names(interestRate_bar) <- loan$grade
barplot(interestRate_bar, xlab = 'grade', ylab = 'maximum interest rate')

loan_sub <- lcdf %>% group_by(sub_grade) %>% summarise(nLoans = n(), loan_amt = sum(loan_amnt), avg_interest = mean(int_rate), 
                                                       std = sd(int_rate), max = max(int_rate), min = min(int_rate))
interestRate_bar2 <- loan_sub$max
names(interestRate_bar2) <- loan_sub$sub_grade
barplot(interestRate_bar2, xlab = 'sub-grade', ylab = 'maximum interest rate')

# barplot for number of loans grade wise
loan  %>%
  ggplot(aes(grade,nLoans))+
  geom_col(position=position_dodge2(preserve = "single")) +
  labs(x="grade", y= "# of loans")

# barplot for loan amount grade wise
loan  %>%
  ggplot(aes(grade,loan_amt))+
  geom_col(position=position_dodge2(preserve = "single")) +
  labs(x="grade", y= "loan amount")

# QUESTION 2.a.iii
# convert issue date and last payment date into DATE data type


lcdf$last_pymnt_d<-paste(lcdf$last_pymnt_d, "-01", sep = "")
lcdf$last_pymnt_d<-parse_date_time(lcdf$last_pymnt_d,  "myd")
head(lcdf[, c("last_pymnt_d", "issue_d")])
lcdf$actualTerm <- ifelse(lcdf$loan_status=="Fully Paid", as.duration(lcdf$issue_d  %--% lcdf$last_pymnt_d)/dyears(1), 3)

fully_paid <- lcdf %>% filter(loan_status=="Fully Paid") %>%  select(grade, loan_status,actualTerm)

ggplot(fully_paid, aes(x=grade, y=actualTerm)) + 
  geom_boxplot()

#QUESTION 2.a.iv
# returns


#annualized percentage return
lcdf$annRet <- ((lcdf$total_pymnt -lcdf$funded_amnt)/lcdf$funded_amnt)*(12/36)*100
#for all loans returns - grade wise
all_loans <- lcdf %>% group_by(grade) %>% 
  summarise(nLoans=n(), avgInterest= mean(int_rate), avgLoanAmt=mean(loan_amnt), 
            avgPmnt=mean(total_pymnt), avgRet=mean(annRet),  minRet=min(annRet), maxRet=max(annRet))
all_loans_sub <- lcdf %>% group_by(sub_grade) %>% summarise(avgPmnt=mean(total_pymnt))
all_loans  %>%
  ggplot(aes(grade,avgPmnt))+
  geom_col(position=position_dodge2(preserve = "single")) +
  labs(x="grade", y= "avg payment")

#charged off returns
charged_off <- lcdf %>% filter( loan_status == "Charged Off") %>% group_by(grade) %>% 
  summarise(nLoans=n(), avgInterest= mean(int_rate), avgLoanAmt=mean(loan_amnt), 
            avgPmnt=mean(total_pymnt), avgRet=mean(annRet),  minRet=min(annRet), maxRet=max(annRet))

charged_off  %>%
  ggplot(aes(grade,avgPmnt))+
  geom_col(position=position_dodge2(preserve = "single")) +
  labs(x="grade", y= "avg payment by charged off loans")

# QUESTION 2.a.v
#purpose

purpose_hist <- as.data.frame(table(lcdf$purpose))
lcdf %>% group_by(purpose) %>% summarise(nLoans = n(), avgAmt = mean(funded_amnt), defaults = sum(loan_status == "Charged Off"))
table(lcdf$purpose, lcdf$grade) #purpose by grade



#QUESTION 2.a.vi
df <- lcdf %>% select(emp_length, annual_inc, loan_amnt, loan_status, grade, purpose, total_pymnt)
install.packages("ggcorrplot")
library(ggcorrplot)
model.matrix(~0+., data=df) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=2)

#QUESTION 2.b - Missing values
sapply(lcdf, function(lcdf) sum(is.na(lcdf))) #to find proportions of missing values with columns
which(colSums(is.na(lcdf))<1000) #to find which columns have missing values

#remove columns which are enitrely or almost null
lcdf <- lcdf %>% select(-c("id",  "member_id", "url", "desc", "next_pymnt_d", "annual_inc_joint","dti_joint", "verification_status_joint",
                               "open_acc_6m","open_act_il","open_il_12m","open_il_24m","mths_since_rcnt_il","total_bal_il","il_util","open_rv_12m", 
                               "open_rv_24m","max_bal_bc", "all_util", "inq_fi","total_cu_tl", "inq_last_12m","mths_since_recent_bc_dlq",
                               "mths_since_recent_revol_delinq", "revol_bal_joint","sec_app_earliest_cr_line","sec_app_inq_last_6mths",
                               "sec_app_mort_acc" , "sec_app_open_acc","sec_app_revol_util"  ,"sec_app_open_act_il",
                               "sec_app_num_rev_accts"  , "sec_app_chargeoff_within_12_mths","sec_app_collections_12_mths_ex_med" , 
                               "sec_app_mths_since_last_major_derog" ,"hardship_type","hardship_reason" , "hardship_status","deferral_term", "hardship_amount",                           
                               "hardship_start_date","hardship_end_date", "payment_plan_start_date","hardship_length", "hardship_dpd","hardship_loan_status",
                               "orig_projected_additional_accrued_interest", "hardship_payoff_balance_amount","hardship_last_payment_amount" ,
                               "debt_settlement_flag_date" ,"settlement_status","settlement_date" ,"settlement_amount"  , "settlement_percentage" , "settlement_term" 
                               ))

#impute appropriate values for certain columns 
lcdf$mths_since_last_delinq[is.na(lcdf$mths_since_last_delinq)] <- max(lcdf$mths_since_last_delinq,na.rm = TRUE)
lcdf$mths_since_last_major_derog[is.na(lcdf$mths_since_last_major_derog)] <- max(lcdf$mths_since_last_major_derog,na.rm = TRUE)
lcdf$mths_since_last_record[is.na(lcdf$mths_since_last_record)] <- max(lcdf$mths_since_last_record,na.rm = TRUE)
lcdf$mo_sin_old_il_acct[is.na(lcdf$mo_sin_old_il_acct)] <- max(lcdf$mo_sin_old_il_acct,na.rm = TRUE)
lcdf$mths_since_recent_bc[is.na(lcdf$mths_since_recent_bc)] <- max(lcdf$mths_since_recent_bc,na.rm = TRUE)
lcdf$mths_since_recent_inq[is.na(lcdf$mths_since_recent_inq)] <- max(lcdf$mths_since_recent_inq,na.rm = TRUE)
lcdf$num_tl_120dpd_2m[is.na(lcdf$num_tl_120dpd_2m)] <- 0
lcdf$percent_bc_gt_75[is.na(lcdf$percent_bc_gt_75)] <- 0
lcdf$bc_util[is.na(lcdf$bc_util)] <- 0
lcdf$revol_util[is.na(lcdf$revol_util)] <- 100
lcdf$bc_open_to_buy[is.na(lcdf$bc_open_to_buy)] <- 0


#----------------------------------- UNIVARIATE ANALYSIS ---------------------------------------------------------------------------------#
#QUESTION 3 - Univariate Analysis
#to consider variables which are probable good predictors of target variable
library(pROC)
aucAll<- sapply(lcdf %>% mutate_if(is.factor, as.numeric) %>% select_if(is.numeric), auc, response=lcdf$loan_status)
library(broom)
tidy(aucAll[aucAll > 0.5])

#----------------------------------- MODEL BUILDING: DECISION TREE --------------------------------------------------------------------------------------#
#QUESTION 5- Model Building
## 75% of the sample size
smp_size <- floor(0.75 * nrow(lcdf))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(lcdf)), size = smp_size)

lcdfTrn <- lcdf[train_ind, ]
lcdfTst <- lcdf[-train_ind, ]


lcdfTrn <- subset(lcdfTrn, select = -c(emp_title, title, actualTerm, annRet, total_pymnt,num_tl_op_past_12m, total_rec_late_fee, percent_bc_gt_75, num_actv_rev_tl, num_rev_tl_bal_gt_0,
            num_actv_bc_tl, pub_rec,num_accts_ever_120_pd, tot_coll_amt, pub_rec_bankruptcies, delinq_2yrs, num_tl_90g_dpd_24m, num_bc_sats, collections_12_mths_ex_med,
            tax_liens,acc_now_delinq, num_tl_30dpd, chargeoff_within_12_mths, delinq_amnt, out_prncp, out_prncp_inv, policy_code, num_tl_120dpd_2m,
            installment, num_sats, num_rev_accts, open_acc, num_il_tl, recoveries,collection_recovery_fee,total_rec_prncp,last_pymnt_amnt,
            total_pymnt_inv,total_pymnt,total_rec_int,funded_amnt_inv,funded_amnt,loan_amnt,total_acc,num_op_rev_tl, pymnt_plan, term, 
            total_bal_ex_mort,mths_since_last_major_derog,mths_since_last_record, debt_settlement_flag, last_pymnt_d, issue_d))

lcdfTst <- subset(lcdfTst, select = -c(emp_title, title, actualTerm, annRet, total_pymnt,num_tl_op_past_12m, total_rec_late_fee, percent_bc_gt_75, num_actv_rev_tl, num_rev_tl_bal_gt_0,
                                       num_actv_bc_tl, pub_rec,num_accts_ever_120_pd, tot_coll_amt, pub_rec_bankruptcies, delinq_2yrs, num_tl_90g_dpd_24m, num_bc_sats, collections_12_mths_ex_med,
                                       tax_liens,acc_now_delinq, num_tl_30dpd, chargeoff_within_12_mths, delinq_amnt, out_prncp, out_prncp_inv, policy_code, num_tl_120dpd_2m,
                                       installment, num_sats, num_rev_accts, open_acc, num_il_tl, recoveries,collection_recovery_fee,total_rec_prncp,last_pymnt_amnt,
                                       total_pymnt_inv,total_pymnt,total_rec_int,funded_amnt_inv,funded_amnt,loan_amnt,total_acc,num_op_rev_tl, pymnt_plan, term,
                                       total_bal_ex_mort,mths_since_last_major_derog,mths_since_last_record, debt_settlement_flag, last_pymnt_d, issue_d))
lcdfTrn <- subset(lcdfTrn, select = -c(hardship_flag, disbursement_method))
lcdfTst <- subset(lcdfTst, select = -c(hardship_flag, disbursement_method))

lcdfTrn_part3 <- lcdfTrn %>% filter(grade %in% c("C", "D", "E", "F", "G"))
lcdfTst_part3 <- lcdfTst %>% filter(grade %in% c("C", "D", "E", "F", "G"))

#----------------------------------------------------------------------------------------------------------------------------------------
lcDT1 <- rpart(loan_status ~., data=lcdfTrn, method="class", parms = list(split = "information"), control = rpart.control(minsplit = 30, maxdepth =  10))
lcDT1 <- rpart(loan_status ~., data=lcdfTrn, method="class", parms = list(split = "gini"), control = rpart.control(minsplit = 30, maxdepth =  10))

printcp(lcDT1)

library(RColorBrewer)
install.packages("rattle")
library(rattle)
plotcp(lcDT1) # visualize cross-validation results 

lcDT1$variable.importance
lcDT1 <- rpart(loan_status ~., data=lcdfTrn, method="class", 
               parms = list(split = "gini"), control = rpart.control(cp=0.010000, minsplit = 70))
#imp variables
lcDT1$variable.importance

#Evaluate performance
predTrn=predict(lcDT1,lcdfTrn, type='class')
table(pred = predTrn, true=lcdfTrn$loan_status)
mean(predTrn == lcdfTrn$loan_status)


table(pred = predict(lcDT1,lcdfTst, type='class'), true=lcdfTst$loan_status)
mean(predict(lcDT1,lcdfTst, type='class') ==lcdfTst$loan_status)


#In our case, cost of 2 types of wrong predictions are not equal.
# cost of classifying a charged off loan as a fully paid loan >>> cost of classifying a fully paid loan as a charged off loan
# so we have to change threshold to classify more efficiently (i.e. threshold shouldn't be at 50%)

# CTHRESH=0.3
# predProbTrn=predict(lcDT1,lcdfTrn, type='prob')
# predTrnCT = ifelse(predProbTrn[, 'Charged Off'] > CTHRESH, 'Charged Off', 'Fully Paid')
# library(caret)
# confusionMatrix(predTrn, lcdfTrn$loan_status)

#----------------------------------- MODEL BUILDING: RANDOM FOREST ---------------------------------------------------------------------#
# QUESTION 6 - RANDOM FOREST AND XGB

install.packages("doParallel")
library(doParallel)
library(randomForest)

lcdfTrn <- na.omit(lcdfTrn)
lcdfTst <- na.omit(lcdfTst)

#converting all character columns to factors
fact_col <- colnames(lcdfTrn)[sapply(lcdfTrn,is.character)]
for(i in fact_col)
  set(lcdfTrn,j=i,value = factor(lcdfTrn[[i]]))
for(i in fact_col)
  set(lcdfTrn,j=i,value = factor(lcdfTrn[[i]]))

fact_col <- colnames(lcdfTst)[sapply(lcdfTst,is.character)]
for(i in fact_col)
  set(lcdfTst,j=i,value = factor(lcdfTst[[i]]))
for(i in fact_col)
  set(lcdfTst,j=i,value = factor(lcdfTst[[i]]))

#mismatch of levels in subgrades
lcdfTrn<-subset(lcdfTrn, sub_grade != "G4")
lcdfTrn$sub_grade <- droplevels(lcdfTrn$sub_grade)

lcdfTst<-subset(lcdfTst, sub_grade != "G4")
lcdfTst$sub_grade <- droplevels(lcdfTst$sub_grade)

#converting Poxcit date format to number of years numeric
lcdfTrn$earliest_cr_line <- trunc((lcdfTrn$earliest_cr_line %--% Sys.Date()) / years(1))
lcdfTrn$last_credit_pull_d <- trunc((lcdfTrn$last_credit_pull_d %--% Sys.Date()) / years(1))

lcdfTst$earliest_cr_line <- trunc((lcdfTst$earliest_cr_line %--% Sys.Date()) / years(1))
lcdfTst$last_credit_pull_d <- trunc((lcdfTst$last_credit_pull_d %--% Sys.Date()) / years(1))
# lcdfTrn$earliest_cr_line <- as.factor(as.Date.factor(lcdfTrn$earliest_cr_line))
# lcdfTrn$last_credit_pull_d <- as.factor(as.Date.factor(lcdfTrn$last_credit_pull_d))
# 
# lcdfTst$earliest_cr_line <- as.factor(as.Date.factor(lcdfTst$earliest_cr_line))
# lcdfTst$last_credit_pull_d <- as.factor(as.Date.factor(lcdfTst$last_credit_pull_d))

library(randomForest)
set.seed(71)
rf <-randomForest(loan_status~.,data=lcdfTrn, ntree=500) 
print(rf)

#Select mtry value with minimum out of bag(OOB) error
mtry <- tuneRF(lcdfTrn[ , -which(names(lcdfTrn) %in% c("loan_status"))], lcdfTrn$loan_status, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
  best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

#Build model again using best mtry value
set.seed(71)
rf <-randomForest(loan_status~.,data=lcdfTrn, mtry=best.m, importance=TRUE,ntree=500)
print(rf)
#Evaluate variable importance
importance(rf)
varImpPlot(rf)

#Prediction and performance metrics
pred1=predict(rf,type = "prob")
library(ROCR)
perf = prediction(pred1[,2], lcdfTrn$loan_status)
# 1. Area under curve
auc <-  performance(perf, "auc")
auc
# 2. True Positive and Negative Rate
pred3 = performance(perf, "tpr","fpr")
# 3. Plot the ROC curve
plot(pred3,main="ROC Curve for Random Forest",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")



#Evaluate performance
predTrn=predict(rf,lcdfTrn, type='class')
table(pred = predTrn, true=lcdfTrn$loan_status)
mean(predTrn == lcdfTrn$loan_status)

table(pred = predict(rf,lcdfTst, type='class'), true=lcdfTst$loan_status)
mean(predict(rf,lcdfTst, type='class') ==lcdfTst$loan_status)

#--------------- QUESTION 7 ---------------------------------------------#
#Question 7 #Random Forest
#Obtain the model's predictions on the training data
#Code
library(ranger)
PROFITVAL<- 6
COSTVAL<- 6

#Performance
rfModel1 <- ranger(loan_status~.,data=lcdfTrn, num.trees=200,importance='permutation',probability=TRUE)

#collection_recovery_fee,total_rec_int,hardship_flag
#lcdftst=lcdf[-trainIndex,]
scoreTstRF <- predict(rfModel1,lcdfTst,type="response")$prediction[,"Fully Paid"]
prPerfRF <- data.frame(scoreTstRF)
prPerfRF <- cbind(prPerfRF, status=lcdfTst$loan_status)
prPerfRF <- prPerfRF[order(-scoreTstRF) ,] #sort in desc order of prob(fully_paid)
prPerfRF$profit <- ifelse(prPerfRF$status == 'Fully Paid', PROFITVAL, COSTVAL)
prPerfRF$cumProfit <- cumsum(prPerfRF$profit)
max(prPerfRF$cumProfit)
prPerfRF$cumProfit[which.max(prPerfRF$cumProfit)]

#Confussion matrix for Random Forest
predTrn=predict(rfModel1,lcdfTrn, type='response')
table(pred = predTrn$predictions[ , "Fully Paid"], true=lcdfTrn$loan_status)
table(pred = predTrn$predictions[ , "Fully Paid"] >0.5, true=lcdfTrn$loan_status)
table(pred = predTrn$predictions[ , "Fully Paid"] >0.6, true=lcdfTrn$loan_status)
table(pred = predTrn$predictions[ , "Fully Paid"] >0.7, true=lcdfTrn$loan_status)
table(pred = predTrn$predictions[ , "Fully Paid"] >0.9, true=lcdfTrn$loan_status)
head(predTrn$predictions[ , "Fully Paid"])
vimpRg1 <- ranger::importance(rfModel1)
vimpRg1
tidy(vimpRg1) %>% view()
#----------------------------------------------------------------------------------------
#----------------------------------------- XGBoost for Loan status ------------------------------------------------------#
# creating dummies with one hot encoding
install.packages("xgboost")
install.packages("caret")
library(xgboost)
library(caret)
library(Matrix)
library(magrittr)
require(dplyr)

lcdfTrn <- lcdfTrn %>% relocate(loan_status, .before = int_rate)
lcdfTst <- lcdfTst %>% relocate(loan_status, .before = int_rate)


lcdfTrn$loan_status <- ifelse(lcdfTrn$loan_status == "Charged Off", 1, 0)
lcdfTst$loan_status <- ifelse(lcdfTst$loan_status == "Charged Off", 1, 0)

#Create a matrix and OneHot Encoding for factor variables
options(na.action='na.pass')
trainm <- sparse.model.matrix(loan_status ~ .-1, data = lcdfTrn)
train_label <- lcdfTrn[, "loan_status"]
train_matrix <- xgb.DMatrix(data = as.matrix(trainm), label = train_label)

testm <- sparse.model.matrix(loan_status ~ .-1, data = lcdfTst)
test_label <- lcdfTst[, "loan_status"]
test_matrix <- xgb.DMatrix(data = as.matrix(testm), label = test_label)

#Parameters
nc <- length(unique(train_label))
xgb_params <- list("objective" = "multi:softprob",
                   "eval_metric" = "mlogloss",
                   "num_class" = nc)
watchlist <- list(train = train_matrix, test = test_matrix)

#XGBoost Model
bst_model <- xgb.train(params = xgb_params, 
                       data = train_matrix,
                       nrounds = 100,
                       watchlist = watchlist)


#training and test error plot
e <- data.frame(bst_model$evaluation_log)
plot(e$iter, e$train_mlogloss, col = 'blue')
lines(e$iter, e$test_mlogloss, col = 'red')
e[e$test_mlogloss == min(e$test_mlogloss), ]


#tuning the model
bst_model <- xgb.train(params = xgb_params, 
                       data = train_matrix,
                       nrounds = 26,
                       watchlist = watchlist,
                       eta = 0.05
                       )


#tuning the model much more
bst_model <- xgb.train(params = xgb_params, 
                       data = train_matrix,
                       nrounds = 116,
                       watchlist = watchlist,
                       eta = 0.06,
                       max.depth = 8,
                       gamma = 0.3,
                       subsample = 1,
                       colsample_bytree = 1,
                       missing = NA,
                       set.seed(333))

#Feature importance
imp <- xgb.importance(colnames(train_matrix), model = bst_model)
print(imp)
xgb.plot.importance(imp[1:20,])

#prediction and confusion matrix for train data
p1 <- predict(bst_model, newdata = train_matrix)
pred1 <- matrix(p1, nrow = nc, ncol = length(p1)/nc) %>% 
  t() %>% data.frame() %>% mutate(label = train_label, max_prob = max.col(., "last")-1)
table_mat_train <- table(Prediction = pred1$max_prob, Actual = pred1$label)
accuracy_Train <- sum(diag(table_mat_train)) / sum(table_mat_train)
print(paste('Accuracy for train', accuracy_Train))

#prediction and confusion matrix for test data
p <- predict(bst_model, newdata = test_matrix)
pred <- matrix(p, nrow = nc, ncol = length(p)/nc) %>% 
  t() %>% data.frame() %>% mutate(label = test_label, max_prob = max.col(., "last")-1)
table_mat_test <- table(Prediction = pred$max_prob, Actual = pred$label)
accuracy_Test <- sum(diag(table_mat_test)) / sum(table_mat_test)
print(paste('Accuracy for test', accuracy_Test))

#--------------------------------------------------------------------------------------------------------------------------------#
# END OF MODELS FOR PREDICTING LOAN STATUS
#--------------------------------------------------------------------------------------------------------------------------------#

# MODELS DOR PREDICTING RETURNS START
#--------------------------------------------------------------------------------------------------------------------------------#

#calculate the annualized percentage return (%)
lcdf$annRet <- ((lcdf$total_pymnt_inv -lcdf$funded_amnt_inv)/lcdf$funded_amnt_inv)*(12/36)*100

#calculate term of the loan (in years)
lcdf$actualTerm <- ifelse(lcdf$loan_status=="Fully Paid", as.duration(lcdf$issue_d  %--% lcdf$last_pymnt_d)/dyears(1), 3)

#Then, considering this actual term, the actual annual return (%)
lcdf$actualReturn <- ifelse(lcdf$actualTerm>0, ((lcdf$total_pymnt_inv -lcdf$funded_amnt_inv)/lcdf$funded_amnt_inv)*(1/lcdf$actualTerm)*100, 0)

#take a look these variables for the first few rows of data 
lcdf %>% select(loan_status, int_rate, funded_amnt_inv, total_pymnt_inv, annRet, actualTerm, actualReturn) %>%  head()

#This summary can also help understand:
lcdf%>% group_by(loan_status) %>% summarise(intRate=mean(int_rate), totRet=mean((total_pymnt_inv - funded_amnt_inv)/funded_amnt_inv), 
                                            avgActRet=mean(actualReturn))

#Returns from 'Fully Paid' loans
lcdf %>% filter( loan_status == "Fully Paid") %>% group_by(grade) %>% 
  summarise(nLoans=n(), avgInterest= mean(int_rate), avgFundedAmt=mean(funded_amnt_inv), avgPmnt=mean(total_pymnt_inv), 
            avgActuRet=mean(actualReturn), minActRet=min(actualReturn), maxActRet=max(actualReturn))

#Similarly, returns from 'Charged Off" loans
lcdf %>% filter( loan_status == "Charged Off") %>% group_by(grade) %>% 
  summarise(nLoans=n(), avgInterest= mean(int_rate), avgLoanAmt=mean(loan_amnt), avgPmnt=mean(total_pymnt), 
            avgRet=mean(annRet), minRet=min(annRet), maxRet=max(annRet))


# Prediction class: for fully paid loans - predict actualReturn; for charged off loans - predict annRet -- build two models 
# so unseen data should be subjected to predict loanStatus first and based on the predicted loan status, the unseen data then subjected to 
# one of the models to predict either actualReturn or annRet. 

#But for the sake of simplicity in assignment, we are building single model for both fully paid and charged off loans.

## 75% of the sample size
smp_size_ret <- floor(0.75 * nrow(lcdf))

## set the seed to make your partition reproducible and data cleaning for FULLY PAID
set.seed(145)
train_ind_ret <- sample(seq_len(nrow(lcdf)), size = smp_size_ret)

lcdfTrn_ret <- lcdf[train_ind_ret, ]
lcdfTst_ret <- lcdf[-train_ind_ret, ]

lcdfTrn_ret <- subset(lcdfTrn_ret, select = -c(emp_title, title, actualTerm, annRet, total_pymnt,num_tl_op_past_12m, total_rec_late_fee, percent_bc_gt_75, num_actv_rev_tl, num_rev_tl_bal_gt_0,
                                       num_actv_bc_tl, pub_rec,num_accts_ever_120_pd, tot_coll_amt, pub_rec_bankruptcies, delinq_2yrs, num_tl_90g_dpd_24m, num_bc_sats, collections_12_mths_ex_med,
                                       tax_liens,acc_now_delinq, num_tl_30dpd, chargeoff_within_12_mths, delinq_amnt, out_prncp, out_prncp_inv, policy_code, num_tl_120dpd_2m,
                                       installment, num_sats, num_rev_accts, open_acc, num_il_tl, recoveries,collection_recovery_fee,total_rec_prncp,last_pymnt_amnt,
                                       total_pymnt_inv,total_pymnt,total_rec_int,funded_amnt_inv,funded_amnt,loan_amnt,total_acc,num_op_rev_tl, pymnt_plan, term, 
                                       total_bal_ex_mort,mths_since_last_major_derog,mths_since_last_record, debt_settlement_flag, last_pymnt_d, issue_d))

lcdfTst_ret <- subset(lcdfTst_ret, select = -c(emp_title, title, actualTerm, annRet, total_pymnt,num_tl_op_past_12m, total_rec_late_fee, percent_bc_gt_75, num_actv_rev_tl, num_rev_tl_bal_gt_0,
                                       num_actv_bc_tl, pub_rec,num_accts_ever_120_pd, tot_coll_amt, pub_rec_bankruptcies, delinq_2yrs, num_tl_90g_dpd_24m, num_bc_sats, collections_12_mths_ex_med,
                                       tax_liens,acc_now_delinq, num_tl_30dpd, chargeoff_within_12_mths, delinq_amnt, out_prncp, out_prncp_inv, policy_code, num_tl_120dpd_2m,
                                       installment, num_sats, num_rev_accts, open_acc, num_il_tl, recoveries,collection_recovery_fee,total_rec_prncp,last_pymnt_amnt,
                                       total_pymnt_inv,total_pymnt,total_rec_int,funded_amnt_inv,funded_amnt,loan_amnt,total_acc,num_op_rev_tl, pymnt_plan, term,
                                       total_bal_ex_mort,mths_since_last_major_derog,mths_since_last_record, debt_settlement_flag, last_pymnt_d, issue_d))

lcdfTrn_ret <- subset(lcdfTrn_ret, select = -c(hardship_flag, disbursement_method))
lcdfTst_ret <- subset(lcdfTst_ret, select = -c(hardship_flag, disbursement_method))

lcdfTrn_ret <- subset(lcdfTrn_ret, select = -c(loan_status))
lcdfTst_ret <- subset(lcdfTst_ret, select = -c(loan_status))

#-------------------------------------------------- DECISION TREE FOR PREDICTING ACUTAL RETURNS -----------------------------------------------------------------------------------------#
lc_ret_DT1 <- rpart(actualReturn ~., data=lcdfTrn_ret, method="anova")

printcp(lc_ret_DT1)

plotcp(lc_ret_DT1) # visualize cross-validation results 

lc_ret_DT1$variable.importance
rpart.plot(lc_ret_DT1)

#Behind the scenes rpart is automatically applying a range of cost complexity (α) values to prune the tree. 
#To compare the error for each α value, rpart performs a 10-fold cross validation so that the error associated with a 
#given α value is computed on the hold-out validation data

#after involving control parameters
lc_ret_DT2 <- rpart(actualReturn ~., data=lcdfTrn_ret, method="anova", 
              control =  list(cp = 0.005, xval = 10))
plotcp(lc_ret_DT2)
lc_ret_DT2$cptable

# #for tuning the parameters and search for the best combination
# hyper_grid <- expand.grid(
#   minsplit = seq(5, 20, 1),
#   maxdepth = seq(8, 15, 1)
# )
# # total number of combinations - number of models are gonna be compared and picked the best
# nrow(hyper_grid)
# 
# models <- list()
# 
# for (i in 1:nrow(hyper_grid)) {
#   
#   # get minsplit, maxdepth values at row i
#   minsplit <- hyper_grid$minsplit[i]
#   maxdepth <- hyper_grid$maxdepth[i]
#   
#   # train a model and store in the list
#   models[[i]] <- rpart(
#     formula = actualReturn ~ .,
#     data    = lcdfTrn_ret,
#     method  = "anova",
#     control = list(minsplit = minsplit, maxdepth = maxdepth)
#   )
# }
# 
# # function to get optimal cp
# get_cp <- function(x) {
#   min <- which.min(x$cptable[, "xerror"])
#   cp <- x$cptable[min, "CP"] 
# }
# 
# # function to get minimum error
# get_min_error <- function(x) {
#   min <- which.min(x$cptable[, "xerror"])
#   xerror <- x$cptable[min, "xerror"] 
# }
# 
# hyper_grid %>%
#   mutate(
#     cp    = purrr::map_dbl(models, get_cp),
#     error = purrr::map_dbl(models, get_min_error)
#   ) %>%
#   arrange(error) %>%
#   top_n(-5, wt = error)

#optimal tree - from the above commented code, I got optimal values
optimal_tree <- rpart(
  formula = actualReturn ~ .,
  data    = lcdfTrn_ret,
  method  = "anova",
  control = list(minsplit = 17, maxdepth = 14, cp = 0.01)
)

#Evaluate performance on training and testing data
pred <- predict(optimal_tree, newdata = lcdfTrn_ret)
RMSE(pred = pred, obs = lcdfTrn_ret$actualReturn)

pred <- predict(optimal_tree, newdata = lcdfTst_ret)
RMSE(pred = pred, obs = lcdfTst_ret$actualReturn)

# plot most important variables
as.data.frame(optimal_tree$variable.importance)


#------------------------------------------ GENERALISED LINEAR MODEL (Linear Regression) ---------------------------------------------------------#
# Linear regression: y = b0 + sum_of(bi * f(xi)), where b0 is intercept, bi are coefficients and xi are numerical variables

#but this data set contains categorical variables also but we need all numerical variables for linear regression. 
#So those need to convert them into factors, so that R can create dummy variables automatically.

#converting all character columns to factors
fact_col <- colnames(lcdfTrn_ret)[sapply(lcdfTrn_ret,is.character)]
for(i in fact_col)
  set(lcdfTrn_ret,j=i,value = factor(lcdfTrn_ret[[i]]))
for(i in fact_col)
  set(lcdfTrn_ret,j=i,value = factor(lcdfTrn_ret[[i]]))

fact_col <- colnames(lcdfTst_ret)[sapply(lcdfTst_ret,is.character)]
for(i in fact_col)
  set(lcdfTst_ret,j=i,value = factor(lcdfTst_ret[[i]]))
for(i in fact_col)
  set(lcdfTst_ret,j=i,value = factor(lcdfTst_ret[[i]]))

lcdfTrn_ret<-subset(lcdfTrn_ret, sub_grade != "G4")
lcdfTrn_ret$sub_grade <- droplevels(lcdfTrn_ret$sub_grade)

lcdfTst_ret<-subset(lcdfTst_ret, sub_grade != "G4")
lcdfTst_ret$sub_grade <- droplevels(lcdfTst_ret$sub_grade)

#converting Poxcit date format to number of years numeric
lcdfTrn_ret$earliest_cr_line <- trunc((lcdfTrn_ret$earliest_cr_line %--% Sys.Date()) / years(1))
lcdfTrn_ret$last_credit_pull_d <- trunc((lcdfTrn_ret$last_credit_pull_d %--% Sys.Date()) / years(1))

lcdfTst_ret$earliest_cr_line <- trunc((lcdfTst_ret$earliest_cr_line %--% Sys.Date()) / years(1))
lcdfTst_ret$last_credit_pull_d <- trunc((lcdfTst_ret$last_credit_pull_d %--% Sys.Date()) / years(1))

#EDA on continuous variables
continuous <-select_if(lcdfTrn_ret, is.numeric)
summary(continuous)

# Histogram with kernel density curve
ggplot(continuous, aes(x = actualReturn)) +
  geom_density(alpha = .2, fill = "#FF6666")

#to handle the outliers
top_one_percent <- quantile(lcdfTrn_ret$actualReturn, .99)
top_one_percent

lcdfTrn_ret <- lcdfTrn_ret %>% #dropping the top one percentile
  filter(actualReturn < top_one_percent)
dim(lcdfTrn_ret)

# #standardize the numerical data to get better performance
# lcdfTrn_ret <- lcdfTrn_ret %>%
#   mutate_if(is.numeric, funs(as.numeric(scale(.))))
# 
# lcdfTst_ret <- lcdfTst_ret %>%
#   mutate_if(is.numeric, funs(as.numeric(scale(.))))

#EDA on categorical variables
# Select categorical column
factor <- data.frame(select_if(lcdfTrn_ret, is.factor))


# Create graph for each column
graph <- lapply(names(factor),
                function(x) 
                  ggplot(factor, aes(get(x))) +
                  geom_bar() +
                  theme(axis.text.x = element_text(angle = 90)))

#mutate levels of employment length
lcdfTrn_ret <- lcdfTrn_ret %>%
  mutate(emp_length = factor(emp_length, levels = c("< 1 year", "1 year", "2 years","3 years","4 years","5 years","6 years","7 years","8 years","9 years", "10+ years"), 
                             labels = c('< 1 year', '1-5 years','1-5 years', '1-5 years', '1-5 years','1-5 years', '6-9 years','6-9 years','6-9 years','6-9 years','10+ years' ))) %>% 
  na.omit()

lcdfTrn_ret <- lcdfTrn_ret %>%
  mutate(purpose = factor(purpose, levels = c("car","credit_card","debt_consolidation", "home_improvement","house", "major_purchase","medical",
                                              "moving","other", "renewable_energy","small_business","vacation","wedding" ), 
                          labels = c("home goods", "debt", "debt", "home goods","home goods","home goods", "medical", 
                                     "home goods", "other", "business", "business", "other","other"))) %>% 
  na.omit()

lcdfTrn_ret$addr_state <- state.region[match(lcdfTrn_ret$addr_state,state.abb)]


#mutate testing data as well to equate factor levels
lcdfTst_ret <- lcdfTst_ret %>%
  mutate(emp_length = factor(emp_length, levels = c("< 1 year", "1 year", "2 years","3 years","4 years","5 years","6 years","7 years","8 years","9 years", "10+ years"), 
                             labels = c('< 1 year', '1-5 years','1-5 years', '1-5 years', '1-5 years','1-5 years', '6-9 years','6-9 years','6-9 years','6-9 years','10+ years' ))) %>% 
  na.omit()

lcdfTst_ret <- lcdfTst_ret %>%
  mutate(purpose = factor(purpose, levels = c("car","credit_card","debt_consolidation", "home_improvement","house", "major_purchase","medical",
                                              "moving","other", "renewable_energy","small_business","vacation","wedding" ), 
                          labels = c("home goods", "debt", "debt", "home goods","home goods","home goods", "medical", 
                                     "home goods", "other", "business", "business", "other","other"))) %>% 
  na.omit()

lcdfTst_ret$addr_state <- state.region[match(lcdfTst_ret$addr_state,state.abb)]
lcdfTrn_ret <- na.omit(lcdfTrn_ret)
lcdfTst_ret <- na.omit(lcdfTst_ret)

lcdfTst_ret$annual_inc <- as.numeric(lcdfTst_ret$annual_inc) 
lcdfTst_ret$zip_code <- as.numeric(lcdfTst_ret$zip_code) 
lcdfTst_ret$dti <- as.numeric(lcdfTst_ret$dti) 
lcdfTst_ret$tot_cur_bal <- as.numeric(lcdfTst_ret$tot_cur_bal) 



model.matrix(~0+., data=lcdfTrn_ret) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=2)

# devtools::install_github("laresbernardo/lares")
install.packages("lares")
library(lares)

corr_cross(lcdfTrn_ret, # name of dataset
           max_pvalue = 0.05, # display only significant correlations (at 5% level)
           top = 10 # display top 10 couples of variables (by correlation coefficient)
)

#Model building
#'actual returns' column data is not normally distributed => we have to use GLM model
# Generalized Linear Model is a collection of models

base.mod <- glm(actualReturn ~ 1 , data= lcdfTrn_ret) # base intercept only model

all.mod <- glm(actualReturn ~ . , data= lcdfTrn_ret) # full model with all predictors

stepMod <- step(base.mod, scope = list(lower = base.mod, upper = all.mod), direction = "both", trace = 1, steps = 100) # perform step-wise algorithm

shortlistedVars <- names(unlist(stepMod[[1]])) # get the shortlisted variable.

shortlistedVars <- shortlistedVars[!shortlistedVars %in% "(Intercept)"] # remove intercept
print(shortlistedVars)

summary(all.mod)

#--------------------------------------- XGBoost MODEL TO PREDICT ACTUAL RETURNS -------------------------------------#

library(xgboost)
library(caret)

#relocating actualReturn to the first column
lcdfTrn_ret <- lcdfTrn_ret %>% relocate(actualReturn, .before = int_rate)
lcdfTst_ret <- lcdfTst_ret %>% relocate(actualReturn, .before = int_rate)

#Preparing the data
train_x = data.matrix(lcdfTrn_ret[,-1])
train_y = lcdfTrn_ret$actualReturn

test_x = data.matrix(lcdfTst_ret[,-1])
test_y = lcdfTst_ret$actualReturn

xgb_train = xgb.DMatrix(data = train_x, label = train_y)
xgb_test = xgb.DMatrix(data = test_x, label = test_y)

#Fitting the model and prediction
xgbc = xgboost(data = xgb_train, max.depth = 5, nrounds = 60)
print(xgbc)


pred_y_trn = predict(xgbc, xgb_train)

#Checking accuracy
mse = mean((train_y - pred_y_trn)^2)
mae = caret::MAE(train_y, pred_y_trn)
rmse = caret::RMSE(train_y, pred_y_trn)
cat("MSE_trn: ", mse, "MAE_trn: ", mae, " RMSE_trn: ", rmse)

pred_y_test = predict(xgbc, xgb_test)

#Checking accuracy
mse = mean((test_y - pred_y_test)^2)
mae = caret::MAE(test_y, pred_y_test)
rmse = caret::RMSE(test_y, pred_y_test)

cat("MSE_test: ", mse, "MAE_test: ", mae, " RMSE_test: ", rmse)

# Compute feature importance matrix
importance_matrix = xgb.importance(colnames(xgb_train), model = xgbc)
importance_matrix
# Nice graph
xgb.plot.importance(importance_matrix[1:10,])

#--------------------------------------- RANDOM FOREST TO PREDICT ACTUAL RETURNS -------------------------------------#
library(randomForest)
library(ggplot2)
library(ranger)
library(caret)

library(ranger)
library(rsample)
library(ROSE)

lcdfTrn_ret <- na.omit(lcdfTrn_ret)
#training the RF model
rfModel_Ret <- ranger(actualReturn ~., data=lcdfTrn_ret, mtry = 11, num.trees =320,importance='impurity', min.node.size = 3,
                      max.depth = 30)

#predicting the actual return on training set and testing set
rfPredRet_trn <- predict(rfModel_Ret, lcdfTrn_ret)
rfPredRet_tst <- predict(rfModel_Ret, lcdfTst_ret)

#calculating RMSE
rmse_trn <- sqrt(mean((rfPredRet_trn$predictions - lcdfTrn_ret$actualReturn)^2))
print(rmse_trn)
rmse_tst <- sqrt(mean((rfPredRet_tst$predictions - lcdfTst_ret$actualReturn)^2))
print(rmse_tst)

plot ((predict(rfModel_Ret, lcdfTrn_ret))$predictions, lcdfTrn_ret$actualReturn)
plot ((predict(rfModel_Ret, lcdfTst_ret))$predictions, lcdfTst_ret$actualReturn)


#Build model again using best mtry value
set.seed(71)
rfModel_Ret <-ranger(actualReturn ~., data=lcdfTrn_ret, mtry = 11, num.trees =320,importance='impurity')
print(rfModel_Ret)
#Evaluate variable importance
importance(rfModel_Ret)



#------------------------------------------------ QUESTION 10 Part B for low grade loans ------------------------------------------------------------------------------------#
# higher grade loans are less likely to default, but also carry lower interest rates; 
# many lower grade loans are fully paid, and these can yield higher returns. 
# Considering this, one approach to making investment decisions may be to focus on lower grade loans (C and below) 
# and try to identify those which are likely to be paid off. 

#we have to take only rows which have grades C, D, E, F and G and then perform every other model in similar manner
#-----------------------------------------
#DECISION TREE
#-----------------------------------------
lcdfTrn <- lcdfTrn_part3
lcdfTst <- lcdfTst_part3

lcDT1 <- rpart(loan_status ~., data=lcdfTrn, method="class", parms = list(split = "information"), control = rpart.control(minsplit = 30, maxdepth =  10))
lcDT1 <- rpart(loan_status ~., data=lcdfTrn, method="class", parms = list(split = "gini"), control = rpart.control(minsplit = 30, maxdepth =  10))

printcp(lcDT1)

library(RColorBrewer)
library(rattle)
plotcp(lcDT1) # visualize cross-validation results 

lcDT1$variable.importance
lcDT1 <- rpart(loan_status ~., data=lcdfTrn, method="class", 
               parms = list(split = "gini"), control = rpart.control(cp=0.010000, minsplit = 70))
#imp variables
lcDT1$variable.importance

#Evaluate performance
predTrn=predict(lcDT1,lcdfTrn, type='class')
table(pred = predTrn, true=lcdfTrn$loan_status)
mean(predTrn == lcdfTrn$loan_status)


table(pred = predict(lcDT1,lcdfTst, type='class'), true=lcdfTst$loan_status)
mean(predict(lcDT1,lcdfTst, type='class') ==lcdfTst$loan_status)


#----------------------------------------------
#RANDOM FOREST FOR LOW GRADE LOANS ESTIMATION
#----------------------------------------------
lcdfTrn <- na.omit(lcdfTrn)
lcdfTst <- na.omit(lcdfTst)

#converting all character columns to factors
fact_col <- colnames(lcdfTrn)[sapply(lcdfTrn,is.character)]
for(i in fact_col)
  set(lcdfTrn,j=i,value = factor(lcdfTrn[[i]]))
for(i in fact_col)
  set(lcdfTrn,j=i,value = factor(lcdfTrn[[i]]))

fact_col <- colnames(lcdfTst)[sapply(lcdfTst,is.character)]
for(i in fact_col)
  set(lcdfTst,j=i,value = factor(lcdfTst[[i]]))
for(i in fact_col)
  set(lcdfTst,j=i,value = factor(lcdfTst[[i]]))

#mismatch of levels in subgrades
lcdfTrn<-subset(lcdfTrn, sub_grade != "G4")
lcdfTrn$sub_grade <- droplevels(lcdfTrn$sub_grade)

lcdfTst<-subset(lcdfTst, sub_grade != "G4")
lcdfTst$sub_grade <- droplevels(lcdfTst$sub_grade)

#converting Poxcit date format to number of years numeric
lcdfTrn$earliest_cr_line <- trunc((lcdfTrn$earliest_cr_line %--% Sys.Date()) / years(1))
lcdfTrn$last_credit_pull_d <- trunc((lcdfTrn$last_credit_pull_d %--% Sys.Date()) / years(1))

lcdfTst$earliest_cr_line <- trunc((lcdfTst$earliest_cr_line %--% Sys.Date()) / years(1))
lcdfTst$last_credit_pull_d <- trunc((lcdfTst$last_credit_pull_d %--% Sys.Date()) / years(1))
# lcdfTrn$earliest_cr_line <- as.factor(as.Date.factor(lcdfTrn$earliest_cr_line))
# lcdfTrn$last_credit_pull_d <- as.factor(as.Date.factor(lcdfTrn$last_credit_pull_d))
# 
# lcdfTst$earliest_cr_line <- as.factor(as.Date.factor(lcdfTst$earliest_cr_line))
# lcdfTst$last_credit_pull_d <- as.factor(as.Date.factor(lcdfTst$last_credit_pull_d))

library(randomForest)
set.seed(81)
rf <-randomForest(loan_status~.,data=lcdfTrn, ntree=500) 
print(rf)

#Select mtry value with minimum out of bag(OOB) error
mtry <- tuneRF(lcdfTrn[ , -which(names(lcdfTrn) %in% c("loan_status"))], lcdfTrn$loan_status, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

#Build model again using best mtry value
set.seed(71)
rf <-randomForest(loan_status~.,data=lcdfTrn, mtry=best.m, importance=TRUE,ntree=500)
print(rf)
#Evaluate variable importance
importance(rf)
varImpPlot(rf)

#Prediction and performance metrics
pred1=predict(rf,type = "prob")
library(ROCR)
perf = prediction(pred1[,2], lcdfTrn$loan_status)
# 1. Area under curve
auc <-  performance(perf, "auc")
auc
# 2. True Positive and Negative Rate
pred3 = performance(perf, "tpr","fpr")
# 3. Plot the ROC curve
plot(pred3,main="ROC Curve for Random Forest",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")



#Evaluate performance
predTrn=predict(rf,lcdfTrn, type='class')
table(pred = predTrn, true=lcdfTrn$loan_status)
mean(predTrn == lcdfTrn$loan_status)

table(pred = predict(rf,lcdfTst, type='class'), true=lcdfTst$loan_status)
mean(predict(rf,lcdfTst, type='class') ==lcdfTst$loan_status)

#--------------------------------
#XGBoost Model
#--------------------------------

lcdfTrn <- lcdfTrn %>% relocate(loan_status, .before = int_rate)
lcdfTst <- lcdfTst %>% relocate(loan_status, .before = int_rate)


lcdfTrn$loan_status <- ifelse(lcdfTrn$loan_status == "Charged Off", 1, 0)
lcdfTst$loan_status <- ifelse(lcdfTst$loan_status == "Charged Off", 1, 0)

#Create a matrix and OneHot Encoding for factor variables
options(na.action='na.pass')
trainm <- sparse.model.matrix(loan_status ~ .-1, data = lcdfTrn)
train_label <- lcdfTrn[, "loan_status"]
train_matrix <- xgb.DMatrix(data = as.matrix(trainm), label = train_label)

testm <- sparse.model.matrix(loan_status ~ .-1, data = lcdfTst)
test_label <- lcdfTst[, "loan_status"]
test_matrix <- xgb.DMatrix(data = as.matrix(testm), label = test_label)

#Parameters
nc <- length(unique(train_label))
xgb_params <- list("objective" = "multi:softprob",
                   "eval_metric" = "mlogloss",
                   "num_class" = nc)
watchlist <- list(train = train_matrix, test = test_matrix)

#XGBoost Model
bst_model <- xgb.train(params = xgb_params, 
                       data = train_matrix,
                       nrounds = 100,
                       watchlist = watchlist)


#training and test error plot
e <- data.frame(bst_model$evaluation_log)
plot(e$iter, e$train_mlogloss, col = 'blue')
lines(e$iter, e$test_mlogloss, col = 'red')
e[e$test_mlogloss == min(e$test_mlogloss), ]


#tuning the model
bst_model <- xgb.train(params = xgb_params, 
                       data = train_matrix,
                       nrounds = 21,
                       watchlist = watchlist,
                       eta = 0.05
)


#tuning the model much more
bst_model <- xgb.train(params = xgb_params, 
                       data = train_matrix,
                       nrounds = 116,
                       watchlist = watchlist,
                       eta = 0.06,
                       max.depth = 8,
                       gamma = 0.3,
                       subsample = 1,
                       colsample_bytree = 1,
                       missing = NA,
                       set.seed(333))

#Feature importance
imp <- xgb.importance(colnames(train_matrix), model = bst_model)
print(imp)
xgb.plot.importance(imp[1:20,])

#prediction and confusion matrix for train data
p1 <- predict(bst_model, newdata = train_matrix)
pred1 <- matrix(p1, nrow = nc, ncol = length(p1)/nc) %>% 
  t() %>% data.frame() %>% mutate(label = train_label, max_prob = max.col(., "last")-1)
table_mat_train <- table(Prediction = pred1$max_prob, Actual = pred1$label)
accuracy_Train <- sum(diag(table_mat_train)) / sum(table_mat_train)
print(paste('Accuracy for train', accuracy_Train))

#prediction and confusion matrix for test data
p <- predict(bst_model, newdata = test_matrix)
pred <- matrix(p, nrow = nc, ncol = length(p)/nc) %>% 
  t() %>% data.frame() %>% mutate(label = test_label, max_prob = max.col(., "last")-1)
table_mat_test <- table(Prediction = pred$max_prob, Actual = pred$label)
accuracy_Test <- sum(diag(table_mat_test)) / sum(table_mat_test)
print(paste('Accuracy for test', accuracy_Test))














