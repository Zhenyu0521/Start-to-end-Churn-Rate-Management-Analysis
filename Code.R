######################################## Setting up ################################################
# install.packages(c("glmnet","purrr"))
if (!exists("r_environment")) library(radiant)
library(purrr)
library(glmnet)
library(ROCR)

## Recall that Radiant stores all datasets in a list called r_data, if you are planning to use data
## transformation commands generated in Radiant uncomment the lines below and comment out the line above
r_data <- list()
r_data[["intuit75k_wrk"]] <- readr::read_rds(file.path(find_dropbox(), "MGTA455-2018/data/intuit75k.rds"))
intuit75k_wrk <- readr::read_rds(file.path(find_dropbox(), "MGTA455-2018/data/intuit75k.rds"))
# data pre-checking
# glimpse(head(intuit75k_wrk,10))
# map_df(intuit75k_wrk,function(x){any(is.na(x))})

# Break even rate
mailing_cost <- 1.41
net_rev <- 60
BE_resp_rate <- mailing_cost / net_rev

###################################### Sequential RFM model #########################################
# create sequential RFM index
intuit75k_wrk <- intuit75k_wrk %>%
  mutate(rec_sq = xtile(last,5)) %>%  # using time sice last oder from Intuit Direct as recency
  group_by(rec_sq) %>%
  mutate(freq_sq = xtile(numords,5, rev = T)) %>%
  group_by(rec_sq,freq_sq) %>%
  mutate(mon_sq = xtile(dollars, 5, rev = T),
         rfm_sq = paste0(rec_sq, freq_sq, mon_sq)) %>%
  group_by(rfm_sq) %>%
  mutate(prob_rfm = mean(res1 == "Yes"), mailto_rfm = (prob_rfm / 2 > BE_resp_rate) & (res1 == "No")) %>%
  ungroup()

# split training and test data set
training <- filter(intuit75k_wrk,training == 1)
test <- filter(intuit75k_wrk,training == 0)

# # Response Rate for each RFM group
# unique_rfm <- training %>%
#   filter(mailto_rfm == TRUE) %>%
#   select(rfm_sq) %>%
#   unique()
#
# # Response rate for each rfm group in Testing data
# test <- test %>%
#   inner_join(unique_rfm, by = "rfm_sq") #%>%
#   #filter(res1 == "No")

r_data[['test']] <- test

#################################### Logistic regression model #######################################
##### Full model with interaction

## build logistic regression model
result <- logistic(
  dataset = "training",
  rvar = "res1",
  evar = c("zip_bins", "sex", "bizflag",
    "numords", "dollars", "last", "sincepurch", "version1", "owntaxprod", "upgraded"
  ),
  lev = "Yes",
  int = "version1:upgraded"
)
summary(result)

## predict response probability lower bound and stored in new variable "log_resp_lb"
pred <- predict(result, pred_data = "test", conf_lev = 0.9, se = TRUE)
store(pred, data = "test", name = c("prob_log", "prob_log_lb","prob_log_ub"))

## create deciles for predicted resp_rate
test <- r_data$test
test <- mutate(test, mailto1_log = ifelse(prob_log > BE_resp_rate, TRUE, FALSE),
                     mailto2_log = ifelse(prob_log * 0.5 > BE_resp_rate, TRUE, FALSE),
                     dec_log = xtile(prob_log, 10, rev = TRUE))

## create deciles for predicted resp_rate lower bound
test <- mutate(test, mailto1_log_lb = ifelse(prob_log_lb > BE_resp_rate, TRUE, FALSE),
                     mailto2_log_lb = ifelse(prob_log_lb * 0.5 > BE_resp_rate, TRUE, FALSE),
                     dec_log_lb = xtile(prob_log_lb, 10, rev = TRUE))

##### confusion matrix and auc
## check mailto_sq levels and reorder levels if possible
#table(test$mailto2_log)
#table(test$mailto2_log_lb)
#test[["mailto2_log"]] <- factor(test[["mailto2_log"]], levels = c(TRUE, FALSE))
#test[["mailto2_log_lb"]] <- factor(test[["mailto2_log_lb"]], levels = c(TRUE, FALSE))

## create confusion matrix using res1 and mailto1
conf.mat <- table(test$res1, test$mailto1_log)
conf.mat_lb <- table(test$res1, test$mailto1_log_lb)

## calculate accuracy
acc_log <- (conf.mat[1,1] + conf.mat[2,2]) / sum(conf.mat)
acc_log_lb <- (conf.mat_lb[1,1] + conf.mat_lb[2,2]) / sum(conf.mat_lb)

cat('Model Accuracy =', acc_log)  ## 0.67
cat('Model Accuracy =', acc_log_lb) ## 0.62

##### Use new divided zipbins and rebuild up log model

## build logistic regression model: # zip_bins_new????
# result <- logistic(
#   dataset = "training",
#   rvar = "res1",
#   evar = c("zip_bins_new", "sex", "bizflag",
#     "numords", "dollars", "last", "sincepurch", "version1", "owntaxprod", "upgraded"
#   ),
#   lev = "Yes",
#   int = "version1:upgraded"
# )
# summary(result)
#
# ## predict response probability lower bound and stored in new variable "log_resp_lb"
# pred <- predict(result, pred_data = "test", conf_lev = 0.9, se = TRUE)
# store(pred, data = "test", name = c("resp_log_new", "resp_log_lb_new","resp_log_ub_new"))
#
# ## create deciles for predicted resp_rate
# test <- mutate(test, mailto2_log_new = ifelse(resp_log*0.5 > BE_resp_rate, TRUE, FALSE),
#                      dec_log_new = xtile(resp_log, 10, rev = TRUE))
#
# ## create deciles for predicted resp_rate lower bound
# test <- mutate(test, mailto2_log_lb_new = ifelse(resp_log_lb*0.5 > BE_resp_rate, TRUE, FALSE),
#                      dec_log_lb_new = xtile(resp_log_lb, 10, rev = TRUE))
#
# ##### recheck confusion matrix and auc
# ## check mailto_sq levels and reorder levels if possible
# table(test$mailto2_log_new)
# table(test$mailto2_log_lb_new)
# #test[["mailto2_log_new"]] <- factor(test[["mailto2_log_new"]], levels = c(TRUE, FALSE))
# #test[["mailto2_log_lb_new"]] <- factor(test[["mailto2_log_lb_new"]], levels = c(TRUE, FALSE))
#
# ## create confusion matrix
# conf.mat_new <- table(test$res1,test[["mailto2_log_new"]])
# conf.mat_lb_new <- table(test$res1,test[["mailto2_log_lb_new"]])
#
# ## calculate accuracy
# acc_log_new <- (conf.mat_new[1,1] + conf.mat_new[2,2]) / sum(conf.mat_new)
# acc_log_lb_new <- (conf.mat_lb_new[1,1] + conf.mat_lb_new[2,2]) / sum(conf.mat_lb_new)
#
# cat('Model Accuracy =', acc_log_new)  ## still 0.6456
# cat('Model Accuracy =', acc_log_lb_new) ## still 0.6924
#
# ##### check accuracy of prediction result
#
# ## generate sample bootstrap: cannot run!!!!!!!
# set.seed(1234)
# accuracy <- data.frame(matrix(0, 52500, 101))
# accuracy[, 1] <- training$id
# for(i in 1:100){
# sample <- sample_n(training, size = 52500, replace = TRUE)
# ## fit log model for each sample
#  result <- logistic(
#   dataset = sample,
#   rvar = "res1",
#   evar = c(
#     "zip_bins", "numords", "dollars", "last", "version1", "owntaxprod", "upgraded"
#   ),
#   lev = "Yes"
# )
# accuracy[, i] <- predict(result, pred_data = sample)
# }
#
# ## select 10th percentile of prediction as lower bound
# accuracy$log_lb <- apply(accuracy[,-1], 1, quantile(probs = 0.05))

##### Profit and ROME
# res_log <- perf_calc("mailto2_log", "Based on targeting,")
# profit_log <- res_log$profit
# ROME_log <- res_log$ROME
# cat(res_log$prn)
#
# ##### Lift and gains
# lift_log <- lift("dec_log")
# gains_log <- gains("dec_log", lift_log)

#################################### Naive bayes model #######################################
# Full model
result <- nb(
  dataset = "training",
  rvar = "res1",
  evar = c(
    "zip_bins", "sex", "bizflag", "numords", "dollars", "last",
    "sincepurch", "version1", "owntaxprod", "upgraded"
  )
)
summary(result)
plot(result)
pred <- predict(result, pred_data = "test")
store(pred, data = "test", name = "prob_nb")

# Create prob_nb and mailto_nb:
test <- r_data$test
test <- mutate(test, resp_nb = ifelse(prob_nb > BE_resp_rate, TRUE, FALSE),
                     mailto_nb = ifelse(((prob_nb / 2) > BE_resp_rate) & (res1 == "No"), TRUE, FALSE)) %>%
        ungroup()

# Compute model accuracy
# table(test$res1, test$resp_nb)
# (817 + 11711) / 22500 # 55.68%

# # Profit function
# perf_calc <- function(sms, intro) {
#   # no. of wave II mails to be sent to validation base
#   nr_mail <- sum(test[[sms]])
#   # estimate response rate on wave 2 based on the response rate in wave 1
#   rep_rate <- mean(test['res1'] == "Yes") * 0.5
#   # no. of customers responsed in 22.5k base
#   nr_resp <- nr_mail * rep_rate
#   mail_cost <- 1.41 * nr_mail
#   profit = 60 * nr_resp - mail_cost
#   ROME = profit / mail_cost
#
#   prn <- paste(intro, "the number of customers Intuit should send is", paste0(round(nr_mail, 1), "."),
#                "The response rate for the selected customers is predicted to be",
#                paste0(round(rep_rate*100, 2),"%"), "or", round(nr_resp), paste0("buyers", "."), "The expected profit is",
#                paste0("$", round(profit), "."), "The mail cost is estimated to be", paste0(round(mail_cost)), "with a ROME of",
#                paste0(round(ROME*100, 2), "%", "."))
#   results <- data.frame(profit, ROME, prn, rep_rate)
#   return(results)
# }
#
# # save result
# res <- perf_calc(sms = "mailto_nb", intro = "Based on Naive Bayes model")
# profit_nb <- res$profit
# ROME_nb <- res$ROME
# cat(res$prn)

# Tried to xtile binzip into 50/100, no much difference on model accuracy and profit

################################ Neural network with bootsrap ####################################
# it's a sample of neural network with bootstrap
# set.seed(1234)
# nn_result <- data.frame(test[["id"]])
# for (i in 1:5){
#   dat <- sample_n(training,52500,replace = TRUE)
#   result <- nn(
#     dataset = dat,
#     rvar = "res1",
#     evar = c(
#       "zip_bins", "numords", "dollars", "last", "sincepurch",
#       "version1", "owntaxprod", "upgraded"
#     ),
#     lev = "Yes",
#     seed = 1234
#   )
#   pred <- predict(result, pred_data = test)
#   store(pred, data = nn_result, name = paste0("predict_nn", i))
# this `store` line has something wrong with loop, need to fix
# }
nn_result <- readRDS("nn_result.rds")
test$prob_nn_lb <- nn_result$prob_nn_lb

# save test in radiant environment
r_data$test <- test

# Evaluation
result <- evalbin(
  dataset = "test",
  pred = c(
    "prob_rfm", "prob_log", "prob_log_lb", "prob_log_ub",
    "prob_nb", "prob_nn_lb"
  ),
  rvar = "res1",
  lev = "Yes",
  cost = 1.41,
  margin = 60,
  train = "All"
)
summary(result)
plot(result, plots = c("lift", "gains", "profit", "rome"), custom = TRUE) %>%
  gridExtra::grid.arrange(grobs = ., top = "Model evaluation", ncol = 2)

# Confusion matrix for different models
result <- confusion(
  dataset = "test",
  pred = c(
    "prob_rfm", "prob_log", "prob_log_lb", "prob_log_ub",
    "prob_nb", "prob_nn_lb"
  ),
  rvar = "res1",
  lev = "Yes",
  cost = 1.41,
  margin = 60,
  train = "All"
)
summary(result)
plot(result)
