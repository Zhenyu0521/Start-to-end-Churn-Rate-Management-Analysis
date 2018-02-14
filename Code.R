######################################## Setting up ################################################
# install.packages(c("glmnet","purrr","ROCR"))
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

intuit75k_wrk$zip_bins <- as.factor(intuit75k_wrk$zip_bins)

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
  mutate(prob_rfm = mean(res1 == "Yes")) %>%
  ungroup()

# split training and test data set
r_data[['training']] <- filter(intuit75k_wrk,training == 1)
r_data[['test']] <- filter(intuit75k_wrk,training == 0)

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

#################################### Logistic regression model #######################################

# 1. Build logistic regression model
## Full model
result <- logistic(
  dataset = "training",
  rvar = "res1",
  evar = c("zip_bins", "sex", "bizflag",
    "numords", "dollars", "last", "sincepurch", "version1", "owntaxprod", "upgraded"
  ),
  lev = "Yes"
)
summary(result)
pred <- predict(result, pred_data = "test", conf_lev = 0.9, se = TRUE)
store(pred, data = "test", name = c("prob_logit", "prob_logit_lb","prob_logit_ub"))

## Model without sex
result <- logistic(
  dataset = "training",
  rvar = "res1",
  evar = c("zip_bins", "bizflag",
           "numords", "dollars", "last", "sincepurch", "version1", "owntaxprod", "upgraded"
  ),
  lev = "Yes"
)
summary(result)
pred <- predict(result, pred_data = "test")
store(pred, data = "test", name = "prob_logit_ns")

## Model without bizflag
result <- logistic(
  dataset = "training",
  rvar = "res1",
  evar = c("zip_bins", "sex",
           "numords", "dollars", "last", "sincepurch", "version1", "owntaxprod", "upgraded"
  ),
  lev = "Yes"
)
summary(result)
pred <- predict(result, pred_data = "test")
store(pred, data = "test", name = "prob_logit_nbf")

## Model without sincepurch
result <- logistic(
  dataset = "training",
  rvar = "res1",
  evar = c("zip_bins", "sex", "bisflag",
           "numords", "dollars", "last", "version1", "owntaxprod", "upgraded"
  ),
  lev = "Yes"
)
summary(result)
pred <- predict(result, pred_data = "test")
store(pred, data = "test", name = "prob_logit_nsp")


## Model without sex and bizflag
result <- logistic(
  dataset = "training",
  rvar = "res1",
  evar = c("zip_bins",
           "numords", "dollars", "last", "sincepurch", "version1", "owntaxprod", "upgraded"
  ),
  lev = "Yes"
)
summary(result)
pred <- predict(result, pred_data = "test")
store(pred, data = "test", name = "prob_logit_nsbf")

## Model without sex and sincepurch
result <- logistic(
  dataset = "training",
  rvar = "res1",
  evar = c("zip_bins",
           "numords", "dollars", "last", "sincepurch", "version1", "owntaxprod", "upgraded"
  ),
  lev = "Yes"
)
summary(result)
pred <- predict(result, pred_data = "test")
store(pred, data = "test", name = "prob_logit_nssp")

## Model without bizflag and sincepurch
result <- logistic(
  dataset = "training",
  rvar = "res1",
  evar = c("zip_bins",
           "numords", "dollars", "last", "version1", "owntaxprod", "upgraded"
  ),
  lev = "Yes"
)
summary(result)
pred <- predict(result, pred_data = "test")
store(pred, data = "test", name = "prob_logit_nbfsp")

## Model without sex, bizflag and sincepurch
result <- logistic(
  dataset = "training",
  rvar = "res1",
  evar = c("zip_bins",
           "numords", "dollars", "last", "version1", "owntaxprod", "upgraded"
  ),
  lev = "Yes"
)
summary(result)
pred <- predict(result, pred_data = "test")
store(pred, data = "test", name = "prob_logit_nsbfsp")


# 2. Evaluation of lift, gains, profit and ROME
result <- evalbin(
  dataset = "test",
  pred = c(
    "prob_logit", "prob_logit_lb",
    "prob_logit_ns", "prob_logit_nbf", "prob_logit_nsp", "prob_logit_nsbf", "prob_logit_nssp", "prob_logit_nbfsp", "prob_logit_nsbfsp"
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

# 3. Confusion matrix for different models
result <- confusion(
  dataset = "test",
  pred = c(
    "prob_logit", "prob_logit_lb",
    "prob_logit_ns", "prob_logit_nbf", "prob_logit_nsp", "prob_logit_nsbf", "prob_logit_nssp", "prob_logit_nbfsp", "prob_logit_nsbfsp"
  ),
  rvar = "res1",
  lev = "Yes",
  cost = 1.41,
  margin = 60,
  train = "All"
)
summary(result)
plot(result)


## creating mailto columns

r_data[["test"]] <- r_data[["test"]] %>%
  mutate(mailto_logit = factor(ifelse(prob_logit > BE_resp_rate, T, F),levels = c(T, F)),
         mailto_logit_lb = factor(ifelse(prob_logit_lb > BE_resp_rate, T, F),levels = c(T, F)),
         mailto_logit_ns = factor(ifelse(prob_logit_ns > BE_resp_rate, T, F),levels = c(T, F)),
         mailto_logit_nbf = factor(ifelse(prob_logit_nbf > BE_resp_rate, T, F),levels = c(T, F)),
         mailto_logit_nsp = factor(ifelse(prob_logit_nsp > BE_resp_rate, T, F),levels = c(T, F)),
         mailto_logit_nsbf = factor(ifelse(prob_logit_nsbf > BE_resp_rate, T, F),levels = c(T, F)),
         mailto_logit_nssp = factor(ifelse(prob_logit_nssp > BE_resp_rate, T, F),levels = c(T, F)),
         mailto_logit_nbfsp = factor(ifelse(prob_logit_nbfsp > BE_resp_rate, T, F),levels = c(T, F)),
         mailto_logit_nsbfsp = factor(ifelse(prob_logit_nsbfsp > BE_resp_rate, T, F),levels = c(T, F))
  )


## confusion matrix and auc
test <- r_data[["test"]]
### create confusion matrix using res1 and mailto
conf.mat_logit <- table(test$res1, test$mailto_logit)
conf.mat_logit_lb <- table(test$res1, test$mailto_logit_lb)
conf.mat_logit_ns <- table(test$res1, test$mailto_logit_ns)
conf.mat_logit_nbf <- table(test$res1, test$mailto_logit_nbf)
conf.mat_logit_nsp <- table(test$res1, test$mailto_logit_nsp)
conf.mat_logit_nsbf <- table(test$res1, test$mailto_logit_nsbf)
conf.mat_logit_nssp <- table(test$res1, test$mailto_logit_nssp)
conf.mat_logit_nbfsp <- table(test$res1, test$mailto_logit_nbfsp)
conf.mat_logit_nsbfsp <- table(test$res1, test$mailto_logit_nsbfsp)

### calculate accuracy
acc_logit <- (conf.mat_logit[1,1] + conf.mat_logit[2,2]) / sum(conf.mat_logit)
acc_logit_lb <- (conf.mat_logit_lb[1,1] + conf.mat_logit_lb[2,2]) / sum(conf.mat_logit_lb)
acc_logit_ns <- (conf.mat_logit_ns[1,1] + conf.mat_logit_ns[2,2]) / sum(conf.mat_logit_ns)
acc_logit_nbf <- (conf.mat_logit_nbf[1,1] + conf.mat_logit_nbf[2,2]) / sum(conf.mat_logit_nbf)
acc_logit_nsp <- (conf.mat_logit_nsp[1,1] + conf.mat_logit_nsp[2,2]) / sum(conf.mat_logit_nsp)
acc_logit_nbf <- (conf.mat_logit_nbf[1,1] + conf.mat_logit_nbf[2,2]) / sum(conf.mat_logit_nbf)
acc_logit_nsbf <- (conf.mat_logit_nsbf[1,1] + conf.mat_logit_nsbf[2,2]) / sum(conf.mat_logit_nsbf)
acc_logit_nssp <- (conf.mat_logit_nssp[1,1] + conf.mat_logit_nssp[2,2]) / sum(conf.mat_logit_nssp)
acc_logit_nbfsp <- (conf.mat_logit_nbfsp[1,1] + conf.mat_logit_nbfsp[2,2]) / sum(conf.mat_logit_nbfsp)
acc_logit_nsbfsp <- (conf.mat_logit_nsbfsp[1,1] + conf.mat_logit_nsbfsp[2,2]) / sum(conf.mat_logit_nsbfsp)

### omparison with radiant
data.frame(
  hand_clac = c(acc_logit,acc_logit_lb,acc_logit_ns,acc_logit_nbf,acc_logit_nsp, acc_logit_nsbf, acc_logit_nssp, acc_logit_nbfsp, acc_logit_nsbfsp),
  radiant = result$dat$accuracy
)



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
pred <- predict(result, pred_data = "test")
store(pred, data = "test", name = "prob_nb")

# full - bizflag
result <- nb(
  dataset = "training",
  rvar = "res1",
  evar = c(
    "zip_bins", "sex", "numords", "dollars", "last",
    "sincepurch", "version1", "owntaxprod", "upgraded"
  )
)
summary(result)
pred <- predict(result, pred_data = "test")
store(pred, data = "test", name = "prob_nb_nobiz")

# full - biz - sex
result <- nb(
  dataset = "training",
  rvar = "res1",
  evar = c(
    "zip_bins", "numords", "dollars", "last",
    "sincepurch", "version1", "owntaxprod", "upgraded"
  )
)
summary(result)
pred <- predict(result, pred_data = "test")
store(pred, data = "test", name = "prob_nb_nobiz_sex")

# Full model - sex
result <- nb(
  dataset = "training",
  rvar = "res1",
  evar = c(
    "zip_bins", "bizflag", "numords", "dollars", "last",
    "sincepurch", "version1", "owntaxprod", "upgraded"
  )
)
summary(result)
pred <- predict(result, pred_data = "test")
store(pred, data = "test", name = "prob_nb_nosex")

# Evaluation
result <- evalbin(
  dataset = "test",
  pred = c(
    "prob_nb", "prob_nb_nobiz", "prob_nb_nobiz_sex",
    "prob_nb_nosex"
  ),
  rvar = "res1",
  lev = "Yes",
  cost = 1.41,
  margin = 60,
  train = "All"
)
summary(result)
plot(result, plots = c("lift", "gains", "profit", "rome"), custom = TRUE) %>%
  gridExtra::grid.arrange(grobs = ., top = "Model evaluation", ncol = 2) # no much difference

result <- confusion(
  dataset = "test",
  pred = c(
    "prob_nb", "prob_nb_nobiz", "prob_nb_nobiz_sex",
    "prob_nb_nosex"
  ),
  rvar = "res1",
  lev = "Yes",
  cost = 1.41,
  margin = 60,
  train = "All"
)
summary(result)
# prob_nb_nosex is slightly better on profit, so we choose prob_nb_nosex as the best estimator among naive bayes

################################ Neural network with bootsrap ####################################

#----------------------------------------- Run on server------------------------------------------
# set.seed(1234)
# nn_result <- data.frame(matrix(NA, nrow = 22500, ncol = 101))
# nn_result[[1]] <- test[["id"]]
# for (i in 1:100){
#   dat <- sample_n(training,52500,replace = TRUE)
#   result <- nn(
#     dataset = test,
#     rvar = "res1",
#     evar = c(
#       "zip_bins", "numords", "dollars", "last", "sincepurch",
#       "version1", "owntaxprod", "upgraded"
#     ),
#     lev = "Yes",
#     seed = 1234
#   )
#   nn_result[[i+1]] <- predict(result, pred_data = test)$Prediction
# }
#
# nn_result$prob_nn_lb <- apply(nn_result[,2:101],1,quantile,probs = 0.05)
# nn_result <- nn_result %>% select(id = X1, prob_nn_lb)
#-------------------------------------------------------------------------------------------------

nn_result <- readRDS("nn_result.rds")
r_data[['test']]$prob_nn_lb <- nn_result$prob_nn_lb

########################################## Evaluation ############################################
# Evaluation
result <- evalbin(
  dataset = "test",
  pred = c(
    "prob_rfm", "prob_logit", "prob_logit_lb",
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
    "prob_rfm", "prob_logit", "prob_logit_lb",
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


# ## creating mailto columns

r_data[["test"]] <- r_data[["test"]] %>%
  mutate(mailto_rfm = factor(ifelse(prob_rfm > BE_resp_rate, T, F),levels = c(T, F)),
         mailto_logit = factor(ifelse(prob_logit > BE_resp_rate, T, F),levels = c(T, F)),
         mailto_logit_lb = factor(ifelse(prob_logit_lb > BE_resp_rate, T, F),levels = c(T, F)),
         mailto_nb = factor(ifelse(prob_nb > BE_resp_rate, T, F),levels = c(T, F)),
         mailto_nn_lb = factor(ifelse(prob_nn_lb > BE_resp_rate, T, F),levels = c(T, F))
  )


##### confusion matrix and auc
test <- r_data[["test"]]
# create confusion matrix using res1 and mailto
conf.mat_rfm <- table(test$res1, test$mailto_rfm)
conf.mat_logit <- table(test$res1, test$mailto_logit)
conf.mat_logit_lb <- table(test$res1, test$mailto_logit_lb)
conf.mat_nb <- table(test$res1, test$mailto_nb)
conf.mat_nn_lb <- table(test$res1, test$mailto_nn_lb)

# calculate accuracy
acc_rfm <- (conf.mat_rfm[1,1] + conf.mat_rfm[2,2]) / sum(conf.mat_rfm)
acc_logit <- (conf.mat_logit[1,1] + conf.mat_logit[2,2]) / sum(conf.mat_logit)
acc_logit_lb <- (conf.mat_logit_lb[1,1] + conf.mat_logit_lb[2,2]) / sum(conf.mat_logit_lb)
acc_nb <- (conf.mat_nb[1,1] + conf.mat_nb[2,2]) / sum(conf.mat_nb)
acc_nn_lb <- (conf.mat_nn_lb[1,1] + conf.mat_nn_lb[2,2]) / sum(conf.mat_nn_lb)

#comparison with radiant
data.frame(
hand_clac = c(acc_rfm,acc_logit,acc_logit_lb,acc_nb,acc_nn_lb),
radiant = result$dat$accuracy
)
