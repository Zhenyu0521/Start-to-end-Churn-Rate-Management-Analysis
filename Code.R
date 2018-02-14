######################################## Setting up ################################################
# install.packages(c("glmnet","purrr","ROCR"))
if (!exists("r_environment")) library(radiant)
library(purrr)
library(glmnet)
library(ROCR)

## Recall that Radiant stores all datasets in a list called r_data, if you are planning to use data
## transformation commands generated in Radiant uncomment the lines below and comment out the line above
intuit75k_wrk <- readr::read_rds(file.path(find_dropbox(), "MGTA455-2018/data/intuit75k.rds"))

# add zipbins to 50 bins
intuit75k_wrk$zip_bins <- as.factor(intuit75k_wrk$zip_bins)
intuit75k_wrk$zip_bins_50 <- as.factor(xtile(as.numeric(intuit75k_wrk$zip),50))

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
r_data <- list()
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
  evar = c("zip_bins", "sex", "bizflag",
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


# 2. AUC, profit, ROME checking for different models
result <- confusion(
  dataset = "test",
  pred = c(
    "prob_logit", "prob_logit_lb",
    "prob_logit_ns", "prob_logit_nbf", "prob_logit_nsp",
    "prob_logit_nsbf", "prob_logit_nssp", "prob_logit_nbfsp", "prob_logit_nsbfsp"
  ),
  rvar = "res1",
  lev = "Yes",
  cost = 1.41,
  margin = 60,
  train = "All"
)
eval_logit <- summary(result)

# 3. Final model without bizflag adding interaction
result <- logistic(
  dataset = "training",
  rvar = "res1",
  evar = c("zip_bins", "sex",
           "numords", "dollars", "last", "sincepurch", "version1", "owntaxprod", "upgraded"
  ),
  lev = "Yes",
  int = "version1:upgraded"
)
summary(result)
pred <- predict(result, pred_data = "test")
store(pred, data = "test", name = "prob_logit_nbf_in")

result <- confusion(
  dataset = "test",
  pred = c(
    "prob_logit", "prob_logit_lb",
    "prob_logit_ns", "prob_logit_nbf", "prob_logit_nsp",
    "prob_logit_nsbf", "prob_logit_nssp", "prob_logit_nbfsp", "prob_logit_nsbfsp", "prob_logit_nbf_in"
  ),
  rvar = "res1",
  lev = "Yes",
  cost = 1.41,
  margin = 60,
  train = "All"
)

summary(result)


#################################### Naive bayes model #######################################
# 1. Build Naive bayes model

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
store(pred, data = "test", name = "prob_nb_nobiz_nosex")

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

# 2. AUC, profit, ROME checking for different models

result <- confusion(
  dataset = "test",
  pred = c(
    "prob_nb", "prob_nb_nobiz", "prob_nb_nobiz_nosex",
    "prob_nb_nosex"
  ),
  rvar = "res1",
  lev = "Yes",
  cost = 1.41,
  margin = 60,
  train = "All"
)
eval_nb <- summary(result)

# prob_nb_nobiz is slightly better on profit, so we choose prob_nb_nobiz as the best estimator
# among naive bayes. Also we want to add a zip_bins_50

# full - bizflag -
result <- nb(
  dataset = "training",
  rvar = "res1",
  evar = c(
    "zip_bins_50", "sex", "numords", "dollars", "last",
    "sincepurch", "version1", "owntaxprod", "upgraded"
  )
)
summary(result)
pred <- predict(result, pred_data = "test")
store(pred, data = "test", name = "prob_nb_nobiz_zip50")





################################ Neural network with bootsrap ####################################

#----------------------------------------- Run on server------------------------------------------

## 1. choose the best size & decay combination

# for (i in 1:5){
#   size = i
#   for (j in 1:6) {
#     decay = j/2
#     result <- nn(
#       dataset = "training",
#       rvar = "res1",
#       evar = c(
#         "zip_bins","sex","bizflag", "numords", "dollars", "last", "sincepurch",
#         "version1", "owntaxprod", "upgraded"),
#       decay = decay,
#       size = size,
#       lev = "Yes",
#       seed = 1234)
#     pred <- predict(result, pred_data = "intuit75k_wrk")
#     store(pred, data = "intuit75k_wrk", name = paste("nn",i,j/2,sep="_"))
#   }
# }
#
# result <- confusion(
#   dataset = "intuit75k_wrk",
#   pred = colnames(r_data[["intuit75k_wrk"]])[15:43],
#   rvar = "res1",
#   lev = "Yes",
#   cost = 1.41,
#   margin = 60,
#   train = "All"
# )

## 2. use size = 5 & decay = 0.5 as the best combination to predict with bootstrap

# set.seed(1234)
# nn_result <- data.frame(matrix(NA, nrow = 22500, ncol = 101))
# nn_result[[1]] <- test[["id"]]
# for (i in 1:100){
#   dat <- sample_n(training,52500,replace = TRUE)
#   result <- nn(
#     dataset = dat,
#     rvar = "res1",
#     evar = c(
#       "zip_bins_50", "sex","bizflag","numords", "dollars", "last", "sincepurch",
#       "version1", "owntaxprod", "upgraded"
#     ),
#     size = 5,
#     decay = 0.5,
#     lev = "Yes",
#     seed = 1234
#   )
#   nn_result[[i+1]] <- predict(result, pred_data = test)$Prediction
# }
#
# nn_result$prob_nn_lb <- apply(nn_result[,2:101],1,quantile,probs = 0.05)
# nn_result <- nn_result %>% select(id = X1, prob_nn_lb)
#-------------------------------------------------------------------------------------------------

eval_nn.rds <- readRDS("eval_nn.rds")
nn_result <- readRDS("nn_result.rds")
r_data[['test']]$prob_nn_lb <- nn_result$prob_nn_lb

########################################## Evaluation ############################################

# Evaluation
result <- evalbin(
  dataset = "test",
  pred = c(
    "prob_rfm", "prob_logit", "prob_logit_lb",
    "prob_nb_nobiz_zip50", "prob_nn_lb"
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
    "prob_nb_nobiz_zip50", "prob_nn_lb"
  ),
  rvar = "res1",
  lev = "Yes",
  cost = 1.41,
  margin = 60,
  train = "All"
)
summary(result)
plot(result)

##################################### prediction for wave 2########################################
## creating mailto columns

r_data[["test"]] <- r_data[["test"]] %>%
mutate(mailto_wave2 = factor(
                        ifelse( ( (prob_nn_lb/2) > BE_resp_rate ) & res1 == "No", T, F),
                        levels = c(T, F))
)

saveRDS(r_data[["test"]]%>% select(id,mailto_wave2),"Lei_Qingqing_Xiaochen_NehalGroup4.rds")

# scale the profit

## profit for validation set
perf_calc <- function(mailto, intro){
  dat <- r_data[["test"]]
  perc_mailto <- mean(dat[[mailto]]== TRUE)
  nr_mailto <- sum(dat[[mailto]] == TRUE)
  rep_rate <- mean(dat$res1 == "Yes")*0.5
  nr_resp <- nr_mailto*rep_rate
  mailto_cost <- 1.41*nr_mailto
  profit <- 60*nr_resp - mailto_cost
  ROME <- profit/ mailto_cost
  prn <- paste(intro, "the number of businesses to which we should mail offers is",
               paste0(nr_mailto,"(", round(perc_mailto,4)*100, "%)."),
               "The response rate for the targeted businesses is predicted to be",
               paste0(round(rep_rate,4)*100, "%"), "or", nr_resp, "buyers. The expected profit is",
               paste0("$", round(profit, 2), "."), "The mailing cost is estimated to be",
               paste0("$", mailto_cost), "with a ROME of", paste0(round(ROME,4)*100, "%." ))
  return(data.frame(perc_mailto, nr_mailto, rep_rate, nr_resp, mailto_cost, profit, ROME, prn))
}

perf_cale(mailto = "mailto_wave2", intro = "With Neural network model")

# scale to 801,821 businesses with 38,487 already responded

profit_scaled <-
  (profit / (nrow(r_data[["test"]]) - sum(r_data[["test"]][["res1"]] == "Yes")) )*(801821 - 38487)


