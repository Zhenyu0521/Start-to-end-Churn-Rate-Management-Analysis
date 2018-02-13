######################################## Setting up ################################################
# install.packages(c("glmnet","purrr"))
if (!exists("r_environment")) library(radiant)
library(purrr)
library(glmnet)
library(ROCR)

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
  mutate(mailto_rfm = (mean(res1 == "Yes")/2 > BE_resp_rate) & (res1 == "No")) %>%
  ungroup()

# split training and test data set
training <- filter(intuit75k_wrk,training == 1)
test <- filter(intuit75k_wrk,training == 0)

#################################### Logistic regression model #######################################

# it's a sample of logistic regreesion
# result <- logistic(
#   dataset = training,
#   rvar = "res1",
#   evar = c(
#     "zip_bins", "numords", "dollars", "last", "sincepurch",
#     "version1", "owntaxprod", "upgraded"
#   ),
#   lev = "Yes"
# )
# summary(result)
# pred <- predict(result, pred_data = test)
# store(pred, data = test, name = "predict_logit")




#################################### Naive bayes model #######################################
# Full model
result <- nb(
  dataset = "intuit75k_wrk",
  rvar = "res1",
  evar = c(
    "zip_bins", "sex", "bizflag", "numords", "dollars", "last",
    "sincepurch", "version1", "owntaxprod", "upgraded"
  ),
  data_filter = "training == '1'"
)
summary(result)
plot(result)
pred <- predict(result, pred_data = "intuit75k_wrk")
store(pred, data = "intuit75k_wrk", name = "purch_prob_nb")

# Create resp_nb and mailto_nb:
intuit75k_wrk <- mutate(intuit75k_wrk, resp_nb = ifelse(purch_prob_nb > BE_resp_rate, TRUE, FALSE),
                                       mailto_nb = ifelse(((purch_prob_nb / 2) > BE_resp_rate) & (res1 == "No"), TRUE, FALSE)) %>%
                 ungroup()

# Split training and test data set
training <- filter(intuit75k_wrk, training == 1)
test <- filter(intuit75k_wrk, training == 0)

# Compute model accuracy
table(test$res1, test$resp_nb)
(817 + 11711) / 22500 # 55.68%

# Profit function
perf_calc <- function(sms, intro) {
  # no. of wave II mails to be sent to validation base
  nr_mail <- sum(test[[sms]])
  # estimate response rate on wave 2 based on the response rate in wave 1
  rep_rate <- mean(test['res1'] == "Yes") * 0.5
  # no. of customers responsed in 22.5k base
  nr_resp <- nr_mail * rep_rate
  mail_cost <- 1.41 * nr_mail
  profit = 60 * nr_resp - mail_cost
  ROME = profit / mail_cost

  prn <- paste(intro, "the number of customers Intuit should send is", paste0(round(nr_mail, 1), "."),
               "The response rate for the selected customers is predicted to be",
               paste0(round(rep_rate*100, 2),"%"), "or", round(nr_resp), paste0("buyers", "."), "The expected profit is",
               paste0("$", round(profit), "."), "The mail cost is estimated to be", paste0(round(mail_cost)), "with a ROME of",
               paste0(round(ROME*100, 2), "%", "."))
  results <- data.frame(profit, ROME, prn, rep_rate)
  return(results)
}

# save result
res <- perf_calc(sms = "mailto_nb", intro = "Based on Naive Bayes model")
profit_nb <- res$profit
ROME_nb <- res$ROME
cat(res$prn)

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
