######################################## Setting up ################################################
# install.packages(c("glmnet","purrr"))
if (!exists("r_environment")) library(radiant)
library(purrr)
library(glmnet)

intuit75k_wrk <- readr::read_rds(file.path(find_dropbox(), "MGTA455-2018/data/intuit75k.rds"))
# data pre-checking
# glimpse(head(intuit75k_wrk,10))
# map_df(intuit75k_wrk,function(x){any(is.na(x))})
??wd()
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

# it's a sample of naive bayes
# result <- nb(
#   dataset = training,
#   rvar = "res1",
#   evar = c(
#     "zip_bins", "sex", "bizflag", "numords", "dollars", "last",
#     "sincepurch", "version1", "owntaxprod", "upgraded"
#   )
# )
# summary(ressult)
# plot(result)
# pred <- predict(result, pred_data = test)
# store(pred, data = test, name = "predict_nb")


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
#   store(pred, data = nn_result, name = paste0("predict_nn",i))
# this `store` line has something wrong with loop, need to fix
# }
