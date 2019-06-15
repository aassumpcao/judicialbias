library(tidyverse)
library(recipes)

# load data
load('data/tjspFinal.Rda')

# split dataset to elected-candidates only and create running var
tjspElected <- tjspAnalysis %>%
  mutate_at(vars(candidate.votes, total.votes, election.votes), as.integer) %>%
  mutate(election.share = ifelse(
    office.ID == 11,
    (candidate.votes / total.votes) - .5,
    (candidate.votes / total.votes) - (election.votes / total.votes)
  ))

# prepare data to regression models
tjsp_data <- tjspElected %>%
  mutate(sct.favorable = case_when(
    case.claimant.win == 1 & str_detect(candidate.litigant.type, 'Claimant') ~ 1,
    case.claimant.win == 0 & str_detect(candidate.litigant.type, 'Defendant') ~ 1,
    TRUE ~ 0
  )) %>%
  transmute(
    sct.favorable = factor(sct.favorable),
    case.claim = parse_number(case.claim, locale = locale(grouping_mark = ".", decimal_mark = ",")),
    # case.claim = cut(case.claim, c(0, 1000, 5000, 10000, 20000, 40000, Inf)),
    # judge.pay = cut(as.numeric(judge.pay), c(0, 10000, 30000, 50000, Inf)),
    judge.pay = as.numeric(judge.pay),
    # judge.tenure = cut(as.numeric(judge.tenure), c(0, 5000, 10000, Inf)),
    judge.tenure = as.numeric(judge.tenure),
    case.claim,
    election.year,
    election.share,
    judge.pay,
    judge.gender,
    judge.tenure,
    election.year,
    election.year,
    office.ID,
    candidate.litigant.type,
    # candidate.age = cut(as.numeric(candidate.age), c(0, 30, 50, 60, 80, Inf)),
    candidate.age = as.numeric(candidate.age),
    candidate.gender,
    # candidate.gender.same.judge = as.factor(as.numeric(candidate.gender == judge.gender)),
    candidate.education,
    candidate.maritalstatus,
    candidate.experience,
    candidate.litigant.type,
    # candidate.votes = cut(as.numeric(candidate.votes), c(0, 1000, 10000, 100000, Inf)),
    # candidate.votes = as.numeric(candidate.votes),
    candidate.elected,
    candidacy.situation,
    party.number = fct_lump(party.number, 10)
  ) %>%
  filter_all(all_vars(!is.na(.))) %>%
  mutate_if(is.character, as.factor)

# seed
set.seed(19910401)

# separate train and test data
id_train <- sample(seq_len(nrow(tjsp_data)), 4200)
tjsp_train <- tjsp_data[id_train, ]
tjsp_test <- tjsp_data[-id_train, ]

# recipe to prepare data
rec_obj <- recipe(sct.favorable ~ ., data = tjsp_train) %>%
  step_dummy(all_predictors(), -all_numeric()) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors())

trained_rec <- prep(rec_obj, training = tjsp_train)

# prepare data
train_data <- bake(trained_rec, new_data = tjsp_train)
test_data  <- bake(trained_rec, new_data = tjsp_test)

# model 1: random forest
rf_model <- randomForest::randomForest(
  sct.favorable ~ .,
  ntree = 1000, mtry = 10,
  data = train_data
)

summary(glm(sct.favorable ~ ., data = train_data, family = "binomial"))

# model 2: random forest with tuning (~20 min)
caret_rf_model <- caret::train(
  sct.favorable ~ .,
  method = "rf",
  data = train_data
)

# model 3: logistic lasso
caret_glm_model <- caret::train(
  sct.favorable ~ .,
  method = "glmnet",
  data = train_data
)

# check how to print significant variables
summary(caret_glm_model)

# model 4: gradient boosting
# caret_h2o_model <- caret::train(
#   sct.favorable ~ .,
#   method = "gbm_h2o",
#   data = train_data
# )

# model 5: extreme gradient boosting
caret_xgb_model <- caret::train(
  sct.favorable ~ .,
  method = "xgbTree",
  data = train_data
)

# predictions
tjsp_test$pred_caret_rf <- predict(caret_rf_model, test_data)
tjsp_test$pred_caret_lasso <- predict(caret_glm_model, test_data)
tjsp_test$pred_rf <- predict(rf_model, test_data)
tjsp_test$pred_xgb <- predict(caret_xgb_model, test_data)

# accuracy
tab <- with(tjsp_test, table(sct.favorable, pred_rf))
sum(diag(tab)) / sum(tab)
tab <- with(tjsp_test, table(sct.favorable, pred_xgb))
sum(diag(tab)) / sum(tab)

# importance plot
randomForest::varImpPlot(m$model)
randomForest::varImpPlot(rf_model)


library(iml)
X <- dplyr::select(test_data, -sct.favorable)
predictor <- Predictor$new(rf_model, data = X, y = as.numeric(test_data$sct.favorable))
# imp <- FeatureImp$new(predictor, loss = "mae")
# plot(imp)

ale = FeatureEffect$new(predictor, feature = "case.claim")
ale$plot()
ale = FeatureEffect$new(predictor, feature = "judge.tenure")
ale$plot()
ale = FeatureEffect$new(predictor, feature = "judge.pay")
ale$plot()
ale = FeatureEffect$new(predictor, feature = "election.share")
ale$plot()
