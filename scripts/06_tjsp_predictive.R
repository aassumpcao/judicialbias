### judicial favoritism of politicians
# this script produces outcome predictions
# author: andre assumpcao
# email:  andre.assumpcao@gmail.com

# import statements
library(tidyverse)
library(patchwork)
library(recipes)
library(iml)
library(keras)

# seed
s <- 19910401
set.seed(s)
tensorflow::use_session_with_seed(s)

# data -------------------------------------------------------------------------
load('data/tjspFinal.Rda')

# split dataset to elected-candidates only and create running var
tjsp_data <- tjspAnalysis %>%
  mutate_at(vars(candidate.votes, total.votes, election.votes), as.integer) %>%
  mutate(
    election.share = ifelse(
      office.ID == 11,
      (candidate.votes / total.votes) - .5,
      (candidate.votes / total.votes) - (election.votes / total.votes)
    ),
    sct.favorable = case_when(
      case.claimant.win == 1 & str_detect(candidate.litigant.type, 'Claimant') ~ 1,
      case.claimant.win == 0 & str_detect(candidate.litigant.type, 'Defendant') ~ 1,
      TRUE ~ 0
    )
  )

# prepare data to regression models
loc <- locale(grouping_mark = ".", decimal_mark = ",")
tjsp_data <- tjspElected %>%
  mutate(sct.favorable = case_when(
    case.claimant.win == 1 & str_detect(candidate.litigant.type, 'Claimant') ~ 1,
    case.claimant.win == 0 & str_detect(candidate.litigant.type, 'Defendant') ~ 1,
    TRUE ~ 0
  )) %>%
  transmute(
    sct.favorable = factor(sct.favorable),
    case.claim = parse_number(case.claim, locale = loc),
    judge.pay = as.numeric(judge.pay),
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
    candidate.age = as.numeric(candidate.age),
    candidate.gender,
    candidate.education,
    candidate.maritalstatus,
    candidate.experience,
    candidate.litigant.type,
    candidate.elected,
    candidacy.situation,
    party.number = fct_lump(party.number, 10)
  ) %>%
  filter_all(all_vars(!is.na(.))) %>%
  mutate_if(is.character, as.factor)

# separate train and test data
N <- nrow(tjsp_data)
id_train <- sample(seq_len(N), ceiling(N * .8))
tjsp_train <- tjsp_data[id_train, ]
tjsp_test  <- tjsp_data[-id_train, ]

# create recipe preparing data for analysis
rec_obj <- recipe(sct.favorable ~ ., data = tjsp_train) %>%
  step_dummy(all_predictors(), -all_numeric()) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors())

# store predictors and outcomes for future estimation
trained_rec <- prep(rec_obj, training = tjsp_train)

# prepare data for analysis
train_data <- bake(trained_rec, new_data = tjsp_train)
test_data  <- bake(trained_rec, new_data = tjsp_test)

# models -----------------------------------------------------------------------
# model 1: random forest
rf_model <- randomForest::randomForest(
  sct.favorable ~ .,
  ntree = 1000, mtry = 10,
  data = train_data
)

# model 2: logistic lasso
caret_glm_model <- caret::train(
  sct.favorable ~ .,
  method = 'glmnet',
  data = train_data
)

# model 2: deep neural network
dnn_model <- keras_model_sequential()
dnn_model <- dnn_model %>%
  layer_dense(100, input_shape = ncol(train_data) - 1) %>%
  layer_activation_leaky_relu() %>%
  layer_dense(100) %>%
  layer_activation_leaky_relu() %>%
  layer_dense(50) %>%
  layer_dropout(.1) %>%
  layer_activation_leaky_relu() %>%
  layer_dense(1, activation = "sigmoid")

dnn_model %>%
  compile(
    loss = loss_binary_crossentropy,
    optimizer = optimizer_adam(),
    metrics = "accuracy"
  )

dnn_model %>%
  fit(
    x = as.matrix(train_data[,-1]),
    y = as.numeric(as.character(train_data$sct.favorable)),
    epochs = 50, batch_size = 128, validation_split = .1
  )

# model 4: extreme gradient boosting
caret_xgb_model <- caret::train(
  sct.favorable ~ .,
  method = 'xgbTree',
  data = train_data
)

# predictions
predictions <- tibble::tibble(
  pred_rf = predict(rf_model, test_data),
  pred_gbm = predict(caret_xgb_model, test_data),
  pred_lasso = predict(caret_glm_model, test_data),
  pred_dnn = as.factor(predict_classes(dnn_model, as.matrix(test_data[,-1])))
) %>% bind_cols(select(tjsp_test, sct.favorable))

# accuracy / kappa table
tab_acc <- predictions %>%
  gather(model, pred, -sct.favorable) %>%
  group_by(model) %>%
  summarise(accuracy = mean(pred == sct.favorable), acc = accuracy) %>%
  arrange(desc(accuracy)) %>%
  mutate(kappa = (accuracy - mean(tjsp_data$sct.favorable == 1)) / mean(tjsp_data$sct.favorable == 1)) %>%
  mutate_at(vars(accuracy, kappa), scales::percent, .01)

# output -----------------------------------------------------------------------

# output for latex
tab_latex <- tab_acc %>%
  select(-acc)

# variable importance plot functions
d_graf_imp <- function(m_list, cor) {
  m <- m_list[[1]]
  acc <- m_list[[2]]
  d_graf <- m %>%
    randomForest::importance() %>%
    as.data.frame() %>%
    tibble::rownames_to_column() %>%
    tibble::as_tibble() %>%
    dplyr::mutate(rowname = stringr::str_extract(rowname, "[^(]+")) %>%
    dplyr::mutate(rowname = forcats::fct_reorder(rowname, MeanDecreaseGini),
                  acc = acc, cor = cor)
  d_graf
}
graf_imp <- function(d_graf) {
  d_graf %>%
    ggplot(aes(x = MeanDecreaseGini, y = rowname)) +
    geom_segment(aes(xend = 0, yend = rowname), size = 1, colour = d_graf$cor[[1]]) +
    geom_point(size = 4, colour = d_graf$cor[[1]]) +
    theme_minimal(14) +
    labs(x = "Mean decrease Gini",
         y = "",
         title = "Random Forest Model",
         subtitle = paste(scales::percent(d_graf$acc[[1]]), "Accuracy"))
}

# variable importance plot
plot_rf <- rf_model %>%
  list(acc = tab_acc$acc[1]) %>%
  d_graf_imp("darkblue") %>%
  graf_imp()

# partial dependency plot (PDP)
X <- dplyr::select(test_data, -sct.favorable)
predictor <- Predictor$new(rf_model, data = X,
                           y = test_data$sct.favorable,
                           class = 2)
vars <- c("case.claim", "judge.tenure", "judge.pay", "election.share")

pdp <- map(vars, ~{
  ale <- FeatureEffect$new(predictor, feature = .x, method = "pdp")
  ale$plot() +
    theme_bw(14) +
    ggtitle(.x)
})

plot_pdp <- reduce(pdp, `+`)

out <- list(tab_latex, plot_rf, plot_pdp)
readr::write_rds(out, "plots/out.rds")
