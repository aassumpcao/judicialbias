rm(list = ls())
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
tjspElected <- tjspAnalysis %>%
  mutate_at(vars(candidate.votes, total.votes, election.votes), as.integer) %>%
  mutate(election.share = ifelse(
    office.ID == 11,
    (candidate.votes / total.votes) - .5,
    (candidate.votes / total.votes) - (election.votes / total.votes)
  ))

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
tjsp_test <- tjsp_data[-id_train, ]

# recipe to prepare data
rec_obj <- recipe(sct.favorable ~ ., data = tjsp_train) %>%
  step_dummy(all_predictors(), -all_numeric())

# needed to create new recipe for dnn model
# if we center and scale variables to the RF model, the PDP plots will be
# harder to understand
rec_dnn <- rec_obj %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors())

trained_rec <- prep(rec_obj, training = tjsp_train)
trained_rec_dnn <- prep(rec_dnn, training = tjsp_train)

# prepare data
train_data <- bake(trained_rec, new_data = tjsp_train)
test_data  <- bake(trained_rec, new_data = tjsp_test)
train_data_dnn <- bake(trained_rec_dnn, new_data = tjsp_train)
test_data_dnn  <- bake(trained_rec_dnn, new_data = tjsp_test)


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
  method = "glmnet",
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
    x = as.matrix(train_data_dnn[,-1]),
    y = as.numeric(as.character(train_data_dnn$sct.favorable)),
    epochs = 50, batch_size = 128, validation_split = .1
  )

# model 4: extreme gradient boosting
caret_xgb_model <- caret::train(
  sct.favorable ~ .,
  method = "xgbTree",
  data = train_data
)

# predictions
predictions <- tibble::tibble(
  pred_rf = predict(rf_model, test_data),
  # pred_gbm = predict(caret_xgb_model, test_data),
  # pred_lasso = predict(caret_glm_model, test_data),
  # pred_dnn = as.factor(predict_classes(dnn_model, as.matrix(test_data_dnn[,-1])))
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
    geom_point(size = 2, colour = d_graf$cor[[1]]) +
    theme_minimal() +
    labs(x = "Mean decrease Gini",
         y = "",
         title = "Random Forest Model",
         subtitle = paste(scales::percent(d_graf$acc[[1]]), "Accuracy"))
}

# variable importance plot
plot_rf <- rf_model %>%
  list(acc = tab_acc$acc[1]) %>%
  d_graf_imp("gray20") %>%
  graf_imp() +
  theme(axis.title = element_text(size = 10),
        axis.title.y = element_text(margin = margin(r = 12)),
        axis.title.x = element_text(margin = margin(t = 12)),
        axis.text.y = element_text(size = 10, lineheight = 1.1),
        axis.text.x = element_text(size = 10, lineheight = 1.1, face = 'bold'),
        text = element_text(family = 'LM Roman 10'),
        # panel.border = element_rect(color = 'black', size = 1),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color = 'grey79'))

# partial dependency plot (PDP)
X <- dplyr::select(test_data, -sct.favorable)
predictor <- Predictor$new(rf_model, data = X,
                           y = test_data$sct.favorable,
                           class = 2)
vars <- c("judge.tenure", "case.claim", "judge.pay", "election.share")
labs <- c("Judge Tenure (days)",
          "Case claim (Brazilian Reais)",
          "Judge Pay (Brazilian Reais)",
          "Election Share")


pdp <- map2(vars, labs, ~{
  ale <- FeatureEffect$new(predictor, feature = .x, method = "pdp")
  ale$plot() +
    scale_y_continuous(limits = c(.65, .83)) +
    labs(x = .y, y = "Win Probability") +
    theme_bw() +
    theme(axis.title = element_text(size = 10),
          axis.title.y = element_text(margin = margin(r = 12)),
          axis.title.x = element_text(margin = margin(t = 12)),
          axis.text.y = element_text(size = 10, lineheight = 1.1, face = 'bold'),
          axis.text.x = element_text(size = 10, lineheight = 1.1, face = 'bold'),
          text = element_text(family = 'LM Roman 10'),
          panel.border = element_rect(color = 'black', size = 1),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_line(color = 'grey79'))
})

plot_pdp <- reduce(pdp, `+`)


out <- list(tab_latex, plot_rf, plot_pdp)
readr::write_rds(out, "plots/out.rds", compress = "xz")
# export -----------------------------------------------------------------------

out <- readr::read_rds("plots/out.rds")
ggsave("plots/rf-varimp.pdf", out[[2]], device = cairo_pdf, width = 10, height = 6.3)
ggsave("plots/rf-pdp.pdf", out[[3]], device = cairo_pdf, width = 8, height = 6)
