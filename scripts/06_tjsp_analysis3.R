### judicial favoritism of politicians
#  this script executes the prediction algorithms on the sct data
# andre assumpcao and julio trecenti
# email: andre.assumpcao@gmail.com
# email: julio.trecenti@gmail.com

### data and library calls
# import libraries
library(tidyverse)
library(patchwork)
library(recipes)
library(iml)
library(keras)

# load data
load('data/tjspElected.Rda')

# set seed for prediction exercises
s <- 19910401
set.seed(s)
tensorflow::use_session_with_seed(s)

# wrangle data for regressions
loc <- locale(grouping_mark = '.', decimal_mark = ',')

# narrow down to numeric variables which can be used for regression analysis
tjsp_data <- tjspElected %>%
  transmute(
    sct.favorable = factor(sct.favorable), case.claim,
    judge.pay = as.numeric(judge.pay), judge.tenure = as.numeric(judge.tenure),
    election.year, election.share, judge.gender, office.ID,
    candidate.age = as.numeric(candidate.age), candidate.gender,
    candidate.education, candidate.maritalstatus, candidate.experience,
    candidate.litigant.type, candidate.elect, candidacy.situation,
    party.number = fct_lump(party.number, 10)
  ) %>%
  filter_all(all_vars(!is.na(.))) %>%
  mutate_if(is.character, as.factor)

# separate train and test data
N <- nrow(tjsp_data)
id_train <- sample(seq_len(N), ceiling(N * .8))
tjsp_train <- tjsp_data[ id_train, ]
tjsp_test  <- tjsp_data[-id_train, ]

# create recipe to prepare data for most models
rec_obj <- recipe(sct.favorable ~ ., data = tjsp_train) %>%
  step_dummy(all_predictors(), -all_numeric())

# create recipe for dnn model. we need to center and scale them, a problem for
# the random forest model
rec_dnn <- rec_obj %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors())

# create objects taking in training results
trained_rec     <- prep(rec_obj, training = tjsp_train)
trained_rec_dnn <- prep(rec_dnn, training = tjsp_train)

# prepare data for analysis
train_data     <- bake(trained_rec,     new_data = tjsp_train)
test_data      <- bake(trained_rec,     new_data = tjsp_test)
train_data_dnn <- bake(trained_rec_dnn, new_data = tjsp_train)
test_data_dnn  <- bake(trained_rec_dnn, new_data = tjsp_test)

# prepare cross-validation objects for
train_control <- caret::trainControl(method = 'cv', number = 5)

### train models
# model 1: random forest
rf_model <- randomForest::randomForest(
  sct.favorable ~ ., ntree = 1000, mtry = 10, data = train_data,
  trControl = train_control
)

# model 2: logistic lasso
caret_glm_model <- caret::train(
  sct.favorable ~ ., method = 'glmnet', data = train_data,
  trControl = train_control
)

# model 3: deep neural network
dnn_model <- keras_model_sequential()
dnn_model %<>%
  layer_dense(150, input_shape = ncol(train_data) - 1) %>%
  layer_activation_leaky_relu() %>%
  layer_dense(100) %>%
  layer_activation_leaky_relu() %>%
  layer_dense(100) %>%
  layer_dropout(.1) %>%
  layer_activation_leaky_relu() %>%
  layer_dense(1, activation = 'sigmoid')

dnn_model %>%
  compile(
    loss = loss_binary_crossentropy,
    optimizer = optimizer_adam(),
    metrics = 'accuracy'
  )

dnn_fit <- dnn_model %>%
  fit(
    x = as.matrix(train_data_dnn[,-1]),
    y = as.numeric(as.character(train_data_dnn$sct.favorable)),
    epochs = 50, batch_size = 128, validation_split = .2
  )

# model 4: extreme gradient boosting
caret_xgb_model <- caret::train(
  sct.favorable ~ ., method = 'xgbTree', data = train_data,
  trControl = train_control
)

# perform prediction exercises
predictions <- tibble(
  rf    = predict(rf_model, test_data),
  gbm   = predict(caret_xgb_model, test_data),
  lasso = predict(caret_glm_model, test_data),
  dnn   = as.factor(predict_classes(dnn_model, as.matrix(test_data_dnn[,-1])))
) %>%
bind_cols(select(tjsp_test, sct.favorable))

# construct accuracy / kappa table
tab_acc <- predictions %>%
  gather(model, pred, -sct.favorable) %>%
  group_by(model) %>%
  summarise(accuracy = mean(pred == sct.favorable), acc = accuracy) %>%
  arrange(desc(accuracy)) %>%
  mutate(
    kappa = (accuracy - mean(tjsp_data$sct.favorable == 1)) /
            mean(tjsp_data$sct.favorable == 1)
  ) %>%
  mutate_at(vars(accuracy, kappa), scales::percent, .01)

# rename models in accuracy table
tab_acc %>%
  mutate(
    model = c('Random Forest','Gradient Boost','Lasso','Deep Neural Networks')
  ) %>%
  xtable::xtable()

# produce mean decrease in gini object
model_information <- list(rf_model, acc = tab_acc$acc[1])
gini <- model_information[[1]] %>%
  randomForest::importance() %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  as_tibble() %>%
  mutate(rowname = str_extract(rowname, '[^(]+')) %>%
  mutate(
    rowname = fct_reorder(rowname, MeanDecreaseGini),
    acc = model_information[[2]], cor = 'gray20'
  )

# save to file just in case
saveRDS(gini, 'data/MeanDecreaseGini.Rds')

# load results
gini %<>%
  arrange(desc(MeanDecreaseGini)) %>%
  slice(1:6) %>%
  mutate_at(1, as.character)

# insert last row to cover eveything else
gini[7,] <- c('All Others', 50, rep(NA_character_, 2))
gini %<>% mutate_at(2, as.integer)

# create labels
labs <- c(
  'Case Claim', 'Judge Tenure', 'Judge Pay', 'Politician is Defendant',
  'Vote Share', 'Candidate Age', 'All Others'
)

# produce mean decrease in gini plot
p <- gini %>%
  ggplot() +
  geom_point(aes(MeanDecreaseGini, 1:7), size = 2) +
  geom_segment(
    aes(x = 0, xend = MeanDecreaseGini, y = 1:7, yend = 1:7), size = 1
  ) +
  geom_segment(
    aes(x = 0, xend = 300, y = 6.5, yend = 6.5), linetype = 'dashed',
    color = 'gray79'
  ) +
  geom_text(
    aes(MeanDecreaseGini, 1:7), label = c(unlist(gini[1:6, 2]), '< 50'),
    nudge_x = 20, family = 'LM Roman 10'
  ) +
  scale_y_reverse(name = NULL, breaks = 1:7, labels = labs) +
  theme_bw() +
  xlab('Mean Decrease in Gini') +
  theme(
    axis.title = element_text(size = 10),
    # axis.title.y = element_text(margin = margin(r = 12)),
    # axis.title.x = element_text(margin = margin(t = 12)),
    axis.text.y = element_text(size = 10, lineheight = 1, face = 'bold'),
    axis.text.x = element_text(size = 10, lineheight = 1, face = 'bold'),
    text = element_text(family = 'LM Roman 10'),
    panel.border = element_rect(color = 'black', size = 1),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  )

# save plot
ggsave(
  plot = p, 'rf-varimp.pdf', device = cairo_pdf, path = 'plots', dpi = 100,
  width = 7, height = 5
)

# create list of variables to loop over in map2 function
# partial dependency plot (PDP)
X <- select(test_data, -sct.favorable)
predictor <- Predictor$new(
  rf_model, data = X, y = test_data$sct.favorable, class = 2
)
vars <- c(
  'case.claim', 'judge.tenure', 'judge.pay', 'candidate.litigant.type_Defendant'
)

# execute function
pdp_plots <- map2(vars, labs[1:4], function(.x, .y){
  FeatureEffect$new(predictor, feature = .x, method = 'pdp')$plot() +
    labs(x = .y) +
    scale_y_continuous(limits = c(.1, 1)) +
    theme_bw() +
    theme(
      axis.title = element_text(size = 10),
      axis.title.y = element_blank(),
      # axis.title.x = element_text(margin = margin(t = 12)),
      axis.text.y = element_text(size = 10, lineheight = 1.1, face = 'bold'),
      axis.text.x = element_text(size = 10, lineheight = 1.1, face = 'bold'),
      text = element_text(family = 'LM Roman 10'),
      panel.border = element_rect(color = 'black', size = 1),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_line(color = 'grey79')
    )
})

# arrange them in the same scalse
p <- gridExtra::grid.arrange(
  pdp_plots[[1]], pdp_plots[[2]], pdp_plots[[3]], pdp_plots[[4]]
)

# save to disk
ggsave(
  plot = p, 'rf-pdp.pdf', device = cairo_pdf, path = 'plots', dpi = 100,
  width = 7, height = 5
)
