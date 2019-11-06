library(keras)

use_session_with_seed(123,disable_parallel_cpu = TRUE)
data = iris

# shuffle data for validation purpose
data = data[sample(nrow(data), nrow(data)), ]

y = data[, "Species"]
x = data[,1:4]

# scale to [0,1]
x = as.matrix(apply(x, 2, function(x) (x-min(x))/(max(x) - min(x))))

# one hot encode classes
levels(y) = 1:length(y)
y = to_categorical(as.integer(y) - 1 , num_classes = 3)

# create sequential model
model = keras_model_sequential()

# add layers, first layer needs input dimension
model %>%
  layer_dense(input_shape = ncol(x), units = 10, activation = "relu") %>%
  layer_dense(units = 10, activation = "relu") %>%
  layer_dense(units = 3, activation = "softmax") # -> one-hot encoding

# add a loss function and optimizer
model %>%
  compile(
    loss = "categorical_crossentropy",
    optimizer = "adagrad",
    metrics = "accuracy"
  )

# fit model with our training data set, training will be done for 200 times data set
fit = model %>%
  fit(
    x = x,
    y = y,
    shuffle = T,
    batch_size = 5,
    validation_split = 0.3,
    epochs = 200
  )

# Prepare bins:
# a list of bins per column, the nth-entry provides the bins of the nth-column
# Per bin: define whether numeric, the cuts
# of the bins, and whether the right or the left border of a bin is included
# Note that the bins are scaled accordingly to the input data
bins = list()
bins[[1]] = list(cuts = c(0.3513865, 0.441276, 0.514822, 0.6455705))
bins[[2]] = list(cuts = c(0.30857, 0.4474265, 0.4937119, 0.6788539))
bins[[3]] = list(cuts = c(0.112116, 0.2952389, 0.5493686, 0.7736007))
bins[[4]] = list(cuts = c(0.03232353, 0.2801373, 0.5171765, 0.8080883))

#  Anchors expect a dataframe
x = as.data.frame(x)

model = as_classifier(model, labels=factor(c("setosa","versicolor", "virginica")))

# Prepare explainer to explain model with anchors, omit target parameter as
# input data to not have target column
explainer = anchors(x, model, bins = bins, tau = 1)

# Construct the explanations
explanations = explain(x[sample(nrow(x),2),], explainer)

# print explanations
printExplanations(explainer, explanations)

plotExplanations(explanations)
