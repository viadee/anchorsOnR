library("keras")

load(system.file("inst", "data", "mnist_train", package = "anchors"))
load(system.file("inst", "data", "mnist_test", package = "anchors"))

# Shuffle rows
mnist_train = mnist_train[sample(nrow(mnist_train)),]
mnist_test = mnist_test[sample(nrow(mnist_test)),]

# For testing different model designs, it may be useful to reduces your training set. Run this to reduce the data_size to 20% of its original size
mnist_train = mnist_train[1:12000,]
mnist_test = mnist_test[1:2000,]

# Convert the labels to one-hot encoding
y_train = to_categorical(mnist_train[,785],  num_classes = 10)
y_test = to_categorical(mnist_test[,785],  num_classes = 10)

# Separate the input from the output
x_train = as.matrix(mnist_train[,1:784])
x_test = as.matrix(mnist_test[,1:784])

# check for missing values
any(is.na(x_train[,1:784]))
any(is.na(x_test[,1:784]))

# normalize the features
x_train = x_train %>% normalize()
x_test = x_test %>% normalize()

# number of dimensions does not fit to common understanding of image
# dim(x_train)
# > 60000   784
# an image has a height and a width
# sqrt(784)
# > 28
# every image should have two dimensions with heigth 28, width 28
# keras additionally wants to know as to whether it is an greyscale (1) or RGB (3) image
dim(x_train) = c(nrow(x_train), 28, 28, 1)
dim(x_test) = c(nrow(x_test), 28, 28, 1)
# dim(x_train)
# > 60000 28 28 1

set.seed(1234)

# Create the model.
model <- keras_model_sequential()
model %>%
  layer_conv_2d(filters = 32, kernel_size = c(5,5),padding = 'Valid',
                input_shape = c(28,28,1))%>%
  layer_batch_normalization()%>% #https://towardsdatascience.com/dont-use-dropout-in-convolutional-networks-81486c823c16
  layer_activation(activation = "relu")%>%
  layer_max_pooling_2d(pool_size = c(2,2))%>% #https://medium.com/technologymadeeasy/the-best-explanation-of-convolutional-neural-networks-on-the-internet-fbb8b1ad5df8

  layer_conv_2d(filters = 64, kernel_size = c(5,5))%>%
  layer_batch_normalization() %>%
  layer_activation(activation = "relu")%>%
  layer_max_pooling_2d(pool_size = c(2,2))%>%

  layer_conv_2d(filters = 64, kernel_size = c(3,3))%>%
  layer_batch_normalization() %>%
  layer_activation(activation = "relu")%>%
  layer_max_pooling_2d(pool_size = c(2,2))%>%

  layer_flatten()%>%
  # layer_dropout(0.5) %>% #https://medium.com/@amarbudhiraja/https-medium-com-amarbudhiraja-learning-less-to-learn-better-dropout-in-deep-machine-learning-74334da4bfc5
  layer_dense(units = 10, activation = 'softmax') # softmax makes the output sum up to 1 so the output can be interpreted as probabilities

# Compile initializes the model. The used optimizer is Stochastic gradient descent. But you can change it to other optimizer
model %>% compile(
  optimizer = 'sgd',
  loss = 'categorical_crossentropy', # Always use categorical crossentropy as error function for MNIST
  metrics = list('accuracy')
)

# Print a summary about the model
summary(model)

# Train your model
model %>% fit(
  x = x_train,
  y = y_train,
  validation_data = list(x_test, y_test),
  epochs = 50, # Number of epochs. Can be changed
  batch_size = 100,
  shuffle=T,
  verbose=2
  #,callback_early_stopping(monitor = "val_loss", min_delta = 0.0025, patience = 3, baseline = 0.054, mode = "auto") # additional measure to avoid overfitting, but unsure if good idea
)

ctrl = initAnchors(ip = "localhost", port = 6666, startAnchors = TRUE)

perturbator = makePerturbFun("tabular.featureless")
ctrl = registerAnchorsComponent(ctrl, "perturbate", perturbator)


predict.fn <- function(){
  predict(model, x_text)
}
ctrl = registerAnchorsComponent(ctrl, "predict")
ctrl = registerAnchorsComponent(ctrl, "datasets")
ctrl = registerAnchorsComponent(ctrl, "task")

# Save your model
model %>% save_model_hdf5('model_t_hell07.h5')

## evaluation of model ##

new_model <- load_model_hdf5("model_t_hell07.h5")

pred <- data.frame(y = predict(new_model, x_test))

df<-data.frame(pred,x_test)

score = new_model %>% evaluate(x_train, y_train)
cat('Train loss:', score$loss, sep = "\n")
cat('Train accuracy:', score$acc, sep = "\n")

score = new_model %>% evaluate(x_test, y_test)
cat('Test loss:', score$loss, sep = "\n")
cat('Test accuracy:', score$acc, sep = "\n")
