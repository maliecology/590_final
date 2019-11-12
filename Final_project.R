######################
#Final Project Code
#Using Keras and Tensorflow
######################


#Installing packages
install.packages("keras")
library(keras)
library(tensorflow)
install_tensorflow()
install.packages("opencv")
library(opencv)

#Defining the data
myData <- dataset_mnist()
#myData <- dataset_fashion_mnist()

#Defining variables from the dataset
train_images <- myData$train$x
train_labels <- myData$train$y
test_images <- myData$test$x
test_labels <- myData$test$y

#Looking at the training dataset
str(train_images)
str(train_labels)
str(test_images)
str(test_labels)

#Building a network
network <- keras_model_sequential() %>%
  layer_dense(units = 512, activation = "relu", input_shape = c(28 * 28)) %>%
  layer_dense(units = 10, activation = "softmax")

#Building a compilation layer
network %>% compile(
  optimizer = "rmsprop",
  loss = "categorical_crossentropy",
  metrics = c("accuracy")
)

#Pre-processing the data
train_images <- array_reshape(train_images, c(60000, 28 * 28))
train_images <- train_images / 255

test_images <- array_reshape(test_images, c(10000, 28 * 28))
test_images <- test_images / 255

#Adding labels
train_labels <- to_categorical(train_labels)
test_labels <- to_categorical(test_labels)

#Using the fit method to train the network
network %>% fit(train_images, train_labels, epochs = 5, batch_size = 128)

#Outputting the metrics
metrics <- network %>% evaluate(test_images, test_labels)
metrics
