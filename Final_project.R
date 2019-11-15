#######################################
#Final Project Code
#Due completely on December 2, 2019
#Oldies but Goodies
#A program that takes a dataset of images and can classify the image, simplify them to edges, then identify the lines visible
######################################

## Want to create a matrix output of processed images with Hough transform to identify lines



#Installing packages
install.packages("keras")
library(keras)
library(tensorflow)
install_tensorflow()
install.packages("opencv")
library(opencv)
install.packages("imager")
library(imager)
install.packages("FixedPoint")
library(FixedPoint)
install.packages("purrr")
library(purrr)


#######################
#Classification Step
#######################


#Defining the data
#myData <- dataset_mnist()
myData <- dataset_fashion_mnist()

#Detecting edges using Canny
#Using a gaussian filter to denoise
image <- grayscale(myData) %>% isoblur(2)

#Computing an image gradient
gradient <- imgradient(image,"xy")
plot(gradient,layout="row")

#Computing the gradient magnitude
magitude <- with(gradient,sqrt(x^2+y^2))
plot(magnitude)

#Determining the local orientation with the gradient angle
angle <- with(gradient,atan2(y,x))
plot(angle)

#Simplifying the image using non-maxima thresholding
threshold(magnitude) %>% plot



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




#######################
#Edge Detection Step
#######################

#Using a gaussian filter to denoise
im <- grayscale(boats) %>% isoblur(2)

#Computing an image gradient
gr <- imgradient(im,"xy")
plot(gr,layout="row")

#Computing the gradient magnitude
mag <- with(gr,sqrt(x^2+y^2))
plot(mag)

#Determining the local orientation with the gradient angle
ang <- with(gr,atan2(y,x))
plot(ang)

#Simplifying the image using non-maxima thresholding
threshold(mag) %>% plot

#Going along the (normalised) gradient
#Xc(im) is an image containing the x coordinates of the image
nX <- Xc(im) + gr$x/mag 
nY <- Yc(im) + gr$y/mag
#nX and nY are not integer values, so we can't use them directly as indices.
#We can use interpolation, though:
val.fwd <- interp(mag,data.frame(x=as.vector(nX),y=as.vector(nY)))

nX <- Xc(im) - gr$x/mag 
nY <- Yc(im) - gr$y/mag
val.bwd <- interp(mag,data.frame(x=as.vector(nX),y=as.vector(nY)))

throw <- (mag < val.bwd) | (mag < val.fwd)
mag[throw] <- 0
plot(mag)

#Identifying strong and weak thresholds for classifying edge pixels
#strong threshold
t2 <- quantile(mag,.96)
#weak threshold 
t1 <- quantile(mag,.90)
layout(t(1:2))

strong <- mag>t2
plot(strong,main="Initial set of strong edges")
weak <- mag %inr% c(t1,t2)
plot(weak,main="Initial set of weak edges")

#Final step is producing the image called strong.new that is strong edges only version of OG image
overlap <- grow(strong,3) & weak 
strong.new <- strong | overlap
plot(strong.new,main="New set of strong edges")

#Converting strong.new from a pixel image to dataframe
as.data.frame(strong.new)







##########################
#Hough transform Step
##########################

#Blurring the boats image
im.blurry <- isoblur(boats, 10)

#Identifying the location of lines in an image
boat_line <- hough_line(strong.new, ntheta = 500, data.frame = FALSE, shift = FALSE) %>% plot
#boat_line <- hough_line(strong.new, ntheta = 500, data.frame = TRUE, shift = FALSE) %>% plot
#boat_line <- hough_line(grayscale(im.blurry), ntheta = 500, data.frame = FALSE, shift = TRUE) %>% plot