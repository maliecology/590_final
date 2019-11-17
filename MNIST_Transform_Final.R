#######################################
#Final Project Code
#Due completely on December 2, 2019
#Oldies but Goodies
#A program that takes a dataset of images and can classify the image, simplify them to edges, then identify the lines visible
######################################



# Install Keras for RStudio 
devtools::install_github("rstudio/keras")
library(keras) # Installing keras
system("conda config --set ssl_verify false")  # Get past conda sslerror's
install_keras(tensorflow = "gpu")

# Updating libraries for analysis

library(tidyr) # Install tidyr
library(ggplot2) # Install ggplot
library(imager) # Install imager

# Installing additional packages

install_tensorflow()
library(tensorflow)
install.packages("opencv")
library(opencv)
install.packages("imager")
library(imager)
install.packages("FixedPoint")
library(FixedPoint)
install.packages("purrr")
library(purrr)


# Importing the mnist fashion data set
fashion_mnist <- dataset_fashion_mnist()

# Training the images, 60,000 images
c(train_images, train_labels) %<-% fashion_mnist$train
c(test_images, test_labels) %<-% fashion_mnist$test

# Classifying the images by object
class_names = c('T-shirt/top',
                'Trouser',
                'Pullover',
                'Dress',
                'Coat',
                'Sandal',
                'Shirt',
                'Sneaker',
                'Bag',
                'Ankle boot')

# Exploring the data

dim(train_images) # Number of training images
dim(train_labels) # Integers labeled
dim(test_images) # Tested images
dim(test_labels) # Number of test images


# Preprocess the data

image_1 <- as.data.frame(train_images[1, , ])
colnames(image_1) <- seq_len(ncol(image_1))
image_1$y <- seq_len(nrow(image_1))
image_1 <- gather(image_1, "x", "value", -y)
image_1$x <- as.integer(image_1$x)

# Plot the images

ggplot(image_1, aes(x = x, y = y, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "black", na.value = NA) +
  scale_y_reverse() +
  theme_minimal() +
  theme(panel.grid = element_blank())   +
  theme(aspect.ratio = 1) +
  xlab("") +
  ylab("")

# Scale the values

train_images <- train_images / 255
test_images <- test_images / 255


# Display first 25 images and class name, verify the data

par(mfcol=c(5,5))
par(mar=c(0, 0, 1.5, 0), xaxs='i', yaxs='i')
for (i in 1:25) {
  img <- train_images[i, , ]
  img <- t(apply(img, 2, rev))
  image(1:28, 1:28, img, col = gray((0:255)/255), xaxt = 'n', yaxt = 'n',
        main = paste(class_names[train_labels[i] + 1]))
}


# Edge Detection Step

# Initiate Array for line images
lines <- array(NaN, dim = c(1000,28,28))
# For loop
for(i in 1:1000){


#Transpose the image array
imgd <- t(train_images[i, , ])
#Changing the class to an cimg
imgs <- as.cimg(imgd)


#Using a gaussian filter to denoise
#im <- grayscale(imgs) %>% isoblur(2)


#Computing an image gradient
gr <- imgradient(imgs,"xy")
#plot(gr,layout="row")

#Computing the gradient magnitude
mag <- with(gr,sqrt(x^2+y^2))
#plot(mag)

#Determining the local orientation with the gradient angle
ang <- with(gr,atan2(y,x))
#plot(ang)

#Simplifying the image using non-maxima thresholding
threshold(mag) 

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
#plot(mag)

#Identifying strong and weak thresholds for classifying edge pixels
#strong threshold
t2 <- quantile(mag,.90)
#weak threshold 
t1 <- quantile(mag,.85)
layout(t(1:2))

strong <- mag>t2
#plot(strong,main="Initial set of strong edges")
weak <- mag %inr% c(t1,t2)
#plot(weak,main="Initial set of weak edges")

#Final step is producing the image called strong.new that is strong edges only version of OG image
overlap <- grow(strong,3) & weak 
strong.new <- strong | overlap

#Put Images back into an array
lines[i, , ] <- array(strong.new, dim = c(1,28,28))
#plot(strong.new,main="New set of strong edges")

#Converting strong.new from a pixel image to dataframe
#as.data.frame(strong.new)

}

# Display first 25 images and class name, verify the data

par(mfcol=c(5,5))
par(mar=c(0, 0, 1.5, 0), xaxs='i', yaxs='i')
for (i in 1:25) {
  plot(as.cimg(lines[i, , ]))
}




# Hough transform Step

# Initiate array for hough transform images
hough_mat <- array(NaN, dim = c(1000,80,500))

for (i in 1:1000) {

#change class to cimg
imght <- as.cimg(lines[i, , ])

#Identifying the location of lines in an image
fashion_line <- hough_line(imght, ntheta = 500, data.frame = FALSE, shift = FALSE)

#Put images into an array
hough_mat[i, , ] <- array(fashion_line, dim = c(1,80,500))

#boat_line <- hough_line(strong.new, ntheta = 500, data.frame = TRUE, shift = FALSE) %>% plot
#boat_line <- hough_line(grayscale(im.blurry), ntheta = 500, data.frame = FALSE, shift = TRUE) %>% plot

}

# Display first 25 images and class name, verify the data

par(mfcol=c(5,5))
par(mar=c(0, 0, 1.5, 0), xaxs='i', yaxs='i')
for (i in 1:25) {
  plot(as.cimg(hough_mat[i, , ]))
}



