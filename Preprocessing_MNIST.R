

# Install Keras for RStudio 
devtools::install_github("rstudio/keras")
library(keras) # Installing keras
system("conda config --set ssl_verify false")  # Get past conda sslerror's
install_keras(tensorflow = "gpu")

# Installing libraries for analysis

library(tidyr) # Install tidyr
library(ggplot2) # Install ggplot
library(imager) # Install imager


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

# Display lines in an image using Hough Transform

linesegments <- line_segment_detector(x)
linesegments
plot(image)
plot(linesegments, add = TRUE, col = "red")


# Set up the layers