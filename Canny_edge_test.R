install.packages("opencv")
library(opencv)
install.packages("imager")
library(imager)
install.packages("FixedPoint")
install.packages("purrr")
library(FixedPoint)
library(purrr)

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

#Running morphological dialation on potential edges
px <- imnoise(100,100) > 1
layout(t(1:2))
plot(px,"Original")
plot(grow(px,3),"Dilated Set")

overlap <- grow(strong,3) & weak 
strong.new <- strong | overlap
plot(strong.new,main="New set of strong edges")

#delta <- sum(strong.new)-sum(strong)
#delta

##Example of a fixed point iteration:
##divide a number by 2 until the result doesn't change
#f <- function(x) x/2
#g <- fp(f)
#g(3) #Run the fixed point iteration from 3.

##expandStrong <- function(ws)
#{
#  overlap <- grow(ws$strong,3) & ws$weak
#  ws$strong[overlap] <- TRUE
#  ws$weak[overlap] <- FALSE
#  ws
#}


##hystFP is a new function that will call expandStrong repeatedly until
##the weak and strong sets don't change anymore
#hystFP <- fp(expandStrong)

##Call hystFP
#out <- list(strong=strong,weak=weak) %>% hystFP
#out

#canny <- out$strong
#plot(canny,main="Canny edges")

##Using bucket fill method for speeding up processing
##Collect seed pixels and plot their location
#plot(strong)
#map_df(pxs,~ where(.)[1,]) %$% points(x,y,col="red")
#v <- as.cimg(strong)
#v[weak==1] <- .9 #Strong pixels have value 1, weak .9, and the rest are 0.

#add <- function(l) reduce(l,function(acc,item) acc+item,.init=0)
#add(1:3) #equals sum(1:3)
#mult <- function(l) reduce(l,function(acc,item) acc*item,.init=1)
#mult(1:3) #equals prod(1:3)
##put the three colour channels side-by-side
#imsplit(boats,"c") %>% reduce(function(acc,l) imappend(list(acc,l),"x")) %>% plot

#Gives a set of initial locations for the bucket fill
#fillInit <- function(strong)
#{
#  pxs <- split_connected(strong,high_connectivity=TRUE)
#  map_df(pxs,~ where(.)[1,])
#}

##Starts a fill at each successive location, and accumulates the results
#rescueFill <- function(strong,weak)
#{
#  v <- as.cimg(strong)
#  v[weak] <- .9
#  loc <- fillInit(strong)
#  #Transform the data.frame into a list of locations
#  loc <- transpose(loc)
#  #Fold
#  out <- reduce(loc,function(v,l) bucketfill(v,l$x,l$y,color=1,sigma=.1,high=TRUE),
#                .init=v)
#  out==1
#}

#canny2 <- rescueFill(strong,weak)
#all.equal(canny,canny2)

#system.time(hystFP(list(strong=strong,weak=weak)))
#system.time(rescueFill(strong,weak))