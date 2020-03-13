###################################################################
### Purpose: Create an interactive plotly graph of electric field 
###     intensity inside a rectangular waveguide
###     (adapted from the original python version I created)
### Author: Kurt Burdick
### modified: 3/1/2020
###################################################################

library(plotly)
library(dplyr)
library(lubridate)

# heatmap needs a matrix or data frame to create the plot
# these particular calculations require a small step size
x1 <- c(1:2000) / 2000
y1 <- c(1:3000) / 3000

mat <- matrix(nrow = 2000, ncol = 3000)

# testing population of the larger non-square matrix
mat[,1:3000] <- ( c(1:3000) / 3000)
mat[1:2000,] <- ( c(1:2000) / 2000)

# creating the matrix to populate with electric field values in the X, Y, and Z directions
mat1 <- matrix(nrow = 2000, ncol = 3000)

# some constants for the field equations
E0 <- 100
a <- 1
b <- 1
m <- 2
n <- 3

# main function to populate the matrix with calculated values
i <- 0
j <- 0
for(i in 1:2000){
  for(j in 1:3000){
    
    # need small steps to align calcs
    k <- i / 2000
    l <- j / 3000
    
    # field in all directions
    E_x <- -100*(m*pi/a)*cos(m*pi*k/a)*sin(n*pi*l/b)
    
    E_y <- -100*(n*pi/b)*sin(m*pi*k/a)*cos(n*pi*l/b)
    
    E_z <- 100*(sin(m*pi*k/a)*sin(n*pi*l/b))
    
    # intensity of the field
    E_1 <- (E_x*E_x + E_y*E_y + E_z*E_z)
    
    # add it to the  matrix
    mat1[i,j] <- E_1 
  }
} 

# check that the matrix is good
dim(mat1)

# Create a plotly interactive heatmap with the resulting matrix
test <- plot_ly(
  x = x1, y = y1, z = mat1, type = "heatmap"
)

# show test graph
test
