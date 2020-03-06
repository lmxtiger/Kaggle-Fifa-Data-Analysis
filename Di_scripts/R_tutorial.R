?print
help(print)

#downloading a package
#install.packages("MASS")

# loading a package
# (needs to be installed)
#require(MASS)
#library("MASS")

#install.packages("devtools")

# Vectors
vec1 <- c(1,2,3)
vec3 <- c(vec1, c(4,5,6))

#factor
reviews <- c('a', 'b', 'a', 'b', 'c', 'b')
reviews <- factor(reviews)
reviews

# Dataframe
x <- data.frame(A=c(1,2), 
                B=c(2,3,4,5))
# Recycling
x <- data.frame(A=1, 
                B=c(2,3,4)) # len(B)=3, len(A)=1
x
x <- data.frame(A=c(1,2,3), 
                B=c(3,4,5,6,7,8)) # len(B)=6, len(A)=3
x
# OK
x <- data.frame(A=c(1,2,3), 
                 B=c(20,30,40), 
                 C=c('a','b','a'))
# Main difference between matrix and data.frame:
# 1. Every col of a data.frame should have the data, but the classes of the rows needn't be the same;
# All the elements of a matrix should have the same data class
# 2. They have some different operations. For example, "$" only works for a data.frame

x
x$A
x[, "A"]
x[, c("A", "C")]
x[2:3, c("A", "C")]

# A CSV file can be read into a data frame and a data frame can be written to a CSV file
x <- data.frame(A=c(1,2,3))


