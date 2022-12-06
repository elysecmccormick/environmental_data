#Create a vector
v1 <- c(-2,-1, 0, 1, 2)
v1

#Create a second vector with multiplication
V2 <- v1 * 3
v2

#Sum v2
sum(V2)

#Create vec_4
vec_4 <- (1:12)

#Create a matrix mat_1
mat_1 <- matrix(vec_4, byrow = TRUE, nrow = 3, ncol = 4)

#Create mat_2
mat_2 <- matrix(vec_4, byrow = FALSE, nrow = 3, ncol = 4)

#Create a list

v3 <- c(0:5)

my_list_1 <- list("two" = 5.2, "one" = "five point two","three" = v3)

my_list_1[[3]]

my_list_1$"one"

my_list_1[["one"]]

#Logical Tests and Subsetting

my_vec = rep(1:3, 5)
my_vec

my_bool_vec <- my_vec > 2
my_bool_vec  
data.frame(my_vec, my_bool_vec)  
  
  
my_vec[my_bool_vec == TRUE]
  
  
  
  
  
  





