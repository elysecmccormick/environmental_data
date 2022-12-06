# Question 1

c(1, 2, 3)
"c(1, 2, 3)"

# Questions 2-4

c_1 = c(1, 2, 3)
c_2 = "c(1, 2, 3)"

c_1
c_2

#Questions 5 & 6

my_vec <- (1:6)

mat_1 = matrix(my_vec, nrow = 3)

#Retrieve anything with the value 3

my_bool_vec <- my_vec == 3
my_bool_vec  
data.frame(my_vec, my_bool_vec)  


my_vec[my_bool_vec == TRUE]

#Questions 7-11

#Create mat_2

mat_2 <- matrix(my_vec, nrow = 2, ncol = 3)

#Create mat_3

mat_3 <- matrix(my_vec, nrow = 3, ncol =2)

#Create mat_4

mat_4 <- matrix(my_vec, nrow = 2, ncol = 5)

#Warning message:
#  In matrix(my_vec, nrow = 2, ncol = 5) :
# data length [6] is not a sub-multiple or multiple of the number of columns [5]

#Questions 12-14

v3 <- c(0:5)

my_list_1 <- list("two" = 5.2, "one" = "five point two","three" = v3)

my_list_1[[1]]
my_list_1[[as.numeric("1")]]
my_list_1[["1"]]
my_list_1[["one"]]
my_list_1$one
my_list_1$"one"
my_list_1$1
my_list_1$"1"







