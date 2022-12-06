#Questions 1 and 2 - Logical Subsetting I

n = 12345
vec_1 = sample(12, n, replace = TRUE)
head(vec_1)

vec_2 <- vec_1 == 3
vec_1[vec_2]

#Questions 3-5

n = 12345
vec_1 = sample(12, n, replace = TRUE)
head(vec_1)

length(vec_1)

sum(vec_1 == 3)

n = 10
vec_1 = sample(12, n, replace = TRUE)
paste0("Sum of elements with value 3: ", sum(vec_1 == 3))

#Question 6

for (i in 1:10)
{
  print(i)
}


for (i in 1:10)
{
  print(paste0("This is loop iteration: ", i))
}

# Question 7

n <- 1:17
for (i in n)
{
  print(i)
}

# Question 8

n <- 17
vec_1 = sample(10, n, replace = TRUE)
for (i in 1:n)
{
  print(paste0("The element of vec_1 at index " ,  i  , " is ", vec_1[i]))
}


#Question 9


create_and_print_vec = function(n, min = 1, max = 10)
{
     vec_1 = sample(x = min:max, size = n, replace = TRUE)

    for(i in 1:n)
  {
    print(paste0("The element at index " ,  i  , " is ", vec_1[i]))
    }
}
create_and_print_vec(10, min = 1, max = 10)
    
  


















