#BUS 256a homework 3
rm(list = ls())
gc();
#problem 1
eq_2nd_degree_V1=function(a,b,c){
  condition_sqr_positive = (b*b)-(4*a*c)>=0
  if(!condition_sqr_positive){ print("Xavi the SQRT of a negative number does not exist") } 
  if(condition_sqr_positive){
    if(a == 0) print("Xavi you cannot divide by zero")
    if(a!=0){
    sol_1 = (-b - sqrt(b*b-(4*a*c)))/(2*a) 
    sol_2 = (-b + sqrt(b*b-(4*a*c)))/(2*a)
    } 
  }
  print(paste('solution one is equal to ',sol_1))
  print(paste('solution two is equal to ',sol_2)) 
}
eq_2nd_degree_V1(6,-10,4)

eq_2nd_degree_V2=function(a,b,c){ 
  condition_sqr_positive = (b*b)-(4*a*c)>=0 
  if(!condition_sqr_positive){ print("Xavi the SQRT of a negative number doesnot exist")}
  if(condition_sqr_positive){
    if(a == 0) print("Xavi you cannot divide by zero")
    if(a!=0){
    sol_1 = (-b - sqrt(b*b-(4*a*c)))/(2*a) 
    sol_2 = (-b + sqrt(b*b-(4*a*c)))/(2*a)
    } 
  }
  solution = c(sol_1, sol_2)
  return(solution) 
}
eq_2nd_degree_V2(6,-10,4)

#answer for problem1
#The return results will be different.
#In eq_2nd_degree_V1, the result will be two lines of characters, which contains the string “the solution is…”and the solution of 1 and 2 separately.
#In eq_2nd_degree_V2, the result will be numeric, and the two solutions are in one vector.
#The purpose of two functions is the same, both of them can calculate and return the results of Binary equation.#

#problem 2 Fibonacci sequence
fibonacci=function(N){
  condition_interger = (N %% 1 == 0)
  if (!condition_interger) { print("N must be interger")}
  if (condition_interger){
    if(N <0)  {print("N must be positive")}
    if(N == 0){return(0)}
    if(N == 1) {return(1)}
    if(N == 2) {return(c(1,1))}
    if(N >= 3){ 
      fibonacci <- c()
      fibonacci[1] <- 1
      fibonacci[2] <- 1
      for (i in 3:N)
      fibonacci[i] = fibonacci[i-1] + fibonacci[i-2]
      return(fibonacci)
    }
  }
}
fibonacci(1.5)
fibonacci(-8)
fibonacci(2)
fibonacci(7)
fibonacci(9)
fibonacci(0)
#Problem 3 Matrix Multiplication
matMult = function(A,B){ 
  condition_compatible = (ncol(A) == nrow(B))
  if(!condition_compatible) {print("Wrong dimensions!!")}
  if(condition_compatible){
    M <- matrix(0,nrow=nrow(A),ncol=ncol(B),TRUE)
    for (i in 1:nrow(A)){
      for (j in 1:ncol(B)){
        for (k in 1:ncol(A)) {
          M[i,j] = M[i,j] + A[i,k]*B[k,j]
        }
      }
    }
    return(M)
  }
}    
       
A = matrix(c(0,5,3,5,5,2),nrow=2,ncol=3) 
B = matrix(c(3,3,4,4,-2,-2),nrow=3,ncol=2)
C = matMult(A,B)
print(C)

#problem 4
#step 1:create a list
N = 100000
cards_Received = as.list(1:N)
#step 2:create a matrix and simulation
cards = matrix(data = 0, nrow = 13, ncol = 4)
colnames(cards) = sort(c('Spades','Clubs','Hearts','Diamonds'))
random_cards = sample(1:(13*4))[1:5]
cards[random_cards] = 1
cards
#step 3:Store in the list cards_Received N simulated poker hands
cards_Received = as.list(1:N)
for (i in 1:length(cards_Received)){
  cards = matrix(data = 0, nrow = 13, ncol = 4)
  colnames(cards) = sort(c('Hearts','Clubs','Spades','Diamonds'))
  random_cards = sample(1:(13*4))[1:5]
  cards[random_cards] = 1
  cards_Received[[i]] = cards
}
#step 4:Count the number of times you get 2 cards of the same number and the divide it by the total number
has_a_pair = function(cards){
  solution =0
  if(any(rowSums(cards)==2)){solution = 1} 
  
  return(solution)
}

has_a_pair(cards)
has_a_three = function(cards){
  solution =0
  if(any(rowSums(cards)==3)){solution = 1} 
  
  return(solution)
}
has_a_three(cards)
b = sum(sapply(cards_Received, has_a_three))/N
a = sum(sapply(cards_Received, has_a_pair))/N
print(a/b)
#Problem 5
#5.1 If instead of A,B,C,D,E I follow A,B,D,E,C how long should it take me? 
#45+51+32+44 = 172

#5.2 Can you tell me the shortest path that takes me to all the cities?
a<-matrix(0,5,5)
a[1,2]=45;a[1,3]=89;a[1,4]=78;a[1,5]=24
a[2,3]=70;a[2,4]=51;a[2,5]=18;
a[3,4]=104;a[3,5]=44;
a[4,5]=32;
moving<-a+t(a)
moving[moving==0]<-Inf
rownames(moving) <- c('City A','City B','City C','City D','City E') 
colnames(moving) <- c('City A','City B','City C','City D','City E')   
moving


####定义Floyd函数
floyd<-function(A){
  n<-nrow(A)
  D<-A
  path<-matrix(0,n,n)
  for(i in 1:n){
    for(j in 1:n){
      if(is.finite(D[i,j])==T){path[i,j]=j}
    }
  }
  for(k in 1:n){
    for(i in 1:n){
      for(j in 1:n){
        if(D[i,k]+D[k,j]<D[i,j]){
          D[i,j]=D[i,k]+D[k,j];
          path[i,j]=path[i,k]
        }
      }
    }
  }
  return(list(D=D,path=path))
}

floyd(moving)


## answer
rm(list=ls())
install.packages('combinat')
library(combinat); # install.packages('combinat')
set.seed(123456)

# Construct dist_matrix
n_cities              = 8
dist_matrix           = matrix(runif(n_cities*n_cities),n_cities,n_cities)
dist_matrix           = t(dist_matrix)%*%dist_matrix
diag(dist_matrix)     = 0
dist_matrix
colnames(dist_matrix) = LETTERS[1:n_cities]
rownames(dist_matrix) = LETTERS[1:n_cities]
possible_paths = permn(LETTERS[1:ncol(dist_matrix)])  #permutataions  排列组合

# Find the distances

dist_calculator_given_one_path = function(x) {
  counter = 0
  for(i in 2:length(x)){
    counter =  .GlobalEnv$dist_matrix[x[i-1],x[i]] + counter  #  .GlobalEnv environment access
  } 
  return(counter)
}
x = possible_paths
x[1]
x[2]

# Computing the solution
distances  = sapply(X =  possible_paths,
                    FUN = dist_calculator_given_one_path)
min_distance = which.min(distances)
path         = permn(LETTERS[1:ncol(dist_matrix)])[min_distance]
path
