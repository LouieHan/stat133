# Homework 6
# Stat 133, Lec 2, Spring 2015
# Due : Friday March 20th by 5 pm

# Review the slides on simulations for this assignment.

# Consider the following model on use of a new drug:
# We have a population of doctors, population size : <n.doctors>
# Every doctor has either adopted the use of a new drug, or not (0/1 status)
# Now write a function that runs a simulation for a period of :
# <n.days> where
# - every day exactly two _random_ doctors meet
# - if one has adopted but the other one has not then the
#   holdout adopts the use of the drug with probability p
# Return a matrix that shows for each day which doctors have adopted
# the use of the drug.

# Input varibles are
# <n.days> : the number of days the simulation should be run for
# <n.doctors> : total number of doctors 
# <initial.doctors> : a 0/1 vector of length <n.doctors>, 1 for adopters
# <p> : the probability that the non-adopter adopts the drug.

# Ouput variable
# <has_adopted> : matrix with <n.doctors> rows and <n.days> columns
#                 i.e. one row for each doctor
#                 the entries are 0 for days where the doctor is a
#                 non-adopter, else 1 (so once a row turns to 1 it stays as 1).

sim.doctors <- function(initial.doctors, n.doctors, n.days, p){
  if (p>1){
    return("p greater than 1")
  }
  #checks if p is greater than 1
  if (length(initial.doctors) > n.doctors){
    return("initial.doctors and n.doctors are not equal")
  }
  #checks if length of initial.doctors is equal to n.doctors so we can construct a matrix
  if (n.doctors > length(initial.doctors)){
    return("initial.doctors and n.doctors are not equal")
  }
  #checks if length of initial.doctors is equal to n.doctors so we can construct a matrix
  i = 0
  for (i in 1:length(initial.doctors)){
    if (initial.doctors[i] > 1){
      return("initial.doctors does not contain only 0s or 1s")
    }
  }
  #check if initial.doctors is a vector with only values 0 or 1
  seqseq = 1:length(initial.doctors)
  #sequence of integers from 1 to lenth of initial.doctors so we can construct a matrix
  has_adopted <- matrix(c(seqseq, initial.doctors), nrow = 2, ncol = n.doctors, byrow = TRUE)
  #creates a matrix with sequence of 1 to n.doctors in the first row and our values of initial.doctors in the second row
  for (i in 1:n.days){
    dum <- runif(1,0,1)
    #creates a decimal figure to compare p against
    get <- sample(seqseq, 2, replace=F)
    #selects random integers from the values from 1 to n.doctors
    if (has_adopted[2,get[1]] > has_adopted[2,get[2]]){
      if (dum <= p){
        has_adopted[2,get[2]] = has_adopted[2,get[1]]
      } 
    }
    #this loop compares two random matrix elements against each other.
    #if one doctor adopted the method and another one didn't, the one who did not will adopt it with probability p
    if (has_adopted[2,get[2]] > has_adopted[2,get[1]]){
      if (dum <= p){
        has_adopted[2,get[1]] = has_adopted[2,get[2]]
      }
    #this loop compares two random matrix elements against each other.
    #if one doctor adopted the method and another one didn't, the one who did not will adopt it with probability p
    if (sum(has_adopted[2,]) == n.doctors){
      return(has_adopted)
    }
    #saves runtime because once all elements are 1, we return the matrix early
      i = i + 1
    #raise counter
    }
  }
  return(has_adopted)
}


sim.doctors(c(1,0,0,0,0,0,0,0,0,0), 10, 17, 0.8)

# When you test your function you have to generate <initial.doctors> and
# pick values for the other input parameters.

set.seed(17)
"
base <- function(){
  rough <- sample(x = c(0,1),size = 10, prob = c(0.9,0.1), replace = TRUE)
  if(is.element(1,rough) != TRUE){
    return(base())
  }
  else 
    return(rough)
}
"

initial.doctors <- sample(c(0, 1), 17, prob = c(0.9, 0.1), replace = TRUE)
sim1 <- sim.doctors(initial.doctors, length(initial.doctors), n.days = 17, p = 0.1)
sim2 <- sim.doctors(initial.doctors, length(initial.doctors), n.days = 17, p = 0.3)
sim3 <- sim.doctors(initial.doctors, length(initial.doctors), n.days = 17, p = 0.5)
sim4 <- sim.doctors(initial.doctors, length(initial.doctors), n.days = 17, p = 0.65)
sim5 <- sim.doctors(initial.doctors, length(initial.doctors), n.days = 17, p = 0.8)


plot(c(1:17), type = "l")
lines(c(1:17), type = "l")
lines(c(1:17), type = "l")
lines(c(1:17), type = "l")
lines(c(1:17), type = "l")


# Generate a value for <initial.doctors> that has 10% 1s and 90% 0s.
# Run your function for at least 5 different values of <p> and plot
# on x-axis: days,
# on y-axis : the number of doctors that have already adopted the drug, on that day
# Put all 5 lines in one figure (e.g. use first plot() then lines() for the subsequent lines)

"
dexter <- matrix(c(1,2,3, 11,12,13), nrow = 2, ncol = 3, byrow = TRUE)
"
# Set up the output variable, define it as a matrix then use initial.doctors
# to set the first column (day)

# Run a simulation for <n.days> (use a for loop).  In the loop:
# 1) pick two random doctors
# 2) check if one has adopted the other hasn't
# 3) convert the non-adopter with probability p

# return the output