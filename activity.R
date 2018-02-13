#Part 1
studentmaker <- function(person){ #write a function that takes labels a students traits
  name1 <- person
  ambition1 <- sample(1:100,1)
  intelligence1 <- sample(1:100,1)
  courage1 <- sample(1:100,1)
  effort1 <- sample(1:100,1)
  student <- list(name = name1,courage = courage1,ambition = ambition1,intelligence = intelligence1,effort = effort1) #creates a vector of the student and all of their traits
  class(student) <- "student" #creates the class for student
  return(student)
}
stud <- studentmaker("Draco") #


matrixHog <- matrix(sample(1:100, 16), ncol = 4) #a matrix to weigh the traits of each student

#Part 2
sort.student <- function(x, y){
  if(length(y) != 16){
   return("Second argument must be a matrix with 16 observations")
  }
  a = matrix(c(x$courage, x$ambition, x$intelligence, x$effort), ncol = 1)
  b = y%*%a
  if(identical(b[1], max(b))){
    return("GRYFFINDOR!")
  } else if(identical(b[2], max(b))){
    return("SLYTHERIN!")
  } else if(identical(b[3], max(b))){
    return("RAVENCLAW!")
  } else if(identical(b[4], max(b))){
    return("HUFFLEPUFF!")
  } else{
    return("I can't decide!")
  }
}


house <- sort.student(stud, matrixHog)

#Part 3

if(house == "GRYFFINDOR!"){ 
  stud <- structure(stud, class = c("student", "Gryffindor"))
} else if (house == "SLYTHERIN!") {
  stud <- structure(stud, class = c("student", "Slytherin"))
} else if (house == "RAVENCLAW!") {
  stud <- structure(stud, class = c("student", "Ravenclaw"))
} else if (house == "HUFFLEPUFF!"){
  stud <- structure(stud, class = c("student", "Hufflepuff"))
}


#Part 4

Gryffindor_Tower <- new.env()
Black_Lake <- new.env()
Ravenclaw_Tower <- new.env()
Basement <- new.env()



curfew <- function (y) {
  if(class(y)[2] %in% c("Gryffindor") == TRUE){ 
    assign(deparse(substitute(y)), y, envir = Gryffindor_Tower)
  } else if (class(y)[2] %in% c("Slytherin") == TRUE) {
    assign("y", y, envir = Black_Lake)
  } else if (class(y)[2] %in% c("Ravenclaw") == TRUE) {
    assign("y", y, envir = Ravenclaw_Tower)
  } else if (class(y)[2] %in% c("Hufflepuff") == TRUE){
    assign(deparse(substitute(y)), y, envir = Basement)
  }
  rm(as.character(y$name), envir = globalenv())
}

curfew(stud)
ls(Basement)
  
