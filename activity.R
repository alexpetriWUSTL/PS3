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
stud <- studentmaker("Draco") #vectorize the studentmaker by labelling the student


matrixHog <- matrix(sample(1:100, 16), ncol = 4) #a matrix to weigh the traits of each student

#Part 2
sort.student <- function(x, y){ #method to sort the students
  if(length(y) != 16){ #if the matrix inputed isn't 16 (due to 1*4 matrix for the student)
   return("Second argument must be a matrix with 16 observations")
  }
  a = matrix(c(x$courage, x$ambition, x$intelligence, x$effort), ncol = 1) #subset the student matrix
  b = y%*%a #multiply the matrices
  if(identical(b[1], max(b))){ #find the weighted averages of each calculated matrix to return the student's house
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


house <- sort.student(stud, matrixHog) #vectorize the house with appropriate x and y

#Part 3

if(house == "GRYFFINDOR!"){  #assign new classes using structure which takes a vector of values and assigns them class(es)
  stud <- structure(stud, class = c("student", "Gryffindor"))
} else if (house == "SLYTHERIN!") {
  stud <- structure(stud, class = c("student", "Slytherin"))
} else if (house == "RAVENCLAW!") {
  stud <- structure(stud, class = c("student", "Ravenclaw"))
} else if (house == "HUFFLEPUFF!"){
  stud <- structure(stud, class = c("student", "Hufflepuff"))
}


#Part 4

Gryffindor_Tower <- new.env() #set up 4 new environments
Black_Lake <- new.env()
Ravenclaw_Tower <- new.env()
Basement <- new.env()



curfew <- function (x) { #create a curfew method
  UseMethod("curfew")
}

curfew.Gryffindor <- function(x){ #if the second class is gryffindor, assign the student to Gryffindor_Tower
  if(class(x)[2] %in% c("Gryffindor") == TRUE){ 
    assign(deparse(substitute(x)), x, envir = Gryffindor_Tower)
    name <- deparse(substitute(x))
    rm(list = ls(envir=globalenv())[grep(name, ls(envir = globalenv()))], envir = globalenv()) #unlist the student from the global environment
  } 
}

curfew.Slytherin <- function(x){
  if (class(x)[2] %in% c("Slytherin") == TRUE) {#if the second class is Slytherin, assign the student to Black_Lake
    assign(deparse(substitute(x)), x, envir = Black_Lake)
    name <- deparse(substitute(x))
    rm(list = ls(envir=globalenv())[grep(name, ls(envir = globalenv()))], envir = globalenv())
  }  
} 

curfew.Ravenclaw <- function(x){
  if (class(x)[2] %in% c("Ravenclaw") == TRUE) {#if the second class is Ravenclaw, assign the student to Ravenclaw_Tower
    assign(deparse(substitute(x)), x, envir = Ravenclaw_Tower)
    name <- deparse(substitute(x))
    rm(list = ls(envir=globalenv())[grep(name, ls(envir = globalenv()))], envir = globalenv())
  }
}

curfew.Hufflepuff <- function(x){#if the second class is Hufflepuff, assign the student to Basement
  if (class(x)[2] %in% c("Hufflepuff") == TRUE){
  assign(deparse(substitute(x)), x, envir = Basement)
    name <- deparse(substitute(x))
    rm(list = ls(envir=globalenv())[grep(name, ls(envir = globalenv()))], envir = globalenv())
  }
} 

curfew(stud) #use Curfew to assign the student to their new environment
ls(Black_Lake) #list whichever environment to ensure stud was assigned
  
