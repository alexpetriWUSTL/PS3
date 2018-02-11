#s3

doorNum <- c(1, 2, 3) #Write out the possible list of door numbers
class(doorNum) <- "Door" #Assign doorNum to class Door
PlayGame <- function(x,...){ #Write out the generic function
  UseMethod("PlayGame")
}
PlayGame.Door <- function(x){ #Write out my specific method
  car <- sample(doorNum, 1) #Produce a sample number for where the car is
  if(identical(x, car)) { #if the user picks the car door, they return a congrats statement
    print("Congratulations, you have chosen the correct door")
  } else { #otherwise...
    print("Sorry, you have chosen the incorrect door")
  }
}
PlayGame.Door(doorNum[[3]]) #call PlayGame.Door and choose your door number to see if you're correct!
methods(class = "Door") #Use methods() to ensure PlayGame is in class "Door"





#S4
