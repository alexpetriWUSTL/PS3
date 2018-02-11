doorNum <- c(1,2,3)
q <- sample(doorNum, 1)
q
class(doorNum) <- "Door"
PlayGame <- function(x,...){
  UseMethod("PlayGame")
}
PlayGame.Door <- function(x){
  if(identical(x, q) { 
    print("Congratulations, you have chosen the correct door")
  } else {
    print("Sorry, you have chosen the incorrect door")
  }
}
PlayGame.Door(3)
