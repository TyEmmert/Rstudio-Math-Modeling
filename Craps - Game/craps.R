dice <- c(1,2,3,4,5,6)
sample(dice, 2)
roll <- function() {sum(sample(dice, 2, replace = TRUE))}
roll()

craps = function() {
  x <<- roll()
  print(x)
  
  if (x == 7 || x == 11) {
    print("Win")
    return(TRUE)
  }
  else if (x == 2 || x == 3 || x == 12) {
    print("Lose")
    return(FALSE)
    
  }
  else {
    print(paste("Roll again. You lose if you roll a 7. You win if you get a", x))
    while (TRUE) {
      y <<- roll()
      print(y)
      if (x == y) {
        print("Win")
        return(TRUE)
      }
      else if (y == 7) {
        print("Lose")
        return(FALSE)
      }
      else {
        print("Roll again")
      }
    }
  }
}
craps()


cleancraps = function () {
  x <- roll()
  
  if (x == 7 || x == 11) {
    return(TRUE)
  }
  else if (x == 2 || x == 3 || x == 12) {
    return(FALSE)
    
  }
  else {
    while (TRUE) {
      y <- roll()
      if (x == y) {
        return(TRUE)
      }
      else if (y == 7) {
        return(FALSE)
      }
    }
  }
}


chancetowincraps = function () {
  return(mean(replicate(100000, cleancraps())))
}
chancetowincraps()
