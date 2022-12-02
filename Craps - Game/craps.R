dice <- c(1,2,3,4,5,6) #Create a die
sample(dice, 2) #roll 2 die
roll <- function() {sum(sample(dice, 2, replace = TRUE))} #create a function that rolls two dice and sums up their total.
roll()

craps = function() { #creating a craps function that prints out what is happening through each process
  x <<- roll() #store your first roll
  print(x) #print what that roll is
  
  if (x == 7 || x == 11) { #You win if you rolled a 7 or 11.
    print("Win") #print win
    return(TRUE) #return true for later calculating the percent chance to win
  }
  else if (x == 2 || x == 3 || x == 12) { #If you rolled a 2, 3, or 12, you lose.
    print("Lose") #print lose
    return(FALSE) #return false for later calculating the percent chance to win
    
  }
  else {
    print(paste("Roll again. You lose if you roll a 7. You win if you get a", x)) #You didn't win or lose, so we need to roll again.
    while (TRUE) { #while loop that continuously runs until we get a return()
      y <<- roll() #Store a second roll in a different variable other than x.
      print(y) #print the second roll
      if (x == y) { #check if your second roll is the same as your first one.
        print("Win") #print you win
        return(TRUE) #return true for later calculating the percent chance to win
      }
      else if (y == 7) { #You lose if you get a 7 on any consecutive roll
        print("Lose") #print lose
        return(FALSE) #return false for later calculating the percent chance to win
      }
      else {
        print("Roll again") #roll until you get a 7 or your first roll
      }
    }
  }
}
craps()


cleancraps = function () { #a clean craps function that doesn't print commands, and only produces a true for win or false for lose.
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


chancetowincraps = function () { #new function that runs 100000 games of craps
  return(mean(replicate(100000, cleancraps()))) #replicates cleancraps() 100000 times. this returns a vector of true/fase values. takes the mean of the true false values. returns
}
chancetowincraps()
