#Gamne of Life

apply_rules <- function(mymat){
  counter <- 0
  for(i in nrow(mymat)){
    for(j in ncol(mymat)){
      if(mymat[i,j] == 1){ #Also make sure i != j
        counter <- counter +1 
      }
  }
  }
  
  if(counter < 2 & counter >3){
    return(0)
  } 
  else{
    return(1)
  }
  
}



next_gen <- function(oldstate){
  new_state <- matrix(runif(n = nrow(oldstate)*ncol(oldstate), min =0, max =0) ,nrow(oldstate),ncol(oldstate))
  for( i in (nrow(oldstate))-3){
    for(j in (ncol(oldstate))-3){
      if(i > 1 & j > 1){
      
      new_state[i,j] <- apply_rules(oldstate[i:i+3,j:j+3])
      }
    }
  }
  return(new_state)
}

game_of_life <- function(N,M,p,nGen){
  initialState <- matrix(runif(N*M), ncol =N)
  initialState[initialState < p] <- 0
  initialState[initialState > p] <- 1
  for(i in nGen){
    image(next_gen(initialState))
  }
}

game_of_life(90,90,0.4,600)
