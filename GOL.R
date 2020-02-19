#Gamne of Life

apply_rules <- function(matr){
  counter <- 0
  for(l in 1:nrow(matr)){
    for(k in 1:ncol(matr)){
      if(matr[l,k] == 1 & (l != 2 & k!=2)){ #Also make sure i != j
        counter <- counter +1 
      }
  }
  }
  
  if(counter < 2 | counter >3){
    return(0)
  } 
  else{
    return(1)
  }
  
}



next_gen <- function(oldstate){
  new_state <- matrix( , nrow =nrow(oldstate), ncol = ncol(oldstate))
  for( i in 1:(nrow(oldstate))){
    for(j in 1:(ncol(oldstate))){
      if( (i !=1 & j != 1) &(i !=1 & j != 0) &(i !=0 & j != 1)& (i !=0 & j != 0)){
      new_state[i,j] <- apply_rules(oldstate[i-1:i+1,j-1:j+1])
      }
    }
  }
  return(new_state)
}

game_of_life <- function(N,M,p,nGen){
  initialState <- matrix(runif(M*N), ncol = N)
  initialState[initialState < p] <- 0
  initialState[initialState > p] <- 1
  for(i in 1:nGen){
    initialState <- (next_gen(oldstate = initialState))
    image(((next_gen(initialState))))
    }
}

game_of_life(20,20,0.4,10)


