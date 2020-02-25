#Gamne of Life

apply_rules <- function(matr){
 
 population <- matr[1,1] + matr[1,2]+ matr[1,3]+ matr[2,1]+ matr[2,3]+ matr[3,1]+ matr[3,2]+ matr[3,3]
 #waprint(population)
 if(population == 3){
   return(1)
 }
 else if(population == 2){
   return(matr[2,2])
 }
 else{ return(0)
 }
 
}



next_gen <- function(oldState){
  new_state <- matrix( runif(nrow(oldState)*ncol(oldState), min =0,max =0) , nrow = nrow(oldState), ncol = ncol(oldState))
  for( i in 2: (nrow(oldState)-1)){
    for(j in 2: (ncol(oldState)-1)){
     
      new_state[i,j] <- apply_rules(oldState[c(i-1,i,i+1) , c(j-1,j,j+1)])
      
    }
  }
  
  return(new_state)
}

game_of_life <- function(N,M,p,nGen){
  initialState <- matrix(runif(N*M), nrow = N)
  initialState[initialState < p] <- 0
  initialState[initialState > p] <- 1
  image(initialState)
  for(i in 1:nGen){
    initialState <- next_gen(initialState)
    image((initialState))
    
    }
} 




game_of_life(150,150,.9,40)

