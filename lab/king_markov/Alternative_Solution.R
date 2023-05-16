inhabitant = 1000*abs(sin((1:10)/2))
plot(inhabitant)

num_weeks <- 1e5
positions <- rep(0,num_weeks) 
current <- 5
for ( i in 1:num_weeks ) {
  ## record current position
  positions[i] <- current
  ## flip coin to generate proposal
  proposal <- current + sample( c(-1,1) , size=1 ) ## now make sure he loops around the archipelago
  if ( proposal < 1 ) proposal <- 10
  if ( proposal > 10 ) proposal <- 1 ## move?
  prob_move <- inhabitant[proposal]/inhabitant[current]
  current <- ifelse( runif(1) < prob_move , proposal , current ) 
}
plot(positions[1:100]) 
lines(positions[1:100]) 
plot(table(positions)/num_weeks)
