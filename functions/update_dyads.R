update.dyads <- function(A, B, A_choice, B_choice, matrix, dyads){

# find the box the agents end with
  if        (A_choice == "LA1" & B_choice == "LB1") {
    box = 1
  } else if (A_choice == "LA2" & B_choice == "LB1") {
    box = 2 
  } else if (A_choice == "LA1" & B_choice == "LB2") {
    box = 3
  } else if (A_choice == "LA2" & B_choice == "LB2") {
    box = 4
  }

# find each agents outcome
ao <- which(matrix$agent == "A" & matrix$box == box)
A_outcome <- matrix$outcome[ao]

bo <- which(matrix$agent == "B" & matrix$box == box)
B_outcome <- matrix$outcome[bo]

# update dyads: sat, dep, tick_tog and outcome
A_B <- which(dyads$agent_1 == A & dyads$agent_2 == B) # find the row no for the relevant dyad
dyads$outcome[A_B] <- A_outcome # add the outcome
dyads$ticks_tog[A_B] <- dyads$ticks_tog[A_B] + 1 # add a tick to ticks spent together
A_trait_index <- which(traits$agent == A) # Get agent A's index in traits


dyads$sat[A_B] <- dyads$sat[A_B] + ((A_outcome - traits$cl_sat[A_trait_index]) # update sat
                                    /dyads$ticks_tog[A_B])

dyads$dep[A_B] <- dyads$dep[A_B] + ((A_outcome - traits$cl_alt[A_trait_index]) # update dep
                                    /dyads$ticks_tog[A_B])

dyads$commit[A_B] <- dyads$commit[A_B] + (traits$inv[A_trait_index] * dyads$ticks_tog[A_B]) # update commit

# repeat for B
B_A <- which(dyads$agent_1 == B & dyads$agent_2 == A) # find the row no for the relevant dyad
dyads$outcome[B_A] <- B_outcome # add the outcome
dyads$ticks_tog[B_A] <- dyads$ticks_tog[B_A] + 1 # add a tick to ticks spent together
B_trait_index <- which(traits$agent == B) # Get agent B's index in traits


dyads$sat[B_A] <- dyads$sat[B_A] + ((B_outcome - traits$cl_sat[B_trait_index]) # update sat
                                    /dyads$ticks_tog[B_A])

dyads$dep[B_A] <- dyads$dep[B_A] + ((B_outcome - traits$cl_alt[B_trait_index]) # update dep
                                    /dyads$ticks_tog[B_A])

dyads$commit[B_A] <- dyads$commit[B_A] + (traits$inv[B_trait_index] * dyads$ticks_tog[B_A]) # update commit

return(dyads)
}


