update.dyads <- function(A, B, A_choice, B_choice, matrix, dyads, theory){

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

dyads$ticks_tog[A_B] <- dyads$ticks_tog[A_B] + 1 # add a tick to ticks spent together

################## WOJNG LOOK INTO TRAITS!?!?!?

dyads$sat[A_B] <- dyads$sat[A_B] + ((A_outcome - traits$cl_sat[A]) # update sat
                                    /dyads$ticks_tog[A_B])

dyads$dep[A_B] <- dyads$dep[A_B] + ((A_outcome - traits$cl_alt[A]) # update dep
                                    /dyads$ticks_tog[A_B])

dyads$commit[A_B] <- dyads$commit[A_B] + (traits$inv[A] * dyads$ticks_tog[A_B])

dyads$outcome[A_B] <- A_outcome # add the outcome

# repeat for B

B_A <- which(dyads$agent_1 == B & dyads$agent_2 == A) # find the row no for the relevant dyad

dyads$ticks_tog[B_A] <- dyads$ticks_tog[B_A] + 1 # add a tick to ticks spent together

dyads$sat[B_A] <- dyads$sat[B_A] + ((A_outcome - traits$cl_sat[B]) # update sat
                                    /dyads$ticks_tog[B_A])

dyads$dep[B_A] <- dyads$dep[B_A] + ((A_outcome - traits$cl_alt[B]) # update dep
                                    /dyads$ticks_tog[B_A])

dyads$outcome[B_A] <- B_outcome # add the outcome

return(dyads)
}


