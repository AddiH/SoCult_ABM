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
ao <- which(matrix$agent == "A" & matrix$box == box) # find "raw" outcome
A_out <- matrix$outcome[ao] # the  outcome

bo <- which(matrix$agent == "B" & matrix$box == box) # find "raw" outcome
B_out <- matrix$outcome[bo] # the  outcome

# update dyads: outcome, sat, dep and commit
A_B_i <- which(dyads$agent_1 == A & dyads$agent_2 == B) # find the row no for the relevant dyad
dyads$ticks_tog[A_B_i] <- dyads$ticks_tog[A_B_i] + 1 # add a tick to ticks spent together

A_i <- which(traits$agent == A) # Get agent A's index in traits

dyads$outcome[A_B_i] <- A_out # add outcomes and adjusted outcomes
dyads$out_sat[A_B_i] <- A_out - traits$cl_sat[A_i]
dyads$out_dep[A_B_i] <- A_out - traits$cl_alt[A_i]


prev_out <- dyads_his %>% # find the previous outcomes
  filter(agent_1 == A & agent_2 == B)

dyads$sat[A_B_i] <- (sum(prev_out$out_sat) + dyads$out_sat[A_B_i]) / dyads$ticks_tog[A_B_i] # update sat

dyads$dep[A_B_i] <- (sum(prev_out$out_dep) + dyads$out_dep[A_B_i]) / dyads$ticks_tog[A_B_i] # update dep

dyads$commit[A_B_i] <- dyads$commit[A_B_i] + (traits$inv[A_i] * dyads$ticks_tog[A_B_i]) # update com

# repeat for B
B_A_i <- which(dyads$agent_1 == B & dyads$agent_2 == A) # find the row no for the relevant dyad
dyads$ticks_tog[B_A_i] <- dyads$ticks_tog[B_A_i] + 1 # add a tick to ticks spent together

B_i <- which(traits$agent == B) # Get agent B's index in traits

dyads$outcome[B_A_i] <- B_out # add outcomes and adjusted outcomes
dyads$out_sat[B_A_i] <- B_out - traits$cl_sat[B_i]
dyads$out_dep[B_A_i] <- B_out - traits$cl_alt[B_i]


prev_out <- dyads_his %>% # find the previous outcomes
  filter(agent_1 == A & agent_2 == B)

dyads$sat[B_A_i] <- (sum(prev_out$out_sat) + dyads$out_sat[B_A_i]) / dyads$ticks_tog[B_A_i] # update sat

dyads$dep[B_A_i] <- (sum(prev_out$out_dep) + dyads$out_dep[B_A_i]) / dyads$ticks_tog[B_A_i] # update dep

dyads$commit[B_A_i] <- dyads$commit[B_A_i] + (traits$inv[B_i] * dyads$ticks_tog[B_A_i]) # update com

return(dyads)
}


