update.dyads <- function(A, B, A_choice, B_choice, matrix, dyads, tick, cl, cl_timeline, sort){

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
dyads$date[A_B_i] <- T # mark that the agents had a date this tick

dyads$outcome[A_B_i] <- A_out # add outcomes and adjusted outcomes
dyads$out_sat[A_B_i] <- A_out - dyads$cl_sat[A_B_i]
dyads$out_dep[A_B_i] <- A_out - dyads$cl_alt[A_B_i]


prev_out_A_B <- dyads_his %>% # find the previous outcomes
  filter(agent_1 == A & agent_2 == B)

dyads$sat[A_B_i] <- (sum(prev_out_A_B$out_sat) + dyads$out_sat[A_B_i]) / dyads$ticks_tog[A_B_i] # update sat

if (sort == "sat"){
  dyads$dep[A_B_i] <- (sum(prev_out_A_B$out_dep) + dyads$out_dep[A_B_i]) / dyads$ticks_tog[A_B_i] # update dep
} else if (sort == "commit"){
  dyads$dep[A_B_i] <- ((sum(prev_out_A_B$out_dep) + dyads$out_dep[A_B_i]) / dyads$ticks_tog[A_B_i])  # update dep
}

dyads$commit[A_B_i] <- dyads$inv[A_B_i] * dyads$ticks_tog[A_B_i] + dyads$dep[A_B_i] # update com


# repeat for B
B_A_i <- which(dyads$agent_1 == B & dyads$agent_2 == A) # find the row no for the relevant dyad
dyads$ticks_tog[B_A_i] <- dyads$ticks_tog[B_A_i] + 1 # add a tick to ticks spent together
dyads$date[B_A_i] <- T # mark that the agents had a date this tick

dyads$outcome[B_A_i] <- B_out # add outcomes and adjusted outcomes
dyads$out_sat[B_A_i] <- B_out - dyads$cl_sat[B_A_i]
dyads$out_dep[B_A_i] <- B_out - dyads$cl_alt[B_A_i]


prev_out_B_A <- dyads_his %>% # find the previous outcomes
  filter(agent_1 == B & agent_2 == A)

dyads$sat[B_A_i] <- (sum(prev_out_B_A$out_sat) + dyads$out_sat[B_A_i]) / dyads$ticks_tog[B_A_i] # update sat

if (sort == "sat"){
  dyads$dep[B_A_i] <- (sum(prev_out_B_A$out_dep) + dyads$out_dep[B_A_i]) / dyads$ticks_tog[B_A_i] # update dep
} else if (sort == "commit"){
  dyads$dep[B_A_i] <- ((sum(prev_out_B_A$out_dep) + dyads$out_dep[B_A_i]) / dyads$ticks_tog[B_A_i])  # update dep
}

dyads$commit[B_A_i] <- dyads$inv[B_A_i] * dyads$ticks_tog[B_A_i] + dyads$dep[B_A_i] # update com

# update cl's
if (cl == "dynamic"){

# cl_sat
cl_sat_A_B <- prev_out_A_B %>% # find prev outcomes for this dyad
          filter(date == T) %>%  
  arrange(desc(tick_no)) %>% 
  slice(1:(cl_timeline - 1)) 

    if(length(cl_sat_A_B$tick_no) > (cl_timeline - 1)){ # set correct div for division in next step
      div <- length(cl_sat_A_B$tick_no) + 1
    } else {div <- cl_timeline}

cl_sat_sum_out_A_B <- ((sum(cl_sat_A_B$outcome) + dyads$outcome[A_B_i]) / div) # sum of prev + current outcome / div
dyads$cl_sat[A_B_i] <- cl_sat_sum_out_A_B + dyads$cl_sat_b[A_B_i] # update cl_sat by adding cl_sat_base

# cl_alt
cl_alt_A_B <- dyads_his %>% # find the previous outcomes
  filter(agent_1 == A & agent_2 != B) %>% 
  filter(date == T) %>%  
  arrange(desc(tick_no)) %>% 
  slice(1:(cl_timeline - 1)) 

    if(length(cl_alt_A_B$tick_no) > (cl_timeline - 1)){ # set correct div for division in next step
      div <- length(cl_alt_A_B$tick_no) + 1
    } else {div <- cl_timeline}

cl_alt_sum_out_A_B <- ((sum(cl_alt_A_B$outcome) + dyads$outcome[A_B_i]) / div) # sum of prev + current outcome / div
dyads$cl_alt[A_B_i] <- cl_alt_sum_out_A_B + dyads$cl_alt_b[A_B_i] # update cl_alt by adding cl_alt_base

# repeat for B
# cl_sat
cl_sat_B_A <- prev_out_B_A %>% # find prev outcomes for this dyad
  filter(date == T) %>%  
  arrange(desc(tick_no)) %>% 
  slice(1:(cl_timeline - 1)) 

if(length(cl_sat_B_A$tick_no) > (cl_timeline - 1)){ # set correct div for division in next step
  div <- length(cl_sat_B_A$tick_no) + 1
} else {div <- cl_timeline}

cl_sat_sum_out_B_A <- ((sum(cl_sat_B_A$outcome) + dyads$outcome[B_A_i]) / div) # sum of prev + current outcome / div
dyads$cl_sat[B_A_i] <- cl_sat_sum_out_B_A + dyads$cl_sat_b[B_A_i] # update cl_sat by adding cl_sat_base

# cl_alt
cl_alt_B_A <- dyads_his %>% # find the previous outcomes
  filter(agent_1 == B & agent_2 != A) %>% 
  filter(date == T) %>%  
  arrange(desc(tick_no)) %>% 
  slice(1:(cl_timeline - 1)) 

if(length(cl_alt_B_A$tick_no) > (cl_timeline - 1)){ # set correct div for division in next step
  div <- length(cl_alt_B_A$tick_no) + 1
} else {div <- cl_timeline}

cl_alt_sum_out_B_A <- ((sum(cl_alt_B_A$outcome) + dyads$outcome[B_A_i]) / div) # sum of prev + current outcome / div
dyads$cl_alt[B_A_i] <- cl_alt_sum_out_B_A + dyads$cl_alt_b[B_A_i] # update cl_alt by adding cl_alt_base

}


# dimensions 
aca <- dyads$ac[A_B_i]
pca <- dyads$pc[A_B_i]
jca <- dyads$jc[A_B_i]

acb <- dyads$ac[B_A_i]
pcb <- dyads$pc[B_A_i]
jcb <- dyads$jc[B_A_i]

dyads$LOD[A_B_i] <- (pca^2 + jca^2) / (aca^2 + pca^2 + jca^2)
dyads$BOD[A_B_i] <- pca/jca

dyads$LOD[B_A_i] <- (pcb^2 + jcb^2) / (acb^2 + pcb^2 + jcb^2)
dyads$BOD[B_A_i] <- pcb/jcb

dyads$MOD[A_B_i] <- dyads$LOD[A_B_i] - dyads$LOD[B_A_i]
dyads$MOD[B_A_i] <- dyads$LOD[B_A_i] - dyads$LOD[A_B_i]

COI <- ((acb*pca) + (aca*pcb) + (jcb*jca)) / (acb^2 + pcb^2 + jcb^2 + aca^2 + pcb^2 + jca^2)
  
dyads$COI[B_A_i] <- COI
dyads$COI[B_A_i] <- COI

return(dyads)
}


