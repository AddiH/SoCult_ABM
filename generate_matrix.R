################################################################################################
# This function returns the matrix for relecant agents
################################################################################################

generate.matrix <- function(A, B, dyads, matrix, ka, kp) {

  #### calculate outcomes for agent A ####
  
  A_B <- dyads %>% filter(agent_1 == A & agent_2 == B) # find relevant dyad

  # calculate ac and pc
  if (A_B$dep > 0){ # if dep is pos
    ac <- ka / A_B$dep
    pc <- A_B$dep / kp
  } 
  if (A_B$dep < 0){ # if dep is neg
    ac <- A_B$dep / ka
    pc <- kp / A_B$dep
  }
  
  # add ac in correct boxes
  if (ac > 0){ # if ac is pos
    matrix$outcome[1] <- abs(ac)
    matrix$outcome[3] <- abs(ac)
  }
  if (ac < 0){ # if ac is neg
    matrix$outcome[2] <- abs(ac)
    matrix$outcome[4] <- abs(ac)
  }
  
  # add pc in correct boxes
  if (pc > 0){ # if pc is pos
    matrix$outcome[1] <- abs(pc) + matrix$outcome[1]
    matrix$outcome[2] <- abs(pc) + matrix$outcome[2]
  }
  if (pc < 0){ # if pc is neg
    matrix$outcome[3] <- abs(pc) + matrix$outcome[3]
    matrix$outcome[4] <- abs(pc) + matrix$outcome[4]
  }
  
  # if intercept = TRUE do it here
  
  #### calculate outcomes for agent B ####
  
  B_A <- dyads %>% filter(agent_1 == B & agent_2 == A) # find relevant dyad
  
  # calculate ac and pc
  if (B_A$dep > 0){ # if dep is pos
    ac <- ka / B_A$dep
    pc <- B_A$dep / kp
  } 
  if (B_A$dep < 0){ # if dep is neg
    ac <- B_A$dep / ka
    pc <- kp / B_A$dep
  }
  
  # add ac in correct boxes
  if (ac > 0){ # if ac is pos
    matrix$outcome[1+4] <- abs(ac)
    matrix$outcome[2+4] <- abs(ac)
  }
  if (ac < 0){ # if ac is neg
    matrix$outcome[3+4] <- abs(ac)
    matrix$outcome[4+4] <- abs(ac)
  }
  
  # add pc in correct boxes
  if (pc > 0){ # if pc is pos
    matrix$outcome[1+4] <- abs(pc) + matrix$outcome[1+4]
    matrix$outcome[3+4] <- abs(pc) + matrix$outcome[3+4]
  }
  if (pc < 0){ # if pc is neg
    matrix$outcome[2+4] <- abs(pc) + matrix$outcome[2+4]
    matrix$outcome[4+4] <- abs(pc) + matrix$outcome[4+4]
  }
  
  matrix$outcome <- round(matrix$outcome,2)
  
  return(matrix)
  }# end of function
