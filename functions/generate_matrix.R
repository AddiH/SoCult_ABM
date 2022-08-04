################################################################################
############# This function returns the matrix for relevant agents #############
################################################################################

generate.matrix <- function(A, B, dyads, matrix, ka, kp, intercept) {

  #### calculate outcomes for agent A ####
  
  A_B <- dyads %>% filter(agent_1 == A & agent_2 == B) # find relevant dyad

  # calculate ac and pc
  if (A_B$dep > 0){ # if dep is pos
    ac <- ka / A_B$dep
    pc <- A_B$dep / kp
  } else if (A_B$dep < 0){ # if dep is neg
    ac <- A_B$dep / ka
    pc <- kp / A_B$dep
  } else if (A_B$dep == 0) { # if dep is 0
    ac <- 0
    pc <- 0
  }
  
  # add ac in correct boxes
  if (ac > 0){ # if ac is pos
    matrix$outcome[1] <- abs(ac)
    matrix$outcome[3] <- abs(ac)
  } else if (ac < 0){ # if ac is neg
    matrix$outcome[2] <- abs(ac)
    matrix$outcome[4] <- abs(ac)
  }
  
  # add pc in correct boxes
  if (pc > 0){ # if pc is pos
    matrix$outcome[1] <- abs(pc) + matrix$outcome[1]
    matrix$outcome[2] <- abs(pc) + matrix$outcome[2]
  } else if (pc < 0){ # if pc is neg
    matrix$outcome[3] <- abs(pc) + matrix$outcome[3]
    matrix$outcome[4] <- abs(pc) + matrix$outcome[4]
  }
  
  # add intercept
  if (intercept == "random"){  
    matrix$outcome[1] <- matrix$outcome[1] + sample(-10:10, 1)  
    matrix$outcome[2] <- matrix$outcome[2] + sample(-10:10, 1)  
    matrix$outcome[3] <- matrix$outcome[3] + sample(-10:10, 1)  
    matrix$outcome[4] <- matrix$outcome[4] + sample(-10:10, 1)  
  }
  
  A_B_i <- which(dyads$agent_1 == A & dyads$agent_2 == B) # find the row no for the relevant dyad
  dyads$ac[A_B_i] <- ac
  dyads$pc[A_B_i] <- pc
  
  #### calculate outcomes for agent B ####
  
  B_A <- dyads %>% filter(agent_1 == B & agent_2 == A) # find relevant dyad
  
  # calculate ac and pc
  if (B_A$dep > 0){ # if dep is pos
    ac <- ka / B_A$dep
    pc <- B_A$dep / kp
  } else if (B_A$dep < 0){ # if dep is neg
    ac <- B_A$dep / ka
    pc <- kp / B_A$dep
  } else if (A_B$dep == 0) { # if dep is 0
    ac <- 0
    pc <- 0
  }
  
  # add ac in correct boxes
  if (ac > 0){ # if ac is pos
    matrix$outcome[1+4] <- abs(ac)
    matrix$outcome[2+4] <- abs(ac)
  } else if (ac < 0){ # if ac is neg
    matrix$outcome[3+4] <- abs(ac)
    matrix$outcome[4+4] <- abs(ac)
  }
  
  # add pc in correct boxes
  if (pc > 0){ # if pc is pos
    matrix$outcome[1+4] <- abs(pc) + matrix$outcome[1+4]
    matrix$outcome[3+4] <- abs(pc) + matrix$outcome[3+4]
  } else if (pc < 0){ # if pc is neg
    matrix$outcome[2+4] <- abs(pc) + matrix$outcome[2+4]
    matrix$outcome[4+4] <- abs(pc) + matrix$outcome[4+4]
  }
  
  matrix$outcome <- round(matrix$outcome,2)
  
  B_A_i <- which(dyads$agent_1 == B & dyads$agent_2 == A) # find the row no for the relevant dyad
  dyads$ac[B_A_i] <- ac
  dyads$pc[B_A_i] <- pc
  
  return(matrix)
  }# end of function
