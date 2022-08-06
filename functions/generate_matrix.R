################################################################################
############# This function returns the matrix for relevant agents #############
################################################################################

generate.matrix <- function(A, B, dyads, matrix, ka, kp, intercept) {

  #### calculate outcomes for agent A ####
  A_B <- dyads %>% filter(agent_1 == A & agent_2 == B) # find relevant dyad
  A_B_i <- which(dyads$agent_1 == A & dyads$agent_2 == B) # find the row no for the relevant dyad
  
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
  
  # saving
  dyads$ac[A_B_i] <<- ac
  dyads$pc[A_B_i] <<- pc
  
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
    ia1 <- sample(-10:10, 1)
    ia2 <- sample(-10:10, 1)  
    ia3 <- sample(-10:10, 1)  
    ia4 <- sample(-10:10, 1)  
    
    matrix$outcome[1] <- matrix$outcome[1] + ia1  
    matrix$outcome[2] <- matrix$outcome[2] + ia2  
    matrix$outcome[3] <- matrix$outcome[3] + ia3  
    matrix$outcome[4] <- matrix$outcome[4] + ia4  
    
    # calculating jc
    jc <- (ia1 + ia2) - (ia3 + ia4)
    dyads$jc[A_B_i] <<- jc
  }
  
  
  #### calculate outcomes for agent B ####
  
  B_A <- dyads %>% filter(agent_1 == B & agent_2 == A) # find relevant dyad
  B_A_i <- which(dyads$agent_1 == B & dyads$agent_2 == A) # find the row no for the relevant dyad
  
  
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
  
  # saving
  dyads$ac[B_A_i] <<- ac
  dyads$pc[B_A_i] <<- pc
  
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
  
  # add intercept
  if (intercept == "random"){
    ib1 <- sample(-10:10, 1)
    ib2 <- sample(-10:10, 1)  
    ib3 <- sample(-10:10, 1)  
    ib4 <- sample(-10:10, 1)  
    
    matrix$outcome[1+4] <- matrix$outcome[1+4] + ib1  
    matrix$outcome[2+4] <- matrix$outcome[2+4] + ib2  
    matrix$outcome[3+4] <- matrix$outcome[3+4] + ib3  
    matrix$outcome[4+4] <- matrix$outcome[4+4] + ib4  
    
    # calculating jc
    jc <- (ib1 + ib2) - (ib3 + ib4)
    dyads$jc[B_A_i] <<- jc
  }
  
  matrix$outcome <- round(matrix$outcome,2)
  
  return(matrix)
  }# end of function
