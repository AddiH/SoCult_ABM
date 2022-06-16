################################################################################
########################## Returns choice for agent ###########################
################################################################################

own.max <- function(matrix, agent) {
  
  if (agent == "A"){
    LA1 <- matrix$outcome[1] + matrix$outcome[3] # line A 1: left column of matrix
    LA2 <- matrix$outcome[2] + matrix$outcome[4] # line A 2: right column of matrix
    if (LA1 > LA2){ # choose the line with the higest sum of outcomes
      choice <- "LA1"
    } else if (LA1 < LA2){
      choice <- "LA2"
    } else if (LA1 == LA2){ # or randomly if they are identical
      c <- c("LA2", "LA1")
      choice <- c[sample(1:2,1)]
    }
  } else if (agent == "B"){
    LB1 <- matrix$outcome[1] + matrix$outcome[2] # line B 1: 1st row of matrix
    LB2 <- matrix$outcome[3] + matrix$outcome[4] # line B 2: 2nd row of matrix
    if (LB1 > LB2){ # choose the line with the higest sum of outcomes
      choice <- "LB1"
    } else if (LB1 < LB2){
      choice <- "LB2"
    } else if (LB1 == LB2){ # or randomly if they are identical
      c <- c("LB2", "LB1")
      choice <- c[sample(1:2,1)]
    }
  }
  return(choice)
}