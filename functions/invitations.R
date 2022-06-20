
simple.invite <- function(invit, dyads, sort) {
for (agent in invit$agent){ # each agent sends invites
  choice <- dyads %>% # make a df from dyads
    filter(agent_1 == agent) %>% # only choose current agent
    filter(agent_2 %in% invit$agent) %>%  # remove agents that already have a date (are no longer in invit)
    arrange(desc(.data[[sort]])) %>%  # sort so agent with higest sort is on top
    filter(.data[[sort]] == .data[[sort]][1]) # choose all agents that have the same sort as the top agent
  
  n <- sample(1:nrow(choice),1) # 1 random number between 1 and the number of rows in choice
  invit_agent <- choice$agent_2[n] # select random agent from choice
  agent_index <- which(invit$agent == agent) # get the row number for the current agent in invit
  invit$to[agent_index] <- invit_agent # in invit set agent choice 
}
  return(invit)
}


prob.invite <- function(invit, dyads, sort) {
  for (agent in invit$agent){ # each agent sends invites
    choice <- dyads %>% # make a df from dyads
      filter(agent_1 == agent) %>% # only choose current agent
      filter(agent_2 %in% invit$agent) %>%  # remove agents that already have a date (are no longer in invit)
      mutate(abs_sort = .data[[sort]] + abs(min(.data[[sort]]))) %>%  # add the abs value of the min sat to all sat
      mutate(prob = abs_sort/sum(abs_sort)) # add a row with probability of choosing each agent
  
    if (is.nan(choice$prob[1]) == FALSE) { # when only one agent with neg sort is in choice df, prob = NaN. This fixes that
      n <- sample(x = 1:nrow(choice), size = 1, prob = choice$prob) # 1 random number between 1 and the number of rows in choice
    } else {n <- 1}
    
    invit_agent <- choice$agent_2[n] # select random agent from choice
    agent_index <- which(invit$agent == agent) # get the row number for the current agent in invit
    invit$to[agent_index] <- invit_agent # in invit set agent choice 
  }
  return(invit)
}