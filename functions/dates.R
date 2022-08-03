################################################################################################
# These functions takes the number of agents, the dyad df and a sort option (sat or commit), 
# and returns dyads for date phase
################################################################################################

dates <- function(N, dyads, sort, invite_function) {
  
  hangout <- tibble(agent = numeric(), # create empty hangout table
                    date = logical(), 
                    to = numeric(), 
                    with = numeric())
  
  invit <- tibble(agent = as.numeric(seq(1:N)), # create empty invit table with all agents 
                  date = rep(FALSE,N), 
                  to = as.numeric(rep(NA,N)), 
                  with = as.numeric(rep(NA,N)))

while (nrow(invit) != 0) { # while some agents are left in invite-phase
  
  ############################### Send invites ###################################
  if (invite_function == "simpleinvite"){
    invit <- simpleinvite(invit = invit, dyads = dyads, sort = sort) 
  } else if (invite_function == "probinvite"){
    invit <- probinvite(invit = invit, dyads = dyads, sort = sort) 
  }

  ####################### Check for mutual invites ###############################
  for (agent in invit$agent){ # each agent checks whether they have mutual invites
    agent_index <- which(invit$agent == agent) # get the row number for the current agent in invit
    invit_agent <- invit$to[agent_index] # the agent i invited
    invit_agent_index <- which(invit$agent == invit_agent) # get the row number for the invited agent in invit
    if (invit$to[invit_agent_index] == agent){ # if the invite from my invited agent is to me
      invit$date[agent_index] <- TRUE # set that i have a date
      invit$with[agent_index] <- invit_agent # set that it is with the person i invited
    }
  }
  ############################ Update invit df ###################################          
  hangout_dyads <- invit %>% filter(date == TRUE) # select set dyads
  hangout <- rbind(hangout, hangout_dyads) # append dyads to hangout df
  invit <- invit %>% filter(date == FALSE) # remove agents with dates from invit
  rows <- sample(nrow(invit)) # for making random invit
  invit <- invit[rows, ] # define new, shuffled invit
  
  ############################ Accept invites ####################################
  for (agent in invit$agent){
    agent_index <- which(invit$agent == agent) # get index of agent in invit
    
    if (invit$date[agent_index] == FALSE){ # Only look at agents without a date
      a <- agent # annoying workaroud, filter() is confused otherwise
      options <- invit %>% filter(to == a & date == FALSE) # make a df with agents that sent me invites, and are still available
      
      if (nrow(options) == 1){ # if i have one option
        agent_option <- options$agent[1] # the name of the one agent that sent me an invite
        agent_option_index <- which(invit$agent == agent_option) # index of agent_option
        invit$date[agent_index] <- TRUE # set that i have a date
        invit$with[agent_index] <- agent_option # set that it is with the person on my options list
        invit$date[agent_option_index] <- TRUE # set that other agent has a date
        invit$with[agent_option_index] <- agent # set that it is with me
      }
      
      if (nrow(options) > 1){ # if i have more than one option
        choice <- dyads %>% # make a df from dyads
          filter(agent_1 == agent) %>% # only choose current agent
          filter(agent_2 %in% options$agent) %>% # only choose agents that i have as options
          arrange(desc(.data[[sort]])) %>%  # sort so agent with highest sort is on top 
          filter(.data[[sort]] == .data[[sort]][1]) # choose all agents that have the same sort as the top agent
        
        n <- sample(1:nrow(choice),1) # 1 random number between 1 and the number of rows in choice
        agent_choice <- choice$agent_2[n] # the name of the top agent
        agent_choice_index <- which(invit$agent == agent_choice) # index of agent_choice
        invit$date[agent_index] <- TRUE # set that i have  a date
        invit$with[agent_index] <- agent_choice # set that it is with the top agent
        invit$date[agent_choice_index] <- TRUE # set that other agent has a date
        invit$with[agent_choice_index] <- agent # set that it is with me
      }
    }
  }
  
  ############################ Update invit df ###################################          
  hangout_dyads <- invit %>% filter(date == TRUE) # select set dyads
  hangout <- rbind(hangout, hangout_dyads) # append dyads to hangout df
  invit <- invit %>% filter(date == FALSE)# remove agents with dates from invite
 
} # end of while loop
  
  hangout <- hangout %>% # clean up df before returning
    select(-c(date,to)) %>% 
    arrange(agent) %>% 
    rename(A = agent, B = with)
  
  hangout <- hangout[!duplicated(data.frame(list(do.call(pmin, hangout),do.call(pmax, hangout)))),] # removing duplicate pairs
  
  return(hangout)
}
