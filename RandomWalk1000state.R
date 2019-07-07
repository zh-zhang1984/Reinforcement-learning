#######################################################################
# Copyright (C)                                                       #
# 2016-2018 Shangtong Zhang(zhangshangtong.cpp@gmail.com)             #
# 2016 Kenta Shimada(hyperkentakun@gmail.com)                         #
# Permission given to modify the code as long as you keep this        #
# declaration at the top                                              #
#######################################################################
#the R code is modified from the above python code
# # of states except for terminal states
N_STATES = 1000

# all states
STATES = 1:(N_STATES+2);

# start from a central state
START_STATE = 500

# terminal states
END_STATES = c(1,N_STATES+2)

# possible actions
ACTION_LEFT = -1
ACTION_RIGHT = 1
ACTIONS = c(ACTION_LEFT, ACTION_RIGHT)

# maximum stride for an action
STEP_RANGE = 100
compute_true_value <- function(){
  # true state value, just a promising guess
  true_value = seq(-1001,1001,2)/1001
  # Dynamic programming to find the true state values, based on the promising guess above
  # Assume all rewards are 0, given that we have already given value -1 and 1 to terminal states
  while (TRUE) {
    old_value = true_value
    for (state in STATES) {
      true_value[state]=0
      for (action in ACTIONS) {
        for (step in 1:STEP_RANGE) {
          step = step*action
          next_state = state + step
          next_state = max(min(next_state, N_STATES + 2), 1)
          # asynchronous update for faster convergence
          true_value[state] = true_value[state] + 1.0 / (2 * STEP_RANGE) * true_value[next_state]
        }
      }
    }
    error = sum(abs(old_value - true_value))
    if (error < 1e-2){
      break
    }
  }
  # correct the state value for terminal states to 0
  true_value[1] = true_value[N_STATES+2] = 0
  return (true_value)
}
true_value <- compute_true_value()
plot(1:length(true_value), true_value,cex=0.5)


# take an @action at @state, return new state and reward for this transition
stepAction <- function(state, action){
  step = sample(1: STEP_RANGE,size = 1)
  step = step*action
  state = state + step
  state = max(min(state, N_STATES + 2), 1)
  if (state == 1){
    reward = -1
  }else if(state == N_STATES + 2){#else must be at the end of prior if bracket
    reward = 1
    } else{
    reward = 0
    }
  return(list(state=state, reward=reward))
}
# get an action, following random policy
get_action <- function(){
  if (rbinom(n=1,size = 1,prob = 0.5) == 1){
    return(1)
  }
  return(-1)
}
# a wrapper class for aggregation value function
num_of_groups = 20


# get the value of @state
value <- function(state,params,num_of_groups){
  group_size = N_STATES %/% num_of_groups
  if(state%in%END_STATES){
    return(0)
  }else{
    group_index = (state-2) %/% group_size +1
    return(params[group_index])
  }
}
# update parameters
# @delta: step size * (target - old estimation)
# @state: state of current sample
update <- function(delta, state, params,num_of_groups){
  group_size = N_STATES %/% num_of_groups
  group_index = (state-2) %/% group_size +1
  params[group_index] = params[group_index] + delta
  params#to update the weight and return it
}
# gradient Monte Carlo algorithm
# @value_function: an instance of class ValueFunction
# @alpha: step size
# @distribution: array to store the distribution statistics
gradient_monte_carlo <-function(num_of_groups, params, alpha, 
                                distribution=NULL){
  #initialize
  num_of_groups = num_of_groups
  # weights
  params = params
  
  state = START_STATE
  trajectory = state
  # We assume gamma = 1, so return is just the same as the latest reward
  while(!state%in%END_STATES){
    action = get_action()
    stepNext <- stepAction(state, action)#repeat only once
    next_state = stepNext$state; 
    reward = stepNext$reward;
    trajectory <- c(trajectory,next_state)
    state = next_state
  }
  # Gradient update for each state in this trajectory
  for (state in trajectory) {
    delta = alpha * (reward - value(state,params = params,
                                    num_of_groups))
    params <- update(delta, state, params = params,num_of_groups)
    if(!is.null(distribution)){
      distribution[state] = distribution[state] + 1
    }
  }
  return(list(params=params,trajectory=trajectory,
              distribution = distribution))
}
#draw figure 9.1
episodes = 100000#need enough iteration 
alpha = 2e-5#need small enough value

#progress bar
pb = txtProgressBar(min = 1, max = episodes, initial = 0) 
stepi = 0
# we have 20 aggregations in this example, each has 50 states
num_of_groups = 20
distribution = rep(0,N_STATES+2)
param = rep(0,num_of_groups)
for (ep in 1:episodes) {
  RoundMC <- gradient_monte_carlo(num_of_groups = 20, params=param,
                       alpha = 2e-5, 
                       distribution=distribution)
  param <- RoundMC$params
  distribution <- RoundMC$distribution
  stepi = stepi + 1
  setTxtProgressBar(pb,stepi)
}
distribution = distribution/sum(distribution)
state_values = NULL;
for (i in STATES) {
  state_values = c(state_values,value(i,params=param,num_of_groups))
}
par(mar = c(5, 4, 4, 4) + 0.3)  # Leave space for distribution axis
plot(STATES,state_values,cex=0.5)
lines(1:length(true_value), true_value,cex=0.5)
par(new = TRUE)
barplot(distribution,axes = FALSE, 
        bty = "n", xlab = "", ylab = "")
axis(side=4, at = pretty(range(distribution)))
mtext("distribution", side=4, line=3)
# semi-gradient TD on 1000-state random walk
semi_gradient_temporal_difference <- function(num_of_groups, params, alpha, 
                                              n){
  # initial starting state
  state = START_STATE
  
  # vectors to store states and rewards for an episode
  # space isn't a major consideration, so I didn't use the mod trick
  states = state
  rewards = 0
  
  # track the time
  time = 1
  
  # the length of this episode
  T = Inf
  while (TRUE) {
    time = time + 1
    if(time < T){
      # choose an action randomly
      action = get_action()
      NextStep <- stepAction(state, action)
      next_state = NextStep$state; 
      reward = NextStep$reward
      # store new state and new reward
      states = c(states,next_state)
      rewards = c(rewards,reward)
      if (next_state %in% END_STATES){
        T = time
      }
    }
    # get the time of the state to update
    update_time = time - n
    if(update_time > 0){
      returns = 0.0
      # calculate corresponding rewards
      for (t in (update_time) : (min(T, update_time + n))){
        returns = returns +rewards[t]
      }
        
      # add state value to the return
      if(update_time + n <= T){
          returns = returns + value(states[update_time + n],params,
                                    num_of_groups)
      }
      state_to_update = states[update_time]
      # update the value function
      if (!(state_to_update %in% END_STATES)){
        delta = alpha * (returns - value(state_to_update,params,
                                         num_of_groups))
        params <- update(delta, state_to_update, params = params,
                         num_of_groups)
      }
            
    }
    if( update_time == T){
      break
    }
    state = next_state
  }
  return(list(params=params,states=states))
}
# semi-gradient TD on 1000-state random walk
episodes = 1e5#need enough iteration 
alpha = 2e-4#need small enough value

#progress bar
pb = txtProgressBar(min = 1, max = episodes, initial = 0) 
stepi = 0
# we have 20 aggregations in this example, each has 50 states
num_of_groups = 20
param = rep(0,num_of_groups)
for (ep in 1:episodes) {
  RoundTD <- semi_gradient_temporal_difference(num_of_groups = 20, 
                                               params=param,
                                  alpha = alpha, n=512)
  param <- RoundTD$params
  stepi = stepi + 1
  setTxtProgressBar(pb,stepi)
}
state_valuesDT = NULL;
for (i in STATES) {
  state_valuesDT = c(state_valuesDT,
                     value(i,params=param,num_of_groups))
}
#figure 9.2 left
plot(STATES,state_valuesDT,cex=0.5)
lines(1:length(true_value), true_value,cex=0.5)
# different alphas and steps for semi-gradient TD(fig9.2 right)
# all possible steps
steps = 2^(1:9)
# all possible alphas
alphas = c(seq(0,0.18,0.02),#need more granularity
           seq(0.2,1,0.1))
# each run has 10 episodes
episodes = 10

# perform 100 independent runs
runs = 100
# track the errors for each (step, alpha) combination
errors = matrix(rep(0,length(steps)*length(alphas)),
                nrow = length(steps))
for (run in 1:runs) {
  for (step in steps) {
    for (alpha in alphas) {
      param <- rep(0,20)
      for (ep in 1:episodes) {
        RoundTD <- semi_gradient_temporal_difference(num_of_groups = 20, 
                                                     params=param,
                                                     alpha = alpha, n=step)
        param <- RoundTD$params
      }
      state_valuesDT = NULL;
      for (i in STATES) {
        state_valuesDT = c(state_valuesDT,
                           value(i,params=param,num_of_groups))
      }
      errors[which(steps==step), which(alphas==alpha)] = 
        errors[which(steps==step), which(alphas==alpha)] + 
        sqrt(sum(`^`(state_valuesDT - true_value, 2)) / N_STATES)
    }
    cat('.')
  }
  cat('|')
}
# take average
errors = errors/runs
#fig9.2right
dtError <- data.frame(error = as.vector(errors),
                      alpha = rep(alphas,each=length(steps)),
                      steps = as.factor(rep(steps,length(alphas))))
library(ggplot2)
ggplot(data = dtError,aes(x=alpha,y = error,group=steps,
                          fill=steps,colour=steps))+
  geom_point()+
  geom_smooth(method = "loess")+
  coord_cartesian(ylim = c(0.2, 0.6)) 
