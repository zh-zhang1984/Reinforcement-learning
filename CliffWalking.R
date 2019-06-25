#cliff walking example for both Qlearning and SASAR
# world height
WORLD_HEIGHT = 4

# world width
WORLD_WIDTH = 12

# probability for exploration
EPSILON = 0.1

# step size
ALPHA = 0.5

# gamma for Q-Learning and Expected Sarsa
GAMMA = 1

# all possible actions
ACTION_UP = 1
ACTION_DOWN = 2
ACTION_LEFT = 3
ACTION_RIGHT = 4
ACTIONS = c(ACTION_UP, ACTION_DOWN, ACTION_LEFT, ACTION_RIGHT)
# initial state action pair values
START = c(4, 1)
GOAL = c(4, 12)
step <- function(state,action){
  i=state[1]
  j=state[2]
  if(action==ACTION_UP){
    next_state = c(max(i - 1, 1), j)
  } else if(action == ACTION_LEFT){
    next_state = c(i, max(j - 1, 1))
  } else if(action == ACTION_RIGHT){
    next_state = c(i, min(j + 1, WORLD_WIDTH))
  } else if(action == ACTION_DOWN){
    next_state = c(min(i + 1, WORLD_HEIGHT), j)
  } else {
    print('invalid action values')
  }
  reward=-1
  if((action == ACTION_DOWN & i == 3 & j>1 &j< 12) |
     (action == ACTION_RIGHT & state[1] == START[1]&state[2] == START[2])){
    reward=-100;
    next_state=START
  }
  return(list(next_state=next_state,
              reward=reward))
}
# choose an action based on epsilon greedy algorithm
choose_action <- function(state, q_value){
  if(rbinom(1,1,prob=EPSILON)==1){
    return(sample(x=ACTIONS,size=1))
  } else{
    values_ = q_value[state[1], state[2],]
    return(ifelse(sum(values_==max(values_))>1,
                  sample(x=ACTIONS[values_==max(values_)],size = 1),
                  ACTIONS[values_==max(values_)]))
  }
}
#test with an example
state=c(3,2)
q_value=sample(1:24,WORLD_HEIGHT*WORLD_WIDTH*length(ACTIONS),replace = T)
dim(q_value)=c(WORLD_HEIGHT,WORLD_WIDTH,length(ACTIONS))
action_choose <- NULL
for (i in 1:1000) {
  action_choose <- c(action_choose,
                     choose_action(state,q_value))
}
table(action_choose)/1000#appears correct with several sample test
# an episode with Sarsa
# @q_value: values for state action pair, will be updated
# @expected: if True, will use expected Sarsa algorithm
# @step_size: step size for updating
# @return: total rewards within this episode
sarsa <- function(q_value, expected=FALSE, step_size=ALPHA){
  state = START
  action = choose_action(state, q_value)
  rewards = 0.0
  while (state[1]!=GOAL[1]|state[2]!=GOAL[2]) {
    next_state = step(state, action)$next_state
    reward =  step(state, action)$reward
    next_action = choose_action(next_state, q_value)
    rewards = rewards + reward
    if(!expected){
      target = q_value[next_state[1], next_state[2], next_action]
    } else{
      # calculate the expected value of new state
      target = 0.0
      q_next = q_value[next_state[1], next_state[2], ]
      best_actions = ACTIONS[q_next == max(q_next)]
      for (action in ACTIONS) {
        if(action %in% best_actions){
          target = target + ((1.0 - EPSILON) / length(best_actions) + EPSILON / length(ACTIONS)) * q_value[next_state[1], next_state[2], action]
        }else{
          target = target + EPSILON / length(ACTIONS) * q_value[next_state[1], next_state[2], action]
        }
      }
    }
    target = target*GAMMA
    q_value[state[1], state[2], action] = q_value[state[1], state[2], action] +
      step_size * (reward + target - q_value[state[1], state[2], action])
    state = next_state
    action = next_action
  }
  return(list(rewards=rewards,q_value=q_value))
}
# an episode with Q-Learning
# @q_value: values for state action pair, will be updated
# @step_size: step size for updating
# @return: total rewards within this episode
q_learning <- function(q_value, step_size=ALPHA){
  state = START
  rewards = 0.0
  while (state[1]!=GOAL[1]|state[2]!=GOAL[2]) {
    action = choose_action(state, q_value)
    next_state = step(state, action)$next_state;
    reward = step(state, action)$reward;
    rewards = rewards + reward;
    # Q-Learning update
    q_value[state[1], state[2], action] = q_value[state[1], state[2], action] +
      step_size * (reward + GAMMA * 
                     max(q_value[next_state[1],next_state[2], ]) -
        q_value[state[1], state[2], action])
    state = next_state
  }
  return(list(rewards=rewards,q_value=q_value))
}
#initilize the q table value and run a Q-learn and SARSA
q_value <- rep(0,WORLD_HEIGHT*WORLD_WIDTH*length(ACTIONS))
dim(q_value) <- c(WORLD_HEIGHT,WORLD_WIDTH,length(ACTIONS))
QLearnValue <- q_learning(q_value)$q_value
QsarsaValue <- sarsa(q_value)$q_value
# print optimal policy
print_optimal_policy <- function(q_value){
  optimal_policy =NULL
  for (i in 1:WORLD_HEIGHT) {
    for (j in 1:WORLD_WIDTH) {
      if(i==GOAL[1]&j==GOAL[2]){
        optimal_policy<- c(optimal_policy,'G')
      } else{
        bestAction = ACTIONS[q_value[i, j, ]==max(q_value[i, j, ])]
        if(bestAction == ACTION_UP){
          optimal_policy<- c(optimal_policy,'U')
        } else if(bestAction == ACTION_DOWN){
          optimal_policy<- c(optimal_policy,'D')
        } else if(bestAction == ACTION_LEFT){
          optimal_policy<- c(optimal_policy,'L')
        } else if(bestAction == ACTION_RIGHT){
          optimal_policy<- c(optimal_policy,'R')
        }
      }
    }
  }
  optimal_policy <- matrix(optimal_policy,
                           ncol = WORLD_WIDTH,byrow = T)
  print(optimal_policy)
}
#solution to several equal optimal actions (seems not necessary)
print_optimal_policy <- function(q_value){
  optimal_policy =NULL
  for (i in 1:WORLD_HEIGHT) {
    for (j in 1:WORLD_WIDTH) {
      if(i==GOAL[1]&j==GOAL[2]){
        optimal_policy<- c(optimal_policy,'G')
      } else{
        bestAction = ACTIONS[q_value[i, j, ]==max(q_value[i, j, ])]
        optimal_policy <- c(optimal_policy,
                            paste(as.character(bestAction),
                                                 collapse = ''))
        optimal_policy <- gsub("1", "U", optimal_policy)
        optimal_policy <- gsub("2", "D", optimal_policy)
        optimal_policy <- gsub("3", "L", optimal_policy)
        optimal_policy <- gsub("4", "R", optimal_policy)
      }
    }
  }
  optimal_policy <- matrix(optimal_policy,
                           ncol = WORLD_WIDTH,byrow = T)
  print(optimal_policy)
}
print_optimal_policy(q_value = QLearnValue)
#to draw a figure for the changes in returns for each episode for SARSA and Qlearning
# episodes of each run
episodes = 1000

# perform 40 independent runs
runs = 10

rewards_sarsa = rep(0,episodes)
rewards_q_learning = rep(0,episodes)
pb = txtProgressBar(min = 0, max = runs, initial = 0) 
stepi = 0
#CAUTION: this for loop takes a long time(2 days)!!!!!!
for (r in 1:runs) {
  q_sarsa=rep(0,WORLD_HEIGHT*WORLD_WIDTH*length(ACTIONS))
  dim(q_sarsa)=c(WORLD_HEIGHT,WORLD_WIDTH,length(ACTIONS))
  q_q_learning = rep(0,WORLD_HEIGHT*WORLD_WIDTH*length(ACTIONS))
  dim(q_q_learning)=c(WORLD_HEIGHT,WORLD_WIDTH,length(ACTIONS))
  for (i in 1:episodes) {
    # cut off the value by -100 to draw the figure more elegantly
    #rewards_sarsa[i] = rewards_sarsa[i] + max(sarsa(q_sarsa), -100)
    #rewards_q_learning[i] = rewards_q_learning[i] + max(q_learning(q_q_learning), -100)
    rewards_sarsa[i] = rewards_sarsa[i] + sarsa(q_sarsa)$rewards
    rewards_q_learning[i] = rewards_q_learning[i] + q_learning(q_q_learning)$rewards
    q_sarsa <- sarsa(q_sarsa)$q_value#update the Q-table
    q_q_learning <- q_learning(q_q_learning)$q_value
  }
  stepi = stepi + 1
  setTxtProgressBar(pb,stepi)
}
# averaging over independent runs
rewards_sarsa = rewards_sarsa /runs
rewards_q_learning = rewards_q_learning /runs
dtRewards <- data.frame(rewards=c(rewards_sarsa,rewards_q_learning),
                        type=c(rep('sarsa',episodes),rep('Qlearn',episodes)),
                        episodes=c(1:episodes,1:episodes))
library(ggplot2)
ggplot2::ggplot(dtRewards,aes(x=episodes,y=rewards,color=type))+
  geom_line()+
  scale_y_continuous(limits = c(-500, 0))
  
print_optimal_policy(q_sarsa)
print_optimal_policy(q_q_learning)
