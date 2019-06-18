#https://stackoverflow.com/questions/39353580/how-to-implement-q-learning-in-r
q.learn <- function(R, N, alpha, gamma, tgt.state) {
  ## initialize Q to be zero matrix, same size as R
  Q <- matrix(rep(0,length(R)), nrow=nrow(R))
  ## loop over episodes
  for (i in 1:N) {
    ## for each episode, choose an initial state at random
    cs <- sample(1:nrow(R), 1)
    ## iterate until we get to the tgt.state
    while (1) {
      ## choose next state from possible actions at current state
      ## Note: if only one possible action, then choose it;
      ## otherwise, choose one at random
      next.states <- which(R[cs,] > -1)
      if (length(next.states)==1)
        ns <- next.states
      else
        ns <- sample(next.states,1)
      ## this is the update
      Q[cs,ns] <- Q[cs,ns] + alpha*(R[cs,ns] + gamma*max(Q[ns, which(R[ns,] > -1)]) - Q[cs,ns])
      ## break out of while loop if target state is reached
      ## otherwise, set next.state as current.state and repeat      
      if (ns == tgt.state) break
      cs <- ns
    }
  }
  ## return resulting Q normalized by max value
  return(100*Q/max(Q))
}

N <- 1000
alpha <- 1
gamma <- 0.8
tgt.state <- 6
R <- matrix(c(-1,-1,-1,-1,0,-1,
              -1,-1,-1,0,-1,0,
              -1,-1,-1,0,-1,-1,
              -1,0,0,-1,0,-1,0,
              -1,-1,0,-1,0,-1,
              100,-1,-1,100,100),nrow=6)
print(R)
Q <- q.learn(R,N,alpha,gamma,tgt.state)
print(Q)
######
library(reinforcelearn)
library(keras)
library(tidyverse)
model = keras_model_sequential()
model %>% layer_dense(32, input_shape = 10, activation = "relu")
model %>% layer_dense(4, activation = "softmax")
keras::compile(model, loss = "mae", optimizer = keras::optimizer_sgd(lr = 0.4))

val = makeValueFunction("neural.network", model = model)
policy = makePolicy("epsilon.greedy", epsilon = 0.2)
algorithm = makeAlgorithm("qlearning")
agent = makeAgent("epsilon.greedy", 
                  val.fun = val,
                  algorithm.args = list(class="qlearning"), 
                  policy.args = list(epsilon = 0.2))


####MDP environment
rand_vect <- function(N, M, sd = 1, pos.only = TRUE) {
  vec <- rnorm(N, M/N, sd)
  if (abs(sum(vec)) < 0.01) vec <- vec + 1
  vec <- round(vec / sum(vec) * M)
  deviation <- M - sum(vec)
  for (. in seq_len(abs(deviation))) {
    vec[i] <- vec[i <- sample(N, 1)] + sign(deviation)
  }
  if (pos.only) while (any(vec < 0)) {
    negs <- vec < 0
    pos  <- vec > 0
    vec[negs][i] <- vec[negs][i <- sample(sum(negs), 1)] + 1
    vec[pos][i]  <- vec[pos ][i <- sample(sum(pos ), 1)] - 1
  }
  vec
}


P = array(0, c(10, 10, 5))
for (i in 1:5) {
  rdvec<-NULL
  for (j in 1:9) {
    rd<-rand_vect(10,100,10)/100
    rdvec <- c(rdvec,rd)
  }
  P[, , i] = matrix(c(rdvec,c(rep(0,9),1)), 10, 10, byrow = TRUE)
}


# Reward matrix
R = matrix(sample(-50:50,100,replace = T), 10, 10, byrow = TRUE)
env = makeEnvironment("mdp", transitions = P, rewards = R)
policy = makePolicy("epsilon.greedy", epsilon = 0.2)
val.fun = makeValueFunction("table", n.states = 10L, n.actions = 5L)
algorithm = makeAlgorithm("qlearning")

agent = makeAgent(policy, val.fun, algorithm)
interact(env, agent, n.episodes = 1000,learn = T)
getValueFunction(agent)
#######Frozen lake problem
N=4
v=array(rep(0,N*N),dim = c(N,N))# Is our value vector.
THRESHOLD = 1e-5
A = rbind(c(0,1),c(0,-1),c(1,0),c(-1,0))
MAP = rbind(strsplit("SFFF",split = '')[[1]],
            strsplit("FHFH",split = '')[[1]],
            strsplit("FFFH",split = '')[[1]],
            strsplit("HFFG",split = '')[[1]])
proj <- function(n, minn, maxn){
  return(max(min(maxn, n), minn))
}
move <- function(s, tpl, stochasticity=0){
  if (MAP[s[1],s[2]] == 'H'){
    return( c(1,1))# Go back to the start 
  } else if(runif(1,min = 0,max = 1) < stochasticity){
      return(A[sample(1:4,1),])
  }else{
    return(c(proj(s[1] + tpl[1], 1, N), 
             proj(s[2] + tpl[2], 1, N)))
  }
}
reward <- function(s){
  return( MAP[s[1],s[2]] == 'G')
}
run_with_value <- function(v, gamma=0.9){
  old_v = v
  for( i in 1:N){
    for (j in 1:N) {
      best_val = 0
      for (a in 1:4) {
        new_s = move(c(i,j), A[a,])
        best_val = max(best_val, reward(new_s) + 
                         gamma * old_v[new_s[1],new_s[2]])
      }
      v[i,j] = best_val
    }
  }
  return (v)
}
while(norm(v - old_v) >=THRESHOLD){
  old_v <-v
  v = run_with_value(v)
}
# Extracting policy from v:
pi <- function(s,v){
  cur_best = -Inf
  cur_a = NULL
  for (a in 1:4) {
    new_s <- move(s,A[a,])
    val = v[new_s[1],new_s[2]]
    if(val>cur_best){
      cur_a = a
      cur_best = val
    }
  }
  return(cur_a)
}
