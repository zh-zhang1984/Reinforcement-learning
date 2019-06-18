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
