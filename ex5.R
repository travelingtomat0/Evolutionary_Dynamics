# Simulation of the Wright-Fisher Process

N <- c(10, 100)

# X is number of cells at time t
l <- 100
x <- integer(l)

transition_matrix <- function (N) {
  df <- data.frame(integer(N))

  for(i in 1:N) {
    tmp <- integer(N)
    for(j in 1:N) {
      tmp[j] <- choose(N, j)*(i/N)^j*((N-i)/N)^(N-j)
    }
    df <- cbind(df, tmp)
  }
  df <- df[-c(1)]
  colnames(df) <- 1:N
  return(df)
}

transition <- function(df, i, j) {
  return(df[i, j])
}

# calculation loop for first N
l <- N[1]
x <- integer(l)
x[1] <- N[1]/2

for(sim in 1:100) {
  for(step in 2:20) {
    tm <- transition_matrix(N[1])
    # X is number of cells at time t
    #print(tm)
    #print()
    #print(x[step-1])
    if(x[step-1] == 1 || x[step-1] == N[1]) {
      x[step] <- x[step-1]
    } else {
      x[step] <- sample(x=1:N[1], size=1, prob=tm[, x[step-1]])
    }
    # print(x)
  }
}

#print(x)

l <- N[2]
x <- integer(l)
x[1] <- N[2]/2

for(sim in 1:100) {
  print(sim)
  for(step in 2:100) {
    tm <- transition_matrix(N[2])
    # X is number of cells at time t
    #print(tm)
    #print()
    #print(x[step-1])
    if(x[step-1] == 1 || x[step-1] == N[2]) {
      x[step] <- x[step-1]
    } else {
      x[step] <- sample(x=1:N[2], size=1, prob=tm[, x[step-1]])
    }
    # print(x)
  }
}
print(x)
