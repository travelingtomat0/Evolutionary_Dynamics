# Plot K=1000 trajectories for t in {1, ..., 100}

N <- c(10, 100)

I = N/2

empirical_sd <- function (x, m) {
  res <- 0
  res <- sum(x-m)
  return(sqrt(res^2/(len(x)-1)))
}

a_function <- function (n, i) {
  res <- c()
  p = i/n
  for(t in range(1, 100)) {
    tmp <- (i+1)*p*(1-p) + (i-1)*(1-p)*p + i*(p^2+(1-p)^2)
    res <- c(res, tmp)
  }
  # return simulated trajectory
  return(data.frame(res))
}

b_function <- function (n, i) {
  res <- c()
  for(t in range(1, 100)) {
    tmp <- (2*i*(1-(1-2/n^2)^t))/(n*(1-i/n)*2/n^2)
    res <- c(res, tmp)
  }
  return(data.frame(res))
}

c_function <- function(n, i) {
  res <- c()
  for(t in range(1, 100)) {

  }
  # show the approximation of f = Var[X(t)|X(0)=i] and plot f
  return(data.frame(res))
}


# Calculations for (1a):
for(i in c(1, 2)) {
  for(j in range(1, 1000)) {
    if(j == 1){
      res <- a_function(N[i],I[i])
    } else {
      res <- rbind(res, a_function(N[i],I[i]))
    }
  }

  # calculate empirical sd and empirical mean:
  for(j in range(1, 1000)) {
    em <- mean(res[j])
    esd <- empirical_sd(res[j], mean)
  }

  # plot
}

# Calculations for (1b): Show the trajectory of the variance formula
for(i in c(1, 2)) {
  for(j in range(1, 1000)) {
    if(j == 1){
      res <- b_function(N[i],I[i])
    } else {
      res <- rbind(res, b_function(N[i],I[i]))
    }
  }

  # calculate empirical sd and empirical mean:
  for(j in range(1, 1000)) {
    em <- mean(res[j])
    esd <- empirical_sd(res[j], mean)
  }

  # plot
}


# Calculations for (1c): Show the approximation 0 holds
for(i in c(1, 2)) {
  for(j in range(1, 1000)) {
    if(j == 1){
      res <- c_function(N[i],I[i])
    } else {
      res <- rbind(res, c_function(N[i],I[i]))
    }
  }

  # calculate empirical sd and empirical mean:
  for(j in range(1, 1000)) {
    em <- mean(res[j])
    esd <- empirical_sd(res[j], mean)
  }

  # plot
}
