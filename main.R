n_steps <- 100
rs <- c(0.5,1.5,2.5)
xs <- integer(n_steps+1)
xs[1] <- 1.5

for(i in 2:n_steps) {
  xs[i] = rs[1]*xs[i-1]*(1-xs[i-1])
}


xm <- xs[2:length(xs)]

jpeg("development.jpg")
plot(x=xs, ylab = "x", xlab="Steps", col = "dodgerblue", main="Development (r=0.5)")

# Plot the pointcaré
jpeg("plot.jpg")
plot(x=xs[1:(length(xs)-1)], y=xm, xlab="xt-1", main="Pointcaré", ylab="xt", col = "dodgerblue")
lines(xs[1:(length(xs)-1)], xm, col = "dodgerblue", lwd = 1.5)