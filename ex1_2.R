n_steps <- 100
xs <- integer(n_steps)
xs[1] <- 0.9
r = 3.9

for(i in 2:n_steps) {
  xs[i] = r*xs[i-1]*(1-xs[i-1])
}

# Plot the pointcaré
svg("pointcare5.svg")
plot(x=xs[1:(length(xs)-1)], y=xs[2:length(xs)], main="Pointcaré (r=3.9)",
     xlab=expression(x[t-1]), ylab=expression(x[t]), col = "dodgerblue")
lines(xs[1:(length(xs)-1)], y=xs[2:length(xs)], col = "dodgerblue", lwd = 1.5)
abline(b = 1, a = 0, col = "#ff8c00", lwd = 2)

# plot development plot..
svg("development5.svg")
plot(x=xs, ylab = "x", main="(r=3.9)", xlab="Steps", col = "dodgerblue")
#abline(h=1/3, col = "#ff8c00", lwd = 2)