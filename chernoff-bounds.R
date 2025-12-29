# Compare the three main bounds: Markov, Chebyshev, and (sub-Gaussian) Chernoff
t = seq(0, 8, by = 0.01)
markov = 1 / (2*t)
chebyshev = 1 / (2*t^2)
chernoff = exp(-t^2/2)

png(filename = "chernoff-bounds.png", res=160, width = 800, height = 800)
plot(t, markov, type = "l", col = "red", ylim = c(0, 1.1), xlim = c(0, 8), ylab = "P(X - μ > t)", xlab = "t", yaxs="i", xaxs ="i", main = "Markov, Chebyshev, and Chernoff Bounds")
lines(t, chebyshev, col = "blue")
lines(t, chernoff, col = "green")
abline(h = 0, col = "black", lty = 2)
legend("topright", legend = c("Markov", "Chebyshev", "Chernoff"), col = c("red", "blue", "green"), lty = 1)
dev.off()

# Next, we compare Chernoff bounds for sub-Gaussian vs sub-exponential random variables
subexp <- function(t, a){
    if(t < 1/a){
        exp(-t^2/2)
    } else {
        exp(-t / (2*a))
    }
}

subexponential1 = sapply(t, \(t) subexp(t, 1))
subexponential2 = sapply(t, \(t) subexp(t, 0.5))

png(filename = "subexponential-bounds.png", res=160, width = 800, height = 800)
plot(t, chernoff, type = "l", col = "green", ylim = c(0, 1.1), xlim = c(0, 8), ylab = "P(X - μ > t)", xlab = "t", yaxs="i", xaxs ="i", main = "Sub-Gaussian vs Sub-exponential Bounds")
lines(t, subexponential1, col = "magenta")
lines(t, subexponential2, col = "purple")
abline(h = 0, col = "black", lty = 2)
legend("topright", legend = c("sub-Gaussian", "sub-exponential, alpha = 1", "sub-exponential, alpha = 2"), col = c("green", "magenta", "purple"), lty = 1)
dev.off()
