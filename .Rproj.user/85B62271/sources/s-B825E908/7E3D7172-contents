library(adehabitatHR)

data(puechcirc)
x <- puechcirc[1]
x

lik <- liker(x, sig2 = 58, rangesig1 = c(1, 10))

tata <- kernelbb(x, sig1 = 6.23, sig2 = 58, grid = 50)
tata

image(tata)
plot(getverticeshr(tata, 95), add=TRUE, lwd=2)

bb.95 <- getverticeshr(tata, percent = 95)
bb.95
