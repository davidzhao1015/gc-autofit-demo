x <- seq(6000,6040)
yyy <- dnorm(seq(0,40), mean=25, sd=5)
plot(x, y)
spl <- smooth.spline(yyy ~ x)
lines(spl, col=2)

newx <- 6017
pred1 <- predict(spl, x=newx, deriv=1);pred1

x <- seq(0,40)
y <- dnorm(seq(0,40), mean=25, sd=5)
plot(x, y)
sm <- smooth.spline(y ~ x)
sm <- data.frame(rt=sm$x, intensity=sm$y)

lines(spl, col=2)

newx <- 20
pred1 <- predict(spl, x=newx, deriv=1);pred1
newx <- 10
pred1 <- predict(spl, x=newx, deriv=1);pred1
