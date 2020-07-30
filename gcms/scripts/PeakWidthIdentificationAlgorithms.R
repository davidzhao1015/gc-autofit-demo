# peak width finding algorithms

# http://stackoverflow.com/questions/29642867/drawing-a-tangent-to-the-plot-and-finding-the-x-intercept-using-r


x <- seq(0,40)
y <- dnorm(seq(0,40), mean=25, sd=5)
y <- y * x*200000
y <- y * x

plot(x, y)
spl <- smooth.spline(y ~ x)
lines(spl, col=2)

newx <- 1010
pred0 <- predict(spl, x=newx, deriv=0)
pred1 <- predict(spl, x=newx, deriv=1)

yint <- pred0$y - (pred1$y*newx)
xint <- -yint/pred1$y
xint; pred1$y

plot(x, y)
abline(h=0, col=8)
lines(spl, col=2) # spline
points(pred0, col=2, pch=19) # point to predict tangent 
lines(x, yint + pred1$y*x, col=3) # tangent (1st deriv. of spline at newx)
points(xint, 0, col=3, pch=19) # x intercept

for(i in 2:length(x)-1) {
    # i<-15
    slope1 <- (y[i] - y[i-1])/(x[i]-x[i-1]); 
    slope2 <- (y[i+1] - y[i])/(x[i+1]-x[i]); 
    slope.avg <- (slope1 + slope2)/2
    cat(i,") x:", x[i]," slope1:", slope1, " slope2:", slope2, " avg:", slope.avg/y[20], "\n" )
}
