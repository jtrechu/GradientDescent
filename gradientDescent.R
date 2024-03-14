
library(ggplot2)

##Gradient based method

gradient.descent <- function(f, gradf, x0, iterations=50000, eta=0.0002, momentum = 0, ascent = FALSE, xlim = 3, ylim = 3, detail =0.1) {
  max <- ifelse(ascent,"Maximum","Minimum")
  asc <- ifelse(ascent,"ascent","descent")
  x <- x0
  neg_gradf <- function(x){return(-1*gradf(x))}
  grad_f <- ifelse(ascent,neg_gradf,gradf)
  step <- 0
  for (i in 1:iterations){
    step <- momentum*step - eta*grad_f(x)
    x <- x + step
  }
  gradient <- (list(x = x, f = f(x)))
  if (any(is.nan(gradient$x))){print("The method did not converge")
    gradient$x <- c()
    gradient$f <- c()}
  if (length(gradient$x)==1) {
    x <- gradient$x[1]
    support <- seq(-xlim+x, 2+xlim, by = detail)
    plot(support, f(support), type = "l", main = paste(max ,"by gradient", asc),xlab = "x", ylab = "f(x)", lwd = 2, col = "red", ylim = c(-5,5))
    lines(support,gradf(support), type = "l", col = "blue", lwd = 2, lty = "dashed")
    points(c(gradient$x),c((gradient$f)),col = "black", pch = 16)
    legend("bottomleft", legend = c(expression(paste(f(x))), expression(paste("f '"(x))), paste("Gradient", asc, max,"with x0 =", as.character(x0))),
           col = c("red", "blue", "black"), lty = c("solid", "dashed", "blank"), lwd = 2, pch = c(NA, NA, 19), cex = .6)
  }
  
  if (length(gradient$x)==2) {
    c <- gradient$x
    x <- seq(-xlim+c[1],xlim+c[1],by = detail)
    y <- seq(-ylim+c[2],ylim+c[2],by = detail)
    grid <- expand.grid(x = x, y = y)
    grid$z <- x
    grid$z <- sapply(1:nrow(grid), function(i) f(c(grid$x[i], grid$y[i])))
    point_data <- data.frame(x = c[1], y = c[2])
    point_data$z <- f(c(point_data$x, point_data$y))
    plot <- ggplot(grid, aes(x = x, y = y, fill = z)) +
      geom_tile() +
      geom_point(data = point_data, aes(x = x, y = y), color = "red", size = 3) +
      geom_text(x = 1.1, y = 1.1, label = paste(max,"by G.D."), color = "black", size =4, vjust = -0.5) +
      scale_fill_gradientn(colors = terrain.colors(10), trans = "sqrt") +
      labs(title = paste("Gradient", asc,"on f"),
           x = "X",
           y = "Y",
           fill = "Function Value") +
      theme_minimal()
      print(plot)
  }
  if (length(gradient$x) > 2){print("Function is too high-dimensional to be plotted")}
  return(gradient)
}


##Examples

f1 <- function(x){x^2}
gradf1 <- function(x){2*x}
gradient.descent(f1,gradf1, x0 = 4)

f2 <- function(x){cos(x)*(5)^(x^2)}
gradf2 <- function(x){-(sin(x)/(.2^(x^2))) + 2*x*log(5)*(cos(x))*(5^(x^2))}
gradient.descent(f2,gradf2, x0 = 1)
gradient.descent(f2,gradf2, x0 = 1,ascent = TRUE)

f3 <- function(x){x[1]^2+x[2]^2}
gradf3 <- function(x){c(2*x[1],2*x[2])}
gradient.descent(f3,gradf3, x0 = c(2,3))

f4 <- function(x){x[1]^2*(x[2])^2}
gradf4 <- function(x){c(2*x[1]*(x[2])^2,2*x[1]^2*(x[2]))}
gradient.descent(f4,gradf4, x0 = c(2,2), iterations = 100000, detail = 0.5)

rosenbrock <- function(x) (x[1]-1)^2 + 100*(x[1]^2-x[2])^2
grad_rosenbrock <- function(x) {return(c(2*(x[1]-1)+400*x[1]*(x[1]^2-x[2]),-200*(x[1]^2-x[2])))}
gradient.descent(f = rosenbrock, gradf = grad_rosenbrock, x0 = c(3,4), eta = 2^-10, detail = 0.1, xlim = 10, ylim = 5)

##The Rosenbrock function, which is common for gradient
##based algorithms can be generalized further than shown 
##before as it is done below:


##Generalization of the Rosenbrock function

rosenbrock_function = function(x,a=1,b=100) {
  s <- 0
  for (i in 1:(length(x)-1)) {
    s <- s + b*(x[i+1]-x[i]^2)^2+(a-x[i])^2
  }
  return(s)
}




