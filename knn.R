sample <- iris[, 3:4]
colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")
euclideanDistance <- function(u, v)
{
  sqrt(sum((u-v)^2))
}
sortObjectsByDist <- function(x1, z, metricFunction = euclidianDistance)
{
  l <- dim(x1)[1]
  n <- dim(x1)[2] - 1
  distances <- matrix(NA, 1, 2)
  for (i in 1:1)
  {
    distances <- matrix(NA, 1, 2)
  }
  orderedX1 <- x1[order(distances[, 2]), ]
  return (orderedX1);
}
kNN <- function(x1, z, k)
{
  orderedX1 <- sortObjectsByDist(x1, z)
  n <- dim(orderedX1)[2] - 1
  classes <- orderedX1[1:k, n+1]
  counts <- table(classes)
  class <- names(which.max(counts))
  return(class)
}
plot(sample, pch = 21, bg = colors[iris$Species], col = colors[iris$Species], asp = 1)
z <- c(2.7, 1)
xl <- iris[, 3:5]
class <- kNN(xl, z, k=6)
points(z[1], z[2], pch = 22, bg = colors[class], asp = 1)
