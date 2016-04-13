dispatch = function(x, y) {
  contains = length(which(x$mean == y, arr.ind = T))
  index = which(x$mean == y, arr.ind = T)
  if (contains == 0) {
    x$mean = c(x$mean, y)
    x$count = c(x$count, 1)
  } else if (contains == 1) {
    x$count[index] = x$count[index] + 1
  }
  return(x)
}

sample.mean = function(data, size) {
  result = mean(sample(data, size))
  return(result)
}

summary.list = function(n, data, size) {
  r = list(mean = c(), count = c(), freq = c(), density = c()) 
  for (i in 1:n) {
    r = dispatch(r, sample.mean(data, size))
  }
  r$freq = rep(0, length(r$count))
  r$density = rep(0, length(r$count))
  for (j in 1:length(r$count)) {
    r$freq[j] = r$count[j] / n
    r$density[j] = r$freq[j] / 0.01
  }
  sorted = order(r$mean)
  mean = rep(0, length(r$mean))
  count = rep(0, length((r$count)))
  freq = rep(0, length(r$freq))
  density = rep(0, length(r$density))
  m = 1
  for (k in sorted) {
    mean[m] = r$mean[k]
    count[m] = r$count[k]
    freq[m] = r$freq[k]
    density[m] = r$density[k]
    m = m + 1
  }
  r$mean = mean
  r$count = count
  r$freq = freq
  r$density = density
  return(r)
}

l = summary.list(10000, height, 40)
barplot(l$count, names.arg = l$mean)
barplot(l$freq, names.arg = l$mean)
barplot(l$density, names.arg = l$mean)
random = rnorm(10000, mean = mean(height), sd = sd(height) / sqrt(40))
#x = seq(min(random), max(random), by = 10)
#x = sort(random)
y = dnorm(x, mean = mean(x), sd = sd(x))
x = x[] - min(l$mean)
lines(x, y, col = "blue")
