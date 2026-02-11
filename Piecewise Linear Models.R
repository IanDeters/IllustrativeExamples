# install.packages('data.table')
library(data.table)

# install.packages('plot3D')
library(plot3D)

# Create x values in [-2, 2] and the values of x^2

DataSet = data.table(x = seq(-2, 2, by = .01))
DataSet$y = DataSet$x^2

# Create a linear model

LinearModel = glm(formula = as.formula('y ~ x'), data = DataSet, family = gaussian(link = 'identity'))

# Apply the linear model

DataSet$LinearModel = LinearModel$fitted.values

# Create a scatter plot

plot(DataSet$x, DataSet$y, main = 'X Squared VS Piecewise Model', xlab = 'x variable', ylab = 'y variable', pch = 19, col = 'blue', lwd = 1)
lines(DataSet$x, DataSet$LinearModel, col = 'red', lty = 1, lwd = 8)
legend('top', legend = c('X Squared', 'Linear'), col = c('blue', 'red'), lty = c(1, 1), lwd = 8)

# Create transformations

DataSet$xN1 = pmax(DataSet$x - (-1), 0)
DataSet$xP1 = pmax(DataSet$x - 1, 0)

# Create a simple piecewise linear model

PiecewiseLinearModel = glm(formula = as.formula('y ~ x + xN1 + xP1'), data = DataSet, family = gaussian(link = 'identity'))

# Apply the simple piecewise linear model

DataSet$PiecewiseLinearModel = PiecewiseLinearModel$fitted.values

# Update the scatterplot

lines(DataSet$x, DataSet$PiecewiseLinearModel, col = 'green', lty = 1, lwd = 8)
legend('top', legend = c('X Squared', 'Linear', 'Piecewise Linear'), col = c('blue', 'red', 'green'), lty = c(1, 1, 1), lwd = 8)

# Create transformations

DataSet$xN2 = pmax(DataSet$x - (-4 / 3), 0)
DataSet$xN3 = pmax(DataSet$x - (-2 / 3), 0)
DataSet$xP2 = pmax(DataSet$x - 2 / 3, 0)
DataSet$xP3 = pmax(DataSet$x - 4 / 3, 0)

# Create a more complex piecewise linear model

PiecewiseLinearModel02 = glm(formula = as.formula('y ~ x + xN2 + xN3 + xP2 + xP3'), data = DataSet, family = gaussian(link = 'identity'))

# Apply the more complex piecewise linear model

DataSet$PiecewiseLinearModel02 = PiecewiseLinearModel02$fitted.values

# Update the scatterplot

lines(DataSet$x, DataSet$PiecewiseLinearModel02, col = 'yellow', lty = 1, lwd = 8)
legend('top', legend = c('X Squared', 'Linear', 'Piecewise Linear', 'Piecewise Linear 02'), col = c('blue', 'red', 'green', 'yellow'), lty = c(1, 1, 1, 1), lwd = 8)

# Create a surface

DataSet = as.data.table(expand.grid(x = seq(0, 2 * pi, by = 2 * pi / 100), y = seq(0, 2 * pi, by = 2 * pi / 100)))
DataSet$z = sin(DataSet$x) + cos(DataSet$y)

# Create a surface plot

z = outer(seq(0, 2 * pi, by = 2 * pi / 100), seq(0, 2 * pi, by = 2 * pi / 100), function(x, y){sin(x) + cos(y)})
persp3D(seq(0, 2 * pi, by = 2 * pi / 100), seq(0, 2 * pi, by = 2 * pi / 100), z, theta = 30, phi = 30, col = 'blue', border = 'black', xlab = 'X', ylab = 'Y', zlab = 'Z', main = 'Trigonometric Surface')

# Create a linear model

LinearModel = glm(formula = as.formula('z ~ x + y'), data = DataSet, family = gaussian(link = 'identity'))

# Update the surface plot

zLinearModel = outer(seq(0, 2 * pi, by = 2 * pi / 100), seq(0, 2 * pi, by = 2 * pi / 100), function(x, y){cbind(rep(1, length(x)), x, y) %*% LinearModel$coefficients})
persp3D(seq(0, 2 * pi, by = 2 * pi / 100), seq(0, 2 * pi, by = 2 * pi / 100), zLinearModel, theta = 30, phi = 30, col = 'red', border = 'black', xlab = 'X', ylab = 'Y', zlab = 'Z', add = TRUE)

# View from another angle

persp3D(seq(0, 2 * pi, by = 2 * pi / 100), seq(0, 2 * pi, by = 2 * pi / 100), z, theta = 90, phi = 15, col = 'blue', border = 'black', xlab = 'X', ylab = 'Y', zlab = 'Z', main = 'Trigonometric Surface')
persp3D(seq(0, 2 * pi, by = 2 * pi / 100), seq(0, 2 * pi, by = 2 * pi / 100), zLinearModel, theta = 90, phi = 15, col = 'red', border = 'black', xlab = 'X', ylab = 'Y', zlab = 'Z', add = TRUE)

# Create transformations

DataSet$x1 = pmax(DataSet$x - pi, 0)
DataSet$y1 = pmax(DataSet$y - pi, 0)
DataSet$x1y1 = DataSet$x1 * DataSet$y1

# Create a simple piecewise linear model

PiecewiseLinearModel = glm(formula = as.formula('z ~ x + y + x1 + y1 + x1y1'), data = DataSet, family = gaussian(link = 'identity'))

# Update the surface plot

zPiecewiseLinearModel = outer(seq(0, 2 * pi, by = 2 * pi / 100), seq(0, 2 * pi, by = 2 * pi / 100), function(x, y){cbind(rep(1, length(x)), x, y, pmax(x - pi, 0), pmax(y - pi, 0), pmax(x - pi, 0) * pmax(y - pi, 0)) %*% PiecewiseLinearModel$coefficients})
persp3D(seq(0, 2 * pi, by = 2 * pi / 100), seq(0, 2 * pi, by = 2 * pi / 100), z, theta = 90, phi = 30, col = 'blue', border = 'black', xlab = 'X', ylab = 'Y', zlab = 'Z', main = 'Trigonometric Surface')
persp3D(seq(0, 2 * pi, by = 2 * pi / 100), seq(0, 2 * pi, by = 2 * pi / 100), zPiecewiseLinearModel, theta = 90, phi = 30, col = 'green', border = 'black', xlab = 'X', ylab = 'Y', zlab = 'Z', add = TRUE)

# View from another angle

persp3D(seq(0, 2 * pi, by = 2 * pi / 100), seq(0, 2 * pi, by = 2 * pi / 100), z, theta = 90, phi = 15, col = 'blue', border = 'black', xlab = 'X', ylab = 'Y', zlab = 'Z', main = 'Trigonometric Surface')
persp3D(seq(0, 2 * pi, by = 2 * pi / 100), seq(0, 2 * pi, by = 2 * pi / 100), zPiecewiseLinearModel, theta = 90, phi = 15, col = 'green', border = 'black', xlab = 'X', ylab = 'Y', zlab = 'Z', add = TRUE)

# Create transformations

DataSet$x2 = pmax(DataSet$x - 2 * pi / 3, 0)
DataSet$x3 = pmax(DataSet$x - 4 * pi / 3, 0)
DataSet$y2 = pmax(DataSet$y - 2 * pi / 3, 0)
DataSet$y3 = pmax(DataSet$y - 4 * pi / 3, 0)
DataSet$x2y2 = DataSet$x2 * DataSet$y2
DataSet$x2y3 = DataSet$x2 * DataSet$y3
DataSet$x3y2 = DataSet$x3 * DataSet$y2
DataSet$x3y3 = DataSet$x3 * DataSet$y3

# Apply the more complex piecewise linear model

PiecewiseLinearModel02 = glm(formula = as.formula('z ~ x + y + x2 + x3 + y2 + y3 + x2y2 + x2y3 + x3y2 + x3y3'), data = DataSet, family = gaussian(link = 'identity'))

# Update the surface plot

zPiecewiseLinearModel02 = outer(seq(0, 2 * pi, by = 2 * pi / 100), seq(0, 2 * pi, by = 2 * pi / 100), function(x, y){cbind(rep(1, length(x)), x, y, pmax(x - 2 * pi / 3, 0), pmax(x - 4 * pi / 3, 0), pmax(y - 2 * pi / 3, 0), pmax(y - 4 * pi / 3, 0), pmax(x - 2 * pi / 3, 0) * pmax(y - 2 * pi / 3, 0), pmax(x - 2 * pi / 3, 0) * pmax(y - 4 * pi / 3, 0), pmax(x - 4 * pi / 3, 0) * pmax(y - 2 * pi / 3, 0), pmax(x - 4 * pi / 3, 0) * pmax(y - 4 * pi / 3, 0)) %*% PiecewiseLinearModel02$coefficients})
persp3D(seq(0, 2 * pi, by = 2 * pi / 100), seq(0, 2 * pi, by = 2 * pi / 100), z, theta = 30, phi = 30, col = 'blue', border = 'black', xlab = 'X', ylab = 'Y', zlab = 'Z', main = 'Trigonometric Surface')
persp3D(seq(0, 2 * pi, by = 2 * pi / 100), seq(0, 2 * pi, by = 2 * pi / 100), zPiecewiseLinearModel02, theta = 30, phi = 30, col = 'yellow', border = 'black', xlab = 'X', ylab = 'Y', zlab = 'Z', main = 'Check It Out', add = TRUE)

# View from another angle

persp3D(seq(0, 2 * pi, by = 2 * pi / 100), seq(0, 2 * pi, by = 2 * pi / 100), z, theta = 90, phi = 30, col = 'blue', border = 'black', xlab = 'X', ylab = 'Y', zlab = 'Z', main = 'Trigonometric Surface')
persp3D(seq(0, 2 * pi, by = 2 * pi / 100), seq(0, 2 * pi, by = 2 * pi / 100), zPiecewiseLinearModel02, theta = 90, phi = 30, col = 'yellow', border = 'black', xlab = 'X', ylab = 'Y', zlab = 'Z', main = 'Check It Out', add = TRUE)