# install.packages('data.table')
library(data.table)

# Set the seed

set.seed(667)

# Example 1

# Sample from the space

Sample = sample(x = seq(4), size = 1000000, replace = TRUE, prob = NULL)

# Create a sample of random variables from the sample from the space

A = Sample %in% c(2, 4)
B = Sample %in% c(3, 4)
X = A + B

# Confirm their independence by examining the sample means

mean(A)
mean(B)
mean(A * B)

# Observe the estimates of their correlation

cor(A, X)
cor(B, X)
cor(A, B)

# Lemma 1

# Create two variables with a correlation of approximately .3

X = runif(1000000)
U = runif(1000000)

U = U - cor(X, U) * sqrt(var(U)) / sqrt(var(X)) * X - mean(U) + cor(X, U) * sqrt(var(U)) / sqrt(var(X)) * mean(X)

Y = .3 / sqrt(var(X)) * (X - mean(X)) + sqrt(1 - .3^2) / sqrt(var(U)) * (U - mean(U))

# Examine the correlation

cor(X, Y)

# Create a variable from X which has approximately 0 correlation with X

W = (X + (mean(X^2) * mean(X) - mean(X^3)) / (2 * var(X)))^2

cor(X, W)

# Create an affine combination of X and Y with a approximate correlation of 0 with X

W = Y - cor(X, Y) * sqrt(var(Y)) / sqrt(var(X)) * X - mean(Y) + cor(X, Y) * sqrt(var(Y)) / sqrt(var(X)) * mean(X)

cor(X, W)

# Create an affine combination of X and W with a approximate correlation of .7 with X

Z = .7 / sqrt(var(X)) * (X - mean(X)) + sqrt(1 - .7^2) / sqrt(var(W)) * (W - mean(W))

cor(X, Z)

# Lemma 2

# Randomly generate two matrices.  With probability one C will be invertible

A = matrix(data = runif(n = 20), nrow = 5, ncol = 4)
C = matrix(data = runif(n = 16), nrow = 4, ncol = 4)

# Define B

B = A %*% C

# Observe that the formula holds

max(abs(A %*% solve(t(A) %*% A) %*% t(A) - B %*% solve(t(B) %*% B) %*% t(B)))

# Example illustrating the relationship among the coefficients of the three projections in Birnbaum's examples

NumberOfVariables = 3
SampleSize = 1000

# Generate the data

Data = matrix(data = runif(SampleSize * (NumberOfVariables + 1)), ncol = NumberOfVariables + 1)

# Project the additional variable onto the other variables

a = glm.fit(x = cbind(rep(1, nrow(Data)), Data[, seq(NumberOfVariables)]), y = Data[, NumberOfVariables + 1], family = gaussian(link = 'identity'))[['coefficients']]

# Generate a target

Target = runif(SampleSize)

# Project the target onto the two sets of variables

b1 = glm.fit(x = cbind(rep(1, nrow(Data)), Data[, seq(NumberOfVariables)]), y = Target, family = gaussian(link = 'identity'))[['coefficients']]
b2 = glm.fit(x = cbind(rep(1, nrow(Data)), Data), y = Target, family = gaussian(link = 'identity'))[['coefficients']]

# Observe the equality

max(abs(b2[NumberOfVariables + 2] * a + b2[seq(NumberOfVariables + 1)] - b1))

# Outcome 1

SampleSize = 1000

# Generate the data

Data = data.table(X1 = runif(n = SampleSize), X2 = runif(n = SampleSize), X3 = runif(n = SampleSize))

# Generate the target

Data$Y = rnorm(SampleSize, mean = Data$X1 + 2 * Data$X2 + 3 * Data$X3)

# Create a fourth variable perfectly correlated with X1

Data$X4 = runif(n = 1) * Data$X1

cor(Data$X1, Data$X4)

# Observe the parameter estimates

summary(glm(formula = as.formula('Y ~ X1 + X2 + X3'), family = gaussian(link = 'identity'), data = Data))[['coefficients']]
summary(glm(formula = as.formula('Y ~ X1 + X2 + X3 + X4'), family = gaussian(link = 'identity'), data = Data))[['coefficients']]

# Outcome 2

# Select your correlation

Correlation = .01

# Generate the data

X1 = runif(1000000)
R = runif(1000000)

X1 = X1 - cor(R, X1) * sqrt(var(X1)) / sqrt(var(R)) * R - mean(X1) + cor(R, X1) * sqrt(var(X1)) / sqrt(var(R)) * mean(R)
X1 = Correlation / sqrt(var(R)) * (R - mean(R)) + sqrt(1 - Correlation^2) / sqrt(var(X1)) * (X1 - mean(X1))

# Adjust the variables

a = glm.fit(x = matrix(data = c(rep(1, length(R)), X1), ncol = 2), y = R, family = gaussian(link = 'identity'))[['coefficients']]

R = R - a[1]
X1 = X1 * a[2]

# Observe the correlation

cor(X1, R)

# Project onto X1

summary(glm(formula = as.formula('R ~ X1'), family = gaussian(link = 'identity'), data = data.table(X1 = X1, R = R)))

# Generate a target

Target = rnorm(1000000, mean = 2 * R, sd = 1)

# Compare the parameter estimates

b1 = summary(glm(formula = as.formula('Target ~ X1'), family = gaussian(link = 'identity'), data = data.table(X1 = X1, Target = Target)))[['coefficients']]
b2 = summary(glm(formula = as.formula('Target ~ X1 + R'), family = gaussian(link = 'identity'), data = data.table(X1 = X1, R = R, Target = Target)))[['coefficients']]

b1
b2

# Outcome 3

# Generate the data

Correlation = .01

X1 = runif(1000000)
X2 = runif(1000000)
X3 = runif(1000000)
R = runif(1000000)

X1 = X1 - cor(R, X1) * sqrt(var(X1)) / sqrt(var(R)) * R - mean(X1) + cor(R, X1) * sqrt(var(X1)) / sqrt(var(R)) * mean(R)
X1 = Correlation / sqrt(var(R)) * (R - mean(R)) + sqrt(1 - Correlation^2) / sqrt(var(X1)) * (X1 - mean(X1))

X2 = X2 - cor(R, X2) * sqrt(var(X2)) / sqrt(var(R)) * R - mean(X2) + cor(R, X2) * sqrt(var(X2)) / sqrt(var(R)) * mean(R)
X2 = Correlation / sqrt(var(R)) * (R - mean(R)) + sqrt(1 - Correlation^2) / sqrt(var(X2)) * (X2 - mean(X2))

X3 = X3 - cor(R, X3) * sqrt(var(X3)) / sqrt(var(R)) * R - mean(X3) + cor(R, X3) * sqrt(var(X3)) / sqrt(var(R)) * mean(R)
X3 = Correlation / sqrt(var(R)) * (R - mean(R)) + sqrt(1 - Correlation^2) / sqrt(var(X3)) * (X3 - mean(X3))

# Adjust the variables

a = glm.fit(x = matrix(data = c(rep(1, length(X1)), X1, X2, X3), ncol = 4), y = R, family = gaussian(link = 'identity'))[['coefficients']]

# Adjust the variable

R = R - a[1]
X1 = X1 * a[2]
X2 = X2 * a[3]
X3 = X3 * a[4]

# Observe the correlation

cor(X1, R)
cor(X2, R)
cor(X3, R)

# Project onto X1, X2, and X3

summary(glm(formula = as.formula('R ~ X1 + X2 + X3'), family = gaussian(link = 'identity'), data = data.table(X1 = X1, X2 = X2, X3 = X3, R = R)))[['coefficients']]

# Generate a target

Target = rnorm(1000000, mean = X1 + X2 + X3 - 2 * R, sd = 1)

# Compare the parameter estimates

b1 = summary(glm(formula = as.formula('Target ~ X1 + X2 + X3'), family = gaussian(link = 'identity'), data = data.table(X1 = X1, X2 = X2, X3 = X3, Target = Target)))[['coefficients']]
b2 = summary(glm(formula = as.formula('Target ~ X1 + X2 + X3 + R'), family = gaussian(link = 'identity'), data = data.table(X1 = X1, X2 = X2, X3 = X3, R = R, Target = Target)))[['coefficients']]

b1
b2

# Example 3

# Sample from the space

SampleSize = 1000000

Sample = sample(x = seq(4), size = SampleSize, replace = TRUE, prob = NULL)

# Create a sample of random variables from the sample from the space

B = as.integer(Sample %in% c(1, 2, 3))
C = as.integer(Sample %in% c(1, 3))
X = (Sample %in% c(1, 2)) - (Sample %in% c(3, 4))

# Observe the mean

mean(X)

# Observe the conditional expectation of X given C

data.table(X = X, C = C)[, .(X = mean(X)), by = c('C')]

# Observe the conditional expectation of the conditional of X given B given C

merge(data.table(X = X, B = B, C = C), data.table(X = X, B = B)[, .(Rate = mean(X)), by = c('B')], by = c('B'))[, .(Rate = mean(Rate)), by = c('C')]

# Example 4

# Sample from the space

Sample = sample(x = seq(4), size = 1000000, replace = TRUE, prob = NULL)

# Create a sample of random variables from the sample from the space

A = as.integer(Sample %in% c(1, 3))
B = as.integer(Sample %in% c(1, 4))
C = as.integer(Sample %in% c(1, 2))
X = (Sample %in% c(1, 2)) - (Sample %in% c(3, 4))

# Observe the mean

mean(X)

# Observe the conditional expectations of X given A and B

data.table(X = X, A = A)[, .(X = mean(X)), by = c('A')]
data.table(X = X, B = B)[, .(X = mean(X)), by = c('B')]

# Observe the conditional expectations of the conditional expectations of X given A and B given C

merge(data.table(X = X, A = A, C = C), data.table(X = X, A = A)[, .(Rate = mean(X)), by = c('A')], by = c('A'))[, .(Rate = mean(Rate)), by = c('C')]
merge(data.table(X = X, B = B, C = C), data.table(X = X, B = B)[, .(Rate = mean(X)), by = c('B')], by = c('B'))[, .(Rate = mean(Rate)), by = c('C')]

# Show that conditioning on A and B are sufficient to specify the outcome

data.table(X = X, A = A, B = B, Sample = Sample)[, .(X = mean(X)), by = c('A', 'B', 'Sample')]

# Observe the conditional expectation of X given C

data.table(X = X, C = C)[, .(X = mean(X)), by = c('C')]

# Example 5

# Sample from the space

SampleSize = 1000000

Sample = sample(x = seq(4), size = SampleSize, replace = TRUE, prob = NULL)

# Create a sample of random variables from the sample from the space

A = as.integer(Sample %in% c(1, 2))
B = as.integer(Sample %in% c(1, 3))
C = as.integer(Sample %in% c(2, 3))

# Observe the conditional expectations of A, B, and A * B given C

data.table(A = A, C = C)[, .(A = mean(A)), by = c('C')]
data.table(B = B, C = C)[, .(B = mean(B)), by = c('C')]
data.table(A = A, B = B, C = C)[, .(AB = mean(A * B)), by = c('C')]