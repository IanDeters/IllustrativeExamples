# Generate the data

set.seed(666)

NumberOfObservations = 101

OrderingVariable = seq(NumberOfObservations)
Weight = runif(NumberOfObservations)
Target = rnorm(NumberOfObservations, seq(NumberOfObservations) * Weight, sd = 1 * sqrt(Weight)) / Weight

# Check the construction

summary(glm(Target ~ OrderingVariable, data = data.frame('OrderingVariable' = OrderingVariable, 'Weight' = Weight, 'Target' = Target), family = gaussian(link = 'identity'), weights = Weight))

# Write the data to the disk for examples in Excel

write.csv(x = data.frame('OrderingVariable' = OrderingVariable, 'Weight' = Weight, 'Target' = Target), file = '?')

# Define the objective function

ObjectiveFunction = function(x){sum(Weight * (x - Target)^2)}

# Create the matrix needed to define the inequality function

InequalityFunctionJacobianMatrix = matrix(data = 0, nrow = NumberOfObservations - 1, ncol = NumberOfObservations)
InequalityFunctionJacobianMatrix[matrix(data = c(seq(NumberOfObservations - 1), seq(NumberOfObservations - 1)), nrow = NumberOfObservations - 1, ncol = 2)] = -1
InequalityFunctionJacobianMatrix[matrix(data = c(seq(NumberOfObservations - 1), seq(2, NumberOfObservations)), nrow = NumberOfObservations - 1, ncol = 2)] = 1

# Define the inequality function

InequalityFunction = function(x){InequalityFunctionJacobianMatrix %*% x}

# Load the necessary library

library(alabama)

# Initialize the performance summary

Summary = data.frame()

# Run a constrained optimization with numerical differentiation ten times

StartTime = Sys.time()

for (i in seq(10))
{

  Results = auglag(par = cummax(Target), fn = ObjectiveFunction, hin = InequalityFunction, control.outer = list(kkt2.check = FALSE, eps = 1e-8))

}

Summary = rbind(Summary, data.frame('Optimization' = 'Constrained', 'ConstraintMapping' = NA, 'Derivative' = 'Numerical', 'AverageDuration' = as.numeric(Sys.time() - StartTime, units = 'secs') / 10))
Summary

# Define the objective function gradient

ObjectiveFunctionGradient = function(x){2 * Weight * (x - Target)}

# Create a random vector on which to test the derivative calculations

x = runif(NumberOfObservations)

# Check the formula for the derivative

max(abs(numDeriv::grad(ObjectiveFunction, x) - ObjectiveFunctionGradient(x)))

# Define the inequality function Jacobi

InequalityFunctionJacobian = function(x){InequalityFunctionJacobianMatrix}

# Check the formula for the derivative

max(abs(numDeriv::jacobian(InequalityFunction, x) - InequalityFunctionJacobianMatrix))

# Run a constrained optimization with formulaic differentiation ten times

StartTime = Sys.time()

for (i in seq(10))
{
  
  Results = auglag(par = cummax(Target), fn = ObjectiveFunction, gr = ObjectiveFunctionGradient, hin = InequalityFunction, hin.jac = InequalityFunctionJacobian, control.outer = list(kkt2.check = FALSE, eps = 1e-8))
  
}

Summary = rbind(Summary, data.frame('Optimization' = 'Constrained', 'ConstraintMapping' = NA, 'Derivative' = 'Formulaic', 'AverageDuration' = as.numeric(Sys.time() - StartTime, units = 'secs') / 10))
Summary

# Create an auxiliary matrix

LowerTriangularMatrix = lower.tri(diag(x = NumberOfObservations), diag = TRUE)

# Define the new objective function

ObjectiveFunction = function(x){sum(Weight * (LowerTriangularMatrix %*% exp(x) - Target)^2)}

# Create an initial value by creating the log of the difference of the components of the cumulative
# maximum of the target vector

InitialValue = log(cummax(Target) - c(0, cummax(Target)[seq(NumberOfObservations - 1)]))
InitialValue[is.infinite(InitialValue) == TRUE] = log(10^(-8))

# Check that the initial value returns the cumulative maximum of the target vector

max(abs(LowerTriangularMatrix %*% exp(InitialValue) - cummax(Target)))

# Run an unconstrained optimization with numeric differentiation ten times

StartTime = Sys.time()

for (i in seq(10))
{
  
  Results = optim(par = InitialValue, fn = ObjectiveFunction, method = 'BFGS', hessian = FALSE)
  
}

Summary = rbind(Summary, data.frame('Optimization' = 'Unconstrained', 'ConstraintMapping' = 'Exponential', 'Derivative' = 'Numerical', 'AverageDuration' = as.numeric(Sys.time() - StartTime, units = 'secs') / 10))
Summary

# Create an auxiliary matrix

UpperTriangularMatrix = t(LowerTriangularMatrix)

# Define the objective function gradient

ObjectiveFunctionGradient = function(x)
{
  
  2 * exp(x) * UpperTriangularMatrix %*% (Weight * (LowerTriangularMatrix %*% exp(x) - Target))
  
}

# Check the formula for the derivative

max(abs(numDeriv::grad(ObjectiveFunction, x) - ObjectiveFunctionGradient(x)))

# Run an unconstrained optimization with formulaic differentiation ten times

StartTime = Sys.time()

for (i in seq(10))
{
  
  Results = optim(par = InitialValue, fn = ObjectiveFunction, gr = ObjectiveFunctionGradient, method = 'BFGS', hessian = FALSE)
  
}

Summary = rbind(Summary, data.frame('Optimization' = 'Unconstrained', 'ConstraintMapping' = 'Exponential', 'Derivative' = 'Formulaic', 'AverageDuration' = as.numeric(Sys.time() - StartTime, units = 'secs') / 10))
Summary

# Create a large vector of random numbers

x = runif(10^7)

# Exponentiate the numbers ten times

StartTime = Sys.time()

for (i in seq(10))
{
  
  exp(x)
  
}

as.numeric(Sys.time() - StartTime, units = 'secs') / 10

# Square the numbers ten times

StartTime = Sys.time()

for (i in seq(10))
{
  
  x^2
  
}

as.numeric(Sys.time() - StartTime, units = 'secs') / 10

# Define the new objective function

ObjectiveFunction = function(x){sum(Weight * (LowerTriangularMatrix %*% (x^2) - Target)^2)}

# Create an initial value by creating the square root of the difference of the components of the cumulative
# maximum of the target vector

InitialValue = sqrt(cummax(Target) - c(0, cummax(Target)[seq(NumberOfObservations - 1)]))

# Check that the initial value returns the cumulative maximum of the target vector

max(abs(LowerTriangularMatrix %*% InitialValue^2 - cummax(Target)))

# Run an unconstrained optimization with numerical differentiation ten times

StartTime = Sys.time()

for (i in seq(10))
{
  
  Results = optim(par = InitialValue, fn = ObjectiveFunction, method = 'BFGS', hessian = FALSE)
  
}

Summary = rbind(Summary, data.frame('Optimization' = 'Unconstrained', 'ConstraintMapping' = 'Squaring', 'Derivative' = 'Numerical', 'AverageDuration' = as.numeric(Sys.time() - StartTime, units = 'secs') / 10))
Summary

# Define the objective function gradient

ObjectiveFunctionGradient = function(x)
{
  
  4 * x * UpperTriangularMatrix %*% (Weight * (LowerTriangularMatrix %*% (x^2) - Target))
  
}

# Check the formula for the derivative

x = runif(NumberOfObservations)
max(abs(numDeriv::grad(ObjectiveFunction, x) - ObjectiveFunctionGradient(x)))

# Run an unconstrained optimization with formulaic differentiation ten times

StartTime = Sys.time()

for (i in seq(10))
{
  
  Results = optim(par = InitialValue, fn = ObjectiveFunction, gr = ObjectiveFunctionGradient, method = 'BFGS', hessian = FALSE)
  
}

Summary = rbind(Summary, data.frame('Optimization' = 'Unconstrained', 'ConstraintMapping' = 'Squaring', 'Derivative' = 'Formulaic', 'AverageDuration' = as.numeric(Sys.time() - StartTime, units = 'secs') / 10))
Summary

# Load the necessary library

library(data.table)

# Apply the function

StartTime = Sys.time()

CheckThis = IncreasingGroups(OrderingVariable, Weight = Weight, Target = Target, Tolerance = .00000001, Direction = 1, MethodList = 'BFGS')

EndTime = Sys.time()

# Observe the duration

EndTime - StartTime

CheckThis = data.table(Weight = Weight, Target = Target, OrderingVariable = OrderingVariable, Group = CheckThis)

# Merge in the grouped values of the target variable

CheckThis = merge(CheckThis, CheckThis[, .(GroupedTarget = sum(Weight * Target) / sum(Weight)), by = c('Group')], by = 'Group')

# Observe the results

min(CheckThis$GroupedTarget[seq(2, nrow(CheckThis))] - CheckThis$GroupedTarget[seq(nrow(CheckThis) - 1)])
as.data.frame(CheckThis[, c('OrderingVariable', 'Weight', 'Target', 'GroupedTarget')])

# Increase the number of observations

NumberOfObservations = 2000

OrderingVariable = seq(NumberOfObservations)
Weight = runif(NumberOfObservations)
Target = rnorm(NumberOfObservations, seq(NumberOfObservations) * Weight, sd = 1 * sqrt(Weight)) / Weight

# Check the construction

summary(glm(Target ~ OrderingVariable, data = data.frame('OrderingVariable' = OrderingVariable, 'Weight' = Weight, 'Target' = Target), family = gaussian(link = 'identity'), weights = Weight))

# Apply the function

StartTime = Sys.time()

CheckThis = IncreasingGroups(OrderingVariable, Weight = Weight, Target = Target, Tolerance = .0001, Direction = 1, MethodList = c('BFGS'))

EndTime = Sys.time()

# Observe the duration

EndTime - StartTime

CheckThis = data.table(Weight = Weight, Target = Target, OrderingVariable = OrderingVariable, Group = CheckThis)

# Summarize to the level of the grouping

CheckThis = merge(CheckThis, CheckThis[, .(GroupedTarget = sum(Weight * Target) / sum(Weight)), by = c('Group')], by = 'Group')

# Observe the results

min(CheckThis$GroupedTarget[seq(2, nrow(CheckThis))] - CheckThis$GroupedTarget[seq(nrow(CheckThis) - 1)])
as.data.frame(CheckThis[, c('OrderingVariable', 'Weight', 'Target', 'GroupedTarget')])

# Examine grouping discretizations

StartTime = Sys.time()

CheckThis = IncreasingGroupSearch(Weight = Weight, Target = Target, OrderingVariable = OrderingVariable, Direction = 1, Tolerance = .0001, MethodList = c('BFGS'), DiscretizationVector = seq(10, 1000, by = 10), RawDiscrete = TRUE, Cluster = NULL)

EndTime = Sys.time()

# Observe the duration

EndTime - StartTime

# Create a table to check the results

CheckThis02 = data.table(OrderingVariable = OrderingVariable, Weight = Weight, Target = Target, Group = Discretize(Weight = Weight, x = OrderingVariable, GroupNumber = CheckThis[['Discretization']]), IncreasingGroup = CheckThis[['Grouping']])

# Observe that the increasing groups are in agreement

max(abs(CheckThis02$IncreasingGroup - IncreasingGroups(OrderingVariable = CheckThis02$Group, Weight = Weight, Target = Target, Tolerance = .0001, Direction = 1, MethodList = c('BFGS'))))

# Merge in the average value of the target variable by group

CheckThis02 = merge(CheckThis02, CheckThis02[, .(GroupedTarget = sum(Weight * Target) / sum(Weight)), by = 'IncreasingGroup'], by = 'IncreasingGroup')

# Observe the error is recovered

abs(sum(CheckThis02$Weight * (CheckThis02$Target - CheckThis02$GroupedTarget)^2) - CheckThis[['Error']])
as.data.frame(CheckThis02[, c('OrderingVariable', 'Weight', 'Target', 'GroupedTarget')])