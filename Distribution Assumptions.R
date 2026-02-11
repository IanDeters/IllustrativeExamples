library(data.table)
library(tweedie)
library(statmod)
library(rootSolve)
library(openxlsx)

# Generate the data

set.seed(666)

NumberOfObservations = 100000

DataSet = data.table(x1 = runif(NumberOfObservations), x2 = runif(NumberOfObservations), x3 = runif(NumberOfObservations), x4 = runif(NumberOfObservations))

# Create a normal target

DataSet$Target = rnorm(n = NumberOfObservations, mean = 1 * DataSet$x1 + 2 * DataSet$x2 + 3 * DataSet$x3 + 4 * DataSet$x4, sd = 1)

# Check the creation

summary(glm(formula = 'Target ~ x1 + x2 + x3 + x4', family = gaussian(link = 'identity'), data = DataSet))[['coefficients']]

# Create the models with various distributional assumptions

NormalModel = glm(formula = 'Target ~ x1 + x2 + x3 + x4', family = gaussian(link = 'identity'), data = DataSet[0 < DataSet$Target])
PoissonModel = glm(formula = 'Target ~ x1 + x2 + x3 + x4', family = poisson(link = 'identity'), data = DataSet[0 < DataSet$Target, ])
PoissonModel = glm(formula = 'Target ~ x1 + x2 + x3 + x4', family = poisson(link = 'identity'), data = DataSet[0 < DataSet$Target, ], start = NormalModel$coefficients)
GammaModel = glm(formula = 'Target ~ x1 + x2 + x3 + x4', family = Gamma(link = 'identity'), data = DataSet[0 < DataSet$Target, ])
GammaModel = glm(formula = 'Target ~ x1 + x2 + x3 + x4', family = Gamma(link = 'identity'), data = DataSet[0 < DataSet$Target, ], start = NormalModel$coefficients)
TweedieModel = glm(formula = 'Target ~ x1 + x2 + x3 + x4', family = tweedie(var.power = 1.5, link.power = 1), data = DataSet[0 < DataSet$Target, ])
TweedieModel = glm(formula = 'Target ~ x1 + x2 + x3 + x4', family = tweedie(var.power = 1.5, link.power = 1), data = DataSet[0 < DataSet$Target, ], start = NormalModel$coefficients)

# Compare the parameter estimates

NormalModel$coefficients
PoissonModel$coefficients
GammaModel$coefficients
TweedieModel$coefficients

# Examine the correlations

cor(NormalModel$fitted.values, PoissonModel$fitted.values)
cor(NormalModel$fitted.values, GammaModel$fitted.values)
cor(NormalModel$fitted.values, TweedieModel$fitted.values)

# Create a Poisson target

DataSet$Target = rpois(n = NumberOfObservations, lambda = exp(1 * DataSet$x1 + 2 * DataSet$x2 + 3 * DataSet$x3 + 4 * DataSet$x4))

# Check the creation

summary(glm(formula = 'Target ~ x1 + x2 + x3 + x4', family = poisson(link = 'log'), data = DataSet))[['coefficients']]

# Create the models with various distributional assumptions

PoissonModel = glm(formula = 'Target ~ x1 + x2 + x3 + x4', family = poisson(link = 'log'), data = DataSet[0 < DataSet$Target, ])
NormalModel = glm(formula = 'Target ~ x1 + x2 + x3 + x4', family = gaussian(link = 'log'), data = DataSet[0 < DataSet$Target, ])
NormalModelcoefficients = NormalModel$coefficients
NormalModel = glm(formula = 'Target ~ x1 + x2 + x3 + x4', family = gaussian(link = 'log'), data = DataSet[0 < DataSet$Target, ], start = PoissonModel$coefficients)
GammaModel = glm(formula = 'Target ~ x1 + x2 + x3 + x4', family = Gamma(link = 'log'), data = DataSet[0 < DataSet$Target, ])
GammaModelcoefficients = GammaModel$coefficients
GammaModel = glm(formula = 'Target ~ x1 + x2 + x3 + x4', family = Gamma(link = 'log'), data = DataSet[0 < DataSet$Target, ], start = PoissonModel$coefficients)
TweedieModel = glm(formula = 'Target ~ x1 + x2 + x3 + x4', family = tweedie(var.power = 1.5, link.power = 0), data = DataSet[0 < DataSet$Target, ])
TweedieModelcoefficients = TweedieModel$coefficients
TweedieModel = glm(formula = 'Target ~ x1 + x2 + x3 + x4', family = tweedie(var.power = 1.5, link.power = 0), data = DataSet[0 < DataSet$Target, ], start = PoissonModel$coefficients)

# Compare the parameter estimates

PoissonModel$coefficients
NormalModelcoefficients
NormalModel$coefficients
GammaModelcoefficients
GammaModel$coefficients
TweedieModelcoefficients
TweedieModel$coefficients

# Examine the correlations

cor(PoissonModel$fitted.values, NormalModel$fitted.values)
cor(PoissonModel$fitted.values, GammaModel$fitted.values)
cor(PoissonModel$fitted.values, TweedieModel$fitted.values)

# Create a Gamma target

DataSet$Target = rgamma(n = NumberOfObservations, shape = exp(log(.5) + 1 * DataSet$x1 + 2 * DataSet$x2 + 3 * DataSet$x3 + 4 * DataSet$x4), scale = 2)

# Check the creation

GammaModel = glm(formula = 'Target ~ x1 + x2 + x3 + x4', family = Gamma(link = 'log'), data = DataSet)
summary(GammaModel)[['coefficients']]

# Create the models with various distributional assumptions

NormalModel = glm(formula = 'Target ~ x1 + x2 + x3 + x4', family = gaussian(link = 'log'), data = DataSet)
NormalModelcoefficients = NormalModel$coefficients
NormalModel = glm(formula = 'Target ~ x1 + x2 + x3 + x4', family = gaussian(link = 'log'), data = DataSet, start = GammaModel$coefficients)
PoissonModel = glm(formula = 'Target ~ x1 + x2 + x3 + x4', family = poisson(link = 'log'), data = DataSet)
PoissonModelcoefficients = PoissonModel$coefficients
PoissonModel = glm(formula = 'Target ~ x1 + x2 + x3 + x4', family = poisson(link = 'log'), data = DataSet, start = GammaModel$coefficients)
TweedieModel = glm(formula = 'Target ~ x1 + x2 + x3 + x4', family = tweedie(var.power = 1.5, link.power = 0), data = DataSet)
TweedieModelcoefficients = TweedieModel$coefficients
TweedieModel = glm(formula = 'Target ~ x1 + x2 + x3 + x4', family = tweedie(var.power = 1.5, link.power = 0), data = DataSet, start = GammaModel$coefficients)

# Compare the parameter estimates

GammaModel$coefficients
NormalModelcoefficients
NormalModel$coefficients
PoissonModelcoefficients
PoissonModel$coefficients
TweedieModelcoefficients
TweedieModel$coefficients

# Examine the correlations

cor(GammaModel$fitted.values, PoissonModel$fitted.values)
cor(GammaModel$fitted.values, NormalModel$fitted.values)
cor(GammaModel$fitted.values, TweedieModel$fitted.values)

# Create a Bernoulli target

DataSet$Target = rbinom(n = NumberOfObservations, size = 1, prob = 1 / (1 + exp((-1) * (1 * DataSet$x1 - 2 * DataSet$x2 + 3 * DataSet$x3 - 4 * DataSet$x4))))

# Check the creation

BernoulliModel = glm(formula = 'Target ~ x1 + x2 + x3 + x4', family = binomial(link = 'logit'), data = DataSet)
summary(BernoulliModel)[['coefficients']]

# Create the models with various distributional assumptions

DataSet$Target = as.numeric(DataSet$Target)

NormalModel = glm(formula = 'Target ~ x1 + x2 + x3 + x4', family = gaussian(link = 'logit'), data = DataSet)
NormalModel = glm(formula = 'Target ~ x1 + x2 + x3 + x4', family = gaussian(link = 'logit'), data = DataSet, start = BernoulliModel$coefficients)
PoissonModel = glm(formula = 'Target ~ x1 + x2 + x3 + x4', family = poisson(link = 'logit'), data = DataSet)
PoissonModel = glm(formula = 'Target ~ x1 + x2 + x3 + x4', family = poisson(link = 'logit'), data = DataSet, start = BernoulliModel$coefficients)

# Compare the parameter estimates

BernoulliModel$coefficients
NormalModel$coefficients
PoissonModel$coefficients

# Examine the correlations

cor(BernoulliModel$fitted.values, PoissonModel$fitted.values)
cor(BernoulliModel$fitted.values, NormalModel$fitted.values)

# Create a Tweedie target

DataSet$Target = rtweedie(n = NumberOfObservations, mu = exp(1 * DataSet$x1 + 2 * DataSet$x2 + 3 * DataSet$x3 + 4 * DataSet$x4), phi = 4, power = 1.5)

# Check the creation

summary(glm(formula = 'Target ~ x1 + x2 + x3 + x4', family = tweedie(var.power = 1.5, link.power = 0), data = DataSet))[['coefficients']]

# Create the models with various distributional assumptions

TweedieModel = glm(formula = 'Target ~ x1 + x2 + x3 + x4', family = tweedie(var.power = 1.5, link.power = 0), data = DataSet[0 < DataSet$Target, ])
NormalModel = glm(formula = 'Target ~ x1 + x2 + x3 + x4', family = gaussian(link = 'log'), data = DataSet[0 < DataSet$Target, ])
NormalModelcoefficients = NormalModel$coefficients
NormalModel = glm(formula = 'Target ~ x1 + x2 + x3 + x4', family = gaussian(link = 'log'), data = DataSet[0 < DataSet$Target, ], start = TweedieModel$coefficients)
PoissonModel = glm(formula = 'Target ~ x1 + x2 + x3 + x4', family = poisson(link = 'log'), data = DataSet[0 < DataSet$Target, ])
PoissonModelcoefficients = PoissonModel$coefficients
PoissonModel = glm(formula = 'Target ~ x1 + x2 + x3 + x4', family = poisson(link = 'log'), data = DataSet[0 < DataSet$Target, ], start = TweedieModel$coefficients)
GammaModel = glm(formula = 'Target ~ x1 + x2 + x3 + x4', family = Gamma(link = 'log'), data = DataSet[0 < DataSet$Target, ])
GammaModelcoefficients = GammaModel$coefficients
GammaModel = glm(formula = 'Target ~ x1 + x2 + x3 + x4', family = Gamma(link = 'log'), data = DataSet[0 < DataSet$Target, ], start = TweedieModel$coefficients)

# Compare the parameter estimates

TweedieModel$coefficients
NormalModelcoefficients
NormalModel$coefficients
PoissonModelcoefficients
PoissonModel$coefficients
GammaModelcoefficients
GammaModel$coefficients

# Examine the correlations

cor(TweedieModel$fitted.values, PoissonModel$fitted.values)
cor(TweedieModel$fitted.values, NormalModel$fitted.values)
cor(TweedieModel$fitted.values, GammaModel$fitted.values)

# Set the number of clusters and the standard deviation

n = 10
sd = .01

# Create a mapping from R^n into R^n such that the expected value at each value of j / n is j

meanSolveFunction = function(x){sapply(seq(n), function(j){sum(pnorm(q = rep(j, n) / n, mean = x, sd = sd))}) - seq(n)}

# Find the roots of the function

Roots = multiroot(f = meanSolveFunction, start = seq(n) / n - 3 * sd)

# Check the results

max(abs(meanSolveFunction(Roots$root)))

# Randomly select from the set of roots

DataSet = data.table(mean = sample(x = Roots$root, size = NumberOfObservations, replace = TRUE))

# Use the vector of means to create normal random variables

DataSet$Normal = rnorm(n = NumberOfObservations, mean = DataSet$mean, sd = sd)

# Check the creation

summary(glm('Normal ~ mean', family = gaussian(link = 'identity'), data = DataSet))[['coefficients']]
sqrt(summary(glm('Normal ~ mean', family = gaussian(link = 'identity'), data = DataSet))[['dispersion']])

# Create the empirical cumulative distribution function

ECDF = DataSet[order(DataSet$Normal), .(ECDF = .N), by = 'Normal']
ECDF$ECDF = cumsum(ECDF$ECDF) / sum(ECDF$ECDF)

max(abs(ECDF$Normal - ECDF$ECDF))

# Record the results

Workbook = createWorkbook()

addWorksheet(Workbook, 'ECDF')
writeData(Workbook, 'ECDF', ECDF)

saveWorkbook(Workbook, 'C:/My Documents/EP Analytic Solutions/Presentations/Distribution Assumptions.xlsx')