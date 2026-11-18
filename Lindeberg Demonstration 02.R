# Observe the relevant directories

DependsOnDirectory = ''
MethodsDirectory = DependsOnDirectory

# Set the sample size and number of simulations

SampleSize = 10000
SimulationNumber = 1000000

# Create the function vector

FunctionVector = c('data.table', 'betaMethodOfMoments', 'binomMethodOfMoments', 'chisqMethodOfMoments', 'gammaMethodOfMoments', 'lnormMethodOfMoments', 'nbinomMethodOfMoments', 'unifMethodOfMoments', 'weibullMethodOfMoments02', 'doParallel', 'Discretize', 'ProcessCompletionText', 'gammaConditionalMoment', 'lnormConditionalExpectation', 'weibullConditionalMoment')

# Load openxlsx so as to read the function dependency information

library(openxlsx)

# Read in the function data

DependsOn = read.xlsx(xlsxFile = paste(DependsOnDirectory, '/R Methods.xlsx', sep = ''), sheet = 'DependsOn')

# Update the function vector with the dependencies

FunctionVector = unique(c(FunctionVector, DependsOn$DependsOn[(DependsOn$Function %in% FunctionVector) & (is.na(DependsOn$DependsOn) == FALSE)]))

for (i in seq(length(FunctionVector)))
{
  
  # If the function exists in the methods directory, then load it
  
  if (file.exists(paste(MethodsDirectory, '/', FunctionVector[i], '.R', sep = '')))
  {
    
    source(paste(MethodsDirectory, '/', FunctionVector[i], '.R', sep = ''))
    
  }
  
  # If the function does not exist in the methods directory then assume it refers to a library and load the
  # library
  
  else
  {
    
    library(FunctionVector[i], character.only = TRUE)
    
  }
  
}

# The following function randomly selects a distribution from one of fourteen distributions in base R. The three ignored distributions
# are the Cauchy, Hypergeometric, and Multinomial.  The Caucy is ignored since it has no mean.  The Hypergeometric is ignored since it
# has three parameters and hence is not totally specified by a mean and variance.  The Multinomial is ignored since it is vector
# valued.

# The function randomly selects a mean and variance between the given bounds, applies distribution specific bounds to the values,
# applies the method of moments to the mean and variance, and then draws from the specified distribution.

DistributionGeneration = function(MeanMin = 0, MeanMax = 100000, VarianceMin = 0, VarianceMax = 500000^2)
{
  
  # Clean the bounds

  MeanMin = pmin(MeanMin, MeanMax)
  MeanMax = pmax(MeanMin, MeanMax)
  
  VarianceMin = pmax(0, VarianceMin)
  VarianceMin = pmin(VarianceMin, VarianceMax)
  VarianceMax = pmax(VarianceMin, VarianceMax)
  
  # Select the distribution
  
  Distribution = sample(x = seq(12), size = 1, replace = FALSE)

  if (Distribution == 1) # beta distribution
  {
    
    # Apply distribution specific bounds
    
    MeanLowerBound = pmin(pmax(0, MeanMin), 1)
    MeanUpperBound = pmax(pmin(1, MeanMax), 0)
    
    # Generate a mean
    
    Mean = runif(n = 1, min = MeanLowerBound, max = MeanUpperBound)
    
    # Apply distribution specific bounds
    
    VarianceLowerBound = pmin(VarianceMin, Mean * (1 - Mean))
    VarianceUpperBound = pmin(VarianceMax, Mean * (1 - Mean))
    
    # Generate a variance
    
    Variance = runif(n = 1, min = VarianceLowerBound, max = VarianceUpperBound)

    # Apply the method of moments
    
    shape1 = betaMethodOfMoments(Mean, Variance)
    shape2 = shape1[2]
    shape1 = shape1[1]
    
    # Select from the distribution
    
    return(data.frame('Distribution' = 'beta', 'Observation' = rbeta(n = 1, shape1 = shape1, shape2 = shape2), 'Mean' = Mean, 'Variance' = Variance))

  }
  
  if (Distribution == 2) # Binomial distribution
  {
    
    # Apply distribution specific bounds
    
    MeanLowerBound = pmax(0, MeanMin)
    MeanUpperBound = pmax(0, MeanMax)
    
    # Generate a mean
    
    Mean = runif(n = 1, min = MeanLowerBound, max = MeanUpperBound)
    
    # Apply distribution specific bounds
    
    VarianceLowerBound = pmin(VarianceMin, Mean)
    VarianceUpperBound = pmin(VarianceMax, Mean)
    
    # Generate a variance
    
    Variance = runif(n = 1, min = VarianceLowerBound, max = VarianceUpperBound)
    
    # Apply the method of moments
    
    prob = binomMethodOfMoments(Mean, Variance)
    size = round(prob[2])
    prob = prob[1]
    
    # Select from the distribution
    
    return(data.frame('Distribution' = 'binom', 'Observation' = rbinom(n = 1, size = size, prob = prob), 'Mean' = Mean, 'Variance' = Variance))
    
  }
  
  if (Distribution == 3) # Chi squared distribution
  {
    
    # Apply distribution specific bounds
    
    MeanLowerBound = pmax(0, MeanMin)
    MeanUpperBound = pmax(MeanMin, MeanMax)
    
    # Generate a mean
    
    Mean = runif(n = 1, min = MeanLowerBound, max = MeanUpperBound)
    
    # Apply distribution specific bounds
    
    VarianceLowerBound = pmax(VarianceMin, 2 * Mean)
    VarianceUpperBound = pmin(VarianceMax, 4 * Mean)
    
    # Generate a variance
    
    Variance = runif(n = 1, min = VarianceLowerBound, max = VarianceUpperBound)
    
    # Apply the method of moments
    
    df = chisqMethodOfMoments(Mean, Variance)
    ncp = df[2]
    df = df[1]
    
    # Select from the distribution
    
    return(data.frame('Distribution' = 'chisq', 'Observation' = rchisq(n = 1, df = df, ncp = ncp), 'Mean' = Mean, 'Variance' = Variance))

  }
  
  if (Distribution == 4) # Exponential distribution
  {
    
    # Apply distribution specific bounds
    
    MeanLowerBound = pmax(0, MeanMin)
    MeanUpperBound = pmax(0, MeanMax)
    
    # Generate a mean
    
    Mean = runif(n = 1, min = MeanLowerBound, max = MeanUpperBound)
    
    # Select from the distribution

    return(data.frame('Distribution' = 'exp', 'Observation' = rexp(n = 1, rate = 1 / Mean), 'Mean' = Mean, 'Variance' = Mean^2))
    
  }
    
  if (Distribution == 5) # gamma distribution
  {
    
    # Apply distribution specific bounds
    
    MeanLowerBound = pmax(0, MeanMin)
    MeanUpperBound = pmax(MeanLowerBound, MeanMax)
    
    # Generate a mean
    
    Mean = runif(n = 1, min = MeanLowerBound, max = MeanUpperBound)
    
    # Apply distribution specific bounds
    
    VarianceLowerBound = VarianceMin
    VarianceUpperBound = VarianceMax
    
    # Generate a variance
    
    Variance = runif(n = 1, min = VarianceLowerBound, max = VarianceUpperBound)
    
    shape = gammaMethodOfMoments(Mean, Variance)
    scale = shape[2]
    shape = shape[1]
    
    # Select from the distribution
    
    return(data.frame('Distribution' = 'gamma', 'Observation' = rgamma(n = 1, shape = shape, scale = scale), 'Mean' = Mean, 'Variance' = Variance))
    
  }
  
  if (Distribution == 6) # Geometric distribution
  {

    # Apply distribution specific bounds
    
    MeanLowerBound = pmax(1, MeanMin)
    MeanUpperBound = pmax(MeanLowerBound, MeanMax)
    
    # Generate a mean
    
    Mean = runif(n = 1, min = MeanLowerBound, max = MeanUpperBound)
    
    # Select from the distribution
    
    return(data.frame('Distribution' = 'geom', 'Observation' = rgeom(n = 1, prob = 1 / Mean), 'Mean' = Mean, 'Variance' = Mean^2 - Mean))
    
  }

  if (Distribution == 7) # Lognormal distribution
  {
    
    # Apply distribution specific bounds
    
    MeanLowerBound = pmax(0, MeanMin)
    MeanUpperBound = pmax(MeanLowerBound, MeanMax)
    
    # Generate a mean
    
    Mean = runif(n = 1, min = MeanLowerBound, max = MeanUpperBound)
    
    # Apply distribution specific bounds
    
    VarianceLowerBound = VarianceMin
    VarianceUpperBound = VarianceMax
    
    # Generate a variance
    
    Variance = runif(n = 1, min = VarianceLowerBound, max = VarianceUpperBound)
    
    meanlog = lnormMethodOfMoments(Mean, Variance)
    sdlog = meanlog[2]
    meanlog = meanlog[1]
    
    # Select from the distribution
    
    return(data.frame('Distribution' = 'lnorm', 'Observation' = rlnorm(n = 1, meanlog = meanlog, sdlog = sdlog), 'Mean' = Mean, 'Variance' = Variance))
    
  }

  if (Distribution == 8) # Negative binomial distribution
  {
    
    # Apply distribution specific bounds
    
    MeanLowerBound = pmax(0, MeanMin)
    MeanUpperBound = pmax(MeanLowerBound, MeanMax)
    
    # Generate a mean
    
    Mean = runif(n = 1, min = MeanLowerBound, max = MeanUpperBound)
    
    # Apply distribution specific bounds
    
    VarianceLowerBound = pmax(Mean, VarianceMin)
    VarianceUpperBound = VarianceMax
    
    # Generate a variance
    
    Variance = runif(n = 1, min = VarianceLowerBound, max = VarianceUpperBound)
    
    size = nbinomMethodOfMoments(Mean, Variance)
    prob = size[2]
    size = size[1]

    # Select from the distribution

    return(data.frame('Distribution' = 'nbinom', 'Observation' = rnbinom(n = 1, size = size, prob = prob), 'Mean' = Mean, 'Variance' = Variance))
    
  }
  
  if (Distribution == 9) # normal distribution
  {
    
    # Apply distribution specific bounds
    
    MeanLowerBound = MeanMin
    MeanUpperBound = MeanMax
    
    # Generate a mean
    
    Mean = runif(n = 1, min = MeanLowerBound, max = MeanUpperBound)
    
    # Apply distribution specific bounds
    
    VarianceLowerBound = VarianceMin
    VarianceUpperBound = VarianceMax
    
    # Generate a variance
    
    Variance = runif(n = 1, min = VarianceLowerBound, max = VarianceUpperBound)
    
    # Select from the distribution
    
    return(data.frame('Distribution' = 'norm', 'Observation' = rnorm(n = 1, mean = Mean, sd = sqrt(Variance)), 'Mean' = Mean, 'Variance' = Variance))
    
  }
  
  if (Distribution == 10) # Poisson distribution
  {
    
    # Apply distribution specific bounds
    
    MeanLowerBound = pmax(0, MeanMin)
    MeanUpperBound = pmax(MeanLowerBound, MeanMax)
    
    # Generate a mean
    
    Mean = runif(n = 1, min = MeanLowerBound, max = MeanUpperBound)

    # Select from the distribution
    
    return(data.frame('Distribution' = 'pois', 'Observation' = rpois(n = 1, lambda = Mean), 'Mean' = Mean, 'Variance' = Mean))
    
  }
  
  if (Distribution == 11) # Uniform distribution
  {
    
    # Apply distribution specific bounds
    
    MeanLowerBound = MeanMin
    MeanUpperBound = MeanMax
    
    # Generate a mean
    
    Mean = runif(n = 1, min = MeanLowerBound, max = MeanUpperBound)
    
    # Apply distribution specific bounds
    
    VarianceLowerBound = VarianceMin
    VarianceUpperBound = VarianceMax
    
    # Generate a variance
    
    Variance = runif(n = 1, min = VarianceLowerBound, max = VarianceUpperBound)
    
    min = unifMethodOfMoments(Mean, Variance)
    max = min[2]
    min = min[1]
    
    # Select from the distribution

    return(data.frame('Distribution' = 'unif', 'Observation' = runif(n = 1, min = min, max = max), 'Mean' = Mean, 'Variance' = Variance))
    
  }
  
  if (Distribution == 12) # Weibull distribution
  {
    
    # Apply distribution specific bounds
    
    MeanLowerBound = pmax(0, MeanMin)
    MeanUpperBound = pmax(MeanMin, MeanMax)
    
    # Generate a mean
    
    Mean = runif(n = 1, min = MeanLowerBound, max = MeanUpperBound)
    
    # Apply distribution specific bounds
    
    VarianceLowerBound = VarianceMin
    VarianceUpperBound = VarianceMax
    
    # Generate a variance
    
    Variance = runif(n = 1, min = VarianceLowerBound, max = VarianceUpperBound)
    
    scale = weibullMethodOfMoments(Mean, Variance)
    shape = scale[2]
    scale = scale[1]

    # Select from the distribution
    
    return(data.frame('Distribution' = 'weibull', 'Observation' = rweibull(n = 1, shape = shape, scale = scale), 'Mean' = Mean, 'Variance' = Variance))
    
  }

}

# Create a cluster for parallel execution

detectCores()
Cluster = makeCluster(32)

# Register the cluster

registerDoParallel(Cluster)

# Generate some observations

set.seed(666)

Results = as.data.table(do.call(rbind, foreach(X = seq(15 * SampleSize)) %dopar% {set.seed(X)
  DistributionGeneration(MeanMin = 0, MeanMax = 25000, VarianceMin = 0, VarianceMax = 150000^2)}))

# Get a sense of the results

Results[order(Results$Distribution), .(Count = .N), by = 'Distribution']

# gamma

Mean = mean(Results$Mean[Results$Distribution == 'gamma'][seq(SampleSize)])
Variance = mean(Results$Variance[Results$Distribution == 'gamma'][seq(SampleSize)])

# Use the method of moments

shape = gammaMethodOfMoments(Mean, Variance)
scale = shape[2]
shape = shape[1]

# Demonstrate the efficacy of the method

Mean
shape * scale

Variance
shape * scale^2

# Run the simulations

Simulations = do.call(c, foreach(X = seq(SimulationNumber)) %dopar% {set.seed(X)
  mean(rgamma(n = SampleSize, shape = shape, scale = scale))})
Simulations = data.table(Group = Discretize(Weight = rep(1, length(Simulations)), x = Simulations, GroupNumber = 10000), Simulation = Simulations)

# Record the results

Workbook = createWorkbook()

addWorksheet(wb = Workbook, sheetName = 'gamma')
writeData(wb = Workbook, sheet = 'gamma', x = Simulations[order(Simulations$Simulation), .(Count = .N, Minimum = min(Simulation), Simulation = mean(Simulation), Maximum = max(Simulation), StandardDeviation = sd(Simulation)), by = 'Group'])

addWorksheet(wb = Workbook, sheetName = 'gammaParameters')
writeData(wb = Workbook, sheet = 'gammaParameters', x = data.table(Mean = c(Mean, mean(Simulations$Simulation)), Variance = c(Variance, var(Simulations$Simulation))))

# lnorm

Mean = mean(Results$Mean[Results$Distribution == 'lnorm'][seq(SampleSize)])
Variance = mean(Results$Variance[Results$Distribution == 'lnorm'][seq(SampleSize)])

# Use the method of moments

meanlog = lnormMethodOfMoments(Mean, Variance)
sdlog = meanlog[2]
meanlog = meanlog[1]

# Demonstrate the efficacy of the method

Mean
exp(meanlog + .5 * sdlog^2)

Variance
exp(2 * meanlog + sdlog^2) * (exp(sdlog^2) - 1)

# Run the simulations

Simulations = do.call(c, foreach(X = seq(SimulationNumber)) %dopar% {set.seed(X)
  mean(rlnorm(n = SampleSize, meanlog = meanlog, sdlog = sdlog))})
Simulations = data.table(Group = Discretize(Weight = rep(1, length(Simulations)), x = Simulations, GroupNumber = 10000), Simulation = Simulations)

# Record the results

addWorksheet(wb = Workbook, sheetName = 'lnorm')
writeData(wb = Workbook, sheet = 'lnorm', x = Simulations[order(Simulations$Simulation), .(Count = .N, Minimum = min(Simulation), Simulation = mean(Simulation), Maximum = max(Simulation), StandardDeviation = sd(Simulation)), by = 'Group'])

addWorksheet(wb = Workbook, sheetName = 'lnormParameters')
writeData(wb = Workbook, sheet = 'lnormParameters', x = data.table(Mean = c(Mean, mean(Simulations$Simulation)), Variance = c(Variance, var(Simulations$Simulation))))

# Increase the sample size by a factor of 100

Simulations = do.call(c, foreach(X = seq(SimulationNumber)) %dopar% {set.seed(X)
  mean(rlnorm(n = 100 * SampleSize, meanlog = meanlog, sdlog = sdlog))})
Simulations = data.table(Group = Discretize(Weight = rep(1, length(Simulations)), x = Simulations, GroupNumber = 10000), Simulation = Simulations)

# Record the results

addWorksheet(wb = Workbook, sheetName = 'lnorm02')
writeData(wb = Workbook, sheet = 'lnorm02', x = Simulations[order(Simulations$Simulation), .(Count = .N, Minimum = min(Simulation), Simulation = mean(Simulation), Maximum = max(Simulation), StandardDeviation = sd(Simulation)), by = 'Group'])

addWorksheet(wb = Workbook, sheetName = 'lnormParameters02')
writeData(wb = Workbook, sheet = 'lnormParameters02', x = data.table(Mean = c(Mean, mean(Simulations$Simulation)), Variance = c(Variance, var(Simulations$Simulation))))

# weibull

Mean = mean(Results$Mean[Results$Distribution == 'weibull'][seq(SampleSize)])
Variance = mean(Results$Variance[Results$Distribution == 'weibull'][seq(SampleSize)])

# Use the method of moments

scale = weibullMethodOfMoments(Mean, Variance)
shape = scale[2]
scale = scale[1]

# Demonstrate the efficacy of the method

Mean
scale * gamma(1 + 1 / shape)

Variance
scale^2 * (gamma(1 + 2 / shape) - gamma(1 + 1 / shape)^2)

# Run the simulations

Simulations = do.call(c, foreach(X = seq(SimulationNumber)) %dopar% {set.seed(X)
  mean(rweibull(n = SampleSize, shape = shape, scale = scale))})
Simulations = data.table(Group = Discretize(Weight = rep(1, length(Simulations)), x = Simulations,GroupNumber = 10000), Simulation = Simulations)

# Record the results

addWorksheet(wb = Workbook, sheetName = 'weibull')
writeData(wb = Workbook, sheet = 'weibull', x = Simulations[order(Simulations$Simulation), .(Count = .N, Minimum = min(Simulation), Simulation = mean(Simulation), Maximum = max(Simulation), StandardDeviation = sd(Simulation)), by = 'Group'])

addWorksheet(wb = Workbook, sheetName = 'weibullParameters')
writeData(wb = Workbook, sheet = 'weibullParameters', x = data.table(Mean = c(Mean, mean(Simulations$Simulation)), Variance = c(Variance, var(Simulations$Simulation))))

# Write the parameters of the function

MeanMin = 0
MeanMax = 25000
VarianceMin = 0
VarianceMax = 150000^2

# Initialize the table of distribution parameters

DistributionMoments = data.table()

CheckThis = function(i) # Beta distribution
{
  
  # Apply distribution specific bounds
  
  MeanLowerBound = pmin(pmax(0, MeanMin), 1)
  MeanUpperBound = pmax(pmin(1, MeanMax), 0)
  
  # Generate a mean
  
  Mean = runif(n = 1, min = MeanLowerBound, max = MeanUpperBound)
  
  # Apply distribution specific bounds
  
  VarianceLowerBound = pmin(VarianceMin, Mean * (1 - Mean))
  VarianceUpperBound = pmin(VarianceMax, Mean * (1 - Mean))
  
  # Generate a variance
  
  Variance = runif(n = 1, min = VarianceLowerBound, max = VarianceUpperBound)
  
  # Apply the method of moments
  
  shape1 = betaMethodOfMoments(Mean, Variance)
  shape2 = shape1[2]
  shape1 = shape1[1]
  
  # Select from the distribution
  
  rbeta(n = 1, shape1 = shape1, shape2 = shape2)
  
}

# Check the values

Sample = sapply(X = seq(SampleSize), FUN = CheckThis)

mean(Sample)
var(Sample)

DistributionMoments = rbind(DistributionMoments, data.table(Distribution = 'beta', Mean = .5, Variance = 1/6))

CheckThis = function(i) # Binomial distribution
{
  
  # Apply distribution specific bounds
  
  MeanLowerBound = pmax(0, MeanMin)
  MeanUpperBound = pmax(0, MeanMax)
  
  # Generate a mean
  
  Mean = runif(n = 1, min = MeanLowerBound, max = MeanUpperBound)
  
  # Apply distribution specific bounds
  
  VarianceLowerBound = pmin(VarianceMin, Mean)
  VarianceUpperBound = pmin(VarianceMax, Mean)
  
  # Generate a variance
  
  Variance = runif(n = 1, min = VarianceLowerBound, max = VarianceUpperBound)
  
  # Apply the method of moments
  
  prob = binomMethodOfMoments(Mean, Variance)
  size = round(prob[2])
  prob = prob[1]
  
  # Select from the distribution
  
  rbinom(n = 1, size = size, prob = prob)
  
}

# Check the values

Sample = sapply(X = seq(SampleSize), FUN = CheckThis)

mean(Sample)
var(Sample)

DistributionMoments = rbind(DistributionMoments, data.table(Distribution = 'binomial', Mean = 12500, Variance = 25000^2 / 12 + .5 * .5 * 25000))

CheckThis = function(i) # Chi squared distribution
{
  
  # Apply distribution specific bounds
  
  MeanLowerBound = pmax(0, MeanMin)
  MeanUpperBound = pmax(MeanMin, MeanMax)
  
  # Generate a mean
  
  Mean = runif(n = 1, min = MeanLowerBound, max = MeanUpperBound)
  
  # Apply distribution specific bounds
  
  VarianceLowerBound = pmax(VarianceMin, 2 * Mean)
  VarianceUpperBound = pmin(VarianceMax, 4 * Mean)
  
  # Generate a variance
  
  Variance = runif(n = 1, min = VarianceLowerBound, max = VarianceUpperBound)
  
  # Apply the method of moments
  
  df = chisqMethodOfMoments(Mean, Variance)
  ncp = df[2]
  df = df[1]
  
  # Select from the distribution
  
  rchisq(n = 1, df = df, ncp = ncp)
  
}

# Check the values

Sample = sapply(X = seq(SampleSize), FUN = CheckThis)

mean(Sample)
var(Sample)

DistributionMoments = rbind(DistributionMoments, data.table(Distribution = 'chisq', Mean = 12500, Variance = 25000^2 / 12 + .5 * .5 * (4 * 25000 - 2 * 25000)))

CheckThis = function(i) # Exponential distribution
{
  
  # Apply distribution specific bounds
  
  MeanLowerBound = pmax(0, MeanMin)
  MeanUpperBound = pmax(0, MeanMax)
  
  # Generate a mean
  
  Mean = runif(n = 1, min = MeanLowerBound, max = MeanUpperBound)
  
  # Select from the distribution
  
  rexp(n = 1, rate = 1 / Mean)
  
}

Sample = sapply(X = seq(SampleSize), FUN = CheckThis)

mean(Sample)
var(Sample)

DistributionMoments = rbind(DistributionMoments, data.table(Distribution = 'exp', Mean = 12500, Variance = 25000^2 / 12 + 25000^2 / 3))

CheckThis = function(i) # gamma distribution
{
  
  # Apply distribution specific bounds
  
  MeanLowerBound = pmax(0, MeanMin)
  MeanUpperBound = pmax(MeanLowerBound, MeanMax)
  
  # Generate a mean
  
  Mean = runif(n = 1, min = MeanLowerBound, max = MeanUpperBound)
  
  # Apply distribution specific bounds
  
  VarianceLowerBound = VarianceMin
  VarianceUpperBound = VarianceMax
  
  # Generate a variance
  
  Variance = runif(n = 1, min = VarianceLowerBound, max = VarianceUpperBound)
  
  shape = gammaMethodOfMoments(Mean, Variance)
  scale = shape[2]
  shape = shape[1]
  
  # Select from the distribution
  
  rgamma(n = 1, shape = shape, scale = scale)
  
}

Sample = sapply(X = seq(100 * SampleSize), FUN = CheckThis)

mean(Sample)
var(Sample)

DistributionMoments = rbind(DistributionMoments, data.table(Distribution = 'gamma', Mean = 12500, Variance = 25000^2 / 12 + .5 * 150000^2))

CheckThis = function(i) # Geometric distribution
{
  
  # Apply distribution specific bounds
  
  MeanLowerBound = pmax(1, MeanMin)
  MeanUpperBound = pmax(MeanLowerBound, MeanMax)
  
  # Generate a mean
  
  Mean = runif(n = 1, min = MeanLowerBound, max = MeanUpperBound)
  
  # Select from the distribution
  
  rgeom(n = 1, prob = 1 / Mean)
  
}

Sample = sapply(X = seq(SampleSize), FUN = CheckThis)

mean(Sample)
var(Sample)

DistributionMoments = rbind(DistributionMoments, data.table(Distribution = 'geom', Mean = .5 * (25000 + 1), Variance = (25000 - 1)^2 / 12 + (25000 - 1)^2 / 3 - .5 * (25000 - 1)))

CheckThis = function(i) # Lognormal distribution
{
  
  # Apply distribution specific bounds
  
  MeanLowerBound = pmax(0, MeanMin)
  MeanUpperBound = pmax(MeanLowerBound, MeanMax)
  
  # Generate a mean
  
  Mean = runif(n = 1, min = MeanLowerBound, max = MeanUpperBound)
  
  # Apply distribution specific bounds
  
  VarianceLowerBound = VarianceMin
  VarianceUpperBound = VarianceMax
  
  # Generate a variance
  
  Variance = runif(n = 1, min = VarianceLowerBound, max = VarianceUpperBound)
  
  meanlog = lnormMethodOfMoments(Mean, Variance)
  sdlog = meanlog[2]
  meanlog = meanlog[1]
  
  # Select from the distribution
  
 rlnorm(n = 1, meanlog = meanlog, sdlog = sdlog)
  
}

Sample = sapply(X = seq(100 * SampleSize), FUN = CheckThis)

mean(Sample)
var(Sample)

DistributionMoments = rbind(DistributionMoments, data.table(Distribution = 'lnorm', Mean = 12500, Variance = 25000^2 / 12 + .5 * 150000^2))

CheckThis = function(i) # Negative binomial distribution
{
  
  # Apply distribution specific bounds
  
  MeanLowerBound = pmax(0, MeanMin)
  MeanUpperBound = pmax(MeanLowerBound, MeanMax)
  
  # Generate a mean
  
  Mean = runif(n = 1, min = MeanLowerBound, max = MeanUpperBound)
  
  # Apply distribution specific bounds
  
  VarianceLowerBound = pmax(Mean, VarianceMin)
  VarianceUpperBound = VarianceMax
  
  # Generate a variance
  
  Variance = runif(n = 1, min = VarianceLowerBound, max = VarianceUpperBound)
  
  size = nbinomMethodOfMoments(Mean, Variance)
  prob = size[2]
  size = size[1]
  
  # Select from the distribution
  
  rnbinom(n = 1, size = size, prob = prob)
  
}

Sample = sapply(X = seq(100 * SampleSize), FUN = CheckThis)

mean(Sample)
var(Sample)

DistributionMoments = rbind(DistributionMoments, data.table(Distribution = 'nbinom', Mean = 12500, Variance = 25000^2 / 12 + .5 * 150000^2 - .5 * .5 * 25000))

CheckThis = function(i) # normal distribution
{
  
  # Apply distribution specific bounds
  
  MeanLowerBound = MeanMin
  MeanUpperBound = MeanMax
  
  # Generate a mean
  
  Mean = runif(n = 1, min = MeanLowerBound, max = MeanUpperBound)
  
  # Apply distribution specific bounds
  
  VarianceLowerBound = VarianceMin
  VarianceUpperBound = VarianceMax
  
  # Generate a variance
  
  Variance = runif(n = 1, min = VarianceLowerBound, max = VarianceUpperBound)
  
  # Select from the distribution
  
  rnorm(n = 1, mean = Mean, sd = sqrt(Variance))
  
}

Sample = sapply(X = seq(SampleSize), FUN = CheckThis)

mean(Sample)
var(Sample)

DistributionMoments = rbind(DistributionMoments, data.table(Distribution = 'norm', Mean = 12500, Variance = 25000^2 / 12 + .5 * 150000^2))

CheckThis = function(i) # Poisson distribution
{
  
  # Apply distribution specific bounds
  
  MeanLowerBound = pmax(0, MeanMin)
  MeanUpperBound = pmax(MeanLowerBound, MeanMax)
  
  # Generate a mean
  
  Mean = runif(n = 1, min = MeanLowerBound, max = MeanUpperBound)
  
  # Select from the distribution
  
  rpois(n = 1, lambda = Mean)
  
}

Sample = sapply(X = seq(SampleSize), FUN = CheckThis)

mean(Sample)
var(Sample)

DistributionMoments = rbind(DistributionMoments, data.table(Distribution = 'pois', Mean = 12500, Variance = 25000^2 / 12 + .5 * 25000))

CheckThis = function(i) # Uniform distribution
{
  
  # Apply distribution specific bounds
  
  MeanLowerBound = MeanMin
  MeanUpperBound = MeanMax
  
  # Generate a mean
  
  Mean = runif(n = 1, min = MeanLowerBound, max = MeanUpperBound)
  
  # Apply distribution specific bounds
  
  VarianceLowerBound = VarianceMin
  VarianceUpperBound = VarianceMax
  
  # Generate a variance
  
  Variance = runif(n = 1, min = VarianceLowerBound, max = VarianceUpperBound)
  
  min = unifMethodOfMoments(Mean, Variance)
  max = min[2]
  min = min[1]
  
  # Select from the distribution
  
  runif(n = 1, min = min, max = max)
  
}

Sample = sapply(X = seq(SampleSize), FUN = CheckThis)

mean(Sample)
var(Sample)

DistributionMoments = rbind(DistributionMoments, data.table(Distribution = 'unif', Mean = 12500, Variance = 25000^2 / 12 + .5 * 150000^2))

CheckThis = function(i) # Weibull distribution
{
  
  # Apply distribution specific bounds
  
  MeanLowerBound = pmax(0, MeanMin)
  MeanUpperBound = pmax(MeanMin, MeanMax)
  
  # Generate a mean
  
  Mean = runif(n = 1, min = MeanLowerBound, max = MeanUpperBound)
  
  # Apply distribution specific bounds
  
  VarianceLowerBound = VarianceMin
  VarianceUpperBound = VarianceMax
  
  # Generate a variance
  
  Variance = runif(n = 1, min = VarianceLowerBound, max = VarianceUpperBound)
  
  scale = weibullMethodOfMoments(Mean, Variance)
  shape = scale[2]
  scale = scale[1]
  
  # Select from the distribution
  
  rweibull(n = 1, shape = shape, scale = scale)
  
}

Sample = sapply(X = seq(SampleSize), FUN = CheckThis)

mean(Sample)
var(Sample)

DistributionMoments = rbind(DistributionMoments, data.table(Distribution = 'weibull', Mean = 12500, Variance = 25000^2 / 12 + .5 * 150000^2))

# Calculate the expected mean and variance from the sampling scheme

Mean = mean(DistributionMoments$Mean)
Variance = mean(DistributionMoments$Variance) + var(DistributionMoments$Mean) * (nrow(DistributionMoments) - 1) / nrow(DistributionMoments)

# Sample from the very random distribution

Results02 = as.data.table(do.call(rbind, foreach(X = seq(100 * SampleSize)) %dopar% {set.seed(X)
  DistributionGeneration(MeanMin = 0, MeanMax = 25000, VarianceMin = 0, VarianceMax = 150000^2)}))

# Observe the similarity of the values

Mean
mean(Results02$Mean)
mean(Results02$Observation)

Variance
mean(Results02$Variance)
var(Results02$Observation)

# Run the simulations

StartTime = Sys.time()

Simulations = do.call(c, foreach(X = seq(SimulationNumber)) %dopar% {set.seed(X)
  sum(sapply(X = seq(SampleSize), FUN = function(i){DistributionGeneration(MeanMin = 0, MeanMax = 25000, VarianceMin = 0, VarianceMax = 150000^2)[['Observation']]}))})

EndTime = Sys.time()

as.numeric(EndTime - StartTime, units = 'secs')

ProcessCompletionText()

# Stop the cluster

stopCluster(Cluster)

# Check the connections

showConnections()

Simulations = data.table(Group = Discretize(Weight = rep(1, length(Simulations)), x = Simulations, GroupNumber = 10000), Simulation = Simulations)

# Record the results

addWorksheet(wb = Workbook, sheetName = 'RandomDistribution')
writeData(wb = Workbook, sheet = 'RandomDistribution', x = Simulations[order(Simulations$Simulation), .(Count = .N, Minimum = min(Simulation), Simulation = mean(Simulation), Maximum = max(Simulation), StandardDeviation = sd(Simulation)), by = 'Group'])

addWorksheet(wb = Workbook, sheetName = 'RandomDistributionParameters')
writeData(wb = Workbook, sheet = 'RandomDistributionParameters', x = data.table(Mean = c(Mean, mean(Simulations$Simulation)), Variance = c(Variance, var(Simulations$Simulation))))

saveWorkbook(wb = Workbook, file = '')

# Apply the method of moments

meanlog = lnormMethodOfMoments(50, 100)
sdlog = meanlog[2]
meanlog = meanlog[1]

# Demonstrate the efficacy of the method

exp(meanlog + .5 * sdlog^2)
exp(2 * meanlog + sdlog^2) * (exp(sdlog^2) - 1)

# Compute the skewness

(exp(sdlog^2) + 2) * sqrt(exp(sdlog^2) - 1)

# Observe it coheres to empirical results

mean(((rlnorm(n = SimulationNumber, meanlog = meanlog, sdlog = sdlog) - 50) / sqrt(100))^3)

# Apply the method of moments

meanlog = lnormMethodOfMoments(12500, .5 * 150000^2)
sdlog = meanlog[2]
meanlog = meanlog[1]

# Demonstrate the efficacy of the method

exp(meanlog + .5 * sdlog^2)
exp(2 * meanlog + sdlog^2) * (exp(sdlog^2) - 1)

# Compute the skewness

(exp(sdlog^2) + 2) * sqrt(exp(sdlog^2) - 1)

# The volatility of the variable precludes empirical verification in a reasonable amount of time

# mean(((rlnorm(n = SimulationNumber, meanlog = meanlog, sdlog = sdlog) - 12500) / sqrt(.5 * 150000^2))^3)

# Apply the method of moments

shape = gammaMethodOfMoments(12500, .5 * 150000^2)
scale = shape[2]
shape = shape[1]

# Demonstrate the efficacy of the method

shape * scale
shape * scale^2

# Compute the skewness

2 / sqrt(shape)

# Observe it coheres to empirical results

mean(((rgamma(n = SimulationNumber, shape = shape, scale = scale) - 12500) / sqrt(.5 * 150000^2))^3)

# Find the right tail quantile at which the gamma equals the lognormal

RootFunction = function(p){qgamma(p = p, shape = shape, scale = scale) - qlnorm(p = p, meanlog = meanlog, sdlog = sdlog)}

p = uniroot(f = RootFunction, interval = c(.99, .999999), tol = .Machine$double.eps)[['root']]

# Verify the results

qgamma(p = p, shape = shape, scale = scale)
qlnorm(p = p, meanlog = meanlog, sdlog = sdlog)
p

gammaConditionalMoment(shape = shape, scale = scale, min = qgamma(p = p, shape = shape, scale = scale), max = Inf, moment = 1)
lnormConditionalExpectation(meanlog = meanlog, sdlog = sdlog, min = qlnorm(p = p, meanlog = meanlog, sdlog = sdlog), max = Inf)

# Apply the method of moments

scale = weibullMethodOfMoments(12500, .5 * 150000^2)
shape = scale[2]
scale = scale[1]

# Demonstrate the efficacy of the method

scale * gamma(1 + 1 / shape)
scale^2 * (gamma(1 + 2 / shape) - gamma(1 + 1 / shape)^2)

# Compute the skewness

(gamma(1 + 3 / shape) * scale^3 - 3 * 12500 * .5 * 150000^2 - 12500^3) / sqrt(.5 * 150000^2)^3

# Observe it coheres to empirical results

mean(((rweibull(n = 1000 * SimulationNumber, shape = shape, scale = scale) - 12500) / sqrt(.5 * 150000^2))^3)

# Find the right tail quantile at which the gamma equals the lognormal

RootFunction = function(p){qweibull(p = p, shape = shape, scale = scale) - qlnorm(p = p, meanlog = meanlog, sdlog = sdlog)}

p = uniroot(f = RootFunction, interval = c(.99, .999999), tol = .Machine$double.eps)[['root']]

# Verify the results

qweibull(p = p, shape = shape, scale = scale)
qlnorm(p = p, meanlog = meanlog, sdlog = sdlog)
p

weibullConditionalMoment(scale = scale, shape = shape, min = qweibull(p = p, shape = shape, scale = scale), max = Inf, moment = 1)
lnormConditionalExpectation(meanlog = meanlog, sdlog = sdlog, min = qlnorm(p = p, meanlog = meanlog, sdlog = sdlog), max = Inf)