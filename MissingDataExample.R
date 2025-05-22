library(data.table)

# Set the number of observations

NumberOfObservations = 1000

# Set the seed

set.seed(666)

# Generate predictor variables

DataSet = data.table(x1 = runif(NumberOfObservations), x2 = runif(NumberOfObservations), x3 = runif(NumberOfObservations))

# Generate the target variable

DataSet$Target = rnorm(NumberOfObservations, mean = as.matrix(DataSet) %*% seq(3) + 4, sd = 1)

# Check the creation

summary(glm(formula = as.formula('Target ~ x1 + x2 + x3'), family = gaussian(link = 'identity'), data = DataSet))

# Mark different values as missing

DataSet$x1[sample(c(TRUE, FALSE), NumberOfObservations, replace = TRUE, prob = c(1 / 3, 2 / 3))] = NA
DataSet$x2[sample(c(TRUE, FALSE), NumberOfObservations, replace = TRUE, prob = c(1 / 3, 2 / 3))] = NA
DataSet$x3[sample(c(TRUE, FALSE), NumberOfObservations, replace = TRUE, prob = c(1 / 3, 2 / 3))] = NA

# Create a variable indicating which variables are missing

DataSet$Situation = is.na(DataSet[, c('x1', 'x2', 'x3')]) %*% (2^seq(0, 2))

# Give the variables a bogus value

DataSet$x1[is.na(DataSet$x1) == TRUE] = -1
DataSet$x2[is.na(DataSet$x2) == TRUE] = -1
DataSet$x3[is.na(DataSet$x3) == TRUE] = -1

DataSet$i1 = (DataSet$x1 == -1)
DataSet$i2 = (DataSet$x2 == -1)
DataSet$i3 = (DataSet$x3 == -1)

# Create a model where x1 is populated

Populatedcoefficients = glm(formula = as.formula('Target ~ x1'), family = gaussian(link = 'identity'), data = DataSet[!(DataSet$i1), ])[['coefficients']]

# Create a model where x1 is missing

Missingcoefficients = glm(formula = as.formula('Target ~ 1'), family = gaussian(link = 'identity'), data = DataSet[DataSet$i1, ])[['coefficients']]

# Create a model on all data, controlling for the missing values of x1

Allcoefficients = glm(formula = as.formula('Target ~ x1 + i1'), family = gaussian(link = 'identity'), data = DataSet)[['coefficients']]

# Compare the coefficients

Populatedcoefficients
Missingcoefficients
Allcoefficients

# Recover the intercept for the model on the missing data

Allcoefficients[1] + (-1) * Allcoefficients[2] + Allcoefficients[3]

# Create the full design matrix

DataSet02 = do.call(cbind, lapply(seq(0, 7), function(i){cbind(DataSet[, paste('x', seq(3), sep = '')[!(as.logical(DataSet[DataSet$Situation == i, paste('i', seq(3), sep = ''), with = FALSE][1, ]))], with = FALSE], rep(1, nrow(DataSet))) * matrix(data = (DataSet$Situation == i), nrow = nrow(DataSet), ncol = (as.matrix(!(DataSet[DataSet$Situation == i, paste('i', seq(3), sep = ''), with = FALSE])) %*% rep(1, 3) + 1)[1])}))

# Update the names

names(DataSet02) = ifelse(names(DataSet02) == 'V2', paste('I', cumsum(names(DataSet02) == 'V2') - 1, sep = ''), paste(names(DataSet02), '_', cumsum(names(DataSet02) == 'V2'), sep = ''))

# Observe that the number of columns conforms to the expected number: 2^(3 - 1)(3 + 2) = 20

ncol(DataSet02)

# Create the formula for the total model

TotalFormula = paste(names(DataSet02), collapse = ' + ')
TotalFormula = paste('Target ~ ', TotalFormula, ' + 0', sep = '')

# Include the target

DataSet02$Target = DataSet$Target

# Create the model on the total data

TotalModel = glm(formula = TotalFormula, family = gaussian(link = 'identity'), data = DataSet02)

# Create the total model on the full data

DataSet$TotalModel = predict(TotalModel, DataSet02)

for (i in seq(0, 7))
{
  
  # Observe the difference from the total model to one developed on the individual situations
   
  print(max(abs(DataSet$TotalModel[DataSet$Situation == i] - glm(formula = 'Target ~ x1 + x2 + x3', family = gaussian(link = 'identity'), data = DataSet[DataSet$Situation == i, ])[['fitted.values']])))
  
}