Levene's test: Use the function below to generate the Lenvene's deviates. The function calculates from the median but you could just as easily use the mode or mean.

# y = response variable
# group = grouping levels (Group1:Group2:Etc)
levene.test <- function(y, group) {
    meds <- tapply(y, group, median, na.rm=TRUE)
    abs(y - meds[group])}

Usage:

outbred.averaged$ls <- with(outbred.averaged, levene.test(CentroidSize, Sex:Survival))

Once you have your new levene's deviates variable you just use it as your dependent variable.

base.levene <- lm(ls ~ Sex + Survival, data = outbred.averaged)

Coefficient of Variation: Estimates

cv <- function(x) sd(x)/mean(x)

Usage: You can calculate for a an entire data set or groups within the data set.

cv(outbred.averaged$CentroidSize)

cv.base <- with(outbred.averaged, tapply(CentroidSize, INDEX = Sex:Survival, FUN = cv))

Coefficient of Variation: Nonparamentric bootstrapped confidence intervals

# data = your data set
# response = Column heading for numeric variable for calculating CV
# groups = Column headings for grouping variables in the data set
boot.cv <- function(data, response, groups) {
  with(data[sample(nrow(data), nrow(data),replace = T),], 
    tapply(eval(parse(text=response)), eval(parse(text=groups)), FUN=cv))
}

Usage: Items that appear in quotes must be in quotes.

cv.bootstrap.female.base <- replicate(2000, boot.cv(data = outbred.averaged, response = "CentroidSize", groups = "Survival"))