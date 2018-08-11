library(lavaan)
library(sem)
library(semTools)
library(semPlot)

setwd("C:/Users/User-pc/Dropbox/Education/2016 Essex MA on Social Sciences Data Analysis/2018 Confirmatory Factor Analysis and SEM/Exercises/Day 05")
data <- read.table("NL2.dat")

# Change missing values reported as 7-9 in .dat file into NA into R

sort(unique(data$V1), na.last = FALSE)

sort(unique(data$V2), na.last = FALSE)
data$V2[data$V2 == 7] <- NA
data$V2[data$V2 == 8] <- NA
sort(unique(data$V2), na.last = FALSE)

sort(unique(data$V3), na.last = FALSE)
data$V3[data$V3 == 7] <- NA
data$V3[data$V3 == 8] <- NA
sort(unique(data$V3), na.last = FALSE)

sort(unique(data$V4), na.last = FALSE)
data$V4[data$V4 == 7] <- NA
data$V4[data$V4 == 8] <- NA
sort(unique(data$V4), na.last = FALSE)

sort(unique(data$V5), na.last = FALSE)

sort(unique(data$V6), na.last = FALSE)
data$V6[data$V6 == 9] <- NA
sort(unique(data$V6), na.last = FALSE)

sort(unique(data$V7), na.last = FALSE)
data$V7[data$V7 == 7] <- NA
data$V7[data$V7 == 8] <- NA
sort(unique(data$V7), na.last = FALSE)

sort(unique(data$V8), na.last = FALSE)

sort(unique(data$V9), na.last = FALSE)
data$V9[data$V9 == 9] <- NA
sort(unique(data$V9), na.last = FALSE)

sort(unique(data$V10), na.last = FALSE)
data$V10[data$V10 == 9] <- NA
sort(unique(data$V10), na.last = FALSE)

sort(unique(data$V11), na.last = FALSE)
data$V11[data$V11 == 9] <- NA
sort(unique(data$V11), na.last = FALSE)

sort(unique(data$V12), na.last = FALSE)
data$V12[data$V12 == 9] <- NA
sort(unique(data$V12), na.last = FALSE)

sort(unique(data$V13), na.last = FALSE)
data$V13[data$V13 == 9] <- NA
sort(unique(data$V13), na.last = FALSE)

sort(unique(data$V14), na.last = FALSE)
data$V14[data$V14 == 9] <- NA
sort(unique(data$V14), na.last = FALSE)

sort(unique(data$V15), na.last = FALSE)
data$V15[data$V15 == 9] <- NA
sort(unique(data$V15), na.last = FALSE)

# Change column names
var_names <- c("Country", "imsmetn", "imdfetn", "impcntr", "gndr", "yrbrn", "edulvl", "eduyrs", "ipmodst", "imptrad", "iprule", "ipbhprp", "ipeqopt", "ipudrst", "impenv")
data <- `colnames<-`(data, var_names)
remove(var_names)

# Confirmatory Factor Analysis: model with one latent variables and ten free independent variables
model <- 'TraCo =~ ipmodst + imptrad + iprule + ipbhprp'

# Test for configural invariance
conf_inv <- cfa(model = model, data = data, missing = "FIML", group = "gndr")

summary(conf_inv, header = TRUE, fit.measures = TRUE, standardized = TRUE, estimates = TRUE, ci = TRUE, fmi = TRUE, cov.std = TRUE, rsquare = TRUE, std.nox = FALSE, modindices = TRUE, nd = 3L) # All values are the same as in MPlus

modificationindices(conf_inv)

# Test for metric invariance
metr_inv <- cfa(model = model, data = data, missing = "FIML", group = "gndr", group.equal = c("loadings"))

summary(metr_inv, header = TRUE, fit.measures = TRUE, standardized = TRUE, estimates = TRUE, ci = FALSE, fmi = TRUE, cov.std = TRUE, rsquare = TRUE, std.nox = FALSE, modindices = TRUE, nd = 3L) # All values are the same as in MPlus

modificationindices(metr_inv)

# Test for scalar invariance
scal_inv <- cfa(model = model, data = data, missing = "FIML", group = "gndr", group.equal = c("loadings", "intercepts"))

summary(scal_inv, header = TRUE, fit.measures = TRUE, standardized = TRUE, estimates = TRUE, ci = FALSE, fmi = TRUE, cov.std = TRUE, rsquare = TRUE, std.nox = FALSE, modindices = TRUE, nd = 3L) # All values are the same as in MPlus BUT NOT THE MODIFICATION INDEXES

modificationindices(scal_inv, minimum.value = 0, sort. = TRUE)

measurementInvariance(model = model, data = data, group = "gndr" )

# Plot the paths for configural invariance
semPaths(conf_inv, "est", layout = "tree", mar = c(9,1,9,1), intercepts = TRUE, title = TRUE, panelGroups = TRUE, sizeInt = 10, sizeInt2 = 10, sizeMan = 10, sizeMan2 = 10, sizeLat = 10, sizeLat2 = 10, edge.label.cex = 1.5)
semPaths(conf_inv, "est", layout = "tree", mar = c(9,1,9,1), intercepts = TRUE,  title = TRUE, groups = 2, sizeInt = 10, sizeInt2 = 10, sizeMan = 10, sizeMan2 = 10, sizeLat = 10, sizeLat2 = 10, edge.label.cex = 1.5)








