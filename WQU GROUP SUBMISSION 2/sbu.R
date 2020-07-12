library(bvartools) # Load the package, which contains the data
data("e6") # Load data
plot(e6) # Plot data

library(vars) # Load package

# Estimate VAR
var_aic <- VAR(e6, type = "const", lag.max = 8, ic = "AIC", season = 4)

# Lag order suggested by AIC
var_aic$p