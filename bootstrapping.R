# James Caldwell
# Feb 2024
# 
# This R script demonstrates the concept of bootstrapping as a method for quantifying uncertainty in a fitted curve. It generates synthetic data using a defined data-generating process, fits a polynomial model to the data, and visualizes the true model alongside bootstrap samples and their confidence intervals.
# The script begins by defining functions for simulating data and the true mean function. It then generates synthetic data, creates scatterplots, fits polynomial models, and plots the true model and bootstrapped samples.
# The bootstrap resampling process involves randomly sampling observations with replacement from the original dataset, fitting polynomial models to each bootstrap sample, and calculating confidence intervals for the fitted curves.
# Finally, the script saves all generated plots as PNG files for further analysis or documentation

library(tidymodels)# for optional tidymodels solutions
library(tidyverse) # functions for data manipulation  

library(broom)
library(splines)

#Bootstrapping 

# Bootstrap resampling can be used to quantify the uncertainty in a fitted curve. 

## Data Generating Process

sim_x <- function(n) runif(n,min = 0, max = 2) # U[0,2]
f <- function(x) 1 + 2*x + 5*sin(5*x) # true mean function
sim_y <- function(x){ # generate Y|X from N{f(x),sd}
  n = length(x)
  f(x) + rnorm(n, mean = 0, sd = 2.5)
  
}

#-- Settings
n = 100       # number of observations

#-- Generate Data
set.seed(211)
x = sim_x(n)
y = sim_y(x)

data_train = tibble(x,y)

#-- Scatterplot: Tidyverse
ggplot(tibble(x,y), aes(x,y)) +
  geom_point() + 
  geom_function(fun=f, color="blue")

#-- Settings
n = 100       # number of observations

#-- Generate Data
set.seed(211)
x = sim_x(n)
y = sim_y(x)

data_train = tibble(x,y)

#-- Scatterplot: Tidyverse
ggplot(tibble(x,y), aes(x,y)) +
  geom_point() + 
  geom_function(fun=f, color="blue")


xseq = seq(0, 2, length=200)
fit5 = lm(y~poly(x,5))   # 5th deg polyfit
yhat5 = predict(fit5, tibble(x=xseq))

#: make data for plotting; convert to long format 
pred.data = tibble(x=xseq, fifth=yhat5, true=f(xseq)) %>%
  pivot_longer(cols=-x, names_to="model", values_to="y")

#: plot
ggplot(tibble(x,y), aes(x,y)) +
  geom_point() +
  geom_line(data=pred.data, aes(color=model)) +
  scale_color_manual(values=c(true = "blue", fifth="red"))

M = 200 # number of bootstrap samples

eval_pts = tibble(x=seq(0, 2, length=100))

yhat5 = matrix(NA, length(eval_pts), M) # initialize matrix for fitted values
yhat5 = matrix(NA, 100, M) # initialize matrix for fitted values

set.seed(212)

for(m in 1:M){
  # sample indices/rows from empirical distribution (with replacement)
  ind = sample(n, replace=TRUE)
  
  data_boot = data_train[ind,]
  m_boot = lm(y~poly(x,5), data=data_boot) #
  
  yhat5[,m] = predict(m_boot, eval_pts)
  
}

# : Convert to tibble and plot
data_fit = as_tibble(yhat5, .name_repair = "unique_quiet") %>% # convert matrix to tibble
  bind_cols(eval_pts) %>% # add the eval points
  pivot_longer(-x, names_to="simulation", values_to="y") # convert to long format

# print(head(yhat5,15))
print(head(data_fit,30))

true_data <- pred.data %>%
  filter(model == "true")

ggplot(data_train, aes(x, y)) +
  geom_line(data = true_data, aes(color = "True Model"), linetype = "solid", linewidth = 1) +
  geom_line(data = data_fit, aes(color = "Bootstraps", group = simulation), alpha = 0.1, linewidth = 0.5) +
  geom_point() +
  scale_color_manual(values = c("True Model" = "blue", "Bootstraps" = "red"), labels = c("True Model" = "True Model", "Bootstraps" = "Bootstraps")) +
  labs(title = "Comparison of True Model and Bootstraps",
       x = "X",
       y = "Y")


print(dim(yhat5))

# Calculate pointwise 95% confidence intervals
upper_ci <- apply(yhat5, 1, quantile, probs = 0.975)
lower_ci <- apply(yhat5, 1, quantile, probs = 0.025)

#-- Generate Data
set.seed(211)
x = sim_x(n)
y = sim_y(x)
ci_x <- seq(0, 2, length.out = 100)

# Create data frame for upper and lower confidence intervals
# ci_data <- data.frame(x = xseq, upper_ci = upper_ci, lower_ci = lower_ci)
ci_data <- data.frame(x = x, y = y, upper_ci = upper_ci, lower_ci = lower_ci,ci_x = ci_x)

print(head(ci_data))
print(dim(ci_data))

ggplot(ci_data, 
       aes(x,y)) +
  geom_point() +
  geom_ribbon(data = ci_data, aes(x = ci_x, ymin = lower_ci, ymax = upper_ci), fill = "gray", alpha = 0.5) +
  geom_line(data=pred.data, aes(color=model)) +
  scale_color_manual(values=c(true = "blue", fifth="red"))





