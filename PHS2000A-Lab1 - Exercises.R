library(ggplot2)
library(tidyverse)


##########################
#~~ Coding Exercise #1 ~~#


# Import the population data
ice_cream <- read.csv("C:/Users/denaj/Dropbox/PhD Course for PHS/Labs/PHS2000A_2021/Lab 1 - Sampling/ice_cream.csv") # update this line with the directory where you saved the CSV file
pop_size <- nrow(ice_cream)


# Draw the random sample of n=10
set.seed(12345)
n <- 10
samp_ids <- sample(1:pop_size, n)
samp <- ice_cream[samp_ids, ]
samp

# What is the sample mean? What's the sample variance? 

>>>>>

##########################
#~~ Coding Exercise #2 ~~#

# How many combinations are possible

choose(16000, 10)

# Draw 1000 samples of size 10
set.seed(12345)
all.samples <- replicate(1000, sample(1:pop_size, size = 10))

sample.means <- rep(NA, 1000)
sample.vars <- rep(NA, 1000)
for (i in 1:1000) {
  samp_obs <- all.samples[, i]
  vals <- ice_cream$scoops[samp_obs]
  sample.means[i] <- mean(vals)
  sample.vars[i] <- var(vals)
}

# Look at the sample means from the first few samples of size 10
head(sample.means)

# Plot the sampling distribution of means from samples of size 10
ggplot(data.frame(sample.means)) +
  geom_histogram(aes(x = sample.means), bins = 50) +
  theme_classic() +
  coord_cartesian(expand = FALSE) +
  labs(
    title = "Sampling distribution of sample mean of X, with n = 10",
    x = "sample mean"
  )

mean(sample.means)
var(sample.means)

# actual variance of sampling distribution of sample mean
var(sample.means)
# population variance of random variable
var_scoops <- var(ice_cream$scoops) * (pop_size - 1) / pop_size
# sampling variance
var_scoops
(var_scoops / n)

# why is the actual variance of sampling distribution of sample mean different from the sampling variance?

# Alter code above to produce and describe the sampling distribution from 1000 samples of size 100

>>>>>


##########################
#~~ Coding Exercise #3 ~~#

## Stratified random sampling

# check distribution by field
table(as.factor(ice_cream$field))

# subset by field
GHP <- subset(ice_cream, field == "GHP")
EH <- subset(ice_cream, field == "EH")
Nut <- subset(ice_cream, field == "Nut")
SBS <- subset(ice_cream, field == "SBS")
Epi <- subset(ice_cream, field == "Epi")

# sample 1/5 (n=20) from each subset
n_strat <- 100

# sample 1/5 from each subset
set.seed(039682)
samp_strat <- rbind(
  GHP[sample(1:nrow(GHP), n_strat/5), ],
  EH[sample(1:nrow(EH), n_strat/5), ],
  Nut[sample(1:nrow(Nut), n_strat/5), ],
  SBS[sample(1:nrow(SBS), n_strat/5), ],
  Epi[sample(1:nrow(Epi), n_strat/5), ]
)
  
# subset each sample by fields of study of interest
samp_nut_strat <- subset(samp_strat, field == "Nut")
samp_ghp_strat <- subset(samp_strat, field == "GHP")
samp_eh_strat <- subset(samp_strat, field == "EH")
samp_epi_strat <- subset(samp_strat, field == "Epi")
samp_sbs_strat <- subset(samp_strat, field == "SBS")
  
  
# and stratified means of interest
samp_nut_mean_strat <- mean(samp_nut_strat$scoops)
samp_ghp_mean_strat <- mean(samp_ghp_strat$scoops)
samp_eh_mean_strat <- mean(samp_eh_strat$scoops)
samp_epi_mean_strat <- mean(samp_epi_strat$scoops)
samp_sbs_mean_strat <- mean(samp_sbs_strat$scoops)
  
# apply weights
samp_nut_mean_strat.w <- mean(samp_nut_strat$scoops)*(1626/16000)
samp_ghp_mean_strat.w <- mean(samp_ghp_strat$scoops)*(1608/16000)
samp_eh_mean_strat.w <- mean(samp_eh_strat$scoops)*(3978/16000)
samp_epi_mean_strat.w <- mean(samp_epi_strat$scoops)*(5610/16000)
samp_sbs_mean_strat.w <- mean(samp_sbs_strat$scoops)*(3178/16000)

# add to find overall mean

>>>>>