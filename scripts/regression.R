library(ggplot2)
library(gridExtra)
library(reshape2)
library(rstan)

travel_fl <- read.csv("../data/travel_florida.csv")
colnames(travel_fl) <- sapply(colnames(travel_fl), function(x){gsub("X", "", x)})
rownames(travel_fl) <- travel_fl[,1]
travel_fl <- travel_fl[,-1]

cases_fl <- read.csv("../data/travel_cases_florida.csv")
colnames(cases_fl) <- sapply(colnames(cases_fl), function(x){gsub("X", "", x)})
rownames(cases_fl) <- cases_fl[,1]
cases_fl <- cases_fl[,-1]

population_local <- read.csv("../data/local_population.csv")
colnames(population_local) <- sapply(colnames(population_local), function(x){gsub("X", "", x)})
rownames(population_local) <- population_local[,1]
population_local <- population_local[,-1]

cases_local <- read.csv("../data/local_cases.csv")
colnames(cases_local) <- sapply(colnames(cases_local), function(x){gsub("X", "", x)})
rownames(cases_local) <- cases_local[,1]
cases_local <- cases_local[,-1]

travel_incidence <- (cases_fl/travel_fl) * 100000
local_incidence <- (cases_local/population_local) * 100000

## Drop US Virgin Islands
travel_incidence <- travel_incidence[rownames(travel_incidence)!="Virgin Islands (US)",]
local_incidence <- local_incidence[rownames(local_incidence)!="Virgin Islands (US)",]

sociodem <- read.csv("../data/sociodem.csv", row.names=1)
meansuit <- read.csv("../data/mean_suitability.csv", row.names=1)
sdsuit <- read.csv("../data/sd_suitability.csv", row.names = 1)

colnames(meansuit) <- colnames(travel_incidence)
colnames(sdsuit) <- colnames(travel_incidence)

## Training Data
good.rep <- c("Jamaica", "Puerto Rico", "Grenada", "Venezuela", "Trinidad-Tobago")
tdf <- local_incidence - travel_incidence
tdf <- tdf[good.rep,]
tdf <- melt(t(tdf))
colnames(tdf) <- c("Month", "Country", "delta")

tdf.meansuit <- melt(t(meansuit[good.rep,]))
colnames(tdf.meansuit) <- c("Month", "Country", "MeanSuit")
tdf.sdsuit <- melt(t(sdsuit[good.rep,]))
colnames(tdf.sdsuit) <- c("Month", "Country", "SDSuit")

tdf.sociodem <- sociodem[good.rep,]

tdf <- cbind(tdf, tdf.meansuit[,"MeanSuit"])
tdf <- cbind(tdf, tdf.sdsuit[,"SDSuit"])
tdf <- cbind(tdf,tdf.sociodem[tdf[,"Country"],])
rownames(tdf) <- seq(1:nrow(tdf))
colnames(tdf) <- c("Month", "Country", "Delta", "MeanSuit", "SDSuit", "PhysiciansPer10000", "HospitalBedPer10000", "GDP", "PopulationDensity")

tdf <- tdf[complete.cases(tdf),]

## Test Data
bad.rep <- c("Cuba")
df <- local_incidence - travel_incidence
df <- df[bad.rep,]
df <- melt(t(df))
colnames(df) <- c("Month", "Country", "delta")

df.meansuit <- melt(t(meansuit[bad.rep,]))
colnames(df.meansuit) <- c("Month", "Country", "MeanSuit")
df.sdsuit <- melt(t(sdsuit[bad.rep,]))
colnames(df.sdsuit) <- c("Month", "Country", "SDSuit")

df.sociodem <- sociodem
df.sociodem[,"Country"] <- rownames(df.sociodem)

df <- merge(df, df.meansuit, by=c("Month", "Country"))
df <- merge(df, df.sdsuit, by=c("Month", "Country"))
df <- merge(df, df.sociodem, by="Country")


delta_glm <- glm( delta ~ MeanSuit + SDSuit + Physicians.10.000.people + Hospital.Beds.10.000.people + GDP.PPP + Population.Density, data = df, family="gaussian")
df <- df[!is.na(df[,"delta"]),]
df[,"predicted_delta"] <- predict(delta_glm, df)

m_norm<-stan(file="regression.stan",data = list())

library(rstanarm)

CHAINS <- 4
CORES <- 6
SEED <-  112358

delta_bglm <- stan_glm( delta ~ MeanSuit + SDSuit + Physicians.10.000.people + Hospital.Beds.10.000.people + GDP.PPP + Population.Density,
                              na.action = na.exclude,
                              data = df,
                              family = "gaussian",
                              prior = cauchy(0, 5),
                              prior_intercept = student_t(df = 7),
                              chains = CHAINS, cores = CORES, seed = SEED)

ci95 <- posterior_interval(delta_bglm, prob = 0.95, pars = "MeanSuit")
round(ci95, 2)

cbind(Median = coef(delta_bglm), MAD_SD = se(delta_bglm))

summary(residuals(delta_bglm)) # not deviance residuals

cov2cor(vcov(delta_bglm))

launch_shinystan(delta_bglm)

y_rep <- posterior_predict(delta_bglm)
dim(y_rep)

par(mfrow = 1:2, mar = c(5,3.7,1,0) + 0.1, las = 3)
boxplot(sweep(y_rep[,womensrole$gender == "Male"], 2, STATS = 
               womensrole$total[womensrole$gender == "Male"], FUN = "/"), 
        axes = FALSE, main = "Male", pch = NA,
        xlab = "Years of Education", ylab = "Proportion of Agrees")
with(womensrole, axis(1, at = education[gender == "Male"] + 1, 
                      labels = 0:20))
axis(2, las = 1)
with(womensrole[womensrole$gender == "Male",], 
     points(education + 1,  agree / (agree + disagree), 
            pch = 16, col = "red"))
boxplot(sweep(y_rep[,womensrole$gender == "Female"], 2, STATS = 
          womensrole$total[womensrole$gender == "Female"], FUN = "/"), 
          axes = FALSE, main = "Female", pch = NA,
        xlab = "Years of Education", ylab = "")
with(womensrole, axis(1, at = education[gender == "Female"] + 1,
     labels = 0:20))
with(womensrole[womensrole$gender == "Female",], 
     points(education + 1,  agree / (agree + disagree), 
            pch = 16, col = "red"))

(womensrole_bglm_2 <- update(delta_bglm, formula. = . ~ . + I(education^2)))

loo_bglm_1 <- loo(delta_bglm)
loo_bglm_2 <- loo(womensrole_bglm_2)

par(mfrow = 1:2, mar = c(5,3.8,1,0) + 0.1, las = 3)
plot(loo_bglm_1, label_points = TRUE)
plot(loo_bglm_2, label_points = TRUE)

compare_models(loo_bglm_1, loo_bglm_2)

loo_bglm_1

# note: in newdata we want agree and disgree to sum to the number of people we
# want to predict for. the values of agree and disagree don't matter so long as
# their sum is the desired number of trials. we need to explicitly imply the
# number of trials like this because our original data are aggregate. if we had
# bernoulli data then it would be a given we wanted to predict for single
# individuals.
newdata <- data.frame(agree = c(0,0), disagree = c(100,100), education = c(12,16),
                      gender = factor("Female", levels = c("Male", "Female")))
y_rep <- posterior_predict(womensrole_bglm_2, newdata)
summary(apply(y_rep, 1, diff))

bad_rhat <- stan_glm(mpg ~ ., data = mtcars, iter = 20, 
                     chains = CHAINS, cores = CORES, seed = SEED)

rhat <- summary(bad_rhat)[, "Rhat"]
rhat[rhat > 1.1]

