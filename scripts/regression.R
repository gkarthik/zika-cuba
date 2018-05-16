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
meansuit <- read.csv("../data/mean_suitability.csv", row.names = 1)
sdsuit <- read.csv("../data/mean_suitability.csv", row.names = 1)


## Plot GDP vs reporting bias
c <- sapply(rownames(travel_incidence), function(x){
    cols <- !is.na(travel_incidence[x,]) & !is.na(local_incidence[x,])
    cor(t(travel_incidence[x,cols]), t(local_incidence[x,cols]), method="spearman")
})
c <- c[!is.na(c)]
df <- data.frame(c, sociodem[names(c), "GDP.PPP"], names(c))
colnames(df) <- c("Correlation", "GDP", "Name")
pdf("../plots/gdp_vs_correlation.pdf")
ggplot(df, aes(x=Correlation, y= GDP, label=Name)) + geom_point() + geom_text(aes(label=Name)) + xlab("Spearman's Rank correlation between travel and local incidence")
dev.off()

## Get distribution of local incidence
t <- unlist(local_incidence)
t <- t[!is.na(t)]
pdf("../plots/local_incidence.pdf")
hist(t)
dev.off()
## Follows a log normal dsitribution

## Get distribution of travel incidence
t <- unlist(travel_incidence)
t <- t[!is.na(t)]
pdf("../plots/travel_incidence.pdf")
hist(t)
dev.off()
## Follows a log normal dsitribution


pl <- c()
i <- 1
for(x in rownames(travel_incidence)){
    t <- travel_incidence[x,!is.na(travel_incidence[x,])]
    l <- local_incidence[x,!is.na(local_incidence[x,])]
    if(length(t) > 0 & length(l) > 0){
        l <- melt(l)
        l[,"ID"] <- "Local"
        t <- melt(t)
        t[,"ID"] <- "Travel"    
        d <- rbind(t,l)
        pl[[i]] <- ggplot(d, aes(x=variable, y=value, color=ID, group=ID)) + geom_point() + geom_line() + theme_bw() + ggtitle(x)
        i <- i + 1;
    }    
}

n <- length(pl)
nCol <- floor(sqrt(n))
pdf("../plots/travel_vs_local.pdf", h = 20, w = 20)
do.call("grid.arrange", c(pl, ncol=nCol))
dev.off()

## Regression

library(rstan)
x <- "Jamaica"

params <- list()
i <- 1
for(x in rownames(travel_incidence)){
    t <- travel_incidence[x,!is.na(travel_incidence[x,])]
    l <- local_incidence[x,!is.na(local_incidence[x,])]
    c <- intersect(colnames(t), colnames(l))
    la <- NULL
    if(length(c) > 0){
        dat <- list(N = length(c),
                    K = 1,
                    x = matrix(t[,c]),
                    y = as.vector(t(l[,c])));    
        fit <- stan(file = "regression.stan", data = dat, iter = 1000, chains = 4)
    }
    params[[i]] <- fit
    i = i + 1
}
saveRDS(params, "params.rds")

la <- lapply(params, function(x){
    extract(x, permuted=TRUE)
})
saveRDS(la, "la.rds")

params <- readRDS("params.rds")
la <- readRDS("la.rds")

pl <- list()
i <- 1
for(x in c(1:length(params))){
    if(x!=match("Cuba", rownames(travel_incidence))){
        p <- stan_hist(params[[x]], pars="beta") + ggtitle(rownames(travel_incidence)[x])
        pl[[i]] <- p
        i <- i+1
    }
}

n <- length(pl)
nCol <- floor(sqrt(n))
pdf("../plots/beta.pdf", h = 20, w = 20)
do.call("grid.arrange", c(pl, ncol=nCol))
dev.off()

pl <- list()
i <- 1
for(x in c(1:length(params))){
    if(x!=match("Cuba", rownames(travel_incidence))){
        p <- stan_hist(params[[x]], pars="alpha") + ggtitle(rownames(travel_incidence)[x])
        pl[[i]] <- p
        i <- i+1
    }
}

n <- length(pl)
nCol <- floor(sqrt(n))
pdf("../plots/alpha.pdf", h = 20, w = 20)
do.call("grid.arrange", c(pl, ncol=nCol))
dev.off()


beta <- sapply(la[-match("Cuba", rownames(travel_incidence))], function(x){
    x$beta
});

alpha <- sapply(la[-match("Cuba", rownames(travel_incidence))], function(x){
    x$alpha
});

hist(beta)
hist(alpha)


t.all <- lapply(rownames(travel_incidence), function(x){
    print(x)
    t <- travel_incidence[x,!(is.na(travel_incidence[x,]) | travel_incidence[x,] == Inf)]
    l <- local_incidence[x,!(is.na(local_incidence[x,]) | local_incidence[x,] == Inf)]
    c <- intersect(colnames(t), colnames(l))
    if(length(c) > 0){
        as.vector(t(t[,c]))
    } else {
        NULL
    }
});
t.all <- unlist(t.all)

l.all <- lapply(rownames(travel_incidence), function(x){
    print(x)
    t <- travel_incidence[x,!(is.na(travel_incidence[x,]) | travel_incidence[x,] == Inf)]
    l <- local_incidence[x,!(is.na(local_incidence[x,]) | local_incidence[x,] == Inf)]
    c <- intersect(colnames(t), colnames(l))
    if(length(c) > 0){
        as.vector(t(l[,c]))
    } else {
        NULL
    }
});
l.all <- unlist(l.all)

dat <- list(N = length(l.all),
            x = t.all,
            y = l.all);
fit <- stan(file = "regression.stan", data = dat, iter = 20000, chains = 4)
print(fit)
stan_trace(fit)
stan_plot(fit)
stan_hist(fit)

ex <- sapply(t.all, function(x){
    length(x) > 0
})

