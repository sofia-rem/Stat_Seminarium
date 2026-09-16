
#install.packages("dplyr")
#library(dplyr)
#install.packages("EnvStats")
#library(EnvStats)

# for calculating G*Power 
#install.packages("pwr")
#library(pwr)

#install.packages("devtools")
#library(devtools)
#install.packages("remotes")
#library(remotes)
#install_github("adamdarwichkth/CM2018rpackage")
#library(CM2018rpackage)

#install.packages("car")
#library(car)

# Seed

# Read files from Task1
task1data <- read.csv("T1_data.csv", header = TRUE, sep = ",")
head(task1data)
Hnfl_subset <- task1data %>%
filter(Group == "Healthy")
Dnfl_subset <- task1data %>%
filter(Group == "Disease")

# Histogram of task 1
#Hhist <- hist(Hnfl_subset$NfL)
#Dhist <- hist(Dnfl_subset$NfL)
#plot(Hhist, col=rgb(0,0,1,1/4),main="blue = Healthy \n red = Disease",xlim=c(0,160), xlab = "NfL ρg/mL")
#plot(Dhist,col=rgb(1,0,0,1/4),add=T)


DGeomean <- geoMean(Dnfl_subset$NfL, na.rm=TRUE)
DesiredGeomean <- 0.8 * DGeomean
DGeoSD <- geoSD(Dnfl_subset$NfL)
DGeomean_log <- log10(DGeomean)
DesGeomean_log <- log10(DesiredGeomean)
expSD_log <- log10(DGeoSD)

es_d_t <- (DesGeomean_log - DGeomean_log) / expSD_log
p.t.two_t <- pwr.t.test(d=es_d_t, power=0.8, type="two.sample", alternative="two.sided")
#plot(p.t.two_t, xlab = "Sample size for t-test")
N <- ceiling(p.t.two_t$n)

# Get values for Test 2
task2data <- hpd_trial_data(N2)
treat_subset <- task2data %>%
filter(Group == 1)
contr_subset <- task2data %>%
filter(Group == 0)

# Geometric mean
treat_Geomean <- geoMean(treat_subset$NfL)
contr_Geomean <- geoMean(contr_subset$NfL)
the_ratio <- treat_Geomean / contr_Geomean
print(the_ratio)

# P-value
treat_log <- log10(treat_subset$NfL)
contr_log <- log10(contr_subset$NfL)
the_stats <- t.test(treat_log, contr_log, alternative="two.sided", paired=FALSE, var.equal=TRUE, conf.level=0.95)
print(the_stats)

# Standard deviation + cohen's d
treat_GeoSD <- geoSD(treat_subset$NfL)
contr_GeoSD <- geoSD(contr_subset$NfL)
print(treat_GeoSD)
print(contr_GeoSD)
es_d <- (log10(treat_Geomean) - log10(contr_Geomean)) / (sqrt((log10(treat_GeoSD)^2 + log10(contr_GeoSD)^2)/2))

# Calculate minimum actual sample size
p.t.two_tt <- pwr.t.test(d=es_d, power=0.8, type="two.sample", alternative="two.sided")
#plot(p.t.two_tt, xlab = "Sample size from t-test after the fact")
N2 <- ceiling(p.t.two_tt$n)

# Histogram
treathist <- hist(treat_subset$NfL)
contrhist <- hist(contr_subset$NfL)
plot(treathist, col=rgb(0,0,1,1/4),main="blue = treatment \n red = control", xlab = "NfL ρg/mL")
plot(contrhist,col=rgb(1,0,0,1/4),add=T)

# QQ-plot
#qqPlot(log10(treat_subset$NfL),xlab="qqplot for log10 of treatment group")

# Test if equal variances (after test is done)
variancetest <- data.frame(dfGroup = task2data$Group, dfNfL = log10(task2data$NfL))
#boxplot(dfNfL ~ dfGroup, data = variancetest, main = "Distribution of NfL by group")


#datalist <- data.frame(dftreat = (dftreat <- task2data %>%
#filter(Group==1))$NfL, dfcontr = (dfcontr <- task2data %>% 
#filter(Group==0))$NfL, dftreatlog = log10((dftreat <- task2data %>%
#filter(Group==1))$NfL), dfcontrlog = log10((dfcontr <- task2data %>% 
#filter(Group==0))$NfL))

#datalist2 <- data.frame(dftreatlog = log10((dftreat <- task2data %>%
#filter(Group==1))$NfL), dfcontrlog = log10((dfcontr <- task2data %>% 
#filter(Group==0))$NfL))


print(the_stats)