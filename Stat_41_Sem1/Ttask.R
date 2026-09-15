
#install.packages("dplyr")
#library(dplyr)
#install.packages("EnvStats")
#library(EnvStats)

# for calculating G*Power 
#install.packages("pwr")
#library(pwr)

# for calculating power w. Welch t test
#install.packages("MKpower")
#library(MKpower)


task1data <- read.csv("T1_data.csv", header = TRUE, sep = ",")
head(task1data)

Hnfl_subset <- task1data %>%
filter(Group == "Healthy")

Dnfl_subset <- task1data %>%
filter(Group == "Disease")

#hist(Hnfl_subset$NfL,main="Healthy")
#hist(Dnfl_subset$NfL,main="Disease")


#HGeomean = geoMean(Hnfl_subset$NfL, na.rm = TRUE)
#DGeomean = geoMean(Dnfl_subset$NfL, na.rm = TRUE)
#print(HGeomean)
#print(DGeomean)
#HGeoSD = geoSD(Hnfl_subset$NfL)
#DGeoSD = geoSD(Dnfl_subset$NfL)
#print(HGeoSD)
#print(DGeoSD)


# Calculate desired N w. 2 sample unpaired t-test
DGeomean <- geoMean(Dnfl_subset$NfL, na.rm=TRUE)
DesiredGeomean <- 0.8 * DGeomean
DGeoSD = geoSD(Dnfl_subset$NfL) # assume similar standard deviation for both groups

DGeomean_log <- log10(DGeomean)
DesGeomean_log <- log10(DesiredGeomean)
expSD_log <- log10(DGeoSD)

#sd_pooled_t <- sqrt((expSD_log^2 + expSD_log^2)/2)
es_d_t <- (DesGeomean_log - DGeomean_log) / expSD_log
p.t.two_t <- pwr.t.test(d=es_d_t, power=0.8, type="two.sample", alternative="two.sided")
#plot(p.t.two_t, xlab = "Sample size from t-test")

# Calculate desired N w. welch
difference_in_mean = DesGeomean_log - DGeomean_log
p.t.two_welch <- power.welch.t.test(delta=difference_in_mean, sd1=expSD_log, sd2=expSD_log, power=0.8, alternative="two.sided")
#plot(p.t.two_welch)

# Show
x_contr <- 73
sd_contr <- 9
nr_samples <- 1000
bpm_contr <- rnorm(nr_samples, x_contr, sd_contr) # control vector (untreated)

x_treat <- x_contr + 10 #would treat if mean is this or more extreme
sd_treat <- 9 #assume? #assume that same standard difference

sd_pooled <- sqrt((sd_treat^2 + sd_contr^2)/2)
es_d <- (x_treat - x_contr) / sd_pooled
p.t.two <- pwr.t.test(d=es_d, power=0.8, type = "two.sample", alternative = "two.sided")
#plot(p.t.two, xlab = "sample size per group")

# från filen
sd_pooledX <- sqrt((0.447^2+0.447^2)/2)
es_dX <- (2.3 - 2)/0.477
p.t.twoX <- pwr.t.test(d=es_dX, power = 0.8, type="two.sample",alternative = "two.sided")
#plot(p.t.twoX,xlab="filen")

# från filen testar w. fig 19
geomean_cntr <- 103*10^(-9)
geomean_treat <- 302*10^(-9)
