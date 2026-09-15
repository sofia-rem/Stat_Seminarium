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

# Read files from Task1
task1data <- read.csv("T1_data.csv", header = TRUE, sep = ",")
head(task1data)
Hnfl_subset <- task1data %>%
filter(Group == "Healthy")
Dnfl_subset <- task1data %>%
filter(Group == "Disease")

DGeomean <- geoMean(Dnfl_subset$NfL, na.rm=TRUE)
DesiredGeomean <- 0.8 * DGeomean
DGeoSD <- geoSD(Dnfl_subset$NfL)
DGeomean_log <- log10(DGeomean)
DesGeomean_log <- log10(DesiredGeomean)
expSD_log <- log10(DGeoSD)

es_d_t <- (DesGeomean_log - DGeomean_log) / expSD_log
p.t.two_t <- pwr.t.test(d=es_d_t, power=0.8, type="two.sample", alternative="two.sided")
#plot(p.t.two_t, xlab = "Sample size from t-test")
N <- ceiling(p.t.two_t$n)

# Get values for Test 2
task2data <- hpd_trial_data(N)
treat_subset <- task2data %>%
filter(Group == 1)
contr_subset <- task2data %>%
filter(Group == 0)

# Geometric mean
treat_Geomean <- geoMean(treat_subset$NfL)
contr_Geomean <- geoMean(contr_subset$NfL)
the_ratio <- treat_Geomean / contr_Geomean
print(the_ratio)
# ca. 0.73-0.78 -> < 0.8 -> reduces w. 20% or more

# P-value
treat_log <- log10(treat_subset$NfL)
contr_log <- log10(contr_subset$NfL)
the_stats <- t.test(treat_log, contr_log, alternative="two.sided", paired=FALSE, var.equal=TRUE, conf.level=0.95)
print(the_stats)
# p-value of 9.698308e-10 ~ 0 < 0.05 -> reject H0 -> is significant
# Confidence Interval : LCL = -0.18023369   UCL = -0.09431898

# Standard deviation + cohen's d
treat_GeoSD <- geoSD(treat_subset$NfL)
contr_GeoSD <- geoSD(contr_subset$NfL)
print(treat_GeoSD)
print(contr_GeoSD)
# 1.549 vs 1.614     1.615 vs 1.639
es_d <- (log10(treat_Geomean) - log10(contr_Geomean)) / (sqrt((log10(treat_GeoSD)^2 + log10(contr_GeoSD)^2)/2))


# Histogram
treathist <- hist(treat_subset$NfL)
contrhist <- hist(contr_subset$NfL)
#plot(treathist, col=rgb(0,0,1,1/4),main="blue = treatment red = control")
#plot(contrhist,col=rgb(1,0,0,1/4),add=T)

# QQ-plot
#qqPlot(log10(contr_subset$NfL),xlab="qqplot for log10 of control group")

# Test if equal variances (after test is done)
datalist <- data.frame(dftreat = (dftreat <- task2data %>%
filter(Group==1))$NfL, dfcontr = (dfcontr <- task2data %>% 
filter(Group==0))$NfL, dftreatlog = log10((dftreat <- task2data %>%
filter(Group==1))$NfL), dfcontrlog = log10((dfcontr <- task2data %>% 
filter(Group==0))$NfL))

datalist2 <- data.frame(dftreatlog = log10((dftreat <- task2data %>%
filter(Group==1))$NfL), dfcontrlog = log10((dfcontr <- task2data %>% 
filter(Group==0))$NfL))

#print(datalist2)

print(task2data$Group)

# normalized vec
variancetest <- data.frame(dfGroup = task2data$Group, dfNfL = log10(task2data$NfL))
#leveneTest(dfNfL ~ dfGroup, data = variancetest)

boxplot(dfNfL ~ dfGroup, data = variancetest, main = "Distribution of NfL by group")

# logarithmized t test
