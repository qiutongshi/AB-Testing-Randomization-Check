#Qiutong Shi HW3

setwd("~/Desktop/luan/Customer Analytics/Week 4 randomizied controlled experiment/HW3")
rm(list=ls())
#read into a data frame, working directory set manually
ds <- read.csv('rocketfuel_data.csv')

#check data variables
attach(ds)
summary(ds)
#drop rows with missing values
ds <- na.omit(ds)

#share of users allocated to the control group 
library(dplyr)
ds %>% 
  group_by(test) %>% 
  summarise(percent = 100 * n() / nrow(ds))
table(ds$test)

#randomization test
options(digits=15)#avoid R's automatic rounding by displaying more digits
ave<-split(ds$tot_impr, ds$test)
t.test(ave[[1]],ave[[2]], alternative = "two.sided")

#an alternative way to find means of the two groups using sapply that returns named nums
#aves <- sapply(split(ds$tot_impr, ds$test), mean)#find average of ad impressions by binary of test store into a named vector
#mean.control = aves[['0']]
#mean.test = aves[['1']]

#compare the conversion rates between test and control groups 
#run (and report) the proper statistical test results.
options(digits=15)#avoid R's automatic rounding by displaying more digits
con<-split(ds$converted, ds$test)
#one-sided test with null hypothesis that conversion of test is larger than control
t.test(con[[2]], con[[1]],alternative = "less")

#How much more money did TaskBella make by running the ad campaign?
#split ds into exposed vs. non-exposed
min(ds$tot_impr)
exp<-split(ds$converted, ds$tot_impr==1)
#conversion rates
con_exp = mean(exp[[1]])
con_nexp = mean(exp[[2]])
incremental_con = con_exp-con_nexp
#count of converted given imp>1
con_count_exp = length(exp[[1]])
incremental_revenue = con_count_exp * incremental_con  * 40
round(incremental_revenue,digits=2)

#cost of campaign
#average CPM = $9
cost = (14597182/1000)*9
round(cost,digits=2)
#ROI
ROI = (incremental_revenue - cost)/cost * 100
round(ROI, digits=2)

# library
library(ggplot2)
#create and assign bins, groupname, 
ds$myBins <- ifelse(ds$tot_impr>=0&ds$tot_impr<=9,'0-9',
             ifelse(ds$tot_impr>=10&ds$tot_impr<=19,'10-19',
             ifelse(ds$tot_impr>=20&ds$tot_impr<=29,'20-29',
             ifelse(ds$tot_impr>=30&ds$tot_impr<=39,'30-39',
             ifelse(ds$tot_impr>=40&ds$tot_impr<=49,'40-49',
             ifelse(ds$tot_impr>=50&ds$tot_impr<=59,'50-59',
             ifelse(ds$tot_impr>=60&ds$tot_impr<=69,'60-69',
             ifelse(ds$tot_impr>=70&ds$tot_impr<=79,'70-79',
             ifelse(ds$tot_impr>=80&ds$tot_impr<=89,'80-89',
             ifelse(ds$tot_impr>=90&ds$tot_impr<=99,'90-99',
             ifelse(ds$tot_impr>=100&ds$tot_impr<=109,'100-109',
             ifelse(ds$tot_impr>=110&ds$tot_impr<=119,'110-119',
             ifelse(ds$tot_impr>=120&ds$tot_impr<=129,'120-129',
             ifelse(ds$tot_impr>=130&ds$tot_impr<=139,'130-139',
             ifelse(ds$tot_impr>=140&ds$tot_impr<=149,'140-149',
             ifelse(ds$tot_impr>=150&ds$tot_impr<=159,'150-159',
             ifelse(ds$tot_impr>=160&ds$tot_impr<=169,'160-169',
             ifelse(ds$tot_impr>=170&ds$tot_impr<=179,'170-179',
             ifelse(ds$tot_impr>=180&ds$tot_impr<=189,'180-189',
             ifelse(ds$tot_impr>=190&ds$tot_impr<=199,'190-199',
             ifelse(ds$tot_impr>=200,'200+',0)))))))))))))))))))))
ds$groupname<-ifelse(ds$test==0,'Control','Test')
#assign conversion rate based on group of different bin 
bin_con<-split(ds$converted, list(ds$myBins,ds$test))
ds$conversion_rate <- ifelse(ds$test==0&ds$myBins=='0-9',mean(bin_con[["0-9.0"]]),
             ifelse(ds$test==0&ds$myBins=='10-19',mean(bin_con[['10-19.0']]),
             ifelse(ds$test==0&ds$myBins=='20-29',mean(bin_con[['20-29.0']]),
             ifelse(ds$test==0&ds$myBins=='30-39',mean(bin_con[['30-39.0']]),
             ifelse(ds$test==0&ds$myBins=='40-49',mean(bin_con[['40-49.0']]),
             ifelse(ds$test==0&ds$myBins=='50-59',mean(bin_con[['50-59.0']]),
             ifelse(ds$test==0&ds$myBins=='60-69',mean(bin_con[['60-69.0']]),
             ifelse(ds$test==0&ds$myBins=='70-79',mean(bin_con[['70-79.0']]),
             ifelse(ds$test==0&ds$myBins=='80-89',mean(bin_con[['80-89.0']]),
             ifelse(ds$test==0&ds$myBins=='90-99',mean(bin_con[['90-99.0']]),
             ifelse(ds$test==0&ds$myBins=='100-109',mean(bin_con[['100-109.0']]),
             ifelse(ds$test==0&ds$myBins=='110-119',mean(bin_con[['110-119.0']]),
             ifelse(ds$test==0&ds$myBins=='120-129',mean(bin_con[['120-129.0']]),
             ifelse(ds$test==0&ds$myBins=='130-139',mean(bin_con[['130-139.0']]),
             ifelse(ds$test==0&ds$myBins=='140-149',mean(bin_con[['140-149.0']]),
             ifelse(ds$test==0&ds$myBins=='150-159',mean(bin_con[['150-159.0']]),
             ifelse(ds$test==0&ds$myBins=='160-169',mean(bin_con[['160-169.0']]),
             ifelse(ds$test==0&ds$myBins=='170-179',mean(bin_con[['170-179.0']]),
             ifelse(ds$test==0&ds$myBins=='180-189',mean(bin_con[['180-189.0']]),
             ifelse(ds$test==0&ds$myBins=='190-199',mean(bin_con[['190-199.0']]),
             ifelse(ds$test==0&ds$myBins=='200+',mean(bin_con[['200+.0']]),
             ifelse(ds$test==1&ds$myBins=='0-9',mean(bin_con[["0-9.1"]]),
             ifelse(ds$test==1&ds$myBins=='10-19',mean(bin_con[['10-19.1']]),
             ifelse(ds$test==1&ds$myBins=='20-29',mean(bin_con[['20-29.1']]),
             ifelse(ds$test==1&ds$myBins=='30-39',mean(bin_con[['30-39.1']]),
             ifelse(ds$test==1&ds$myBins=='40-49',mean(bin_con[['40-49.1']]),
             ifelse(ds$test==1&ds$myBins=='50-59',mean(bin_con[['50-59.1']]),
             ifelse(ds$test==1&ds$myBins=='60-69',mean(bin_con[['60-69.1']]),
             ifelse(ds$test==1&ds$myBins=='70-79',mean(bin_con[['70-79.1']]),
             ifelse(ds$test==1&ds$myBins=='80-89',mean(bin_con[['80-89.1']]),
             ifelse(ds$test==1&ds$myBins=='90-99',mean(bin_con[['90-99.1']]),
             ifelse(ds$test==1&ds$myBins=='100-109',mean(bin_con[['100-109.1']]),
             ifelse(ds$test==1&ds$myBins=='110-119',mean(bin_con[['110-119.1']]),
             ifelse(ds$test==1&ds$myBins=='120-129',mean(bin_con[['120-129.1']]),
             ifelse(ds$test==1&ds$myBins=='130-139',mean(bin_con[['130-139.1']]),
             ifelse(ds$test==1&ds$myBins=='140-149',mean(bin_con[['140-149.1']]),
             ifelse(ds$test==1&ds$myBins=='150-159',mean(bin_con[['150-159.1']]),
             ifelse(ds$test==1&ds$myBins=='160-169',mean(bin_con[['160-169.1']]),
             ifelse(ds$test==1&ds$myBins=='170-179',mean(bin_con[['170-179.1']]),
             ifelse(ds$test==1&ds$myBins=='180-189',mean(bin_con[['180-189.1']]),
             ifelse(ds$test==1&ds$myBins=='190-199',mean(bin_con[['190-199.1']]),
             ifelse(ds$test==1&ds$myBins=='200+',mean(bin_con[['200+.1']]),0
                    ))))))))))))))))))))))))))))))))))))))))))


#conversion rates as a function of the number of ads displayed to consumers. 
ggplot(ds, aes(fill=groupname, y=conversion_rate, x=myBins)) + 
  geom_bar(position="dodge", stat="identity")


