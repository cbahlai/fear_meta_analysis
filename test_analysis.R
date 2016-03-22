# Test meta analysis of subset of data to demonstrate usage of the metafor package
# using subset of data. 
# analysis based on tuturial located at 
# http://rstudio-pubs-static.s3.amazonaws.com/10913_5858762ec84b458d89b0f4a4e6dd5e81.html


# Prior to importation, data was manipulated in excel to remove
# embedded metadata and to remove characters R will interpret as operators(+/-) and 
# replace them with text strings (sign: Neg Pos)
# metadata is as follows:
#
#Nc	control sample size
#Ne	experimental sample size
#Xc	control mean
#Xe	experimental mean
#Sc	control SD
#Se	experimental SD

#quality control- should be 43 observations

#import data
data1<- read.csv(file="Gur_Hedge.csv", header=TRUE, na.strings="")

#load metafor package
library(metafor)
#call the citation so it's handy in the output
citation("metafor")

#also call plyr, we'll need it later
library(plyr)
#call the citation so it's handy in the output
citation("plyr")

#compute effect size
#use Hedge's g to account for small sample bias
#Hedge's g is the stadardized mean difference, corrected for negative bias and is encoded as SMD in metafor

data1<-escalc(n1i=Nc,n2i=Ne,
            m1i=Xc, m2i=Xe,
            sd1i=Sc,sd2i=Se,
            data=data1,  measure="SMD", append=TRUE)

#this analysis appends yi, the effect size, and vi, the variance of the effect size. Let's plot this so we can
#visualize it

#first create the meta analysis model

ma_model_1<-rma(yi, vi, data=data1)
summary(ma_model_1)
#note output is based on zval, not t value by default as this is the standard, so this may cause slight 
#deviations from the Metawin output

#compute failsafe number
fsn(yi, vi,  data=data1, type="Rosenthal", alpha=.05)

#create forest plot
forest(ma_model_1, slab=paste(data1$Source, data1$Genus, data1$species))

#quick test for publication bias- look for asymetrical distribturion
funnel(ma_model_1)

#does effect size vary by habitat?
boxplot(yi ~ Habitat, data = data1)
#summary data
ddply(data1, c("Habitat"), summarise,
      mean=mean(yi), sd=mean(vi))

#does effect size vary by type of interaction?
boxplot(yi ~ Sign, data = data1)
#summary data
ddply(data1, c("Sign"), summarise,
      mean=mean(yi), sd=mean(vi))

#rerun model with these factors accounted for
ma_model_2<-rma(yi, vi, mods=~Habitat+Sign, data=data1, method="ML")
summary(ma_model_2)


forest(ma_model_2, slab=paste(data1$Source, data1$Genus, data1$species))
