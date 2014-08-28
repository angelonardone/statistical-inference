1  140	132
2	138	135
3	150	151
4	148	146
5	135	130

data <- data.frame(baseline=c(140,138,150,148,135),later=c(132,135,151,146,130))
#data$diff <- data$later-data$baseline
means <- apply(data,2,mean)
var1 <- apply(data,2,var)

t.test(x=data$later,y=data$baseline,var.equal=T,paired=T)


#2
1100 + c(-1,1)*qt(0.975,8)*30/sqrt(9)


#3
#Researchers conducted a blind taste test of Coke versus Pepsi. 
#Each of four people was asked which of two blinded drinks given in random order that they preferred. 
#The data was such that 3 of the 4 people chose Coke. Assuming that this sample is representative, 
#report a P-value for a test of the hypothesis that Coke is preferred to Pepsi using a one sided exact test.

binom.test(3,4,p=0.5,alternative="g")


#4
#Infection rates at a hospital above 1 infection per 100 person days at risk are believed to be too high 
#and are used as a benchmark. A hospital that had previously been above the benchmark recently had 10 
#infections over the last 1,787 person days at risk. 
#About what is the one sided P-value for the relevant test of whether the hospital is *below* the standard?

poisson.test(10,T=1787,r=1/100,alternative="l")

#5
# Suppose that 18 obese subjects were randomized, 9 each, to a new diet pill and a placebo. 
# Subjects’ body mass indices (BMIs) were measured at a baseline and again after having received 
# the treatment or placebo for four weeks. 
# The average difference from follow-up to the baseline (followup - baseline) was −3 kg/m2 for the treated group 
# and 1 kg/m2 for the placebo group. 
# The corresponding standard deviations of the differences was 1.5 kg/m2 for the treatment group 
# and 1.8 kg/m2 for the placebo group. 
# Does the change in BMI over the two year period appear to differ between the treated and placebo groups? 
# Assuming normality of the underlying data and a common population variance, give a pvalue for a two sided t test.

pooled.var = (1.5^2*8 + 1.8^2*8)/(9+9-2)
test_stat <- -3-1/sqrt(pooled.var/9)
pt(test_stat,8)
pt(test_stat,8) < .01


#7
# Researchers would like to conduct a study of 100 healthy adults to detect 
# a four year mean brain volume loss of .01 mm3. 
# Assume that the standard deviation of four year volume loss in this population is .04 mm3. 
# About what would be the power of the study for a 5% one sided test versus a null hypothesis of no volume loss?

power.t.test(n=100,delta=.01,sd=0.04,type="one.sample",alternative = "one.sided")


#8
# Researchers would like to conduct a study of n healthy adults 
# to detect a four year mean brain volume loss of .01 mm3. 
# Assume that the standard deviation of four year volume loss in this population is .04 mm3. 
# About what would be the value of n needded for 90% 
# power of type one error rate of 5% one sided test versus a null hypothesis of no volume loss?

power.t.test(power=.9,delta=.01,sd=0.04,type="one.sample",alternative = "one.sided")

#10
# The Daily Planet ran a recent story about Kryptonite poisoning in the water supply 
# after a recent event in Metropolis. Their usual field reporter, Clark Kent, 
# called in sick and so Lois Lane reported the story. 
# Researchers sampled 288 individuals and found mean blood Kryptonite levels of 44, 
# both measured in Lex Luthors per milliliter (LL/ml). 
# They compared this to 288 sampled individuals from Gotham city who had an average level of 42.04. 
# About what is the Pvalue for a two sided Z test of the relevant hypothesis? 
#Assume that the standard deviation is 12 for both groups.

test_stat <- (44-42.04)/((12+12)/sqrt(288+288))
pnorm(44,mean=42.04,sd=sqrt(((12+12)/sqrt(288+288))),lower.tail=F)
pnorm(test_stat,lower.tail = F)

zeta = (44 - 42.04) / (sqrt(144/288 + 144/288))
