setwd("C:\\Users\\Aramouni\\Desktop\\Antoni\\Usek\\Spring 2020-2021\\Mac400\\Chap7")
CONTRACTS=read.csv("Contracts_a.csv")
CONTRACTS.f=CONTRACTS
CONTRACTS.f$AGEDRIVER=cut(CONTRACTS$AGEDRIVER,c(17,22,26,42,74,Inf))
CONTRACTS.f$AGECAR=cut(CONTRACTS$AGECAR,c(0,1,4,15,Inf),include.lowest = TRUE)
CONTRACTS.f$DENSITY=cut(CONTRACTS$DENSITY,c(0,40,200,500,4500,Inf),include.lowest = TRUE)
#So as mentioned below, We compute as.factor(CONTRACTS.f$GAZ) in order to have Diesel and Regular 
#as Factors not like any random text, just 2 factors for Gaz Category
CONTRACTS.f$GAZ=as.factor(CONTRACTS.f$GAZ)
hist(CONTRACTS$AGECAR)
hist(CONTRACTS$AGEDRIVER)
########### ESTIMATION OF THE FREQUENCY ############
VY=CONTRACTS.f$NB
VE=CONTRACTS.f$EXPOSURE
m=sum(VY)/sum(VE)
m

## On average we have 7 accidents per year ##
### Now we compute the variance because for analyzing we need to find the mean ##
### that we found previously and now we compute the variance ###
Va=sum((VY-m*VE)^2)/sum(VE)
Va
Phi = Va/m
Phi
## If Phi: correlation Coefficient of Mean and Variance is Approx 1 Then it is
# a strong relation, so the frequency follows a poisson distribution
# Having lambda = mean = variance with lambda the number of events
lambda=m
#Note that we should mention that the Gaz category should be read as Factor not as
# a random text. So we go back and mention that in the Gaz Column that we have
#two factors Regular and Diesel

# Now, we should link every client with his/her criteria
#To do so, we should compute a regression model, but since we already knew that 
# We are computing a poisson distribution then we cannot use linear model following the equation y=ax+b
# But we should assign that it is a general linear model  that allows for response variables 
#that have error distribution models other than a normal distribution
reg_freq=glm(NB~GAZ+AGEDRIVER+AGECAR+DENSITY+offset(log(EXPOSURE)),family=poisson,data=CONTRACTS.f)
summary(reg_freq)
#We notice that the AGECAR is not significant in all the intervals so we decide
#to remove it from the glm function in order to have a more efficient summary
# in other words using the glm is to link the frequency of accidents with every criteria
# So we take the NB of accidents and look at this variable in taking into consideration
#the criteria like AGEDRIVER, DENSITY,GAZ... We remark that AGECAR is not significant
# which mean its p value > 0.05 it doesn't affect the NB of accidents
#so we can remove it from the glm function
reg_freq=glm(NB~GAZ+AGEDRIVER+DENSITY+offset(log(EXPOSURE)),family=poisson,data=CONTRACTS.f)
summary(reg_freq)
# Now I want to export this summary on excel 
write.csv(summary(reg_freq)$coef,"Frequency_reg.csv")

#### Severity ####
## We should do the same procedure that we already did for the frequency but now 
### We should look on how much the accident costs per client so
## We should look in the "Claims" Excel sheet in order to calculate the severity
## Not in the contracts because we have different criterias for severity

CLAIMS=read.csv("Claims.csv")
#### Note that: Each Id, is for a specific client. I n the "Claims" sheet we notice
# that it indicates the loss that every client caused to the insurance company 
# but note that every time this "Client" represented by an Id number cause a new loss
# to the company it will be inserted in the table as a new loss even if the Id 
# number existed before in the sheet, we will have a new cell having a new loss
# but with the same Id number inserted before.

# Important: Since we have the same client(ID) in the two sheets "Claims" and "Contracts"
# We can by using R, merge the two databases into one in order to easily read the
# description indicated one time in one table

CLAIMS.f=merge(CLAIMS,CONTRACTS.f)

# Like we remarked, that for the frequency it is more practical to follow
# a poisson distribution in order to obtain an efficient summary for the frequency variable
# Usually Every severity distribution follows a "gamma" or "binomial" distribution
# for the severity, we use gamma distribution in order to obtain a positive distribution
# because when pricing we should get a positive number.
#Now we compute the regression, but because it isn't a normal one so we have to
# to use a generalized linear model also.
reg_sev=glm(Loss~GAZ+AGECAR+AGEDRIVER+DENSITY+POWER+BRAND+REGION,family=Gamma(link="log"),data=CLAIMS.f)
summary(reg_sev)
# So we remark that not all the variables are significant, which means that
# not all the variables cause the losses, because their P value > 0.05 so we remove them
# We should visualize the losses, because if we were computing the regression
# let's assume a linear regression we should draw a line that merge all the dots

# but if we have an outlier in the losses it could make a false assumption for the line
# So first, we should plot these claims visualizing the losses
plot(CLAIMS$Loss)
mean(CLAIMS$Loss)
max(CLAIMS$Loss)
# When plotting, we remark that we have a very far outlier that will affect my regression
# that's why we have very few significant variables, so to have a better and efficient estimation
# We need to remove this outlier and then compute the regression
# Also, we remark that when computing the mean and the maximum value, we notice
# a huge difference between these two giving a maximum value of 2 million approx
# and a mean of 2 thousand so, here we are sure we have an outlier that is debugging our regression estimation


# Then, we notice in the previous plot that all other claims are below 
# 500 000 so we try to plot the losses < 500 000

plot(CLAIMS$Loss<500000)

# in the insurance company, we use a clustering theory which means we make a policy that
## suits in general the majority of people in this case like the majority of people
## have cars that costs 15 000 -20 000$ so it covers more than 65% of people
## and the rest that have more expensive car we deal with each case individually
reg_sev_t=glm(Loss~GAZ+AGECAR+AGEDRIVER+DENSITY+POWER+BRAND+REGION,family=Gamma(link="log"),data=CLAIMS.f[CLAIMS$Loss<15000,])
summary(reg_sev_t)
## Note that we should take the categories like we did for the contracts so
## Sure containing the contracts and claims in categories so we delete the previous one
# And we merge the claims with contracts.f
## Since when visualizing the summary we found that AGECAR and GAZ regular is not significant 
## we remove it from the reg
reg_sev_t2=glm(Loss~AGEDRIVER+DENSITY+POWER+BRAND+REGION,family=Gamma(link="log"),data=CLAIMS.f[CLAIMS$Loss<15000,])
summary(reg_sev_t2)
## Then we notice that the Density is not significant so we remove it also
reg_sev_t3=glm(Loss~AGEDRIVER+POWER+BRAND+REGION,family=Gamma(link="log"),data=CLAIMS.f[CLAIMS$Loss<15000,])
summary(reg_sev_t3)
### But When it comes to the POWER and BRAND AND REGION, we have some of them significant values 
# and other no significant but here we leave it in the reg
write.csv(summary(reg_sev_t3)$coef,"Severity_regression.csv")
## We go to copy it in the 2nd sheet after the Frequency on excel "Model_me"
write.csv(CLAIMS.f,"Data_Bucketed.csv")
