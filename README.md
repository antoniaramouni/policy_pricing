Here you can find my project.
The company gave me two files named "claims.csv" and "contracts.csv", I will send "contracts.csv" via email because we can't load it here.
You should start with the notepad file named "Premium Calculation.R", you can find all the description needed in this file.
Note that to see all the visualizations(tables, histograms and regression models) you need to run "Premium Calculation.R" on RStudio.
Objective: Build a pricing model that you can find in the final file "Model_class_1_me.xlsx"
Hope you like it!
#How to quantify the risk and how to price a premium of a policy in a specific country
#This will depend on two variables "Frequency" & "Severity"
# "Frequency": I need to know the number of accidents per client
# "Severity": I need to know how much the company is willing to pay in case of accidents
# which means how much the coverage is per client
# the Premium will be the product of these two variables
# Note that after forecasting the premium I need to add some % of profit in the end

#First of all I will set the directory, it means I will indicate to R where to go
setwd("C:\\Users\\Ara's Family\\Desktop\\Project")

#I let R read the excel file of Contracts.csv
CONTRACTS=read.csv("Contracts.csv")
#we read now the data of our file
# ID: specific unique number for each client
# NB: number of accidents
# exposure: the probability of being exposed to an accident
#power: a character for the type of engine
#density: number of people living in each region

# Objective1: I need to find the "Frequency":nbr of acc per client
#As we can see in our excel file that we have different ages for drivers and cars
# But, when we price a premium we can't take each individual alone it will 
# take more time. That's why we need to bucket our data, which means to divide 
# our data into different intervals that will make it easier to calculate the freq
# So we create another excel file but with data "bucketed"
CONTRACTS.f=CONTRACTS
# So we cut three variables AGECAR, AGEDRIVER, DENSITY
CONTRACTS.f$AGEDRIVER=cut(CONTRACTS$AGEDRIVER,c(17,22,26,42,74,Inf))
CONTRACTS.f$AGECAR=cut(CONTRACTS$AGECAR,c(0,1,4,15,Inf),include.lowest = TRUE)
CONTRACTS.f$DENSITY=cut(CONTRACTS$DENSITY,c(0,40,200,500,4500,Inf),include.lowest = TRUE)
# note that include lowest=TRUE that the lowest value is the one indicated first
# which means we take this value as the minimum from now on

#So, as we will see below, we compute as.factor(CONTRACTS.f$GAZ) in order to have
# Diesel and Regular as "Factors" not like any random text, just 2 factors for
# Gaz Category
CONTRACTS.f$GAZ=as.factor(CONTRACTS.f$GAZ)
# Now we take a look, we can plot an histogram to see the AGECAR criteria 
hist(CONTRACTS$AGECAR)
# We notice that the majority of the cars are have the ages between 0 and 20 years
# and 50% of the cars are between 0 and 10 years

# for the age of the drives
hist(CONTRACTS$AGEDRIVER)
# We can see the majority of the drivers are adults between 30 and 45 years old
# These histograms can help us in visualizing our population to have an idea 

####### ESTIMATION OF THE FREQUENCY ######
# As we said before to estimate the frequency we need to find the number of
# exposures and the number of accidents (NB)
VY = CONTRACTS.f$NB
VE = CONTRACTS.f$EXPOSURE
# Now to see how much each client is costing the insurance company annually
# we need to multiply the number of accidents (NB) by the nbr of exposures
# (NB x Exposures) = Annual cost of each client 
# So to know per average the number of accidents in my portfolio, I need to 
# divide the sum of number of accidents by the sum of number of exposures
m=sum(VY)/sum(VE)
m
m*100
# On average we have 7 accidents per year/client #

# We should also compute the variance because for analyzing we need to find the
# average of the nbr of acc/client and to compute the variance that indicates
# to see how well the average represents the an entire set of data in order to
# determine the type of our distribution
Va=sum((VY-m*VE)^2)/sum(VE)
Va
Va*100

#we can see that the variance and the average are approx =, how we make sure?
# Phi: If the correlation Coefficient of Average and Variance is Approx 1 then,
# the average and the variance have a strong relation, and approx =
Phi=Va/m
Phi
# In each statistical study we do, our main goal is to fit a type of distribution
# to our data set,
# So, the frequency follows a poisson(lambda) distribution
# Having lambda=mean=variance with lambda as its parameter
lambda=m

# As you know, each frequency is unique for each client
# We need to see in our data what other variables are affecting the frequency
# We should mention that the GAZ category should be read as Factors not as a 
# random text, it was edited before, having two factors "Diesel" and "Regular"

# Now we should link every client with his/her criteria
# To do so, we should compute a regression model, but since we already knew that
# we are computing a poisson distribution then we can't use linear model 
# following the equation y=ax+b, But we should assign that it is a general 
# linear model that allows for response variables that have error distribution 
# models other than a normal distribution
reg_freq=glm(NB~GAZ+AGEDRIVER+AGECAR+DENSITY+offset(log(EXPOSURE)),family=poisson,data=CONTRACTS.f)
# I need to compute the NB to see how much the other criteria GAZ, AGEDRIVER,
# AGECAR, DENSITY... BUT Exposure won't change with time
summary(reg_freq)

##Data Cleaning
# We noticed that the AGECAR criteria is not significant in all the intervals so
# we decided to remove it from the glm function in order to have a more efficient
# summary in other words using the glm function to link the frequency of accidents
# with every criteria, so we take the NB of acc and see how much the other variables
# are affecting it. AGECAR is not significant which means its p value > 0.05
# it doesn't affect the NB of accidents
# after removing AGECAR the glm function became
reg_freq=glm(NB~GAZ+AGEDRIVER+DENSITY+offset(log(EXPOSURE)),family=poisson,data=CONTRACTS.f)
summary(reg_freq)
#Finally, we export this summary to excel to use it when calculating the premium

write.csv(summary(reg_freq)$coef,"Frequency_reg.csv")


##### SEVERITY ####
# we should do the same procedure that we already did for the Frequency but now
# we should look on how much the accident costs per client, that's why
# we look in the "Claims.csv" sheets in order to calculate the severity not in 
# the contracts because we have different criteria for severity

CLAIMS=read.csv("Claims.csv")
# We only have two columns here ID and Loss, as we saw earlier in the Contracts
# excel we have also the key "ID" that's why both of them are related
# Note that each ID, is for a specific client. In the "Claims" sheet we notice
# that it indicates the loss that every client caused to the insurance company
# but note that every time this client represented by an ID number cause a new 
# loss to the company it will be inserted and added to the table as a new loss
# even if the ID number existed before in the sheet with the same ID number of 
# this client


# Important note: since we have the same client "ID" in the two sheets "Claims"
# and "Contracts" we can by using R, merge the two databases into one in order
# to easily read the description indicated one time in one table
CLAIMS.f=merge(CLAIMS,CONTRACTS.f)

#Like we remarked, for the frequency it was more practical to follow a poisson 
# distribution in order to obtain an efficient summary for the frequency variable
# for the severity, we use gamma distribution in order to obtain a positive
# distribution because when pricing the premium we should get a positive number
# We compute the regression also in a generalized linear model since it is not
# normal, to see which criteria is affecting the loss
reg_sev=glm(Loss~GAZ+AGECAR+AGEDRIVER+DENSITY+POWER+BRAND+REGION, family=Gamma(link="log"),data=CLAIMS.f)
summary(reg_sev)

#So we remark that the majority of the variables aren't significant, which means
# that not all of them cause the losses or that they are some outlier
# In order to identify if there is some outlier, we should visualize the losses
# because an outlier in the losses could make a false assumption for our estimations

#So first we should plot these claims visualizing the losses
plot(CLAIMS$Loss)
mean(CLAIMS$Loss)
max(CLAIMS$Loss)

#When plotting, we remark that we have a very far outlier that will affect my
# regression that's why we have very few significant variables, so to have a better
# and efficient estimation we need to remove this outlier and recompute the
# regression.
#Also, we can remark that when computing the mean and the maximum value, we notice
# a huge difference between these two giving a max value of 2 Million approx
# and a mean of 2 thousand approx. So, now we are sure that we have an outlier
# that is debugging our regression estimation

#Plus, we notice when plotting that more than 98% of the losses are below 500 000
# to get a more efficient analysis we will plot the losses< 500 000
plot(CLAIMS$Loss<500000)


#In the insurance industry, we use a clustering theory which means we make a
# policy that suits in general the majority of the people in this case like the
#majority of them own cars that cost between 15 000 - 20 000 so it covers more
#than 65% of the people and the rest that have expensive cars have their own 
# formula
reg_sev_t=glm(Loss~GAZ+AGECAR+AGEDRIVER+DENSITY+POWER+BRAND+REGION,family=Gamma(link="log"),data=CLAIMS.f[CLAIMS$Loss<15000,])
summary(reg_sev_t)
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
