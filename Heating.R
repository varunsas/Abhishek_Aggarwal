# Case Study Solutions : Heating.csv file
#---------------------------------------------------------------------------------------------------------------
# Soln. to Question 1:
# Reading the CSV file and dropping the first column
data=read.csv('C:\\Users\\VISHAL\\Documents\\Case Studies_10K\\Datasets-20200307T133324Z-001\\Datasets\\heating.csv')
# View the data loaded
data
# Dropping the first column which is nothing but the Serial number
data=data[2:22]
# View the dimensions (shape) of the data to be used for the analysis
dim(data)
# There are 900 rows and 21 columns

# Heating is a "horizontal" data.frame with three choice-specific
# variables (ic: investment cost, oc: operating cost, pb : ratio of oc/ic ) and some
# individual-specific variables (income, region, rooms)

#---------------------------------------------------------------------------------------------------------------
# Soln. to Question 2:

# The key objective of the case study is to model the choice or alternatives amongst the different heating systems
# The response variable : depvar using the predictor variables given in the dataset like ic, oc etc.

# The data collected in the given excel file is grouped under which data class ? Ans : Cross sectional wide form of data

#---------------------------------------------------------------------------------------------------------------
# Soln. to Question 3:
data$depvar

# Here, depvar is a 5 level factor variable, having alternatives or choices as : ec, er, gc, gr and hp

# gc : gas central
# gr : gas room
# ec : electric central
# er : electric room
# hp : heat pump

#-------------------------------------------------------------------------------------------------
# Soln. to Question 4:
# Plotting a scatterplot matrix

pairs(data[,c("depvar","agehed","rooms","income")], pch=16, las=1)

#-------------------------------------------------------------------------------------------------

# Soln. to Question 5:

#Summarising the dataset : 
summary(data)

# Observations :
# Out of 900 : maximum people have choosen gc as the choice followed by gr
# Median number of rooms is 4 
# Median agehed is 45 years
# Annual income of the household median is 5

# Check the datatypes
str(data)
sapply(data, class)

# Here : depvar, region are factors . Rest all the variables are numeric

#-------------------------------------------------------------------------------------------------
# Soln. to Question 6:

# Creating a frequency distribution table
table(data$depvar)

counts <- table(data$depvar)
barplot(counts, main="Alternatives Distribution")

# We observe gc has the highest count, most chosen among the households followed by gr and er


#-------------------------------------------------------------------------------------------------
# Soln. to Question 7:

# Creating a frequency distribution table
table(data$region)

counts <- table(data$region)
barplot(counts, main="Region Distribution")

# We observe : scostl : souther coastal region is the region  having the max count
# Followed by ncostl, valley and mountain
#-------------------------------------------------------------------------------------------------
# Soln. to Question 8:

boxplot(agehed ~ depvar, data = data, xlab = "Age",
        ylab = "Age", main = "Age vs Depvar")

# Observation:
# It can be seen that the distribution for different alternatives are quite similar wrt. income
# Also : Median age is lowest for hp and almost similar for ec, gc and gr 
# Skewness is towards the left, clearly seen from the plot above except for hp choice which has it towards the right

#-------------------------------------------------------------------------------------------------

# Soln. to Question 9:

# Creating a 2 way frequency table

freq_table <- table(data$depvar,data$region)
freq_table

#-------------------------------------------------------------------------------------------------
# Soln. to Question 10:

# Draw a bar plot to the table created above: 

barplot(freq_table, main="Choice vs Region Distribution",xlab="Treatment Group",legend = rownames(freq_table), beside=TRUE)

# Clearly seen : GC is the most favored choice in all the 4 regions

#-------------------------------------------------------------------------------------------------
# Soln. to Question 11:
# Creating a long dataset based on the alternatives/choices :

# install.packages("mlogit")
library(mlogit)
heating <- mlogit.data(data, shape = "wide", choice = "depvar", varying = c(3:12))
heating

#-------------------------------------------------------------------------------------------------
# Soln. to Question 12:
# To estimate the logit model :
# The researcher needs data on the attributes of all the alternatives, not just the attributes for the chosen alternative. 
# The researcher needs to determine how much it would have cost to install each of the systems if they had been installed. 
# Hence, to achieve it, we transform the dataset into long format 
summary(heating)

#-------------------------------------------------------------------------------------------------
# Soln. to Question 13:

model <- mlogit(depvar ~ ic + oc | 0, heating)
summary(model)

#-------------------------------------------------------------------------------------------------
# Soln. to Question 14:
# Yes, they are negative as expected (with ic being -0.006 and oc being -0.004)
# As the cost of a system rises and the costs of the other systems remain the same,
# the probability of that system being chosen falls.
# Significance : Yes, the t-statistics are greater than 1.96 : showing significance & the critical level for 95% confidence level.

#-------------------------------------------------------------------------------------------------
# Soln. to Question 15:

wtp<-coef(model)["oc"]/coef(model)["ic"]

# WTP ( WIllingness to pay )
# Ratio of the operating cost coefficient to the installation cost coefficient.

#-------------------------------------------------------------------------------------------------
# Soln. to Question 16:

wtp

# Value of willingness to pay (WTP) is 0.735
# Observations :
# The model implies that the decision-maker is willing to pay $.73 (ie., 73 cents) 
# in higher installation cost in order to reduce annual operating costs by $1.
# it isn't very reasonable : A $1 reduction in annual operating costs recurs each year.
# It is unreasonable to think that the decision-maker is only willing to pay only 73 cents as a one-time payment in return for a $1/year stream of saving. 
#-------------------------------------------------------------------------------------------------
# Soln. to Question 17:

model1 <- mlogit(depvar ~ oc + I(ic / income), heating)
summary(model1)


#-------------------------------------------------------------------------------------------------
# Soln. to Question 18:
summary(model)
summary(model1)
# The model seems to get better compared to the previous model (-1010.2 vs -1095.2) 
# The lnL is higher (less negative) but the coefficient on installation cost/income becomes insignificant

#-------------------------------------------------------------------------------------------------
# Soln. to Question 19:

model2 <- mlogit(depvar ~ oc + ic | income, heating, reflevel = "hp")
summary(model2)

#-------------------------------------------------------------------------------------------------
# Soln. to Question 20:

# What do the estimates imply about the impact of income on the choice of central systems versus room system? 
# The model implies that as income rises, the probability of heat pump rises relative to all the others since income in the heat pump alt is normalized to zero, and the others enter with negative signs such that they are lower than that for heat pumps.
# Also, as income rises, the probability of gas room drops relative to the other non-heat-pump systems (since it is most negative).

# Do these income terms enter significantly? 
# No. It seems that income doesn't really have an effect. 
# Maybe this is because income is for the family that lives in the house, whereas the builder made decision of which system to install.

#-------------------------------------------------------------------------------------------------
# Answer to Question 21 :

waldtest(model,model2)

#-------------------------------------------------------------------------------------------------