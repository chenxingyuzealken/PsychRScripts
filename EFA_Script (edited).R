#References: 
#code, procedures & other information were gathered from:

##Dang, A.T. (2021, May 11). Exploratory Factor Analysis in R. Learning by doing. Towards Data Science. https://towardsdatascience.com/exploratory-factor-analysis-in-r-e31b0015f224
##Murphy, P. (2021, April 21). Exploratory Factor Analysis. R Pubs by Rstudio  https://rpubs.com/pjmurphy/758265

##Pituch, K.A., & Stevens, J.P. (2016). Exploratory Factor Analysis. In K.A. Pituch, & J.P. Stevens (Eds.), Applied Multivariate Statistics for the Social Sciences: Analyses with SAS and IBM’s SPSS, Sixth Edition (6th ed., pp.339-390 ). 
##Routledge. https://doi.org/10.4324/9781315814919

##Taber, K.S.(2018) The Use of Cronbach’s Alpha When Developing and Reporting Research Instruments in Science Education. 
##Research in Science Education, 48, 1273–1296. https://doi.org/10.1007/s11165-016-9602-2

##Watkins, M. W. (2021). A step-by-step guide to exploratory factor analysis with R and Rstudio. Routledge. https://doi.org/10.4324/9781003120001  


#load needed packages
library(psych)
library(nFactors)
library(corrplot)
library(dplyr)


#get the dataset
data_one <- read.csv("<..>")

#STEP 1
#exploring the data

#select the variables that you might want to run an EFA on
data_two <- data_one %>%
  select(<..>)

summary(data_two)

##identify which variables have the most missing values
colSums(is.na(data_two))

##eliminate observations with missing data
data_three <- na.omit(data_two)

#evaluate the correlation matrix
datamatrix <- cor(data_three)
corrplot(datamatrix, method="number")
#interpretation#
#You should first ensure that there are several coefficients >=.30 (Hair et al., 2019, as cited in Watkins, 2021; Tabachnick & Fidell, 2019, as cited in Watkins, 2021)
#Correlations should not be above 0.90 (Tabachnick and Fidell, 2019, , as cited in Watkins, 2021)
#If there are variables that are too highly correlated (i.e., r>0.90), select one variable from each pair to omit from the dataset (Murphy, 2021)
write.csv(cor(data_three)>0.9, file="Suspect_Correlations.csv")
write.csv(cor(data_three), file = "Correlation_Values.csv")
#use the CSV files to find variables that are too highly correlated (r>0.90) and select one variable from each pair to omit from the dataset (Murphy, 2021)
#remove one variable from each pair to omit from the dataset
overcorrelated <- c("<..>","<..>") #replace "<..>" with the variables that you wish to remove. You can add as many variables as you need; it doesn't necessarily need to be two variables
data_four <- data_three[ ,!(names(data_three) %in% overcorrelated)] #remove the above specified variables from the data


#KMO test
KMO(data_four) #look at the results and eliminate all low-contribution variables if needed (Murphy, 2021)
#interpretation#
#KMO values for each variable and the total sample should be reviewed (Watkins, 2021)
#KMO values <0.50 are unacceptable (Kaiser, 1974, as cited in Watkins, 2021)
#some scholars recommend a minimum value of 0.60 (Mvududu & Sink, 2013, , as cited in Watkins, 2021; Watson, 2017, , as cited in Watkins, 2021)
#values of >=0.70 is preferred (Hoelzle & Meyer, 2013, as cited in Watkins, 2021)

#Bartlett's test of sphericity
cortest.bartlett(data_four) #only proceed if the null hypothesis can be rejected

#interpretation#
#A KMO value of over 0.5 and significance level of p<0.05 for Bartlett's test show that
#there is substantial correlation in the data 

#STEP 2: DETERMINE NUMBER OF FACTORS TO EXTRACT 
#Method 1: Eigenvalue method
ev <- eigen(cor(data_four)) #get eigenvalues
ev$values

#Method 2: scree plot
scree(data_four, pc=FALSE, hline="-1") #use pc=FALSE for factor analysis

#Method 3: parallel analysis
fa.parallel(data_four, fa="fa")

#evaluation: evaluate the results to determine the number of factors to extract

#STEP 3: EXTRACT (AND ROTATE) FACTORS
#here, we are beginning with the assumption of correlated factors. Hence, oblique rotation. (Murphy, 2021) Overall, oblique rotations are favored over orthogonal rotations (Watkins, 2021)

Nfacs <- <..> #This value specifies the number of factors
No_of_observations <- nrow(data_four)
fit <- fa(data_four, nfactors=Nfacs, rotate="promax",  residuals = TRUE, SMC = TRUE, missing = FALSE, fm="ml", n.obs = No_of_observations) #promax rotation, ML extraction (Watkins, 2021)

print(fit, digits=3, sort=TRUE) 
#evaluate the correlation matrix to justify oblique vs orthogonal rotation

#STEP 4: EVALUATE WHAT YOU HAVE PRODUCED

#export factor loadings
dim_factor_loadings <- dim(fit$loadings)
no_of_variables <- dim_factor_loadings[1]
no_of_factors <- dim_factor_loadings[2]

round(fit$loadings[1:no_of_variables,], no_of_factors)

FactorLoadings <- round(fit$loadings[1:no_of_variables,], no_of_factors)
write.csv(FactorLoadings, file="FacLoads.csv")

#evaluation: 
#Decide whether there are any variables that do not load sufficiently well onto any of the factors 
#Factor loadings which are >= |0.4| are considered meaningful (Pituch & Stevens, 2016)
#Despite the above guideline, one should exercise judgments regarding which factor loadings are meaningful using the literature

#if you choose to remove variables, you will have to re-run the factor analysis with the newly reduced data set (only proceed when satisfied with the model) (Murphy, 2021)

#evaluating the factors for their internal consistency (Cronbach's Alpha)
##specify which variables belong to each factor
##add or remove the number of factors here as needed (e.g., f1, f2, f3, f4, f5,...)
f1 <- data_four[ , c("<...>","<...>")]
f2 <- data_four[ , c("<...>","<...>")]
f3 <- data_four[ , c("<...>","<...>")]
f4 <- data_four[ , c("<...>","<...>")]

##get the Cronbach's Alpha for each factor
##add or remove the number of factors here as needed (e.g., f1, f2, f3, f4, f5,...)
alpha(f1, check.keys=TRUE) #specify check.keys=TRUE if some of the variables are loading negatively onto the factor
alpha(f2, check.keys=TRUE)
alpha(f3, check.keys=TRUE)
alpha(f4, check.keys=TRUE)

##evaluation: look at the raw-alpha value for the entire factor (Murphy, 2021)
##alpha values of >=0.70 is generally considered as desirable (Taber, 2018)
#then, look at the raw_alpha column under "reliability if an item is dropped". this tells you how alpha will change if each of the variables were removed (Murphy, 2021)
#at this point, if you choose to remove the variable from the data set, you need to rerun the analysis before re-evaluating (Murphy, 2021)

##to just get the overall alpha ("raw_alpha" in the total set measures) for each factor
##add or remove the number of factors here as needed (e.g., f1, f2, f3, f4, f5,...)
alpha(f1, check.keys=TRUE)$total[1]
alpha(f2, check.keys=TRUE)$total[1]
alpha(f3, check.keys=TRUE)$total[1]
alpha(f4, check.keys=TRUE)$total[1]








