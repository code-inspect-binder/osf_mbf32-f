#############################################################################################################
######        SSSS Preconference Workshop: Latent Variable Analysis in R Made Easy (Ver. 1.0)
######        Instructor/Script-Writer: John K. Sakaluk, University of Victoria
######        This script, and all other workshop materials are made available
######        under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International
######        License. Please consider citing Sakaluk & Short (2016) if you find the EFA
######        materials, in particular,  help you in your own research, as citations show my department that
######        this kind of work is valued by the academic commmunity and enables me to do more.
#############################################################################################################


#This is a comment; use it to leave yourself/others notes about what you are doing with your code.

#Embed Text in ##### on either side, and you create a "heading" for a set of related code--useful for organizing 

#### (1) Installing/calling packages, and importing data ####

#Most of the analyses we will do today use the psych (for EFA), lavaan (for CFA/SEM), and ggplot2 (for plotting) packages.
#You only need to install a package once, and then you can use it forever. Re-installing it will update it (if necessary).
install.packages("psych")
install.packages("lavaan")
install.packages("ggplot2")

#Every time you open R-Studio/R, you need to "call" the packages into memory that you want to use.
#You can do this at any time, but it's often most convenient to do this in one step at the start.
#We'll install/call a few later, for advanced/specific needs, but these will be the big three. 
library(psych)
library(lavaan)
library(ggplot2)

#Note: developers create packages for you to benefit from, for free: cite them in your research when you use them (and R)
citation("psych")
citation("lavaan")
citation("ggplot2")
citation()

#IF YOU DIDN'T BRING YOUR OWN DATA: don't worry, each package has example datasets to use.
#We'll use the psych() package's "BFI" data set, of personality dimension data.

#Store bfi data in data-frame object called "SSSSdat". (Go ahead and click on it [top right window] to see what it looks like)
SSSSdat = bfi

#OR IF YOU BROUGHT YOUR OWN DATA (in .csv format):
SSSSdat = read.csv(file.choose())

#Note that in either/both cases, the "SSSS.dat" name is totally arbitrary. Name your datasets/output/plot objects something helpful.

#### (2) CFA of initially plausible measurement models ####

#Specify a unidimensional model (e.g., it's all personality), call it uni.cfa
uni.cfa = '
personality =~ A1 + A2 + A3 + A4 + A5 + C1 + C2 + C3 + C4 + C5 + E1 + E2 + E3 + E4 + E5 + N1 + N2 + N3 + N4 + N5 + O1 + O2 + O3 + O4 + O5
'
#Fit uni.cfa and save output in uni.cfi.fit. Use fixed factor (std.lv = TRUE), and FIML for missing data)
uni.cfa.fit = cfa(uni.cfa, data = SSSSdat, std.lv = TRUE, missing = "ML")
#Request summary output from model, including fit indexes, standardized estimates, and R^2/communalities
summary(uni.cfa.fit, fit.measures = TRUE, standardized = TRUE, rsquare=TRUE)

#Try a two factor model (e.g., positive and negative elements of personality)
posneg.cfa = '
positive =~ A1 + A2 + A3 + A4 + A5 +C1 + C2 + C3 + C4 + C5 + E1 + E2 + E3 + E4 + E5 +O1 + O2 + O3 + O4 + O5
negative =~ N1 + N2 + N3 + N4 + N5
'
posneg.fit = cfa(posneg.cfa, data = SSSSdat, std.lv = TRUE, missing = "ML")
summary(posneg.fit, fit.measures = TRUE, standardized = TRUE, rsquare=TRUE)

#Initial models aren't good, so now is a good time to move onto an exploratory data-driven model

#### (3) EFA of data-driven measurement models ####

#Now's a good time to look at what column #'s each variable is located in.
#We will need this information in order to specify the variables to-be factor-analzyed
names(SSSSdat)

#We need to get an idea of how many factors are likely needed. Parallel analysis can help.
#Save output in object "parallel", using psych() fa.parallel function.
#Variables 1-25 (the BFI items) are to be analzyed, using maximum likelihood (ml) common factors (fa).
#Simulate 50 other samples of "garbage factors", using R^2s (SMC) as initial communality estimates
#and compare observed eigenvalues to 95th quantile of simulated "garbage factor" eigenvalues
parallel = fa.parallel(SSSSdat[1:25], fm = 'ml', fa = 'fa', n.iter = 50, SMC = TRUE, quant = .95)

# RUN ALL THIS (UNTIL THE NEXT COMMENT) MINDLESSLY TO GET PRETTIER PLOT 
obs = data.frame(parallel$fa.values)
obs$type = c('Observed Data')
obs$num = c(row.names(obs))
obs$num = as.numeric(obs$num)
colnames(obs) = c('eigenvalue', 'type', 'num')

percentile = apply(parallel$values,2,function(x) quantile(x,.95))
min = as.numeric(nrow(obs))
min = (4*min) - (min-1)
max = as.numeric(nrow(obs))
max = 4*max
percentile1 = percentile[min:max]

sim = data.frame(percentile1)
sim$type = c('Simulated Data (95th Quantile)')
sim$num = c(row.names(obs))
sim$num = as.numeric(sim$num)
colnames(sim) = c('eigenvalue', 'type', 'num')

eigendat = rbind(obs,sim)

apatheme=theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        text=element_text(family='Arial'),
        legend.title=element_blank(),
        legend.position=c(.7,.8),
        axis.line.x = element_line(color='black'),
        axis.line.y = element_line(color='black'))

p = ggplot(eigendat, aes(x=num, y=eigenvalue, shape=type)) +
  geom_line()+
  geom_point(size=4)+
  scale_y_continuous(name='Eigenvalue')+
  scale_x_continuous(name='Factor Number', breaks=min(eigendat$num):max(eigendat$num))+
  scale_shape_manual(values=c(16,1)) +
  geom_vline(xintercept = parallel$nfact, linetype = 'dashed')+
  apatheme
p

# STOP HERE FOR PRETTY-PLOTTING

#If you want a hi-res version for publication, run this to save to root directory
ggsave('parallel.png', width=6, height=6, unit='in', dpi=300)

#Parallel analysis suggests 8 factors max. And CFA has already ruled out the unidimensional solution.
#How to proceed? Fit EFA models for 2-8 factors. Which ones fit well? Of those fitting well, 
#which have good simple structure and are interpretable? Helpful to make a table to keep all info organized (see OSF)...

#Fit solutions to BFI data of 2-8 factors and save output. Use oblique (oblimin) rotation, R^2s (SMC) as initial
#communalities, maximum likelihood (ml) as estimator, so we can get model fit indexes. Oblique ten Berge 
#factor scores can be used later for more powerful/accurate analysis of factors (e.g., in ANOVAs/regressions)
two.fac = fa(SSSSdat[1:25], nfactors = 2, rotate = "oblimin", SMC = TRUE, fm = "ml",  scores = "tenBerge",oblique.scores = TRUE)
three.fac = fa(SSSSdat[1:25], nfactors = 3, rotate = "oblimin", SMC = TRUE, fm = "ml",  scores = "tenBerge",oblique.scores = TRUE)
four.fac = fa(SSSSdat[1:25], nfactors = 4, rotate = "oblimin", SMC = TRUE, fm = "ml",  scores = "tenBerge",oblique.scores = TRUE)
five.fac = fa(SSSSdat[1:25], nfactors = 5, rotate = "oblimin", SMC = TRUE, fm = "ml",  scores = "tenBerge",oblique.scores = TRUE)
six.fac = fa(SSSSdat[1:25], nfactors = 6, rotate = "oblimin", SMC = TRUE, fm = "ml",  scores = "tenBerge",oblique.scores = TRUE)
seven.fac = fa(SSSSdat[1:25], nfactors = 7, rotate = "oblimin", SMC = TRUE, fm = "ml",  scores = "tenBerge",oblique.scores = TRUE)
eight.fac = fa(SSSSdat[1:25], nfactors = 8, rotate = "oblimin", SMC = TRUE, fm = "ml",  scores = "tenBerge",oblique.scores = TRUE)

#Request the output from each EFA model
two.fac
three.fac
four.fac
five.fac
six.fac
seven.fac
eight.fac

#### (4) CFA of a previoulsy-developmed measurement model ####

#Save lavaan syntax for five-factor model in object called "five.cfa"
five.cfa =  '
agree =~ A1 + A2 + A3 + A4 + A5  
conscie =~ C1 + C2 + C3 + C4 + C5 
extravert =~ E1 + E2 + E3 + E4 + E5
neurot =~ N1 + N2 + N3 + N4 + N5 
open =~ O1 + O2 + O3 + O4 + O5
'
#Fit five.cfa and save output in five.cfa.fit Use fixed factor (std.lv = TRUE), and FIML for missing data)
five.cfa.fit = cfa(five.cfa, data = SSSSdat, std.lv = TRUE, missing = "ML")
#Request summary output from model, including fit indexes, standardized estimates, and R^2/communalities
summary(five.cfa.fit, fit.measures = TRUE, standardized = TRUE, rsquare=TRUE)

#We can request mod indexes for our particular model
modindices(five.cfa.fit)

#And then you could respecify  your model and check its fit again.
#Model fit sucks so bad that it's not even worth revising, but we will carry on for sake of example...

#Measurement nvariance testing by group can be easily conducted with help of semTools package
install.packages("semTools")
library(semTools)
citation("semTools")

#Fit measurement invariance models based on five.cfa model, distinguishing by group levels of "gender".
#Save output in invar.output object
invar.output = measurementInvariance(five.cfa, data = SSSSdat, group = "gender")

#If you want to look at the full output of a particular model, then run...
#For configural invariance model...
summary(invar.output$fit.configural, fit.measures = TRUE, standardized = TRUE, rsquare=TRUE)
#For weak invariance model...
summary(invar.output$fit.loadings, fit.measures = TRUE, standardized = TRUE, rsquare=TRUE)
#For strong invariance model
summary(invar.output$fit.intercepts, fit.measures = TRUE, standardized = TRUE, rsquare=TRUE)

#### (5) SEM for evaluating validity####

#Let's pretend our model fit from the previous step was good, so now we want to parcel our measurement model
#to look at correlations/regressions involving our factors and other validity-relevant variables

#Create 3 parcels (e.g., O.P.1 for openness parcel 1) for each factor by averaging items in groups (1 loner, since 5 items)

#For opennesss
SSSSdat$O.P.1 = (SSSSdat$O1 + SSSSdat$O2)/2
SSSSdat$O.P.2 = (SSSSdat$O3 + SSSSdat$O4)/2
SSSSdat$O.P.3 = SSSSdat$O5

#For agreeableness
SSSSdat$A.P.1 = (SSSSdat$A1 + SSSSdat$A2)/2
SSSSdat$A.P.2 = (SSSSdat$A3 + SSSSdat$A4)/2
SSSSdat$A.P.3 = SSSSdat$A5

#For conscientiousness
SSSSdat$C.P.1 = (SSSSdat$C1 + SSSSdat$C2)/2
SSSSdat$C.P.2 = (SSSSdat$C3 + SSSSdat$C4)/2
SSSSdat$C.P.3 = SSSSdat$C5

#For extraversion
SSSSdat$E.P.1 = (SSSSdat$E1 + SSSSdat$E2)/2
SSSSdat$E.P.2 = (SSSSdat$E3 + SSSSdat$E4)/2
SSSSdat$E.P.3 = SSSSdat$E5

#For neuroticism
SSSSdat$N.P.1 = (SSSSdat$N1 + SSSSdat$N2)/2
SSSSdat$N.P.2 = (SSSSdat$N3 + SSSSdat$N4)/2
SSSSdat$N.P.3 = SSSSdat$N5

#Now measurement model of parcelled factors correlated w/ education and age (observed)
#To get mere correlations, we need to "trick" lavaan into thinking they are single-item latent variables
#and then correlations will be automatically calculated
five.sem.parcel.corr =  '
agree =~ A.P.1 + A.P.2 + A.P.3 
conscie =~ C.P.1 + C.P.2 + C.P.3 
extravert =~ E.P.1 + E.P.2 + E.P.3 
neurot =~ N.P.1 + N.P.2 + N.P.3 
open =~ O.P.1 + O.P.2 + O.P.3
P.age =~ age
P.edu =~ education
'

#Fit five.sem.parcel.corr and save output in five.sem.parcel.corr.fit Use fixed factor (IMPORTANT), and FIML for missing data)
five.sem.parcel.corr.fit = cfa(five.sem.parcel.corr, data = SSSSdat, std.lv = TRUE, missing = "ML")
#Request summary output from model, including fit indexes, standardized estimates, and R^2/communalities
summary(five.sem.parcel.corr.fit, fit.measures = TRUE, standardized = TRUE, rsquare=TRUE)


#If we wanted to use regression models instead (e.g., unique prediction of edu/age by BFI factors), then we 
#can drop the faux-latent variables for age and education and indicate regression pathways using "~"
five.sem.parcel.reg =  '
agree =~ A.P.1 + A.P.2 + A.P.3 
conscie =~ C.P.1 + C.P.2 + C.P.3 
extravert =~ E.P.1 + E.P.2 + E.P.3 
neurot =~ N.P.1 + N.P.2 + N.P.3 
open =~ O.P.1 + O.P.2 + O.P.3

age ~ agree + conscie + extravert + neurot + open
education ~ agree + conscie + extravert + neurot + open
'

#Fit five.sem.parcel.corr and save output in five.sem.parcel.corr.fit Use fixed factor (IMPORTANT), and FIML for missing data)
five.sem.parcel.reg.fit = cfa(five.sem.parcel.reg, data = SSSSdat, std.lv = TRUE, missing = "ML")
#Request summary output from model, including fit indexes, standardized estimates, and R^2/communalities
summary(five.sem.parcel.reg.fit, fit.measures = TRUE, standardized = TRUE, rsquare=TRUE)

#### (6) IRT and LCA for Deeper/Alternative Understanding ####

#IRT

#Use ltm() package for IRT analysis 
install.packages("ltm")
library(ltm)
citation("ltm")

#Fit gpcm IRT model to conscientiousness items
conscie.irt = gpcm (SSSSdat[6:10], constraint = "gpcm")
#Request summary output
summary(conscie.irt)

#Plot item informative curve of the conscientiousness items
plot(conscie.irt, type = c("IIC"))

#Plot item informative curve of the conscientiousness composite
plot(conscie.irt, type = c("IIC"), items = c(0))

#LCA 

#Use poLCA package for LCA analysis
install.packages("poLCA")
library(poLCA)
citation("poLCA")

#Store in an object a list of all the variables to be used in LCA models
personality=cbind(A1, A2, A3, A4, A5, O1, O2, O3, O4, O5, C1, C2, C3, C4, C5, N1, N2, N3, N4, N5, E1, E2, E3, E4, E5)~1

#Fit models that increase in number of classes extracted
one.class=poLCA(personality, SSSSdat, nclass=1, maxiter = 1000, nrep = 5, graphs=TRUE)
two.class=poLCA(personality, SSSSdat, nclass=2, maxiter = 1000, nrep = 5, graphs=TRUE)
three.class=poLCA(personality, SSSSdat, nclass=3, maxiter = 1000, nrep = 5, graphs=TRUE)
four.class=poLCA(personality, SSSSdat, nclass=4, maxiter = 1000, nrep = 5, graphs=TRUE)
five.class=poLCA(personality, SSSSdat, nclass=5, maxiter = 1000, nrep = 5, graphs=TRUE)
six.class=poLCA(personality, SSSSdat, nclass=6, maxiter = 1000, nrep = 5, graphs=TRUE)
seven.class=poLCA(personality, SSSSdat, nclass=7, maxiter = 1000, nrep = 5, graphs=TRUE)
