library(sem)

# Duncan, Haller, and Portes's nonrecursive peer-influences model

    # FIML estimates from the sem function

R.DHP <- readMoments(diag=FALSE, names=c('ROccAsp', 'REdAsp', 'FOccAsp', 
  'FEdAsp', 'RParAsp', 'RIQ', 'RSES', 'FSES', 'FIQ', 'FParAsp')) 
  .6247                                                 
  .3269  .3669                                                
  .4216  .3275  .6404                               
  .2137  .2742  .1124  .0839                          
  .4105  .4043  .2903  .2598  .1839                     
  .3240  .4047  .3054  .2786  .0489  .2220                
  .2930  .2407  .4105  .3607  .0186  .1861  .2707           
  .2995  .2863  .5191  .5007  .0782  .3355  .2302  .2950      
  .0760  .0702  .2784  .1988  .1147  .1021  .0931 -.0438  .2087
  
  # Note: lower triangle of correlation matrix

 #  path                parameter  start-value
model.DHP.1 <- specifyModel()
    RIQ     ->  ROccAsp, gamma51,  NA
    RSES    ->  ROccAsp, gamma52,  NA
    FSES    ->  FOccAsp, gamma63,  NA
    FIQ     ->  FOccAsp, gamma64,  NA  
    FOccAsp ->  ROccAsp, beta56,   NA
    ROccAsp ->  FOccAsp, beta65,   NA
    ROccAsp <-> ROccAsp, sigma77,  NA 
    FOccAsp <-> FOccAsp, sigma88,  NA   
    ROccAsp <-> FOccAsp, sigma78,  NA
    
model.DHP.1

model.DHP.1 <- specifyEquations()  # alternative, equivalent specification
ROccAsp = gamma51*RIQ + gamma52*RSES + beta56*FOccAsp
FOccAsp = gamma64*FIQ + gamma63*FSES + beta65*ROccAsp
V(ROccAsp) = sigma77
V(FOccAsp) = sigma88
C(ROccAsp, FOccAsp) = sigma78

model.DHP.1

sem.DHP.1 <- sem(model.DHP.1, S=R.DHP, N=329,
                 fixed.x=c('RIQ', 'RSES', 'FSES', 'FIQ'))
summary(sem.DHP.1)


# a latent-variable model for the Duncan, Haller, and Portes Data

model.DHP.2 <- specifyEquations(covs="RGenAsp, FGenAsp") # alternative specification to V() and C()
RGenAsp = gam11*RParAsp + gam12*RIQ + gam13*RSES + gam14*FSES + beta12*FGenAsp
FGenAsp = gam23*RSES + gam24*FSES + gam25*FIQ + gam26*FParAsp + beta21*RGenAsp
ROccAsp = 1*RGenAsp
REdAsp = lam21(1)*RGenAsp  # to illustrate setting start values
FOccAsp = 1*FGenAsp
FEdAsp = lam42(1)*FGenAsp

sem.DHP.2 <- sem(model.DHP.2, S=R.DHP, N=329,
                 fixed.x=c('RParAsp', 'RIQ', 'RSES', 'FSES', 'FIQ', 'FParAsp'))
summary(sem.DHP.2)

    # a model with cross-friend equality constraints

model.DHP.3 <- specifyEquations() 
RGenAsp = gam1*RParAsp + gam2*RIQ + gam3*RSES + gam4*FSES + beta*FGenAsp
FGenAsp = gam4*RSES + gam3*FSES + gam2*FIQ + gam1*FParAsp + beta*RGenAsp
ROccAsp = 1*RGenAsp
REdAsp = lam*RGenAsp 
FOccAsp = 1*FGenAsp
FEdAsp = lam*FGenAsp
V(RGenAsp) = theta1
V(FGenAsp) = theta1
C(RGenAsp, FGenAsp) = theta12

sem.DHP.3 <- sem(model.DHP.3, S=R.DHP, N=329,
                 fixed.x=c('RParAsp', 'RIQ', 'RSES', 'FSES', 'FIQ', 'FParAsp'))
summary(sem.DHP.3)
anova(sem.DHP.2, sem.DHP.3) # LR test of cross-friend constraints


# Confirmatory Factor Analyis of Holzinger's data on 9 psychological tests

R.Holzinger <- read.moments(diag=FALSE, names=c("Word.meaning", 
  "Sentence.completion", "Odd.words", "Mixed.arithmetic", "Remainders", 
  "Missing.numbers", "Gloves", "Boots", "Hatchets"))                                            
     .75                                       
     .78   .72                                   
     .44   .52    .47                              
     .45   .53    .48    .82                         
     .51   .58    .54    .82    .74                    
     .21   .23    .28    .33    .37    .35               
     .30   .32    .37    .33    .36    .38    .45          
     .31   .30    .37    .31    .36    .38    .52    .67      

R.Holzinger

    # CFA model with correlated factors

model.Holzinger.1 <- cfa(reference.indicators=FALSE)
Verbal: Word.meaning, Sentence.completion, Odd.words
Arithmetic: Mixed.arithmetic, Remainders, Missing.numbers
Spatial: Gloves, Boots, Hatchets

sem.Holzinger.1 <- sem(model.Holzinger.1, S=R.Holzinger, N=696)
summary(sem.Holzinger.1)

    # CFA model with uncorrelated factors

model.Holzinger.2 <- cfa(reference.indicators=FALSE, 
                         covs=c("Verbal", "Arithmetic", "Spatial"))
Verbal: Word.meaning, Sentence.completion, Odd.words
Arithmetic: Mixed.arithmetic, Remainders, Missing.numbers
Spatial: Gloves, Boots, Hatchets

sem.Holzinger.2 <- sem(model.Holzinger.2, S=R.Holzinger, N=696)
summary(sem.Holzinger.2)

    # test of factor correlations

anova(sem.Holzinger.1, sem.Holzinger.2)

# A model fit to raw data: Bollen's industrialization and democracy example 

?Bollen

model.bollen <- specifyEquations()
y1 = 1*Demo60 # measurement submodel
y2 = lam2*Demo60
y3 = lam3*Demo60
y4 = lam4*Demo60
y5 = 1*Demo65
y6 = lam2*Demo65
y7 = lam3*Demo65
y8 = lam4*Demo65
x1 = 1*Indust
x2 = lam6*Indust
x3 = lam7*Indust
c(y1, y5) = theta15
c(y2, y4) = theta24
c(y2, y6) = theta26
c(y3, y7) = theta37
c(y4, y8) = theta48
c(y6, y8) = theta68
Demo60 = gamma11*Indust # structural submodel
Demo65 = gamma21*Indust + beta21*Demo60
v(Indust) = phi

model.bollen

sem.bollen <- sem(model.bollen, data=Bollen)
summary(sem.bollen)
summary(sem.bollen, robust=TRUE) # robust SEs and tests
