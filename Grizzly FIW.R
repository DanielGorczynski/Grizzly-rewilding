

###############################################################################

##load helper functions
source('code/fuzzy membership splus functions.R')

#### Fuzzy Interaction Web Function
##Has specifications for interaction effects incorporated

fcm<- function(s,w,fix=rep(0,length(s)),
               lambda=0.5, max.iter=500,
               af=rep(1,length(s)), tol=0.001,c=5) {
  #
  # Fuzzy cognitive map algorithm
  # s is the initial state vector variable [0,1]
  # w is the matrix of edge weights [-1,1]
  # fix is the vector indicating which element of s are fixed boundary conditions
  # lambda is the relaxation parameter to speed convergence
  # activation functions are (1)logistic, (2) threshold exponential or (3) threshold linear
  # af - vector specifying which activation function to be used on each element of s
  #
  # D. Ramsey 30/4/04
  #
  
  state<- matrix(nrow=max.iter,ncol=length(s))
  raw<- matrix(nrow=max.iter,ncol=length(s))
  ##Set based on number of interaction effects
  interact <- matrix(nrow=max.iter, ncol = 5)
  state[1,]<- s
  raw[1,]<- s
  colnames(interact) <- c("elk.ml","elk.griz","carc.griz","griz.elk","griz.carc")
  
  iter<- 1
  
  for(z in 2:max.iter) {
    
    ##Inclusion of iterative interaction effects
    elk.ml <- -0.3 + (-0.05*state[z-1,22]) + (-0.05*state[z-1,18])
    w["elk/deer","mountain lions"] <- elk.ml
    interact[z-1,1] <- elk.ml
    
    elk.griz <- -0.05 + (-0.05*state[z-1,16])
    w["elk/deer","grizzlies"] <- elk.griz
    interact[z-1,2] <- elk.griz
    
    carc.griz <- -0.1 + (0.05*state[z-1,16]) 
    w["carcasses","grizzlies"] <- carc.griz
    interact[z-1,3] <- carc.griz
    
    griz.elk <- 0.1 + (0.1*state[z-1,16])
    w["grizzlies","elk/deer"] <- griz.elk
    interact[z-1,4] <- griz.elk
    
    griz.carc <- 0.2 + (-0.1*state[z-1,16]) 
    w["grizzlies","carcasses"] <- griz.carc
    interact[z-1,5] <- griz.carc
    
    tmp<- w %*% state[z-1,]
    for(i in 1:nrow(w)) {
      
      
      if(af[i]==1){
        # logistic
        state[z,i] <- lambda* (1/(1+exp(-c*tmp[i]))) + (1-lambda)*state[z-1,i]
        raw[z,i]<- tmp[i]
      }
      else if(af[i]==2) {
        # exponential
        state[z,i]<- lambda * max(0,1-exp(-c*tmp[i])) + (1-lambda)*state[z-1,i]
        raw[z,i]<- tmp[i]
      }
      else if(af[i]==3){
        # linear
        state[z,i] <- lambda* (min(1,max(0,c*tmp[i]))) + (1-lambda)*state[z-1,i]
        raw[z,i]<- tmp[i]
      }
      
    }
    state[z,fix==1]<- state[1,fix==1] # reset boundary conditions (control)
    
    
    ind<- abs(state[z,] - state[z-1,]) < tol
    
    if(length(state[z,ind])==length(s)) break
    iter<- iter + 1
  }
  
  if(iter == max.iter) cat(" WARNING ! Convergence not reached in ",max.iter," iterations",'\n')
  else cat("convergence reached after ",iter," iterations",'\n')
  
  interact <- as.data.frame(na.omit(interact))
  list(state=state[1:iter,],raw=raw[1:iter,],iter=iter, interact=interact)
  
}



#### STEP 1: Build Matrix for null-model
#### extracted from MPG Grassland Matrix https://matrix.mpgranch.com/#/matrices/bd9880

w <- matrix(0,nrow=24,ncol=24)
species <- c("shrub","precipitation","exotic forb","elk/deer","small carnivores",
             "invertebrate herbivores","invertebrate predators","forb","song birds",
             "grass","scavenging birds","small mammals","disturbance","hunters",
             "mountain lions","coyotes","veg crew","black bear",
             "exotic grass","snakes","pollinators","grizzlies","carcasses","wolves")
dimnames(w) <- list(species,species)

##Parameterize interactions


##Shrub
w["elk/deer","shrub"] <- 0.45
w["small carnivores","shrub"] <- 0.2
w["invertebrate herbivores","shrub"] <- 0.9
w["invertebrate predators","shrub"] <- 0.3
w["song birds","shrub"] <- 0.5
w["small mammals","shrub"] <- 0.5
w["disturbance","shrub"] <- -0.25
w["coyotes","shrub"] <- 0.15
w["black bear","shrub"] <- 0.25
w["exotic grass","shrub"] <- 0.2
w["pollinators","shrub"] <- 0.75
w["grizzlies","shrub"] <- 0.1

##Precipitation
w["shrub","precipitation"] <- 0.7
w["exotic forb","precipitation"] <- 0.95
w["forb","precipitation"] <- 1
w["grass","precipitation"] <- 1
w["exotic grass","precipitation"] <- 0.97

#Exotic forb
w["shrub","exotic forb"] <- -0.1
w["elk/deer","exotic forb"] <- 0.5
w["invertebrate herbivores","exotic forb"] <- 0.25
w["invertebrate predators","exotic forb"] <- 0.2
w["forb","exotic forb"] <- -0.25
w["grass","exotic forb"] <- -0.2
w["small mammals","exotic forb"] <- 0.4
w["disturbance","exotic forb"] <- -0.5
w["exotic grass","exotic forb"] <- -0.2
w["pollinators","exotic forb"] <- 0.28
w["grizzlies","exotic forb"] <- 0.15

#Elk/deer
w["shrub","elk/deer"] <- -0.27
w["exotic forb","elk/deer"] <- -0.15
w["forb","elk/deer"] <- -0.22
w["grass","elk/deer"] <- -0.35
w["disturbance","elk/deer"] <- 0.75
w["mountain lions","elk/deer"] <- 0.85
w["coyotes","elk/deer"] <- 0.25
w["exotic grass","elk/deer"] <- -0.05
w["grizzlies","elk/deer"] <- 0.1 #Variable interaction detailed above in FCM function
w["wolves","elk/deer"] <- 0.95

#small carnivores
w["invertebrate herbivores","small carnivores"] <- -0.1
w["song birds","small carnivores"] <- -0.45
w["scavenging birds","small carnivores"] <- 0.1
w["small mammals","small carnivores"] <- -0.5
w["disturbance","small carnivores"] <- 0.2
w["snakes","small carnivores"] <- -0.3
w["carcasses","small carnivores"] <- -0.05

#Invertebrate herbivores
w["shrub","invertebrate herbivores"] <- -0.2
w["small carnivores","invertebrate herbivores"] <- 0.5
w["invertebrate predators","invertebrate herbivores"] <- 0.85
w["forb","invertebrate herbivores"] <- -0.27
w["song birds","invertebrate herbivores"] <- 0.6
w["grass","invertebrate herbivores"] <- -0.15
w["small mammals","invertebrate herbivores"] <- 0.67
w["coyotes","invertebrate herbivores"] <- 0.2
w["black bear","invertebrate herbivores"] <- 0.1
w["snakes","invertebrate herbivores"] <- 0.5
w["grizzlies","invertebrate herbivores"] <- 0.05

#Invertebrate predators
w["invertebrate herbivores","invertebrate predators"] <- -0.5
w["song birds","invertebrate predators"] <- 0.6
w["small mammals","invertebrate predators"] <- 0.5
w["snakes","invertebrate predators"] <- 0.35
w["pollinators","invertebrate predators"] <- -0.25

#Forb
w["exotic forb","forb"] <- -0.4
w["elk/deer","forb"] <- 0.55
w["invertebrate herbivores","forb"] <- 0.9
w["invertebrate predators","forb"] <- 0.3
w["song birds","forb"] <- 0.25
w["grass","forb"] <- -0.1
w["small mammals","forb"] <- 0.75
w["disturbance","forb"] <- -0.25
w["black bear","forb"] <- 0.35
w["exotic grass","forb"] <- -0.3
w["pollinators","forb"] <- 1
w["grizzlies","forb"] <- 0.15

#Song birds
w["shrub","song birds"] <- -0.1
w["small carnivores","song birds"] <- 0.25
w["invertebrate herbivores","song birds"] <- -0.3
w["invertebrate predators","song birds"] <- -0.3
w["forb","song birds"] <- -0.05
w["scavenging birds","song birds"] <- 0.2
w["snakes","song birds"] <- 0.45
w["pollinators","song birds"] <- -0.25

#Grass
w["exotic forb","grass"] <- -0.55
w["elk/deer","grass"] <- 0.95
w["invertebrate herbivores","grass"] <- 0.8
w["invertebrate predators","grass"] <- 0.2
w["forb","grass"] <- -0.4
w["song birds","grass"] <- 0.25
w["small mammals","grass"] <- 0.65
w["disturbance","grass"] <- -0.25
w["black bear","grass"] <- 0.3
w["exotic grass","grass"] <- -0.5
w["grizzlies","grass"] <- 0.4

#scavenging birds
w["small carnivores","scavenging birds"] <- -0.25
w["invertebrate herbivores","scavenging birds"] <- -0.1
w["song birds","scavenging birds"] <- -0.25
w["small mammals","scavenging birds"] <- -0.5
w["snakes","scavenging birds"] <- -0.35
w["carcasses","scavenging birds"] <- -0.75

#Small mammals
w["shrub","small mammals"] <- -0.5
w["small carnivores","small mammals"] <- 0.5
w["invertebrate herbivores","small mammals"] <- -0.25
w["invertebrate predators","small mammals"] <- -0.3
w["forb","small mammals"] <- -0.2
w["song birds","small mammals"] <- -0.25
w["grass","small mammals"] <- -0.1
w["scavenging birds","small mammals"] <- 0.8
w["disturbance","small mammals"] <- 0.75
w["mountain lions","small mammals"] <- 0.25
w["coyotes","small mammals"] <- 0.75
w["black bear","small mammals"] <- 0.1
w["snakes","small mammals"] <- 0.85
w["wolves","small mammals"] <- 0.1

#Disturbance
w["exotic forb","disturbance"] <- 0.57
w["forb","disturbance"] <- -0.1
w["grass","disturbance"] <- 0
w["exotic grass","disturbance"] <- 0.6

#Hunters
w["elk/deer","hunters"] <- -1
w["carcasses","hunters"] <- 1

#Mountain Lions
w["elk/deer","mountain lions"] <- -0.3 #Variable interaction detailed above in FCM function
#w["small carnivores","mountain lions"] <- 0.35
#w["scavenging birds","mountain lions"] <- 0.2
w["small mammals","mountain lions"] <- -0.15
w["coyotes","mountain lions"] <- -0.25
w["carcasses","mountain lions"] <- 0.333

#Coyotes
w["elk/deer","coyotes"] <- -0.25
w["small carnivores","coyotes"] <- -0.4
w["invertebrate herbivores","coyotes"] <- -0.05
w["small mammals","coyotes"] <- -0.25
w["snakes","coyotes"] <- -0.15
w["carcasses","coyotes"] <- -0.25

#Veg Crew
w["shrub","veg crew"] <- 0.5
w["exotic forb","veg crew"] <- -0.4
w["forb","veg crew"] <- 0.45
w["grass","veg crew"] <- 0.5
w["exotic grass","veg crew"] <- -0.3

#Black bear
w["shrub","black bear"] <- -0.01
w["small carnivores","black bear"] <- -0.1
w["forb","black bear"] <- -0.025
w["exotic forb","black bear"] <- -0.025
w["grass","black bear"] <- -0.01
w["small mammals","black bear"] <- -0.05
w["mountain lions","black bear"] <- -0.05
w["carcasses","black bear"] <- -0.05

#Exotic grass
w["shrub","exotic grass"] <- -0.25
w["exotic forb","exotic grass"] <- -0.25
w["elk/deer","exotic grass"] <- 0.2
w["invertebrate herbivores","exotic grass"] <- 0.1
w["invertebrate predators","exotic grass"] <- 0
w["forb","exotic grass"] <- -0.55
w["grass","exotic grass"] <- -0.2
w["small mammals","exotic grass"] <- -0.4
w["disturbance","exotic grass"] <- -0.5

#Snakes
w["small carnivores","snakes"] <- 0.15
w["invertebrate herbivores","snakes"] <- -0.25
w["invertebrate predators","snakes"] <- -0.25
w["song birds","snakes"] <- -0.25
w["scavenging birds","snakes"] <- 0.1
w["small mammals","snakes"] <- -0.25
w["coyotes","snakes"] <- 0.1

#Pollinators
w["shrub","pollinators"] <- 0.9
w["exotic forb","pollinators"] <- 0.62
w["invertebrate predators","pollinators"] <- 0.4
w["forb","pollinators"] <- 0.97
w["song birds","pollinators"] <- 0.5

##Grizzlies
w["elk/deer","grizzlies"] <- -0.05 #Variable interaction detailed above in FCM function
w["mountain lions","grizzlies"] <- -0.025
w["black bear","grizzlies"] <- -0.075
w["coyotes","grizzlies"] <- -0.05
w["carcasses","grizzlies"] <- -0.1 #Variable interaction detailed above in FCM function
w["shrub","grizzlies"] <- 0.01
w["forb","grizzlies"] <- -0.01
w["exotic forb","grizzlies"] <- -0.01
w["grass","grizzlies"] <- -0.005
w["wolves","grizzlies"] <- -0.025
w["invertebrate herbivores","grizzlies"] <- -0.01


##Carcasses
w["grizzlies","carcasses"] <-  0.2 #Variable interaction detailed above in FCM function
w["small carnivores","carcasses"] <-  0.2
w["coyotes","carcasses"] <-  0.25
w["black bear","carcasses"] <-  0.2
w["scavenging birds","carcasses"] <-  0.4
w["wolves","carcasses"] <-  0.1


##Wolves
w["elk/deer","wolves"] <- -0.3
w["mountain lions","wolves"] <- -0.025
w["small carnivores","wolves"] <- -0.05
w["coyotes","wolves"] <- -0.15
w["carcasses","wolves"] <- 0.333
w["small mammals","wolves"] <- -0.05
w["grizzlies","wolves"] <- -0.01


# null model run 
s<- c(rep(0.5,24))
##Include precipitation, invasive management, and hunting as fixed
fix<- c(0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,1,0,0,0,0,0,0,0)

af <- c(2,1,2,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,2)

test<- fcm(s,w,tol=0.001,lambda=0.5,c=1,fix=fix,af=af)
colnames(test$state) <- c("shrub","precipitation","exotic forb","elk/deer","small carnivores",
                          "invertebrate herbivores","invertebrate predators","forb","song birds",
                          "grass","scavenging birds","small mammals","disturbance","hunters",
                          "mountain lions","coyotes","veg crew","black bear",
                          "exotic grass","snakes","pollinators","grizzlies","carcasses","wolves")

n<- test$iter
test.state <- as.data.frame(test$state)
test.state$iter <- rownames(test.state)

##Format data from for ggplot
library(tidyr)
test.spread <- pivot_longer(test.state, cols = !iter, names_to = "Species")
test.spread$iter <- as.numeric(test.spread$iter)
library(ggplot2)
ggplot()+
  geom_point(data= test.spread, aes(iter, value, colour = Species))+
  geom_line(data= test.spread, aes(iter, value, colour = Species))+
  theme_classic()


#########################################################################
#### Step 2) Build Membership Functions


# 1) Shrub: 
#exponential function
# based off of percent cover
high<- matrix(cbind(c(3.6,23.5,43),c(0,1,1)),ncol=2)
mod<- matrix(cbind(c(0,4,43),c(0,1,0)),ncol=2)
low<- matrix(cbind(c(0,2,7.9),c(1,1,0)),ncol=2)

shrub.mf<- list(high=high,mod=mod,low=low)

# 2) Precipitation: 
#logistic function
# based off of mm/year
high<- matrix(cbind(c(11.3,12.8,14),c(0,1,1)),ncol=2)
mod<- matrix(cbind(c(9.2,11.5,14),c(0,1,0)),ncol=2)
low<- matrix(cbind(c(9.2,10.3,11.8),c(1,1,0)),ncol=2)

precipitation.mf<- list(high=high,mod=mod,low=low)

# 3) Exotic forb: 
#exponential function
# based off of percent cover
high<- matrix(cbind(c(10.7,26.5,41),c(0,1,1)),ncol=2)
mod<- matrix(cbind(c(0,12,41),c(0,1,0)),ncol=2)
low<- matrix(cbind(c(0,6,14.9),c(1,1,0)),ncol=2)

exotic.forb.mf<- list(high=high,mod=mod,low=low)

# 4) Elk/deer: 
#exponential function
# based off of individual counts
high<- matrix(cbind(c(910.5,1214,1517.5),c(0,1,1)),ncol=2)
mod<- matrix(cbind(c(242.8,971.2,1517.5),c(0,1,0)),ncol=2)
low<- matrix(cbind(c(242.8,789.1,1153.3),c(1,1,0)),ncol=2)

elk.deer.mf<- list(high=high,mod=mod,low=low)

# 5) small carnivores: 
#exponential function
# based off of individual counts
high<- matrix(cbind(c(43,60,75),c(0,1,1)),ncol=2)
mod<- matrix(cbind(c(25,45,75),c(0,1,0)),ncol=2)
low<- matrix(cbind(c(25,35,48),c(1,1,0)),ncol=2)

small.carnivores.mf<- list(high=high,mod=mod,low=low)

# 6) Invertebrate Herbivores: 
#exponential function
# based off of density
high<- matrix(cbind(c(12.9,24.5,35),c(0,1,1)),ncol=2)
mod<- matrix(cbind(c(4,14,35),c(0,1,0)),ncol=2)
low<- matrix(cbind(c(4,9,16.1),c(1,1,0)),ncol=2)

invertebrate.herbivores.mf<- list(high=high,mod=mod,low=low)

# 7) Invertebrate Predators: 
#exponential function
# based off of density
high<- matrix(cbind(c(4.5,12.5,20),c(0,1,1)),ncol=2)
mod<- matrix(cbind(c(0,5,20),c(0,1,0)),ncol=2)
low<- matrix(cbind(c(0,2.5,6.5),c(1,1,0)),ncol=2)

invertebrate.predators.mf<- list(high=high,mod=mod,low=low)

# 8) Forb: 
#exponential function
# based off of percent cover
high<- matrix(cbind(c(14.3,33,50),c(0,1,1)),ncol=2)
mod<- matrix(cbind(c(0,16,50),c(0,1,0)),ncol=2)
low<- matrix(cbind(c(0,8,19.4),c(1,1,0)),ncol=2)

forb.mf<- list(high=high,mod=mod,low=low)

# 9) Song Birds: 
#exponential function
# based off of density
high<- matrix(cbind(c(5.6,12,18),c(0,1,1)),ncol=2)
mod<- matrix(cbind(c(2,6,18),c(0,1,0)),ncol=2)
low<- matrix(cbind(c(2,4,7.2),c(1,1,0)),ncol=2)

song.birds.mf<- list(high=high,mod=mod,low=low)

# 10) Grass: 
#exponential function
# based off of percent cover
high<- matrix(cbind(c(22.4,44,63),c(0,1,1)),ncol=2)
mod<- matrix(cbind(c(0,25,63),c(0,1,0)),ncol=2)
low<- matrix(cbind(c(0,12.5,28.8),c(1,1,0)),ncol=2)

grass.mf<- list(high=high,mod=mod,low=low)

# 11) scavenging birds: 
#exponential function
# based off of density
high<- matrix(cbind(c(.89,2.5,4),c(0,1,1)),ncol=2)
mod<- matrix(cbind(c(0,1,4),c(0,1,0)),ncol=2)
low<- matrix(cbind(c(0,0.5,1.3),c(1,1,0)),ncol=2)

scavenging.birds.mf<- list(high=high,mod=mod,low=low)

# 12) Small Mammals: 
#exponential function
# based off of density
high<- matrix(cbind(c(17.9,40,60),c(0,1,1)),ncol=2)
mod<- matrix(cbind(c(0,20,60),c(0,1,0)),ncol=2)
low<- matrix(cbind(c(0,10,24),c(1,1,0)),ncol=2)

small.mammals.mf<- list(high=high,mod=mod,low=low)

# 13) Disturbance: 
#logistic function
# based off of individuals
high<- matrix(cbind(c(5.4,20.5,35),c(0,1,1)),ncol=2)
mod<- matrix(cbind(c(0,6,35),c(0,1,0)),ncol=2)
low<- matrix(cbind(c(0,3,8.9),c(1,1,0)),ncol=2)

disturbance.mf<- list(high=high,mod=mod,low=low)

# 14) Hunters: 
#exponential function
# based off of individuals
high<- matrix(cbind(c(46.7,86,120),c(0,1,1)),ncol=2)
mod<- matrix(cbind(c(0,52,120),c(0,1,0)),ncol=2)
low<- matrix(cbind(c(0,36,58.8),c(1,1,0)),ncol=2)

hunters.mf<- list(high=high,mod=mod,low=low)

# 15) Mountain Lions: 
#exponential function
# based off of individuals
high<- matrix(cbind(c(2.4,3.2,4.1),c(0,1,1)),ncol=2)
mod<- matrix(cbind(c(1.2,2.6,4.1),c(0,1,0)),ncol=2)
low<- matrix(cbind(c(1.2,1.9,2.7),c(1,1,0)),ncol=2)

mountain.lions.mf<- list(high=high,mod=mod,low=low)

# 16) Coyotes: 
#exponential function
# based off of individuals
high<- matrix(cbind(c(14.6,19.4,24.6),c(0,1,1)),ncol=2)
mod<- matrix(cbind(c(3,16.5,24.6),c(0,1,0)),ncol=2)
low<- matrix(cbind(c(3,10.9,18.2),c(1,1,0)),ncol=2)

coyotes.mf<- list(high=high,mod=mod,low=low)

# 17) Veg Crew: 
#exponential function
# based off of individuals
high<- matrix(cbind(c(12,18,24),c(0,1,1)),ncol=2)
mod<- matrix(cbind(c(8,12,24),c(0,1,0)),ncol=2)
low<- matrix(cbind(c(8,10,13),c(1,1,0)),ncol=2)

veg.crew.mf<- list(high=high,mod=mod,low=low)

# 18) Black bear: 
#exponential function
# based off of individuals
high<- matrix(cbind(c(9.7,15.8,20.4),c(0,1,1)),ncol=2)
mod<- matrix(cbind(c(3.6,10.9,20.4),c(0,1,0)),ncol=2)
low<- matrix(cbind(c(3.6,7.2,12.4),c(1,1,0)),ncol=2)

black.bear.mf<- list(high=high,mod=mod,low=low)

# 19) Exotic grass: 
#exponential function
# based off of percent cover
high<- matrix(cbind(c(3.6,23.5,43),c(0,1,1)),ncol=2)
mod<- matrix(cbind(c(0,4,43),c(0,1,0)),ncol=2)
low<- matrix(cbind(c(0,2,7.9),c(1,1,0)),ncol=2)

exotic.grass.mf<- list(high=high,mod=mod,low=low)

# 20) Snakes: 
#exponential function
# based off of individuals
high<- matrix(cbind(c(14.3,43,70),c(0,1,1)),ncol=2)
mod<- matrix(cbind(c(0,16,70),c(0,1,0)),ncol=2)
low<- matrix(cbind(c(0,8,21.4),c(1,1,0)),ncol=2)

snakes.mf<- list(high=high,mod=mod,low=low)

# 21) Pollinators: 
#exponential function
# based off of density
high<- matrix(cbind(c(8.4,12.5,16),c(0,1,1)),ncol=2)
mod<- matrix(cbind(c(3,9,16),c(0,1,0)),ncol=2)
low<- matrix(cbind(c(3,6,9.7),c(1,1,0)),ncol=2)

pollinators.mf<- list(high=high,mod=mod,low=low)

# 22) Grizzlies: 
#exponential function
#based off of density per 1000 km^2 (max 350)
#for number of individuals in park ()
high <- matrix(cbind(c(0.6,1.1,1.6),c(0,1,1)), ncol= 2)
mod <- matrix(cbind(c(0,0.8,1.6),c(0,1,0)), ncol= 2)
low <- matrix(cbind(c(0,0.5,1),c(1,1,0)), ncol= 2)

grizzlies.mf <- list(high=high,mod=mod,low=low)

# 23) Carcasses
#exponential function
#individuals over ranch- ~59 from hunters, 20 from lions (interaction ratio), 20 from wolves
high <- matrix(cbind(c(100,110,120),c(0,1,1)), ncol = 2)
mod <- matrix(cbind(c(90,100,110),c(0,1,0)), ncol = 2)
low <- matrix(cbind(c(80,90,100),c(1,1,0)), ncol = 2)

carcasses.mf <- list(high=high,mod=mod,low=low)

# 23) Wolves
#exponential function
#individuals on ranch- estimated based on density in region
high <- matrix(cbind(c(1.9,2.8,3.7),c(0,1,1)), ncol = 2)
mod <- matrix(cbind(c(0.4,2.1,3.7),c(0,1,0)), ncol = 2)
low <- matrix(cbind(c(0.4,1.5,2.4),c(1,1,0)), ncol = 2)

wolves.mf <- list(high=high,mod=mod,low=low)


#######################################################################
#### Step 3: RUN FCM with introduction of grizzly bears


# set up data.frame
result <- data.frame(matrix(NA,11,24))
grizzlies <- seq(0,1,0.1)
result[,22] <- grizzlies
colnames(result) <- c("shrub","precipitation","exotic forb","elk/deer","small carnivores",
                      "invertebrate herbivores","invertebrate predators","forb","song birds",
                      "grass","scavenging birds","small mammals","disturbance","hunters",
                      "mountain lions","coyotes","veg crew","black bear",
                      "exotic grass","snakes","pollinators","grizzlies","carcasses","wolves")

# determine starting values via fuzzify, then defuzzify
# create a starting value fxn
start_fxn <- function(mf,start){
  start.fuzz <- fuzzify(mf, start, rescale=F, # rescale MUST BE FALSE
                        min=0,max=mf[[1]][3,1],
                        mean=mf[[2]][2,1])
  start.val <- defuzzify(mf,start.fuzz,rescale=T,
                         min=0,max=mf[[1]][3,1],
                         mean=mf[[2]][2,1])
  start.val[5]
}

##Alter second input varible based on starting value
shrub.s <- start_fxn(shrub.mf,3.9)
precipitation.s <- start_fxn(precipitation.mf,12)
exotic.forb.s <- start_fxn(exotic.forb.mf,11.9)
elk.deer.s <- start_fxn(elk.deer.mf,827)
small.carnivores.s <- start_fxn(small.carnivores.mf,44)
invertebrate.herbivores.s <- start_fxn(invertebrate.herbivores.mf,13.88)
invertebrate.predators.s <- start_fxn(invertebrate.predators.mf,4.99)
forb.s <- start_fxn(forb.mf, 15.8)
song.birds.s <- start_fxn(song.birds.mf, 6.81)
grass.s <- start_fxn(grass.mf,25.3)
scavenging.birds.s <- start_fxn(scavenging.birds.mf,0.99)
small.mammals.s <- start_fxn(small.mammals.mf,20.34)
disturbance.s <- start_fxn(disturbance.mf, 6)
hunters.s <- start_fxn(hunters.mf,28.5)
mountain.lions.s <- start_fxn(mountain.lions.mf,3)
coyotes.s <- start_fxn(coyotes.mf,6)
veg.crew.s <- start_fxn(veg.crew.mf, 12)
black.bear.s <- start_fxn(black.bear.mf,12)
exotic.grass.s <- start_fxn(exotic.grass.mf,16)
snakes.s <- start_fxn(snakes.mf,20)
pollinators.s <- start_fxn(pollinators.mf,9.35)
#grizzlies.s <- start_fxn(grizzlies.mf,1)
carcasses.s <- start_fxn(carcasses.mf,99)
wolves.s <- start_fxn(wolves.mf, 2)

# Fuzzy Model Runs

#trace variable interaction values
full.int <- matrix(ncol=7)
colnames(full.int) <- c("elk.ml","elk.griz","carc.griz","griz.elk","griz.carc","iter","cond")


for(m in 1:nrow(result)){
  
  s <- c(shrub.s, precipitation.s, exotic.forb.s, elk.deer.s, small.carnivores.s,
         invertebrate.herbivores.s, invertebrate.predators.s, forb.s, song.birds.s,
         grass.s, scavenging.birds.s, small.mammals.s, disturbance.s, hunters.s,
         mountain.lions.s, coyotes.s, veg.crew.s, black.bear.s,
         exotic.grass.s, snakes.s, pollinators.s,result[m,22],carcasses.s,wolves.s)
  
  
  ##Precipitation, veg crew, and hunter as fixed
  fix<- c(0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,1,0,0,0,0,1,0,0)
  af <- c(2,1,2,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,2)
  
  test<- fcm(s,w,tol=0.001,lambda=0.5,c=1,fix=fix,af=af)
  
  result[m,1] <- test$state[test$iter,1] #shrub
  result[m,2] <- test$state[test$iter,2] #prec
  result[m,3] <- test$state[test$iter,3] #ex forb
  result[m,4] <- test$state[test$iter,4] #elk deer
  result[m,5] <- test$state[test$iter,5] #meso
  result[m,6] <- test$state[test$iter,6] #inv herb
  result[m,7] <- test$state[test$iter,7] #inv pred
  result[m,8] <- test$state[test$iter,8] #forb
  result[m,9] <- test$state[test$iter,9] #song birds
  result[m,10] <- test$state[test$iter,10] #grass
  result[m,11] <- test$state[test$iter,11] #scavenging birds
  result[m,12] <- test$state[test$iter,12] #small mam
  result[m,13] <- test$state[test$iter,13] #dist
  result[m,14] <- test$state[test$iter,14] #hunters
  result[m,15] <- test$state[test$iter,15] #lions
  result[m,16] <- test$state[test$iter,16] #coyotes
  result[m,17] <- test$state[test$iter,17] #veg crew
  result[m,18] <- test$state[test$iter,18] #black bear
  result[m,19] <- test$state[test$iter,19] #ex grass
  result[m,20] <- test$state[test$iter,20] #snake
  result[m,21] <- test$state[test$iter,21] #pollinators
  #result[m,22] <- test$state[test$iter,22] #grizzlies
  result[m,23] <- test$state[test$iter,23] #carcasses
  result[m,24] <- test$state[test$iter,24] #wolves
  
  ##Trace interaction values through iterations
  int <- test$interact
  int$iter <- rownames(int)
  int$cond <- test$state[1,22]
  full.int <- rbind(full.int,int)
}
#View(result)



##Format data from for ggplot
library(tidyr)
result.spread <- pivot_longer(result, cols = !grizzlies, names_to = "Species")

library(ggplot2)
ggplot()+
  geom_point(data= result.spread, aes(grizzlies, value, colour = Species))+
  geom_line(data= result.spread, aes(grizzlies, value, colour = Species))+
  theme_classic()

##Wrap function to look at each trend individually 
ggplot()+
  geom_point(data= result.spread, aes(grizzlies, value, colour = Species))+
  geom_line(data= result.spread, aes(grizzlies, value, colour = Species))+
  theme_classic()+
  facet_wrap(~Species, scales = "free_y")+
  theme(legend.position = "none")


##Plot variable interaction values
full.int <- na.omit(full.int)
full.int$iter <- as.numeric(full.int$iter)
int.spread <- pivot_longer(full.int, cols = 1:5, names_to = "Interaction")
int.spread$int.cond <- paste(int.spread$cond,int.spread$Interaction,sep = "_")
ggplot()+
  geom_point(data=int.spread, aes(iter, value, colour=int.cond))+
  geom_line(data =int.spread, aes(iter, value, colour=int.cond))+
  facet_grid(rows= vars(Interaction), cols = vars(cond), scales = "free_y")+
  theme(legend.position = "none")

str(int.spread)



#################################################################################

#### SENSITIVITY/UNCERTAINTY ANALYSIS 

# first scenario: variation in direct effect of grizzlies on black bears, mountain lions, coyotes
# intraguild killing of black bears and coyotes, kleptoparasitism of mountain lions
# also assess effect on scavenging birds, small carnivores, wolves, and elk/deer
# setting other effects of grizzlies to 0

fcm.dir<- function(s,w,fix=rep(0,length(s)),
                    lambda=0.5, max.iter=500,
                    af=rep(1,length(s)), tol=0.001,c=5) {
  #
  # Fuzzy cognitive map algorithm
  # s is the initial state vector variable [0,1]
  # w is the matrix of edge weights [-1,1]
  # fix is the vector indicating which element of s are fixed boundary conditions
  # lambda is the relaxation parameter to speed convergence
  # activation functions are (1)logistic, (2) threshold exponential or (3) threshold linear
  # af - vector specifying which activation function to be used on each element of s
  #
  # D. Ramsey 30/4/04
  #
  
  state<- matrix(nrow=max.iter,ncol=length(s))
  raw<- matrix(nrow=max.iter,ncol=length(s))
  ##Set based on number of interaction effects
  interact <- matrix(nrow=max.iter, ncol = 5)
  state[1,]<- s
  raw[1,]<- s
  colnames(interact) <- c("elk.ml","elk.griz","carc.griz","griz.elk","griz.carc")
  
  iter<- 1
  
  for(z in 2:max.iter) {
    
    ##Inclusion of iterative interaction effects
    elk.ml <- -0.3 + (-0.05*state[z-1,22]) + (-0.05*state[z-1,18])
    w["elk/deer","mountain lions"] <- elk.ml
    interact[z-1,1] <- elk.ml
    
    #elk.griz <- -0.05 + (-0.05*state[z-1,16])
    #w["elk/deer","grizzlies"] <- elk.griz
    #interact[z-1,2] <- elk.griz
    
    #carc.griz <- -0.1 + (0.05*state[z-1,16]) 
    #w["carcasses","grizzlies"] <- carc.griz
    #interact[z-1,3] <- carc.griz
    
    griz.elk <- 0.1 + (0.1*state[z-1,16])
    w["grizzlies","elk/deer"] <- griz.elk
    interact[z-1,4] <- griz.elk
    
    griz.carc <- 0.2 + (-0.1*state[z-1,16]) 
    w["grizzlies","carcasses"] <- griz.carc
    interact[z-1,5] <- griz.carc
    
    tmp<- w %*% state[z-1,]
    for(i in 1:nrow(w)) {
      
      
      if(af[i]==1){
        # logistic
        state[z,i] <- lambda* (1/(1+exp(-c*tmp[i]))) + (1-lambda)*state[z-1,i]
        raw[z,i]<- tmp[i]
      }
      else if(af[i]==2) {
        # exponential
        state[z,i]<- lambda * max(0,1-exp(-c*tmp[i])) + (1-lambda)*state[z-1,i]
        raw[z,i]<- tmp[i]
      }
      else if(af[i]==3){
        # linear
        state[z,i] <- lambda* (min(1,max(0,c*tmp[i]))) + (1-lambda)*state[z-1,i]
        raw[z,i]<- tmp[i]
      }
      
    }
    state[z,fix==1]<- state[1,fix==1] # reset boundary conditions (control)
    
    
    ind<- abs(state[z,] - state[z-1,]) < tol
    
    if(length(state[z,ind])==length(s)) break
    iter<- iter + 1
  }
  
  if(iter == max.iter) cat(" WARNING ! Convergence not reached in ",max.iter," iterations",'\n')
  else cat("convergence reached after ",iter," iterations",'\n')
  
  interact <- as.data.frame(na.omit(interact))
  list(state=state[1:iter,],raw=raw[1:iter,],iter=iter, interact=interact)
  
}



sims <- 10000 # 10000

dir.uncert.ml <- rep(NA,sims)
dir.uncert.bb <- rep(NA,sims)
dir.uncert.co <- rep(NA,sims)
dir.uncert.rp <- rep(NA,sims)
dir.uncert.mp <- rep(NA,sims)
dir.uncert.ug <- rep(NA,sims)
dir.uncert.wf <- rep(NA,sims)

for (i in 1:sims){
  
  w <- matrix(0,nrow=24,ncol=24)
  species <- c("shrub","precipitation","exotic forb","elk/deer","small carnivores",
               "invertebrate herbivores","invertebrate predators","forb","song birds",
               "grass","scavenging birds","small mammals","disturbance","hunters",
               "mountain lions","coyotes","veg crew","black bear",
               "exotic grass","snakes","pollinators","grizzlies","carcasses","wolves")
  dimnames(w) <- list(species,species)
  
  ##Parameterize interactions
  
  
  ##Shrub
  w["elk/deer","shrub"] <- 0.45
  w["small carnivores","shrub"] <- 0.2
  w["invertebrate herbivores","shrub"] <- 0.9
  w["invertebrate predators","shrub"] <- 0.3
  w["song birds","shrub"] <- 0.5
  w["small mammals","shrub"] <- 0.5
  w["disturbance","shrub"] <- -0.25
  w["coyotes","shrub"] <- 0.15
  w["black bear","shrub"] <- 0.25
  w["exotic grass","shrub"] <- 0.2
  w["pollinators","shrub"] <- 0.75
  w["grizzlies","shrub"] <- 0.1
  
  ##Precipitation
  w["shrub","precipitation"] <- 0.7
  w["exotic forb","precipitation"] <- 0.95
  w["forb","precipitation"] <- 1
  w["grass","precipitation"] <- 1
  w["exotic grass","precipitation"] <- 0.97
  
  #Exotic forb
  w["shrub","exotic forb"] <- -0.1
  w["elk/deer","exotic forb"] <- 0.5
  w["invertebrate herbivores","exotic forb"] <- 0.25
  w["invertebrate predators","exotic forb"] <- 0.2
  w["forb","exotic forb"] <- -0.25
  w["grass","exotic forb"] <- -0.2
  w["small mammals","exotic forb"] <- 0.4
  w["disturbance","exotic forb"] <- -0.5
  w["exotic grass","exotic forb"] <- -0.2
  w["pollinators","exotic forb"] <- 0.28
  w["grizzlies","exotic forb"] <- 0.15
  
  #Elk/deer
  w["shrub","elk/deer"] <- -0.27
  w["exotic forb","elk/deer"] <- -0.15
  w["forb","elk/deer"] <- -0.22
  w["grass","elk/deer"] <- -0.35
  w["disturbance","elk/deer"] <- 0.75
  w["mountain lions","elk/deer"] <- 0.85
  w["coyotes","elk/deer"] <- 0.25
  w["exotic grass","elk/deer"] <- -0.05
  w["grizzlies","elk/deer"] <- 0.1 #Variable interaction detailed above in FCM function
  w["wolves","elk/deer"] <- 0.95
  
  #small carnivores
  w["invertebrate herbivores","small carnivores"] <- -0.1
  w["song birds","small carnivores"] <- -0.45
  w["scavenging birds","small carnivores"] <- 0.1
  w["small mammals","small carnivores"] <- -0.5
  w["disturbance","small carnivores"] <- 0.2
  w["snakes","small carnivores"] <- -0.3
  w["carcasses","small carnivores"] <- -0.05
  
  #Invertebrate herbivores
  w["shrub","invertebrate herbivores"] <- -0.2
  w["small carnivores","invertebrate herbivores"] <- 0.5
  w["invertebrate predators","invertebrate herbivores"] <- 0.85
  w["forb","invertebrate herbivores"] <- -0.27
  w["song birds","invertebrate herbivores"] <- 0.6
  w["grass","invertebrate herbivores"] <- -0.15
  w["small mammals","invertebrate herbivores"] <- 0.67
  w["coyotes","invertebrate herbivores"] <- 0.2
  w["black bear","invertebrate herbivores"] <- 0.1
  w["snakes","invertebrate herbivores"] <- 0.5
  w["grizzlies","invertebrate herbivores"] <- 0.05
  
  #Invertebrate predators
  w["invertebrate herbivores","invertebrate predators"] <- -0.5
  w["song birds","invertebrate predators"] <- 0.6
  w["small mammals","invertebrate predators"] <- 0.5
  w["snakes","invertebrate predators"] <- 0.35
  w["pollinators","invertebrate predators"] <- -0.25
  
  #Forb
  w["exotic forb","forb"] <- -0.4
  w["elk/deer","forb"] <- 0.55
  w["invertebrate herbivores","forb"] <- 0.9
  w["invertebrate predators","forb"] <- 0.3
  w["song birds","forb"] <- 0.25
  w["grass","forb"] <- -0.1
  w["small mammals","forb"] <- 0.75
  w["disturbance","forb"] <- -0.25
  w["black bear","forb"] <- 0.35
  w["exotic grass","forb"] <- -0.3
  w["pollinators","forb"] <- 1
  w["grizzlies","forb"] <- 0.15
  
  #Song birds
  w["shrub","song birds"] <- -0.1
  w["small carnivores","song birds"] <- 0.25
  w["invertebrate herbivores","song birds"] <- -0.3
  w["invertebrate predators","song birds"] <- -0.3
  w["forb","song birds"] <- -0.05
  w["scavenging birds","song birds"] <- 0.2
  w["snakes","song birds"] <- 0.45
  w["pollinators","song birds"] <- -0.25
  
  #Grass
  w["exotic forb","grass"] <- -0.55
  w["elk/deer","grass"] <- 0.95
  w["invertebrate herbivores","grass"] <- 0.8
  w["invertebrate predators","grass"] <- 0.2
  w["forb","grass"] <- -0.4
  w["song birds","grass"] <- 0.25
  w["small mammals","grass"] <- 0.65
  w["disturbance","grass"] <- -0.25
  w["black bear","grass"] <- 0.3
  w["exotic grass","grass"] <- -0.5
  w["grizzlies","grass"] <- 0.4
  
  #scavenging birds
  w["small carnivores","scavenging birds"] <- -0.25
  w["invertebrate herbivores","scavenging birds"] <- -0.1
  w["song birds","scavenging birds"] <- -0.25
  w["small mammals","scavenging birds"] <- -0.5
  w["snakes","scavenging birds"] <- -0.35
  w["carcasses","scavenging birds"] <- -0.75
  
  #Small mammals
  w["shrub","small mammals"] <- -0.5
  w["small carnivores","small mammals"] <- 0.5
  w["invertebrate herbivores","small mammals"] <- -0.25
  w["invertebrate predators","small mammals"] <- -0.3
  w["forb","small mammals"] <- -0.2
  w["song birds","small mammals"] <- -0.25
  w["grass","small mammals"] <- -0.1
  w["scavenging birds","small mammals"] <- 0.8
  w["disturbance","small mammals"] <- 0.75
  w["mountain lions","small mammals"] <- 0.25
  w["coyotes","small mammals"] <- 0.75
  w["black bear","small mammals"] <- 0.1
  w["snakes","small mammals"] <- 0.85
  w["wolves","small mammals"] <- 0.1
  
  #Disturbance
  w["exotic forb","disturbance"] <- 0.57
  w["forb","disturbance"] <- -0.1
  w["grass","disturbance"] <- 0
  w["exotic grass","disturbance"] <- 0.6
  
  #Hunters
  w["elk/deer","hunters"] <- -1
  w["carcasses","hunters"] <- 1
  
  #Mountain Lions
  w["elk/deer","mountain lions"] <- -0.3 #Variable interaction detailed above in FCM function
  #w["small carnivores","mountain lions"] <- 0.35
  #w["scavenging birds","mountain lions"] <- 0.2
  w["small mammals","mountain lions"] <- -0.15
  w["coyotes","mountain lions"] <- -0.25
  w["carcasses","mountain lions"] <- 0.333
  
  #Coyotes
  w["elk/deer","coyotes"] <- -0.25
  w["small carnivores","coyotes"] <- -0.4
  w["invertebrate herbivores","coyotes"] <- -0.05
  w["small mammals","coyotes"] <- -0.25
  w["snakes","coyotes"] <- -0.15
  w["carcasses","coyotes"] <- -0.25
  
  #Veg Crew
  w["shrub","veg crew"] <- 0.5
  w["exotic forb","veg crew"] <- -0.4
  w["forb","veg crew"] <- 0.45
  w["grass","veg crew"] <- 0.5
  w["exotic grass","veg crew"] <- -0.3
  
  #Black bear
  w["shrub","black bear"] <- -0.01
  w["small carnivores","black bear"] <- -0.1
  w["forb","black bear"] <- -0.025
  w["exotic forb","black bear"] <- -0.025
  w["grass","black bear"] <- -0.01
  w["small mammals","black bear"] <- -0.05
  w["mountain lions","black bear"] <- -0.05
  w["carcasses","black bear"] <- -0.05
  
  #Exotic grass
  w["shrub","exotic grass"] <- -0.25
  w["exotic forb","exotic grass"] <- -0.25
  w["elk/deer","exotic grass"] <- 0.2
  w["invertebrate herbivores","exotic grass"] <- 0.1
  w["invertebrate predators","exotic grass"] <- 0
  w["forb","exotic grass"] <- -0.55
  w["grass","exotic grass"] <- -0.2
  w["small mammals","exotic grass"] <- -0.4
  w["disturbance","exotic grass"] <- -0.5
  
  #Snakes
  w["small carnivores","snakes"] <- 0.15
  w["invertebrate herbivores","snakes"] <- -0.25
  w["invertebrate predators","snakes"] <- -0.25
  w["song birds","snakes"] <- -0.25
  w["scavenging birds","snakes"] <- 0.1
  w["small mammals","snakes"] <- -0.25
  w["coyotes","snakes"] <- 0.1
  
  #Pollinators
  w["shrub","pollinators"] <- 0.9
  w["exotic forb","pollinators"] <- 0.62
  w["invertebrate predators","pollinators"] <- 0.4
  w["forb","pollinators"] <- 0.97
  w["song birds","pollinators"] <- 0.5
  
  ##Grizzlies
  #w["elk/deer","grizzlies"] <- -0.05 #Variable interaction detailed above in FCM function
  w["mountain lions","grizzlies"] <- runif(1,-0.05,0) #-0.025
  w["black bear","grizzlies"] <- runif(1,-0.15,0) #-0.075
  w["coyotes","grizzlies"] <- runif(1,-0.1,0) #-0.05
  #w["carcasses","grizzlies"] <- -0.1 #Variable interaction detailed above in FCM function
  #w["shrub","grizzlies"] <- 0.01
  #w["forb","grizzlies"] <- -0.01
  #w["exotic forb","grizzlies"] <- -0.01
  #w["grass","grizzlies"] <- -0.005 
  w["wolves","grizzlies"] <- runif(1,-0.05,0) #-0.025
  
  ##Carcasses
  w["grizzlies","carcasses"] <-  0.2 #Variable interaction detailed above in FCM function
  w["small carnivores","carcasses"] <-  0.2
  w["coyotes","carcasses"] <-  0.25
  w["black bear","carcasses"] <-  0.2
  w["scavenging birds","carcasses"] <-  0.4 
  w["wolves","carcasses"] <-  0.1
  
  ##Wolves
  w["elk/deer","wolves"] <- -0.3
  w["mountain lions","wolves"] <- -0.025
  w["small carnivores","wolves"] <- -0.05
  w["coyotes","wolves"] <- -0.15
  w["carcasses","wolves"] <- 0.333 
  w["small mammals","wolves"] <- -0.05 
  w["grizzlies","wolves"] <- -0.01
  
  ##Need membership functions from above
  
  ## RUN CODE
  
  result <- data.frame(matrix(NA,11,24))
  grizzlies <- seq(0,1,0.1)
  result[,22] <- grizzlies
  colnames(result) <- c("shrub","precipitation","exotic forb","elk/deer","small carnivores",
                        "invertebrate herbivores","invertebrate predators","forb","song birds",
                        "grass","scavenging birds","small mammals","disturbance","hunters",
                        "mountain lions","coyotes","veg crew","black bear",
                        "exotic grass","snakes","pollinators","grizzlies","carcasses","wolves")
  
  for(m in 1:nrow(result)){
    
    s <- c(shrub.s, precipitation.s, exotic.forb.s, elk.deer.s, small.carnivores.s,
           invertebrate.herbivores.s, invertebrate.predators.s, forb.s, song.birds.s,
           grass.s, scavenging.birds.s, small.mammals.s, disturbance.s, hunters.s,
           mountain.lions.s, coyotes.s, veg.crew.s, black.bear.s,
           exotic.grass.s, snakes.s, pollinators.s,result[m,22],carcasses.s,wolves.s)
  
    
    
    ##Precipitation, veg crew, and hunter as fixed
    fix<- c(0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,1,0,0,0,0,1,0,0)
    af <- c(2,1,2,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,2)
    
    test<- fcm.dir(s,w,tol=0.001,lambda=0.5,c=1,fix=fix,af=af)
    
    result[m,1] <- test$state[test$iter,1] #shrub
    result[m,2] <- test$state[test$iter,2] #prec
    result[m,3] <- test$state[test$iter,3] #ex forb
    result[m,4] <- test$state[test$iter,4] #elk deer
    result[m,5] <- test$state[test$iter,5] #meso
    result[m,6] <- test$state[test$iter,6] #inv herb
    result[m,7] <- test$state[test$iter,7] #inv pred
    result[m,8] <- test$state[test$iter,8] #forb
    result[m,9] <- test$state[test$iter,9] #song birds
    result[m,10] <- test$state[test$iter,10] #grass
    result[m,11] <- test$state[test$iter,11] #scavenging birds
    result[m,12] <- test$state[test$iter,12] #small mam
    result[m,13] <- test$state[test$iter,13] #dist
    result[m,14] <- test$state[test$iter,14] #hunters
    result[m,15] <- test$state[test$iter,15] #lions
    result[m,16] <- test$state[test$iter,16] #coyotes
    result[m,17] <- test$state[test$iter,17] #veg crew
    result[m,18] <- test$state[test$iter,18] #black bear
    result[m,19] <- test$state[test$iter,19] #ex grass
    result[m,20] <- test$state[test$iter,20] #snake
    result[m,21] <- test$state[test$iter,21] #pollinators
    #result[m,22] <- test$state[test$iter,22] #grizzlies
    result[m,23] <- test$state[test$iter,23] #carcasses
    result[m,24] <- test$state[test$iter,24] #wolves
    
  }
  
  
  # record results for species of interest
  dir.uncert.ml[i] <- (result$`mountain lions`[11]-result$`mountain lions`[1])/result$`mountain lions`[1]
  dir.uncert.bb[i] <- (result$`black bear`[11]-result$`black bear`[1])/result$`black bear`[1]
  dir.uncert.co[i] <- (result$coyotes[11]-result$coyotes[1])/result$coyotes[1]
  dir.uncert.rp[i] <- (result$`scavenging birds`[11]-result$`scavenging birds`[1])/result$`scavenging birds`[1]
  dir.uncert.mp[i] <- (result$`small carnivores`[11]-result$`small carnivores`[1])/result$`small carnivores`[1]
  dir.uncert.ug[i] <- (result$`elk/deer`[11]-result$`elk/deer`[1])/result$`elk/deer`[1]
  dir.uncert.wf[i] <- (result$`wolves`[11]-result$`wolves`[1])/result$`wolves`[1]
  print(i)
}

# black bear results 
mean(dir.uncert.bb) 
quantile(dir.uncert.bb, c(0.05,0.95)) 
dir.uncert.bb.df <- as.data.frame(dir.uncert.bb)
colnames(dir.uncert.bb.df) <- "uncert"
dir.uncert.bb.df$Species <- "Black bears"

# mountain lion results 
mean(dir.uncert.ml) 
quantile(dir.uncert.ml, c(0.05,0.95)) 
dir.uncert.ml.df <- as.data.frame(dir.uncert.ml)
colnames(dir.uncert.ml.df) <- "uncert"
dir.uncert.ml.df$Species <- "Mountain lions"

# coyote results 
mean(dir.uncert.co) 
quantile(dir.uncert.co, c(0.05,0.95)) 
dir.uncert.co.df <- as.data.frame(dir.uncert.co)
colnames(dir.uncert.co.df) <- "uncert"
dir.uncert.co.df$Species <- "Coyotes"

# scavenging bird results
mean(dir.uncert.rp) 
quantile(dir.uncert.rp, c(0.05,0.95)) 
dir.uncert.rp.df <- as.data.frame(dir.uncert.rp)
colnames(dir.uncert.rp.df) <- "uncert"
dir.uncert.rp.df$Species <- "Scavenging birds"

# mesopredator results
mean(dir.uncert.mp) 
quantile(dir.uncert.mp, c(0.05,0.95)) 
dir.uncert.mp.df <- as.data.frame(dir.uncert.mp)
colnames(dir.uncert.mp.df) <- "uncert"
dir.uncert.mp.df$Species <- "Small carnivores"

# elk/deer results
mean(dir.uncert.ug) 
quantile(dir.uncert.ug, c(0.05,0.95)) 
dir.uncert.ug.df <- as.data.frame(dir.uncert.ug)
colnames(dir.uncert.ug.df) <- "uncert"
dir.uncert.ug.df$Species <- "Elk/Deer"

# wolves results
mean(dir.uncert.wf) 
quantile(dir.uncert.wf, c(0.05,0.95)) 
dir.uncert.wf.df <- as.data.frame(dir.uncert.wf)
colnames(dir.uncert.wf.df) <- "uncert"
dir.uncert.wf.df$Species <- "Wolves"

dir.uncert.full <- rbind(dir.uncert.bb.df,dir.uncert.ml.df,dir.uncert.co.df,
                         dir.uncert.rp.df,dir.uncert.mp.df,dir.uncert.ug.df,
                         dir.uncert.wf.df)


dir <- ggplot()+
  geom_boxplot(data = dir.uncert.full, aes(uncert,Species,fill=Species))+
  xlab("")+
  ylab("")+
  ggtitle("b) Interference competition")+
  theme_classic()+
  geom_vline(xintercept = 0)+
  scale_fill_manual(values = c(alpha("firebrick3",0.9),alpha("darkgoldenrod",0.9),alpha("forestgreen",0.9),alpha("chocolate4",0.9),alpha("deepskyblue4",0.9),alpha("moccasin",0.9),alpha("darkgrey",0.9)))+
  xlim(-0.35,0.1)+
  annotate(geom = "text", y = 0.9, x= 0.05, label = "**", size = 6)+
  annotate(geom = "text", y = 1.9, x= 0.075, label = "**", size = 6)+
  annotate(geom = "text", y = 4.9, x= 0.1, label = "**", size = 6)+
  annotate(geom = "text", y = 6.9, x= 0.075, label = "**", size = 6)+
  theme(legend.position = "none")

dir.f <- ggplot()+
  geom_boxplot(data = dir.uncert.full, aes(uncert,Species,fill=Species))+
  xlab("")+
  ylab("")+
  ggtitle("b) Interference competition")+
  theme_classic()+
  geom_vline(xintercept = 0)+
  scale_fill_manual(values = c(alpha("firebrick3",0.9),alpha("darkgoldenrod",0.9),alpha("forestgreen",0.9),alpha("chocolate4",0.9),alpha("deepskyblue4",0.9),alpha("moccasin",0.9),alpha("darkgrey",0.9)))+
  #xlim(-0.35,0.1)+
  annotate(geom = "text", y = 0.9, x= 0.05, label = "**", size = 6)+
  annotate(geom = "text", y = 1.9, x= 0.075, label = "**", size = 6)+
  annotate(geom = "text", y = 4.9, x= 0.1, label = "**", size = 6)+
  annotate(geom = "text", y = 6.9, x= 0.075, label = "**", size = 6)+
  theme(legend.position = "none")


#############################################################################################
# second scenario: variation in effects of grizzlies via hunting of elk
# grizzly bear predation on elk 
# Setting other effects to zero
fcm.zero<- function(s,w,fix=rep(0,length(s)),
                    lambda=0.5, max.iter=500,
                    af=rep(1,length(s)), tol=0.001,c=5) {
  #
  # Fuzzy cognitive map algorithm
  # s is the initial state vector variable [0,1]
  # w is the matrix of edge weights [-1,1]
  # fix is the vector indicating which element of s are fixed boundary conditions
  # lambda is the relaxation parameter to speed convergence
  # activation functions are (1)logistic, (2) threshold exponential or (3) threshold linear
  # af - vector specifying which activation function to be used on each element of s
  #
  # D. Ramsey 30/4/04
  #
  
  state<- matrix(nrow=max.iter,ncol=length(s))
  raw<- matrix(nrow=max.iter,ncol=length(s))
  ##Set based on number of interaction effects
  interact <- matrix(nrow=max.iter, ncol = 5)
  state[1,]<- s
  raw[1,]<- s
  colnames(interact) <- c("elk.ml","elk.griz","carc.griz","griz.elk","griz.carc")
  
  iter<- 1
  
  for(z in 2:max.iter) {
    
    ##Inclusion of iterative interaction effects
    #elk.ml <- -0.3 + (-0.05*state[z-1,22]) + (-0.05*state[z-1,18])
    #w["elk/deer","mountain lions"] <- elk.ml
    #interact[z-1,1] <- elk.ml
    
    #elk.griz <- -0.05 + (-0.05*state[z-1,16])
    #w["elk/deer","grizzlies"] <- elk.griz
    #interact[z-1,2] <- elk.griz
    
    #carc.griz <- -0.1 + (0.05*state[z-1,16]) 
    #w["carcasses","grizzlies"] <- carc.griz
    #interact[z-1,3] <- carc.griz
    
    griz.elk <- 0.1 + (0.1*state[z-1,16])
    w["grizzlies","elk/deer"] <- griz.elk
    interact[z-1,4] <- griz.elk
    
    griz.carc <- 0.2 + (-0.1*state[z-1,16]) 
    w["grizzlies","carcasses"] <- griz.carc
    interact[z-1,5] <- griz.carc
    
    tmp<- w %*% state[z-1,]
    for(i in 1:nrow(w)) {
      
      
      if(af[i]==1){
        # logistic
        state[z,i] <- lambda* (1/(1+exp(-c*tmp[i]))) + (1-lambda)*state[z-1,i]
        raw[z,i]<- tmp[i]
      }
      else if(af[i]==2) {
        # exponential
        state[z,i]<- lambda * max(0,1-exp(-c*tmp[i])) + (1-lambda)*state[z-1,i]
        raw[z,i]<- tmp[i]
      }
      else if(af[i]==3){
        # linear
        state[z,i] <- lambda* (min(1,max(0,c*tmp[i]))) + (1-lambda)*state[z-1,i]
        raw[z,i]<- tmp[i]
      }
      
    }
    state[z,fix==1]<- state[1,fix==1] # reset boundary conditions (control)
    
    
    ind<- abs(state[z,] - state[z-1,]) < tol
    
    if(length(state[z,ind])==length(s)) break
    iter<- iter + 1
  }
  
  if(iter == max.iter) cat(" WARNING ! Convergence not reached in ",max.iter," iterations",'\n')
  else cat("convergence reached after ",iter," iterations",'\n')
  
  interact <- as.data.frame(na.omit(interact))
  list(state=state[1:iter,],raw=raw[1:iter,],iter=iter, interact=interact)
  
}

sims <- 10000 # 10000

hunt.uncert.ml <- rep(NA,sims)
hunt.uncert.bb <- rep(NA,sims)
hunt.uncert.co <- rep(NA,sims)
hunt.uncert.rp <- rep(NA,sims)
hunt.uncert.mp <- rep(NA,sims)
hunt.uncert.ug <- rep(NA,sims)
hunt.uncert.wf <- rep(NA,sims)

for (i in 1:sims){
  
  w <- matrix(0,nrow=24,ncol=24)
  species <- c("shrub","precipitation","exotic forb","elk/deer","small carnivores",
               "invertebrate herbivores","invertebrate predators","forb","song birds",
               "grass","scavenging birds","small mammals","disturbance","hunters",
               "mountain lions","coyotes","veg crew","black bear",
               "exotic grass","snakes","pollinators","grizzlies","carcasses","wolves")
  dimnames(w) <- list(species,species)
  
  ##Parameterize interactions
  
  
  ##Shrub
  w["elk/deer","shrub"] <- 0.45
  w["small carnivores","shrub"] <- 0.2
  w["invertebrate herbivores","shrub"] <- 0.9
  w["invertebrate predators","shrub"] <- 0.3
  w["song birds","shrub"] <- 0.5
  w["small mammals","shrub"] <- 0.5
  w["disturbance","shrub"] <- -0.25
  w["coyotes","shrub"] <- 0.15
  w["black bear","shrub"] <- 0.25
  w["exotic grass","shrub"] <- 0.2
  w["pollinators","shrub"] <- 0.75
  w["grizzlies","shrub"] <- 0.1
  
  ##Precipitation
  w["shrub","precipitation"] <- 0.7
  w["exotic forb","precipitation"] <- 0.95
  w["forb","precipitation"] <- 1
  w["grass","precipitation"] <- 1
  w["exotic grass","precipitation"] <- 0.97
  
  #Exotic forb
  w["shrub","exotic forb"] <- -0.1
  w["elk/deer","exotic forb"] <- 0.5
  w["invertebrate herbivores","exotic forb"] <- 0.25
  w["invertebrate predators","exotic forb"] <- 0.2
  w["forb","exotic forb"] <- -0.25
  w["grass","exotic forb"] <- -0.2
  w["small mammals","exotic forb"] <- 0.4
  w["disturbance","exotic forb"] <- -0.5
  w["exotic grass","exotic forb"] <- -0.2
  w["pollinators","exotic forb"] <- 0.28
  w["grizzlies","exotic forb"] <- 0.15
  
  #Elk/deer
  w["shrub","elk/deer"] <- -0.27
  w["exotic forb","elk/deer"] <- -0.15
  w["forb","elk/deer"] <- -0.22
  w["grass","elk/deer"] <- -0.35
  w["disturbance","elk/deer"] <- 0.75
  w["mountain lions","elk/deer"] <- 0.85
  w["coyotes","elk/deer"] <- 0.25
  w["exotic grass","elk/deer"] <- -0.05
  w["grizzlies","elk/deer"] <- 0.1 #Variable interaction detailed above in FCM function
  w["wolves","elk/deer"] <- 0.95
  
  #small carnivores
  w["invertebrate herbivores","small carnivores"] <- -0.1
  w["song birds","small carnivores"] <- -0.45
  w["scavenging birds","small carnivores"] <- 0.1
  w["small mammals","small carnivores"] <- -0.5
  w["disturbance","small carnivores"] <- 0.2
  w["snakes","small carnivores"] <- -0.3
  w["carcasses","small carnivores"] <- -0.05
  
  #Invertebrate herbivores
  w["shrub","invertebrate herbivores"] <- -0.2
  w["small carnivores","invertebrate herbivores"] <- 0.5
  w["invertebrate predators","invertebrate herbivores"] <- 0.85
  w["forb","invertebrate herbivores"] <- -0.27
  w["song birds","invertebrate herbivores"] <- 0.6
  w["grass","invertebrate herbivores"] <- -0.15
  w["small mammals","invertebrate herbivores"] <- 0.67
  w["coyotes","invertebrate herbivores"] <- 0.2
  w["black bear","invertebrate herbivores"] <- 0.1
  w["snakes","invertebrate herbivores"] <- 0.5
  w["grizzlies","invertebrate herbivores"] <- 0.05
  
  #Invertebrate predators
  w["invertebrate herbivores","invertebrate predators"] <- -0.5
  w["song birds","invertebrate predators"] <- 0.6
  w["small mammals","invertebrate predators"] <- 0.5
  w["snakes","invertebrate predators"] <- 0.35
  w["pollinators","invertebrate predators"] <- -0.25
  
  #Forb
  w["exotic forb","forb"] <- -0.4
  w["elk/deer","forb"] <- 0.55
  w["invertebrate herbivores","forb"] <- 0.9
  w["invertebrate predators","forb"] <- 0.3
  w["song birds","forb"] <- 0.25
  w["grass","forb"] <- -0.1
  w["small mammals","forb"] <- 0.75
  w["disturbance","forb"] <- -0.25
  w["black bear","forb"] <- 0.35
  w["exotic grass","forb"] <- -0.3
  w["pollinators","forb"] <- 1
  w["grizzlies","forb"] <- 0.15
  
  #Song birds
  w["shrub","song birds"] <- -0.1
  w["small carnivores","song birds"] <- 0.25
  w["invertebrate herbivores","song birds"] <- -0.3
  w["invertebrate predators","song birds"] <- -0.3
  w["forb","song birds"] <- -0.05
  w["scavenging birds","song birds"] <- 0.2
  w["snakes","song birds"] <- 0.45
  w["pollinators","song birds"] <- -0.25
  
  #Grass
  w["exotic forb","grass"] <- -0.55
  w["elk/deer","grass"] <- 0.95
  w["invertebrate herbivores","grass"] <- 0.8
  w["invertebrate predators","grass"] <- 0.2
  w["forb","grass"] <- -0.4
  w["song birds","grass"] <- 0.25
  w["small mammals","grass"] <- 0.65
  w["disturbance","grass"] <- -0.25
  w["black bear","grass"] <- 0.3
  w["exotic grass","grass"] <- -0.5
  w["grizzlies","grass"] <- 0.4
  
  #scavenging birds
  w["small carnivores","scavenging birds"] <- -0.25
  w["invertebrate herbivores","scavenging birds"] <- -0.1
  w["song birds","scavenging birds"] <- -0.25
  w["small mammals","scavenging birds"] <- -0.5
  w["snakes","scavenging birds"] <- -0.35
  w["carcasses","scavenging birds"] <- -0.75
  
  #Small mammals
  w["shrub","small mammals"] <- -0.5
  w["small carnivores","small mammals"] <- 0.5
  w["invertebrate herbivores","small mammals"] <- -0.25
  w["invertebrate predators","small mammals"] <- -0.3
  w["forb","small mammals"] <- -0.2
  w["song birds","small mammals"] <- -0.25
  w["grass","small mammals"] <- -0.1
  w["scavenging birds","small mammals"] <- 0.8
  w["disturbance","small mammals"] <- 0.75
  w["mountain lions","small mammals"] <- 0.25
  w["coyotes","small mammals"] <- 0.75
  w["black bear","small mammals"] <- 0.1
  w["snakes","small mammals"] <- 0.85
  w["wolves","small mammals"] <- 0.1
  
  #Disturbance
  w["exotic forb","disturbance"] <- 0.57
  w["forb","disturbance"] <- -0.1
  w["grass","disturbance"] <- 0
  w["exotic grass","disturbance"] <- 0.6
  
  #Hunters
  w["elk/deer","hunters"] <- -1
  w["carcasses","hunters"] <- 1
  
  #Mountain Lions
  w["elk/deer","mountain lions"] <- -0.3 #Variable interaction detailed above in FCM function
  #w["small carnivores","mountain lions"] <- 0.35
  #w["scavenging birds","mountain lions"] <- 0.2
  w["small mammals","mountain lions"] <- -0.15
  w["coyotes","mountain lions"] <- -0.25
  w["carcasses","mountain lions"] <- 0.333
  
  #Coyotes
  w["elk/deer","coyotes"] <- -0.25
  w["small carnivores","coyotes"] <- -0.4
  w["invertebrate herbivores","coyotes"] <- -0.05
  w["small mammals","coyotes"] <- -0.25
  w["snakes","coyotes"] <- -0.15
  w["carcasses","coyotes"] <- -0.25
  
  #Veg Crew
  w["shrub","veg crew"] <- 0.5
  w["exotic forb","veg crew"] <- -0.4
  w["forb","veg crew"] <- 0.45
  w["grass","veg crew"] <- 0.5
  w["exotic grass","veg crew"] <- -0.3
  
  #Black bear
  w["shrub","black bear"] <- -0.01
  w["small carnivores","black bear"] <- -0.1
  w["forb","black bear"] <- -0.025
  w["exotic forb","black bear"] <- -0.025
  w["grass","black bear"] <- -0.01
  w["small mammals","black bear"] <- -0.05
  w["mountain lions","black bear"] <- -0.05
  w["carcasses","black bear"] <- -0.05
  
  #Exotic grass
  w["shrub","exotic grass"] <- -0.25
  w["exotic forb","exotic grass"] <- -0.25
  w["elk/deer","exotic grass"] <- 0.2
  w["invertebrate herbivores","exotic grass"] <- 0.1
  w["invertebrate predators","exotic grass"] <- 0
  w["forb","exotic grass"] <- -0.55
  w["grass","exotic grass"] <- -0.2
  w["small mammals","exotic grass"] <- -0.4
  w["disturbance","exotic grass"] <- -0.5
  
  #Snakes
  w["small carnivores","snakes"] <- 0.15
  w["invertebrate herbivores","snakes"] <- -0.25
  w["invertebrate predators","snakes"] <- -0.25
  w["song birds","snakes"] <- -0.25
  w["scavenging birds","snakes"] <- 0.1
  w["small mammals","snakes"] <- -0.25
  w["coyotes","snakes"] <- 0.1
  
  #Pollinators
  w["shrub","pollinators"] <- 0.9
  w["exotic forb","pollinators"] <- 0.62
  w["invertebrate predators","pollinators"] <- 0.4
  w["forb","pollinators"] <- 0.97
  w["song birds","pollinators"] <- 0.5
  
  ##Grizzlies
  w["elk/deer","grizzlies"] <- runif(1,-0.1,0) #-0.05 
  #w["mountain lions","grizzlies"] <- -0.025
  #w["black bear","grizzlies"] <- -0.075
  #w["coyotes","grizzlies"] <- -0.05
  #w["carcasses","grizzlies"] <- -0.1 #Variable interaction detailed above in FCM function
  #w["shrub","grizzlies"] <- 0.01
  #w["forb","grizzlies"] <- -0.01
  #w["exotic forb","grizzlies"] <- -0.01
  #w["grass","grizzlies"] <- -0.005 
  #w["wolves","grizzlies"] <- -0.025
  
  ##Carcasses
  w["grizzlies","carcasses"] <-  0.2 #Variable interaction detailed above in FCM function
  w["small carnivores","carcasses"] <-  0.2
  w["coyotes","carcasses"] <-  0.25
  w["black bear","carcasses"] <-  0.2
  w["scavenging birds","carcasses"] <-  0.4 
  w["wolves","carcasses"] <-  0.1
  
  ##Wolves
  w["elk/deer","wolves"] <- -0.3
  w["mountain lions","wolves"] <- -0.025
  w["small carnivores","wolves"] <- -0.05
  w["coyotes","wolves"] <- -0.15
  w["carcasses","wolves"] <- 0.333 
  w["small mammals","wolves"] <- -0.05 
  w["grizzlies","wolves"] <- -0.01
  
  ##Need membership functions from above
  
  ## RUN CODE
  
  result <- data.frame(matrix(NA,11,24))
  grizzlies <- seq(0,1,0.1)
  result[,22] <- grizzlies
  colnames(result) <- c("shrub","precipitation","exotic forb","elk/deer","small carnivores",
                        "invertebrate herbivores","invertebrate predators","forb","song birds",
                        "grass","scavenging birds","small mammals","disturbance","hunters",
                        "mountain lions","coyotes","veg crew","black bear",
                        "exotic grass","snakes","pollinators","grizzlies","carcasses","wolves")
  
  for(m in 1:nrow(result)){
    
    s <- c(shrub.s, precipitation.s, exotic.forb.s, elk.deer.s, small.carnivores.s,
           invertebrate.herbivores.s, invertebrate.predators.s, forb.s, song.birds.s,
           grass.s, scavenging.birds.s, small.mammals.s, disturbance.s, hunters.s,
           mountain.lions.s, coyotes.s, veg.crew.s, black.bear.s,
           exotic.grass.s, snakes.s, pollinators.s,result[m,22],carcasses.s,wolves.s)
    

    
    ##Precipitation, veg crew, and hunter as fixed
    fix<- c(0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,1,0,0,0,0,1,0,0)
    af <- c(2,1,2,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,2)
    
    test<- fcm.zero(s,w,tol=0.001,lambda=0.5,c=1,fix=fix,af=af)
    
    result[m,1] <- test$state[test$iter,1] #shrub
    result[m,2] <- test$state[test$iter,2] #prec
    result[m,3] <- test$state[test$iter,3] #ex forb
    result[m,4] <- test$state[test$iter,4] #elk deer
    result[m,5] <- test$state[test$iter,5] #meso
    result[m,6] <- test$state[test$iter,6] #inv herb
    result[m,7] <- test$state[test$iter,7] #inv pred
    result[m,8] <- test$state[test$iter,8] #forb
    result[m,9] <- test$state[test$iter,9] #song birds
    result[m,10] <- test$state[test$iter,10] #grass
    result[m,11] <- test$state[test$iter,11] #scavenging birds
    result[m,12] <- test$state[test$iter,12] #small mam
    result[m,13] <- test$state[test$iter,13] #dist
    result[m,14] <- test$state[test$iter,14] #hunters
    result[m,15] <- test$state[test$iter,15] #lions
    result[m,16] <- test$state[test$iter,16] #coyotes
    result[m,17] <- test$state[test$iter,17] #veg crew
    result[m,18] <- test$state[test$iter,18] #black bear
    result[m,19] <- test$state[test$iter,19] #ex grass
    result[m,20] <- test$state[test$iter,20] #snake
    result[m,21] <- test$state[test$iter,21] #pollinators
    #result[m,22] <- test$state[test$iter,22] #grizzlies
    result[m,23] <- test$state[test$iter,23] #carcasses
    result[m,24] <- test$state[test$iter,24] #carcasses
    

  }

  
  # only record results from carnivores
  hunt.uncert.ml[i] <- (result$`mountain lions`[11]-result$`mountain lions`[1])/result$`mountain lions`[1]
  hunt.uncert.bb[i] <- (result$`black bear`[11]-result$`black bear`[1])/result$`black bear`[1]
  hunt.uncert.co[i] <- (result$coyotes[11]-result$coyotes[1])/result$coyotes[1]
  hunt.uncert.rp[i] <- (result$`scavenging birds`[11]-result$`scavenging birds`[1])/result$`scavenging birds`[1]
  hunt.uncert.mp[i] <- (result$`small carnivores`[11]-result$`small carnivores`[1])/result$`small carnivores`[1]
  hunt.uncert.ug[i] <- (result$`elk/deer`[11]-result$`elk/deer`[1])/result$`elk/deer`[1]
  hunt.uncert.wf[i] <- (result$`wolves`[11]-result$`wolves`[1])/result$`wolves`[1]
  print(i)
}

# black bear results 
mean(hunt.uncert.bb) 
quantile(hunt.uncert.bb, c(0.05,0.95)) 
hunt.uncert.bb.df <- as.data.frame(hunt.uncert.bb)
colnames(hunt.uncert.bb.df) <- "uncert"
hunt.uncert.bb.df$Species <- "Black bears"

# mountain lion results 
mean(hunt.uncert.ml) 
quantile(hunt.uncert.ml, c(0.05,0.95)) 
hunt.uncert.ml.df <- as.data.frame(hunt.uncert.ml)
colnames(hunt.uncert.ml.df) <- "uncert"
hunt.uncert.ml.df$Species <- "Mountain lions"

# coyote results 
mean(hunt.uncert.co) 
quantile(hunt.uncert.co, c(0.05,0.95)) 
hunt.uncert.co.df <- as.data.frame(hunt.uncert.co)
colnames(hunt.uncert.co.df) <- "uncert"
hunt.uncert.co.df$Species <- "Coyotes"

# scavenging birds results 
mean(hunt.uncert.rp) 
quantile(hunt.uncert.rp, c(0.05,0.95)) 
hunt.uncert.rp.df <- as.data.frame(hunt.uncert.rp)
colnames(hunt.uncert.rp.df) <- "uncert"
hunt.uncert.rp.df$Species <- "Scavenging birds"

# mesopredator results 
mean(hunt.uncert.mp) 
quantile(hunt.uncert.mp, c(0.05,0.95)) 
hunt.uncert.mp.df <- as.data.frame(hunt.uncert.mp)
colnames(hunt.uncert.mp.df) <- "uncert"
hunt.uncert.mp.df$Species <- "Small carnivores"

# elk/deer results
mean(hunt.uncert.ug) 
quantile(hunt.uncert.ug, c(0.05,0.95)) 
hunt.uncert.ug.df <- as.data.frame(hunt.uncert.ug)
colnames(hunt.uncert.ug.df) <- "uncert"
hunt.uncert.ug.df$Species <- "Elk/Deer"

# wolves results
mean(hunt.uncert.wf) 
quantile(hunt.uncert.wf, c(0.05,0.95)) 
hunt.uncert.wf.df <- as.data.frame(hunt.uncert.wf)
colnames(hunt.uncert.wf.df) <- "uncert"
hunt.uncert.wf.df$Species <- "Wolves"

hunt.uncert.full <- rbind(hunt.uncert.bb.df,hunt.uncert.ml.df,hunt.uncert.co.df,
                          hunt.uncert.rp.df,hunt.uncert.mp.df,hunt.uncert.ug.df,
                          hunt.uncert.wf.df)
library(ggtext)
hunt <- ggplot()+
  geom_boxplot(data = hunt.uncert.full, aes(uncert,Species,fill=Species))+
  xlab("")+
  ylab("")+
  ggtitle("c) Predation")+
  theme_classic()+
  geom_vline(xintercept = 0)+
  scale_fill_manual(values = c(alpha("firebrick3",0.9),alpha("darkgoldenrod",0.9),alpha("forestgreen",0.9),alpha("chocolate4",0.9),alpha("deepskyblue4",0.9),alpha("moccasin",0.9),alpha("darkgrey",0.9)))+
  xlim(-0.35,0.1)+
  annotate(geom = "text", y = 2.9, x= 0.1, label = "**", size = 6)+
  theme(legend.position = "none",axis.text = element_markdown())



hunt.f <- ggplot()+
  geom_boxplot(data = hunt.uncert.full, aes(uncert,Species,fill=Species))+
  xlab("")+
  ylab("")+
  ggtitle("c) Predation")+
  theme_classic()+
  geom_vline(xintercept = 0)+
  scale_fill_manual(values = c(alpha("firebrick3",0.9),alpha("darkgoldenrod",0.9),alpha("forestgreen",0.9),alpha("chocolate4",0.9),alpha("deepskyblue4",0.9),alpha("moccasin",0.9),alpha("darkgrey",0.9)))+
  #xlim(-0.6,0.2)+
  annotate(geom = "text", y = 2.9, x= 0.1, label = "**", size = 6)+
  theme(legend.position = "none",axis.text = element_markdown())

#############################################################################################
# third scenario: variation in indirect effect of grizzlies on black bears, mountain lions, coyotes via scavenging of carcasses
# grizzly bear scavenging on carcasses
# Setting other effects to zero


sims <- 10000 # 10000

scav.uncert.ml <- rep(NA,sims)
scav.uncert.bb <- rep(NA,sims)
scav.uncert.co <- rep(NA,sims)
scav.uncert.rp <- rep(NA,sims)
scav.uncert.mp <- rep(NA,sims)
scav.uncert.ug <- rep(NA,sims)
scav.uncert.wf <- rep(NA,sims)

for (i in 1:sims){
  
  w <- matrix(0,nrow=24,ncol=24)
  species <- c("shrub","precipitation","exotic forb","elk/deer","small carnivores",
               "invertebrate herbivores","invertebrate predators","forb","song birds",
               "grass","scavenging birds","small mammals","disturbance","hunters",
               "mountain lions","coyotes","veg crew","black bear",
               "exotic grass","snakes","pollinators","grizzlies","carcasses","wolves")
  dimnames(w) <- list(species,species)
  
  ##Parameterize interactions
  
  
  ##Shrub
  w["elk/deer","shrub"] <- 0.45
  w["small carnivores","shrub"] <- 0.2
  w["invertebrate herbivores","shrub"] <- 0.9
  w["invertebrate predators","shrub"] <- 0.3
  w["song birds","shrub"] <- 0.5
  w["small mammals","shrub"] <- 0.5
  w["disturbance","shrub"] <- -0.25
  w["coyotes","shrub"] <- 0.15
  w["black bear","shrub"] <- 0.25
  w["exotic grass","shrub"] <- 0.2
  w["pollinators","shrub"] <- 0.75
  w["grizzlies","shrub"] <- 0.1
  
  ##Precipitation
  w["shrub","precipitation"] <- 0.7
  w["exotic forb","precipitation"] <- 0.95
  w["forb","precipitation"] <- 1
  w["grass","precipitation"] <- 1
  w["exotic grass","precipitation"] <- 0.97
  
  #Exotic forb
  w["shrub","exotic forb"] <- -0.1
  w["elk/deer","exotic forb"] <- 0.5
  w["invertebrate herbivores","exotic forb"] <- 0.25
  w["invertebrate predators","exotic forb"] <- 0.2
  w["forb","exotic forb"] <- -0.25
  w["grass","exotic forb"] <- -0.2
  w["small mammals","exotic forb"] <- 0.4
  w["disturbance","exotic forb"] <- -0.5
  w["exotic grass","exotic forb"] <- -0.2
  w["pollinators","exotic forb"] <- 0.28
  w["grizzlies","exotic forb"] <- 0.15
  
  #Elk/deer
  w["shrub","elk/deer"] <- -0.27
  w["exotic forb","elk/deer"] <- -0.15
  w["forb","elk/deer"] <- -0.22
  w["grass","elk/deer"] <- -0.35
  w["disturbance","elk/deer"] <- 0.75
  w["mountain lions","elk/deer"] <- 0.85
  w["coyotes","elk/deer"] <- 0.25
  w["exotic grass","elk/deer"] <- -0.05
  w["grizzlies","elk/deer"] <- 0.1 #Variable interaction detailed above in FCM function
  w["wolves","elk/deer"] <- 0.95
  
  #small carnivores
  w["invertebrate herbivores","small carnivores"] <- -0.1
  w["song birds","small carnivores"] <- -0.45
  w["scavenging birds","small carnivores"] <- 0.1
  w["small mammals","small carnivores"] <- -0.5
  w["disturbance","small carnivores"] <- 0.2
  w["snakes","small carnivores"] <- -0.3
  w["carcasses","small carnivores"] <- -0.05
  
  #Invertebrate herbivores
  w["shrub","invertebrate herbivores"] <- -0.2
  w["small carnivores","invertebrate herbivores"] <- 0.5
  w["invertebrate predators","invertebrate herbivores"] <- 0.85
  w["forb","invertebrate herbivores"] <- -0.27
  w["song birds","invertebrate herbivores"] <- 0.6
  w["grass","invertebrate herbivores"] <- -0.15
  w["small mammals","invertebrate herbivores"] <- 0.67
  w["coyotes","invertebrate herbivores"] <- 0.2
  w["black bear","invertebrate herbivores"] <- 0.1
  w["snakes","invertebrate herbivores"] <- 0.5
  w["grizzlies","invertebrate herbivores"] <- 0.05
  
  #Invertebrate predators
  w["invertebrate herbivores","invertebrate predators"] <- -0.5
  w["song birds","invertebrate predators"] <- 0.6
  w["small mammals","invertebrate predators"] <- 0.5
  w["snakes","invertebrate predators"] <- 0.35
  w["pollinators","invertebrate predators"] <- -0.25
  
  #Forb
  w["exotic forb","forb"] <- -0.4
  w["elk/deer","forb"] <- 0.55
  w["invertebrate herbivores","forb"] <- 0.9
  w["invertebrate predators","forb"] <- 0.3
  w["song birds","forb"] <- 0.25
  w["grass","forb"] <- -0.1
  w["small mammals","forb"] <- 0.75
  w["disturbance","forb"] <- -0.25
  w["black bear","forb"] <- 0.35
  w["exotic grass","forb"] <- -0.3
  w["pollinators","forb"] <- 1
  w["grizzlies","forb"] <- 0.15
  
  #Song birds
  w["shrub","song birds"] <- -0.1
  w["small carnivores","song birds"] <- 0.25
  w["invertebrate herbivores","song birds"] <- -0.3
  w["invertebrate predators","song birds"] <- -0.3
  w["forb","song birds"] <- -0.05
  w["scavenging birds","song birds"] <- 0.2
  w["snakes","song birds"] <- 0.45
  w["pollinators","song birds"] <- -0.25
  
  #Grass
  w["exotic forb","grass"] <- -0.55
  w["elk/deer","grass"] <- 0.95
  w["invertebrate herbivores","grass"] <- 0.8
  w["invertebrate predators","grass"] <- 0.2
  w["forb","grass"] <- -0.4
  w["song birds","grass"] <- 0.25
  w["small mammals","grass"] <- 0.65
  w["disturbance","grass"] <- -0.25
  w["black bear","grass"] <- 0.3
  w["exotic grass","grass"] <- -0.5
  w["grizzlies","grass"] <- 0.4
  
  #scavenging birds
  w["small carnivores","scavenging birds"] <- -0.25
  w["invertebrate herbivores","scavenging birds"] <- -0.1
  w["song birds","scavenging birds"] <- -0.25
  w["small mammals","scavenging birds"] <- -0.5
  w["snakes","scavenging birds"] <- -0.35
  w["carcasses","scavenging birds"] <- -0.75
  
  #Small mammals
  w["shrub","small mammals"] <- -0.5
  w["small carnivores","small mammals"] <- 0.5
  w["invertebrate herbivores","small mammals"] <- -0.25
  w["invertebrate predators","small mammals"] <- -0.3
  w["forb","small mammals"] <- -0.2
  w["song birds","small mammals"] <- -0.25
  w["grass","small mammals"] <- -0.1
  w["scavenging birds","small mammals"] <- 0.8
  w["disturbance","small mammals"] <- 0.75
  w["mountain lions","small mammals"] <- 0.25
  w["coyotes","small mammals"] <- 0.75
  w["black bear","small mammals"] <- 0.1
  w["snakes","small mammals"] <- 0.85
  w["wolves","small mammals"] <- 0.1
  
  #Disturbance
  w["exotic forb","disturbance"] <- 0.57
  w["forb","disturbance"] <- -0.1
  w["grass","disturbance"] <- 0
  w["exotic grass","disturbance"] <- 0.6
  
  #Hunters
  w["elk/deer","hunters"] <- -1
  w["carcasses","hunters"] <- 1
  
  #Mountain Lions
  w["elk/deer","mountain lions"] <- -0.3 #Variable interaction detailed above in FCM function
  #w["small carnivores","mountain lions"] <- 0.35
  #w["scavenging birds","mountain lions"] <- 0.2
  w["small mammals","mountain lions"] <- -0.15
  w["coyotes","mountain lions"] <- -0.25
  w["carcasses","mountain lions"] <- 0.333
  
  #Coyotes
  w["elk/deer","coyotes"] <- -0.25
  w["small carnivores","coyotes"] <- -0.4
  w["invertebrate herbivores","coyotes"] <- -0.05
  w["small mammals","coyotes"] <- -0.25
  w["snakes","coyotes"] <- -0.15
  w["carcasses","coyotes"] <- -0.25
  
  #Veg Crew
  w["shrub","veg crew"] <- 0.5
  w["exotic forb","veg crew"] <- -0.4
  w["forb","veg crew"] <- 0.45
  w["grass","veg crew"] <- 0.5
  w["exotic grass","veg crew"] <- -0.3
  
  #Black bear
  w["shrub","black bear"] <- -0.01
  w["small carnivores","black bear"] <- -0.1
  w["forb","black bear"] <- -0.025
  w["exotic forb","black bear"] <- -0.025
  w["grass","black bear"] <- -0.01
  w["small mammals","black bear"] <- -0.05
  w["mountain lions","black bear"] <- -0.05
  w["carcasses","black bear"] <- -0.05
  
  #Exotic grass
  w["shrub","exotic grass"] <- -0.25
  w["exotic forb","exotic grass"] <- -0.25
  w["elk/deer","exotic grass"] <- 0.2
  w["invertebrate herbivores","exotic grass"] <- 0.1
  w["invertebrate predators","exotic grass"] <- 0
  w["forb","exotic grass"] <- -0.55
  w["grass","exotic grass"] <- -0.2
  w["small mammals","exotic grass"] <- -0.4
  w["disturbance","exotic grass"] <- -0.5
  
  #Snakes
  w["small carnivores","snakes"] <- 0.15
  w["invertebrate herbivores","snakes"] <- -0.25
  w["invertebrate predators","snakes"] <- -0.25
  w["song birds","snakes"] <- -0.25
  w["scavenging birds","snakes"] <- 0.1
  w["small mammals","snakes"] <- -0.25
  w["coyotes","snakes"] <- 0.1
  
  #Pollinators
  w["shrub","pollinators"] <- 0.9
  w["exotic forb","pollinators"] <- 0.62
  w["invertebrate predators","pollinators"] <- 0.4
  w["forb","pollinators"] <- 0.97
  w["song birds","pollinators"] <- 0.5
  
  ##Grizzlies
  #w["elk/deer","grizzlies"] <- -0.05 
  #w["mountain lions","grizzlies"] <- -0.025
  #w["black bear","grizzlies"] <- -0.075
  #w["coyotes","grizzlies"] <- -0.05
  w["carcasses","grizzlies"] <- runif(1,-0.2,0) #-0.1 #Variable interaction detailed above in FCM function
  #w["shrub","grizzlies"] <- 0.01
  #w["forb","grizzlies"] <- -0.01
  #w["exotic forb","grizzlies"] <- -0.01
  #w["grass","grizzlies"] <- -0.005 
  #w["wolves","grizzlies"] <- -0.025
  
  ##Carcasses
  w["grizzlies","carcasses"] <-  0.2 #Variable interaction detailed above in FCM function
  w["small carnivores","carcasses"] <-  0.2
  w["coyotes","carcasses"] <-  0.25
  w["black bear","carcasses"] <-  0.2
  w["scavenging birds","carcasses"] <-  0.4 
  w["wolves","carcasses"] <-  0.1
  
  ##Wolves
  w["elk/deer","wolves"] <- -0.3
  w["mountain lions","wolves"] <- -0.025
  w["small carnivores","wolves"] <- -0.05
  w["coyotes","wolves"] <- -0.15
  w["carcasses","wolves"] <- 0.333 
  w["small mammals","wolves"] <- -0.05 
  w["grizzlies","wolves"] <- -0.01
  
  ##Need membership functions from above
  
  ## RUN CODE
  
  result <- data.frame(matrix(NA,11,24))
  grizzlies <- seq(0,1,0.1)
  result[,22] <- grizzlies
  colnames(result) <- c("shrub","precipitation","exotic forb","elk/deer","small carnivores",
                        "invertebrate herbivores","invertebrate predators","forb","song birds",
                        "grass","scavenging birds","small mammals","disturbance","hunters",
                        "mountain lions","coyotes","veg crew","black bear",
                        "exotic grass","snakes","pollinators","grizzlies","carcasses","wolves")
  

  for(m in 1:nrow(result)){
    
    s <- c(shrub.s, precipitation.s, exotic.forb.s, elk.deer.s, small.carnivores.s,
           invertebrate.herbivores.s, invertebrate.predators.s, forb.s, song.birds.s,
           grass.s, scavenging.birds.s, small.mammals.s, disturbance.s, hunters.s,
           mountain.lions.s, coyotes.s, veg.crew.s, black.bear.s,
           exotic.grass.s, snakes.s, pollinators.s,result[m,22],carcasses.s,wolves.s)
    

    ##Precipitation, veg crew, and hunter as fixed
    fix<- c(0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,1,0,0,0,0,1,0,0)
    af <- c(2,1,2,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,2)
    
    test<- fcm.zero(s,w,tol=0.001,lambda=0.5,c=1,fix=fix,af=af)
    
    result[m,1] <- test$state[test$iter,1] #shrub
    result[m,2] <- test$state[test$iter,2] #prec
    result[m,3] <- test$state[test$iter,3] #ex forb
    result[m,4] <- test$state[test$iter,4] #elk deer
    result[m,5] <- test$state[test$iter,5] #meso
    result[m,6] <- test$state[test$iter,6] #inv herb
    result[m,7] <- test$state[test$iter,7] #inv pred
    result[m,8] <- test$state[test$iter,8] #forb
    result[m,9] <- test$state[test$iter,9] #song birds
    result[m,10] <- test$state[test$iter,10] #grass
    result[m,11] <- test$state[test$iter,11] #scavenging birds
    result[m,12] <- test$state[test$iter,12] #small mam
    result[m,13] <- test$state[test$iter,13] #dist
    result[m,14] <- test$state[test$iter,14] #hunters
    result[m,15] <- test$state[test$iter,15] #lions
    result[m,16] <- test$state[test$iter,16] #coyotes
    result[m,17] <- test$state[test$iter,17] #veg crew
    result[m,18] <- test$state[test$iter,18] #black bear
    result[m,19] <- test$state[test$iter,19] #ex grass
    result[m,20] <- test$state[test$iter,20] #snake
    result[m,21] <- test$state[test$iter,21] #pollinators
    #result[m,22] <- test$state[test$iter,22] #grizzlies
    result[m,23] <- test$state[test$iter,23] #carcasses
    result[m,24] <- test$state[test$iter,24] #wolves
    

  }

  
  # only record results from species of interest
  scav.uncert.ml[i] <- (result$`mountain lions`[11]-result$`mountain lions`[1])/result$`mountain lions`[1]
  scav.uncert.bb[i] <- (result$`black bear`[11]-result$`black bear`[1])/result$`black bear`[1]
  scav.uncert.co[i] <- (result$coyotes[11]-result$coyotes[1])/result$coyotes[1]
  scav.uncert.rp[i] <- (result$`scavenging birds`[11]-result$`scavenging birds`[1])/result$`scavenging birds`[1]
  scav.uncert.mp[i] <- (result$`small carnivores`[11]-result$`small carnivores`[1])/result$`small carnivores`[1]
  scav.uncert.ug[i] <- (result$`elk/deer`[11]-result$`elk/deer`[1])/result$`elk/deer`[1]
  scav.uncert.wf[i] <- (result$wolves[11]-result$wolves[1])/result$wolves[1]
  print(i)
}

# black bear results 
mean(scav.uncert.bb) 
quantile(scav.uncert.bb, c(0.05,0.95)) 
scav.uncert.bb.df <- as.data.frame(scav.uncert.bb)
colnames(scav.uncert.bb.df) <- "uncert"
scav.uncert.bb.df$Species <- "Black bears"

# mountain lion results 
mean(scav.uncert.ml) 
quantile(scav.uncert.ml, c(0.05,0.95)) 
scav.uncert.ml.df <- as.data.frame(scav.uncert.ml)
colnames(scav.uncert.ml.df) <- "uncert"
scav.uncert.ml.df$Species <- "Mountain lions"

# coyote results 
mean(scav.uncert.co) 
quantile(scav.uncert.co, c(0.05,0.95)) 
scav.uncert.co.df <- as.data.frame(scav.uncert.co)
colnames(scav.uncert.co.df) <- "uncert"
scav.uncert.co.df$Species <- "Coyotes"

# scavenging bird results 
mean(scav.uncert.rp) 
quantile(scav.uncert.rp, c(0.05,0.95)) 
scav.uncert.rp.df <- as.data.frame(scav.uncert.rp)
colnames(scav.uncert.rp.df) <- "uncert"
scav.uncert.rp.df$Species <- "Scavenging birds"

# mesopredator results 
mean(scav.uncert.mp) 
quantile(scav.uncert.mp, c(0.05,0.95)) 
scav.uncert.mp.df <- as.data.frame(scav.uncert.mp)
colnames(scav.uncert.mp.df) <- "uncert"
scav.uncert.mp.df$Species <- "Small carnivores"

# elk/deer results
mean(scav.uncert.ug) 
quantile(scav.uncert.ug, c(0.05,0.95)) 
scav.uncert.ug.df <- as.data.frame(scav.uncert.ug)
colnames(scav.uncert.ug.df) <- "uncert"
scav.uncert.ug.df$Species <- "Elk/Deer"

# wolves results
mean(scav.uncert.wf) 
quantile(scav.uncert.wf, c(0.05,0.95)) 
scav.uncert.wf.df <- as.data.frame(scav.uncert.wf)
colnames(scav.uncert.wf.df) <- "uncert"
scav.uncert.wf.df$Species <- "Wolves"

scav.uncert.full <- rbind(scav.uncert.bb.df,scav.uncert.ml.df,scav.uncert.co.df,
                          scav.uncert.rp.df,scav.uncert.mp.df,scav.uncert.ug.df,
                          scav.uncert.wf.df)

scav <- ggplot()+
  geom_boxplot(data = scav.uncert.full, aes(uncert,Species,fill=Species))+
  xlab("Relative change in population")+
  ylab("")+
  ggtitle("d) Scavenging")+
  theme_classic()+
  geom_vline(xintercept = 0)+
  scale_fill_manual(values = c(alpha("firebrick3",0.9),alpha("darkgoldenrod",0.9),alpha("forestgreen",0.9),alpha("chocolate4",0.9),alpha("deepskyblue4",0.9),alpha("moccasin",0.9),alpha("darkgrey",0.9)))+
  xlim(-0.35,0.1)+
  theme(legend.position = "none")

scav.f <- ggplot()+
  geom_boxplot(data = scav.uncert.full, aes(uncert,Species,fill=Species))+
  xlab("Relative change in population")+
  ylab("")+
  ggtitle("d) Scavenging")+
  theme_classic()+
  geom_vline(xintercept = 0)+
  scale_fill_manual(values = c(alpha("firebrick3",0.9),alpha("darkgoldenrod",0.9),alpha("forestgreen",0.9),alpha("chocolate4",0.9),alpha("deepskyblue4",0.9),alpha("moccasin",0.9),alpha("darkgrey",0.9)))+
  #xlim(-0.6,0.2)+
  theme(legend.position = "none")



#############################################################################################
# fourth scenario: variation in indirect effect of grizzlies on black bears, mountain lions, coyotes via herbivory
# grizzly bear herbivory
# other effects set to zero


sims <- 10000 # 10000

herb.uncert.ml <- rep(NA,sims)
herb.uncert.bb <- rep(NA,sims)
herb.uncert.co <- rep(NA,sims)
herb.uncert.rp <- rep(NA,sims)
herb.uncert.mp <- rep(NA,sims)
herb.uncert.ug <- rep(NA,sims)
herb.uncert.wf <- rep(NA,sims)

for (i in 1:sims){
  
  w <- matrix(0,nrow=24,ncol=24)
  species <- c("shrub","precipitation","exotic forb","elk/deer","small carnivores",
               "invertebrate herbivores","invertebrate predators","forb","song birds",
               "grass","scavenging birds","small mammals","disturbance","hunters",
               "mountain lions","coyotes","veg crew","black bear",
               "exotic grass","snakes","pollinators","grizzlies","carcasses","wolves")
  dimnames(w) <- list(species,species)
  
  ##Parameterize interactions
  
  
  ##Shrub
  w["elk/deer","shrub"] <- 0.45
  w["small carnivores","shrub"] <- 0.2
  w["invertebrate herbivores","shrub"] <- 0.9
  w["invertebrate predators","shrub"] <- 0.3
  w["song birds","shrub"] <- 0.5
  w["small mammals","shrub"] <- 0.5
  w["disturbance","shrub"] <- -0.25
  w["coyotes","shrub"] <- 0.15
  w["black bear","shrub"] <- 0.25
  w["exotic grass","shrub"] <- 0.2
  w["pollinators","shrub"] <- 0.75
  w["grizzlies","shrub"] <- 0.1
  
  ##Precipitation
  w["shrub","precipitation"] <- 0.7
  w["exotic forb","precipitation"] <- 0.95
  w["forb","precipitation"] <- 1
  w["grass","precipitation"] <- 1
  w["exotic grass","precipitation"] <- 0.97
  
  #Exotic forb
  w["shrub","exotic forb"] <- -0.1
  w["elk/deer","exotic forb"] <- 0.5
  w["invertebrate herbivores","exotic forb"] <- 0.25
  w["invertebrate predators","exotic forb"] <- 0.2
  w["forb","exotic forb"] <- -0.25
  w["grass","exotic forb"] <- -0.2
  w["small mammals","exotic forb"] <- 0.4
  w["disturbance","exotic forb"] <- -0.5
  w["exotic grass","exotic forb"] <- -0.2
  w["pollinators","exotic forb"] <- 0.28
  w["grizzlies","exotic forb"] <- 0.15
  
  #Elk/deer
  w["shrub","elk/deer"] <- -0.27
  w["exotic forb","elk/deer"] <- -0.15
  w["forb","elk/deer"] <- -0.22
  w["grass","elk/deer"] <- -0.35
  w["disturbance","elk/deer"] <- 0.75
  w["mountain lions","elk/deer"] <- 0.85
  w["coyotes","elk/deer"] <- 0.25
  w["exotic grass","elk/deer"] <- -0.05
  w["grizzlies","elk/deer"] <- 0.1 #Variable interaction detailed above in FCM function
  w["wolves","elk/deer"] <- 0.95
  
  #small carnivores
  w["invertebrate herbivores","small carnivores"] <- -0.1
  w["song birds","small carnivores"] <- -0.45
  w["scavenging birds","small carnivores"] <- 0.1
  w["small mammals","small carnivores"] <- -0.5
  w["disturbance","small carnivores"] <- 0.2
  w["snakes","small carnivores"] <- -0.3
  w["carcasses","small carnivores"] <- -0.05
  
  #Invertebrate herbivores
  w["shrub","invertebrate herbivores"] <- -0.2
  w["small carnivores","invertebrate herbivores"] <- 0.5
  w["invertebrate predators","invertebrate herbivores"] <- 0.85
  w["forb","invertebrate herbivores"] <- -0.27
  w["song birds","invertebrate herbivores"] <- 0.6
  w["grass","invertebrate herbivores"] <- -0.15
  w["small mammals","invertebrate herbivores"] <- 0.67
  w["coyotes","invertebrate herbivores"] <- 0.2
  w["black bear","invertebrate herbivores"] <- 0.1
  w["snakes","invertebrate herbivores"] <- 0.5
  w["grizzlies","invertebrate herbivores"] <- 0.05
  
  #Invertebrate predators
  w["invertebrate herbivores","invertebrate predators"] <- -0.5
  w["song birds","invertebrate predators"] <- 0.6
  w["small mammals","invertebrate predators"] <- 0.5
  w["snakes","invertebrate predators"] <- 0.35
  w["pollinators","invertebrate predators"] <- -0.25
  
  #Forb
  w["exotic forb","forb"] <- -0.4
  w["elk/deer","forb"] <- 0.55
  w["invertebrate herbivores","forb"] <- 0.9
  w["invertebrate predators","forb"] <- 0.3
  w["song birds","forb"] <- 0.25
  w["grass","forb"] <- -0.1
  w["small mammals","forb"] <- 0.75
  w["disturbance","forb"] <- -0.25
  w["black bear","forb"] <- 0.35
  w["exotic grass","forb"] <- -0.3
  w["pollinators","forb"] <- 1
  w["grizzlies","forb"] <- 0.15
  
  #Song birds
  w["shrub","song birds"] <- -0.1
  w["small carnivores","song birds"] <- 0.25
  w["invertebrate herbivores","song birds"] <- -0.3
  w["invertebrate predators","song birds"] <- -0.3
  w["forb","song birds"] <- -0.05
  w["scavenging birds","song birds"] <- 0.2
  w["snakes","song birds"] <- 0.45
  w["pollinators","song birds"] <- -0.25
  
  #Grass
  w["exotic forb","grass"] <- -0.55
  w["elk/deer","grass"] <- 0.95
  w["invertebrate herbivores","grass"] <- 0.8
  w["invertebrate predators","grass"] <- 0.2
  w["forb","grass"] <- -0.4
  w["song birds","grass"] <- 0.25
  w["small mammals","grass"] <- 0.65
  w["disturbance","grass"] <- -0.25
  w["black bear","grass"] <- 0.3
  w["exotic grass","grass"] <- -0.5
  w["grizzlies","grass"] <- 0.4
  
  #scavenging birds
  w["small carnivores","scavenging birds"] <- -0.25
  w["invertebrate herbivores","scavenging birds"] <- -0.1
  w["song birds","scavenging birds"] <- -0.25
  w["small mammals","scavenging birds"] <- -0.5
  w["snakes","scavenging birds"] <- -0.35
  w["carcasses","scavenging birds"] <- -0.75
  
  #Small mammals
  w["shrub","small mammals"] <- -0.5
  w["small carnivores","small mammals"] <- 0.5
  w["invertebrate herbivores","small mammals"] <- -0.25
  w["invertebrate predators","small mammals"] <- -0.3
  w["forb","small mammals"] <- -0.2
  w["song birds","small mammals"] <- -0.25
  w["grass","small mammals"] <- -0.1
  w["scavenging birds","small mammals"] <- 0.8
  w["disturbance","small mammals"] <- 0.75
  w["mountain lions","small mammals"] <- 0.25
  w["coyotes","small mammals"] <- 0.75
  w["black bear","small mammals"] <- 0.1
  w["snakes","small mammals"] <- 0.85
  w["wolves","small mammals"] <- 0.1
  
  #Disturbance
  w["exotic forb","disturbance"] <- 0.57
  w["forb","disturbance"] <- -0.1
  w["grass","disturbance"] <- 0
  w["exotic grass","disturbance"] <- 0.6
  
  #Hunters
  w["elk/deer","hunters"] <- -1
  w["carcasses","hunters"] <- 1
  
  #Mountain Lions
  w["elk/deer","mountain lions"] <- -0.3 #Variable interaction detailed above in FCM function
  #w["small carnivores","mountain lions"] <- 0.35
  #w["scavenging birds","mountain lions"] <- 0.2
  w["small mammals","mountain lions"] <- -0.15
  w["coyotes","mountain lions"] <- -0.25
  w["carcasses","mountain lions"] <- 0.333
  
  #Coyotes
  w["elk/deer","coyotes"] <- -0.25
  w["small carnivores","coyotes"] <- -0.4
  w["invertebrate herbivores","coyotes"] <- -0.05
  w["small mammals","coyotes"] <- -0.25
  w["snakes","coyotes"] <- -0.15
  w["carcasses","coyotes"] <- -0.25
  
  #Veg Crew
  w["shrub","veg crew"] <- 0.5
  w["exotic forb","veg crew"] <- -0.4
  w["forb","veg crew"] <- 0.45
  w["grass","veg crew"] <- 0.5
  w["exotic grass","veg crew"] <- -0.3
  
  #Black bear
  w["shrub","black bear"] <- -0.01
  w["small carnivores","black bear"] <- -0.1
  w["forb","black bear"] <- -0.025
  w["exotic forb","black bear"] <- -0.025
  w["grass","black bear"] <- -0.01
  w["small mammals","black bear"] <- -0.05
  w["mountain lions","black bear"] <- -0.05
  w["carcasses","black bear"] <- -0.05
  
  #Exotic grass
  w["shrub","exotic grass"] <- -0.25
  w["exotic forb","exotic grass"] <- -0.25
  w["elk/deer","exotic grass"] <- 0.2
  w["invertebrate herbivores","exotic grass"] <- 0.1
  w["invertebrate predators","exotic grass"] <- 0
  w["forb","exotic grass"] <- -0.55
  w["grass","exotic grass"] <- -0.2
  w["small mammals","exotic grass"] <- -0.4
  w["disturbance","exotic grass"] <- -0.5
  
  #Snakes
  w["small carnivores","snakes"] <- 0.15
  w["invertebrate herbivores","snakes"] <- -0.25
  w["invertebrate predators","snakes"] <- -0.25
  w["song birds","snakes"] <- -0.25
  w["scavenging birds","snakes"] <- 0.1
  w["small mammals","snakes"] <- -0.25
  w["coyotes","snakes"] <- 0.1
  
  #Pollinators
  w["shrub","pollinators"] <- 0.9
  w["exotic forb","pollinators"] <- 0.62
  w["invertebrate predators","pollinators"] <- 0.4
  w["forb","pollinators"] <- 0.97
  w["song birds","pollinators"] <- 0.5
  
  ##Grizzlies
  #w["elk/deer","grizzlies"] <- -0.05 
  #w["mountain lions","grizzlies"] <- -0.025
  #w["black bear","grizzlies"] <- -0.075
  #w["coyotes","grizzlies"] <- -0.05
  #w["carcasses","grizzlies"] <- -0.1 #Variable interaction detailed above in FCM function
  w["shrub","grizzlies"] <- runif(1,0,0.02) #0.01
  w["forb","grizzlies"] <- runif(1,-0.02,0) #-0.01
  w["exotic forb","grizzlies"] <- runif(1,-0.02,0) #-0.01
  w["grass","grizzlies"] <- runif(1,-0.01,0) #-0.005
  #w["wolves","grizzlies"] <- -0.025
  
  ##Carcasses
  w["grizzlies","carcasses"] <-  0.2 #Variable interaction detailed above in FCM function
  w["small carnivores","carcasses"] <-  0.2
  w["coyotes","carcasses"] <-  0.25
  w["black bear","carcasses"] <-  0.2
  w["scavenging birds","carcasses"] <-  0.4 
  w["wolves","carcasses"] <-  0.1
  
  ##Wolves
  w["elk/deer","wolves"] <- -0.3
  w["mountain lions","wolves"] <- -0.025
  w["small carnivores","wolves"] <- -0.05
  w["coyotes","wolves"] <- -0.15
  w["carcasses","wolves"] <- 0.333 
  w["small mammals","wolves"] <- -0.05 
  w["grizzlies","wolves"] <- -0.01
  
  ##Need membership functions from above
  
  ## RUN CODE
  
  result <- data.frame(matrix(NA,11,24))
  grizzlies <- seq(0,1,0.1)
  result[,22] <- grizzlies
  colnames(result) <- c("shrub","precipitation","exotic forb","elk/deer","small carnivores",
                        "invertebrate herbivores","invertebrate predators","forb","song birds",
                        "grass","scavenging birds","small mammals","disturbance","hunters",
                        "mountain lions","coyotes","veg crew","black bear",
                        "exotic grass","snakes","pollinators","grizzlies","carcasses","wolves")
  
  for(m in 1:nrow(result)){
    
    s <- c(shrub.s, precipitation.s, exotic.forb.s, elk.deer.s, small.carnivores.s,
           invertebrate.herbivores.s, invertebrate.predators.s, forb.s, song.birds.s,
           grass.s, scavenging.birds.s, small.mammals.s, disturbance.s, hunters.s,
           mountain.lions.s, coyotes.s, veg.crew.s, black.bear.s,
           exotic.grass.s, snakes.s, pollinators.s,result[m,22],carcasses.s,wolves.s)
    
    
    
    ##Precipitation, veg crew, and hunter as fixed
    fix<- c(0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,1,0,0,0,0,1,0,0)
    af <- c(2,1,2,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,2)
    
    test<- fcm.zero(s,w,tol=0.001,lambda=0.5,c=1,fix=fix,af=af)
    
    result[m,1] <- test$state[test$iter,1] #shrub
    result[m,2] <- test$state[test$iter,2] #prec
    result[m,3] <- test$state[test$iter,3] #ex forb
    result[m,4] <- test$state[test$iter,4] #elk deer
    result[m,5] <- test$state[test$iter,5] #meso
    result[m,6] <- test$state[test$iter,6] #inv herb
    result[m,7] <- test$state[test$iter,7] #inv pred
    result[m,8] <- test$state[test$iter,8] #forb
    result[m,9] <- test$state[test$iter,9] #song birds
    result[m,10] <- test$state[test$iter,10] #grass
    result[m,11] <- test$state[test$iter,11] #scavenging birds
    result[m,12] <- test$state[test$iter,12] #small mam
    result[m,13] <- test$state[test$iter,13] #dist
    result[m,14] <- test$state[test$iter,14] #hunters
    result[m,15] <- test$state[test$iter,15] #lions
    result[m,16] <- test$state[test$iter,16] #coyotes
    result[m,17] <- test$state[test$iter,17] #veg crew
    result[m,18] <- test$state[test$iter,18] #black bear
    result[m,19] <- test$state[test$iter,19] #ex grass
    result[m,20] <- test$state[test$iter,20] #snake
    result[m,21] <- test$state[test$iter,21] #pollinators
    #result[m,22] <- test$state[test$iter,22] #grizzlies
    result[m,23] <- test$state[test$iter,23] #carcasses
    result[m,24] <- test$state[test$iter,24] #wolves
    

  }

  
  # only record results from species of interest
  herb.uncert.ml[i] <- (result$`mountain lions`[11]-result$`mountain lions`[1])/result$`mountain lions`[1]
  herb.uncert.bb[i] <- (result$`black bear`[11]-result$`black bear`[1])/result$`black bear`[1]
  herb.uncert.co[i] <- (result$coyotes[11]-result$coyotes[1])/result$coyotes[1]
  herb.uncert.rp[i] <- (result$`scavenging birds`[11]-result$`scavenging birds`[1])/result$`scavenging birds`[1]
  herb.uncert.mp[i] <- (result$`small carnivores`[11]-result$`small carnivores`[1])/result$`small carnivores`[1]
  herb.uncert.ug[i] <- (result$`elk/deer`[11]-result$`elk/deer`[1])/result$`elk/deer`[1]
  herb.uncert.wf[i] <- (result$wolves[11]-result$wolves[1])/result$wolves[1]
  print(i)
}

# black bear results 
mean(herb.uncert.bb) 
quantile(herb.uncert.bb, c(0.05,0.95)) 
herb.uncert.bb.df <- as.data.frame(herb.uncert.bb)
colnames(herb.uncert.bb.df) <- "uncert"
herb.uncert.bb.df$Species <- "Black bears"

# mountain lion results 
mean(herb.uncert.ml) 
quantile(herb.uncert.ml, c(0.05,0.95)) 
herb.uncert.ml.df <- as.data.frame(herb.uncert.ml)
colnames(herb.uncert.ml.df) <- "uncert"
herb.uncert.ml.df$Species <- "Mountain lions"

# coyote results 
mean(herb.uncert.co) 
quantile(herb.uncert.co, c(0.05,0.95)) 
herb.uncert.co.df <- as.data.frame(herb.uncert.co)
colnames(herb.uncert.co.df) <- "uncert"
herb.uncert.co.df$Species <- "Coyotes"

# scavenging bird results 
mean(herb.uncert.rp) 
quantile(herb.uncert.rp, c(0.05,0.95)) 
herb.uncert.rp.df <- as.data.frame(herb.uncert.rp)
colnames(herb.uncert.rp.df) <- "uncert"
herb.uncert.rp.df$Species <- "Scavenging birds"

# mesopredator results 
mean(herb.uncert.mp) 
quantile(herb.uncert.mp, c(0.05,0.95)) 
herb.uncert.mp.df <- as.data.frame(herb.uncert.mp)
colnames(herb.uncert.mp.df) <- "uncert"
herb.uncert.mp.df$Species <- "Small carnivores"

# elk/deer results
mean(herb.uncert.ug) 
quantile(herb.uncert.ug, c(0.05,0.95)) 
herb.uncert.ug.df <- as.data.frame(herb.uncert.ug)
colnames(herb.uncert.ug.df) <- "uncert"
herb.uncert.ug.df$Species <- "Elk/Deer"

# wolves results
mean(herb.uncert.wf) 
quantile(herb.uncert.wf, c(0.05,0.95)) 
herb.uncert.wf.df <- as.data.frame(herb.uncert.wf)
colnames(herb.uncert.wf.df) <- "uncert"
herb.uncert.wf.df$Species <- "Wolves"

herb.uncert.full <- rbind(herb.uncert.bb.df,herb.uncert.ml.df,herb.uncert.co.df,
                          herb.uncert.rp.df,herb.uncert.mp.df,herb.uncert.ug.df,
                          herb.uncert.wf.df)

herb <- ggplot()+
  geom_boxplot(data = herb.uncert.full, aes(uncert,Species,fill=Species))+
  xlab("Relative change in population")+
  ylab("")+
  ggtitle("e) Herbivory")+
  theme_classic()+
  geom_vline(xintercept = 0)+
  scale_fill_manual(values = c(alpha("firebrick3",0.9),alpha("darkgoldenrod",0.9),alpha("forestgreen",0.9),alpha("chocolate4",0.9),alpha("deepskyblue4",0.9),alpha("moccasin",0.9),alpha("darkgrey",0.9)))+
  xlim(-0.35,0.1)+
  theme(legend.position = "none")

herb.f <- ggplot()+
  geom_boxplot(data = herb.uncert.full, aes(uncert,Species,fill=Species))+
  xlab("Relative change in population")+
  ylab("")+
  ggtitle("e) Herbivory")+
  theme_classic()+
  geom_vline(xintercept = 0)+
  scale_fill_manual(values = c(alpha("firebrick3",0.9),alpha("darkgoldenrod",0.9),alpha("forestgreen",0.9),alpha("chocolate4",0.9),alpha("deepskyblue4",0.9),alpha("moccasin",0.9),alpha("darkgrey",0.9)))+
  #xlim(-0.35,0.1)+
  theme(legend.position = "none")

library(gridExtra)
grid.arrange(dir,hunt,scav,herb,ncol=2)

plot(rep(1,7),col=c(alpha("firebrick3",0.9),alpha("darkgoldenrod",0.9),alpha("forestgreen",0.9),alpha("chocolate4",0.9),alpha("deepskyblue4",0.9),alpha("moccasin",0.9),alpha("darkgrey",0.9)),pch=19,cex=7,yaxt='n', ann=FALSE)

#############################################################################################
# fifth scenario: variation in direct and indirect effects of grizzlies via competition, predation, scavenging, and herbivory


sims <- 10000 # 10000

comb.uncert.ml <- rep(NA,sims)
comb.uncert.bb <- rep(NA,sims)
comb.uncert.co <- rep(NA,sims)
comb.uncert.rp <- rep(NA,sims)
comb.uncert.mp <- rep(NA,sims)
comb.uncert.ug <- rep(NA,sims)
comb.uncert.wf <- rep(NA,sims)

for (i in 1:sims){
  
  w <- matrix(0,nrow=24,ncol=24)
  species <- c("shrub","precipitation","exotic forb","elk/deer","small carnivores",
               "invertebrate herbivores","invertebrate predators","forb","song birds",
               "grass","scavenging birds","small mammals","disturbance","hunters",
               "mountain lions","coyotes","veg crew","black bear",
               "exotic grass","snakes","pollinators","grizzlies","carcasses","wolves")
  dimnames(w) <- list(species,species)
  
  ##Parameterize interactions
  
  
  ##Shrub
  w["elk/deer","shrub"] <- 0.45
  w["small carnivores","shrub"] <- 0.2
  w["invertebrate herbivores","shrub"] <- 0.9
  w["invertebrate predators","shrub"] <- 0.3
  w["song birds","shrub"] <- 0.5
  w["small mammals","shrub"] <- 0.5
  w["disturbance","shrub"] <- -0.25
  w["coyotes","shrub"] <- 0.15
  w["black bear","shrub"] <- 0.25
  w["exotic grass","shrub"] <- 0.2
  w["pollinators","shrub"] <- 0.75
  w["grizzlies","shrub"] <- 0.1
  
  ##Precipitation
  w["shrub","precipitation"] <- 0.7
  w["exotic forb","precipitation"] <- 0.95
  w["forb","precipitation"] <- 1
  w["grass","precipitation"] <- 1
  w["exotic grass","precipitation"] <- 0.97
  
  #Exotic forb
  w["shrub","exotic forb"] <- -0.1
  w["elk/deer","exotic forb"] <- 0.5
  w["invertebrate herbivores","exotic forb"] <- 0.25
  w["invertebrate predators","exotic forb"] <- 0.2
  w["forb","exotic forb"] <- -0.25
  w["grass","exotic forb"] <- -0.2
  w["small mammals","exotic forb"] <- 0.4
  w["disturbance","exotic forb"] <- -0.5
  w["exotic grass","exotic forb"] <- -0.2
  w["pollinators","exotic forb"] <- 0.28
  w["grizzlies","exotic forb"] <- 0.15
  
  #Elk/deer
  w["shrub","elk/deer"] <- -0.27
  w["exotic forb","elk/deer"] <- -0.15
  w["forb","elk/deer"] <- -0.22
  w["grass","elk/deer"] <- -0.35
  w["disturbance","elk/deer"] <- 0.75
  w["mountain lions","elk/deer"] <- 0.85
  w["coyotes","elk/deer"] <- 0.25
  w["exotic grass","elk/deer"] <- -0.05
  w["grizzlies","elk/deer"] <- 0.1 #Variable interaction detailed above in FCM function
  w["wolves","elk/deer"] <- 0.95
  
  #small carnivores
  w["invertebrate herbivores","small carnivores"] <- -0.1
  w["song birds","small carnivores"] <- -0.45
  w["scavenging birds","small carnivores"] <- 0.1
  w["small mammals","small carnivores"] <- -0.5
  w["disturbance","small carnivores"] <- 0.2
  w["snakes","small carnivores"] <- -0.3
  w["carcasses","small carnivores"] <- -0.05
  
  #Invertebrate herbivores
  w["shrub","invertebrate herbivores"] <- -0.2
  w["small carnivores","invertebrate herbivores"] <- 0.5
  w["invertebrate predators","invertebrate herbivores"] <- 0.85
  w["forb","invertebrate herbivores"] <- -0.27
  w["song birds","invertebrate herbivores"] <- 0.6
  w["grass","invertebrate herbivores"] <- -0.15
  w["small mammals","invertebrate herbivores"] <- 0.67
  w["coyotes","invertebrate herbivores"] <- 0.2
  w["black bear","invertebrate herbivores"] <- 0.1
  w["snakes","invertebrate herbivores"] <- 0.5
  w["grizzlies","invertebrate herbivores"] <- 0.05
  
  #Invertebrate predators
  w["invertebrate herbivores","invertebrate predators"] <- -0.5
  w["song birds","invertebrate predators"] <- 0.6
  w["small mammals","invertebrate predators"] <- 0.5
  w["snakes","invertebrate predators"] <- 0.35
  w["pollinators","invertebrate predators"] <- -0.25
  
  #Forb
  w["exotic forb","forb"] <- -0.4
  w["elk/deer","forb"] <- 0.55
  w["invertebrate herbivores","forb"] <- 0.9
  w["invertebrate predators","forb"] <- 0.3
  w["song birds","forb"] <- 0.25
  w["grass","forb"] <- -0.1
  w["small mammals","forb"] <- 0.75
  w["disturbance","forb"] <- -0.25
  w["black bear","forb"] <- 0.35
  w["exotic grass","forb"] <- -0.3
  w["pollinators","forb"] <- 1
  w["grizzlies","forb"] <- 0.15
  
  #Song birds
  w["shrub","song birds"] <- -0.1
  w["small carnivores","song birds"] <- 0.25
  w["invertebrate herbivores","song birds"] <- -0.3
  w["invertebrate predators","song birds"] <- -0.3
  w["forb","song birds"] <- -0.05
  w["scavenging birds","song birds"] <- 0.2
  w["snakes","song birds"] <- 0.45
  w["pollinators","song birds"] <- -0.25
  
  #Grass
  w["exotic forb","grass"] <- -0.55
  w["elk/deer","grass"] <- 0.95
  w["invertebrate herbivores","grass"] <- 0.8
  w["invertebrate predators","grass"] <- 0.2
  w["forb","grass"] <- -0.4
  w["song birds","grass"] <- 0.25
  w["small mammals","grass"] <- 0.65
  w["disturbance","grass"] <- -0.25
  w["black bear","grass"] <- 0.3
  w["exotic grass","grass"] <- -0.5
  w["grizzlies","grass"] <- 0.4
  
  #scavenging birds
  w["small carnivores","scavenging birds"] <- -0.25
  w["invertebrate herbivores","scavenging birds"] <- -0.1
  w["song birds","scavenging birds"] <- -0.25
  w["small mammals","scavenging birds"] <- -0.5
  w["snakes","scavenging birds"] <- -0.35
  w["carcasses","scavenging birds"] <- -0.75
  
  #Small mammals
  w["shrub","small mammals"] <- -0.5
  w["small carnivores","small mammals"] <- 0.5
  w["invertebrate herbivores","small mammals"] <- -0.25
  w["invertebrate predators","small mammals"] <- -0.3
  w["forb","small mammals"] <- -0.2
  w["song birds","small mammals"] <- -0.25
  w["grass","small mammals"] <- -0.1
  w["scavenging birds","small mammals"] <- 0.8
  w["disturbance","small mammals"] <- 0.75
  w["mountain lions","small mammals"] <- 0.25
  w["coyotes","small mammals"] <- 0.75
  w["black bear","small mammals"] <- 0.1
  w["snakes","small mammals"] <- 0.85
  w["wolves","small mammals"] <- 0.1
  
  #Disturbance
  w["exotic forb","disturbance"] <- 0.57
  w["forb","disturbance"] <- -0.1
  w["grass","disturbance"] <- 0
  w["exotic grass","disturbance"] <- 0.6
  
  #Hunters
  w["elk/deer","hunters"] <- -1
  w["carcasses","hunters"] <- 1
  
  #Mountain Lions
  w["elk/deer","mountain lions"] <- -0.3 #Variable interaction detailed above in FCM function
  #w["small carnivores","mountain lions"] <- 0.35
  #w["scavenging birds","mountain lions"] <- 0.2
  w["small mammals","mountain lions"] <- -0.15
  w["coyotes","mountain lions"] <- -0.25
  w["carcasses","mountain lions"] <- 0.333
  
  #Coyotes
  w["elk/deer","coyotes"] <- -0.25
  w["small carnivores","coyotes"] <- -0.4
  w["invertebrate herbivores","coyotes"] <- -0.05
  w["small mammals","coyotes"] <- -0.25
  w["snakes","coyotes"] <- -0.15
  w["carcasses","coyotes"] <- -0.25
  
  #Veg Crew
  w["shrub","veg crew"] <- 0.5
  w["exotic forb","veg crew"] <- -0.4
  w["forb","veg crew"] <- 0.45
  w["grass","veg crew"] <- 0.5
  w["exotic grass","veg crew"] <- -0.3
  
  #Black bear
  w["shrub","black bear"] <- -0.01
  w["small carnivores","black bear"] <- -0.1
  w["forb","black bear"] <- -0.025
  w["exotic forb","black bear"] <- -0.025
  w["grass","black bear"] <- -0.01
  w["small mammals","black bear"] <- -0.05
  w["mountain lions","black bear"] <- -0.05
  w["carcasses","black bear"] <- -0.05
  
  #Exotic grass
  w["shrub","exotic grass"] <- -0.25
  w["exotic forb","exotic grass"] <- -0.25
  w["elk/deer","exotic grass"] <- 0.2
  w["invertebrate herbivores","exotic grass"] <- 0.1
  w["invertebrate predators","exotic grass"] <- 0
  w["forb","exotic grass"] <- -0.55
  w["grass","exotic grass"] <- -0.2
  w["small mammals","exotic grass"] <- -0.4
  w["disturbance","exotic grass"] <- -0.5
  
  #Snakes
  w["small carnivores","snakes"] <- 0.15
  w["invertebrate herbivores","snakes"] <- -0.25
  w["invertebrate predators","snakes"] <- -0.25
  w["song birds","snakes"] <- -0.25
  w["scavenging birds","snakes"] <- 0.1
  w["small mammals","snakes"] <- -0.25
  w["coyotes","snakes"] <- 0.1
  
  #Pollinators
  w["shrub","pollinators"] <- 0.9
  w["exotic forb","pollinators"] <- 0.62
  w["invertebrate predators","pollinators"] <- 0.4
  w["forb","pollinators"] <- 0.97
  w["song birds","pollinators"] <- 0.5
  
  ##Grizzlies
  w["elk/deer","grizzlies"] <- runif(1,-0.1,0) #Variable interaction detailed above in FCM function
  w["mountain lions","grizzlies"] <- runif(1,-0.05,0) #-0.025
  w["black bear","grizzlies"] <- runif(1,-0.15,0) #-0.075
  w["coyotes","grizzlies"] <- runif(1,-0.1,0) #-0.05
  w["carcasses","grizzlies"] <- runif(1,-0.2,0) #Variable interaction detailed above in FCM function
  w["shrub","grizzlies"] <- runif(1,0,0.02) #0.01
  w["forb","grizzlies"] <- runif(1,-0.02,0) #-0.01
  w["exotic forb","grizzlies"] <- runif(1,-0.02,0) #-0.01
  w["grass","grizzlies"] <- runif(1,-0.01,0) #-0.005
  w["wolves","grizzlies"] <- runif(1,-0.05,0) #-0.025
  
  
  ##Carcasses
  w["grizzlies","carcasses"] <-  0.2 #Variable interaction detailed above in FCM function
  w["small carnivores","carcasses"] <-  0.2
  w["coyotes","carcasses"] <-  0.25
  w["black bear","carcasses"] <-  0.2
  w["scavenging birds","carcasses"] <-  0.4 
  w["wolves","carcasses"] <-  0.1
  
  ##Wolves
  w["elk/deer","wolves"] <- -0.3
  w["mountain lions","wolves"] <- -0.025
  w["small carnivores","wolves"] <- -0.05
  w["coyotes","wolves"] <- -0.15
  w["carcasses","wolves"] <- 0.333 
  w["small mammals","wolves"] <- -0.05 
  w["grizzlies","wolves"] <- -0.01
  
  ##Need membership functions from above
  
  ## RUN CODE
  
  result <- data.frame(matrix(NA,11,24))
  grizzlies <- seq(0,1,0.1)
  result[,22] <- grizzlies
  colnames(result) <- c("shrub","precipitation","exotic forb","elk/deer","small carnivores",
                        "invertebrate herbivores","invertebrate predators","forb","song birds",
                        "grass","scavenging birds","small mammals","disturbance","hunters",
                        "mountain lions","coyotes","veg crew","black bear",
                        "exotic grass","snakes","pollinators","grizzlies","carcasses","wolves")
  

  for(m in 1:nrow(result)){
    
    s <- c(shrub.s, precipitation.s, exotic.forb.s, elk.deer.s, small.carnivores.s,
           invertebrate.herbivores.s, invertebrate.predators.s, forb.s, song.birds.s,
           grass.s, scavenging.birds.s, small.mammals.s, disturbance.s, hunters.s,
           mountain.lions.s, coyotes.s, veg.crew.s, black.bear.s,
           exotic.grass.s, snakes.s, pollinators.s,result[m,22],carcasses.s,wolves.s)
    
 
    
    ##Precipitation, veg crew, and hunter as fixed
    fix<- c(0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,1,0,0,0,0,1,0,0)
    af <- c(2,1,2,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,2)
    

    test<- fcm.dir(s,w,tol=0.001,lambda=0.5,c=1,fix=fix,af=af)
    
    result[m,1] <- test$state[test$iter,1] #shrub
    result[m,2] <- test$state[test$iter,2] #prec
    result[m,3] <- test$state[test$iter,3] #ex forb
    result[m,4] <- test$state[test$iter,4] #elk deer
    result[m,5] <- test$state[test$iter,5] #meso
    result[m,6] <- test$state[test$iter,6] #inv herb
    result[m,7] <- test$state[test$iter,7] #inv pred
    result[m,8] <- test$state[test$iter,8] #forb
    result[m,9] <- test$state[test$iter,9] #song birds
    result[m,10] <- test$state[test$iter,10] #grass
    result[m,11] <- test$state[test$iter,11] #scavenging birds
    result[m,12] <- test$state[test$iter,12] #small mam
    result[m,13] <- test$state[test$iter,13] #dist
    result[m,14] <- test$state[test$iter,14] #hunters
    result[m,15] <- test$state[test$iter,15] #lions
    result[m,16] <- test$state[test$iter,16] #coyotes
    result[m,17] <- test$state[test$iter,17] #veg crew
    result[m,18] <- test$state[test$iter,18] #black bear
    result[m,19] <- test$state[test$iter,19] #ex grass
    result[m,20] <- test$state[test$iter,20] #snake
    result[m,21] <- test$state[test$iter,21] #pollinators
    #result[m,22] <- test$state[test$iter,22] #grizzlies
    result[m,23] <- test$state[test$iter,23] #carcasses
    result[m,24] <- test$state[test$iter,24] #wolves
    

  }

  
  # only record results from species of interest
  comb.uncert.ml[i] <- (result$`mountain lions`[11]-result$`mountain lions`[1])/result$`mountain lions`[1]
  comb.uncert.bb[i] <- (result$`black bear`[11]-result$`black bear`[1])/result$`black bear`[1]
  comb.uncert.co[i] <- (result$coyotes[11]-result$coyotes[1])/result$coyotes[1]
  comb.uncert.rp[i] <- (result$`scavenging birds`[11]-result$`scavenging birds`[1])/result$`scavenging birds`[1]
  comb.uncert.mp[i] <- (result$`small carnivores`[11]-result$`small carnivores`[1])/result$`small carnivores`[1]
  comb.uncert.ug[i] <- (result$`elk/deer`[11]-result$`elk/deer`[1])/result$`elk/deer`[1]
  comb.uncert.wf[i] <- (result$wolves[11]-result$wolves[1])/result$wolves[1]
  print(i)
}

# black bear results 
mean(comb.uncert.bb) 
quantile(comb.uncert.bb, c(0.05,0.95)) 
comb.uncert.bb.df <- as.data.frame(comb.uncert.bb)
colnames(comb.uncert.bb.df) <- "uncert"
comb.uncert.bb.df$Species <- "Black bears"

# mountain lion results 
mean(comb.uncert.ml) 
quantile(comb.uncert.ml, c(0.05,0.95)) 
comb.uncert.ml.df <- as.data.frame(comb.uncert.ml)
colnames(comb.uncert.ml.df) <- "uncert"
comb.uncert.ml.df$Species <- "Mountain lions"

# coyote results 
mean(comb.uncert.co) 
quantile(comb.uncert.co, c(0.05,0.95)) 
comb.uncert.co.df <- as.data.frame(comb.uncert.co)
colnames(comb.uncert.co.df) <- "uncert"
comb.uncert.co.df$Species <- "Coyotes"

# scavenging bird results 
mean(comb.uncert.rp) 
quantile(comb.uncert.rp, c(0.05,0.95)) 
comb.uncert.rp.df <- as.data.frame(comb.uncert.rp)
colnames(comb.uncert.rp.df) <- "uncert"
comb.uncert.rp.df$Species <- "Scavenging birds"

# mesopredator results 
mean(comb.uncert.mp) 
quantile(comb.uncert.mp, c(0.05,0.95)) 
comb.uncert.mp.df <- as.data.frame(comb.uncert.mp)
colnames(comb.uncert.mp.df) <- "uncert"
comb.uncert.mp.df$Species <- "Small carnivores"

# elk/deer results
mean(comb.uncert.ug) 
quantile(comb.uncert.ug, c(0.05,0.95)) 
comb.uncert.ug.df <- as.data.frame(comb.uncert.ug)
colnames(comb.uncert.ug.df) <- "uncert"
comb.uncert.ug.df$Species <- "Elk/Deer"

# wolves results
mean(comb.uncert.wf) 
quantile(comb.uncert.wf, c(0.05,0.95)) 
comb.uncert.wf.df <- as.data.frame(comb.uncert.wf)
colnames(comb.uncert.wf.df) <- "uncert"
comb.uncert.wf.df$Species <- "Wolves"

comb.uncert.full <- rbind(comb.uncert.bb.df,comb.uncert.ml.df,comb.uncert.co.df,
                          comb.uncert.rp.df,comb.uncert.mp.df,comb.uncert.ug.df,
                          comb.uncert.wf.df)

comb <- ggplot()+
  geom_boxplot(data = comb.uncert.full, aes(uncert,Species,fill=Species))+
  xlab("Relative change in population")+
  ggtitle("a) All grizzly bear effects")+
  theme_classic()+
  geom_vline(xintercept = 0)+
  scale_fill_manual(values = c(alpha("firebrick3",0.9),alpha("darkgoldenrod",0.9),alpha("forestgreen",0.9),alpha("chocolate4",0.9),alpha("deepskyblue4",0.9),alpha("moccasin",0.9),alpha("darkgrey",0.9)))+
  xlim(-0.35,0.1)+
  theme(legend.position = "none")

comb

#grid.arrange(arrangeGrob(comb,ncol = 1),
#             arrangeGrob(dir,hunt,scav,herb,ncol=2),
#             widths=c(1,2))

grid.arrange(arrangeGrob(comb,ncol = 1),
             arrangeGrob(dir,hunt,scav,herb,ncol=2),
             heights=c(1,2))

grid.arrange(arrangeGrob(comb,ncol = 1),
             arrangeGrob(dir.f,hunt.f,scav.f,herb.f,ncol=2),
             heights=c(1,2))



###




