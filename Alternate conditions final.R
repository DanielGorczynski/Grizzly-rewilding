#############################################################################################
##MPG Ranch alternate bottom up and top down effects including grizzlies
#############################################################################################

##NOTE
##Need membership functions and starting values for other nodes from "MPG Ranch.R"

#############################################################################################
# Doubled veg crew under full griz effect variation


sims <- 10000 # 10000

veg.uncert.ml <- rep(NA,sims)
veg.uncert.bb <- rep(NA,sims)
veg.uncert.co <- rep(NA,sims)
veg.uncert.rp <- rep(NA,sims)
veg.uncert.mp <- rep(NA,sims)
veg.uncert.ug <- rep(NA,sims)
veg.uncert.wf <- rep(NA,sims)

##Set alternate values for hunting/veg
hunters.s <- start_fxn(hunters.mf, 52) #52
veg.crew.sd <- start_fxn(veg.crew.mf, 24) #12
precipitation.s <- start_fxn(precipitation.mf, 12) #12

for (i in 1:sims){
  
  w <- matrix(0,nrow=24,ncol=24)
  species <- c("shrub","precipitation","exotic forb","elk/deer","mesopredators",
               "invertebrate herbivores","invertebrate predators","forb","song birds",
               "grass","scavenging birds","small mammals","disturbance","hunters",
               "mountain lions","coyotes","veg crew","black bear",
               "exotic grass","snakes","pollinators","grizzlies","carcasses","wolves")
  dimnames(w) <- list(species,species)
  
  ##Parameterize interactions
  
  
  ##Shrub
  w["elk/deer","shrub"] <- 0.45
  w["mesopredators","shrub"] <- 0.2
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
  
  #Mesopredators
  w["invertebrate herbivores","mesopredators"] <- -0.1
  w["song birds","mesopredators"] <- -0.45
  w["scavenging birds","mesopredators"] <- 0.1
  w["small mammals","mesopredators"] <- -0.5
  w["disturbance","mesopredators"] <- 0.2
  w["snakes","mesopredators"] <- -0.3
  w["carcasses","mesopredators"] <- -0.05
  
  #Invertebrate herbivores
  w["shrub","invertebrate herbivores"] <- -0.2
  w["mesopredators","invertebrate herbivores"] <- 0.5
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
  w["mesopredators","song birds"] <- 0.25
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
  w["mesopredators","scavenging birds"] <- -0.25
  w["invertebrate herbivores","scavenging birds"] <- -0.1
  w["song birds","scavenging birds"] <- -0.25
  w["small mammals","scavenging birds"] <- -0.5
  w["snakes","scavenging birds"] <- -0.35
  w["carcasses","scavenging birds"] <- -0.75
  
  #Small mammals
  w["shrub","small mammals"] <- -0.5
  w["mesopredators","small mammals"] <- 0.5
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
  #w["mesopredators","mountain lions"] <- 0.35
  #w["scavenging birds","mountain lions"] <- 0.2
  w["small mammals","mountain lions"] <- -0.15
  w["coyotes","mountain lions"] <- -0.25
  w["carcasses","mountain lions"] <- 0.333
  
  #Coyotes
  w["elk/deer","coyotes"] <- -0.25
  w["mesopredators","coyotes"] <- -0.4
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
  w["mesopredators","black bear"] <- -0.1
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
  w["mesopredators","snakes"] <- 0.15
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
  w["elk/deer","grizzlies"] <- runif(1,-0.2,0) #Variable interaction detailed above in FCM function
  w["mountain lions","grizzlies"] <- runif(1,-0.1,0) #-0.1
  w["black bear","grizzlies"] <- runif(1,-0.3,0) #-0.1
  w["coyotes","grizzlies"] <- runif(1,-0.2,0) #-0.1
  w["carcasses","grizzlies"] <- runif(1,-0.4,0) #Variable interaction detailed above in FCM function
  w["shrub","grizzlies"] <- runif(1,-0.01,0) #-0.005
  w["forb","grizzlies"] <- runif(1,-0.02,0) #-0.01
  w["exotic forb","grizzlies"] <- runif(1,-0.02,0) #-0.01
  w["grass","grizzlies"] <- runif(1,-0.01,0) #-0.005
  
  
  ##Carcasses
  w["grizzlies","carcasses"] <-  0.2 #Variable interaction detailed above in FCM function
  w["mesopredators","carcasses"] <-  0.2
  w["coyotes","carcasses"] <-  0.25
  w["black bear","carcasses"] <-  0.2
  w["scavenging birds","carcasses"] <-  0.4 
  w["wolves","carcasses"] <-  0.1
  
  ##Wolves
  w["elk/deer","wolves"] <- -0.3
  w["mountain lions","wolves"] <- -0.025
  w["mesopredators","wolves"] <- -0.05
  w["coyotes","wolves"] <- -0.15
  w["carcasses","wolves"] <- 0.333 
  w["small mammals","wolves"] <- -0.05
  
  ##Need membership functions from above
  
  ## RUN CODE
  
  result <- data.frame(matrix(NA,11,24))
  grizzlies <- seq(0,1,0.1)
  result[,22] <- grizzlies
  colnames(result) <- c("shrub","precipitation","exotic forb","elk/deer","mesopredators",
                        "invertebrate herbivores","invertebrate predators","forb","song birds",
                        "grass","scavenging birds","small mammals","disturbance","hunters",
                        "mountain lions","coyotes","veg crew","black bear",
                        "exotic grass","snakes","pollinators","grizzlies","carcasses","wolves")
  
 

  for(m in 1:nrow(result)){
    
    
    
    s <- c(shrub.s, precipitation.s, exotic.forb.s, elk.deer.s, mesopredators.s,
           invertebrate.herbivores.s, invertebrate.predators.s, forb.s, song.birds.s,
           grass.s, scavenging.birds.s, small.mammals.s, disturbance.s, hunters.s,
           mountain.lions.s, coyotes.s, veg.crew.sd, black.bear.s,
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
  veg.uncert.ml[i] <- (result$`mountain lions`[11]-result$`mountain lions`[1])/result$`mountain lions`[1]
  veg.uncert.bb[i] <- (result$`black bear`[11]-result$`black bear`[1])/result$`black bear`[1]
  veg.uncert.co[i] <- (result$coyotes[11]-result$coyotes[1])/result$coyotes[1]
  veg.uncert.rp[i] <- (result$`scavenging birds`[11]-result$`scavenging birds`[1])/result$`scavenging birds`[1]
  veg.uncert.mp[i] <- (result$mesopredators[11]-result$mesopredators[1])/result$mesopredators[1]
  veg.uncert.ug[i] <- (result$`elk/deer`[11]-result$`elk/deer`[1])/result$`elk/deer`[1]
  veg.uncert.wf[i] <- (result$wolves[11]-result$wolves[1])/result$wolves[1]
  print(i)
}

# black bear results 
mean(veg.uncert.bb) 
quantile(veg.uncert.bb, c(0.05,0.95)) 
veg.uncert.bb.df <- as.data.frame(veg.uncert.bb)
colnames(veg.uncert.bb.df) <- "uncert"
veg.uncert.bb.df$Species <- "Black bears"

# mountain lion results 
mean(veg.uncert.ml) 
quantile(veg.uncert.ml, c(0.05,0.95)) 
veg.uncert.ml.df <- as.data.frame(veg.uncert.ml)
colnames(veg.uncert.ml.df) <- "uncert"
veg.uncert.ml.df$Species <- "Mountain lions"

# coyote results 
mean(veg.uncert.co) 
quantile(veg.uncert.co, c(0.05,0.95)) 
veg.uncert.co.df <- as.data.frame(veg.uncert.co)
colnames(veg.uncert.co.df) <- "uncert"
veg.uncert.co.df$Species <- "Coyotes"

# scavenging bird results 
mean(veg.uncert.rp) 
quantile(veg.uncert.rp, c(0.05,0.95)) 
veg.uncert.rp.df <- as.data.frame(veg.uncert.rp)
colnames(veg.uncert.rp.df) <- "uncert"
veg.uncert.rp.df$Species <- "Scavenging birds"

# mesopredator results 
mean(veg.uncert.mp) 
quantile(veg.uncert.mp, c(0.05,0.95)) 
veg.uncert.mp.df <- as.data.frame(veg.uncert.mp)
colnames(veg.uncert.mp.df) <- "uncert"
veg.uncert.mp.df$Species <- "Mesopredators"

# elk/deer results
mean(veg.uncert.ug) 
quantile(veg.uncert.ug, c(0.05,0.95)) 
veg.uncert.ug.df <- as.data.frame(veg.uncert.ug)
colnames(veg.uncert.ug.df) <- "uncert"
veg.uncert.ug.df$Species <- "Elk/Deer"

# wolves results
mean(veg.uncert.wf) 
quantile(veg.uncert.wf, c(0.05,0.95)) 
veg.uncert.wf.df <- as.data.frame(veg.uncert.wf)
colnames(veg.uncert.wf.df) <- "uncert"
veg.uncert.wf.df$Species <- "Wolves"

veg.uncert.full <- rbind(veg.uncert.bb.df,veg.uncert.ml.df,veg.uncert.co.df,
                         veg.uncert.rp.df,veg.uncert.mp.df,veg.uncert.ug.df,
                         veg.uncert.wf.df)

veg <- ggplot()+
  geom_boxplot(data = veg.uncert.full, aes(uncert,Species,color=Species))+
  xlab("Relative change in population")+
  ggtitle("All grizzly effects with doubled veg crew")+
  theme_classic()+
  geom_vline(xintercept = 0)+
  xlim(-1,0.4)+
  theme(legend.position = "none")

#veg



#############################################################################################
# Zero veg crew under full griz effect variation


sims <- 10000 # 10000

veg.0.uncert.ml <- rep(NA,sims)
veg.0.uncert.bb <- rep(NA,sims)
veg.0.uncert.co <- rep(NA,sims)
veg.0.uncert.rp <- rep(NA,sims)
veg.0.uncert.mp <- rep(NA,sims)
veg.0.uncert.ug <- rep(NA,sims)
veg.0.uncert.wf <- rep(NA,sims)

##Set alternate values for hunting/veg
hunters.s <- start_fxn(hunters.mf, 52) #52
veg.crew.sz <- start_fxn(veg.crew.mf, 0) #12
precipitation.s <- start_fxn(precipitation.mf, 12) #12

for (i in 1:sims){
  
  w <- matrix(0,nrow=24,ncol=24)
  species <- c("shrub","precipitation","exotic forb","elk/deer","mesopredators",
               "invertebrate herbivores","invertebrate predators","forb","song birds",
               "grass","scavenging birds","small mammals","disturbance","hunters",
               "mountain lions","coyotes","veg crew","black bear",
               "exotic grass","snakes","pollinators","grizzlies","carcasses","wolves")
  dimnames(w) <- list(species,species)
  
  ##Parameterize interactions
  
  
  ##Shrub
  w["elk/deer","shrub"] <- 0.45
  w["mesopredators","shrub"] <- 0.2
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
  
  #Mesopredators
  w["invertebrate herbivores","mesopredators"] <- -0.1
  w["song birds","mesopredators"] <- -0.45
  w["scavenging birds","mesopredators"] <- 0.1
  w["small mammals","mesopredators"] <- -0.5
  w["disturbance","mesopredators"] <- 0.2
  w["snakes","mesopredators"] <- -0.3
  w["carcasses","mesopredators"] <- -0.05
  
  #Invertebrate herbivores
  w["shrub","invertebrate herbivores"] <- -0.2
  w["mesopredators","invertebrate herbivores"] <- 0.5
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
  w["mesopredators","song birds"] <- 0.25
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
  w["mesopredators","scavenging birds"] <- -0.25
  w["invertebrate herbivores","scavenging birds"] <- -0.1
  w["song birds","scavenging birds"] <- -0.25
  w["small mammals","scavenging birds"] <- -0.5
  w["snakes","scavenging birds"] <- -0.35
  w["carcasses","scavenging birds"] <- -0.75
  
  #Small mammals
  w["shrub","small mammals"] <- -0.5
  w["mesopredators","small mammals"] <- 0.5
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
  #w["mesopredators","mountain lions"] <- 0.35
  #w["scavenging birds","mountain lions"] <- 0.2
  w["small mammals","mountain lions"] <- -0.15
  w["coyotes","mountain lions"] <- -0.25
  w["carcasses","mountain lions"] <- 0.333
  
  #Coyotes
  w["elk/deer","coyotes"] <- -0.25
  w["mesopredators","coyotes"] <- -0.4
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
  w["mesopredators","black bear"] <- -0.1
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
  w["mesopredators","snakes"] <- 0.15
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
  w["elk/deer","grizzlies"] <- runif(1,-0.2,0) #Variable interaction detailed above in FCM function
  w["mountain lions","grizzlies"] <- runif(1,-0.1,0) #-0.1
  w["black bear","grizzlies"] <- runif(1,-0.3,0) #-0.1
  w["coyotes","grizzlies"] <- runif(1,-0.2,0) #-0.1
  w["carcasses","grizzlies"] <- runif(1,-0.4,0) #Variable interaction detailed above in FCM function
  w["shrub","grizzlies"] <- runif(1,-0.01,0) #-0.005
  w["forb","grizzlies"] <- runif(1,-0.02,0) #-0.01
  w["exotic forb","grizzlies"] <- runif(1,-0.02,0) #-0.01
  w["grass","grizzlies"] <- runif(1,-0.01,0) #-0.005
  
  
  ##Carcasses
  w["grizzlies","carcasses"] <-  0.2 #Variable interaction detailed above in FCM function
  w["mesopredators","carcasses"] <-  0.2
  w["coyotes","carcasses"] <-  0.25
  w["black bear","carcasses"] <-  0.2
  w["scavenging birds","carcasses"] <-  0.4 
  w["wolves","carcasses"] <-  0.1
  
  ##Wolves
  w["elk/deer","wolves"] <- -0.3
  w["mountain lions","wolves"] <- -0.025
  w["mesopredators","wolves"] <- -0.05
  w["coyotes","wolves"] <- -0.15
  w["carcasses","wolves"] <- 0.333 
  w["small mammals","wolves"] <- -0.05
  
  ##Need membership functions from above
  
  ## RUN CODE
  
  result <- data.frame(matrix(NA,11,24))
  grizzlies <- seq(0,1,0.1)
  result[,22] <- grizzlies
  colnames(result) <- c("shrub","precipitation","exotic forb","elk/deer","mesopredators",
                        "invertebrate herbivores","invertebrate predators","forb","song birds",
                        "grass","scavenging birds","small mammals","disturbance","hunters",
                        "mountain lions","coyotes","veg crew","black bear",
                        "exotic grass","snakes","pollinators","grizzlies","carcasses","wolves")
  

  for(m in 1:nrow(result)){
    

    
    s <- c(shrub.s, precipitation.s, exotic.forb.s, elk.deer.s, mesopredators.s,
           invertebrate.herbivores.s, invertebrate.predators.s, forb.s, song.birds.s,
           grass.s, scavenging.birds.s, small.mammals.s, disturbance.s, hunters.s,
           mountain.lions.s, coyotes.s, veg.crew.sz, black.bear.s,
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
  veg.0.uncert.ml[i] <- (result$`mountain lions`[11]-result$`mountain lions`[1])/result$`mountain lions`[1]
  veg.0.uncert.bb[i] <- (result$`black bear`[11]-result$`black bear`[1])/result$`black bear`[1]
  veg.0.uncert.co[i] <- (result$coyotes[11]-result$coyotes[1])/result$coyotes[1]
  veg.0.uncert.rp[i] <- (result$`scavenging birds`[11]-result$`scavenging birds`[1])/result$`scavenging birds`[1]
  veg.0.uncert.mp[i] <- (result$mesopredators[11]-result$mesopredators[1])/result$mesopredators[1]
  veg.0.uncert.ug[i] <- (result$`elk/deer`[11]-result$`elk/deer`[1])/result$`elk/deer`[1]
  veg.0.uncert.wf[i] <- (result$wolves[11]-result$wolves[1])/result$wolves[1]
  print(i)
}

# black bear results 
mean(veg.0.uncert.bb) 
quantile(veg.0.uncert.bb, c(0.05,0.95)) 
veg.0.uncert.bb.df <- as.data.frame(veg.0.uncert.bb)
colnames(veg.0.uncert.bb.df) <- "uncert"
veg.0.uncert.bb.df$Species <- "Black bears"

# mountain lion results 
mean(veg.0.uncert.ml) 
quantile(veg.0.uncert.ml, c(0.05,0.95)) 
veg.0.uncert.ml.df <- as.data.frame(veg.0.uncert.ml)
colnames(veg.0.uncert.ml.df) <- "uncert"
veg.0.uncert.ml.df$Species <- "Mountain lions"

# coyote results 
mean(veg.0.uncert.co) 
quantile(veg.0.uncert.co, c(0.05,0.95)) 
veg.0.uncert.co.df <- as.data.frame(veg.0.uncert.co)
colnames(veg.0.uncert.co.df) <- "uncert"
veg.0.uncert.co.df$Species <- "Coyotes"

# scavenging bird results 
mean(veg.0.uncert.rp) 
quantile(veg.0.uncert.rp, c(0.05,0.95)) 
veg.0.uncert.rp.df <- as.data.frame(veg.0.uncert.rp)
colnames(veg.0.uncert.rp.df) <- "uncert"
veg.0.uncert.rp.df$Species <- "Scavenging birds"

# mesopredator results 
mean(veg.0.uncert.mp) 
quantile(veg.0.uncert.mp, c(0.05,0.95)) 
veg.0.uncert.mp.df <- as.data.frame(veg.0.uncert.mp)
colnames(veg.0.uncert.mp.df) <- "uncert"
veg.0.uncert.mp.df$Species <- "Mesopredators"

# elk/deer results
mean(veg.0.uncert.ug) 
quantile(veg.0.uncert.ug, c(0.05,0.95)) 
veg.0.uncert.ug.df <- as.data.frame(veg.0.uncert.ug)
colnames(veg.0.uncert.ug.df) <- "uncert"
veg.0.uncert.ug.df$Species <- "Elk/Deer"

# wolves results
mean(veg.0.uncert.wf) 
quantile(veg.0.uncert.wf, c(0.05,0.95)) 
veg.0.uncert.wf.df <- as.data.frame(veg.0.uncert.wf)
colnames(veg.0.uncert.wf.df) <- "uncert"
veg.0.uncert.wf.df$Species <- "Wolves"

veg.0.uncert.full <- rbind(veg.0.uncert.bb.df,veg.0.uncert.ml.df,veg.0.uncert.co.df,
                         veg.0.uncert.rp.df,veg.0.uncert.mp.df,veg.0.uncert.ug.df,
                         veg.0.uncert.wf.df)

veg.0 <- ggplot()+
  geom_boxplot(data = veg.0.uncert.full, aes(uncert,Species,color=Species))+
  xlab("Relative change in population")+
  ggtitle("All grizzly effects with zero veg crew")+
  theme_classic()+
  geom_vline(xintercept = 0)+
  xlim(-1,0.4)+
  theme(legend.position = "none")

#veg.0



#############################################################################################
# Doubled human hunting under full griz effect variation


sims <- 10000 # 10000

hum.uncert.ml <- rep(NA,sims)
hum.uncert.bb <- rep(NA,sims)
hum.uncert.co <- rep(NA,sims)
hum.uncert.rp <- rep(NA,sims)
hum.uncert.mp <- rep(NA,sims)
hum.uncert.ug <- rep(NA,sims)
hum.uncert.wf <- rep(NA,sims)

##Set alternate values for hunting/veg
hunters.sd <- start_fxn(hunters.mf, 104) #52
veg.crew.s <- start_fxn(veg.crew.mf, 12) #12
precipitation.s <- start_fxn(precipitation.mf, 12) #12

for (i in 1:sims){
  
  w <- matrix(0,nrow=24,ncol=24)
  species <- c("shrub","precipitation","exotic forb","elk/deer","mesopredators",
               "invertebrate herbivores","invertebrate predators","forb","song birds",
               "grass","scavenging birds","small mammals","disturbance","hunters",
               "mountain lions","coyotes","veg crew","black bear",
               "exotic grass","snakes","pollinators","grizzlies","carcasses","wolves")
  dimnames(w) <- list(species,species)
  
  ##Parameterize interactions
  
  
  ##Shrub
  w["elk/deer","shrub"] <- 0.45
  w["mesopredators","shrub"] <- 0.2
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
  
  #Mesopredators
  w["invertebrate herbivores","mesopredators"] <- -0.1
  w["song birds","mesopredators"] <- -0.45
  w["scavenging birds","mesopredators"] <- 0.1
  w["small mammals","mesopredators"] <- -0.5
  w["disturbance","mesopredators"] <- 0.2
  w["snakes","mesopredators"] <- -0.3
  w["carcasses","mesopredators"] <- -0.05
  
  #Invertebrate herbivores
  w["shrub","invertebrate herbivores"] <- -0.2
  w["mesopredators","invertebrate herbivores"] <- 0.5
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
  w["mesopredators","song birds"] <- 0.25
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
  w["mesopredators","scavenging birds"] <- -0.25
  w["invertebrate herbivores","scavenging birds"] <- -0.1
  w["song birds","scavenging birds"] <- -0.25
  w["small mammals","scavenging birds"] <- -0.5
  w["snakes","scavenging birds"] <- -0.35
  w["carcasses","scavenging birds"] <- -0.75
  
  #Small mammals
  w["shrub","small mammals"] <- -0.5
  w["mesopredators","small mammals"] <- 0.5
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
  #w["mesopredators","mountain lions"] <- 0.35
  #w["scavenging birds","mountain lions"] <- 0.2
  w["small mammals","mountain lions"] <- -0.15
  w["coyotes","mountain lions"] <- -0.25
  w["carcasses","mountain lions"] <- 0.333
  
  #Coyotes
  w["elk/deer","coyotes"] <- -0.25
  w["mesopredators","coyotes"] <- -0.4
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
  w["mesopredators","black bear"] <- -0.1
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
  w["mesopredators","snakes"] <- 0.15
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
  w["elk/deer","grizzlies"] <- runif(1,-0.2,0) #Variable interaction detailed above in FCM function
  w["mountain lions","grizzlies"] <- runif(1,-0.1,0) #-0.1
  w["black bear","grizzlies"] <- runif(1,-0.3,0) #-0.1
  w["coyotes","grizzlies"] <- runif(1,-0.2,0) #-0.1
  w["carcasses","grizzlies"] <- runif(1,-0.4,0) #Variable interaction detailed above in FCM function
  w["shrub","grizzlies"] <- runif(1,-0.01,0) #-0.005
  w["forb","grizzlies"] <- runif(1,-0.02,0) #-0.01
  w["exotic forb","grizzlies"] <- runif(1,-0.02,0) #-0.01
  w["grass","grizzlies"] <- runif(1,-0.01,0) #-0.005
  
  
  ##Carcasses
  w["grizzlies","carcasses"] <-  0.2 #Variable interaction detailed above in FCM function
  w["mesopredators","carcasses"] <-  0.2
  w["coyotes","carcasses"] <-  0.25
  w["black bear","carcasses"] <-  0.2
  w["scavenging birds","carcasses"] <-  0.4 
  w["wolves","carcasses"] <-  0.1
  
  ##Wolves
  w["elk/deer","wolves"] <- -0.3
  w["mountain lions","wolves"] <- -0.025
  w["mesopredators","wolves"] <- -0.05
  w["coyotes","wolves"] <- -0.15
  w["carcasses","wolves"] <- 0.333 
  w["small mammals","wolves"] <- -0.05
  
  ##Need membership functions from above
  
  ## RUN CODE
  
  result <- data.frame(matrix(NA,11,24))
  grizzlies <- seq(0,1,0.1)
  result[,22] <- grizzlies
  colnames(result) <- c("shrub","precipitation","exotic forb","elk/deer","mesopredators",
                        "invertebrate herbivores","invertebrate predators","forb","song birds",
                        "grass","scavenging birds","small mammals","disturbance","hunters",
                        "mountain lions","coyotes","veg crew","black bear",
                        "exotic grass","snakes","pollinators","grizzlies","carcasses","wolves")
  

  for(m in 1:nrow(result)){
    

    
    s <- c(shrub.s, precipitation.s, exotic.forb.s, elk.deer.s, mesopredators.s,
           invertebrate.herbivores.s, invertebrate.predators.s, forb.s, song.birds.s,
           grass.s, scavenging.birds.s, small.mammals.s, disturbance.s, hunters.sd,
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
  hum.uncert.ml[i] <- (result$`mountain lions`[11]-result$`mountain lions`[1])/result$`mountain lions`[1]
  hum.uncert.bb[i] <- (result$`black bear`[11]-result$`black bear`[1])/result$`black bear`[1]
  hum.uncert.co[i] <- (result$coyotes[11]-result$coyotes[1])/result$coyotes[1]
  hum.uncert.rp[i] <- (result$`scavenging birds`[11]-result$`scavenging birds`[1])/result$`scavenging birds`[1]
  hum.uncert.mp[i] <- (result$mesopredators[11]-result$mesopredators[1])/result$mesopredators[1]
  hum.uncert.ug[i] <- (result$`elk/deer`[11]-result$`elk/deer`[1])/result$`elk/deer`[1]
  hum.uncert.wf[i] <- (result$wolves[11]-result$wolves[1])/result$wolves[1]
  print(i)
}

# black bear results 
mean(hum.uncert.bb) 
quantile(hum.uncert.bb, c(0.05,0.95)) 
hum.uncert.bb.df <- as.data.frame(hum.uncert.bb)
colnames(hum.uncert.bb.df) <- "uncert"
hum.uncert.bb.df$Species <- "Black bears"

# mountain lion results 
mean(hum.uncert.ml) 
quantile(hum.uncert.ml, c(0.05,0.95)) 
hum.uncert.ml.df <- as.data.frame(hum.uncert.ml)
colnames(hum.uncert.ml.df) <- "uncert"
hum.uncert.ml.df$Species <- "Mountain lions"

# coyote results 
mean(hum.uncert.co) 
quantile(hum.uncert.co, c(0.05,0.95)) 
hum.uncert.co.df <- as.data.frame(hum.uncert.co)
colnames(hum.uncert.co.df) <- "uncert"
hum.uncert.co.df$Species <- "Coyotes"

# scavenging bird results 
mean(hum.uncert.rp) 
quantile(hum.uncert.rp, c(0.05,0.95)) 
hum.uncert.rp.df <- as.data.frame(hum.uncert.rp)
colnames(hum.uncert.rp.df) <- "uncert"
hum.uncert.rp.df$Species <- "Scavenging birds"

# mesopredator results 
mean(hum.uncert.mp) 
quantile(hum.uncert.mp, c(0.05,0.95)) 
hum.uncert.mp.df <- as.data.frame(hum.uncert.mp)
colnames(hum.uncert.mp.df) <- "uncert"
hum.uncert.mp.df$Species <- "Mesopredators"

# elk/deer results
mean(hum.uncert.ug) 
quantile(hum.uncert.ug, c(0.05,0.95)) 
hum.uncert.ug.df <- as.data.frame(hum.uncert.ug)
colnames(hum.uncert.ug.df) <- "uncert"
hum.uncert.ug.df$Species <- "Elk/Deer"

# wolves results
mean(hum.uncert.wf) 
quantile(hum.uncert.wf, c(0.05,0.95)) 
hum.uncert.wf.df <- as.data.frame(hum.uncert.wf)
colnames(hum.uncert.wf.df) <- "uncert"
hum.uncert.wf.df$Species <- "Wolves"

hum.uncert.full <- rbind(hum.uncert.bb.df,hum.uncert.ml.df,hum.uncert.co.df,
                         hum.uncert.rp.df,hum.uncert.mp.df,hum.uncert.ug.df,
                         hum.uncert.wf.df)

hum <- ggplot()+
  geom_boxplot(data = hum.uncert.full, aes(uncert,Species,color=Species))+
  xlab("Relative change in population")+
  ggtitle("All grizzly effects with doubled human hunting")+
  theme_classic()+
  geom_vline(xintercept = 0)+
  xlim(-1,0.4)+
  theme(legend.position = "none")

#hum



#############################################################################################
# Zero human hunting under full griz effect variation


sims <- 10000 # 10000

hum.0.uncert.ml <- rep(NA,sims)
hum.0.uncert.bb <- rep(NA,sims)
hum.0.uncert.co <- rep(NA,sims)
hum.0.uncert.rp <- rep(NA,sims)
hum.0.uncert.mp <- rep(NA,sims)
hum.0.uncert.ug <- rep(NA,sims)
hum.0.uncert.wf <- rep(NA,sims)

##Set alternate values for hunting/veg
hunters.sz <- start_fxn(hunters.mf, 0) #52
veg.crew.s <- start_fxn(veg.crew.mf, 12) #12
precipitation.s <- start_fxn(precipitation.mf, 12) #12

for (i in 1:sims){
  
  w <- matrix(0,nrow=24,ncol=24)
  species <- c("shrub","precipitation","exotic forb","elk/deer","mesopredators",
               "invertebrate herbivores","invertebrate predators","forb","song birds",
               "grass","scavenging birds","small mammals","disturbance","hunters",
               "mountain lions","coyotes","veg crew","black bear",
               "exotic grass","snakes","pollinators","grizzlies","carcasses","wolves")
  dimnames(w) <- list(species,species)
  
  ##Parameterize interactions
  
  
  ##Shrub
  w["elk/deer","shrub"] <- 0.45
  w["mesopredators","shrub"] <- 0.2
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
  
  #Mesopredators
  w["invertebrate herbivores","mesopredators"] <- -0.1
  w["song birds","mesopredators"] <- -0.45
  w["scavenging birds","mesopredators"] <- 0.1
  w["small mammals","mesopredators"] <- -0.5
  w["disturbance","mesopredators"] <- 0.2
  w["snakes","mesopredators"] <- -0.3
  w["carcasses","mesopredators"] <- -0.05
  
  #Invertebrate herbivores
  w["shrub","invertebrate herbivores"] <- -0.2
  w["mesopredators","invertebrate herbivores"] <- 0.5
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
  w["mesopredators","song birds"] <- 0.25
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
  w["mesopredators","scavenging birds"] <- -0.25
  w["invertebrate herbivores","scavenging birds"] <- -0.1
  w["song birds","scavenging birds"] <- -0.25
  w["small mammals","scavenging birds"] <- -0.5
  w["snakes","scavenging birds"] <- -0.35
  w["carcasses","scavenging birds"] <- -0.75
  
  #Small mammals
  w["shrub","small mammals"] <- -0.5
  w["mesopredators","small mammals"] <- 0.5
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
  #w["mesopredators","mountain lions"] <- 0.35
  #w["scavenging birds","mountain lions"] <- 0.2
  w["small mammals","mountain lions"] <- -0.15
  w["coyotes","mountain lions"] <- -0.25
  w["carcasses","mountain lions"] <- 0.333
  
  #Coyotes
  w["elk/deer","coyotes"] <- -0.25
  w["mesopredators","coyotes"] <- -0.4
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
  w["mesopredators","black bear"] <- -0.1
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
  w["mesopredators","snakes"] <- 0.15
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
  w["elk/deer","grizzlies"] <- runif(1,-0.2,0) #Variable interaction detailed above in FCM function
  w["mountain lions","grizzlies"] <- runif(1,-0.1,0) #-0.1
  w["black bear","grizzlies"] <- runif(1,-0.3,0) #-0.1
  w["coyotes","grizzlies"] <- runif(1,-0.2,0) #-0.1
  w["carcasses","grizzlies"] <- runif(1,-0.4,0) #Variable interaction detailed above in FCM function
  w["shrub","grizzlies"] <- runif(1,-0.01,0) #-0.005
  w["forb","grizzlies"] <- runif(1,-0.02,0) #-0.01
  w["exotic forb","grizzlies"] <- runif(1,-0.02,0) #-0.01
  w["grass","grizzlies"] <- runif(1,-0.01,0) #-0.005
  
  
  ##Carcasses
  w["grizzlies","carcasses"] <-  0.2 #Variable interaction detailed above in FCM function
  w["mesopredators","carcasses"] <-  0.2
  w["coyotes","carcasses"] <-  0.25
  w["black bear","carcasses"] <-  0.2
  w["scavenging birds","carcasses"] <-  0.4 
  w["wolves","carcasses"] <-  0.1
  
  ##Wolves
  w["elk/deer","wolves"] <- -0.3
  w["mountain lions","wolves"] <- -0.025
  w["mesopredators","wolves"] <- -0.05
  w["coyotes","wolves"] <- -0.15
  w["carcasses","wolves"] <- 0.333 
  w["small mammals","wolves"] <- -0.05
  
  ##Need membership functions from above
  
  ## RUN CODE
  
  result <- data.frame(matrix(NA,11,24))
  grizzlies <- seq(0,1,0.1)
  result[,22] <- grizzlies
  colnames(result) <- c("shrub","precipitation","exotic forb","elk/deer","mesopredators",
                        "invertebrate herbivores","invertebrate predators","forb","song birds",
                        "grass","scavenging birds","small mammals","disturbance","hunters",
                        "mountain lions","coyotes","veg crew","black bear",
                        "exotic grass","snakes","pollinators","grizzlies","carcasses","wolves")

  for(m in 1:nrow(result)){
    

    
    s <- c(shrub.s, precipitation.s, exotic.forb.s, elk.deer.s, mesopredators.s,
           invertebrate.herbivores.s, invertebrate.predators.s, forb.s, song.birds.s,
           grass.s, scavenging.birds.s, small.mammals.s, disturbance.s, hunters.sz,
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
  hum.0.uncert.ml[i] <- (result$`mountain lions`[11]-result$`mountain lions`[1])/result$`mountain lions`[1]
  hum.0.uncert.bb[i] <- (result$`black bear`[11]-result$`black bear`[1])/result$`black bear`[1]
  hum.0.uncert.co[i] <- (result$coyotes[11]-result$coyotes[1])/result$coyotes[1]
  hum.0.uncert.rp[i] <- (result$`scavenging birds`[11]-result$`scavenging birds`[1])/result$`scavenging birds`[1]
  hum.0.uncert.mp[i] <- (result$mesopredators[11]-result$mesopredators[1])/result$mesopredators[1]
  hum.0.uncert.ug[i] <- (result$`elk/deer`[11]-result$`elk/deer`[1])/result$`elk/deer`[1]
  hum.0.uncert.wf[i] <- (result$wolves[11]-result$wolves[1])/result$wolves[1]
  print(i)
}

# black bear results 
mean(hum.0.uncert.bb) 
quantile(hum.0.uncert.bb, c(0.05,0.95)) 
hum.0.uncert.bb.df <- as.data.frame(hum.0.uncert.bb)
colnames(hum.0.uncert.bb.df) <- "uncert"
hum.0.uncert.bb.df$Species <- "Black bears"

# mountain lion results 
mean(hum.0.uncert.ml) 
quantile(hum.0.uncert.ml, c(0.05,0.95)) 
hum.0.uncert.ml.df <- as.data.frame(hum.0.uncert.ml)
colnames(hum.0.uncert.ml.df) <- "uncert"
hum.0.uncert.ml.df$Species <- "Mountain lions"

# coyote results 
mean(hum.0.uncert.co) 
quantile(hum.0.uncert.co, c(0.05,0.95)) 
hum.0.uncert.co.df <- as.data.frame(hum.0.uncert.co)
colnames(hum.0.uncert.co.df) <- "uncert"
hum.0.uncert.co.df$Species <- "Coyotes"

# scavenging bird results 
mean(hum.0.uncert.rp) 
quantile(hum.0.uncert.rp, c(0.05,0.95)) 
hum.0.uncert.rp.df <- as.data.frame(hum.0.uncert.rp)
colnames(hum.0.uncert.rp.df) <- "uncert"
hum.0.uncert.rp.df$Species <- "Scavenging birds"

# mesopredator results 
mean(hum.0.uncert.mp) 
quantile(hum.0.uncert.mp, c(0.05,0.95)) 
hum.0.uncert.mp.df <- as.data.frame(hum.0.uncert.mp)
colnames(hum.0.uncert.mp.df) <- "uncert"
hum.0.uncert.mp.df$Species <- "Mesopredators"

# elk/deer results
mean(hum.0.uncert.ug) 
quantile(hum.0.uncert.ug, c(0.05,0.95)) 
hum.0.uncert.ug.df <- as.data.frame(hum.0.uncert.ug)
colnames(hum.0.uncert.ug.df) <- "uncert"
hum.0.uncert.ug.df$Species <- "Elk/Deer"

# wolves results
mean(hum.0.uncert.wf) 
quantile(hum.0.uncert.wf, c(0.05,0.95)) 
hum.0.uncert.wf.df <- as.data.frame(hum.0.uncert.wf)
colnames(hum.0.uncert.wf.df) <- "uncert"
hum.0.uncert.wf.df$Species <- "Wolves"

hum.0.uncert.full <- rbind(hum.0.uncert.bb.df,hum.0.uncert.ml.df,hum.0.uncert.co.df,
                           hum.0.uncert.rp.df,hum.0.uncert.mp.df,hum.0.uncert.ug.df,
                           hum.0.uncert.wf.df)

hum.0 <- ggplot()+
  geom_boxplot(data = hum.0.uncert.full, aes(uncert,Species,color=Species))+
  xlab("Relative change in population")+
  ggtitle("All grizzly effects with zero human hunting")+
  theme_classic()+
  geom_vline(xintercept = 0)+
  xlim(-1,0.4)+
  theme(legend.position = "none")





#############################################################################################
### Precipitation


#############################################################################################
# eleven mm precipitation under full griz effect variation


sims <- 10000 # 10000

ele.prec.uncert.ml <- rep(NA,sims)
ele.prec.uncert.bb <- rep(NA,sims)
ele.prec.uncert.co <- rep(NA,sims)
ele.prec.uncert.rp <- rep(NA,sims)
ele.prec.uncert.mp <- rep(NA,sims)
ele.prec.uncert.ug <- rep(NA,sims)
ele.prec.uncert.wf <- rep(NA,sims)

##Set alternate values for hunting/veg
hunters.s <- start_fxn(hunters.mf, 52) #52
veg.crew.s <- start_fxn(veg.crew.mf, 12) #12
precipitation.se <- start_fxn(precipitation.mf, 11) #12

for (i in 1:sims){
  
  w <- matrix(0,nrow=24,ncol=24)
  species <- c("shrub","precipitation","exotic forb","elk/deer","mesopredators",
               "invertebrate herbivores","invertebrate predators","forb","song birds",
               "grass","scavenging birds","small mammals","disturbance","hunters",
               "mountain lions","coyotes","veg crew","black bear",
               "exotic grass","snakes","pollinators","grizzlies","carcasses","wolves")
  dimnames(w) <- list(species,species)
  
  ##Parameterize interactions
  
  
  ##Shrub
  w["elk/deer","shrub"] <- 0.45
  w["mesopredators","shrub"] <- 0.2
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
  
  #Mesopredators
  w["invertebrate herbivores","mesopredators"] <- -0.1
  w["song birds","mesopredators"] <- -0.45
  w["scavenging birds","mesopredators"] <- 0.1
  w["small mammals","mesopredators"] <- -0.5
  w["disturbance","mesopredators"] <- 0.2
  w["snakes","mesopredators"] <- -0.3
  w["carcasses","mesopredators"] <- -0.05
  
  #Invertebrate herbivores
  w["shrub","invertebrate herbivores"] <- -0.2
  w["mesopredators","invertebrate herbivores"] <- 0.5
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
  w["mesopredators","song birds"] <- 0.25
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
  w["mesopredators","scavenging birds"] <- -0.25
  w["invertebrate herbivores","scavenging birds"] <- -0.1
  w["song birds","scavenging birds"] <- -0.25
  w["small mammals","scavenging birds"] <- -0.5
  w["snakes","scavenging birds"] <- -0.35
  w["carcasses","scavenging birds"] <- -0.75
  
  #Small mammals
  w["shrub","small mammals"] <- -0.5
  w["mesopredators","small mammals"] <- 0.5
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
  #w["mesopredators","mountain lions"] <- 0.35
  #w["scavenging birds","mountain lions"] <- 0.2
  w["small mammals","mountain lions"] <- -0.15
  w["coyotes","mountain lions"] <- -0.25
  w["carcasses","mountain lions"] <- 0.333
  
  #Coyotes
  w["elk/deer","coyotes"] <- -0.25
  w["mesopredators","coyotes"] <- -0.4
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
  w["mesopredators","black bear"] <- -0.1
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
  w["mesopredators","snakes"] <- 0.15
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
  w["elk/deer","grizzlies"] <- runif(1,-0.2,0) #Variable interaction detailed above in FCM function
  w["mountain lions","grizzlies"] <- runif(1,-0.1,0) #-0.1
  w["black bear","grizzlies"] <- runif(1,-0.3,0) #-0.1
  w["coyotes","grizzlies"] <- runif(1,-0.2,0) #-0.1
  w["carcasses","grizzlies"] <- runif(1,-0.4,0) #Variable interaction detailed above in FCM function
  w["shrub","grizzlies"] <- runif(1,-0.01,0) #-0.005
  w["forb","grizzlies"] <- runif(1,-0.02,0) #-0.01
  w["exotic forb","grizzlies"] <- runif(1,-0.02,0) #-0.01
  w["grass","grizzlies"] <- runif(1,-0.01,0) #-0.005
  
  
  ##Carcasses
  w["grizzlies","carcasses"] <-  0.2 #Variable interaction detailed above in FCM function
  w["mesopredators","carcasses"] <-  0.2
  w["coyotes","carcasses"] <-  0.25
  w["black bear","carcasses"] <-  0.2
  w["scavenging birds","carcasses"] <-  0.4 
  w["wolves","carcasses"] <-  0.1
  
  ##Wolves
  w["elk/deer","wolves"] <- -0.3
  w["mountain lions","wolves"] <- -0.025
  w["mesopredators","wolves"] <- -0.05
  w["coyotes","wolves"] <- -0.15
  w["carcasses","wolves"] <- 0.333 
  w["small mammals","wolves"] <- -0.05
  
  ##Need membership functions from above
  
  ## RUN CODE
  
  result <- data.frame(matrix(NA,11,24))
  grizzlies <- seq(0,1,0.1)
  result[,22] <- grizzlies
  colnames(result) <- c("shrub","precipitation","exotic forb","elk/deer","mesopredators",
                        "invertebrate herbivores","invertebrate predators","forb","song birds",
                        "grass","scavenging birds","small mammals","disturbance","hunters",
                        "mountain lions","coyotes","veg crew","black bear",
                        "exotic grass","snakes","pollinators","grizzlies","carcasses","wolves")
  

  for(m in 1:nrow(result)){
    

    
    s <- c(shrub.s, precipitation.se, exotic.forb.s, elk.deer.s, mesopredators.s,
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
  ele.prec.uncert.ml[i] <- (result$`mountain lions`[11]-result$`mountain lions`[1])/result$`mountain lions`[1]
  ele.prec.uncert.bb[i] <- (result$`black bear`[11]-result$`black bear`[1])/result$`black bear`[1]
  ele.prec.uncert.co[i] <- (result$coyotes[11]-result$coyotes[1])/result$coyotes[1]
  ele.prec.uncert.rp[i] <- (result$`scavenging birds`[11]-result$`scavenging birds`[1])/result$`scavenging birds`[1]
  ele.prec.uncert.mp[i] <- (result$mesopredators[11]-result$mesopredators[1])/result$mesopredators[1]
  ele.prec.uncert.ug[i] <- (result$`elk/deer`[11]-result$`elk/deer`[1])/result$`elk/deer`[1]
  ele.prec.uncert.wf[i] <- (result$wolves[11]-result$wolves[1])/result$wolves[1]
  print(i)
}

# black bear results 
mean(ele.prec.uncert.bb) 
quantile(ele.prec.uncert.bb, c(0.05,0.95)) 
ele.prec.uncert.bb.df <- as.data.frame(ele.prec.uncert.bb)
colnames(ele.prec.uncert.bb.df) <- "uncert"
ele.prec.uncert.bb.df$Species <- "Black bears"

# mountain lion results 
mean(ele.prec.uncert.ml) 
quantile(ele.prec.uncert.ml, c(0.05,0.95)) 
ele.prec.uncert.ml.df <- as.data.frame(ele.prec.uncert.ml)
colnames(ele.prec.uncert.ml.df) <- "uncert"
ele.prec.uncert.ml.df$Species <- "Mountain lions"

# coyote results 
mean(ele.prec.uncert.co) 
quantile(ele.prec.uncert.co, c(0.05,0.95)) 
ele.prec.uncert.co.df <- as.data.frame(ele.prec.uncert.co)
colnames(ele.prec.uncert.co.df) <- "uncert"
ele.prec.uncert.co.df$Species <- "Coyotes"

# scavenging bird results 
mean(ele.prec.uncert.rp) 
quantile(ele.prec.uncert.rp, c(0.05,0.95)) 
ele.prec.uncert.rp.df <- as.data.frame(ele.prec.uncert.rp)
colnames(ele.prec.uncert.rp.df) <- "uncert"
ele.prec.uncert.rp.df$Species <- "Scavenging birds"

# mesopredator results 
mean(ele.prec.uncert.mp) 
quantile(ele.prec.uncert.mp, c(0.05,0.95)) 
ele.prec.uncert.mp.df <- as.data.frame(ele.prec.uncert.mp)
colnames(ele.prec.uncert.mp.df) <- "uncert"
ele.prec.uncert.mp.df$Species <- "Mesopredators"

# elk/deer results
mean(ele.prec.uncert.ug) 
quantile(ele.prec.uncert.ug, c(0.05,0.95)) 
ele.prec.uncert.ug.df <- as.data.frame(ele.prec.uncert.ug)
colnames(ele.prec.uncert.ug.df) <- "uncert"
ele.prec.uncert.ug.df$Species <- "Elk/Deer"

# wolves results
mean(ele.prec.uncert.wf) 
quantile(ele.prec.uncert.wf, c(0.05,0.95)) 
ele.prec.uncert.wf.df <- as.data.frame(ele.prec.uncert.wf)
colnames(ele.prec.uncert.wf.df) <- "uncert"
ele.prec.uncert.wf.df$Species <- "Wolves"

ele.prec.uncert.full <- rbind(ele.prec.uncert.bb.df,ele.prec.uncert.ml.df,ele.prec.uncert.co.df,
                         ele.prec.uncert.rp.df,ele.prec.uncert.mp.df,ele.prec.uncert.ug.df,
                         ele.prec.uncert.wf.df)

ele.prec <- ggplot()+
  geom_boxplot(data = ele.prec.uncert.full, aes(uncert,Species,color=Species))+
  xlab("Relative change in population")+
  ggtitle("All grizzly effects with 11 mm precipitation")+
  theme_classic()+
  geom_vline(xintercept = 0)+
  xlim(-1,0.4)+
  theme(legend.position = "none")

#ele.prec




#############################################################################################
# ten mm precipitation under full griz effect variation


sims <- 10000 # 10000

ten.prec.uncert.ml <- rep(NA,sims)
ten.prec.uncert.bb <- rep(NA,sims)
ten.prec.uncert.co <- rep(NA,sims)
ten.prec.uncert.rp <- rep(NA,sims)
ten.prec.uncert.mp <- rep(NA,sims)
ten.prec.uncert.ug <- rep(NA,sims)
ten.prec.uncert.wf <- rep(NA,sims)

##Set alternate values for hunting/veg
hunters.s <- start_fxn(hunters.mf, 52) #52
veg.crew.s <- start_fxn(veg.crew.mf, 12) #12
precipitation.st <- start_fxn(precipitation.mf, 10) #12

for (i in 1:sims){
  
  w <- matrix(0,nrow=24,ncol=24)
  species <- c("shrub","precipitation","exotic forb","elk/deer","mesopredators",
               "invertebrate herbivores","invertebrate predators","forb","song birds",
               "grass","scavenging birds","small mammals","disturbance","hunters",
               "mountain lions","coyotes","veg crew","black bear",
               "exotic grass","snakes","pollinators","grizzlies","carcasses","wolves")
  dimnames(w) <- list(species,species)
  
  ##Parameterize interactions
  
  
  ##Shrub
  w["elk/deer","shrub"] <- 0.45
  w["mesopredators","shrub"] <- 0.2
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
  
  #Mesopredators
  w["invertebrate herbivores","mesopredators"] <- -0.1
  w["song birds","mesopredators"] <- -0.45
  w["scavenging birds","mesopredators"] <- 0.1
  w["small mammals","mesopredators"] <- -0.5
  w["disturbance","mesopredators"] <- 0.2
  w["snakes","mesopredators"] <- -0.3
  w["carcasses","mesopredators"] <- -0.05
  
  #Invertebrate herbivores
  w["shrub","invertebrate herbivores"] <- -0.2
  w["mesopredators","invertebrate herbivores"] <- 0.5
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
  w["mesopredators","song birds"] <- 0.25
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
  w["mesopredators","scavenging birds"] <- -0.25
  w["invertebrate herbivores","scavenging birds"] <- -0.1
  w["song birds","scavenging birds"] <- -0.25
  w["small mammals","scavenging birds"] <- -0.5
  w["snakes","scavenging birds"] <- -0.35
  w["carcasses","scavenging birds"] <- -0.75
  
  #Small mammals
  w["shrub","small mammals"] <- -0.5
  w["mesopredators","small mammals"] <- 0.5
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
  #w["mesopredators","mountain lions"] <- 0.35
  #w["scavenging birds","mountain lions"] <- 0.2
  w["small mammals","mountain lions"] <- -0.15
  w["coyotes","mountain lions"] <- -0.25
  w["carcasses","mountain lions"] <- 0.333
  
  #Coyotes
  w["elk/deer","coyotes"] <- -0.25
  w["mesopredators","coyotes"] <- -0.4
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
  w["mesopredators","black bear"] <- -0.1
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
  w["mesopredators","snakes"] <- 0.15
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
  w["elk/deer","grizzlies"] <- runif(1,-0.2,0) #Variable interaction detailed above in FCM function
  w["mountain lions","grizzlies"] <- runif(1,-0.1,0) #-0.1
  w["black bear","grizzlies"] <- runif(1,-0.3,0) #-0.1
  w["coyotes","grizzlies"] <- runif(1,-0.2,0) #-0.1
  w["carcasses","grizzlies"] <- runif(1,-0.4,0) #Variable interaction detailed above in FCM function
  w["shrub","grizzlies"] <- runif(1,-0.01,0) #-0.005
  w["forb","grizzlies"] <- runif(1,-0.02,0) #-0.01
  w["exotic forb","grizzlies"] <- runif(1,-0.02,0) #-0.01
  w["grass","grizzlies"] <- runif(1,-0.01,0) #-0.005
  
  
  ##Carcasses
  w["grizzlies","carcasses"] <-  0.2 #Variable interaction detailed above in FCM function
  w["mesopredators","carcasses"] <-  0.2
  w["coyotes","carcasses"] <-  0.25
  w["black bear","carcasses"] <-  0.2
  w["scavenging birds","carcasses"] <-  0.4 
  w["wolves","carcasses"] <-  0.1
  
  ##Wolves
  w["elk/deer","wolves"] <- -0.3
  w["mountain lions","wolves"] <- -0.025
  w["mesopredators","wolves"] <- -0.05
  w["coyotes","wolves"] <- -0.15
  w["carcasses","wolves"] <- 0.333 
  w["small mammals","wolves"] <- -0.05
  
  ##Need membership functions from above
  
  ## RUN CODE
  
  result <- data.frame(matrix(NA,11,24))
  grizzlies <- seq(0,1,0.1)
  result[,22] <- grizzlies
  colnames(result) <- c("shrub","precipitation","exotic forb","elk/deer","mesopredators",
                        "invertebrate herbivores","invertebrate predators","forb","song birds",
                        "grass","scavenging birds","small mammals","disturbance","hunters",
                        "mountain lions","coyotes","veg crew","black bear",
                        "exotic grass","snakes","pollinators","grizzlies","carcasses","wolves")
  

  for(m in 1:nrow(result)){
    
    
    
    s <- c(shrub.s, precipitation.st, exotic.forb.s, elk.deer.s, mesopredators.s,
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
  ten.prec.uncert.ml[i] <- (result$`mountain lions`[11]-result$`mountain lions`[1])/result$`mountain lions`[1]
  ten.prec.uncert.bb[i] <- (result$`black bear`[11]-result$`black bear`[1])/result$`black bear`[1]
  ten.prec.uncert.co[i] <- (result$coyotes[11]-result$coyotes[1])/result$coyotes[1]
  ten.prec.uncert.rp[i] <- (result$`scavenging birds`[11]-result$`scavenging birds`[1])/result$`scavenging birds`[1]
  ten.prec.uncert.mp[i] <- (result$mesopredators[11]-result$mesopredators[1])/result$mesopredators[1]
  ten.prec.uncert.ug[i] <- (result$`elk/deer`[11]-result$`elk/deer`[1])/result$`elk/deer`[1]
  ten.prec.uncert.wf[i] <- (result$wolves[11]-result$wolves[1])/result$wolves[1]
  print(i)
}

# black bear results 
mean(ten.prec.uncert.bb) 
quantile(ten.prec.uncert.bb, c(0.05,0.95)) 
ten.prec.uncert.bb.df <- as.data.frame(ten.prec.uncert.bb)
colnames(ten.prec.uncert.bb.df) <- "uncert"
ten.prec.uncert.bb.df$Species <- "Black bears"

# mountain lion results 
mean(ten.prec.uncert.ml) 
quantile(ten.prec.uncert.ml, c(0.05,0.95)) 
ten.prec.uncert.ml.df <- as.data.frame(ten.prec.uncert.ml)
colnames(ten.prec.uncert.ml.df) <- "uncert"
ten.prec.uncert.ml.df$Species <- "Mountain lions"

# coyote results 
mean(ten.prec.uncert.co) 
quantile(ten.prec.uncert.co, c(0.05,0.95)) 
ten.prec.uncert.co.df <- as.data.frame(ten.prec.uncert.co)
colnames(ten.prec.uncert.co.df) <- "uncert"
ten.prec.uncert.co.df$Species <- "Coyotes"

# scavenging bird results 
mean(ten.prec.uncert.rp) 
quantile(ten.prec.uncert.rp, c(0.05,0.95)) 
ten.prec.uncert.rp.df <- as.data.frame(ten.prec.uncert.rp)
colnames(ten.prec.uncert.rp.df) <- "uncert"
ten.prec.uncert.rp.df$Species <- "Scavenging birds"

# mesopredator results 
mean(ten.prec.uncert.mp) 
quantile(ten.prec.uncert.mp, c(0.05,0.95)) 
ten.prec.uncert.mp.df <- as.data.frame(ten.prec.uncert.mp)
colnames(ten.prec.uncert.mp.df) <- "uncert"
ten.prec.uncert.mp.df$Species <- "Mesopredators"

# elk/deer results
mean(ten.prec.uncert.ug) 
quantile(ten.prec.uncert.ug, c(0.05,0.95)) 
ten.prec.uncert.ug.df <- as.data.frame(ten.prec.uncert.ug)
colnames(ten.prec.uncert.ug.df) <- "uncert"
ten.prec.uncert.ug.df$Species <- "Elk/Deer"

# wolves results
mean(ten.prec.uncert.wf) 
quantile(ten.prec.uncert.wf, c(0.05,0.95)) 
ten.prec.uncert.wf.df <- as.data.frame(ten.prec.uncert.wf)
colnames(ten.prec.uncert.wf.df) <- "uncert"
ten.prec.uncert.wf.df$Species <- "Wolves"

ten.prec.uncert.full <- rbind(ten.prec.uncert.bb.df,ten.prec.uncert.ml.df,ten.prec.uncert.co.df,
                             ten.prec.uncert.rp.df,ten.prec.uncert.mp.df,ten.prec.uncert.ug.df,
                             ten.prec.uncert.wf.df)

ten.prec <- ggplot()+
  geom_boxplot(data = ten.prec.uncert.full, aes(uncert,Species,color=Species))+
  xlab("Relative change in population")+
  ggtitle("All grizzly effects with 10 mm precipitation")+
  theme_classic()+
  geom_vline(xintercept = 0)+
  xlim(-1,0.4)+
  theme(legend.position = "none")

#ten.prec



##Effect specific

##Veg Crew
comb.uncert.full$cat <- "Current"
veg.uncert.full$cat <- "Double current"
veg.0.uncert.full$cat <- "Zero"
veg.crew.uncert.full <- rbind(comb.uncert.full,veg.uncert.full,veg.0.uncert.full)

veg.crew.uncert.full$cat <- factor(veg.crew.uncert.full$cat,levels = c("Zero",
                                                                       "Current",
                                                                       "Double current"))
library(stringr)
hveg <- ggplot()+
  geom_boxplot(data = veg.crew.uncert.full, aes(uncert,Species,fill=cat), outlier.shape = NA)+
  xlab("Relative change in population")+
  ggtitle(str_wrap("Effects of grizzlies with changes in invasive plant management", width = 35))+
  theme_classic()+
  theme(
    axis.text=element_text(size = 14),
    axis.title.x=element_text(size = 20),
    plot.title = element_text(size = 20),
    legend.text=element_text(size=14),
    legend.title = element_text(size=14)
  )+
  geom_vline(xintercept = 0)+
  xlim(-1,0.3)+
  scale_fill_manual(values = c(alpha("forestgreen",0.1), alpha("forestgreen",0.5), alpha("forestgreen",0.9)))+
  labs(y="",fill = "Invasive plant management")+
  theme(legend.position = c(0.27, 0.48))



##Hunting
comb.uncert.full$cat <- "Current"
hum.uncert.full$cat <- "Double current"
hum.0.uncert.full$cat <- "Zero"
hhunt.uncert.full <- rbind(comb.uncert.full,hum.uncert.full,hum.0.uncert.full)

hhunt.uncert.full$cat <- factor(hhunt.uncert.full$cat,levels = c("Double current",
                                                                 "Current",
                                                                 "Zero"))

hhunt <- ggplot()+
  geom_boxplot(data = hhunt.uncert.full, aes(uncert,Species,fill=cat), outlier.shape = NA)+
  xlab("Relative change in population")+
  ggtitle(str_wrap("Effects of grizzlies with changes in hunting of ungulates", width = 35))+
  theme_classic()+
  theme(
    axis.text=element_text(size = 14),
    axis.title.x=element_text(size = 20),
    plot.title = element_text(size = 20),
    legend.text=element_text(size=14),
    legend.title = element_text(size=14)
  )+
  geom_vline(xintercept = 0)+
  xlim(-1,0.3)+
  scale_fill_manual(values = c(alpha("firebrick3",0.9), alpha("firebrick3",0.5), alpha("firebrick3",0.1)))+
  labs(y="",fill = "Hunting of ungulates")+
  theme(legend.position = c(0.22, 0.48))+
  guides(fill = guide_legend(reverse=TRUE))

##Precipitation
comb.uncert.full$cat <- "30.5 cm (current)"
ele.prec.uncert.full$cat <- "27.9 cm"
ten.prec.uncert.full$cat <- "25.4 cm"
precip.uncert.full <- rbind(ten.prec.uncert.full,ele.prec.uncert.full,comb.uncert.full)
#human.uncert.full$cat <- factor(human.uncert.full$cat,levels = c("Zero Veg Crew",
#                                                                 "Double Veg Crew",
#                                                                 "Current Veg Crew and Human Hunting",
#                                                                 "Zero Human Hunting",
#                                                                 "Double Human Hunting"))



ccprec <- ggplot()+
  geom_boxplot(data = precip.uncert.full, aes(uncert,Species,fill=cat), outlier.shape = NA)+
  xlab("Relative change in population")+
  ggtitle(str_wrap("Effects of grizzlies with changes in precipitation", width = 35))+
  theme_classic()+
  theme(
    axis.text=element_text(size = 14),
    axis.title.x=element_text(size = 20),
    plot.title = element_text(size = 20),
    legend.text=element_text(size=14),
    legend.title = element_text(size=14)
  )+
  geom_vline(xintercept = 0)+
  xlim(-1,0.3)+
  scale_fill_manual(values = c(alpha("deepskyblue4",0.1), alpha("deepskyblue4",0.5), alpha("deepskyblue4",0.9)))+
  labs(y="",fill = "Precipitation")+
  theme(legend.position = c(0.22, 0.48))

#ccprec
grid.arrange(hveg,hhunt,ccprec,ncol=3)
