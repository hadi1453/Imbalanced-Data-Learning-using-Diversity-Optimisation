library(ggplot2)
library(sp)
library(GA)
library(pROC)
library(e1071)
library(rpart)
library(randomForest)
library(smotefamily)
library(mvtnorm)
library(splitstackshape)
library(MASS)
library(expm)
library(FNN)
library(raster)
library(PRROC)
library(dplyr)
library(nnet)
library(mlbench)
library(fastDummies)
library(stream)
library(cluster)

memory.limit(size = 56000)
options(scipen=10000)

## Data sets
# Diabetes
Diabetes <- read.csv(file.path(Path_Imb_Div,"diabetes.csv"))
levels(Diabetes$class) <- c(0,1)

# Oil-spill
Oil <- read.csv(file.path(Path_Imb_Div,"oil-spill.csv"))
Oil$class <- as.factor(Oil$class)
Oil <- Oil[,-c(23,33)]

# Hepatitis
Hepatitis <- read.csv(file.path(Path_Imb_Div,"hepatitis_update.csv"), row.names = 1)
Hepatitis1 <- cbind.data.frame(Hepatitis[,-1], class=Hepatitis[,1])

# Breast Cancer Wisconsin
breast <- read.csv(file.path(Path_Imb_Div,"breast-cancer-wisconsin.csv"))
breast$class[breast$class==4] <- 1
breast$class[breast$class==2] <- 0
breast$class <- as.factor(breast$class)

# Waveform
Waveform <- read.csv(file.path(Path_Imb_Div,"waveform.csv"))
Waveform$class[Waveform$class==0] <- 3
Waveform$class[Waveform$class==1] <- 0
Waveform$class[Waveform$class==2] <- 0
Waveform$class[Waveform$class==3] <- 1
Waveform$class <- as.factor(Waveform$class)
Waveform1 <- Waveform[sample(nrow(Waveform), 1800),]

# Vowel 
data(Vowel)
table(Vowel$Class)
Vowel$V1 <- as.numeric(Vowel$V1)

# Vowel0
Vowel0 <- Vowel[,-11]
Vowel0$class[Vowel$Class!="hid"] <- 0
Vowel0$class[Vowel$Class=="hid"] <- 1
Vowel0$class <- as.factor(Vowel0$class)
levels(Vowel0$class) 
table(Vowel0$class)
Vowel0$V1 <- as.numeric(Vowel0$V1)

# Vowel1
Vowel1 <- Vowel[,-11]
Vowel1$class[Vowel$Class!="hId"] <- 0
Vowel1$class[Vowel$Class=="hId"] <- 1
Vowel1$class <- as.factor(Vowel1$class)
levels(Vowel1$class) 
table(Vowel1$class)

# Vowel2
Vowel2 <- Vowel[,-11]
Vowel2$class[Vowel$Class!="hEd"] <- 0
Vowel2$class[Vowel$Class=="hEd"] <- 1
Vowel2$class <- as.factor(Vowel2$class)
levels(Vowel2$class) 
table(Vowel2$class)

# Vowel10
Vowel10 <- Vowel[,-11]
Vowel10$class[Vowel$Class!="hed"] <- 0
Vowel10$class[Vowel$Class=="hed"] <- 1
Vowel10$class <- as.factor(Vowel10$class)
levels(Vowel10$class) 
table(Vowel10$class)

# KDD Synthetic Control
KDD <- read.csv(file.path(Path_Imb_Div,"synthetic_control.csv"))
KDD$class <- as.factor(KDD$class)

# Vehicle
Vehicle <- read.csv(file.path(Path_Imb_Div,"Vehicle.csv"))

# Vehicle Bus
VehicleB <- Vehicle[,-19]
VehicleB$class[Vehicle$class!="bus"] <- 0
VehicleB$class[Vehicle$class=="bus"] <- 1
VehicleB$class <- as.factor(VehicleB$class)
table(VehicleB$class)/nrow(VehicleB)

# Vehicle Van
VehicleV <- Vehicle[,-19]
VehicleV$class[Vehicle$class!="van"] <- 0
VehicleV$class[Vehicle$class=="van"] <- 1
VehicleV$class <- as.factor(VehicleV$class)
table(VehicleV$class)/nrow(VehicleV)

# Vehicle Saab
VehicleS <- Vehicle[,-19]
VehicleS$class[Vehicle$class!="saab"] <- 0
VehicleS$class[Vehicle$class=="saab"] <- 1
VehicleS$class <- as.factor(VehicleS$class)
table(VehicleS$class)/nrow(VehicleS)

# Vehicle Opel
VehicleO <- Vehicle[,-19]
VehicleO$class[Vehicle$class!="opel"] <- 0
VehicleO$class[Vehicle$class=="opel"] <- 1
VehicleO$class <- as.factor(VehicleO$class)
table(VehicleO$class)/nrow(VehicleO)

# Pima
Pima <- read.csv(file.path(Path_Imb_Div,"pima-indians-diabetes.csv"))
names(Pima)[9] <- "class"
Pima$class <- as.factor(Pima$class)

# Wine 
Wine <- read.csv(file.path(Path_Imb_Div,"wine.csv"))
table(Wine$type)

# White Wine
WineW <- Wine[Wine$type=="W",-13]
table(WineW$quality)

# High Quality White Wine (7,8,9)
WineWH <- WineW[,-12]
WineWH$class[WineW$quality<=6] <- 0
WineWH$class[WineW$quality>=7] <- 1
WineWH$class <- as.factor(WineWH$class)
table(WineWH$class)

# Low Quality White Wine (3,4)
WineWL <- WineW[,-12]
WineWL$class[WineW$quality>=5] <- 0
WineWL$class[WineW$quality<=4] <- 1
WineWL$class <- as.factor(WineWL$class)
table(WineWL$class)

# White Wine Quality Low vs High
WineWLvH <- WineW[WineW$quality<5 | WineW$quality>6,]
WineWLvH$class[WineWLvH$quality>=7] <- 0
WineWLvH$class[WineWLvH$quality<=4] <- 1
WineWLvH$class <- as.factor(WineWLvH$class)
WineWLvH <- WineWLvH[,-12]
table(WineWLvH$class)

# White Wine Quality 3 vs 7
WineW3v7 <- WineW[WineW$quality==3 | WineW$quality==7,]
WineW3v7$class[WineW3v7$quality==7] <- 0
WineW3v7$class[WineW3v7$quality==3] <- 1
WineW3v7$class <- as.factor(WineW3v7$class)
WineW3v7 <- WineW3v7[,-12]
table(WineW3v7$class)

# Red Wine
WineR <- Wine[Wine$type=="R",-13]
table(WineR$quality)

# High Quality Red Wine (7,8)
WineRH <- WineR[,-12]
WineRH$class[WineR$quality<=6] <- 0
WineRH$class[WineR$quality>=7] <- 1
WineRH$class <- as.factor(WineRH$class)
table(WineRH$class)

# Low Quality Red Wine (3,4)
WineRL <- WineR[,-12]
WineRL$class[WineR$quality<=4] <- 1
WineRL$class[WineR$quality>=5] <- 0
WineRL$class <- as.factor(WineRL$class)
table(WineRL$class)

# White Wine Quality Low vs High
WineRLvH <- WineR[WineR$quality<5 | WineR$quality>6,]
WineRLvH$class[WineRLvH$quality>=7] <- 0
WineRLvH$class[WineRLvH$quality<=4] <- 1
WineRLvH$class <- as.factor(WineRLvH$class)
WineRLvH <- WineRLvH[,-12]
table(WineRLvH$class)

# Wine Multi-class, quality: 3,4 --> Low (0), 8,9 --> High (2), others --> Medium (1)
WineMC <- Wine[,-c(12,13)]
table(Wine$quality)
WineMC$class[Wine$quality<=4] <- 0
WineMC$class[Wine$quality>4] <- 1
WineMC$class[Wine$quality>=7] <- 2
table(WineMC$class)/nrow(WineMC)

# Abalone
Abalone <- read.csv(file.path(Path_Imb_Div,"abalone.csv"))
Abalone1 <- dummy_cols(Abalone, select_columns = names(Abalone)[1], remove_first_dummy = T, remove_selected_columns = T)
Abalone2 <- Abalone1[Abalone1$X9>=9 & Abalone1$X9<=18,]
Abalone2$class[Abalone2$X9<=11] <- 0
Abalone2$class[Abalone2$X9>=12] <- 1
Abalone2 <- Abalone2[,-8]
Abalone2$class <- as.factor(Abalone2$class)
table(Abalone2$class)/nrow(Abalone2)

########################################################
### Synthetic data generation
####################
## Generate imbalanced data 95-5
# Linearly separable, low variance and close
a_poly = Polygon(list(rbind(c(0, 0), c(0, 1), c(1, 1), c(0, 0))))
ap = SpatialPolygons(list(Polygons(list(Polygon(a_poly)), "x")))
plot(ap)
set.seed(1)
df <- as.data.frame(spsample(ap, n = 285, "random"))
df[,3] <- 0
b_poly = Polygon(list(rbind(c(0.5, 0.5), c(0.5, 0.25), c(0.75, 0.25), c(0.5, 0.5))))
bp = SpatialPolygons(list(Polygons(list(Polygon(b_poly)), "x")))
plot(bp)
set.seed(1)
df_b <- as.data.frame(spsample(bp, n = 15, "random"))
df_b[,3] <- 1
df <- rbind(df, df_b)
df$class <- as.factor(df$V3)
#ggplot(data=df, aes(x=x, y=y, colour=class)) + geom_point() + labs(x="X", y="Y")
ggplot(data=df, aes(x=x, y=y)) + geom_point(aes(color=class,shape=class), size=3) + theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), legend.position = "top") + scale_color_manual(name = "", labels = c("Majority training", "Minority training", "Synthetic minority"), values=c("red", "blue", "blue")) + scale_shape_manual(name = "", labels = c("Majority training", "Minority training", "Synthetic minority"), values=c(16,16, 10))
ggsave(path = Path_Imb_Div, "DS1.png")

# Linearly non-separable and low variance
a_poly = Polygon(list(rbind(c(0, 0), c(0, 1), c(1, 1), c(0, 0))))
ap = SpatialPolygons(list(Polygons(list(Polygon(a_poly)), "x")))
plot(ap)
set.seed(1)
df <- as.data.frame(spsample(ap, n = 285, "random"))
df[,3] <- 0
b_poly = Polygon(list(rbind(c(0.5, 0.8), c(0.4, 1), c(0.6, 1), c(0.5, 0.8))))
bp = SpatialPolygons(list(Polygons(list(Polygon(b_poly)), "x")))
plot(bp)
set.seed(1)
df_b <- as.data.frame(spsample(bp, n = 15, "random"))
df_b[,3] <- 1
df <- rbind(df, df_b)
df$class <- as.factor(df$V3)
#ggplot(data=df, aes(x=x, y=y, colour=class)) + geom_point() + labs(x="X", y="Y")
ggplot(data=df, aes(x=x, y=y)) + geom_point(aes(color=class,shape=class), size=3) + theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), legend.position = "top") + scale_color_manual(name = "", labels = c("Majority training", "Minority training", "Synthetic minority"), values=c("red", "blue", "blue")) + scale_shape_manual(name = "", labels = c("Majority training", "Minority training", "Synthetic minority"), values=c(16,16, 10))
ggsave(path = Path_Imb_Div, "DS2.png")

# Linearly non-separable and high variance
a_poly = Polygon(list(rbind(c(0, 0), c(0, 1), c(1, 1), c(0, 0))))
ap = SpatialPolygons(list(Polygons(list(Polygon(a_poly)), "x")))
plot(ap)
set.seed(1)
df <- as.data.frame(spsample(ap, n = 285, "random"))
df[,3] <- 0
b_poly = Polygon(list(rbind(c(0.1, 1), c(0.6, 1), c(0.75, 0.7), c(0.1, 1))))
bp = SpatialPolygons(list(Polygons(list(Polygon(b_poly)), "x")))
plot(bp)
set.seed(100)
df_b <- as.data.frame(spsample(bp, n = 15, "random"))
df_b[,3] <- 1
df <- rbind(df, df_b)
df$class <- as.factor(df$V3)
#ggplot(data=df, aes(x=x, y=y, colour=class)) + geom_point() + labs(x="X", y="Y") + theme_bw() + theme(panel.grid.major = element_blank())
ggplot(data=df, aes(x=x, y=y)) + geom_point(aes(color=class,shape=class), size=3) + theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), legend.position = "top") + scale_color_manual(name = "", labels = c("Majority training", "Minority training", "Synthetic minority"), values=c("red", "blue", "blue")) + scale_shape_manual(name = "", labels = c("Majority training", "Minority training", "Synthetic minority"), values=c(16,16, 10))
ggsave(path = Path_Imb_Div, "DS3.png")

#############################################################################################################################
## 2-dimensional different normal distributions for labels 
sigma <- matrix(c(3,2,2,3), ncol=2)
nsamples <- 300
x <- as.data.frame(rmvnorm(n=round(0.95*nsamples,0), mean=c(2,2), sigma=sigma))
x[,"class"] <- rep(0, round(0.95*nsamples,0))
y <- as.data.frame(rmvnorm(n=round(0.05*nsamples,0), mean=c(0,0.5), sigma=sigma))
y[,"class"] <- rep(1, round(0.05*nsamples,0))
x <- rbind(x, y)
x$class <- as.factor(x$class)
#ggplot(data=x, aes(x=V1, y=V2, colour=class)) + geom_point() + labs(x="X", y="Y") + scale_color_manual(breaks = c("0", "1"), values=c("red", "blue")) + theme(legend.position = "none")
ggplot(data=x, aes(x=V1, y=V2)) + geom_point(aes(color=class,shape=class), size=3) + theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), legend.position = "top") + scale_color_manual(name = "", labels = c("Majority training", "Minority training", "Synthetic minority"), values=c("red", "blue", "blue")) + scale_shape_manual(name = "", labels = c("Majority training", "Minority training", "Synthetic minority"), values=c(16,16, 10))
ggsave(path = Path_Imb_Div, "DS4.png")

## 2-dimensional different normal distributions for labels minor in major 
sigma <- matrix(c(5,2,2,5), ncol=2)
nsamples <- 300
x <- as.data.frame(rmvnorm(n=round(0.95*nsamples,0), mean=c(0,0), sigma=sigma))
x[,"class"] <- rep(0, round(0.95*nsamples,0))
sigma <- matrix(c(0.5,0.15,0.15,0.5), ncol=2)
y <- as.data.frame(rmvnorm(n=round(0.05*nsamples,0), mean=c(0,0), sigma=sigma))
y[,"class"] <- rep(1, round(0.05*nsamples,0))
x <- rbind(x, y)
x$class <- as.factor(x$class)
#ggplot(data=x, aes(x=V1, y=V2, colour=class)) + geom_point() + labs(x="X", y="Y")
ggplot(data=x, aes(x=V1, y=V2)) + geom_point(aes(color=class,shape=class), size=3) + theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), legend.position = "top") + scale_color_manual(name = "", labels = c("Majority training", "Minority training", "Synthetic minority"), values=c("red", "blue", "blue")) + scale_shape_manual(name = "", labels = c("Majority training", "Minority training", "Synthetic minority"), values=c(16,16, 10))
ggsave(path = Path_Imb_Div, "DS5.png")

## 2-dimensional normal distribution randomly labelled
sigma <- matrix(c(4,2,2,3), ncol=2)
nsamples <- 300
x <- as.data.frame(rmvnorm(n=nsamples, mean=c(1,2), sigma=sigma))
x[,"class"] <- rep(0, nsamples)
x[sample(1:nsamples, round(0.05*nsamples)), "class"] <- 1
x$class <- as.factor(x$class)
#ggplot(data=x, aes(x=V1, y=V2, colour=class)) + geom_point() + labs(x="X", y="Y")
ggplot(data=x, aes(x=V1, y=V2)) + geom_point(aes(color=class,shape=class), size=3) + theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), legend.position = "top") + scale_color_manual(name = "", labels = c("Majority training", "Minority training", "Synthetic minority"), values=c("red", "blue", "blue")) + scale_shape_manual(name = "", labels = c("Majority training", "Minority training", "Synthetic minority"), values=c(16,16, 10))
ggsave(path = Path_Imb_Div, "DS6.png")

#####################################################################################################################################################################################################################################
#### Multi-class
## 2-dimensional different normal distributions for labels 
nsamples <- 1000
x <- as.data.frame(rmvnorm(n=round(0.85*nsamples,0), mean=c(4,4), sigma=matrix(c(3,2,2,3), ncol=2)))
x[,"class"] <- rep(0, round(0.85*nsamples,0))
y <- as.data.frame(rmvnorm(n=round(0.05*nsamples,0), mean=c(0,0), sigma=matrix(c(1,0.5,0.5,1), ncol=2)))
y[,"class"] <- rep(1, round(0.05*nsamples,0))
z <- as.data.frame(rmvnorm(n=round(0.1*nsamples,0), mean=c(4,0), sigma=matrix(c(2,1.5,1.5,2), ncol=2)))
z[,"class"] <- rep(2, round(0.1*nsamples,0))
x <- rbind.data.frame(x, y, z)
x$class <- as.factor(x$class)
#ggplot(data=x, aes(x=V1, y=V2, colour=class)) + geom_point() + labs(x="X", y="Y") + scale_color_manual(breaks = c("0", "1"), values=c("red", "blue")) + theme(legend.position = "none")
ggplot(data=x, aes(x=V1, y=V2)) + geom_point(aes(color=class,shape=class), size=3) + theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), legend.position = "top") + scale_color_manual(name = "", labels = c("Majority training", "Minority training", "Synthetic minority"), values=c("red", "blue", "blue")) + scale_shape_manual(name = "", labels = c("Majority training", "Minority training", "Synthetic minority"), values=c(16,16, 10))
ggsave(path = Path_Imb_Div, "DSMC.png")

##############################################################################################
# Auxiliary function for Diversity (Solow-Polasky) measure
Div_SP <- function(X, Dis){ #X is the set of datapoints
  M <- as.matrix(exp(-(dist(as.data.frame(X), diag = T, upper = T, method = Dis))))
  #M_1 <- solve(M)
  M_1 <- ginv(M)
  #Div <- 0
  #for (i in 1:nrow(M_1)) {Div <- Div + sum(M_1[,i])/M_1[i,i]}
  Div <- sum(M_1)
  return(Div)
}

# Auxiliary function for diversity difference using Div_SP()
Div_SP_diff1 <- function(X, n){
  a <- Div_SP(X, Dis) - Div_SP(X[-n,], Dis)
  return(a)
}

# Auxiliary function for diversity difference with less computation
Div_SP_diff2 <- function(X, n, Dis){
  M <- as.matrix(exp(-(dist(as.data.frame(X), diag = T, upper = T, method = Dis))))
  M_1 <- solve(M)
  diff <- sum(M_1[,n])/M_1[n,n]
  return(diff)
}

# Auxiliary function for adaptive change of bound
BoundChange <- function(P, Pn, b, r, func){
  PPn <- as.data.frame(rbind(P, Pn))
  val <- ncol(PPn)+1
  PPn[,val] <- apply(PPn, 1, func)
  P1 <- PPn[order(PPn[,val]),]
  P2 <- P1[1:r,1:(val-1)]
  b <- P1[r,val]
  return(list(Pop=P2, bound=b))
}

# Auxiliary function for crossover
crossover1 <- function(pr, L, U){
  Osp1 <- colMeans(pr)
  Osp2 <- colMeans(rbind(pr, L))
  Osp3 <- colMeans(rbind(pr, U))
  return(rbind(Osp1, Osp2, Osp3))
}

# Auxiliary function for mutation 
mutate <- function(p, L, U){
  mu <- if(sample(c(0, 1), 1)==0){L}else{U}
  dif <- mu - p
  teta <- runif(1, 0, 0.1)
  dir<- sample(c(0, 1), length(p), replace = T)*dif
  p1 <- p + dir*teta
  return(p1)
}

# Auxiliary function for ranking instances based diversity (Solow-Polasky) difference with less computation
Div_SP_diff_rank <- function(X, Dis){
  P_col <- ncol(X)+1
  M <- as.matrix(exp(-(dist(as.data.frame(X), diag = T, upper = T, method = Dis))))
  M_1 <- ginv(M)
  for (k in 1:nrow(X)) {
    X[k,P_col] <- sum(M_1[,k])/M_1[k,k]
  }
  P1 <- X[order(X[,P_col], decreasing = T),]
  return(P1)
}

# Auxiliary function for diversity selection
DivSelect_greedy <- function(P, r){
  P_col <- ncol(P)
  P_row <- nrow(P)
  P1 <- P
  i = 0
  while (i < (P_row-r)) {
    P1 <- Div_SP_diff_rank(P1, Dis)
    P1 <- P1[-nrow(P1),1:P_col]
    i <- i + 1
  }
  return(P1)
}

# Auxiliary function for diversity optimization
DivOpt <- function(P, r, b, c, L, U, func){
  i <- 0
  while(i < c){
    P1 <- NULL
    j <- 0
    while (j <= 2*r-nrow(P)) {
      bbb <- NULL
      aaa <- sample(nrow(P), 2)
      bbb <- crossover1(P[aaa,], L, U)
      bbb <- rbind(bbb, mutate(P[aaa[1],], L, U))
      bbb <- rbind(bbb, mutate(P[aaa[2],], L, U))
      bbb <- cbind(bbb, apply(bbb, 1, func))
      P1 <- rbind(P1, bbb[bbb[,ncol(bbb)] < b,-ncol(bbb)])
      j <- nrow(P1)
    }
    P2 <- rbind(P, P1)
    P4 <- DivSelect_greedy(P2, r)
    i <- i + 1
  }
  return(P)
}

## NOAH Function
n = 200
v = 0.85
g = 20
r = 50
c = 1
L = c(0,0) # apply(df_b[,1:2], 2, min)
U = c(1,1) # apply(df_b[,1:2], 2, max)
func = ObjFunc_Eu
NOAH <- function(n, v, g, r, c, L, U, func) {
  b <- Inf
  P <- NULL
  i <- 0
  while (i < c) {
    # ObjOpt
    GA <- ga(type = "real-valued",
             fitness =  function(x) -func(x), 
             lower = L, upper = U, pcrossover = 0.5, pmutation = 0.5,
             popSize = n, maxiter = g, maxFitness = b)
    # BoundChange
    aa <- BoundChange(P, GA@population, b, r, func)
    P <- aa$Pop
    if((b - aa$bound) < v*b){i <- i+1}else{i <- 0}
    b <- aa$bound
    print(b)
    # DivOpt
    if(r > 1){P <- DivOpt(P, r, b, c, L, U, func)}
  }
  return(P)
}

###########################################################################
##### Optimization #####
# GA
ObjFunc_Eu <- function(x){sqrt(sum((x-p)^2))} # Objective function Euclidean distance
ObjFunc_M <- function(x){sum(abs(x-p))} # Objective function Manhattan distance
ObjFunc_C <- function(x){sum(abs(x-p)/(abs(x)+abs(p)))} # Objective function Canberra distance

ObjFunc1 <- function(x){mean(apply(df_b[,1:2], 1, FUN = function(y)(sqrt(sum((x-y)^2)))))} # Objective function sum of Euclidean distance
ObjFunc1 <- function(x){mean(apply(df_b[,1:2], 1, FUN = function(y)(abs(x-y))))} # Objective function sum of MAnhattan distance

ObjFunc_Eu_S <- function(x){mean(apply(df_b, 1, FUN = function(y)(sqrt(sum((x-y)^2)))))} # Objective function sum of Euclidean distance for real datasets
ObjFunc_M_S <- function(x){mean(apply(df_b, 1, FUN = function(y)(sum(abs(x-y)))))} # Objective function sum of Manhattan distance for real datasets
ObjFunc_C_S <- function(x){mean(apply(df_b, 1, FUN = function(y)(sum(abs(x-y)/(abs(x)+abs(y))))))} # Objective function sum of Canberra distance for real datasets

p <- df[100,1:2]
df_b <- df_b[,1:2]

GA <- ga(type = "real-valued", 
         fitness = function(x) -ObjFunc_Eu(x),
         lower = c(0,0), upper = c(1,1), pcrossover = 0.5, pmutation = 0.5,
         popSize = 90, maxiter = 20)
summary(GA)
plot(GA)

GA_gen <- as.data.frame(GA@population)
colnames(GA_gen) <- names(df[,1:2])
GA_gen[,3] <- "Similar-to-1"
GA_gen$class <- as.factor(GA_gen$V3)
new_df <- rbind(df, GA_gen)
ggplot(data=new_df, aes(x=x, y=y)) + geom_point(aes(color=class,shape=class), size=3) + theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), legend.position = "top") + scale_color_manual(name = "", labels = c("Majority training", "Minority training", "Synthetic minority"), values=c("red", "blue", "blue")) + scale_shape_manual(name = "", labels = c("Majority training", "Minority training", "Synthetic minority"), values=c(16,16, 10))

# NOAH
Dis = "manhattan" # "euclidean"
Func = ObjFunc_M
P_NOAH <- NOAH(n, v, g, r, c, L, U, Func)
colnames(P_NOAH) <- names(df[,1:2])
P_NOAH[,3] <- "Similar-to-1"
P_NOAH$class <- as.factor(P_NOAH$V3)
new_df <- rbind(df, P_NOAH)
ggplot(data=new_df, aes(x=x, y=y)) + geom_point(aes(color=class,shape=class), size=3) + theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), legend.position = "top") + scale_color_manual(name = "", labels = c("Majority training", "Minority training", "Synthetic minority"), values=c("red", "blue", "blue")) + scale_shape_manual(name = "", labels = c("Majority training", "Minority training", "Synthetic minority"), values=c(16,16, 10))

##############################################################################################
### Visualisation

df_tr1 <- df_tr_red
df_tr1$classU[df_tr1$class==1] <- "1Tr"
df_tr1$classU[df_tr1$class==0] <- "2Tr"

df_te1 <- df_te
df_te1$classU[df_te1$class==1] <- "1Ts"
df_te1$classU[df_te1$class==0] <- "2Ts"

df <- rbind.data.frame(df_tr1, df_te1)

cl <- c(12,13) 

df_n <- df
#df_n[,-cl] <- apply(df[,-cl], 1, FUN = function(x)((x-min(x))/(max(x)-min(x))))

p.comp <- prcomp(df_n[,-cl], scale. = T)
X.comp <- as.data.frame(p.comp$x[,1:2])
X_comp_Cl <- cbind.data.frame(X.comp, as.factor(df_n$classU))
names(X_comp_Cl) <- c("x", "y", "class")
#X_comp_Cl$class <- factor(X_comp_Cl$class, levels = c("MinTr", "MinTe", "MajTe", "MajTr"))
levels(X_comp_Cl$class)
ggplot(data=X_comp_Cl, aes(x=x, y=y)) + geom_point(data=X_comp_Cl[X_comp_Cl$class!="1Tr",], aes(color=class,shape=class), size=4) + theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), legend.position = "top") + scale_color_manual(name = "", labels = c("Minority training", "Minority test", "Majority training", "Majority test"), values=c("blue", "cyan1", "red", "rosybrown1")) + scale_shape_manual(name = "", labels = c("Minority training", "Minority test", "Majority training", "Majority test"), values=c(16,16,16,16)) +
  geom_point(data=X_comp_Cl[X_comp_Cl$class=="1Tr",], aes(x,y,color=class, shape=class), size=5) 

OS <- -diff(table(df_tr_red$class))

# DADO
MinorGroup <- df_tr_red[df_tr_red$class==1,]
df_b <- MinorGroup[,names(MinorGroup)!="class"]
Dis = "euclidean"
P <- NOAH(101, 0.5, 20, 100, 1, apply(df_b, 2, min), apply(df_b, 2, max), ObjFunc_Eu_S)
Snew <- P
Snew$class <- 1
Snew$classU <- "G"
names(Snew) <- names(df_n)
df_new <- rbind.data.frame(df_n, Snew)

p.comp <- prcomp(df_new[,-cl], scale. = T)
X.comp <- as.data.frame(p.comp$x[,1:2])
X_comp_Cl <- cbind.data.frame(X.comp, df_new$classU)
names(X_comp_Cl) <- c("x", "y", "class")
ggplot(data=X_comp_Cl, aes(x=x, y=-y)) + geom_point(data=X_comp_Cl[X_comp_Cl$class!="1Tr",], aes(color=class,shape=class), size=4) + theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), legend.position = "top") + scale_color_manual(name = "", labels = c("Minority training", "Minority test", "Majority training", "Majority test", "Minority synthetic"), values=c("blue", "cyan1", "red", "rosybrown1", "blue")) + scale_shape_manual(name = "", labels = c("Minority training", "Minority test", "Majority training", "Majority test", "Minority synthetic"), values=c(16,16,16,16,10)) +
  geom_point(data=X_comp_Cl[X_comp_Cl$class=="1Tr",], aes(x,-y,color=class, shape=class), size=5) 

# DIWO
MinorGroup <- df_tr_red[df_tr_red$class==1,]
df_b <- MinorGroup[,names(MinorGroup)!="class"]
Dis = "canberra"
P <- NULL
r <- 100
r1 <- if(r/nrow(df_b) < 1){1}else{round(r/nrow(df_b),0)}
for (ii in 1:nrow(df_b)) {
  p <- df_b[ii,]
  P1 <- NOAH(r1+50, 0.5, 20, r1, 1, apply(df_b, 2, min), apply(df_b, 2, max), ObjFunc_C)
  P <- rbind(P, P1)
}

Snew <- P
Snew$class <- 1
Snew$classU <- "G"
names(Snew) <- names(df_n)
df_new <- rbind.data.frame(df_n, Snew)

p.comp <- prcomp(df_new[,-cl], scale. = T)
X.comp <- as.data.frame(p.comp$x[,1:2])
X_comp_Cl <- cbind.data.frame(X.comp, df_new$classU)
names(X_comp_Cl) <- c("x", "y", "class")
ggplot(data=X_comp_Cl, aes(x=x, y=y)) + geom_point(data=X_comp_Cl[X_comp_Cl$class!="1Tr",], aes(color=class,shape=class), size=4) + theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), legend.position = "top") + scale_color_manual(name = "", labels = c("Minority training", "Minority test", "Majority training", "Majority test", "Minority synthetic"), values=c("blue", "cyan1", "red", "rosybrown1", "blue")) + scale_shape_manual(name = "", labels = c("Minority training", "Minority test", "Majority training", "Majority test", "Minority synthetic"), values=c(16,16,16,16,10)) +
  geom_point(data=X_comp_Cl[X_comp_Cl$class=="1Tr",], aes(x,y,color=class, shape=class), size=5) 


### Visualisation Multi-class
df_tr1 <- WineMC

cl <- 12

df_n <- df_tr1
#df_n[,-cl] <- apply(df[,-cl], 1, FUN = function(x)((x-min(x))/(max(x)-min(x))))

p.comp <- prcomp(df_n[,-cl], scale. = T)
X.comp <- as.data.frame(p.comp$x[,1:2])
X_comp_Cl <- cbind.data.frame(X.comp, as.factor(df_n$class))
names(X_comp_Cl) <- c("x", "y", "class")
#X_comp_Cl$class <- factor(X_comp_Cl$class, levels = c("MinTr", "MinTe", "MajTe", "MajTr"))
levels(X_comp_Cl$class)
ggplot(data=X_comp_Cl, aes(x=x, y=y)) + geom_point(data=X_comp_Cl, aes(color=class,shape=class), size=4) + theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), legend.position = "top") + scale_color_manual(name = "", labels = c("Low", "Medium", "High"), values=c("blue", "green", "red")) + scale_shape_manual(name = "", labels = c("Low", "Medium", "High"), values=c(16,16,16)) +
  geom_point(data=X_comp_Cl, aes(x,y,color=class, shape=class), size=5) 

## Synthetic
ggplot(data=x, aes(x=V1, y=V2)) + geom_point(data=x, aes(color=class,shape=class), size=2) + theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), legend.position = "top") + scale_color_manual(name = "", labels = c("0", "1", "2"), values=c("red", "blue", "orange")) + scale_shape_manual(name = "", labels = c("0", "1", "2"), values=c(16,16,16)) +
  geom_point(data=x, aes(V1,V2,color=class, shape=class), size=2)

# applying DIWO
MinorGroup <- x[x$class==1,]
df_b <- MinorGroup[,names(MinorGroup)!="class"]
Dis = "canberra"
P <- NULL
r <- 800
r1 <- if(r/nrow(df_b) < 1){1}else{round(r/nrow(df_b),0)}
for (ii in 1:nrow(df_b)) {
  p <- df_b[ii,]
  P1 <- NOAH(r1+5, 0.5, 10, r1, 1, apply(df_b, 2, min), apply(df_b, 2, max), ObjFunc_C)
  P <- rbind(P, P1)
}

Snew1 <- P
Snew1$class <- 1
#Snew$classU <- "G"
names(Snew1) <- names(x)

MinorGroup <- x[x$class==2,]
df_b <- MinorGroup[,names(MinorGroup)!="class"]
Dis = "canberra"
P <- NULL
r <- 750
r1 <- if(r/nrow(df_b) < 1){1}else{round(r/nrow(df_b),0)}
for (ii in 1:nrow(df_b)) {
  p <- df_b[ii,]
  P1 <- NOAH(r1+5, 0.5, 10, r1, 1, apply(df_b, 2, min), apply(df_b, 2, max), ObjFunc_C)
  P <- rbind(P, P1)
}

Snew2 <- P
Snew2$class <- 2
names(Snew2) <- names(x)

df_new <- rbind.data.frame(x, Snew1, Snew2)

ggplot(data=df_new, aes(x=V1, y=V2)) + geom_point(data=df_new, aes(color=class,shape=class), size=2) + theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), legend.position = "top") + scale_color_manual(name = "", labels = c("0", "1", "2"), values=c("red", "blue", "orange")) + scale_shape_manual(name = "", labels = c("0", "1", "2"), values=c(16,16,16)) +
  geom_point(data=df_new, aes(V1,V2,color=class, shape=class), size=2)

# applying DADO
MinorGroup <- x[x$class==1,]
df_b <- MinorGroup[,names(MinorGroup)!="class"]
Dis = "euclidean"
P <- NOAH(351, 0.5, 20, 350, 1, apply(df_b, 2, min), apply(df_b, 2, max), ObjFunc_Eu_S)
Snew1 <- P
Snew1$class <- 1
#Snew$classU <- "G"
names(Snew1) <- names(x)

MinorGroup <- x[x$class==2,]
df_b <- MinorGroup[,names(MinorGroup)!="class"]
Dis = "euclidean"
P <- NOAH(301, 0.5, 20, 300, 1, apply(df_b, 2, min), apply(df_b, 2, max), ObjFunc_Eu_S)
Snew2 <- P
Snew2$class <- 2
#Snew$classU <- "G"
names(Snew1) <- names(x)

df_new <- rbind.data.frame(x, Snew1, Snew2)

ggplot(data=df_new, aes(x=V1, y=V2)) + geom_point(data=df_new, aes(color=class,shape=class), size=2) + theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), legend.position = "top") + scale_color_manual(name = "", labels = c("0", "1", "2"), values=c("red", "blue", "orange")) + scale_shape_manual(name = "", labels = c("0", "1", "2"), values=c(16,16,16)) +
  geom_point(data=df_new, aes(V1,V2,color=class, shape=class), size=2)
