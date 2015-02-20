#Jered Karr
#Written May 17th 2013
#Plots trends of articulation for 6 important orders
#need to first run other code to get subsetted data.frames for unique orders.


#subseting Insect DB into orders (Some groups are unnatural and need special assignment)

subset(MLR,MLR$order=="Coleoptera")-> coleoptera

subset(MLR,MLR$order=="Blattodea" | MLR$family=="Necymylacridae" |MLR$family=="Ambloblattidae"|MLR$family=="Bradyblattidae"|MLR$family=="Cobaloblattidae"|MLR$family== "Idiomylacridae" |MLR$family=="Phoberoblattidae" |MLR$family=="Schizoblattidae"|MLR$family=="Heterogamidae" |MLR$family=="Panchloridae"|MLR$family=="Periplanetidae")-> cockroach

subset(MLR,MLR$order=="Orthoptera")-> grasshopper

subset(MLR,MLR$order=="Diptera")-> fly

subset(MLR,MLR$order=="Odonata" | MLR$order=="Archodonata"|MLR$order=="Meganisoptera")-> dragonfly

subset(MLR,MLR$order=="Hemiptera")-> Hemiptera

subset(MLR,MLR$order=="Orthoptera") -> grasshopper

Names <- c("Odonatoptera","Blattodea","Orthoptera","Hemiptera","Diptera","Coleoptera")

insectorder_Databases <- (dragonfly,cockroach,grasshopper,Hemiptera,fly,coleoptera)

#Assign sequences for glms
ld <- seq(0, 320, 0.1)
ldx<- seq(6,320,10)

#names of groups for graphs
Names <- c("Odonatoptera","Blattodea","Orthoptera","Hemiptera","Diptera","Coleoptera")

#####GLMS FOR BOTH AGE AND ENVIRONMENT####
###MAKES RED AND BLACK LINES#####
for(i in 1:length(insectorder_Databases)){
glm2 <- rep(NA,length(insectorder_Databases))	
nam <- paste("glm2",insectorder_Databases[i], sep=".")
glm2[i]
nam <- glm(type.body1~age+environment, family=binomial,data=insectorder_Databases[i])
glm2[i] <- nam
}


##### for GLM just Age##### 
#####MAKES BLUE LINE


for(i in 1:length(insectorder_Databases)){
glm1 <- rep(NA,length(insectorder_Databases))	
nam <- paste("glm1",insectorder_Databases[i], sep=".")
glm1[i]
nam <- glm(type.body1~age, family=binomial,data=insectorder_Databases[i])
glm1[i] <- nam
}

#####PLOTS#####
glmfunction <- function(db,name,glm1,glm2,ld,ldx){
	plot(db$age,jitter(db$type.body1,.2), xlim=c(320,0), yaxt="n", ylab='', xlab='', main=name,col='dark grey')


axis(2, at=c(0,1),label=c("Non-Articulated","Articulated"), tick=T)
lines(ld, predict.glm(glm1, data.frame(age=ld, environment=rep(1,length(ld))),type = "response"),lty=3)
lines(ld, predict.glm(glm1, data.frame(age=ld, environment=rep(0,length(ld))),type = "response"), lty="38")
points(ldx, predict.glm(glm1, data.frame(age=ldx, environment=rep(0,length(ldx))),type = "response"), pch=4, cex=0.6)
lines(ld, predict.glm(glm2, data.frame(age=ld),type = "response"))
}

par(mfrow = c(2, 3))


for(i in 1:length(insectorder_Databases)){
	glmfunction(insectorder_Databases[i],name[i],glm1[i],glm2[i],ld,ldx)
	}



#This will plot all the orders in one plot to get the whole picture
all.glm<-glm(type.body1~age+environment, family=binomial,data=MLR)
all.glm1<-glm(type.body1~age, family=binomial,data=MLR)
plot(MLR$age,jitter(MLR$type.body1,.2), xlim=c(0,320), ylab='Modeled Proportion Articulated',xlab="Age (Ma)",col='dark grey' )
axis(4, at=c(0,1),label=c("Non-Articulated","Articulated"), tick=T)
lines(ld, predict.glm(all.glm, data.frame(age=ld, environment=rep(1,length(ld))),type = "response"),lty=3)
lines(ld, predict.glm(all.glm, data.frame(age=ld, environment=rep(0,length(ld))),type = "response"), lty='38')
points(ldx, predict.glm(all.glm, data.frame(age=ldx, environment=rep(0,length(ldx))),type = "response"), pch=4, cex=0.6)
lines(ld, predict.glm(all.glm1, data.frame(age=ld),type = "response"))

