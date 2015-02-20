#Jered Karr#
#Written on January 1 2013
# Plot to view importance(proportion of occurences) of different groups(insect groups) through geologic time

#import data sets

gts <-read.csv("Geologic timescale.2013.csv") #geologic scale


gts$Stage <- as.character(gts$Stage)
gts$Stage[gts$Stage=="Norian"]<-"Nor" #Norian is large Stage with short name better is abbrievated Nor


#Make fake data set of species occurences through geologic time
sp <-c("sp1","sp2","sp3","sp4","sp5","sp6","sp7","sp8","sp9","sp10")

sp.data <- data.frame(yax=sample(sp, 10000, replace=T), xax=sample(0:33, 1000, replace=T)*10)

#Functions#######


#Functions for adding timescale to bottom of plot
add.lines.fun <-function(x,y,z,a){ #ADDS lines and boxes to fit different time periods
	for(i in 1:y)
	lines(c(x[i],x[i]),c(z,a))
	lines(c(x[1],x[y]),c(z,z))
	lines(c(x[1],x[y]),c(a,a))
}



add.text.fun <-function(labs,xl,yl,z){ #adds names, trimmed to fit into boxes of time lengths
	for(i in 1:(length(xl)-1)) 
	text(x=(xl[i]+xl[i+1])/2,y=yl,labels= strtrim(labs[i],width=(xl[i]-xl[i+1])/(((max(xl)-min(xl))/80)*z)),cex=z) #80 can be adjusted based on preference 50 gives larger space between names and boxes....

}
#######

#functions for ratio in each bin
Ratio.function.occur<-function(x,y){length(subset(x,x==y))/length(x)}

order.names.fun <- function(h){
names(as.factor(sapply(split(h,h),length)))[order(sapply(split(h,h),length),decreasing=T)]
}



####additive Polygon function####
polygon.prop.add.fun <-function(var.x.axis,var.y.axis,rain.color){
add.me <- rep(0,length(levels(as.factor(var.x.axis))))
xaxi <- c(levels(factor(var.x.axis)),rev(levels(factor(var.x.axis))))
for (i in 1:length(levels(as.factor(var.y.axis)))){
polygon(xaxi,c(add.me+by(var.y.axis,var.x.axis,Ratio.function.occur,y=order.names.fun(var.y.axis)[i]),rev(add.me)),col=rain.color[i])
add.me <- by(var.y.axis,var.x.axis,Ratio.function.occur,y=order.names.fun(var.y.axis)[i])+add.me

	
}
}
#####Edits time scale to adjust for smaller periods of time####
replace.max.min.fun <- function(new,old){
	for (i in 1:length(na.omit(old))){	
		if (max(new) <=old[i]){
		old[i] <-max(new)
		} 
		if (min(new) >=old[i]){
		old[i] <-min(new)
		} 
	}	
return(old)
}





#####COlORS########

o <- seq(1,length(as.factor(levels(sp.data$yax))), by=2)
e <- seq(2,length(as.factor(levels(sp.data$yax))), by=2)
colors <- rainbow(length(as.factor(levels(sp.data$yax)))) ####Simple rainbow
colors <-c(colors[o],colors[e])  ####Alternating rainbow

############FUNCTION TO MAKE PLOT##################

plot.poly.fun<- function(var.x.axis,var.y.axis){
	
o <- seq(1,length(as.factor(levels(var.y.axis))), by=2)
e <- seq(2,length(as.factor(levels(var.y.axis))), by=2)	
colors <- rainbow(length(as.factor(levels(var.y.axis))))
colors <-c(colors[o],colors[e])



bf <- layout(matrix(c(1,1,1,2),ncol=1,4,byrow=TRUE), 2, TRUE) #layout for adding timescale at bottom


par(xpd=T,mar=c(0,5,3,4)+c(0,0,0,8))#adds space for legend

plot(levels(factor(var.x.axis)),type='n',xlab="Age (Ma)",ylab="Proportion of Occurrences",xlim=c(min(var.x.axis),max(var.x.axis)),ylim=c(0,1),xaxs="i",yaxs="i",xaxt="n",cex.lab=2,font.lab=2) #plot for occurences

polygon.prop.add.fun(var.x.axis,var.y.axis,colors) #adds polygons

legend(max(var.x.axis)+(max(var.x.axis)-min(var.x.axis))/30,1,order.names.fun(var.y.axis), col="black",fill=colors ,bty="n",xpd=TRUE,cex=1.2) #adds legends


par(mar=c(5,5,0,4)+c(0,0,0,8)) #sets up area for geolgic time scale
plot(levels(factor(var.x.axis)), xlim=c(min(var.x.axis),max(var.x.axis)),ylim=c(0,1),type="n",yaxt='n',ylab='',xlab="Age (Ma)",yaxs="i",xaxs='i', bty="n",cex.lab=2,font.lab=2) #time scale 



add.lines.fun(replace.max.min.fun(var.x.axis,gts$P.age),length(na.omit(replace.max.min.fun(var.x.axis,gts$P.age))),.4,.8) #area for Periods
add.lines.fun(replace.max.min.fun(var.x.axis,gts$S.age),length(na.omit(replace.max.min.fun(var.x.axis,gts$S.age))),.8,1) #area for stages

add.lines.fun(replace.max.min.fun(var.x.axis,gts$E.age),4,0,0.4) #area for Era

na.omit(gts)
add.text.fun(na.omit(gts$Era),na.omit(replace.max.min.fun(var.x.axis,gts$E.age)),.2,2) # labels for Eras
add.text.fun(na.omit(gts$Period),na.omit(replace.max.min.fun(var.x.axis,gts$P.age)),.6,1.5) # labels for Periods
add.text.fun(na.omit(gts$Stage),na.omit(replace.max.min.fun(var.x.axis,gts$S.age)),.9,.7) # labels for Stages
}




#####PLot with different sp. proportion of occurences through geologic time#######
plot.poly.fun(sp.data$xax,sp.data$yax)
