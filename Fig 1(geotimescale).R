###JERED KARR
####CODE FOR BUBBLE PLOTS
####AUGUST 29 2013
###NEEDS to install gts and MLR and ratio function as well as line and text functions


gts <-read.csv("Geologic timescale.2013.csv") #geologic scale


gts <-read.csv("~/Desktop/untitled folder/Geologic timescale.2013.csv")
gts$Stage <- as.character(gts$Stage)
gts$Stage[gts$Stage=="Norian"]<-"Nor" #Norian is large Stage with short name better is abbrievated Nor



#functions

Ratio.function<-function(x){length(subset(x,x=="exoskeleton"))/length(x)} 


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




#add second plot for timescale
bf <- layout(matrix(c(1,1,1,2),ncol=1,4,byrow=TRUE), 2, TRUE)
layout.show(bf)
par(mar=c(0,5,3,4.2))


#plot

plot(levels((factor(MLR$age))),by(MLR$type.body.part,MLR$age,Ratio.function),cex=10*by(MLR$type.body.part,MLR$age,length)/(by(MLR$type.body.part,MLR$age,length)+500),bg="light grey",pch=21,ylab="Proportion Articulated",xlim=c(330.9,0), xaxt="n",yaxs="r",xaxs="i",type='n',cex.lab=2,font.lab=1)
abline(v=gts$S.age[29],lty=2)

points(levels(factor(MLR$age)),by(MLR$type.body.part,MLR$age,Ratio.function),cex=10*by(MLR$type.body.part,MLR$age,length)/(by(MLR$type.body.part,MLR$age,length)+500),bg="light grey",pch=21)

points(levels(factor(MR$age)),by(MR$type.body.part,MR$age,Ratio.function),cex=10*by(MR$type.body.part,MR$age,length)/(by(MR$type.body.part,MR$age,length)+500),bg="pink",pch=21)

legend(300,.8,c("Wings","All"), pt.bg=c("pink",'light grey'),pch=21,pt.cex=2)


par(mar=c(5,5,0,4.2))
plot(MLR$age,jitter(MLR$type.body1,.2), xlim=c(330.9,0),ylim=c(0,1),type="n",yaxt='n',ylab='',xlab="Age (Ma)",yaxs="i",xaxs='i',cex.lab=2,font.lab=1)
add.lines.fun(gts$P.age,length(na.omit(gts$P.age)),.4,.8)
add.lines.fun(gts$S.age,length(na.omit(gts$S.age)),.8,1)

add.lines.fun(gts$E.age,4,0,0.4)

na.omit(gts)
add.text.fun(na.omit(gts$Era),na.omit(gts$E.age),.2,2)
add.text.fun(na.omit(gts$Period),na.omit(gts$P.age),.6,1.5)
add.text.fun(na.omit(gts$Stage),na.omit(gts$S.age),.9,.9)


