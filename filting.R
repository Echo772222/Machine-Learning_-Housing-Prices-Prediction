OurData=read.csv("D:/U of A/2021-winter course/ECE 625/group project/EdmontonRealEstateData.csv")
OpenData=read.csv("D:/U of A/2021-winter course/ECE 625/group project/open_data.csv")
attach(OpenData)
a=OurData$taxroll_number
newdata1=OpenData[c(Assessment.Year==2015),]
newdata2=OpenData[c(Assessment.Year==2016),]
b=match(newdata1$Account.Number,a)
index=which(b!=0)
newdata3=newdata1[index,]
length(index)

c=match(newdata2$Account.Number,a)
index2=which(c!=0)
newdata4=newdata2[index2,]
length(index2)

setwd("D:/U of A/2021-winter course/ECE 625/group project")
write.csv(x=newdata3,file="opendata.sel.2015.csv")
write.csv(x=newdata4,file="opendata.sel.2016.csv")

d=match(a,newdata3$Account.Number)
index3=which(d!=0)
a[-index3]
e=match(a,newdata4$Account.Number)
index4=which(e!=0)
a[-index4]
write.csv(x=OurData[-index4,],file="ourdata.2016.remove.csv")
write.csv(x=OurData[index4,],file="ourdata.2016.keep.csv")

