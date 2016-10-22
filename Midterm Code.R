#Generate the change in presidential approval over the year 
#preceding the election
data$NETAPPROVALLAG<-data$APPROVALLAG-data$DISAPPROVALLAG
data$NETAPPROVAL<-data$APPROVAL-data$DISAPPROVAL
data$DELTAAPPROVAL<-data$NETAPPROVAL-data$NETAPPROVALLAG


#Construct my model
My.model<-lm(VS~DELTAAPPROVAL+INCUMB+PRESNAME2+GDP2Q,
             data=data)


#Check pseudo-predictions with past data
past.predictions<-predict.lm(My.model, interval=c("confidence"), 
                            level=0.95)



#Get the 2016 prediction (this is the prediction for Clinton) 
 new.data<-data.frame(DELTAAPPROVAL=6, PRESNAME2=5, GDP2Q=1.1, 
                      INCUMB=0) 
                   
 
 
#My 2016 prediction
prediction.2016<-predict(My.model, new.data, interval="confidence", 
                 level=0.95)

#Make the beautiful figure

require(gplots)

old.data<- data[-c(18),]

figure.data<-data.frame(old.data$year, old.data$VS, past.predictions)
plot(figure.data$old.data.year, figure.data$old.data.VS, 
     xlab="Year", ylab="Vote Share (%)", ylim=c(40,65), col="red", pch=16)
plotCI(figure.data$old.data.year, figure.data$fit, add=TRUE, 
       pch="O", col="blue")
plotCI(figure.data$old.data.year, figure.data$lwr, add=TRUE, 
       pch="-", col="blue")
plotCI(figure.data$old.data.year, figure.data$upr, add=TRUE, 
       pch="-", col="blue")

