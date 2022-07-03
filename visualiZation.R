QUESTION 1 
## COMAPARE SALES BY REGION FOR 2016 WITH 2015 USING BAR CHART.

df<- SalesData%>%
  group_by(Region)%>%
  summarise(Total_2015 = sum(Sales2015),Total_2016 = sum(Sales2016))%>%group_by(Region)

View(df)

gather

data_frame = gather(df,key = Year, value = Sales,-1 )
data_frame$Sales = round(data_frame$Sales,1)


ggplot(data_frame,aes(x=Region,y=Sales))+geom_bar(position ="dodge" , stat = "identity",
               aes(fill=Year))+ggtitle("city wise spend")

ggplotly(ggplot(data_frame,aes(x=Region,y=Sales))+geom_bar(position ="dodge" , stat = "identity",
                                                           aes(fill=Year))+ggtitle("city wise spend"))






QUESTION 2
##PIE CHART FOR SALES OF EACH REGION IN 2016.
df2<-SalesData%>%select(3,10)%>%group_by(Region)%>%summarise(total_16=sum(Sales2016))
View(df2)

pct=round(df2$total_16/sum(df2$total_16)*100,2)
lbls<-paste(c("Center","East","West"),pct,"%")
View(lbls)

2D chart
pie_chart<-pie(df2$total_16,labels = lbls,main = "PIE CHART", 
    col = c("blue","lightblue","royalblue"))

FOR 3D INATALL "PLOTRIX"
pie3D(df2$total_16,labels = lbls,main = "PIE CHART", 
          col = c("blue","lightblue","royalblue"))



QUESTION 3
##COMPARE SALES OF 2015 AND 2016 WITH REGION AND TIER.
Data_frame<-SalesData%>%select(3,7,9,10)%>%group_by(Region,Tier)%>%summarise(Total_16=sum(Sales2016),
                                         Total_15 = sum(Sales2015))

View(Data_frame)

View(frame_data)

frame_data = gather(Data_frame,key = Year,value = Sales,-c(1,2))

ggplot(frame_data,aes(x=Tier,y=Sales,fill=Year))+geom_bar(stat = "identity",
                           position ="dodge" ) +ggtitle("Comparision of sales with Region and Tire")                              
                                                          
ggplotly(ggplot(frame_data,aes(x=Tier,y=Sales,fill=Year))+geom_bar(position ="dodge",
                stat =  "identity" ) +ggtitle("Comparision of sales with Region and Tire")  )                            



QUESTION 4 
##IN EAST REGION ,WHICH STATE REGISTERED A DECLINE IN 2016 AS COMPARED TO 2015?
data_frame1<-SalesData%>%select(3,6,9,10)%>%filter(Region =="East")%>%
  group_by(State)%>%

  summarise(Total2015 = sum(Sales2015),Total2016 = sum(Sales2016))

View(data_frame12)

data_frame12 = gather(data_frame1,key = Year, value = Sales,-1)


      
  
ggplot(data_frame12,aes(x=State,y=Sales, fill=Year))+geom_bar(position="dodge", stat="identity") +ggtitle( "Comparision of sales")        
            
ggplotly(ggplot(data_frame12,aes(x=State,y=Sales, fill=Year))+geom_bar(position="dodge", stat="identity") +ggtitle( "Comparision of sales with Amount"))       
      
             
      


QUESTION 5 
##IN ALL THE HIGH TIER ,WHICH DIVISION SAW A DECLINE IN NO. OF UNITS SOLD IN 2016 COMPARED TO 2015?
data_frame3<-SalesData%>%group_by(Division)%>%filter(Tier == "High")%>%summarise(Total15 = sum(Units2015),
                                                                                 Total16 = sum(Units2016))

View(data_frame3)
data_frame4<-gather(data_frame3,key=Year,value = Sales,-1)
ggplot(data_frame4,aes(x=Division,y=Sales,fill=Year))+geom_bar(position = "dodge",
          stat="identity")+ggtitle("Comparision of units sold")

ggplotly(ggplot(data_frame4,aes(x=Division,y=Sales,fill=Year))+geom_bar(position = "dodge",
                                                                        stat="identity")+ggtitle("Comparision of units sold"))


Question 6
## Compare quarter wise sale in 2015 and 2016 in a bar plot.
SalesData$Quarters <-if_else(SalesData$Month == "Jan"|SalesData$Month == "Feb"|SalesData$Month == "Mar","Q1",
                    if_else(SalesData$Month == "Apr"|SalesData$Month == "May"|SalesData$Month == "Jun","Q2",
                            if_else(SalesData$Month == "Jul"|SalesData$Month == "Aug"|SalesData$Month == "Sep","Q3","Q4")))


dataframe13<-SalesData%>%group_by(Quarters)%>%summarise(total_15=sum(Sales2015),
                                           total_16=sum(Sales2016))

View(dataframe13)

data_frame13 = gather(dataframe13,key = Year, value = Sales,-Quarters)
ggplot(data_frame13,aes(Quarters,Sales,fill = Year)) + geom_bar(stat = "identity",position = "dodge") + ggtitle("Quarterly comparsion of sales")
ggplotly(ggplot(data_frame13,aes(Quarters,Sales,fill = Year)) + geom_bar(stat = "identity",position = "dodge") + ggtitle("Quarterly comparsion of sales"))



Question 7
##Determine the composition of quarter wise sale in 2015 and 2016

dataframe14 = SalesData%>%filter(Quarters=="Q1")%>%group_by(Quarters,Tier)%>%summarise(Total_2015 = sum(Sales2015))
dataframe15 = SalesData%>%filter(Quarters=="Q2")%>%group_by(Quarters,Tier)%>%summarise(Total_2015 = sum(Sales2015))
dataframe16 = SalesData%>%filter(Quarters=="Q3")%>%group_by(Quarters,Tier)%>%summarise(Total_2015 = sum(Sales2015))
dataframe17 = SalesData%>%filter(Quarters=="Q4")%>%group_by(Quarters,Tier)%>%summarise(Total_2015 = sum(Sales2015))    

PERCENTAGE:
pct14=round(dataframe14$Total_2015/sum(dataframe14$Total_2015)*100,1)
pct15=round(dataframe15$Total_2015/sum(dataframe15$Total_2015)*100,1)
pct16=round(dataframe16$Total_2015/sum(dataframe16$Total_2015)*100,1)
pct17=round(dataframe17$Total_2015/sum(dataframe17$Total_2015)*100,1)

Lables:
  lbls_14 = dataframe14$Tier%>%paste(":",pct14)%>%paste("%",sep = "") 
lbls_15 = dataframe15$Tier%>%paste(":",pct15)%>%paste("%",sep = "") 
lbls_16 = dataframe16$Tier%>%paste(":",pct16)%>%paste("%",sep = "") 
lbls_17 = dataframe17$Tier%>%paste(":",pct17)%>%paste("%",sep = "") 

CHARTS:
  pie(dataframe14$Total_2015,labels = lbls_14 ,col = c("lightgreen","green","darkgreen","red"),radius = 1,main = "Quarter1")
pie(dataframe15$Total_2015,labels = lbls_15 ,col = c("lightgreen","green","darkgreen","red"),radius = 1,main = "Quarter2")
pie(dataframe16$Total_2015,labels = lbls_16,col = c("lightgreen","green","darkgreen","red"),radius = 1,main = "Quarter3")
pie(dataframe17$Total_2015,labels = lbls_17 ,col = c("lightgreen","green","darkgreen","red"),radius = 1,main = "Quarter4")







