library(stringr)
library(lubridate)

####Loading the datasets####
in_time <- read.csv("in_time.csv",stringsAsFactors = FALSE)
out_time <- read.csv("out_time.csv",stringsAsFactors = FALSE)



#### Cleaning in-time and out time####
#First Column Name
colnames(in_time)[1] <- "CustomerID"
colnames(out_time)[1] <- "CustomerID"

#Checking na values in both data set
in_na<-which(is.na(in_time))
out_na<-which(is.na(out_time))
sum(in_na!=out_na)


customer_id <- in_time$CustomerID

in_time1 <- in_time[,-1]
out_time1 <- out_time[,-1]

#converting into date-time format

for(i in 1:ncol(in_time1)){
  
in_time1[,i] <- as.POSIXct(in_time1[,i], format = "%Y-%m-%d %H:%M:%S")

}

for(i in 1:ncol(out_time1)){
  
  out_time1[,i] <- as.POSIXct(out_time1[,i], format = "%Y-%m-%d %H:%M:%S")
  
}


#### working on dataset####

#Creating data frame for storage
ave_working_hour <- data.frame(matrix(nrow = nrow(in_time1), ncol = 12))
colnames(ave_working_hour)<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
ave_working_hour[is.na(ave_working_hour)] <- 0

#Creating difference column for calculating difference in time.

difference<-(out_time1-in_time1)*60
difference[is.na(difference)] <- 0
difference<-mutate_all(difference, function(x) as.numeric(as.character(x)))

#Creating column name for 'FOR' Condition
colname<-as.Date(substring(colnames(in_time1),2),format = "%Y.%m.%d")
sum(is.na(colname))
colname
#Creation of monthly avg dataset 
for (i in 1:nrow(in_time1)) {
     sum=difference[i,1]
      Days=0
  
     for (j in 2:ncol(in_time1)) {
       
         { if((month(colname[j])!=month(colname[j-1]))| j==ncol(in_time1))
      {
           ave_working_hour[i,month(colname[j-1])]=sum/Days
      
      sum=difference[i,j]

      Days=0
      
      
         }
           
           
          
        else{
          sum=sum+difference[i,j]
          Days=Days+1
        }
      }   
    
  } 
}



