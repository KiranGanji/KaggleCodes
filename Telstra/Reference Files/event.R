 #setting the directory
setwd("C:/Users/ganji.kiran/Downloads/Telstra/Reference_files")

event_df<-read.csv("event_type.csv")
resource_df<-read.csv("resource_type.csv")
log_df<-read.csv("log_feature.csv")
severity_df<-read.csv("severity_type.csv")

#Locations
location_df<-read.csv("location.csv")
location_df2<-with(location_df,data.frame(id,model.matrix(~location3-1,location_df)))
write.csv(location_df2,"location_seg.csv")

#Event encodings
event_df2<-with(event_df,
     data.frame(id,model.matrix(~event_type-1,event_df)))
write.csv(event_df3,"event_seg.csv")

event_df3<-aggregate(. ~ id, data=event_df, FUN=sum)
write.csv(event_df3,"event_busy_exp.csv")

#Resource Encodings
resource_df2<-with(resource_df,
                data.frame(id,model.matrix(~resource_type-1,resource_df)))

resource_df3<-aggregate(. ~ id, data=resource_df2, FUN=sum)
write.csv(resource_df3,"resource_seg.csv")

#Severity Encodings
severity_df2<-with(severity_df,
                   data.frame(id,model.matrix(~severity_type-1,severity_df)))

severity_df3<-aggregate(. ~ id, data=severity_df2, FUN=sum)
write.csv(severity_df3,"severity_seg.csv")

#log Features Encodings
log_df2<-with(log_df,
              data.frame(id,volume,model.matrix(~log_feature-1,log_df)))
log_df2[-(1:2)] <- log_df2[["volume"]] * log_df2[-(1:2)]
log_df3<-aggregate(. ~ id, data=log_df2, FUN=sum)
write.csv(log_df3,"log_seg.csv")

#Assuming Event as Weeks calculating time taken between each
df2<-data.frame()
for(i in 1:18552){
  df1<-subset(event_df,id==i)
  df2[i,"id"]<-i
  df2[i,"time_taken"]<-(max(df1$event_type)-min(df1$event_type))
}

#Busy level Encodings
#Busy levels: Number of ids in a event and sorted from 1 to 53 coded as busy levels
busy_df<-read.csv("busy_level.csv")
busy_df2<-with(busy_df, data.frame(id,model.matrix(~busy_level-1,busy_df)))
busy_df2<-aggregate(.~id, data=busy_df2, FUN=sum)
write.csv(busy_df2,"busy_seg.csv")








