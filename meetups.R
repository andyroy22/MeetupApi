#
# Meetup API
#
# https://www.meetup.com/meetup_api/

# global
mkey <- "xxxxxxxxxx7444c702f394f67e637e" # set this to your Meetup API Key

# ex. mname = "Schaumburg-R-Consultants-Meetup"

library(httr)

meetup.GetMembers <- function(mname) {
   offset <- 0
   while (offset >= 0) {
      requrl <- paste0("https://api.meetup.com/2/members?&sign=true&key=",mkey,
                       "&photo-host=public&group_urlname=",mname,
                       "&page=20&offset=",offset)

      message(offset,requrl,appendLF=T)
      req <- GET(url=requrl)
      rsp <- content(req)

      message("count:",length(rsp$results))      
      df <- data.frame(name=sapply(rsp$results,function(x) x$name),
                       id=sapply(rsp$results,function(x) x$id),
                       city=sapply(rsp$results,function(x) x$city),
                       topics="", stringsAsFactors=F)
      for (i in 1:length(rsp$results))
         df$topics[i] <- paste0(sapply(rsp$results[[i]]$topics, function(x) x$name),collapse=",")
 
      if (offset == 0) {
         df.all <- df
      }
      else {
         df.all <- rbind(df.all,df)
      }

      if (length(rsp$results) < 20) 
         offset <- -1 # exit while
      else
         offset <- offset + 1
   }
   
   return (df.all)
}

# Example call
# df <- meetup.GetMembers("Schaumburg-R-Consultants-Meetup")
# View(df)
#
# write.csv(df,"Schaumburg-R-Consultants-Meetup members.csv",row.names = F)
#
# https://www.meetup.com/members/135745112/ << example of seeing member details >>


