#
# Meetup API
#
# https://www.meetup.com/meetup_api/

# global
# set this to your Meetup API Key
# mkey <- 'xxxxxxxxxxxxxxxxx2f394f67e637e'
# find it here: https://secure.meetup.com/meetup_api/key/
mkey <- readRDS("mkey.rds")

# ex. mname = "Schaumburg-R-Consultants-Meetup"

library(httr)

meetup.GetMembers <- function(mname) {
   offset <- 0
   while (offset >= 0) {
      requrl <- paste0("https://api.meetup.com/2/members?&sign=true&key=",mkey,
                       "&photo-host=public&group_urlname=",mname,
                       "&page=20&offset=",offset)

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
      else {
         offset <- offset + 1
         Sys.sleep(.5)     # avoid being throttled by Meetup API by pacing requests
      }
   }
   
   return (df.all)
}

# Example Call
# Note that the 'mname' parameter should be 
# extracted exactly from the Meetup's url
#
# df <- meetup.GetMembers("Schaumburg-R-Consultants-Meetup")
# View(df)
#
## Examples (uncomment and try it)
#
# df$name[df$city=="Schaumburg"]         # members in Schaumburg
# df$name[grepl("R$|R ",df$topics)]      # members with topics that mention 'R'

## browse by name
# nm <- "Ganesh Palani"
# browseURL(paste0("https://www.meetup.com/members/",df$id[df$name==nm],"/"))

## browse by subset
# dfr <- df[grepl("R$|R ",df$topics),c("name","id")]
# nrow(dfr)

## random sample
# for (i in sample(nrow(dfr),5))
#    browseURL(paste0("https://www.meetup.com/members/",dfr$id[i],"/"))
