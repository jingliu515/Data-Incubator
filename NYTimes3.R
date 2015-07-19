
## This code extracts people's names and number of New York Times reports tagged by their names.
## Their birthdate information is furthur scraped from wiki.dbpedia.org.
## Scrapped information is used to find how you should be named and at what day of a year you 
## have to be born to maximize your chance of making a name in New York Times.

## This code scrapes data from the Internet and runs very slowly. 
## You may not want to run the whole code. 
## Instead, the standardized data is uploaded in GitHub for your inspection.
## Given more time, it should be optimized. 

## Code was written in RStudo 3.1.2 Windows 7
## 2015-07-18


getwd()
library(XML)
library(lubridate)
library(ggplot2)
library(dplyr)
library(DescTools)

######################################################################
## 1. extract person's name and links from http://data.nytimes.com/

url<-paste("http://data.nytimes.com/home/",letters,".html",sep="") # html pages to be extracted 

## Find all the persons
## url<-"http://data.nytimes.com/home/a.html"
person<-data.frame()

for (i in 1:26) {
  doc<-htmlTreeParse(url[i], useInternalNode=T) ## page A, page B, ...
  ny_root<-xmlRoot(doc)
  #xmlSize(ny_root)
  #xmlSApply(ny_root, xmlName)
  #xmlSApply(ny_root, xmlSize)
  
  ny_body<-xmlChildren(ny_root)$body
  new_person<-readHTMLTable(ny_root, which=1) ## readHTMLTable
  
  new_person<-subset(new_person, V1=="Person", select=c(V2, V3)) 
      ## only extract peroson's information. Other categories (e.g., organization) are ignored.
  
  person<-rbind(person,new_person)
}

names(person)<-c("name","url") ## assign columns with meaningful names

dim(person)[1]
# 4978 # There are a total of 4978 entries/people

# person<-subset(sig_content, V1=="Person")
# person<-subset(person, select=c(V2, V3))
# names(person)<-c("name","url")

##########################################################################################
## 2. In the data.nytimes.com page for each individual person,
## find how many articles were written about him/her. Also, extract
## the link out to wiki.dbpedia.org, from which birthdate can be scrapped.

for (i in 1:dim(person)[1]) {
  doc2<-htmlTreeParse(person$url[i], useInternalNode=T)
  
  ny_root2<-xmlRoot(doc2)
  
  #xmlSize(ny_root2)
  #xmlSApply(ny_root2, xmlName)
  #xmlSApply(ny_root2, xmlSize)
  
  # ny_body2<-xmlChildren(ny_root2)$body
  sig_content2<-readHTMLTable(ny_root2, which=1) ## readHTMLTable
  
  # articleNum<-sig_content2$V2[sig_content2$V1=="nyt:associated_article_count"]
  # firstuse<-sig_content2$V2[sig_content2$V1=="nyt:first_use"]
  
  articleNum<-as.character(sig_content2$V2[sig_content2$V1=="nyt:associated_article_count"])
  person$articleNum[i]<-ifelse(length(articleNum)==0, 0, articleNum)
  
  # firstuse<-as.character(sig_content2$V2[sig_content2$V1=="nyt:first_use"])
  # person$firstuse[i]<-ifelse(length(firstuse)==0, NA, firstuse)

  # lastuse<-as.character(sig_content2$V2[sig_content2$V1=="nyt:latest_use"])
  # person$lastuse[i]<-ifelse(length(lastuse)==0, NA, lastuse)
                              
  person$dbLink[i]<-as.character(sig_content2[sig_content2$V1=="owl:sameAs",]$V2[2]) ## obtain the link to DBPedia
  
}

## While debugging, I found that some people's dbpedia.org pages are empty and 
## couldn't be loaded, forcing the for-loop in step 3 to abort. These pages 
## have % sign in their url. I'll remove these entries with the following code.

brokenPage<-sapply(person$dbLink, grepl, pattern="%")
person<-person[!brokenPage,]
sum(brokenPage)
# 268 # There are 268 person with broken dbpedia.org page.

## In addition, some people do not have links directing to dbpedia.org and 
## thus their birthdate information could not be found. 
## These people are subsetted into a different data frame: person2.
## Their birthdate information is searched in Google and Wikipedia and 
## manually annotated and combined with the main data frame person later. 

brokenPage2<-sapply(person$dbLink, grepl, pattern="rdf.freebase.com")
sum(brokenPage2)
# 53 # There are 53 person with no dbpedia pages. Their birthdate is manually annotated.

person2<-person[brokenPage2,] ## these 53 person are set aside for manual annotation.

person<-person[!brokenPage2,]

##############################################################
## 3. extract birthdate information from wiki.dbpedia.org

for (i in 1:dim(person)[1]) {
  
  ## for other unidentified reasons (probably due to broken link in dbpedia.org or 
  ## unstable Internet connection), the for-loop might abort.
  ## In such cases, I located where it stopped and manually annotated 
  ## the person's birthdate. Or I waited for the Internet connection got better. 
  ## And then let the for-loop resume from the next cycle. 
  
#  response<- GET(url)
# if (response$status_code!=200){ # HTTP request failed!!
#  # do some stuff...
#    print(paste("Failure:",i,"Status:",response$status_code))
#   next
# }
  
  
  doc3<-htmlTreeParse(person$dbLink[i], useInternalNode=T)  
  ny_root3<-xmlRoot(doc3)
  
  # xmlSize(ny_root3)
  # xmlSApply(ny_root3, xmlName)
  # xmlSApply(ny_root3, xmlSize)
  
  # ny_body3<-xmlChildren(ny_root3)$body
  
  sig_content3<-readHTMLTable(ny_root3, which=1) ## readHTMLTable
  
  # birthDate<-sig_content3$V2[sig_content3$V1=="dbpedia-owl:birthDate"]
  # birthPlace<-sig_content3$V2[sig_content3$V1=="dbpedia-owl:birthPlace"]
  
  birthDate<-as.character(sig_content3$V2[sig_content3$V1=="dbpedia-owl:birthDate"])
  person$birthDate[i]<-ifelse(length(birthDate)==0,NA,birthDate)
  
  # birthPlace<-as.character(sig_content3$V2[sig_content3$V1=="dbpedia-owl:birthPlace"])
  # person$birthPlace[i]<-ifelse(length(birthPlace)==0,NA,birthPlace)
}

##############################################################
## 4. cleaning up data, remove irrelevent intermediate data, 
## convert data into desired format and transforma data.

## read in the manually annotated 53 person
person2<-read.csv("./person2.csv",header=T, row.names=1) 
person<-rbind(person,person2)
    ## these two lines won't execute on your computer
    ## the cleaned data is uploaded to GitHub for your convenience.

person$birthDate<-ymd(substr(person$birthDate, start=1, stop=10))  ## three entries failed to parse
person$birthMonth<-month(person$birthDate)
person$birthDay<-day(person$birthDate)

person$zodiac<-sapply(person$birthDate, Zodiac, lang="en") ## find zodiac signs

person$name<-as.character(person$name)

# a<-strsplit(person$name,split=", ")
# df <- data.frame(matrix(unlist(a), nrow=dim(person)[1], byrow=T))
# names(df)<-c('lastname','firstname')
# person<-cbind(person,df)

# person$lastname<-strsplit(person$name,split=", ")[1]
# person$firstname<-strsplit(person$name,split=", ")[[1]][2]

person$lastname<-sapply(strsplit(person$name, split=", "), function(x) x[1])
person$firstname<-sapply(strsplit(person$name, split=", "), function(x) x[2])

person$articleNum<-as.numeric(person$articleNum)

# person$firstuse<-ymd(person$firstuse)
# person$lastuse<-ymd(person$lastuse)

person<-subset(person, select=c(name, lastname, firstname, articleNum, 
                                birthDate, birthMonth, birthDay, zodiac))

# write.table(person, "./personClean.txt") # save cleaned data

#birthPlace<-as.character(birthPlace)
#birthPlace<-gsub("dbpedia:","",birthPlace)
#birthPlace<-strsplit(birthPlace,"\n\t")
#birthState<-strsplit(birthPlace,"\n\t")[1]
#birthCity<-strsplit(birthPlace,"\n\t")[2]

##############################################################
## 5. Data analysis and visualization 

summary<-summarize(group_by(person, birthMonth, birthDay),
                   count=n(),
                   fame=mean(articleNum))
summary<-summary[2:366,] 
    # 01-01 is not a valid data point because when only the year of birth is known, birth day is assigned 01-01
    # Therefore these data points are removed. People with missing birthdate information are also excluded from analysis. 
   
## normalize data
attach(summary)
summary$fame<-(fame-min(fame))/(max(fame)-min(fame))*100
# summary$percent<-count/sum(count)*100
detach(summary)  

### First Plot ###
myplot<-ggplot(summary, aes(x=birthDay, y=birthMonth, fill=fame)) +
          geom_point(aes(size=count), shape=21) +
          scale_size_area(max_size=18) +
          scale_fill_gradient(low="white", high="blue")

myplot2<-myplot + xlab("Day") + ylab("Month") + ggtitle("When were NYTimes Celebrities born?") +
              theme(axis.title.x=element_text(size=16, face="bold"), axis.title.y=element_text(size=16, face="bold"),
              plot.title=element_text(size=18, face="bold", color="blue"))

#monName<-month.abb[1:12]
myplot3<-myplot2 + scale_x_discrete(breaks=1:31) +
                   scale_y_discrete(labels=month.abb[1:12])

myplot3 

### Second Plot ###
personZodiac<-person[!is.na(person$zodiac),]

# with(person, plot(articleNum ~ zodiac, ylim=c(0,150))) # exploratory plot

myplot4<-ggplot(personZodiac, aes(x=zodiac, y=articleNum))+
  geom_boxplot() +
  scale_y_continuous(limits=c(0,150))

myplot5<-myplot4 + xlab("Zodiac Sign") + ylab("Article Number") + 
  ggtitle(expression(atop("What zodiac signs make the biggest name?", atop(italic("(some outliers truncated)"), "")))) +
  theme(axis.title.x=element_text(size=16, face="bold"), axis.title.y=element_text(size=16, face="bold"),
        plot.title=element_text(size=18, face="bold", color="blue"))

myplot5
