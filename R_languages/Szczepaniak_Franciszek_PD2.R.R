Badges <- read.csv("D:/przetwarzanie danych ustrukturyzowanych/projekt2/Badges.csv.gz")
Comments <- read.csv("D:/przetwarzanie danych ustrukturyzowanych/projekt2/Comments.csv.gz")
PostLinks <- read.csv("D:/przetwarzanie danych ustrukturyzowanych/projekt2/PostLinks.csv.gz")
Posts <- read.csv("D:/przetwarzanie danych ustrukturyzowanych/projekt2/Posts.csv.gz")
Tags <- read.csv("D:/przetwarzanie danych ustrukturyzowanych/projekt2/Tags.csv.gz")
Users <- read.csv("D:/przetwarzanie danych ustrukturyzowanych/projekt2/Users.csv.gz")
Votes <- read.csv("D:/przetwarzanie danych ustrukturyzowanych/projekt2/Votes.csv.gz")
options(stringsAsFactors=FALSE)
library(dplyr)
library(sqldf)
library(data.table)

# ZADANIE 1

#sqldf
df_sql_1 <- function(df1) {
  sqldf<-sqldf("SELECT Count, TagName
   FROM Tags
   WHERE Count > 1000
   ORDER BY Count DESC")
  sqldf
}
sqldf_1 <- df_sql_1(Tags)
sqldf_1



# funkcje podstawowe
df_base_1 <- function(df1) {
  base <- na.omit(df1[ df1$Count > 1000 , c("Count","TagName") ])
  base <- base[order(base$Count, decreasing =TRUE),]
  rownames(base)<- NULL
  base
}
base_1 <- df_base_1(Tags)
base_1

#dplyr
df_dplyr_1 <- function(df1) {
  dplyr <- Tags %>% filter(Count>1000) %>% select(Count,TagName) %>% arrange(desc(Count))
  dplyr
}
dplyr_1 <- df_dplyr_1(Tags)
dplyr_1


#data.table
df_table_1 <- function(df1) {
  tags <- setDT(Tags)
  table <- tags[Count >1000, .(Count,TagName)][order(-Count)]
  table
}
table_1 <- df_table_1(Tags)
table_1


# Sprwadzenie poprawnoœci i czasóW

dplyr::all_equal(sqldf_1, base_1,dplyr_1)
dplyr::all_equal(dplyr_1,table_1)

microbenchmark::microbenchmark(
  sqldf = df_sql_1(Tags),
  base = df_base_1(Tags),
  dplyr = df_dplyr_1(Tags),
  data.table = df_table_1(Tags),times=10L)

#ZADANIE 2

#sqldf
df_sql_2 <- function(df1,df2) {
  sqldf <- sqldf("SELECT Location, COUNT(*) AS Count
  FROM (
  SELECT Posts.OwnerUserId, Users.Id, Users.Location
  FROM Users
  JOIN Posts ON Users.Id = Posts.OwnerUserId
  )
  WHERE Location NOT IN ('')
  GROUP BY Location
  ORDER BY Count DESC
  LIMIT 10")
  sqldf
}
sqldf_2 <- df_sql_2(Posts,Users)
sqldf_2

# base
df_base_2 <- function(df1,df2) {
  Posts <- as.data.frame(Posts)
  Users <- as.data.frame(Users)
  pom <- merge(Users,Posts, by.x="Id",by.y="OwnerUserId")
  pom <- pom[,c("Id","Location")]
  base <- pom[!(pom$Location %in% ("")),]
  base <- aggregate(x = base$Location,by= base["Location"], FUN = length)
  colnames(base)[2] <- "Count"
  base <- base[order(base$Count, decreasing = TRUE),]
  rownames(base)<- NULL
  head(base,10)
}
base_2<-df_base_2(Posts,Users)
base_2


#dplyr
df_dplyr_2 <- function(df1,df2) {
  dplyr <- Users %>% inner_join(Posts, by=c("Id"="OwnerUserId")) %>% 
    select(Id,Location) %>% filter(Location!='') %>% select(Location) %>% 
    group_by(Location) %>% count(Location,sort=TRUE) %>% head(10) %>% rename(Count=n)
  
  dplyr
}
dplyr_2 <- df_dplyr_2(Posts,Users)
dplyr_2

#data.table
df_table_2 <- function(df1,df2) {
  users <- setDT(Users)
  posts <- setDT(Posts)
  table <- merge(users,posts,by.x="Id",by.y="OwnerUserId")
  table <- table[Location != (''),.N,by=.(Location)][order(-N)][,.(Location,Count=N)]
  head(table,10)
}
table_2 <- df_table_2(Posts,Users)
table_2

# Sprwadzenie poprawnoœci i czasóW


dplyr::all_equal(sqldf_2, base_2,dplyr_2)
dplyr::all_equal(dplyr_2,table_2)

microbenchmark::microbenchmark(
  sqldf = df_sql_2(Posts,Users),
  base = df_base_2(Posts,Users),
  dplyr = df_dplyr_2(Posts,Users),
  data.table = df_table_2(Posts,Users),times=10L)


# ZADANIE 3
#sqldf
df_sql_3 <- function(df1) {
  sqldf<-sqldf("SELECT Year, SUM(Number) AS TotalNumber
  FROM (
  SELECT
  Name,
  COUNT(*) AS Number,
  STRFTIME('%Y', Badges.Date) AS Year
  FROM Badges
  WHERE Class = 1
  GROUP BY Name, Year
  )
  GROUP BY Year
  ORDER BY TotalNumber")
  sqldf
}
sqldf_3 <- df_sql_3(Badges)
sqldf_3

#base
df_base_3 <- function(df1) {
  date <- as.POSIXct(Badges$Date, format ="%Y-%m-%dT%H:%M:%S%OS" )
  Badges$Year <- format(date, format="%Y")
  pom <- aggregate(x=Badges[Badges$Class == 1,c("Name","Year")],
                   by=Badges[Badges$Class == 1,c("Name","Year")],
                   FUN = length)
  pom[4]<- NULL
  colnames(pom)[3] <- "Number"
  rownames(pom) <- NULL
  pom <- aggregate(x=pom$Number, by=list(Year=pom$Year),FUN = sum)
  colnames(pom)[2]<- "TotalNumber"
  pom <- pom[order(pom$TotalNumber),]
  rownames(pom) <- NULL
  pom
  
  
}

base_3 <- df_base_3(Badges)
base_3


#dplyr
df_dplyr_3 <- function(df1) {
  date <- as.POSIXct(Badges$Date, format ="%Y-%m-%dT%H:%M:%S%OS" )
  Badges$Year <- format(date, format="%Y")
  dplyr <- Badges %>% filter(Class==1) %>% group_by(Name,Year) %>%
    count(Name,Year) %>% rename(Number=n) %>% group_by(Year) %>%
    summarize(TotalNumber=sum(Number)) %>% arrange(TotalNumber)
  dplyr
}

dplyr_3 <- df_dplyr_3(Badges)
dplyr_3

#data.table
df_table_3 <- function(df1) {
  date <- as.POSIXct(Badges$Date, format ="%Y-%m-%dT%H:%M:%S%OS" )
  Badges$Year <- format(date, format="%Y")
  badges <- setDT(Badges)
  table <- (badges[Class==1,.N,by=.(Name,Year)]
            [,.(Name,Year,Number=N)][,sum(Number),by= Year]
            [,.(Year,TotalNumber=V1)][order(TotalNumber)])
  
  table
}
table_3 <- df_table_3(Badges)
table_3 


# Sprwadzenie poprawnoœci i czasóW


dplyr::all_equal(sqldf_3, base_3,dplyr_3)
dplyr::all_equal(dplyr_3,table_3)

microbenchmark::microbenchmark(
  sqldf = df_sql_3(Badges),
  base = df_base_3(Badges),
  dplyr = df_dplyr_3(Badges),
  data.table = df_table_3(Badges),times=10L
)

# ZADANIE 4

#sqldf
df_sql_4 <- function(df1,df2) {
  sqldf<-sqldf("SELECT
  Users.AccountId,
  Users.DisplayName,
  Users.Location,
  AVG(PostAuth.AnswersCount) as AverageAnswersCount
  FROM
  (
  SELECT
  AnsCount.AnswersCount,
  Posts.Id,
  Posts.OwnerUserId
  FROM (
  SELECT Posts.ParentId, COUNT(*) AS AnswersCount
  FROM Posts
  WHERE Posts.PostTypeId = 2
  GROUP BY Posts.ParentId
  ) AS AnsCount
  JOIN Posts ON Posts.Id = AnsCount.ParentId
  ) AS PostAuth
  JOIN Users ON Users.AccountId=PostAuth.OwnerUserId
  GROUP BY OwnerUserId
  ORDER BY AverageAnswersCount DESC, AccountId ASC
  LIMIT 10
")
  sqldf
}
sqldf_4 <- df_sql_4(Users,Posts)
sqldf_4



#base
df_base_4 <- function(df1,df2) { 
  Posts <- as.data.frame(Posts)
  Users <- as.data.frame(Users)
  AnsCount <- Posts[Posts$PostTypeId==2,]
  AnsCount <- aggregate(AnsCount$ParentId,AnsCount["ParentId"],length)
  colnames(AnsCount)[2] <- "AnswersCount"
  pom <- merge(Posts,AnsCount,by.x="Id",by.y="ParentId")
  PostAuth <- pom[,c("AnswersCount","Id","OwnerUserId")] 
  base <- merge(Users,PostAuth,by.x ="AccountId", by.y="OwnerUserId")
  base <- aggregate(x= base$AnswersCount,
                    b= base[c("AccountId","DisplayName","Location")],
                    FUN = function(x) c(AverageAnswersCount= mean(x)))
  colnames(base)[4] <- "AverageAnswersCount"
  base <- base[ order(base$AverageAnswersCount,base$AccountId , decreasing = c(TRUE, FALSE)), ]
  
  
  head(base,10)
  
}


base_4 <- df_base_4(Users,Posts)
base_4


#dplyr
df_dplyr_4 <- function(df1,df2) {
  AnsCount <- Posts %>% filter(PostTypeId==2) %>% group_by(ParentId) %>%
    count(ParentId) %>% rename(AnswersCount=n)
  PostAuth <- Posts %>% inner_join(AnsCount, by=c("Id"="ParentId")) %>%
    select(AnswersCount,Id,OwnerUserId)
  dplyr <- Users %>% inner_join(PostAuth,by=c("AccountId"="OwnerUserId")) %>%
    group_by(AccountId,DisplayName,Location) %>% 
    summarize(AverageAnswersCount = mean(AnswersCount, na.rm=TRUE)) %>%
    arrange(desc(AverageAnswersCount),AccountId) %>%  head(10)
  
  dplyr
  
}
dplyr_4 <- df_dplyr_4(Users,Posts)
dplyr_4


#data.table
df_table_4 <- function(df1,df2) {
  posts <- setDT(Posts)
  users <- setDT(Users)
  AnsCount<- posts[PostTypeId==2,.N,by=.(ParentId)][,.(ParentId,AnswersCount=N)]
  AnsCountPosts <- merge(Posts,AnsCount,by.x="Id",by.y="ParentId")
  PostAuth <- AnsCountPosts[,.(AnswersCount,Id,OwnerUserId)]
  PostAuthUsers <- merge(Users,PostAuth,by.x="AccountId",by.y="OwnerUserId")
  table <- (PostAuthUsers[,mean(AnswersCount),by=.(AccountId,DisplayName,Location)]
            [,.(AccountId,DisplayName,Location,AverageAnswersCount=V1)]
            [order(-AverageAnswersCount,AccountId)])
  
  head(table,10)
  
}
table_4 <- df_table_4(Users,Posts)
table_4

# Sprwadzenie poprawnoœci i czasóW
dplyr::all_equal(sqldf_4, base_4,dplyr_4)
dplyr::all_equal(dplyr_4,table_4)

microbenchmark::microbenchmark(
  base = df_base_4(Users,Posts),
  sqldf = df_sql_4(Users,Posts),
  dplyr = df_dplyr_4(Users,Posts),
  data.table = df_table_4(Users,Posts),times=10L
)

