##Downloading flat files
if(!file.exists("data")){
        dir.create("data")
        fileUrl <- "https://data.baltimorecity.gov/api/views/dz54-2aru/rows.csv?accessType=DOWNLOAD"
        download.file(fileUrl, destfile = "./data/cameras.csv", method = "curl")
        list.files("./data")
}
#Reading flatfiles
readusingReadTable = read.table(("./data/cameras.csv"))
#error in above statement
readusingReadTable = read.table(("./data/cameras.csv"),header =T) 
#error in above statement
readusingReadTable = read.table(("./data/cameras.csv"),header =T,sep = ",") 
dim(readusingReadTable)
readingUsingReadCsv = read.csv("./data/cameras.csv")
head(readingUsingReadCsv)
# option nrows = to set number of rows to read , useful for initial reading analysys
#option skip to specify no of lines to skip, before starting to read, this migh be useful for some raw files
#option quote , often '' "" issue is overlooked

## READING EXCEL FILES -- as this is still the pop way to share data
if(!file.exists("data")){dir.create("data")}
fileUrl <- "https://data.baltimorecity.gov/api/views/dz54-2aru/rows.xlsx?accessType=DOWNLOAD"
download.file(fileUrl,destfile="./data/cameras.xlsx",method="curl")
#using xlsx package's read.xlsx or read.xlsx2 method
library(xlsx)
install.packages("xlsx")
readingExcelFile = read.xlsx("./data/cameras.xlsx")
#error in the above statement
?read.xlsx
readingExcelData = read.xlsx("./data/cameras.xlsx",sheetIndex = 1)
#error again
readingExcelData = read.xlsx("./data/cameras.xlsx",sheetIndex = 1,header = T)
##ERROR unresolved , illegalstateexception for invalid input stream UNRESOLVED
## maybe using some other popular packages might solve this issue

## reading xmls
library(XML)
install.packages("XML")
##UNRESOLVED package installaton failed
fileUrl <- "http://www.w3schools.com/xml/simple.xml"
doc <- xmlTreeParse(fileUrl,useInternal=TRUE)
rootNode <- xmlRoot(doc)
xmlName(rootNode)


## reading jsons
library(jsonlite)
jsonData <- fromJSON("https://api.github.com/users/jtleek/repos")
#error UNRESOLVED
install.packages('curl')
# installing error UNRESOLVED
names(jsonData)


##data.table from package data.table is similar to data.frame , withmodified syntax for subsetting and is faster than data.frame , use when speed is issue
install.packages("RMySQL")
library(RMySQL)
ucscDb <- dbConnect(MySQL(),user="genome", 
                    host="genome-mysql.cse.ucsc.edu")
result <- dbGetQuery(ucscDb,"show databases")
dbDisconnect(ucscDb)
##unresolved error aspackage is not available for this version of R, explore later

#skipping reading from HDF5, from web, from api , from other sources

##SUBSETTING REVIEW
set.seed(12345)
X <- data.frame("var1"=sample(1:5),"var2"=sample(6:10),"var3"=sample(11:15))
X
X <- X[sample(1:5),]; 
X$var2[c(1,3)] = NA
X[1,]
X[,1]
X[,"var1"]
X[1:2,"var2"]
#logical
X[(X$var1 <= 3 & X$var3 > 11),]
X[(X$var1 <= 3 | X$var3 > 15),]
X[X$var2 > 8,]
?which
X[which(X$var2 > 8),]
#which() considers NA's as false
which(X$var1 <8)
#which returns indeces of logical vectors
#sorting 
sort(X$var1)
sort(X$var1,decreasing = T)
#ordering
X[order(X$var2,X$var3,na.last = T),]

##ordering withplyr package, which has method arrange() 
library(plyr)
arrange
# so its sun=bstitutes using order function
arrange(X,var1)
arrange(X,desc(var1))
# adding rows
X$var4 = X$var1 
var5 = X$var4
X = cbind(X,X$var4)
X = cbind(X,var5)
X$`X$var4` = NULL
X
##Summarising data
if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://data.baltimorecity.gov/api/views/k5ry-ef3g/rows.csv?accessType=DOWNLOAD"
download.file(fileUrl,destfile="./data/restaurants.csv",method="curl")
restData <- read.csv("./data/restaurants.csv")
head(restData)
summary(restData)
str(restData)
quantile(restData$councilDistrict)
table(restData$zipCode)
sum(is.na(restData))
sum(is.na(restData$councilDistrict))
colSums(is.na(restData))
complete.cases(restData)
nrow(restData)-sum(complete.cases(restData))
table(restData$zipCode %in% c("21212","21213"))

##creating new variables
#using the restData from above
#sometimes we want to create sequences as a neww variable
seq(1,10,by = 2);seq(1,10,by = 3);seq_along(restData$name[1:10])
## Creating subsetting variables/##creating binary variables
restData$nearMe = restData$neighborhood %in% c("Roland Park", "Homeland")
table(restData$nearMe)
str(restData$nearMe);summary(restData$nearMe)

restData$zipWrong = ifelse(restData$zipCode < 0, TRUE, FALSE)
table(restData$zipWrong,restData$zipCode < 0)
str(restData$zipWrong)
## creating factor variables & rearranging their levels
class(restData$zipCode)
restData$zcf = factor(restData$zipCode)
class(restData$zcf)
str(restData$zcf)
restData$zcf[1:10]
## creating categorical variables from quantitative ones
restData$zipCodeGroup = cut(restData$zipCode,breaks = quantile(restData$zipCode))
class(restData$zipCodeGroup)
table(restData$zipCodeGroup,restData$zipCode)
str(restData$zipCodeGroup)
summary(restData$zipCodeGroup)
levels(restData$zipCodeGroup)
levels(restData$zipCodeGroup) = c(1,2,3,4)
levels(restData$zipCodeGroup)
str(restData$zipCodeGroup)
table(restData$zipCodeGroup,restData$zipCode)
##rearranging factor levels/reference
yesno <- sample(c("yes","no"),size=10,replace=TRUE)
yesno
yesnofac = factor(yesno,levels=c("yes","no"))
yesnofac
table(yesnofac)
as.numeric(yesnofac)
yesnofac = relevel(yesnofac,ref="no")
as.numeric(yesnofac)
## using MUTATE function
library(plyr)
restData = mutate(restData,zipcodeGrouping = cut(zipCode,breaks = quantile(zipCode)))
summary(restData$zipcodeGrouping)
restData[restData$zipCodeGroup != restData$zipcodeGrouping]
#levels are manually changed above 
#using new way to cut
install.packages("Hmisc")
library(Hmisc)
restData$zipCodeGroup = cut2(restData$zipCode,g = 4)
restData[restData$zipCodeGroup != restData$zipcodeGrouping]
levels(restData$zipcodeGrouping)
## transforming variables
# most common transformations are 
# abs(x) absolute value
# sqrt(x) square root
# ceiling(x) ceiling(3.475) is 4
# floor(x) floor(3.475) is 3
# round(x,digits=n) round(3.475,digits=2) is 3.48
# signif(x,digits=n) signif(3.475,digits=2) is 3.5
# cos(x), sin(x) etc.
# log(x) natural logarithm
# log2(x), log10(x) other common logs
# exp(x) exponentiating x

##RESHAPING
library(reshape2)
head(mtcars)
- ### melting
?melt()
mtcarsGE = mutate(mtcars,carnames=rownames(mtcars))
head(mtcarsGE)
mtcarsMelt = melt(mtcarsGE,id.vars = c("carnames","gear","cyl"),measure.vars = c("hp","disp"))
head(mtcarsMelt)
tail(mtcarsMelt)
mtcarsMelt[mtcarsMelt$carnames == "Datsun 710",]
- ### dcasting
?dcast
mtcarsDcast = dcast(mtcarsMelt,cyl~variable,mean)
mtcarsDcast
- ### without aggregate function default is length
mtcarsDcast = dcast(mtcarsMelt,cyl~variable)
mtcarsDcast
-- ### averaging
head(InsectSprays)
summary(InsectSprays)
str(InsectSprays)
-- ### averaging using tapply
tapply(InsectSprays$count,InsectSprays$spray,sum)
-- ### using split
insectSpraySplit = split(InsectSprays$count,InsectSprays$spray)
lapply(insectSpraySplit,sum)
unlist(lapply(insectSpraySplit, sum))
sapply(insectSpraySplit,sum)
-- ###using plyr
ddply(InsectSprays,.(spray),summarise,sum = sum(count))


## merging data
###important params of merge() x,y,by,by.x,by.y,all
if(!file.exists("./data")){dir.create("./data")}
fileUrl1 = "https://dl.dropboxusercontent.com/u/7710864/data/reviews-apr29.csv"
fileUrl2 = "https://dl.dropboxusercontent.com/u/7710864/data/solutions-apr29.csv"
download.file(fileUrl1,destfile="./data/reviews.csv",method="curl")
download.file(fileUrl2,destfile="./data/solutions.csv",method="curl")
reviews = read.csv("./data/reviews.csv"); solutions <- read.csv("./data/solutions.csv")
head(reviews,2)
head(solutions,2)

names(reviews)
names(solutions)
mergedRevSol =    merge(reviews,solutions)
names(mergedRevSol)
intersect(names(reviews),names(solutions))
head(reviews)
head(solutions)
range(solutions$problem_id)
range(reviews$solution_id)
summary(reviews)
summary(solutions)
mergedRevSol = merge(reviews,solutions,by.x = "reviewer_id",by.y = "subject_id")
mergedRevSol = merge(reviews,solutions,by.x = "reviewer_id",by.y = "subject_id",all=T)
199*205
mergedRevSol = merge(reviews,solutions,by.x = "solution_id",by.y = "id")
mergedRevSolAll = merge(reviews,solutions,by.x = "solution_id",by.y = "id",all=T)
mergedRevSolAll[-intersect(mergedRevSol$id,mergedRevSolAll$id),]
### where did solution id 205 come from if range of solution id is 1 to 201

### using plyr
df1 = data.frame(id=sample(1:10),x=rnorm(10))
df2 = data.frame(id=sample(1:10),y=rnorm(10))
arrange(join(df1,df2),id)
df1 = data.frame(id=sample(1:10),x=rnorm(10))
df2 = data.frame(id=sample(1:10),y=rnorm(10))
df3 = data.frame(id=sample(1:10),z=rnorm(10))
dfList = list(df1,df2,df3)
join_all(dfList)

## Editing Text Variables
if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://data.baltimorecity.gov/api/views/dz54-2aru/rows.csv?accessType=DOWNLOAD"
download.file(fileUrl,destfile="./data/cameras.csv",method="curl")
cameraData <- read.csv("./data/cameras.csv")
names(cameraData)
head(cameraData)
splitNames = strsplit( names(cameraData),"\\.")
splitNames
splitNames[6]
tolower(names(cameraData))
firstelement = function(X){
        X[1]
}
sapply(splitNames,firstelement)
names(cameraData) = sapply(splitNames, firstelement)
names(cameraData)
names(cameraData) = tolower(names(cameraData))
names(cameraData)
errorNames = c('messi_lionel','cristiano_Ronaldo','javier_hernadez_chicharito','wayneRooney','NeymarJr')
sub('_','',errorNames)
gsub('_','',errorNames)

grep('Alameda',cameraData$intersection)
grepl('Alameda',cameraData$intersection)
table(grepl('Alameda',cameraData$intersection))
cameraData2 = cameraData[!grepl('Alameda',cameraData$intersection),]
grepl("cristiano",cameraData$intersection)
grep("cristiano",cameraData$intersection)
length(grep("cristiano",cameraData$intersection))
library(stringr)
nchar(names(cameraData)[1])
substr("Cristiano",1,2)
paste0("Bhaskar","Manguluri")
paste("bhaskar","manguluri")
str_trim("bhaskar                   ")
###*regular expression can be used along with the above methods for further filtering*
###*lubridate package can be used for date manupulations*

## dplyr

###select

### filter

### arrange

### mutate

### rename

### summarise


