# CIS/STA 3920 - Data Mining for Business Analytics
# HW1
# Instructor: Charlie Terng
# Student: @ Xukun LIU

getwd()
setwd("/Users/liuxukun/Google Driver/2018Spring/CIS3920/Assignments/HW1/data")
getwd()

preview_data = read.csv("2004.csv", header = TRUE, sep = ",")
preview_data[1:3,]
nrow(preview_data)
ncol(preview_data)
preview_data[1:10,1:3]

# 1-Combining Datasets

my_data = NULL
temp_data<-data.frame()

for (year in 2004:2016) # 2004.csv
{
  filename = paste(year, ".csv", sep = "", collapse = NULL)
  print(filename)
  
  temp_data = read.csv(filename, header = TRUE)
  features = c("id","sale","units","rating","product", "industry","country","return.client","year")
  temp_data$year = year #create and assgin col "year"
  temp_data = temp_data[,features] 
  
  if(is.null(my_data))
  {#print(is.null(my_data))
    my_data = temp_data}
  else
  {print(is.null(my_data))
    my_data = rbind(my_data,temp_data)}
}

my_data[1:10,]

# 2-Examing the Dataset

dim(my_data) #7739-obervation / 9 varibales
str(my_data)

class(my_data$id)
levels(my_data$product)
length(levels(my_data$product))

# 3.1-Handing missing values
my_length = length(my_data$id)
my_length
names(my_data)
variables = names(my_data)
variables
class(variables)
count_num = vector(mode="integer", length = length(variables))
count_num
count_na = data.frame(variables,count_num)
count_na
names(count_na)
str(count_na)

for (m in 1:my_length) {
  for (n in 1:length(variables))  # length(variables) V.S variables
    {
    if(is.na(my_data[m,n])){
      count_na[n,"count_num"] = count_na[n,"count_num"] + 1}
    }
}

count_na
plot(count_na,xlab="Vars",ylab="Missing",main="NA count")

# 3.2
class(my_data$product)
my_data$product=as.character(my_data$product)
class(my_data$product)
my_data$product[is.na(my_data$product)]="Delta"

# 3.3
class(my_data$industry)
my_data$industry = as.character(my_data$industry)
class(my_data$industry)
my_data$industry[my_data$industry=="999"]=NA
my_data$industry[is.na(my_data$industry)]

# 3.4 remove all overvation
my_data = na.omit(my_data)

# 4.1 Recoding Values
class(my_data$country)
summary(my_data$country)

my_count=0
for (test in my_data$country=="Singapore, ") {
  if(test==T)
    {my_count=my_count+1}
}
my_count

my_data[my_data$country=="Singapore, ","country"] = "Singapore"

my_count=0
for (test in my_data$country=="Singapore, ") {
  if(test==T)
  {my_count=my_count+1}
}
my_count

summary(my_data$country)
my_data[my_data$country=="SWitzerland","country"] <- "Switzerland"
my_data[my_data$country=="Switzerland, Switzerland","country"] <- "Switzerland"
summary(my_data$country)

my_data$country <- as.factor(my_data$country)
levels(my_data$country)

# we could  also use droplevels() function to remove extra(containing no value) levels.

# 4.2 
country_check = c("Singapore", "Switzerland", "United States", "United Kingdom")
! my_data$country %in% country_check

my_data$country = as.character(my_data$country)
my_data$country[!(my_data$country %in% country_check)] <- "other"
my_data$country

# 4.3
class(my_data$product)
my_data$product[my_data$product=="Series A13"|my_data$product=="Series A2"]="Series A"
my_data$product[my_data$product=="Series B55"]="Series B"
summary(my_data$product)

# 5.1
class(my_data$sale)
my_data$sale = as.numeric(my_data$sale)
class(my_data$sale)
class(my_data$units)
my_data$units = as.numeric(my_data$units)
class(my_data$units)

my_data$sale.per.unit = my_data$sale/my_data$units
class(my_data$sale.per.unit) # int/int -> numeric

# 5.2
class(my_data$rating)
my_data$rating.level[ 5 <= my_data$rating ] = "excellent"
my_data$rating.level[ 4 <= my_data$rating & my_data$rating < 5] = "satisfactory"
my_data$rating.level[ my_data$rating < 4] = "poor"

# 5.3 Create dummy variables for levels “biotech” and “environmental” from the “industry” variable.
class(my_data$industry)
my_data$biotech <- as.numeric(my_data$industry=="biotech")
my_data$environmental <- as.numeric(my_data$industry=="environmental")

# 5.4 Create a variable “priority”,
# in which a value of 1 is given for clients who are returning clients (“return.client” = 1)
# and have given a poor rating (“rating.level” = “poor”). Both conditions must be met.
class(my_data$return.client)
class(my_data$rating.level)
my_data$priority = as.numeric( my_data$return.client==1 & my_data$rating.level=="poor")

# 6.1 Show the new dimensions of the dataset
dim(my_data)

# 6.2 Check the data types to make sure they are appropriate for the variable
# (ex. “product”, “industry”, and “country” variables should be factors)
summary(my_data)
str(my_data)
my_data$product <- as.factor(my_data$product)
my_data$industry <- as.factor(my_data$industry)
my_data$country <- as.factor(my_data$country)
str(my_data)

# 7.1 Export dataset
?write.csv
write.csv(my_data,"LIU_Xukun.csv")

# 7.2 rewrite without index number
write.csv(my_data,"LIU_Xukun.csv",row.names = F)

# CIS/STA 3920 - Data Mining for Business Analytics
# HW1
# Instructor: Charlie Terng
# Student: @ Xukun LIU