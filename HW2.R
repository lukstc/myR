# CIS/STA 3920 - Data Mining for Business Analytics
# HW2
# Instructor: @ Charlie Terng
# Student: @ Xukun LIU

# In-class lab assignment
setwd("~/Google Driver/2018Spring/CIS3920/Assignments/HW2")
getwd()

# 1.1 Read in the Facebook data hosted from a link
fb = read.csv(url("http://guides.newman.baruch.cuny.edu/ld.php?content_id=39953204"),sep=";")
# preview the data set
head(fb)

# 1.2 Get an initial understanding of how the data is structured
dim(fb)
str(fb)

# 1.3 Take a look at the variable types. Do they make sense for the given variable?
fb$Category = as.factor(fb$Category)
fb$Type = as.factor(fb$Type)
fb$Post.Month = as.factor(fb$Post.Month)
fb$Post.Weekday = as.factor(fb$Post.Weekday)
fb$Post.Hour = as.factor(fb$Post.Hour)
fb$Paid = as.factor(fb$Paid)

str(fb)

# 1.4 The “Category” variable indicates its marketing purpose (see Appendix for details).
# Take a look at the levels using levels( ). We see it has values of 1, 2, or 3.
levels(fb$Category)

# 1.5 We want to convert the “Category” values to the following: 
# “1” : “action”, “2” : “product”, “3” : “inspiration”. 
fb$Category = as.character(fb$Category)
fb$Category[fb$Category=="1"] = "action"
fb$Category[fb$Category=="2"] = "product"
fb$Category[fb$Category=="3"] = "inspiration"
fb$Category = as.factor(fb$Category)
levels(fb$Category)

# 1.6 How many missing values do we have in the dataset?
colSums(is.na(fb))
sum(is.na(fb))
fb = na.omit(fb)

# Basic descriptive statistics:
# 2.1 Provide descriptive statistics the “like” variable (min, max, median, Q1, Q3, mean, and standard deviation)
summary(fb$share)
sd(fb$share)

# 2.2 What do the summary statistics for “like” tell us about the cosmetic company’s social media performance on Facebook?
summary(fb$like)
sum(fb$like)
sd(fb$like)

# 2.3 Plot a histogram of “like” using the hist( ) function. 
hist(fb$like)
?hist
hist(fb$like,main = "Histogram of Likes",xlab = "# of like",ylab = "# of post")
hist(fb$like,main = "Histogram of Likes",xlab = "# of like",ylab = "# of post",breaks = 100)
hist(fb$like,main = "Histogram of Likes",xlab = "# of like",ylab = "# of post",breaks = 100, xlim = c(1,1000))

# 2.4 Provide descriptive statistics of the categorical variables with summary( ) function (frequency, proportions)
summary(fb$Type)
summary(fb$Post.Month)
plot(fb$Type)
plot(fb$Post.Month,las=2,main="Post by Month",xlab="Month",ylab="# of post")

###Compate Like vs. Share, using scatter plot###
x=(fb$like)
y=(fb$share)
plot(x,y,xlab="likes", ylab="shares",main = "likes vs shares")
plot(x,y,xlab="likes", ylab="shares",main = "likes vs shares",xlim=c(0,1000),ylim= c(0,200))

x=fb$Post.Month
y=fb$like
plot(x,y,ylim=c(0,500),las=2,xlab="month",ylab = "likes")

###PAID###

summary(fb$Paid)
?aggregate
aggregate(fb$like~fb$Paid,FUN=summary)

### Analyze paid vs non-paid posts ###

par(mfrow=c(1,2))
#par(mfrow=c(1,2))
#par(mfcol=c(2,2))

x = fb[fb$Paid=="0", "Type"]    # This is x axis, type of post for non-paid posts
y = fb[fb$Paid=="0", "like"]      # This is y axis, likes for non-paid posts
plot(x, y, las=2,ylab="likes",ylim=c(0,500),main="Non-paid")

x = fb[fb$Paid=="1", "Type"]    # This is x axis, type of post for non-paid posts
y = fb[fb$Paid=="1", "like"]      # This is y axis, likes for non-paid posts
plot(x, y, las=2,ylab="likes",ylim=c(0,500),main="Paid")

# CIS/STA 3920 - Data Mining for Business Analytics
# HW2
# Instructor: Charlie Terng
# Student: @ Xukun LIU

# 1 How many shares on average does each post get?
summary(fb$share)
sum(fb$share)
sd(fb$share)
mean(fb$share)

# 2 What is the most shares a post has gotten? What is the fewest?
max(fb$share)
min(fb$share)

# 3 Plot a histogram of it with enough breaks to get a clear look.
# 3 What is the message you get from this? (refer to section II.3)
par(mfrow=c(1,1))
hist(fb$share)
hist(fb$share,breaks = 200,xlab = "# of share", ylab = "# of post",main = "SHARE")
hist(fb$share,breaks = 200,xlab = "# of share", ylab = "# of post",main = "SHARE",xlim = c(1,300))

# 4 Create a box plot to compare shares by each “Type” of post.
# 4 Which type is most effective? Which is least effective? (refer to section II.4)
class(fb$Type)
levels(fb$Type)
x=fb$Type
y=fb$share
plot(x,y,xlab="type",ylab="share",main="Type V.S Share")
plot(x,y,xlab="type",ylab="share",main="Type V.S Share",ylim=c(1,200))
plot(x,y,xlab="type",ylab="share",main="Type V.S Share",ylim=c(1,100))

# 5 Create a box plot to compare shares by each “Category” of post.
# 5 How do you interpret this? 
class(fb$share)
class(fb$Category)
levels(fb$Category)
x=fb$Category
y=fb$share
plot(x,y,xlab="category",ylab="share",main="Category V.S Share")
plot(x,y,xlab="category",ylab="share",main="Category V.S Share",ylim=c(0,150))

# 6 Create a separate box plot comparing shares by 
# 6 “Post.Month”, “Post.Weekday”, and “Post.Hour”.
# 6 Summarize a few key points from this.

plot(fb$Post.Month,fb$share,xlab="Post by Month",ylab="Share",main="Post by Month V.S Share",ylim=c(0,150))
plot(fb$Post.Weekday,fb$share,xlab="Post by Weekday",ylab="Share",main="Post by Weekday V.S Share",ylim=c(0,150))
plot(fb$Post.Hour,fb$share,xlab="Post by Hour",ylab="Share",main="Post by Hour V.S Share",ylim=c(0,150),las=2)
plot(fb$Post.Hour,fb$share,xlab="Post by Hour",ylab="Share",main="Post by Hour V.S Share",ylim=c(0,50),las=2)

#str(fb$Post.Hour)
# fb$Post.Hour %in% c(1:14)
# plot(fb$Post.Hour[fb$Post.Hour %in% c(1:14)],fb$share[fb$Post.Hour %in% c(1:14)],xlab="Post by Hour",ylab="Share",main="Post by Hour V.S Share",ylim=c(0,150),las=2)

# 7 Create a 1x2 panel plot with one being a boxplot
# 7 comparing shares by “Category” for non-paid posts, and the other for paid posts.
# 7 Is there any noticeable difference? (IV.2)
par(mfrow=c(1,2))
str(fb$Paid)
summary(fb$share)
#paid
x=fb$Category[fb$Paid==1]
y=fb$share[fb$Paid==1]
plot(x,y,xlab="category",ylab="share",main="Paid",ylim=c(1,150))
#non-paid
x=fb$Category[fb$Paid==0]
y=fb$share[fb$Paid==0]
plot(x,y,xlab="category",ylab="share",main="Non-Paid",ylim=c(1,150))

# 8 Compare the mean shares for non-paid and paid posts using the aggregate function.
# 8 Is the result in consistent with the boxplots shown before? (refer to section IV.1)
?aggregate()
aggregate(share~Paid,data=fb,FUN=mean)

# 9 Come up with your own interesting insight from the data.
# 9 Support it your claim with any appropriate statistics and/or visualizations.
# paid V.S. non-paid (total interactions / Type)
par(mfrow=c(1,2))
str(fb$Paid)
summary(fb$share)
#paid
x=fb$Type[fb$Paid==1]
y=fb$Total.Interactions[fb$Paid==1]
plot(x,y,xlab="Type",ylab="Total.Interactions",main="Paid")#,ylim=c(1,150))
#non-paid
x=fb$Type[fb$Paid==0]
y=fb$Total.Interactions[fb$Paid==0]
plot(x,y,xlab="Type",ylab="Total.Interactions",main="Non-Paid")#,ylim=c(1,150))

par(mfrow=c(1,2))
str(fb$Paid)
summary(fb$share)
#paid
x=fb$Type[fb$Paid==1]
y=fb$Total.Interactions[fb$Paid==1]
plot(x,y,xlab="Type",ylab="Total.Interactions",main="Paid",ylim=c(1,600))
#non-paid
x=fb$Type[fb$Paid==0]
y=fb$Total.Interactions[fb$Paid==0]
plot(x,y,xlab="Type",ylab="Total.Interactions",main="Non-Paid",ylim=c(1,600))