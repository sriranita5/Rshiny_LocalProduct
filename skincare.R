# Set Directory
setwd('/Users/erteee/Downloads/SEMESTER 3/Eksplorasi dan Visdas/finalproject')

# Import Dataset
dataset = read.csv('brand.csv')
skincare <- filter(dataset, category == "skin_care")
head(skincare)

# Data Preprocessing
# --Outlier-- 
boxplot(skincare$subscriber)
boxplot(skincare$reviewer)
boxplot(skincare$RATING)

# --Missing Value--
library(mice)
md.pattern(skincare, rotate.names=TRUE)

# --Transforming Data--
## GAKPERLU ###

#-- Dimensionality Reduction-- 
## GAKPERLU ##

# Data Exploration

#OVERALL
# -- Pie Chart -- 
kategori = skincare$category
kategori= as.factor(skincare$category)
table(kategori)
freqkategori=table(kategori)
pie(freqkategori, main = "Frekuensi Setiap Kategori")

# -- Histogram --
hist(skincare$subscriber ,main = "Persebaran Data pada Jumlah Subscriber")

# -- Bar Chart --
range(skincare$subscriber)
barchart <- ggplot(skincare,
  aes(x = category, y = subscriber)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Rasio Subscriber", y = "Jumlah Subscriber", x = "Kategori Produk")
barchart

library(dplyr)
library(ggplot2)
library("RColorBrewer")
#KATEGORIK
# --mebuat data frame khusus kategori skincare--
kategorik=skincare[97:180,]
kategorik

# -- Histogram --
hist(kategorik$subscriber, main = "Persebaran Jumlah Subscriber pada Skin Care")
hist(kategorik$reviewer, main = "Persebaran Jumlah Reviewer pada Skin Care")
hist(kategorik$RATING, main = "Persebaran Jumlah Rating pada Skin Care")

# -- Scatter Plot -- 
plot(x=kategorik$subscriber, y=kategorik$RATING ,main = "Subscriber Vs Rating")
plot(x=kategorik$reviewer, y=kategorik$RATING ,main = "Subscriber Vs Reviewer")

# -- Bar Chart --
# TOP 5 
top5 = skincare[c(110,156,162,164,178),]
top5

bar5 <- ggplot(top5,
  aes(x = brand, y = subscriber)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "TOP5 BRAND SKINCARE", y = "Jumlah Subscriber", x = "Nama Brand")
bar5



#KATEGORI----------------------------

#HISTOGRAM with ggplot ----------------------------
##subscriber
ggplot(skincare, aes(x= subscriber))+
  geom_histogram(bins=50)+
  labs(title = "Subscriber Body Care: bin 50", x= "Subscriber")
##reviewer
ggplot(skincare, aes(x= reviewer))+
  geom_histogram(bins=50)+
  labs(title = "Reviewer Body Care: bin 50", x= "Reviewer")
##rating, error
ggplot(skincare, aes(x= RATING))+
  geom_histogram(bins=50)+
  labs(title = "Rating Body Care: bin 50", x= "Rating")

#HISTOGRAM with hist()--------------------------------------
##subscriber
hist(skincare$subscriber,
     main = "Subscriber Body Care",
     xlab = "Subscriber")
##Reviewer
hist(skincare$reviewer,
     main = "Reviewer Body Care",
     xlab = "Reviewer")
##Rating
hist(skincare$RATING,
     main = "Rating Body Care",
     xlab = "Rating")

#SCATTERPLOT-------------------------------------------
##rating vs subcriber
ggplot(skincare, mapping= aes(x=subscriber, y=RATING))+
  geom_point()+
  geom_smooth(formula = y ~ x, method = "lm")
##rating vs reviewer
ggplot(skincare, mapping= aes(x=reviewer, y=RATING))+
  geom_point()+
  geom_smooth(formula = y ~ x, method = "lm")
##subscriber vs rating
ggplot(skincare, mapping= aes(x=subscriber, y=reviewer))+
  geom_point()+
  geom_smooth(formula = y ~ x, method = "lm")

#BARCHART--------------------------------------------------
##decrease dataset bodycare
new = skincare[order(skincare$subscriber, decreasing = TRUE),]  
##get top5 subscriber
top5 = slice_head(new, n = 5)

##barchart with ggplot
ggplot(top5, mapping=aes(brand,subscriber))+
  geom_col(width = .5)+
  labs(x="Brand", y="Frequency", title="TOP 5 Brand by Subscriber")
##barchart with barplot()
barplot(top5$subscriber,
        main="TOP 5 Brand by Subscriber",
        xlab="Brand",
        ylab="Frequency",
        names.arg = c("Scarlett Whitening","YOU Beauty","Emina","Somethinc","Skintific"),
        col = brewer.pal(n = 5, name = "Accent"),
        horiz = F,
        las = 2
)