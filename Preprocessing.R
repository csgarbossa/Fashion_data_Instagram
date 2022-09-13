rm(list=ls())
library(dplyr)

# load data


# selection of the population useful for the analysis based on the number of followers
min_followers <- 500
max_followers <- 1e5
dat <- dat %>% filter(dat$Followers>= min_followers & dat$Followers <= max_followers)

# elimination of posts without a minimum number of interactions
dat <- dat %>% filter(dat$Likes + dat$Comments > 15)

# cleaning the repetitions
dat <- dat %>% distinct()

# filter based on the connection between the post and the fashion
dat <- dat %>% filter(dat$NonFashion <= 0.5) %>% select(-NonFashion)

# elimination of variables not useful like Followings
# or not usable like Caption (the use of more than one alphabet makes it hard to analyze)
dat <- dat %>% select(- c(UserId, Followings, MediaCount, 
                          Caption, ImgURL, Link, CreationTime, Face))

# rounding some variables to make them more understandable 
dat$NumberOfFashionProduct <- round(dat$NumberOfFashionProduct)
dat$NumberOfPeople <- round(dat$NumberOfPeople)

# convertion of some variables in factors
dat$BrandCategory <- as.factor(dat$BrandCategory)
dat$BrandName <- as.factor(dat$BrandName)

# creation of a new variable in order to consider the usage of hashstags
hashcount.var <- function(data) {
  out <- rep(0, nrow(data))
  for (i in 1:nrow(data)) {
    a <- data$Hashtags[i]
    a.split <- strsplit(a, split = ", ", fixed=T)[[1]]
    a.split <- a.split[a.split != ""]
    if(length(a.split) != 0) out[i] <- length(a.split)
  }
  out
}

dat$HashtagCount <- hashcount.var(dat)
dat <- dat %>% select(- Hashtags)

# creation of a qualitative variable that indicates the predominant emotion
emotions <- dat %>% select(Anger, Contempt, Disgust, Fear, 
                           Happiness, Neutral, Sadness, Surprise)
all_emotions <- apply(emotions, 1, sum)  
emotion_detected <- rep(NA, nrow(dat))
for(i in 1:nrow(dat)){
  if(all_emotions[i] == 0) emotion_detected[i] <- "NONE"
  else{
    emotion_detected[i] <- colnames(emotions)[which.max(emotions[i, ])]
  }
}
emotion_detected <- as.factor(emotion_detected)
table(emotion_detected)
levels(emotion_detected) <- c("Other", "Other", "Happiness", "Neutral", "NONE", "Other", "Other")

dat$Emotion <- emotion_detected

# creation of a qualitative variable that indicates the category of the image
category <- dat %>% select(Selfie, BodySnap, Marketing, ProductOnly)
all_category <- apply(category, 1, sum)
ImageCategory <- rep(NA, nrow(dat))
for(i in 1:nrow(dat)) {
  if(all_category[i] == 0) ImageCategory <- "NONE"
  else {
    ImageCategory[i] <- colnames(category)[which.max(category[i, ])]
  }
}
ImageCategory <- as.factor(ImageCategory)
table(ImageCategory)
dat$ImageCategory <- ImageCategory

# selection of those posts that have at least a person 
dat <- dat %>% filter(dat$ImageCategory != "ProductOnly" & 
                           dat$Emotion != "NONE" &
                           dat$NumberOfPeople != 0)
dat <- dat %>% select(- c(ImageCategory, Emotion, ProductOnly))

# creation of the response variable
dat$ER <- ( (dat$Likes + dat$Comments)/dat$Followers ) * 100

# elimination of the outliers
dat <- dat %>% filter(ER <= 200)
