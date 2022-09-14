# correlation greph
library(corrplot)

dat2 <- dat %>% select(- c(BrandCategory, BrandName, Followers, 
                           Likes, Comments, Emotion, ImageCategory))

corrplot(cor(dat2), "color", diag = F, col = COL2("PiYG"), type = "lower",
         tl.col = 'black', cl.ratio = 0.2, tl.srt = 50, tl.cex = 0.8)

# response and some other variables
library(ggplot2)

ggplot(dat, aes(x = Followers, y = ER)) + 
  geom_point(shape = 1, size = 2) + #ylim(c(0, 20)) +
  theme_classic()  + geom_rug(length = unit(0.01, "npc"))

library(lattice)

length(dat$ER[dat$ER > 20])
hist(dat$ER, nclass = 75, col = "#999999", border = "#000000", 
     xlab = "max = 48.5", ylab = "", xlim = c(0, 20), main = "Engagement Rate")

par(mfrow = c(1, 2))

boxplot(dat$ER, col = "#999999", main = "Engagement Rate")
boxplot(dat$ER, col = "#999999", outline = F)

par(mfrow = c(1, 1))

length(dat$Followers[dat$Followers > 6e+04])
hist(dat$Followers, nclass = 75, col = "#999999", border = "#000000", 
     xlim = c(0, 6e+04), xlab = "max = 100'000", ylab = "", main = "Followers")

length(dat$Likes[dat$Likes > 800])
hist(dat$Likes, nclass = 1000, col = "#999999", border = "#000000", 
     xlim = c(0, 800), xlab = "max = 13277", ylab = "", main = "Likes")

length(dat$Comments[dat$Comments > 30])
hist(dat$Comments, nclass = 300, col = "#999999", border = "#000000", 
     xlim = c(0, 30), xlab = "max = 299", ylab = "", main = "Commenti")

# the values of Followers are greater those of Comments and Likes and that explains the low values of ER

length(dat$HashtagCount[dat$HashtagCount > 30])
hist(dat$HashtagCount, nclass = 80, col = "#999999", border = "#000000", 
     xlim = c(0, 30), xlab = "max = 59", ylab = "", main = "Hashtag")

barplot(table(dat$BrandCategory), col = "#999999")
# Mega couture is way less present that the other levels
boxplot(dat$ER ~ dat$BrandCategory, col = "#999999", 
        ylim = c(0, 20), ylab = "Engagement Rate", xlab = "Brand Category")

barplot(table(dat$NumberOfPeople), col = "#999999")
# also number of people have some values way more present then others
boxplot(dat$ER ~ dat$NumberOfPeople, ylim = c(0, 20), col = "#999999",
        ylab = "Engagement Rate", xlab = "Number of People")

barplot(table(dat$NumberOfFashionProduct), col = "#999999")
boxplot(dat$ER ~ dat$NumberOfFashionProduct, col = "#999999", 
        ylim = c(0, 20), ylab = "Engagement Rate", xlab = "Number of Fashion Product")

barplot(table(dat$ImageCategory), col = "#999999")
boxplot(dat$ER ~ dat$ImageCategory, col = "#999999", ylim = c(0, 20), ylab = "Engagement Rate", xlab = "Logo")

ggplot(dat, aes(x = BrandCategory, y = ER, fill = ImageCategory)) + 
  geom_boxplot(outlier.colour = "grey", outlier.shape = 1,
               outlier.size = 1, position = position_dodge(0.8)) + 
  coord_cartesian(ylim = c(0, 15)) +
  theme_classic() +
  stat_summary(fun = mean, geom = "point", shape = 16, size = 3, position = position_dodge(0.8)) +
  scale_fill_manual(values = c("orange", "violet", "purple", "pink") ) +
  xlab(label = NULL) + ylab(label = "ER") + labs(fill = NULL)
# marketing photos seems to not be the best choice except for the mega couture case

ggplot(dat, aes(x = Emotion, y = ER, fill = BrandCategory)) + 
  geom_boxplot(outlier.colour = "grey", outlier.shape = 1,
               outlier.size = 1, position = position_dodge(0.8)) + 
  coord_cartesian(ylim = c(0, 15)) +
  theme_classic() +
  stat_summary(fun = mean, geom = "point", shape = 16, size = 3, position = position_dodge(0.8)) +
  scale_fill_manual(values = c("orange", "violet", "purple", "pink") ) +
  xlab(label = NULL) + ylab(label = "ER") + labs(fill = NULL)

ggplot(dat, aes(x = ImageCategory, y = ER, fill = Emotion)) + 
  geom_boxplot(outlier.colour = "grey", outlier.shape = 1,
               outlier.size = 1, position = position_dodge(0.8)) + 
  coord_cartesian(ylim = c(0, 15)) +
  theme_classic() +
  stat_summary(fun = mean, geom = "point", shape = 16, size = 3, position = position_dodge(0.8)) +
  scale_fill_manual(values = c("orange", "violet", "purple", "pink") ) +
  xlab(label = NULL) + ylab(label = "ER") + labs(fill = NULL)
# marketing is again the less appreciated image category

# division of the dataset between brand categories, image categories and emotions
# the dataset has some levels more present than others

library(plotly)

a <- c("D", "H", "M", "S")
b <- c("bd", "mk", "se")
liv0 <- a

comb1 <- expand.grid(a, b)

liv <- rep(NA, nrow(comb1))
for(i in 1:length(liv1)) {
  liv[i] <- paste(comb1[i, 1], comb1[i, 2], sep = " - ")
}

liv1 <- rep(liv, 1)
liv1

c <- c("o", "h", "n")

comb2 <- expand.grid(b, c)

liv <- rep(NA, nrow(comb2))
for(i in 1:length(liv)) {
  liv[i] <- paste(comb2[i, 1], comb2[i, 2], sep = " - ")
}

length(liv)
liv

num <- c("D", "S", "H", "M")

comb3 <- expand.grid(liv, num)

liv2 <- rep(NA, nrow(comb3))
for(i in 1:36) {
  liv2[i] <- paste(comb3[i, 2], comb3[i, 1], sep = " - ")
}

liv2

e <- data.frame(
  ids = c(
    "H", "D", "S", "M",
    liv1, liv2 
  ),
  labels = c(
    "High street", "Designer", "Small couture", "Mega couture", 
    "Bodysnap", "Bodysnap", "Bodysnap", "Bodysnap",
    "Marketing", "Marketing", "Marketing", "Marketing",
    "Selfie", "Selfie", "Selfie", "Selfie", 
    "Other", "Other", "Other", "Happiness", "Happiness", "Happiness", "Neutral", "Neutral", "Neutral", 
    "Other", "Other", "Other", "Happiness", "Happiness", "Happiness", "Neutral", "Neutral", "Neutral", 
    "Other", "Other", "Other", "Happiness", "Happiness", "Happiness", "Neutral", "Neutral", "Neutral", 
    "Other", "Other", "Other", "Happiness", "Happiness", "Happiness", "Neutral", "Neutral", "Neutral"
    
  ),
  parents = c(
    "", "", "", "", 
    "D", "H", "M", "S", 
    "D", "H", "M", "S", 
    "D", "H", "M", "S", 
    "D - bd", "D - mk", "D - se", "D - bd", "D - mk", "D - se", "D - bd", "D - mk", "D - se",
    "S - bd", "S - mk", "S - se", "S - bd", "S - mk", "S - se", "S - bd", "S - mk", "S - se",
    "H - bd", "H - mk", "H - se", "H - bd", "H - mk", "H - se", "H - bd", "H - mk", "H - se",
    "M - bd", "M - mk", "M - se", "M - bd", "M - mk", "M - se", "M - bd", "M - mk", "M - se"
  ),
  values = c(table(dat$BrandCategory)[2], table(dat$BrandCategory)[1], table(dat$BrandCategory)[4], table(dat$BrandCategory)[3],
             
             table(dat$ImageCategory[dat$BrandCategory == "Designer"])[1], table(dat$ImageCategory[dat$BrandCategory == "High street"])[1], table(dat$ImageCategory[dat$BrandCategory == "Mega couture"])[1], table(dat$ImageCategory[dat$BrandCategory == "Small couture"])[1],
             table(dat$ImageCategory[dat$BrandCategory == "Designer"])[2], table(dat$ImageCategory[dat$BrandCategory == "High street"])[2], table(dat$ImageCategory[dat$BrandCategory == "Mega couture"])[2], table(dat$ImageCategory[dat$BrandCategory == "Small couture"])[2],
             table(dat$ImageCategory[dat$BrandCategory == "Designer"])[3], table(dat$ImageCategory[dat$BrandCategory == "High street"])[3], table(dat$ImageCategory[dat$BrandCategory == "Mega couture"])[3], table(dat$ImageCategory[dat$BrandCategory == "Small couture"])[3],
             
             table(dat$Emotion[dat$BrandCategory == "Designer" & dat$ImageCategory == "BodySnap"])[1], table(dat$Emotion[dat$BrandCategory == "Designer" & dat$ImageCategory == "Marketing"])[1], table(dat$Emotion[dat$BrandCategory == "Designer" & dat$ImageCategory == "Selfie"])[1],
             table(dat$Emotion[dat$BrandCategory == "Designer" & dat$ImageCategory == "BodySnap"])[2], table(dat$Emotion[dat$BrandCategory == "Designer" & dat$ImageCategory == "Marketing"])[2], table(dat$Emotion[dat$BrandCategory == "Designer" & dat$ImageCategory == "Selfie"])[2],
             table(dat$Emotion[dat$BrandCategory == "Designer" & dat$ImageCategory == "BodySnap"])[3], table(dat$Emotion[dat$BrandCategory == "Designer" & dat$ImageCategory == "Marketing"])[3], table(dat$Emotion[dat$BrandCategory == "Designer" & dat$ImageCategory == "Selfie"])[3],
             
             table(dat$Emotion[dat$BrandCategory == "Small couture" & dat$ImageCategory == "BodySnap"])[1], table(dat$Emotion[dat$BrandCategory == "Small couture" & dat$ImageCategory == "Marketing"])[1], table(dat$Emotion[dat$BrandCategory == "Small couture" & dat$ImageCategory == "Selfie"])[1],
             table(dat$Emotion[dat$BrandCategory == "Small couture" & dat$ImageCategory == "BodySnap"])[2], table(dat$Emotion[dat$BrandCategory == "Small couture" & dat$ImageCategory == "Marketing"])[2], table(dat$Emotion[dat$BrandCategory == "Small couture" & dat$ImageCategory == "Selfie"])[2],
             table(dat$Emotion[dat$BrandCategory == "Small couture" & dat$ImageCategory == "BodySnap"])[3], table(dat$Emotion[dat$BrandCategory == "Small couture" & dat$ImageCategory == "Marketing"])[3], table(dat$Emotion[dat$BrandCategory == "Small couture" & dat$ImageCategory == "Selfie"])[3],
             
             table(dat$Emotion[dat$BrandCategory == "High street" & dat$ImageCategory  == "BodySnap"])[1], table(dat$Emotion[dat$BrandCategory == "High street" & dat$ImageCategory == "Marketing"])[1], table(dat$Emotion[dat$BrandCategory == "High street" & dat$ImageCategory == "Selfie"])[1],
             table(dat$Emotion[dat$BrandCategory == "High street" & dat$ImageCategory == "BodySnap"])[2], table(dat$Emotion[dat$BrandCategory == "High street" & dat$ImageCategory == "Marketing"])[2], table(dat$Emotion[dat$BrandCategory == "High street" & dat$ImageCategory == "Selfie"])[2],
             table(dat$Emotion[dat$BrandCategory == "High street" & dat$ImageCategory == "BodySnap"])[3], table(dat$Emotion[dat$BrandCategory == "High street" & dat$ImageCategory == "Marketing"])[3], table(dat$Emotion[dat$BrandCategory == "High street" & dat$ImageCategory == "Selfie"])[3],
             
             table(dat$Emotion[dat$BrandCategory == "Mega couture" & dat$ImageCategory == "BodySnap"])[1], table(dat$Emotion[dat$BrandCategory == "Mega couture" & dat$ImageCategory == "Marketing"])[1], table(dat$Emotion[dat$BrandCategory == "Mega couture" & dat$ImageCategory == "Selfie"])[1],
             table(dat$Emotion[dat$BrandCategory == "Mega couture" & dat$ImageCategory == "BodySnap"])[2], table(dat$Emotion[dat$BrandCategory == "Mega couture" & dat$ImageCategory == "Marketing"])[2], table(dat$Emotion[dat$BrandCategory == "Mega couture" & dat$ImageCategory == "Selfie"])[2],
             table(dat$Emotion[dat$BrandCategory == "Mega couture" & dat$ImageCategory == "BodySnap"])[3], table(dat$Emotion[dat$BrandCategory == "Mega couture" & dat$ImageCategory == "Marketing"])[3], table(dat$Emotion[dat$BrandCategory == "Mega couture" & dat$ImageCategory == "Selfie"])[3]
  ),
  
  stringsAsFactors = FALSE
)

plot_ly(e, ids = ~ids, labels = ~labels, parents = ~parents, values = ~values, 
        type = 'sunburst', branchvalues = 'total', insidetextorientation = "radial")
