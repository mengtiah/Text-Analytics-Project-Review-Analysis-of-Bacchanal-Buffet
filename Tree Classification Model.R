df=read.csv("reviews_dataset.csv")
library(dplyr)
df=df%>%
  select(-X,-buffett,-buffets,-disappoint,-disappointed,-disappointing,-disappointment,-delicious,-poor,-worst,-great,-love,-even,-go,-well,-hotel,-terrible,-reviews,-excited,-stars,-found,-think,-eat,-amazing,-went,-wynn,-yeah,-amazing,-wow,-visit,
         -horrible,-favorite,-favorites,-always,-told,-paid,-charge,-everything,-never,-every,-us,-one,-first,
         -tax,-pay,-paying,-middle,-seems,-spend,-either,-seated,-sitting,-could,-though,-thought,-skip,-okay,-friend,-etc,-excellent,-trying,-ended,-many,-people,-sure,-money,-nice,-big,-rather,-table,-got,-text,-place,-going,-better,-new,-get,-nothing,-bad,-use,-fan,-fantastic,try,
         -foods,-space,-came,-asked,-different,-awesome,-minutes,-wonderful,-long,-eating,-way,-however,-front,-lady,-average,-ever,-try,-express,-experience,-around,-another,-took,-looked,-day,-days,-dinner,-things,-order,-section,-sections,-pretty,-lot,-bit,-high,-recommend,-recommended,-plates,
         -attention,-attentive,-ticket,-time,-see,-anymore,-rated,-much,-legs,-quality,-overall,-stand,-day,-days,-little,-would,-far,-point,-person,-yelp,-taste,-half,-king,-customer,-customers,-probably,-two,-need,-lunch,-actually,-ok,-know,-made,-thing,-snow,-make,-check,-enjoyed,-flavor,-restaurant,-restaurants,
         -family,-tip,-something,-weird,-times,-finally,-left,-frozen,-number,-husband,-decided,-offered,-tasted,-station,-else.,-sick,-star,-usually,-feel,-bar,-especially,-hour,-hours,-last,-feeling,-items,-sick,-usual,-start,-started,-hours,-system,-elsewhere,-tasted,-fine,-right,-standing,-sum,-thank,-remember,-still,-mostly,-give,-say,-dishes,-basically,
         -unlimited,-years,-waited,-understand,-clear,-call,-night,-stomach,-set,-super,-set,-stations,-felt,-wanted,-enough,-none,-beautiful,-quite,-next.,-truly,-maybe)

library(caTools)
split = sample.split(df$class, SplitRatio = 0.7)
df$split = split
train = subset(df, split==TRUE)
test = subset(df, split==FALSE)
colnames(test)

table(train$class)

4882 /nrow(train) #81% good review


library(rpart)
library(rpart.plot)
cartModel = rpart(class ~., data=train, method="class")
prp(cartModel)

library(caret)
numFolds=trainControl(method = "cv", number = 10)
cpGrid = expand.grid(.cp=seq(0.001, 0.01, 0.001))
train(class ~ ., data = train, method = "rpart", trControl = numFolds, tuneGrid = cpGrid)


cartModelImproved = rpart(class ~ ., data=train, method="class", cp= 0.001)
prp(cartModelImproved,legend.cex=5,split.cex=4,under.cex=1,branch.lwd
=2)

#predict using the model
predictCARTImproved = predict(cartModelImproved, newdata=test, type="class")
table(test$class, predictCARTImproved)

#Accuracy
(145+1967)/nrow(test)
#AUC
library(AUC)
rocobj1 <- roc(test$class, predictCARTImproved)
auc(rocobj1)
