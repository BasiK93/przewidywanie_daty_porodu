#pakiety
library("VGAM")

#pobieranie zbioru

babies<-read.table("./babiesdata_model2.csv", sep=",", header=TRUE)

#tworzenie zbioru treningowego i testowego

babies$born<-babies$born+1
babies<-subset(babies, select=-c(X, gestation, real_date))
train<-babies[1:600, ]
test<-babies[601:1037, ]


#nadawanie wag

for( i in 1:600){
  train$wagi[i]<-if(train$born[i]==2){1}else if(train$born[i]==0){1.1}else{1.8}
}

wagi2<-train$wagi
train<-subset(train, select=-c(wagi))
test2<-subset(test, select=-c(born))
y<-test$born

# regresja logistyczna

model2<-vglm(born ~.,family=multinomial,data=train, weights = wagi2)
summary(model2)
probabilities <- predict(model2, test[,1:16], type="response")
predictions <- apply(probabilities, 1, which.max)
predictions<-predictions-1

#precision, recall

cm = as.matrix(table(Actual = y, Predicted = predictions))
cm
n = sum(cm) 
nc = nrow(cm) 
diag = diag(cm)  
rowsums = apply(cm, 1, sum) 
colsums = apply(cm, 2, sum) 
p = rowsums / n 
q = colsums / n 

precision = diag / colsums 
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall) 

data.frame(precision, recall, f1) 

# save model
saveRDS(model2, "./final_model.rds")
