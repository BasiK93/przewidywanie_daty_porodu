# Pakiety
library("data.table")
library("ggplot2")

# Zbiór danych

babies<-read.table("./babies23.txt", header = TRUE)

####### Analiza danych

table(babies$pluralty)
table(babies$outcome)

#Zmienna real_date

sort(unique(babies$date))
babies$real_date<-as.Date("1961-01-01")+(babies$date-1096)

min(babies$real_date)
max(babies$real_date)

#Histogram w plotly
library(plotly)
a<-as.numeric(as.Date("1961-09-12", format="%Y-%m-%d"))* 24 * 60 * 60 * 1000
b<-as.numeric(as.Date("1962-09-11", format="%Y-%m-%d"))* 24 * 60 * 60 * 1000
p<-plot_ly(data=babies, x=~real_date, type="histogram", nbinsx=365)%>%
  layout( title = "Data porodu", xaxis = list(title = "Data", range=c(a,b)))

p

as.data.frame(table(format(babies$real_date, "%Y-%m")))

#Gestation

summary(babies$gestation)

ggplot(babies, aes(gestation))+
  geom_histogram(fill="lightblue",bins=50)+
  ggtitle("Dzieñ porodu")+
  xlab("Dzieñ")+
  theme_minimal()

zbior1<-babies[babies$gestation<=200,]

ggplot(zbior1, aes(gestation))+
  geom_histogram(fill="lightblue", bins=2)+
  ggtitle("Poród przed 30. tygodniem ci¹¿y")+
  xlab("Dzieñ")+
  theme_minimal()

zbior2<-babies[babies$gestation>294 & babies$gestation!=999,]

ggplot(zbior2, aes(gestation))+
  geom_histogram(fill="lightblue", bins=30)+
  ggtitle("Ci¹¿e zakoñczone po terminie")+
  xlab("Dzieñ")+
  theme_minimal()

zbior3<-babies[babies$gestation>200 & babies$gestation<=294,]

ggplot(zbior3, aes(gestation))+
  geom_histogram(fill="lightblue", bins=30)+
  ggtitle("Ci¹¿e zakoñczone w terminie")+
  xlab("Dzieñ")+
  theme_minimal()

print(paste("Liczba kobiet, które urodzi³y w terminie", length(babies$gestation[babies$gestation>=266 & babies$gestation<=294])))

#p³eæ
table(babies$sex)

#waga noworodka
babies$wt_kg<-babies$wt*0.028

ggplot(babies, aes(x=wt_kg)) + 
  geom_histogram(color="lightpink2", fill="lightpink1", bins=20)+
  ggtitle("Waga noworodka")+
  xlab("kg")+
  theme_minimal()

##wczeœniaki
as.data.frame(table(babies$wt_kg[babies$gestation<=200]))

#Nowy zbiór danych

babies1<-babies[babies$gestation>=210 & babies$gestation!=999,]
babies1<-subset(babies1, select=-c(outcome, pluralty, sex))
babies1<-babies1[babies1$race!=10 & babies1$race!=99,]

#Parity

table(babies1$parity)

#Rasa

table(babies1$race)

d<-seq(1,length(babies1$id))
race2<-c()
for(i in d){
  race2[i]<-if(babies1$race[i]<=5){1} else if(babies1$race[i]==6)
  {2} else if(babies1$race[i]==7)
  {3} else if(babies1$race[i]==8){4} else {5}
}
babies1$race2<-race2

race_name<-c("bia³a", "latynoska", "czarna", "azjatycka", "mieszana")
pie(table(babies1$race2), labels = race_name, col = c("antiquewhite1", "burlywood3", "chocolate4", "lightgoldenrod1", "bisque2"), main="Rasa kobiet")

# Wiek

ggplot(babies1, aes(x=age)) + 
  geom_histogram(color="steelblue3", fill="steelblue2", bins=20)+
  ggtitle("Wiek kobiet")+
  xlab("lata")+
  theme_minimal()

babies1<-babies1[babies1$age!=99,]

#wykszta³cenie

pie(table(babies1$ed), labels = c("<8 klas", "8-12 klas", "12 klas", "12 klas+zawodówka", "college",
                                  "16 klas","zawodówka"), main="Wykszta³cenie kobiet")


ed_agg<-aggregate(babies1, by=list(babies1$race2), FUN=median)

#Popatrzmy na grupê czarnoskórych mam

black_mom<-babies1[babies1$race2==3,]
black_mom<-black_mom[black_mom$ed!=9,]

#WeŸmy mamy w wieku 25-35 lat

black_mom_35<-black_mom[black_mom$age>=25 & black_mom$age<=35,]
table(black_mom_35$ed)

babies1$ed <- replace(babies1$ed, babies1$ed==9, 2)

#Wzrost

babies1$ht_cm<-babies1$ht*2.54

#Zast¹pimy nieznany wzrost wartoœci¹ œredni¹ u poszczególnych ras kobiet

wzrost<-function(x){mean(babies1$ht_cm[babies1$ht_cm!=max(babies1$ht_cm) & babies1$race2==x])}
for (i in 1:4){
  babies1$ht_cm[babies1$ht_cm==max(babies1$ht_cm) & babies1$race2==i] <- wzrost(i)
}

## Waga

#Zamiana jednostki wagi z pounds na kg

babies1$wt.1_kg<-0.454*babies1$wt.1

#Zast¹pimy wagê wartoœci¹ œredni¹ dla poszczególnych ras

waga<-function(x){mean(babies1$wt.1_kg[babies1$wt.1_kg!=max(babies1$wt.1_kg) & babies1$race2==x])}
for (i in 1:4){
  babies1$wt.1_kg[babies1$wt.1_kg==max(babies1$wt.1_kg) & babies1$race2==i] <- waga(i)
}

ggplot(babies1, aes(x=wt.1_kg)) + 
  geom_histogram(color="darkgreen", fill="lightgreen")+
  ggtitle("Waga kobiet przed ci¹¿¹ po korekcie")+
  xlab("kg") +
  theme_minimal()

#Rasa ojca

d<-seq(1,length(babies1$id))
drace2<-c()
for(i in d){
  drace2[i]<-if(babies1$drace[i]<=5){1} else if(babies1$drace[i]==6)
  {2} else if(babies1$drace[i]==7)
  {3} else if(babies1$drace[i]==8){4} else {5}
}
babies1$drace2<-drace2

#Wiek ojca

ggplot(babies1, aes(x=dage)) + 
  geom_histogram(color="royalblue", fill="royalblue1", bins=20)+
  ggtitle("Wiek ojca")+
  xlab("lata")+
  theme_minimal()

mean(babies1$dage[babies1$dage!=99])

paste("Œrednia ró¿nica wieku wynosi",mean(zbior3$dage-zbior3$age))

babies1$dage[babies1$dage==99]<-babies1$age[babies1$dage==99]+3

ggplot(babies1, aes(x=dage)) + 
  geom_histogram(color="royalblue", fill="royalblue1", bins=20)+
  ggtitle("Wiek ojca po korekcie")+
  xlab("lata")+
  theme_minimal()

#Wykszta³cenie

pie(table(babies1$ded), labels = c("<8 klas", "8-12 klas", "12 klas", "12 klas+zawodówka", "college",
                                   "16 klas", "zawodówka", "nieznane"), main="Wykszta³cenie mê¿czyzn")

white_dad<-babies1[babies1$drace2==1 & babies1$ded!=9 & babies1$ded!=6 & (babies1$dage>30 & babies1$dage<=50),]
white_dad$age_group<-ifelse(white_dad$dage<=40,1,2)
white_dad$age_group<-as.factor(white_dad$age_group)
ggplot(white_dad, aes(x=ded, group=age_group, fill=age_group)) + 
  geom_histogram(position="dodge", binwidth=0.5)+
  scale_fill_discrete(name = "Grupa wiekowa", labels = c("30-40", "40-50"))+
  ggtitle("Rozk³ad zmiennej edukacja u mê¿czyzn rasy bia³ej w wieku 30-50 lat")+
  xlab("")+
  scale_x_continuous(breaks=c(0,1,2,3, 4, 5, 7),
                     labels=c("<8 klas", "8-12 klas", "12 klas", "12 klas+zawodówka", "college",
                              "16 klas", "zawodówka"))+
  theme(axis.text.x=element_text(angle=45, hjust = 1))

babies1$ded[(babies1$ded==9 | babies1$ded==6) & babies1$dage<=40 & babies1$drace2==1]<-5
babies1$ded[(babies1$ded==9 | babies1$ded==6) & (babies1$dage>40 & babies1$dage<=55) &babies1$drace2==1]<-2

black_dad<-babies1[babies1$drace2==3 & babies1$ded!=9 & (babies1$dage>30 & babies1$dage<=50),]
black_dad$age_group<-ifelse(black_dad$dage<=40,1,2)
black_dad$age_group<-as.factor(black_dad$age_group)
ggplot(black_dad, aes(x=ded, group=age_group, fill=age_group)) + 
  geom_histogram(position="dodge", binwidth=0.5)+
  scale_fill_brewer(name = "Grupa wiekowa", labels = c("30-40", "40-50"), palette = "Spectral")+
  ggtitle("Rozk³ad zmiennej edukacja u mê¿czyzn rasy czarnej w wieku 30-50 lat")+
  xlab("")+
  scale_x_continuous(breaks=c(0,1,2,3, 4, 5, 7),
                     labels=c("<8 klas", "8-12 klas", "12 klas", "12 klas+zawodówka", "college",
                              "16 klas", "zawodówka"))+
  theme(axis.text.x=element_text(angle=45, hjust = 1))

babies1$ded[babies1$ded==9 & babies1$dage<=40 & babies1$drace2==3]<-2
babies1$ded[babies1$ded==9 & (babies1$dage>40 & babies1$dage<=50) &babies1$drace2==3]<-0

babies1$ded[babies1$drace2==2 & babies1$dage>=40 &babies1$dage<=43]

babies1$ded[babies1$ded==9 & babies1$drace2==2]<-0

#Wzrost

babies1$dht_cm<-babies1$dht*2.54
ggplot(babies1, aes(x=dht_cm)) + 
  geom_histogram(color="gray70", fill="gray70", bins=20)+
  ggtitle("Wzrost mê¿czyzn")+
  xlab("cm")+
  theme_minimal()

#Waga

ggplot(babies1, aes(x=dwt)) + 
  geom_histogram(color="gray63", fill="gray63", bins=20)+
  ggtitle("Waga mê¿czyzn")+
  xlab("pounds")+
  theme_minimal()

babies1<-subset(babies1, select = -c(dht_cm, dht, dwt))

#Stan cywilny

ggplot(babies1, aes(x=marital)) + 
  geom_histogram(color="goldenrod2", fill="gold2", bins=20)+
  ggtitle("Stan cywilny")+
  xlab("")+
  scale_x_continuous(breaks=c(0,1,2,3, 4, 5),
                     labels=c("brak danych", "ma³¿eñstwo", "separacja", "rozwód", "wdowa",
                              "bez œlubu"))+
  theme_minimal()

babies1$marital <- replace(babies1$marital, babies1$marital==0, 1)

#Dochody

ggplot(babies1, aes(x=inc)) + 
  geom_histogram(color="tan3", fill="tan2", binwidth = 0.5)+
  ggtitle("Ca³kowity dochód")+
  xlab("")+
  theme_minimal()

a<-seq(1, length(babies1$id))
age_group<-c()

#Wyznaczymy teraz 5 grup wiekowych

for (i in a){
  age_group[i]<-if(babies1$age[i]<=20){1} else if(babies1$age[i]>20 & babies1$age[i]<26){2} 
  else if(babies1$age[i]>=26 & babies1$age[i]<31){3} else if(babies1$age[i]>=31 & babies1$age[i]<36){4} else if(babies1$age[i]>=36 & babies1$age[i]<41){5} else {6}
}
babies1$age_group<-age_group

##Brak danych w zmiennej inc uzupe³nimy wartoœci¹ inc, która najczêœciej wystêpuje u danej rasy w wybranej grupie wiekowej

a<-seq(1, length(babies1$id))
is.not.null <- function(x) !is.null(x)

for(i in a){
  x<-babies1$race2[i]
  y<-babies1$age_group[i]
  z<-names(sort(summary(as.factor(babies1$inc[babies1$inc!=98 & babies1$race2==x & babies1$age_group==y])), decreasing = TRUE)[1])
  z1<-names(sort(summary(as.factor(babies1$inc[babies1$inc!=98 & babies1$race2==x])), decreasing = TRUE)[1])
  
  if(babies1$inc[i]!=98){babies1$inc[i]} else if(is.not.null(z)){babies1$inc[i]<-z}else{ babies1$inc[i]<-z1}
}
babies1$inc<-as.numeric(babies1$inc)

#Palenie papierosów

smoke_name<-c("nigdy", "pali", "przed ci¹¿¹", "kiedyœ", "brak danych")
pie(table(babies1$smoke), labels = smoke_name, col=c("white", "dimgray", "darkgrey", "gainsboro", "gray92"), main="Czy kobieta pali³a papierosy?")

#Braki danych dotycz¹ce palenia papierosów zast¹pimy wartoœci¹, która wystêpuje najczêœciej u danej rasy

b<-seq(1, length(babies1$id))
for(i in b){
  x<-babies1$race2[i]
  z<-names(sort(summary(as.factor(babies1$smoke[babies1$smoke!=9 & babies1$race2==x])), decreasing = TRUE)[1])
  if(babies1$smoke[i]!=9){babies1$smoke[i]}else{ babies1$smoke[i]<-z}
}

babies1$smoke<-as.factor(babies1$smoke)

#Kiedy rzuci³a palenie

ggplot(babies1, aes(time))+
  geom_histogram(binwidth = 0.5)+
  ggtitle("Kiedy rzuci³a palenie")+
  xlab("")+
  scale_x_continuous(breaks=c(0,1,2,3, 4, 5, 6, 7, 8, 9),
                     labels=c("nie dotyczy", "pali nadal", "podczas ci¹¿y", "rok temu", "2 lata temu",
                              "3 lata temu", "4 lata temu", "5-9 lat temu", "ponad 10 lat temu",       "nie wiadomo kiedy rzuci³a palenie"))+
  theme(axis.text.x=element_text(angle=90, hjust = 1, vjust=0.3))

for(i in a){
  if(babies1$time[i]==98 | babies1$time[i]==99){babies1$time[i]<-babies1$smoke[i]}
}

#Ile pali³a dziennie

for(i in b){
  x<-babies1$time[i]
  z<-names(sort(summary(as.factor(babies1$number[babies1$number!=98 & babies1$time==x])), decreasing = TRUE)[1])
  if(babies1$number[i]!=98 & babies1$number[i]!=10 & babies1$number[i]!=11){babies1$number[i]}else{ babies1$number[i]<-z}
}

babies1$number<-as.numeric(babies1$number)

ggplot(babies1, aes(number))+
  geom_histogram(binwidth = 0.5)+
  ggtitle("Liczba palonych papierosów dziennie")+
  xlab("")+
  scale_x_continuous(breaks=c(0,1,2,3, 4, 5, 6, 7, 8, 9),
                     labels=c("nie dotyczy", "1-4", "5-9", "10-14", "15-19", "20-29", "30-39", "40-60", "60+",       "pali, ale nie wiadomo ile"))+
  theme(axis.text.x=element_text(angle=90, hjust = 1, vjust=0.3))

#Badanie korelacji miêdzy zmiennymi

library(corrplot)
my_data<-babies2[, c(1,2,3,4,5,6,7,8,9,10,11,13,14,15,16,17)]
korelacja<-cor(my_data)
corrplot(korelacja)

###Zbiór do budowy modelu

babies2<-subset(babies1, select=-c(id, date, wt, race, ht, wt.1, drace))
babies2$smoke<-as.numeric(babies2$smoke)

babies2<-subset(babies1, select=-c(id, date, wt, race, ht, wt.1, drace))
babies2$smoke<-as.numeric(babies2$smoke)


#Zmienna do klasyfikatora

for(i in 1:1037){
  babies2$born[i]<-if(babies2$gestation[i]<278){-1}else if(babies2$gestation[i]>=278 & babies2$gestation[i]<=282){0}else{1}
}

write.csv(babies2, "./babiesdata_model2.csv")