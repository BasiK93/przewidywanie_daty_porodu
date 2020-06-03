library(VGAM)
model3<-readRDS("final_model.rds") 

shinyServer(function(input, output) {
    
    output$Prediction<-renderPlot({pred_data2(input$parity, input$age, input$ht_cm, input$race2, input$ed, input$wt.1_kg, input$wt_kg, input$dage, input$drace2, input$ded, input$marital, input$inc,
                                              input$smoke, input$time, input$number)})
    output$Wynik<-renderText({pred_data3(input$parity, input$age, input$ht_cm, input$race2, input$ed, input$wt.1_kg, input$wt_kg, input$dage, input$drace2, input$ded, input$marital, input$inc,
                                         input$smoke, input$time, input$number)})
    output$Data<-renderText({as.character(data_por(input$date1, input$parity, input$age, input$ht_cm, input$race2, input$ed, input$wt.1_kg, input$wt_kg, input$dage, input$drace2, input$ded, input$marital, input$inc,
                                                   input$smoke, input$time, input$number))})})


pred_data2<-function(parity, age, ht_cm, race2, ed, wt.1_kg, wt_kg, dage, drace2, ded, marital, inc, smoke, time, number){
    nazwa<-c("parity", "age", "ed", "dage", "ded", "marital", "inc", "smoke", "time", "number", "wt_kg", "race2", "ht_cm", "wt.1_kg", "drace2", "age_group")
    age_group<-if(age<=20){1} else if(age>20 & age<26){2} else if(age>=26 & age<31){3} else if(age>=31 & age<36){4} else if(age>=36 & age<41){5} else {6}
    input1<-c(parity, age, ed, dage, ded, marital, inc, smoke, time, number, wt_kg, race2, ht_cm, wt.1_kg, drace2, age_group)
    input2<-as.integer(input1)
    dane<-as.data.frame(t(input2))
    names(dane)<-nazwa
    prob_app<-predict(model3, dane, type="response")
    a<-barplot((prob_app[1,]*100), names.arg = c("przed terminem", "w terminie", "po terminie"), col="lightgreen", ylim = c(0, max(prob_app)*100+10), ylab="%")
    wynik2<-prob_app[1,]
    return(a)
}
pred_data3<-function(parity, age, ht_cm, race2, ed, wt.1_kg, wt_kg, dage, drace2, ded, marital, inc, smoke, time, number){
    nazwa<-c("parity", "age", "ed", "dage", "ded", "marital", "inc", "smoke", "time", "number", "wt_kg", "race2", "ht_cm", "wt.1_kg", "drace2", "age_group")
    age_group<-if(age<=20){1} else if(age>20 & age<26){2} else if(age>=26 & age<31){3} else if(age>=31 & age<36){4} else if(age>=36 & age<41){5} else {6}
    input1<-c(parity, age, ed, dage, ded, marital, inc, smoke, time, number, wt_kg, race2, ht_cm, wt.1_kg, drace2, age_group)
    input2<-as.integer(input1)
    dane<-as.data.frame(t(input2))
    names(dane)<-nazwa
    prob_app<-predict(model3, dane, type="response")
    predictions_app <- apply(prob_app, 1, which.max)
    predictions_app<-predictions_app-1
    wynik<-tail(predictions_app, n=1)
    oswiadczenie<-if(wynik==0){"przed terminem"}else{
        if(wynik==1){"w terminie"}else{
            "po terminie"}
    }
    return(oswiadczenie)
}
data_por<-function(date1, parity, age, ht_cm, race2, ed, wt.1_kg, wt_kg, dage, drace2, ded, marital, inc, smoke, time, number){
    nazwa<-c("parity", "age", "ed", "dage", "ded", "marital", "inc", "smoke", "time", "number", "wt_kg", "race2", "ht_cm", "wt.1_kg", "drace2", "age_group")
    age_group<-if(age<=20){1} else if(age>20 & age<26){2} else if(age>=26 & age<31){3} else if(age>=31 & age<36){4} else if(age>=36 & age<41){5} else {6}
    input1<-c(parity, age, ed, dage, ded, marital, inc, smoke, time, number, wt_kg, race2, ht_cm, wt.1_kg, drace2, age_group)
    input2<-as.integer(input1)
    dane<-as.data.frame(t(input2))
    names(dane)<-nazwa
    prob_app<-predict(model3, dane, type="response")
    predictions_app <- apply(prob_app, 1, which.max)
    predictions_app<-predictions_app-1
    wynik<-tail(predictions_app, n=1)
    oswiadczenie<-if(wynik==0){"przed terminem"}else{
        if(wynik==1){"w terminie"}else{
            "po terminie"}
    }
    data1=as.character(date1)
    data2<-if(wynik==0){as.Date(data1)-10}else{if(wynik==1){as.Date(data1)-2}else{as.Date(data1)+3}}
    data3<-if(wynik==0){as.Date(data1)-3}else{if(wynik==1){as.Date(data1)+2}else{as.Date(data1)+10}}
    abc<-paste(data2, "-", data3, sep=" ")
    return(abc)
}
