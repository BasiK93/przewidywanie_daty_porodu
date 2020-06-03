library(shiny)
library(shinythemes)

shinyUI(fluidPage(theme = shinytheme("superhero"),
                      pageWithSidebar(
                          headerPanel("Prognozowanie daty porodu"),
                          sidebarPanel(
                              sliderInput("parity", "Liczba wcze\u015Bniejszych porod\u00F3w", value=1, min=0, max=10, step=1),
                              sliderInput("age", "Wiek matki", value = 23, min=15, max=50, step=1),
                              sliderInput("ht_cm", "Wzrost matki", value = 157, min=140, max=190, step=1),
                              selectizeInput("race2", "Rasa matki", choices=(c("bia\u0142a"=1, "latynoska"=2, "czarna"=3, "azjatycka"=4, "mieszana"=5)), selected=1),
                              selectizeInput("ed", "Wykszta\u0142cenie matki", choices = (c("podstawowe"=0, "\u015Brednie"=1, "maturalne"=2, "technikum"=3, "zawod\u00F3wka"=7, "licencjat"=4, "wy\u017Csze"=5)), selected=5),
                              sliderInput("wt.1_kg", "Waga mamy przed ci\u0105\u017C\u0105", value = 65, min=35, max=110, step=1),
                              sliderInput("wt_kg", "Przewidywana waga noworodka", value = 3, min=2, max=5, step=0.1),
                              sliderInput("dage", "Wiek ojca", value = 32, min=20, max=50, step=1),
                              selectizeInput("drace2", "Rasa ojca", choices=(c("bia\u0142a"=1, "latynoska"=2, "czarna"=3, "azjatycka"=4, "mieszana"=5)), selected=3),
                              selectizeInput("ded", "Wykszta\u0142cenie ojca", choices = (c("podstawowe"=0, "\u015Brednie"=1, "maturalne"=2, "technikum"=3, "zawod\u00F3wka"=7, "licencjat"=4, "wy\u017Csze"=5)), selected=5),
                              selectizeInput("marital", "Stan cywilny", choices = (c("w zwiazku ma\u0142\u017Ce\u0144skim"=1, "w separacji"=2, "po rozwodzie"=3, "wdowa"=4, "singiel"=5)), selected=2),
                              selectizeInput("inc", "Miesi\u0119czny doch\u00F3d", choices=(c("poni\u017Cej 2500"=0, "2500-4999"=1, "5000-7499"=2, "7500-9999"=3, "10 000-12 499"=4, "12 500-14 999"=5, "15 000-17 499"=6, "17 500-19 999"=7, "20 000-22 499"=8, "powy\u017Cej 22 500"=9)), selected=4),
                              selectizeInput("smoke", "Czy mama pali\u0142a papierosy?", choices=(c("Nie, nigdy"=0, "Tak, obecnie"=1, "Tak, przed ci\u0105\u017C\u0105"=2, "Tak, dawno temu"=3)), selected=1),
                              selectizeInput("time", "Kiedy mama rzuci\u0142a palenie?", choices=(c("Nie dotyczy"=0, "Nadal pali"=1, "Podczas ci\u0105\u017Cy"=2, "Rok temu"=3, "2 lata temu"=4, "3 lata temu"=5, "4 lata temu"=6, "5-9 lat temu"=7, "10 lat temu"=8, "nie pami\u0119tam"=9)), selected=0),
                              selectizeInput("number", "Liczba palonych papieros\u00F3w dziennie", choices=(c("Nie dotyczy"=0, "1-4"=1, "5-9"=2, "10-14"=3, "15-19"=4, "20-29"=5, "30-39"=6, "40-60"=7, "ponad 60"=8, "nie wiem ile"=9)), selected=0)
                          ),
                          # Show a plot of the generated distribution
                          mainPanel(
                              dateInput("date1", "Data porodu wyznaczona przez lekarza:", value = "2012-02-29", format = "dd/mm/yyyy"),
                              h3("Prognozowana data porodu:"),
                              plotOutput("Prediction"),
                              h3(textOutput("Data")))
                      )
))