###Diagnostik Fragebogen: Klimaangst---

#Datenvorbereitung-
#Deskriptivstatistik-
#Item- und Skalenanalyse mit allen Items-
#Faktorenanalyse und -rotation-
#Skalen erstellen-
#Item- und Skalenanalyse für Skala 1 und Skala 2-
#Korrelation der beiden Skalen-
#Zusammenhänge mit anderen Variablen-


library(psych)  
library(openxlsx)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(ez)


####Vorbereitung----



#Laden der Daten-
# Session --> Set Working Directory to --> Source File Location
data_raw <- read.csv("Final_Datensatz.csv", sep=";")

#Speicher der Fragebogendaten-
exclude_var <- c(2,3,4,5,6,23,24,25,26,27,28,29,32,33,34,35,36,37)
data <- data_raw %>% select(!colnames(data_raw)[exclude_var])
data <- data[data$FINISHED==1, ]



#Items abspeichern-
items <- c("FG01_01", "FG01_02", "FG01_03", "FG01_04", "FG01_05", "FG01_06",
           "FG01_07", "FG01_08", "FG01_09", "FG01_10", "FG01_11", "FG02_01")


#Wertebereich-
x.min <- 1
x.max <- 4

#Wortlaut der Items-
Wortlaute <- c("01. Der Klimawandel macht mir Angst.",
               "02. Extreme Wetterlagen bekräftigen meine Angst vor dem Klimawandel.",
               "03. Gedanken an die Folgen des Klimawandels verhindern, dass ich Freizeitaktivitäten genießen kann.",
               "04. Wenn meine Freunde oder Bekannte über den Klimawandel sprechen, versuche ich schnell das Thema zu wechseln.",
               "05. Wenn mir auf sozialen Medien Inhalte zum Klimawandel begegnen, dann versuche ich, sie zu ignorieren.",
               "06. Ich fühle mich gestresst, wenn ich an den Klimawandel und seine Folgen denke.",
               "07. Meine Sorgen über den Klimawandeln stören meinen Schlaf.",
               "08. Gedanken an den Klimawandel lassen mich nicht los.",
               "09. Ich habe Hoffnung, dass sich die Erderwärmung noch ausreichend begrenzen lässt.",
               "10. Die möglichen Folgen des Klimawandels besorgen mich.",
               "11. Ich kann etwas gegen die Folgen des Klimawandels ausrichten.",
               "12. Beim Anblick dieses Bildes [Foto eines schmerzenden Eisberges] bleibe ich gelassen.")




####Deskriptivstatistik der Stichprobe----


#Stichprobengröße-
sample_size <- length(data$CASE)

#Geschlecht-
# 1 = männlich, 2 = weiblich, 3 = divers-
data$DD01 <- factor(data$DD01, label = c("männlich", "weiblich", "divers"))
summary(data$DD01)

#Alter-
describe(data$DD03_01)

#Glaube an menschgemachten Klimawandel-
data$KL01 <- factor(data$KL01, label = c("ja", "nein"))
summary(data$KL01)





####Item- und Skalenananalyse mit allen Items----

#Skalenanalyse mit der Funktion "alpha" aus dem psych-Paket-
Skalenanalyse <- psych::alpha(data[,items])

#Tabelle mit Itemkennwerten für Itemanalyse erstellen-
Itemkennwerte <- Skalenanalyse$item.stats                     

#Schwierigkeiten aus den Mittelwerten berechnen-
Itemkennwerte$Schwierigkeit <- (Itemkennwerte$mean-x.min) / (x.max-x.min)  

#Tabelle mit allen relevanten Itemkennwerten-
Itemkennwerte <- Itemkennwerte[,c("n", "r.drop", "mean", "sd", "Schwierigkeit")]   
Itemkennwerte[,c(2:5)] <- round(Itemkennwerte[,c(2:5)], 2)     
names(Itemkennwerte) <- c("N", "r.it", "M", "SD", "Schwierigkeit")         

#Wortlaut der Items und Itemkennwerte zusammenfügen-
Itemkennwerte <- cbind(Wortlaute, Itemkennwerte)               

write.xlsx(Itemkennwerte, file="Itemkennwerte.xlsx")   

#Deskriptive Untersuchung der Itemkennwerte-
psych::describe(Itemkennwerte)

#Reliabilität des Fragebogens über interne Konsistenz mit Cronbachs Alpha-
Skalenanalyse$total$raw_alpha

#Testwerverteilung-
data$Testwert <- apply(data[,items], 1, FUN=sum)

p_scale1_hist <- ggplot(data, aes(x=Testwert)) + 
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "white", colour = "black")  +   
  ylab("Relative Häufigkeit") +                                           
  xlab("Testwert") +
  scale_x_continuous(limits = c(12,48), breaks = c(12,16,20,24,28,32,36,40,44,48)) +
  theme_bw() + 
  theme(plot.title = element_text(size = 16, face = "bold"))


ggsave(p_scale1_hist, filename = "Histogramm_Itemtestwerte.jpg")

#Testwertverteilung-
psych::describe(data$Testwert)


#Cronbachs Alpha, wenn Item 4 entfernt worden wäre-

items_ohne4 <- c("FG01_01", "FG01_02", "FG01_03", "FG01_05", "FG01_06",
           "FG01_07", "FG01_08", "FG01_09", "FG01_10", "FG01_11", "FG02_01")

Skalenanalyse_ohne4 <- psych::alpha(data[,items_ohne4])
Skalenanalyse_ohne4$total$raw_alpha



####Faktorenanalyse und -rotation----

#Parallelanalyse mit Scree-Plot-

Parallelanalyse <- fa.parallel(data[,items], fa="pc", ylabel = "Eigenwert")

eigendat <- rbind(data.frame(Faktor=c(1:length(items)), Eigenwert=Parallelanalyse$pc.values,
                             Typ = "beobachtet"),
                  data.frame(Faktor=c(1:length(items)), Eigenwert=Parallelanalyse$pc.sim,
                             Typ = "simuliert"))

windowsFonts(Arial=windowsFont("TT Arial")) 
apatheme=theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        text=element_text(family='Arial'),
        legend.title=element_blank(),
        legend.position=c(.7,.8),
        axis.line.x = element_line(color='black'),
        axis.line.y = element_line(color='black'))

ggplot(eigendat, aes(x=Faktor, y=Eigenwert, shape=Typ)) +
  geom_line()+
  geom_point(size=2)+
  scale_y_continuous(name='Eigenwert')+
  scale_x_continuous(name='Faktor', breaks=min(eigendat$Faktor):max(eigendat$Faktor))+
  scale_shape_manual(values=c(16,1)) +
  apatheme

ggsave("Screeplot.jpg")



#Faktorenrotation-

library(sjPlot)
library(GPArotation)
tab_pca(data[,items], nmbr.fctr = 2, rotation = "oblimin", fctr.load.tlrn = .2)



####Skalen erstellen für die Items der beiden Faktoren----


Scale1 <- data[,c("CASE","DD01","DD03_01","KL01","FG01_01", "FG01_02", "FG01_03","FG01_06",
                  "FG01_07", "FG01_08","FG01_10","FG02_01")]

items_scale1 <- c("FG01_01", "FG01_02", "FG01_03","FG01_06",
                  "FG01_07", "FG01_08","FG01_10","FG02_01")

Wortlaute_scale1 <- c("01. Der Klimawandel macht mir Angst.",
                      "02. Extreme Wetterlagen bekräftigen meine Angst vor dem Klimawandel.",
                      "03. Gedanken an die Folgen des Klimawandels verhindern, dass ich Freizeitaktivitäten genießen kann.",
                      "06. Ich fühle mich gestresst, wenn ich an den Klimawandel und seine Folgen denke.",
                      "07. Meine Sorgen über den Klimawandeln stören meinen Schlaf.",
                      "08. Gedanken an den Klimawandel lassen mich nicht los.",
                      "10. Die möglichen Folgen des Klimawandels besorgen mich.",
                      "12. Beim Anblick dieses Bildes [Foto eines schmerzenden Eisberges] bleibe ich gelassen.")


Scale2 <- data[,c("CASE","DD01","DD03_01","KL01","FG01_04", "FG01_05","FG01_09","FG01_11")]

items_scale2 <- c("FG01_04", "FG01_05", "FG01_09","FG01_11")

Wortlaute_scale2 <- c( "04. Wenn meine Freunde oder Bekannte über den Klimawandel sprechen, versuche ich schnell das Thema zu wechseln.",
               "05. Wenn mir auf sozialen Medien Inhalte zum Klimawandel begegnen, dann versuche ich, sie zu ignorieren.",
               "09. Ich habe Hoffnung, dass sich die Erderwärmung noch ausreichend begrenzen lässt.",
               "11. Ich kann etwas gegen die Folgen des Klimawandels ausrichten.")

              

####Item- und Skalenanalyse für Skala 1----

#Skalenanalyse mit der Funktion "alpha" aus dem psych-Paket-
Skalenanalyse_scale1 <- psych::alpha(Scale1[,items_scale1])

#Tabelle mit Itemkennwerten für Itemanalyse erstellen-
Itemkennwerte_scale1 <- Skalenanalyse_scale1$item.stats                     

#Schwierigkeiten aus den Mittelwerten berechnen-
Itemkennwerte_scale1$Schwierigkeit <- (Itemkennwerte_scale1$mean-x.min) / (x.max-x.min)  

#Tabelle mit allen relevanten Itemkennwerten-
Itemkennwerte_scale1 <- Itemkennwerte_scale1[,c("n", "r.drop", "mean", "sd", "Schwierigkeit")]   
Itemkennwerte_scale1[,c(2:5)] <- round(Itemkennwerte_scale1[,c(2:5)], 2)     
names(Itemkennwerte_scale1) <- c("N", "r.it", "M", "SD", "Schwierigkeit")         

#Wortlaut der Items und Itemkennwerte zusammenfügen-
Itemkennwerte_scale1 <- cbind(Wortlaute_scale1, Itemkennwerte_scale1)               

write.xlsx(Itemkennwerte_scale1, file="Itemkennwerte_Skala1.xlsx")    

#Deskriptive Untersuchung der Itemkennwerte-
psych::describe(Itemkennwerte_scale1)

#Testwert und Häufigkeitsverteilung für Skala 1-
Scale1$Testwert <- apply(Scale1[,items_scale1], 1, FUN=sum) 

p_scale1_hist <- ggplot(Scale1, aes(x=Testwert)) + 
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "white", colour = "black")  +   
  ylab("Relative Häufigkeit") +                                           
  xlab("Testwert") +
  scale_x_continuous(limits = c(8,32), breaks = c(8,12,16,20,24,28,32)) +
  theme_bw() + 
  theme(plot.title = element_text(size = 16, face = "bold"))

ggsave(p_scale1_hist, filename = "Histogramm_Skala1.jpg")

#Testwertverteilung-
psych::describe(Scale1$Testwert)

#Reliabilität der Skala 1 über interne Konsistenz mit Cronbachs Alpha-
Skalenanalyse_scale1$total$raw_alpha 



####Item- und Skalenanalyse für Skala 2----

#Skalenanalyse mit der Funktion "alpha" aus dem psych-Paket-
Skalenanalyse_scale2 <- psych::alpha(Scale2[,items_scale2])

#Tabelle mit Itemkennwerten für Itemanalyse erstellen-
Itemkennwerte_scale2 <- Skalenanalyse_scale2$item.stats                     

#Schwierigkeiten aus den Mittelwerten berechnen-
Itemkennwerte_scale2$Schwierigkeit <- (Itemkennwerte_scale2$mean-x.min) / (x.max-x.min)  

#Tabelle mit allen relevanten Itemkennwerten-
Itemkennwerte_scale2 <- Itemkennwerte_scale2[,c("n", "r.drop", "mean", "sd", "Schwierigkeit")]   
Itemkennwerte_scale2[,c(2:5)] <- round(Itemkennwerte_scale2[,c(2:5)], 2)     
names(Itemkennwerte_scale2) <- c("N", "r.it", "M", "SD", "Schwierigkeit")         

#Wortlaut der Items und Itemkennwerte zusammenfügen-
Itemkennwerte_scale2 <- cbind(Wortlaute_scale2, Itemkennwerte_scale2)               

write.xlsx(Itemkennwerte_scale2, file="Itemkennwerte_Skala2.xlsx") 

#Deskriptive Untersuchung der Itemkennwerte-
psych::describe(Itemkennwerte_scale2)

#Testwert und Häufigkeitsverteilung für Skala 1-
Scale2$Testwert <- apply(Scale2[,items_scale2], 1, FUN=sum) 

p_scale2_hist <- ggplot(Scale2, aes(x=Testwert)) + 
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "white", colour = "black")  +   
  ylab("Relative Häufigkeit") +                                           
  xlab("Testwert") +
  scale_x_continuous(limits = c(4,16), breaks = c(4,8,12,16)) +
  theme_bw() + 
  theme(plot.title = element_text(size = 16, face = "bold")) 

ggsave(p_scale2_hist, filename = "Histogramm_Skala2.jpg")

#Testwertverteilung-
psych::describe(Scale2$Testwert)

#Reliabilität der Skala 2 über interne Konsistenz mit Cronbachs Alpha-
Skalenanalyse_scale2$total$raw_alpha 









####Korrelation der beiden Skalen----

cor.test(Scale1$Testwert, Scale2$Testwert)

####Zusammenhänge mit anderen Variablen----

#ANOVA für unabhängige Stichproben zum Vergleich von Frauen und Männern-
ezANOVA(Scale1, dv = Testwert, wid = CASE, between = DD01)
pairwise.t.test(Scale1$Testwert, Scale1$DD01, p.adjust = "bonferroni")
describeBy(Scale1$Testwert, Scale1$DD01)

#Korrelation mit dem Alter-
cor.test(Scale1$Testwert, Scale1$DD03_01)

#Unterschiede zwischen Leuten, die an Klimawandel glauben und nicht-
t.test(Testwert ~ KL01, data = Scale1, var.equal = T)
describeBy(Scale1$Testwert, Scale1$KL01)

#Anova für unabhängige Stichproben zum Vergleich von Frauen und Männern-
ezANOVA(Scale2, dv = Testwert, wid = CASE, between = DD01)
pairwise.t.test(Scale2$Testwert, Scale2$DD01, p.adjust = "bonferroni")
describeBy(Scale2$Testwert, Scale2$DD01)

#Korrelation mit dem Alter-
cor.test(Scale2$Testwert, Scale2$DD03_01)

#Unterschiede zwischen Leuten, die an Klimawandel glauben und nicht-
t.test(Testwert ~ KL01, data = Scale2, var.equal = T)
describeBy(Scale2$Testwert, Scale2$KL01)


