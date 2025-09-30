setwd('/Users/ilsongjeon/Desktop/For_Job_Interviews/KFL_Praktikum/data')
library(readr)
library(ggplot2)
library(ggpubr)
library(scales)
data_Almsee <-read.csv("Ergebnisse.csv",header = T,sep = ",")
View(data_Almsee)
as.factor(data_Almsee$Uhrzeit)
names(data_Almsee)

q<-ggplot(data_Almsee, aes(x=factor(Uhrzeit, levels = c('8:10', '8:20', '8:30', '8:40', '8:50', '9:00', '9:10', 
'9:20', '9:30', '9:40', '9:50', '10:00', '11:40', '11:50', '11:55', '12:00', 
'12:05', '12:10', '12:15', '12:20', '12:25', '12:30', '12:35', 
'12:45', '14:55', '15:05', '15:10', '15:15', '15:20', '15:25', 
'15:30', '15:35', '15:40', '15:45', '15:50', '15:55', '16:00', 
'16:05', '16:10', '16:15', '16:20', '16:25', '16:30', '16:35', 
'16:40', '16:45', '16:50', '17:00'))
                           ,y=Kohlmeise_number,fill = Tagesabschnitt))+
  geom_boxplot()
q

ggplot(data_Almsee, aes(x=factor(Uhrzeit, levels = c('8:10', '8:20', '8:30', '8:40', '8:50', '9:00', '9:10', 
                                                     '9:20', '9:30', '9:40', '9:50', '10:00', '11:40', '11:50', '11:55', '12:00', 
                                                     '12:05', '12:10', '12:15', '12:20', '12:25', '12:30', '12:35', 
                                                     '12:45', '14:55', '15:05', '15:10', '15:15', '15:20', '15:25', 
                                                     '15:30', '15:35', '15:40', '15:45', '15:50', '15:55', '16:00', 
                                                     '16:05', '16:10', '16:15', '16:20', '16:25', '16:30', '16:35', 
                                                     '16:40', '16:45', '16:50', '17:00'))
                        ,y=All_Individuen_number,fill = Tagesabschnitt))+
  geom_boxplot()
