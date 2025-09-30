library(readr)
library(ggplot2)
library(ggpubr)
library(scales)
data_Almsee <-read.csv("Ergebnisse.csv",header = T,sep = ",")
View(data_Almsee)
names(data_Almsee)
head(Uhrzeit,10)
as.factor(data_Almsee$Uhrzeit)
attach(data_Almsee)
kruskal.test(All_Individuen_number ~ Tagesabschnitt, data = data_Almsee)
#############Kohlmeise scan & conti###########
q_scan<-ggplot(data_Almsee, aes(y=Kohlmeise_scan))+
  geom_boxplot(aes(x=Tagesabschnitt, color = factor(Uhrzeit,levels=c('8:10', '8:20', '8:30', '8:40', '8:50', '9:00', '9:10', 
                                                                     '9:20', '9:30', '9:40', '9:50', '10:00', '11:40', '11:50', '11:55', '12:00', 
                                                                     '12:05', '12:10', '12:15', '12:20', '12:25', '12:30', '12:35', 
                                                                     '12:45', '14:55', '15:05', '15:10', '15:15', '15:20', '15:25', 
                                                                     '15:30', '15:35', '15:40', '15:45', '15:50', '15:55', '16:00', 
                                                                     '16:05', '16:10', '16:15', '16:20', '16:25', '16:30', '16:35', 
                                                                     '16:40', '16:45', '16:50', '17:00'))))+
  scale_colour_discrete("Uhrzeit")+labs(x='',y='Kohlmeisen_Anzahl (scan)')
q_scan <- q_scan + labs(color = "Uhrzeit")
q_scan
q<-ggplot(data_Almsee, aes(y=Kohlmeise_number))+
  geom_boxplot(aes(x=Tagesabschnitt, color = factor(Uhrzeit,levels=c('8:10', '8:20', '8:30', '8:40', '8:50', '9:00', '9:10', 
                                                                     '9:20', '9:30', '9:40', '9:50', '10:00', '11:40', '11:50', '11:55', '12:00', 
                                                                     '12:05', '12:10', '12:15', '12:20', '12:25', '12:30', '12:35', 
                                                                     '12:45', '14:55', '15:05', '15:10', '15:15', '15:20', '15:25', 
                                                                     '15:30', '15:35', '15:40', '15:45', '15:50', '15:55', '16:00', 
                                                                     '16:05', '16:10', '16:15', '16:20', '16:25', '16:30', '16:35', 
                                                                     '16:40', '16:45', '16:50', '17:00'))))+
  scale_colour_discrete("Uhrzeit")+labs(x='',y='Kohlmeisen_Anzahl (continuous)') +
  guides(color = 'none')
q <- q + guides(color = "none", fill = "none")

a_scan<-ggboxplot(data_Almsee, x='Tagesabschnitt', y= 'Kohlmeise_scan',order = c("Früh", "Mittag", "Nachmittag"), fill = 'Tagesabschnitt')+
  stat_compare_means()+theme(legend.position = 'none')+labs(x='',y='Kohlmeisen_Anzahl (scan)')
a_scan<-a_scan + guides(color = "none", fill = "none")
a <-ggboxplot(data_Almsee, x='Tagesabschnitt', y= 'Kohlmeise_number',order = c("Früh", "Mittag", "Nachmittag"), fill = 'Tagesabschnitt')+
  stat_compare_means()+theme(legend.position = 'none')+labs(x='',y='Kohlmeisen_Anzahl (continuous)')
a<-a+ guides(color = "none", fill = "none")
leg <- get_legend(q_scan)
ggarrange(q,a,q_scan,a_scan,common.legend = TRUE)

#############Blaumeise scan & conti###########
w_scan<-ggplot(data_Almsee, aes(y=Blaumeise_scan))+
  geom_boxplot(aes(x=Tagesabschnitt, color = factor(Uhrzeit,levels=c('8:10', '8:20', '8:30', '8:40', '8:50', '9:00', '9:10', 
                                                                     '9:20', '9:30', '9:40', '9:50', '10:00', '11:40', '11:50', '11:55', '12:00', 
                                                                     '12:05', '12:10', '12:15', '12:20', '12:25', '12:30', '12:35', 
                                                                     '12:45', '14:55', '15:05', '15:10', '15:15', '15:20', '15:25', 
                                                                     '15:30', '15:35', '15:40', '15:45', '15:50', '15:55', '16:00', 
                                                                     '16:05', '16:10', '16:15', '16:20', '16:25', '16:30', '16:35', 
                                                                     '16:40', '16:45', '16:50', '17:00'))))+
  scale_colour_discrete("Uhrzeit")+labs(x='',y='Blaumeisen_Anzahl (scan)')
w_scan <- w_scan + labs(color = "Uhrzeit")
w_scan
w<-ggplot(data_Almsee, aes(y=Blaumeise_number))+
  geom_boxplot(aes(x=Tagesabschnitt, color = factor(Uhrzeit,levels=c('8:10', '8:20', '8:30', '8:40', '8:50', '9:00', '9:10', 
                                                                     '9:20', '9:30', '9:40', '9:50', '10:00', '11:40', '11:50', '11:55', '12:00', 
                                                                     '12:05', '12:10', '12:15', '12:20', '12:25', '12:30', '12:35', 
                                                                     '12:45', '14:55', '15:05', '15:10', '15:15', '15:20', '15:25', 
                                                                     '15:30', '15:35', '15:40', '15:45', '15:50', '15:55', '16:00', 
                                                                     '16:05', '16:10', '16:15', '16:20', '16:25', '16:30', '16:35', 
                                                                     '16:40', '16:45', '16:50', '17:00'))))+
  scale_colour_discrete("Uhrzeit")+labs(x='',y='Blaumeisen_Anzahl (continuous')
w<-w + guides(color = 'none', fill = 'none')
w
b_scan<-ggboxplot(data_Almsee, x='Tagesabschnitt', y= 'Blaumeise_scan',order = c("Früh", "Mittag", "Nachmittag"), fill = 'Tagesabschnitt')+
  stat_compare_means()+theme(legend.position = 'none')+labs(x='',y='Blaumeisen_Anzahl (scan)')
b_scan
b<-ggboxplot(data_Almsee, x='Tagesabschnitt', y= 'Blaumeise_number',order = c("Früh", "Mittag", "Nachmittag"), fill = 'Tagesabschnitt')+
  stat_compare_means()+theme(legend.position = 'none')+labs(x='',y='Blaumeisen_Anzahl (continuous)')
b<-b + guides(color='none', fill ='none')
b
ggarrange(w,b,w_scan, b_scan,common.legend = TRUE)

#############Sumpfmeise##########
e_scan<-ggplot(data_Almsee, aes(y=Sumpfmeise_scan))+
  geom_boxplot(aes(x=Tagesabschnitt, color = factor(Uhrzeit,levels=c('8:10', '8:20', '8:30', '8:40', '8:50', '9:00', '9:10', 
                                                                     '9:20', '9:30', '9:40', '9:50', '10:00', '11:40', '11:50', '11:55', '12:00', 
                                                                     '12:05', '12:10', '12:15', '12:20', '12:25', '12:30', '12:35', 
                                                                     '12:45', '14:55', '15:05', '15:10', '15:15', '15:20', '15:25', 
                                                                     '15:30', '15:35', '15:40', '15:45', '15:50', '15:55', '16:00', 
                                                                     '16:05', '16:10', '16:15', '16:20', '16:25', '16:30', '16:35', 
                                                                     '16:40', '16:45', '16:50', '17:00'))))+
  scale_colour_discrete("Uhrzeit")+labs(x='',y='Sumpfmeisen_Anzahl (scan)')
e_scan <- e_scan + labs(color = 'Uhrzeit')
e_scan
e <- ggplot(data_Almsee, aes(y=Sumpfmeise_number))+
  geom_boxplot(aes(x=Tagesabschnitt, color = factor(Uhrzeit,levels=c('8:10', '8:20', '8:30', '8:40', '8:50', '9:00', '9:10', 
                                                                     '9:20', '9:30', '9:40', '9:50', '10:00', '11:40', '11:50', '11:55', '12:00', 
                                                                     '12:05', '12:10', '12:15', '12:20', '12:25', '12:30', '12:35', 
                                                                     '12:45', '14:55', '15:05', '15:10', '15:15', '15:20', '15:25', 
                                                                     '15:30', '15:35', '15:40', '15:45', '15:50', '15:55', '16:00', 
                                                                     '16:05', '16:10', '16:15', '16:20', '16:25', '16:30', '16:35', 
                                                                     '16:40', '16:45', '16:50', '17:00'))))+
  scale_colour_discrete("Uhrzeit")+labs(x='',y='Sumpfmeisen_Anzahl (continuous)')
e <- e+ guides(color='none', fill ='none')
e
c_scan<-ggboxplot(data_Almsee, x='Tagesabschnitt', y= 'Sumpfmeise_scan',order = c("Früh", "Mittag", "Nachmittag"), fill = 'Tagesabschnitt')+
  stat_compare_means()+theme(legend.position = 'none')+labs(x='',y='Sumpfmeisen_Anzahl (scan)')
c_scan
c<-ggboxplot(data_Almsee, x='Tagesabschnitt', y= 'Sumpfmeise_number',order = c("Früh", "Mittag", "Nachmittag"), fill = 'Tagesabschnitt')+
  stat_compare_means()+theme(legend.position = 'none')+labs(x='',y='Sumpfmeisen_Anzahl (continuous)')
c <- c + guides(color='none', fill='none')

ggarrange(e,c,e_scan, c_scan,common.legend = TRUE)

#############Alle################
r_scan<-ggplot(data_Almsee, aes(y=All_Individuen_scan))+
  geom_boxplot(aes(x=Tagesabschnitt,color = factor(Uhrzeit,levels=c('8:10', '8:20', '8:30', '8:40', '8:50', '9:00', '9:10', 
                                                                    '9:20', '9:30', '9:40', '9:50', '10:00', '11:40', '11:50', '11:55', '12:00', 
                                                                    '12:05', '12:10', '12:15', '12:20', '12:25', '12:30', '12:35', 
                                                                    '12:45', '14:55', '15:05', '15:10', '15:15', '15:20', '15:25', 
                                                                    '15:30', '15:35', '15:40', '15:45', '15:50', '15:55', '16:00', 
                                                                    '16:05', '16:10', '16:15', '16:20', '16:25', '16:30', '16:35', 
                                                                    '16:40', '16:45', '16:50', '17:00'))))+
  scale_colour_discrete("Uhrzeit")+labs(x='',y='Alle_Anzahl (scan)')
r_scan <- r_scan + labs(color='Uhrzeit')
r_scan
r<-ggplot(data_Almsee, aes(y=All_Individuen_number))+
  geom_boxplot(aes(x=Tagesabschnitt,color = factor(Uhrzeit,levels=c('8:10', '8:20', '8:30', '8:40', '8:50', '9:00', '9:10', 
                                                                    '9:20', '9:30', '9:40', '9:50', '10:00', '11:40', '11:50', '11:55', '12:00', 
                                                                    '12:05', '12:10', '12:15', '12:20', '12:25', '12:30', '12:35', 
                                                                    '12:45', '14:55', '15:05', '15:10', '15:15', '15:20', '15:25', 
                                                                    '15:30', '15:35', '15:40', '15:45', '15:50', '15:55', '16:00', 
                                                                    '16:05', '16:10', '16:15', '16:20', '16:25', '16:30', '16:35', 
                                                                    '16:40', '16:45', '16:50', '17:00'))))+
  scale_colour_discrete("Uhrzeit")+labs(x='',y='Alle_Anzahl (continuous)')
r <- r + guides(color='none', fill='none')
r 

d_scan<-ggboxplot(data_Almsee, x='Tagesabschnitt', y= 'All_Individuen_scan',order = c("Früh", "Mittag", "Nachmittag"), fill = 'Tagesabschnitt')+
  stat_compare_means()+theme(legend.position = 'none')+labs(x='',y='Alle_Anzahl (scan)')

d_scan <- d_scan + guides(color='none', fill='none') 
d_scan
d<-ggboxplot(data_Almsee, x='Tagesabschnitt', y= 'All_Individuen_number',order = c("Früh", "Mittag", "Nachmittag"), fill = 'Tagesabschnitt')+
  stat_compare_means()+theme(legend.position = 'none')+labs(x='',y='Alle_Anzahl (continuous)')
d<-d+ guides(color='none', fill='none')
d
ggarrange(r,d,r_scan,d_scan,common.legend = TRUE)


############kruskal############
a<-ggboxplot(data_Almsee, x='Tagesabschnitt', y= 'Kohlmeise_number',order = c("Früh", "Mittag", "Nachmittag"), fill = 'Tagesabschnitt')+
  stat_compare_means()+theme(legend.position = 'none')+labs(x='',y='Kohlmeisen_Anzahl (continuous sampling)')
a
b<-ggboxplot(data_Almsee, x='Tagesabschnitt', y= 'Blaumeise_number',order = c("Früh", "Mittag", "Nachmittag"), fill = 'Tagesabschnitt')+
  stat_compare_means()+theme(legend.position = 'none')+labs(x='',y='Blaumeisen_Anzahl (continuous sampling)')
b
c<-ggboxplot(data_Almsee, x='Tagesabschnitt', y= 'Sumpfmeise_number',order = c("Früh", "Mittag", "Nachmittag"), fill = 'Tagesabschnitt')+
  stat_compare_means()+theme(legend.position = 'none')+labs(x='',y='Sumpfmeisen_Anzahl (continuous sampling)')
c

d<-ggboxplot(data_Almsee, x='Tagesabschnitt', y= 'All_Individuen_number',order = c("Früh", "Mittag", "Nachmittag"), fill = 'Tagesabschnitt')+
  stat_compare_means()+theme(legend.position = 'none')+labs(x='',y='Alle_Anzahl (continuous sampling)')




leg_a <- get_legend(a)

ggarrange(a,b,c,d,common.legend = TRUE)
