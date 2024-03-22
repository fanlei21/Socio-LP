library(readxl)
library(ggplot2)

rwzbsx = c('AFAF','RLR','RP','POP',
           'AMP','FU','GPA','CPA','OCPpc','OCP','TMPpc','TMP','TGPpc','TGP',
           'FE','FR','GDPpc','GDP')

rw_DID_gy_data <- data.frame(matrix(ncol = 2, nrow = 1))
colnames(rw_DID_gy_data) <- c('zhb','group')

rw_DID_gy_data<-rw_DID_gy_data[complete.cases(rw_DID_gy_data),]

for (j in 1:length(rwzbsx)) {
  rw_DID_gy = read_excel("C:/Users/DID_data.xlsx",sheet = rwzbsx[j])
  xx = rw_DID_gy[rw_DID_gy$`dTreat:dt_p_value`<= 0.1,]
  group <- rep(c(rep(j,nrow(xx))))
  data <- data.frame(xx$`dTreat:dt`, group)
  data<-data[complete.cases(data),]
  colnames(data) <- c('zhb','group')
  
  rw_DID_gy_data <- rbind(rw_DID_gy_data,data)
}

group2 <- c(c(rep(1,(length(which(rw_DID_gy_data$group <=4))))),
            c(rep(2,length(which(rw_DID_gy_data$group >=5 & rw_DID_gy_data$group <=14)))),
            c(rep(3,length(which(rw_DID_gy_data$group >=15)))))

rw_DID_gy_data <- cbind(rw_DID_gy_data,group2)
rw_DID_gy_data$group<- factor(rw_DID_gy_data$group)
rw_DID_gy_data$group2<- factor(rw_DID_gy_data$group2)


ggplot(data=rw_DID_gy_data,mapping=aes(x=zhb*100,y=group,color=group2))+
  geom_boxplot(width=0.5,outlier.colour = NA)+  
  stat_summary(fun.y = mean, geom = "point")+
  theme_bw() +
  scale_y_discrete(labels=rwzbsx)+
  labs(x=expression(italic(f)[contribution]~"(%)"))+ 
  geom_vline(xintercept=0, color="red",linetype="dashed",size = 1)+
  theme(panel.grid = element_blank(), 
        legend.position = "none",   
        plot.title = element_blank(), 
        axis.title.y=element_blank(),
        axis.title.x = element_text(size = 12, 
                                    family = "serif", 
                                    color = "black",
                                    hjust = 0.5, 
                                    angle = 0),
        axis.text.x = element_text(size = 12,  
                                   color = "black", 
                                   vjust = 0.5, 
                                   hjust = 0.5, 
                                   angle = 0), 
        axis.text=element_text(size=12,color='black',family="serif") 
  )+
  annotate("text",family="serif",size=4,parse = TRUE,x = -117, y = 18,label = as.character(expression(paste(italic(n)," = 88"))),hjust = 0)+
  annotate("text",family="serif",size=4,parse = TRUE,x = -117, y = 16,label = as.character(expression(paste(italic(n)," = 10"))),hjust = 0)+
  annotate("text",family="serif",size=4,parse = TRUE,x = -117, y = 15,label = as.character(expression(paste(italic(n)," = 5"))),hjust = 0)+
  annotate("text",family="serif",size=4,parse = TRUE,x = -117, y = 17,label = as.character(expression(paste(italic(n)," = 3"))),hjust = 0)+
  annotate("text",family="serif",size=4,parse = TRUE,x = -117, y = 5,label = as.character(expression(paste(italic(n)," = 127"))),hjust = 0)+
  annotate("text",family="serif",size=4,parse = TRUE,x = -117, y = 6,label = as.character(expression(paste(italic(n)," = 115"))),hjust = 0)+
  annotate("text",family="serif",size=4,parse = TRUE,x = -117, y = 8,label = as.character(expression(paste(italic(n)," = 143"))),hjust = 0)+
  annotate("text",family="serif",size=4,parse = TRUE,x = -117, y = 7,label = as.character(expression(paste(italic(n)," = 127"))),hjust = 0)+
  annotate("text",family="serif",size=4,parse = TRUE,x = -117, y = 14,label = as.character(expression(paste(italic(n)," = 130"))),hjust = 0)+
  annotate("text",family="serif",size=4,parse = TRUE,x = -117, y = 10,label = as.character(expression(paste(italic(n)," = 90"))),hjust = 0)+
  annotate("text",family="serif",size=4,parse = TRUE,x = -117, y = 12,label = as.character(expression(paste(italic(n)," = 140"))),hjust = 0)+
  annotate("text",family="serif",size=4,parse = TRUE,x = -117, y = 13,label = as.character(expression(paste(italic(n)," = 66"))),hjust = 0)+
  annotate("text",family="serif",size=4,parse = TRUE,x = -117, y = 9,label = as.character(expression(paste(italic(n)," = 12"))),hjust = 0)+
  annotate("text",family="serif",size=4,parse = TRUE,x = -117, y = 11,label = as.character(expression(paste(italic(n)," = 99"))),hjust = 0)+
  annotate("text",family="serif",size=4,parse = TRUE,x = -117, y = 4,label = as.character(expression(paste(italic(n)," = 55"))),hjust = 0)+
  annotate("text",family="serif",size=4,parse = TRUE,x = -117, y = 2,label = as.character(expression(paste(italic(n)," = 105"))),hjust = 0)+
  annotate("text",family="serif",size=4,parse = TRUE,x = -117, y = 3,label = as.character(expression(paste(italic(n)," = 183"))),hjust = 0)+
  annotate("text",family="serif",size=4,parse = TRUE,x = -117, y = 1,label = as.character(expression(paste(italic(n)," = 95"))),hjust = 0)


#=====================
cor_p = read_excel("C:/Users/corr_data.xlsx",sheet = 'corr') 

rwzbsx = c('AFAF','RLR','RP','POP',
           'AMP','FU','GPA','CPA','OCPpc','OCP','TMPpc','TMP','TGPpc','TGP',
           'FE','FR','GDPpc','GDP')

cor_p2 = cor_p[cor_p$zb== rwzbsx[1],]
cor_p2 = rbind(cor_p2,cor_p[cor_p$zb== rwzbsx[2],])
cor_p2 = rbind(cor_p2,cor_p[cor_p$zb== rwzbsx[3],])
cor_p2 = rbind(cor_p2,cor_p[cor_p$zb== rwzbsx[4],])
cor_p2 = rbind(cor_p2,cor_p[cor_p$zb== rwzbsx[5],])
cor_p2 = rbind(cor_p2,cor_p[cor_p$zb== rwzbsx[6],])
cor_p2 = rbind(cor_p2,cor_p[cor_p$zb== rwzbsx[7],])
cor_p2 = rbind(cor_p2,cor_p[cor_p$zb== rwzbsx[8],])
cor_p2 = rbind(cor_p2,cor_p[cor_p$zb== rwzbsx[9],])
cor_p2 = rbind(cor_p2,cor_p[cor_p$zb== rwzbsx[10],])
cor_p2 = rbind(cor_p2,cor_p[cor_p$zb== rwzbsx[11],])
cor_p2 = rbind(cor_p2,cor_p[cor_p$zb== rwzbsx[12],])
cor_p2 = rbind(cor_p2,cor_p[cor_p$zb== rwzbsx[13],])
cor_p2 = rbind(cor_p2,cor_p[cor_p$zb== rwzbsx[14],])
cor_p2 = rbind(cor_p2,cor_p[cor_p$zb== rwzbsx[15],])
cor_p2 = rbind(cor_p2,cor_p[cor_p$zb== rwzbsx[16],])
cor_p2 = rbind(cor_p2,cor_p[cor_p$zb== rwzbsx[17],])
cor_p2 = rbind(cor_p2,cor_p[cor_p$zb== rwzbsx[18],])


cor_p2$zb <- factor(cor_p2$zb, levels = rwzbsx)

ggplot()+ 
  geom_bar(data=cor_p2,mapping=aes(y=zb,x=cor,fill=group), 
           position="dodge", 
           stat="identity", 
           width = 0.7)+
  scale_y_discrete(labels=rwzbsx)+
  scale_fill_manual(values = c("#F08C55","#6EC8C8"))+
  scale_fill_discrete(labels = c("Restoration area","Restoration duration"))+
  labs(x="Correlation",y=paste('Annual growth rate'))+ 
  annotate(geom = 'text', label="*", x=0.165, y=5.05, angle=0,size=5)+
  annotate(geom = 'text', label="*", x=0.16, y=6.05, angle=0,size=5)+
  annotate(geom = 'text', label="*", x=0.17, y=11.05, angle=0,size=5)+
  annotate(geom = 'text', label="***", x=0.37, y=15.05, angle=0,size=5)+
  annotate(geom = 'text', label="***", x=0.295, y=16.05, angle=0,size=5)+
  annotate(geom = 'text', label="***", x=0.38, y=17.05, angle=0,size=5)+
  annotate(geom = 'text', label="***", x=0.34, y=18.05, angle=0,size=5)+
  annotate(geom = 'text', label="*", x=-0.17, y=4.7, angle=0,size=5)+
  annotate(geom = 'text', label="**", x=-0.205, y=6.7, angle=0,size=5)+
  annotate(geom = 'text', label="*", x=-0.17, y=7.7, angle=0,size=5)+
  annotate(geom = 'text', label="***", x=-0.28, y=8.7, angle=0,size=5)+
  annotate(geom = 'text', label="***", x=-0.26, y=9.7, angle=0,size=5)+
  annotate(geom = 'text', label="**", x=-0.215, y=12.7, angle=0,size=5)+
  annotate(geom = 'text', label="**", x=-0.215, y=13.65, angle=0,size=5)+
  theme(
    legend.position=c(0.735, 0.09),  
    legend.title = element_blank(), 
    legend.text=element_text(size=12,
                             family = "serif"), 
    plot.title = element_text(size = 12,
                              colour = "black",
                              hjust = 0.5),
    axis.title.y = element_text(size = 12, 
                                family = "serif",
                                color = "black",
                                hjust = 0.5, 
                                angle = 90),
    axis.title.x = element_text(size = 12, 
                                family = "serif",
                                color = "black",
                                hjust = 0.5, 
                                angle = 0),
    axis.text.x = element_text(size = 12,  
                               family = "serif", 
                               color = "black", 
                               vjust = 0.5,
                               hjust = 0.5, 
                               angle = 0),
    axis.text=element_text(size=12,color='black',family="serif")
  )
