library(readxl)
library(ggplot2)

# DID algorithm==============================
#  sign Distinctive
p.codes<-function(values_data){
  # Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
  if (values_data <= 0.001){
    return('***')
  }
  if (values_data > 0.001 & values_data <= 0.01){
    return('**')
  }
  if (values_data > 0.01 & values_data <= 0.05){
    return('*')
  }
  if (values_data > 0.05 & values_data <= 0.1){
    return('.')
  }
  if (values_data > 0.1){
    return(' ')
  }
}

# starting time of the VRPs
fqtj = read_excel("C:/Users/starting_time_of_the_VRPs.xlsx",sheet = 'Sheet1')
head(fqtj)
fqtj[c('MEAN')] = fqtj[c('time')]
dzID <- which(fqtj$slope <= 0)   # control group
syID <- which(fqtj$slope > 0)  # experimental group

zzhib = c('AFAF','RLR','RP','POP',
           'AMP','FU','GPA','CPA','OCP','TMP','TGP',
           'FE','FR','GDP')

for (j in 1:length(zzhib)) {
   nmzrk = read_excel("C:/Users/statistical_yearbook.xlsx",sheet = zzhib[j])
   # head(nmzrk)

# Calculate per capita indicators
rj_zzhib = c('OCPpc','TMPpc','TGPpc','GDPpc')

rk = read_excel("C:/Users/statistical_yearbook.xlsx",sheet = 'POP')
head(rk)
rk[5:ncol(rk)] = rk[5:ncol(rk)]*10000.

#for (j in 1:length(rj_zzhib)) {
#  nmzrk = read_excel("C:/Users/statistical_yearbook.xlsx",sheet = rj_zzhib[j])
  
  # per capita ===============  
  # nmzrk2 = rk
  # nmzrk2[1:4] = nmzrk[1:4]
  # nmzrk2[5:ncol(rk)] = nmzrk[5:ncol(nmzrk)]/rk[5:ncol(rk)]
  # nmzrk = nmzrk2
  #  ===============  
  
  output <- data.frame(matrix(ncol = 4+20+5+1+1+1+3+2+1, nrow = nrow(nmzrk)))
  colnames(output) <- c('FID', 'County_level', 'County_code', 'Unit',
                        'Intercept', 'Intercept_Std', 'Intercept_t_value', 'Intercept_p_value', 'Intercept_p_code',
                        'dTreat', 'dTreat_Std', 'dTreat_t_value', 'dTreat_p_value', 'dTreat_p_code',
                        'dt', 'dt_Std', 'dt_t_value', 'dt_p_value', 'dt_p_code',
                        'dTreat:dt', 'dTreat:dt_Std', 'dTreat:dt_t_value', 'dTreat:dt_p_value', 'dTreat:dt_p_code',
                        'Residuals_Min', 'Residuals_1Q', 'Residuals_Median', 'Residuals_3Q', 'Residuals_Max',
                        'RSE', 'R2', 'R2adj', 'f.statistic', 'df1','df2','f_p-value','f_p-code','n')
  output[,1] = nmzrk[,1]
  output[,2] = nmzrk[,2]
  output[,3] = nmzrk[,3]
  output[,4] = nmzrk[,4]
  
  # Delete blank columns
  nmzrk2 = remove_empty(nmzrk,which = 'cols')
  year <- as.integer(colnames(nmzrk2[5:ncol(nmzrk2)])) 
  t1 <- year[1]
  t2 <- year[length(year)]
  
  
  # control data
  dzdata <- data.frame(matrix(ncol = 4, nrow = length(dzID)*(t2-t1+1)))
  colnames(dzdata) <- c('t','t2','zhib','dTreat')
  dTreat <- c(rep(0,length(dzID)*(t2-t1+1)))
  dzdata[,4] = dTreat
  
  for (i in 1:length(dzID)){
    # Time series data
    if(all(is.na(nmzrk2[dzID[i],5:ncol(nmzrk2)]))) {
      next
    }
    
    temp = t(nmzrk2[dzID[i],5:ncol(nmzrk2)])
    colnames(temp) <- c("zhib")
    
    temp <- data.frame(year,temp)
    temp<-temp[complete.cases(temp),]
    
    if(nrow(temp)<=3) {
      next
    }
    
    # temp2 <- data.frame(c(temp[1,1]:temp[nrow(temp),1]),c(1:(temp[nrow(temp),2]-temp[1,2]+1)))
    temp2 <- data.frame(c(t1:t2),c(1:(t2-t1+1)))
    colnames(temp2) <- c("t","t2")
    temp3 <- merge(temp2, temp, by.x = "t", by.y = "year",all = TRUE)
    #Spline interpolation
    temp3 <- as.data.frame(na.spline(temp3))
    # temp3 <- as.data.frame(replace(na.spline(temp3), is.na(na.approx(temp3, na.rm=FALSE)), NA))
    
    if (sum(sign(temp3$zhib)) == nrow(temp3)){
      dzdata[((i-1)*nrow(temp3)+1):(i*nrow(temp3)),1:3] = temp3
    }
    
  }

  dzdata<-dzdata[complete.cases(dzdata),]
  dzdata2 <- aggregate(dzdata$zhib, by=list(type=dzdata$t2),median)
  #dzdata2 <- aggregate(dzdata$zhib, by=list(type=dzdata$t2),mean)
  dzdata2 <- cbind(c(t1:t2),dzdata2,c(rep(0,t2-t1+1)))
  colnames(dzdata2) <- c("t",'t2','zhib','dTreat')
  
  # experimental data
  for (i in 1:length(syID)){

    if(all(is.na(nmzrk2[syID[i],5:ncol(nmzrk2)]))) {
      next
    }
    
    temp = t(nmzrk2[syID[i],5:ncol(nmzrk2)])
    colnames(temp) <- c("zhib")
    
    temp <- data.frame(year,temp)
    temp<-temp[complete.cases(temp),]
    
    if(nrow(temp)<=3) {
      next
    }
    yxz = nrow(temp)
    
    # temp2 <- data.frame(c(temp[1,1]:temp[nrow(temp),1]),c(1:(temp[nrow(temp),2]-temp[1,2]+1)))
    temp2 <- data.frame(c(t1:t2),c(1:(t2-t1+1)))
    colnames(temp2) <- c("t","t2")
    temp3 <- merge(temp2, temp, by.x = "t", by.y = "year",all = TRUE)
    temp3 <- as.data.frame(na.spline(temp3))
    # temp3 <- as.data.frame(replace(na.spline(temp3), is.na(na.approx(temp3, na.rm=FALSE)), NA))
    
    if (sum(sign(temp3$zhib)) == nrow(temp3)){
      tgt = round(fqtj$MEAN[syID[i]])  #time
      if (tgt <= t1 || tgt >= t2){
        next
      }
      
      dTreat <- c(rep(1,(t2-t1+1)))
      # dt <- rep(c(rep(0,tgt-t1),rep(1,t2-tgt+1)),(nrow(dzdata)/nrow(temp3)+1))
      dt <- rep(c(rep(0,tgt-t1),rep(1,t2-tgt+1)),2) 
      sydata <- cbind(temp3,dTreat)
      
      ptDIDdata <- rbind(sydata,dzdata2)
      ptDIDdata <- cbind(ptDIDdata,dt)
      
      # DID
      lm.fit <- lm(zhib ~ dTreat + dt + dTreat*dt, data = ptDIDdata)
      # summary(lm.fit)
      
      coefficientsdata <- summary(lm.fit)$coefficients %>% round(digits = 3) %>% as.data.frame() 
      Residuals <- quantile(summary(lm.fit)$residuals) %>% round(digits = 3) %>% as.data.frame() 
      RSE <- summary(lm.fit)$sigma %>% round(digits = 3)
      R2 <- summary(lm.fit)$r.squared %>% round(digits = 3)
      R2adj <- summary(lm.fit)$adj.r.squared %>% round(digits = 3)
      f.statistic <- summary(lm.fit)$fstatistic %>% round(digits = 3) %>% as.data.frame() # F-statistic
      p.value <- pf(f.statistic[1,1],f.statistic[2,1],f.statistic[3,1], lower.tail = FALSE) %>% round(digits = 3)
      
      output[syID[i],5] = coefficientsdata[1,1]
      output[syID[i],6] = coefficientsdata[1,2]
      output[syID[i],7] = coefficientsdata[1,3]
      output[syID[i],8] = coefficientsdata[1,4]
      output[syID[i],9] = p.codes(coefficientsdata[1,4])
      output[syID[i],10] = coefficientsdata[2,1]
      output[syID[i],11] = coefficientsdata[2,2]
      output[syID[i],12] = coefficientsdata[2,3]
      output[syID[i],13] = coefficientsdata[2,4]
      output[syID[i],14] = p.codes(coefficientsdata[2,4])
      output[syID[i],15] = coefficientsdata[3,1]
      output[syID[i],16] = coefficientsdata[3,2]
      output[syID[i],17] = coefficientsdata[3,3]
      output[syID[i],18] = coefficientsdata[3,4]
      output[syID[i],19] = p.codes(coefficientsdata[3,4])
      output[syID[i],20] = coefficientsdata[4,1]
      output[syID[i],21] = coefficientsdata[4,2]
      output[syID[i],22] = coefficientsdata[4,3]
      output[syID[i],23] = coefficientsdata[4,4]
      output[syID[i],24] = p.codes(coefficientsdata[4,4])
      output[syID[i],25] = Residuals[1,1]
      output[syID[i],26] = Residuals[2,1]
      output[syID[i],27] = Residuals[3,1]
      output[syID[i],28] = Residuals[4,1]
      output[syID[i],29] = Residuals[5,1]
      output[syID[i],30] = RSE
      output[syID[i],31] = R2
      output[syID[i],32] = R2adj
      output[syID[i],33] = f.statistic[1,1]
      output[syID[i],34] = f.statistic[2,1]
      output[syID[i],35] = f.statistic[3,1]
      output[syID[i],36] = p.value
      output[syID[i],37] = p.codes(p.value)
      output[syID[i],38] = yxz
    }
    
  }
  
   write.xlsx(output, file = "C:/Users/DID_data.xlsx",
              row.names = TRUE, col.names = TRUE, sheetName = zzhib[j], showNA = FALSE, append = TRUE)
  #write.xlsx(output, file = "C:/Users/DID_data_pc.xlsx",
  #           row.names = TRUE, col.names = TRUE, sheetName = rj_zzhib[j], showNA = FALSE, append = TRUE)
}


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
  geom_bar(data=cor_p2,mapping=aes(y=zb,x=cor,fill=group), position="dodge", stat="identity", width = 0.7)+
  scale_y_discrete(labels=rwzbsx)+
  labs(x="Correlation",y=paste('Annual growth rate'))+ 
  theme_classic()+ 
  annotate(geom = 'text', label="*", x=-0.136, y=8.05, angle=0,size=5)+
  annotate(geom = 'text', label="**", x=-0.20, y=9.05, angle=0,size=5)+
  annotate(geom = 'text', label="*", x=-0.147, y=10.05, angle=0,size=5)+
  annotate(geom = 'text', label="***", x=-0.218, y=13.05, angle=0,size=5)+
  annotate(geom = 'text', label="*", x=-0.158, y=14.05, angle=0,size=5)+
  annotate(geom = 'text', label="**", x=-0.186, y=4.7, angle=0,size=5)+
  annotate(geom = 'text', label="**", x=-0.21, y=6.7, angle=0,size=5)+
  annotate(geom = 'text', label="**", x=-0.185, y=7.7, angle=0,size=5)+
  annotate(geom = 'text', label="*", x=-0.16, y=8.7, angle=0,size=5)+
  annotate(geom = 'text', label="*", x=-0.168, y=12.7, angle=0,size=5)+
  annotate(geom = 'text', label="**", x=-0.174, y=13.7, angle=0,size=5)+
  theme(
    legend.position=c(0.2, 0.92),  
    legend.title = element_blank(), 
    legend.text=element_text(size=12,family = "serif"), 
    plot.title = element_text(size = 12,colour = "black",hjust = 0.5),
    axis.title.y = element_text(size = 12, family = "serif",color = "black",hjust = 0.5, angle = 90),
    axis.title.x = element_text(size = 12, family = "serif",color = "black",hjust = 0.5, angle = 0),
    axis.text.x = element_text(size = 12, family = "serif", color = "black", vjust = 0.5, hjust = 0.5, angle = 0), 
    axis.text=element_text(size=12,color='black',family="serif")
  )+
  scale_fill_manual(values = c("#396712","#F5C65F"))
