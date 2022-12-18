library(ggplot2)
library(tidyr)

data <- Inpatient
dt1 <- data %>% group_by(YEAR) %>% summarise(mean_amt = mean(CLM_PMT_AMT)) 

idx1 <- which(data$BENE_ESRD_IND==1)
data_esrd <- data[idx1,]

idx2 <- which(data[-idx1,]$AGE < 65)
data_young <- data[idx2,]

data_left <- data[-c(idx1,idx2),]

dt2 <- data_esrd %>% group_by(YEAR) %>% summarise(mean_amt = mean(CLM_PMT_AMT)) 
dt3 <- data_young %>% group_by(YEAR) %>% summarise(mean_amt = mean(CLM_PMT_AMT))
dt4 <- data_left %>% group_by(YEAR) %>% summarise(mean_amt = mean(CLM_PMT_AMT))

colnames(dt1)[2] <- "Total"
colnames(dt2)[2] <- "ESRD"
colnames(dt3)[2] <- "Young_with_disability"
colnames(dt4)[2] <- "Older_than_65"

dt <- cbind(dt1,dt2[2],dt3[2],dt4[2])

### code for figure 1
colors <- c("Young with disability" = "blue", "ESRD" = "red", "Older than 65" = "green", "Total" = "black")
ggplot(data = dt, aes(x=YEAR))+
  geom_smooth(aes(y = ESRD, color = "ESRD"), size = 1.0) +
  geom_smooth(aes(y = Young_with_disability, color = "Young with disability"), size = 1.0) +
  geom_smooth(aes(y = Older_than_65, color = "Older than 65"), size = 1.0) +
  geom_smooth(aes(y = Total, color = "Total"), size = 1.0)+
  labs(x = "Year",
       y = "Mean claim amount",
       color = "Legend") +
  scale_color_manual(values = colors)+theme_bw()


### code for figure 2
df<-as.data.frame(cbind(res1$gamma,res2$gamma))
colnames(df)<-c("gamma1","gamma2")

mu <- as.data.frame(colMeans(df))
colnames(mu)<-"gamma.mean"

p1<-ggplot(df, aes(x=gamma1)) +
  geom_histogram(aes(y=..density..), color="black", fill="lightblue",bins=60)+
  geom_vline(data=mu, aes(xintercept=gamma.mean[1]),linetype="dashed")+
  geom_density(alpha=0.6, fill="white") +
  scale_color_brewer(palette="Accent") + 
  theme_minimal()+theme(legend.position="top") +
  labs(title="Gamma1 Histogram Plot",x="Linear Gamma", y = "Density")
p1+scale_x_continuous(limits = c(0, 20000))

p2<-ggplot(df, aes(x=gamma2)) +
  geom_histogram(aes(y=..density..), color="black", fill="lightblue",bins=60) +
  geom_vline(data=mu, aes(xintercept=gamma.mean[2]),linetype="dashed")+
  geom_density(alpha=0.6, fill="white") +
  scale_color_brewer(palette="Accent") + 
  theme_minimal()+theme(legend.position="top") +
  labs(title="Gamma2 Histogram Plot",x="Logstic Gamma", y = "Density")
p2+scale_x_continuous(limits = c(-5, 0))
