setwd("")

######### WTP #########
library(mlogit)
library(gmnl)
library(MASS)
library(mded)

df.long <- read.csv("df.long.csv")
df.long$nobuy <- ifelse(df.long$alternative == 3, 1,0)
df.long$price_act <- ifelse(df.long$price == 1, 10,
                            ifelse(df.long$price == 2, 10.5,
                                   ifelse(df.long$price == 3, 11, 
                                          ifelse(df.long$price == 4, 11.5, 
                                                 ifelse(df.long$price == 0, 0, -999)))))





ml.wtp<-mlogit.data(data = df.long, 
                    choice ="Choice", 
                    shape = "long", 
                    alt.var = "alternative", 
                    reflevel = 3,
                    id.var = "ID",
                    opposite = 'price',
)
######## mnl wtp #######
start <- c(1,rep(0.1,3),1,0.1)
wtps.control <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|1,
             reflevel = 3,
             model="smnl",
             data=ml.wtp,
             subset = ml.wtp$treat==0,
             fixed= c(T, rep(F,3), T, F),
             panel = T, 
             haltons = NA,
             R=500,
             start = start,
             print.init = 2,
             print.level =2,
             method = "bhhh",
             rterlim = 500
             
)
summary(wtps.control)
AIC(wtps.control)
BIC(wtps.control)

effect.gmnl(wtps.control, effect = "ce")
wtps.t1 <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|1,
                     reflevel = 3,
                     model="smnl",
                     data=ml.wtp,
                     subset = ml.wtp$treat==1,
                     fixed= c(T, rep(F,3), T, F),
                     panel = T, 
                     haltons = NA,
                     start = start,
                     print.init = 2,
                     print.level =2,
                     method = "bhhh",
                     rterlim = 500
                     
)
summary(wtps.t1)
AIC(wtps.t1)
BIC(wtps.t1)

#extract individual specific wtps for all variables
ind.wtp.control <- as.data.frame(effect.gmnl(wtps.control,effect = "ce")$mean)
ind.wtp.t1 <- as.data.frame(effect.gmnl(wtps.t1,effect = "ce")$mean)
# Poe test
library(mded)
m1 <- ind.wtp.control$label
m2 <-ind.wtp.t1$label

if(mean(m1) >= mean(m2)){
  mded(m1,m2)
}else{
  mded(m2,m1)
}
######### wtp rpl #####
### using negative price
##### Control #######
mnl.control <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|0,
                            reflevel = 3,
                            model="mnl",
                            data=ml.wtp,
                            subset = ml.wtp$treat==0,
                            print.init = 2,
                            print.level =2
            )

wtp.control <-coef(mnl.control)/coef(mnl.control)[1]
#without correlation ####
start.control <- c(wtp.control,0.1,0.1,0.1,0.1,0)
wtp.rpl.control <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|1,
                reflevel = 3,
                model="gmnl",
                data=ml.wtp,
                subset = ml.wtp$treat==0,
                ranp = c(label = "n", method = "n"),
                correlation = F,
                fixed= c(T, rep(F,3), rep(F,2), F,F,T),
                panel = T, 
                haltons = NA,
                start = start.control,
                print.init = 2,
                print.level =2,
                method = "bhhh",
                rterlim = 500
                
)
summary(wtp.rpl.control)
AIC(wtp.rpl.control)
BIC(wtp.rpl.control)

## with correlation
start.control.cor <- c(wtp.control,0.1,0.1,0.1,0.1,0.1,0)
wtp.rpl.control.cor <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|1,
                        reflevel = 3,
                        model="gmnl",
                        data=ml.wtp,
                        subset = ml.wtp$treat==0,
                        ranp = c(label = "n", method = "n"),
                        correlation = T,
                        fixed= c(T, rep(F,3), rep(F,3), F,F,T),
                        panel = T, 
                        haltons = NA,
                        start = start.control.cor,
                        print.init = 2,
                        print.level =2,
                        method = "bhhh",
                        rterlim = 500
                        
)
summary(wtp.rpl.control.cor)
AIC(wtp.rpl.control.cor)
BIC(wtp.rpl.control.cor)
### treatment 1 ####
mnl.t1 <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|0,
                    reflevel = 3,
                    model="mnl",
                    data=ml.wtp,
                    subset = ml.wtp$treat==1,
                    print.init = 2,
                    print.level =2
)

wtp.t1 <-coef(mnl.t1)/coef(mnl.t1)[1]

start.t1 <- c(wtp.t1,0.1,0.1,0.1,0.1,0)
wtp.rpl.t1 <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|1,
                        reflevel = 3,
                        model="gmnl",
                        data=ml.wtp,
                        subset = ml.wtp$treat==1,
                        ranp = c(label = "n", method = "n"),
                        correlation = F,
                        fixed= c(T, rep(F,3), rep(F,2), F,F,T),
                        panel = T, 
                        haltons = NA,
                        start = start.t1,
                        print.init = 2,
                        print.level =2,
                        method = "bhhh",
                        rterlim = 500
                        
)
summary(wtp.rpl.t1)
AIC(wtp.rpl.t1)
BIC(wtp.rpl.t1)

start.t1.cor <- c(wtp.t1,0.1,0.1,0.1,0.1,0.1,0)
wtp.rpl.t1.cor <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|1,
                            reflevel = 3,
                            model="gmnl",
                            data=ml.wtp,
                            subset = ml.wtp$treat==1,
                            ranp = c(label = "n", method = "n"),
                            correlation = T,
                            fixed= c(T, rep(F,3), rep(F,3), F,F,T),
                            panel = T, 
                            haltons = NA,
                            start = start.t1.cor,
                            print.init = 2,
                            print.level =2,
                            method = "bhhh",
                            rterlim = 500
                            
)
summary(wtp.rpl.t1.cor)
AIC(wtp.rpl.t1.cor)
BIC(wtp.rpl.t1.cor)

### treatment 2 ####
mnl.t2 <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|0,
               reflevel = 3,
               model="mnl",
               data=ml.wtp,
               subset = ml.wtp$treat==2,
               print.init = 2,
               print.level =2
)

wtp.t2 <-coef(mnl.t2)/coef(mnl.t2)[1]
start.t2 <- c(wtp.t2,0.1,0.1,0.1,0.1,0)
wtp.rpl.t2 <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|1,
                   reflevel = 3,
                   model="gmnl",
                   data=ml.wtp,
                   subset = ml.wtp$treat==2,
                   ranp = c(label = "n", method = "n"),
                   correlation = F,
                   fixed= c(T, rep(F,3), rep(F,2), F,F,T),
                   panel = T, 
                   haltons = NA,
                   start = start.t2,
                   print.init = 2,
                   print.level =2,
                   method = "bhhh",
                   rterlim = 500
                   
)
summary(wtp.rpl.t2)
AIC(wtp.rpl.t2)
BIC(wtp.rpl.t2)

start.t2.cor <- c(wtp.t2,0.1,0.1,0.1,0.1,0.1,0)
wtp.rpl.t2.cor <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|1,
                       reflevel = 3,
                       model="gmnl",
                       data=ml.wtp,
                       subset = ml.wtp$treat==2,
                       ranp = c(label = "n", method = "n"),
                       correlation = T,
                       fixed= c(T, rep(F,3), rep(F,3), F,F,T),
                       panel = T, 
                       haltons = NA,
                       start = start.t2.cor,
                       print.init = 2,
                       print.level =2,
                       method = "bhhh",
                       rterlim = 500
                       
)
summary(wtp.rpl.t2.cor)
AIC(wtp.rpl.t2.cor)
BIC(wtp.rpl.t2.cor)
### treatment 3 ####
mnl.t3 <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|0,
               reflevel = 3,
               model="mnl",
               data=ml.wtp,
               subset = ml.wtp$treat==3,
               print.init = 2,
               print.level =2
)

wtp.t3 <-coef(mnl.t3)/coef(mnl.t3)[1]
start.t3 <- c(wtp.t3,0.1,0.1,0.1,0.1,0)
wtp.rpl.t3 <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|1,
                   reflevel = 3,
                   model="gmnl",
                   data=ml.wtp,
                   subset = ml.wtp$treat==3,
                   ranp = c(label = "n", method = "n"),
                   correlation = F,
                   fixed= c(T, rep(F,3), rep(F,2), F,F,T),
                   panel = T, 
                   haltons = NA,
                   start = start.t3,
                   print.init = 2,
                   print.level =2,
                   method = "bhhh",
                   rterlim = 500
                   
)
summary(wtp.rpl.t3)
AIC(wtp.rpl.t3)
BIC(wtp.rpl.t3)

start.t3.cor <- c(wtp.t3,0.1,0.1,0.1,0.1,0.1,0)
wtp.rpl.t3.cor <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|1,
                       reflevel = 3,
                       model="gmnl",
                       data=ml.wtp,
                       subset = ml.wtp$treat==3,
                       ranp = c(label = "n", method = "n"),
                       correlation = T,
                       fixed= c(T, rep(F,3), rep(F,3), F,F,T),
                       panel = T, 
                       haltons = NA,
                       start = start.t3.cor,
                       print.init = 2,
                       print.level =2,
                       method = "bhhh",
                       rterlim = 500
                       
)
summary(wtp.rpl.t3.cor)
AIC(wtp.rpl.t3.cor)
BIC(wtp.rpl.t3.cor)
### treatment 4 ####
mnl.t4 <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|0,
               reflevel = 3,
               model="mnl",
               data=ml.wtp,
               subset = ml.wtp$treat==4,
               print.init = 2,
               print.level =2
)

wtp.t4 <-coef(mnl.t4)/coef(mnl.t4)[1]
start.t4 <- c(wtp.t4,0.1,0.1,0.1,0.1,0)
wtp.rpl.t4 <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|1,
                   reflevel = 3,
                   model="gmnl",
                   data=ml.wtp,
                   subset = ml.wtp$treat==4,
                   ranp = c(label = "n", method = "n"),
                   correlation = F,
                   fixed= c(T, rep(F,3), rep(F,2), F,F,T),
                   panel = T, 
                   haltons = NA,
                   start = start.t4,
                   print.init = 2,
                   print.level =2,
                   method = "bhhh",
                   rterlim = 500
                   
)
summary(wtp.rpl.t4)
AIC(wtp.rpl.t4)
BIC(wtp.rpl.t4)

start.t4.cor <- c(wtp.t4,0.1,0.1,0.1,0.1,0.1,0)
wtp.rpl.t4.cor <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|1,
                       reflevel = 3,
                       model="gmnl",
                       data=ml.wtp,
                       subset = ml.wtp$treat==4,
                       ranp = c(label = "n", method = "n"),
                       correlation = T,
                       fixed= c(T, rep(F,3), rep(F,3), F,F,T),
                       panel = T, 
                       haltons = NA,
                       start = start.t4.cor,
                       print.init = 2,
                       print.level =2,
                       method = "bhhh",
                       rterlim = 500
                       
)
summary(wtp.rpl.t4.cor)
AIC(wtp.rpl.t4.cor)
BIC(wtp.rpl.t4.cor)
### treatment 5 ####
mnl.t5 <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|0,
               reflevel = 3,
               model="mnl",
               data=ml.wtp,
               subset = ml.wtp$treat==5,
               print.init = 2,
               print.level =2
)

wtp.t5 <-coef(mnl.t5)/coef(mnl.t5)[1]
start.t5 <- c(wtp.t5,0.1,0.1,0.1,0.1,0)
wtp.rpl.t5 <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|1,
                   reflevel = 3,
                   model="gmnl",
                   data=ml.wtp,
                   subset = ml.wtp$treat==5,
                   ranp = c(label = "n", method = "n"),
                   correlation = F,
                   fixed= c(T, rep(F,3), rep(F,2), F,F,T),
                   panel = T, 
                   haltons = NA,
                   start = start.t5,
                   print.init = 2,
                   print.level =2,
                   method = "bhhh",
                   rterlim = 500
                   
)
summary(wtp.rpl.t5)
AIC(wtp.rpl.t5)
BIC(wtp.rpl.t5)

start.t5.cor <- c(wtp.t5,0.1,0.1,0.1,0.1,0.1,0)
wtp.rpl.t5.cor <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|1,
                       reflevel = 3,
                       model="gmnl",
                       data=ml.wtp,
                       subset = ml.wtp$treat==5,
                       ranp = c(label = "n", method = "n"),
                       correlation = T,
                       fixed= c(T, rep(F,3), rep(F,3), F,F,T),
                       panel = T, 
                       haltons = NA,
                       start = start.t5.cor,
                       print.init = 2,
                       print.level =2,
                       method = "bhhh",
                       rterlim = 500
                       
)
summary(wtp.rpl.t5.cor)
AIC(wtp.rpl.t5.cor)
BIC(wtp.rpl.t5.cor)
### treatment 6 ####
mnl.t6 <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|0,
               reflevel = 3,
               model="mnl",
               data=ml.wtp,
               subset = ml.wtp$treat==6,
               print.init = 2,
               print.level =2
)

wtp.t6 <-coef(mnl.t6)/coef(mnl.t6)[1]
start.t6 <- c(wtp.t6,0.1,0.1,0.1,0.1,0)
wtp.rpl.t6 <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|1,
                   reflevel = 3,
                   model="gmnl",
                   data=ml.wtp,
                   subset = ml.wtp$treat==6,
                   ranp = c(label = "n", method = "n"),
                   correlation = F,
                   fixed= c(T, rep(F,3), rep(F,2), F,F,T),
                   panel = T, 
                   haltons = NA,
                   start = start.t6,
                   print.init = 2,
                   print.level =2,
                   method = "bhhh",
                   rterlim = 500
                   
)
summary(wtp.rpl.t6)
AIC(wtp.rpl.t6)
BIC(wtp.rpl.t6)

start.t6.cor <- c(wtp.t6,0.1,0.1,0.1,0.1,0.1,0)
wtp.rpl.t6.cor <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|1,
                       reflevel = 3,
                       model="gmnl",
                       data=ml.wtp,
                       subset = ml.wtp$treat==6,
                       ranp = c(label = "n", method = "n"),
                       correlation = T,
                       fixed= c(T, rep(F,3), rep(F,3), F,F,T),
                       panel = T, 
                       haltons = NA,
                       start = start.t6.cor,
                       print.init = 2,
                       print.level =2,
                       method = "bhhh",
                       rterlim = 500
                       
)
summary(wtp.rpl.t6.cor)
AIC(wtp.rpl.t6.cor)
BIC(wtp.rpl.t6.cor)
####### test ######
###### not correlation ######
#install.packages("MASS")
library(MASS)

model <- c("wtp.rpl.control.cor", "wtp.rpl.t1.cor",
           "wtp.rpl.t2.cor", "wtp.rpl.t3.cor",
           "wtp.rpl.t4.cor", "wtp.rpl.t5.cor","wtp.rpl.t6.cor")
#model <- c("wtp.rpl.control", "wtp.rpl.t1")
variable_l <- c("label")
variable_m <- c("method")
cor = T
kr.wtp.rpl.draw <- function(model, variable,cor){
  set.seed(1234)
  ct = 0
  if(cor == F){
    for (i in 1:length(model)){
      for (j in 1:length(variable)){
        mean <- coef(eval(parse(text = model[i])))[variable[j]]
        sd <- coef(wtp.rpl.control)[paste("sd",variable[j],sep = ".")]
        
        draw <- rnorm(10000, mean, sd)
        table <- data.frame(x= draw)
        colnames(table) <- paste(variable[j],model[i],sep = "_")
        ct = ct + 1
        if(ct == 1) table1 <- table else table1 <- cbind(table1, table)
      }
    }
  } else {
    for (i in 1:length(model)){
      mean <- coef(eval(parse(text = model[i])))[variable]
      cov <- vcov(eval(parse(text = model[i])))[variable,variable]
      
      draw <- as.data.frame(mvrnorm(10000, mean, cov))
      colnames(draw) <-paste(variable,model[i],sep = "_")
      table <- draw
      ct = ct + 1
      if(ct == 1) table1 <- table else table1 <- cbind(table1, table)
    }
  }
  
  return(table1)
}

vcov(wtp.rpl.control, what = "ranp")
sqrt(206.1538)
draw_table_l <- kr.wtp.rpl.draw(model, variable_l,cor)
draw_table_m <- kr.wtp.rpl.draw(model, variable_m,cor)
### Poe test
library(mded)
colnames(draw_table_l)
test1 <- draw_table[,1]
test2 <- draw_table[,3]


poe_multiple_test <- function(table){
  test_variable1 <- colnames(table)
  ct = 0
  for(i in 1:length(test_variable1)){
    test_variable2 <- test_variable1[-i]
    for(j in 1:length(test_variable2)){
      if(mean(table[,test_variable1[i]])>=mean(table[,test_variable2[j]])){
        poetest <- mded(table[,test_variable1[i]], table[,test_variable2[j]])
        test_table <- data.frame(large_variable = test_variable1[i],
                                 large_variable_mean = poetest$means[1],
                                 small_variable = test_variable2[j],
                                 small_variable_mean = poetest$means[2],
                                 p_value = poetest$stat)
      } else {
        poetest <- mded(table[,test_variable2[j]],table[,test_variable1[i]])
        test_table <- data.frame(large_variable = test_variable2[j],
                                 large_variable_mean = poetest$means[1],
                                 small_variable = test_variable1[i],
                                 small_variable_mean = poetest$means[2],
                                 p_value = poetest$stat)
      }
      ct = ct + 1
      if(ct==1) test_table1 <- test_table else test_table1 <- rbind(test_table1,test_table)
    }
  }
  test_table1$p_value_adj <- p.adjust(test_table1$p_value, method = "BH", n = length(test_table1$p_value))  
  test_table1$sig <- ifelse(test_table1$p_value_adj <= 0.001, "***", 
                               ifelse(test_table1$p_value_adj <= 0.01, "**",
                                      ifelse(test_table1$p_value_adj <= 0.05, "*",
                                             ifelse(test_table1$p_value_adj <= 0.1, ".",""))))
  
  return(test_table1)
}



poe_multiple_test(draw_table_l)
poe_multiple_test(draw_table_m)

### 95% confident interval 
ct = 0
for(m in 1:ncol(draw_table)){
  CI_table <- data.frame(variable = colnames(draw_table)[m],
                         mean = mean(draw_table[,m]),
                         lower_CI = quantile(draw_table[,m],0.025),
                         Upper_CI = quantile(draw_table[,m],0.975))
  ct = ct + 1
  if(ct==1)CI_table1 <- CI_table else CI_table1 <- rbind(CI_table1, CI_table)
}
CI_table1

##### individual WTP ####
effect.gmnl(wtp.rpl.control.cor, effect = "ce")$mean





########## wtp rpl interaction with demographic variable ############

colnames(df.long)

df.long$age <- as.numeric(df.long$Q19)
df.long$gender <- df.long$Q41
df.long$race <- df.long$Q42
df.long$region <- df.long$Q43

df.long.1 <- df.long[is.na(df.long$age) == F & is.na(df.long$gender) ==F &
                       is.na(df.long$race) == F & is.na(df.long$region) == F,]

df.long.1$young <- ifelse(df.long.1$age < median(df.long.1$age), 1,0)
df.long.1$female <- ifelse(df.long.1$gender==2, 1, 0)
df.long.1$white <- ifelse(df.long.1$race == 1, 1,0)
df.long.1$midwest <- ifelse(df.long.1$region == 1, 1,0)
df.long.1$northeast <- ifelse(df.long.1$region == 2, 1,0)
df.long.1$south <- ifelse(df.long.1$region == 3, 1,0)

ml.wtp.1<-mlogit.data(data = df.long.1, 
                    choice ="Choice", 
                    shape = "long", 
                    alt.var = "alternative", 
                    reflevel = 3,
                    id.var = "ID",
                    opposite = 'price',
)

#### control ####
mnl.control <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|0,
                    reflevel = 3,
                    model="mnl",
                    data=ml.wtp.1,
                    subset = ml.wtp.1$treat==0,
                    print.init = 2,
                    print.level =2
)

wtp.control <-coef(mnl.control)/coef(mnl.control)[1]
start.control.cor <- c(wtp.control[1],-0.1,wtp.control[3:4],0.1,0.1,0.1,0.1,0,0)
wtp.rpl.control.cor <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|1,
                            reflevel = 3,
                            model="gmnl",
                            data=ml.wtp.1,
                            subset = ml.wtp.1$treat==0,
                            ranp = c(label = "n", method = "n"),
                            correlation = T,
                            fixed= c(T, rep(F,3), rep(F,3), F,F,T),
                            panel = T, 
                            haltons = NA,
                            start = start.control.cor,
                            print.init = 2,
                            print.level =2,
                            method = "bhhh",
                            rterlim = 500
                            
)
summary(wtp.rpl.control.cor)
save(wtp.rpl.control.cor, file = "wtp.rpl.control.cor.rdata")
load("wtp.rpl.control.cor.rdata")
AIC(wtp.rpl.control.cor)
BIC(wtp.rpl.control.cor)

# age
mnl.control.young <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|0,
                    reflevel = 3,
                    model="mnl",
                    data=ml.wtp.1,
                    subset = ml.wtp.1$treat==0 & ml.wtp.1$young==1,
                    print.init = 2,
                    print.level =2
)

wtp.control.young <-coef(mnl.control.young)/coef(mnl.control.young)[1]
start.control.cor.young <- c(wtp.control.young,0.1,0.1,0.1,0.1,0.1,0)
wtp.rpl.control.cor.young <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|1,
                            reflevel = 3,
                            model="gmnl",
                            data=ml.wtp.1,
                            subset = ml.wtp.1$treat==0& ml.wtp.1$young==1,
                            ranp = c(label = "n", method = "n"),
                            correlation = T,
                            fixed= c(T, rep(F,3), rep(F,3), F,F,T),
                            panel = T, 
                            haltons = NA,
                            start = start.control.cor.young,
                            print.init = 2,
                            print.level =2,
                            method = "bhhh",
                            rterlim = 500
                            
)
summary(wtp.rpl.control.cor.young)
save(wtp.rpl.control.cor.young, file = "wtp.rpl.control.cor.young.rdata")
load("wtp.rpl.control.cor.young.rdata")
AIC(wtp.rpl.control.cor.young)
BIC(wtp.rpl.control.cor.young)

mnl.control.old <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|0,
                          reflevel = 3,
                          model="mnl",
                          data=ml.wtp.1,
                          subset = ml.wtp.1$treat==0 & ml.wtp.1$young==0,
                          print.init = 2,
                          print.level =2
)

wtp.control.old <-coef(mnl.control.old)/coef(mnl.control.old)[1]
start.control.cor.old <- c(wtp.control.old,0.1,0.1,0.1,0.1,0.1,0)
wtp.rpl.control.cor.old <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|1,
                                  reflevel = 3,
                                  model="gmnl",
                                  data=ml.wtp.1,
                                  subset = ml.wtp.1$treat==0& ml.wtp.1$young==0,
                                  ranp = c(label = "n", method = "n"),
                                  correlation = T,
                                  fixed= c(T, rep(F,3), rep(F,3), F,F,T),
                                  panel = T, 
                                  haltons = NA,
                                  start = start.control.cor.old,
                                  print.init = 2,
                                  print.level =2,
                                  method = "bhhh",
                                  rterlim = 500
                                  
)
summary(wtp.rpl.control.cor.old)
save(wtp.rpl.control.cor.old, file = "wtp.rpl.control.cor.old.rdata")
load("wtp.rpl.control.cor.old.rdata")
AIC(wtp.rpl.control.cor.old)
BIC(wtp.rpl.control.cor.old)

## gender
mnl.control.female <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|0,
                          reflevel = 3,
                          model="mnl",
                          data=ml.wtp.1,
                          subset = ml.wtp.1$treat==0 & ml.wtp.1$female==1,
                          print.init = 2,
                          print.level =2
)

wtp.control.female <-coef(mnl.control.female)/coef(mnl.control.female)[1]
start.control.cor.female <- c(wtp.control.female,0.1,0.1,0.1,0.1,0.1,0)
wtp.rpl.control.cor.female <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|1,
                                  reflevel = 3,
                                  model="gmnl",
                                  data=ml.wtp.1,
                                  subset = ml.wtp.1$treat==0& ml.wtp.1$female==1,
                                  ranp = c(label = "n", method = "n"),
                                  correlation = T,
                                  fixed= c(T, rep(F,3), rep(F,3), F,F,T),
                                  panel = T, 
                                  haltons = NA,
                                  start = start.control.cor.female,
                                  print.init = 2,
                                  print.level =2,
                                  method = "bhhh",
                                  rterlim = 500
                                  
)
summary(wtp.rpl.control.cor.female)
save(wtp.rpl.control.cor.female, file = "wtp.rpl.control.cor.female.rdata")
load("wtp.rpl.control.cor.female.rdata")
AIC(wtp.rpl.control.cor.female)
BIC(wtp.rpl.control.cor.female)

mnl.control.male <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|0,
                        reflevel = 3,
                        model="mnl",
                        data=ml.wtp.1,
                        subset = ml.wtp.1$treat==0 & ml.wtp.1$female==0,
                        print.init = 2,
                        print.level =2
)

wtp.control.male <-coef(mnl.control.male)/coef(mnl.control.male)[1]
start.control.cor.male <- c(wtp.control.male,0.1,0.1,0.1,0.1,0.1,0)
wtp.rpl.control.cor.male <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|1,
                                reflevel = 3,
                                model="gmnl",
                                data=ml.wtp.1,
                                subset = ml.wtp.1$treat==0& ml.wtp.1$female==0,
                                ranp = c(label = "n", method = "n"),
                                correlation = T,
                                fixed= c(T, rep(F,3), rep(F,3), F,F,T),
                                panel = T, 
                                haltons = NA,
                                start = start.control.cor.male,
                                print.init = 2,
                                print.level =2,
                                method = "bhhh",
                                rterlim = 500
                                
)
summary(wtp.rpl.control.cor.male)
save(wtp.rpl.control.cor.male, file = "wtp.rpl.control.cor.male.rdata")
load("wtp.rpl.control.cor.male.rdata")
AIC(wtp.rpl.control.cor.male)
BIC(wtp.rpl.control.cor.male)

## race
mnl.control.white <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|0,
                           reflevel = 3,
                           model="mnl",
                           data=ml.wtp.1,
                           subset = ml.wtp.1$treat==0 & ml.wtp.1$white==1,
                           print.init = 2,
                           print.level =2
)

wtp.control.white <-coef(mnl.control.white)/coef(mnl.control.white)[1]
start.control.cor.white <- c(wtp.control.white,0.1,0.1,0.1,0.1,0.1,0)
wtp.rpl.control.cor.white <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|1,
                                   reflevel = 3,
                                   model="gmnl",
                                   data=ml.wtp.1,
                                   subset = ml.wtp.1$treat==0& ml.wtp.1$white==1,
                                   ranp = c(label = "n", method = "n"),
                                   correlation = T,
                                   fixed= c(T, rep(F,3), rep(F,3), F,F,T),
                                   panel = T, 
                                   haltons = NA,
                                   start = start.control.cor.white,
                                   print.init = 2,
                                   print.level =2,
                                   method = "bhhh",
                                   rterlim = 500
                                   
)
summary(wtp.rpl.control.cor.white)
save(wtp.rpl.control.cor.white, file = "wtp.rpl.control.cor.white.rdata")
load("wtp.rpl.control.cor.white.rdata")
AIC(wtp.rpl.control.cor.white)
BIC(wtp.rpl.control.cor.white)

mnl.control.non_white <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|0,
                         reflevel = 3,
                         model="mnl",
                         data=ml.wtp.1,
                         subset = ml.wtp.1$treat==0 & ml.wtp.1$white==0,
                         print.init = 2,
                         print.level =2
)

wtp.control.non_white <-coef(mnl.control.non_white)/coef(mnl.control.non_white)[1]
start.control.cor.non_white <- c(wtp.control.non_white,0.1,0.1,0.1,0.1,0.1,0)
wtp.rpl.control.cor.non_white <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|1,
                                 reflevel = 3,
                                 model="gmnl",
                                 data=ml.wtp.1,
                                 subset = ml.wtp.1$treat==0& ml.wtp.1$white==0,
                                 ranp = c(label = "n", method = "n"),
                                 correlation = T,
                                 fixed= c(T, rep(F,3), rep(F,3), F,F,T),
                                 panel = T, 
                                 haltons = NA,
                                 start = start.control.cor.non_white,
                                 print.init = 2,
                                 print.level =2,
                                 method = "bhhh",
                                 rterlim = 500
                                 
)
summary(wtp.rpl.control.cor.non_white)
save(wtp.rpl.control.cor.non_white, file = "wtp.rpl.control.cor.non_white.rdata")
load("wtp.rpl.control.cor.non_white.rdata")
AIC(wtp.rpl.control.cor.non_white)
BIC(wtp.rpl.control.cor.non_white)

# region
mnl.control.midwest <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|0,
                          reflevel = 3,
                          model="mnl",
                          data=ml.wtp.1,
                          subset = ml.wtp.1$treat==0 & ml.wtp.1$region==1,
                          print.init = 2,
                          print.level =2
)

wtp.control.midwest <-coef(mnl.control.midwest)/coef(mnl.control.midwest)[1]
start.control.cor.midwest <- c(wtp.control.midwest,0.1,0.1,0.1,0.1,0.1,0)
wtp.rpl.control.cor.midwest <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|1,
                                  reflevel = 3,
                                  model="gmnl",
                                  data=ml.wtp.1,
                                  subset = ml.wtp.1$treat==0& ml.wtp.1$region==1,
                                  ranp = c(label = "n", method = "n"),
                                  correlation = T,
                                  fixed= c(T, rep(F,3), rep(F,3), F,F,T),
                                  panel = T, 
                                  haltons = NA,
                                  start = start.control.cor.midwest,
                                  print.init = 2,
                                  print.level =2,
                                  method = "bhhh",
                                  rterlim = 500
                                  
)
summary(wtp.rpl.control.cor.midwest)
save(wtp.rpl.control.cor.midwest, file = "wtp.rpl.control.cor.midwest.rdata")
load("wtp.rpl.control.cor.midwest.rdata")
AIC(wtp.rpl.control.cor.midwest)
BIC(wtp.rpl.control.cor.midwest)

mnl.control.northeast <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|0,
                        reflevel = 3,
                        model="mnl",
                        data=ml.wtp.1,
                        subset = ml.wtp.1$treat==0 & ml.wtp.1$region==2,
                        print.init = 2,
                        print.level =2
)

wtp.control.northeast <-coef(mnl.control.northeast)/coef(mnl.control.northeast)[1]
start.control.cor.northeast <- c(wtp.control.northeast,0.1,0.1,0.1,0.1,0.1,0)
wtp.rpl.control.cor.northeast <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|1,
                                reflevel = 3,
                                model="gmnl",
                                data=ml.wtp.1,
                                subset = ml.wtp.1$treat==0& ml.wtp.1$region==2,
                                ranp = c(label = "n", method = "n"),
                                correlation = T,
                                fixed= c(T, rep(F,3), rep(F,3), F,F,T),
                                panel = T, 
                                haltons = NA,
                                start = start.control.cor.northeast,
                                print.init = 2,
                                print.level =2,
                                method = "bhhh",
                                rterlim = 500
                                
)
summary(wtp.rpl.control.cor.northeast)
save(wtp.rpl.control.cor.northeast, file = "wtp.rpl.control.cor.northeast.rdata")
load("wtp.rpl.control.cor.northeast.rdata")
AIC(wtp.rpl.control.cor.northeast)
BIC(wtp.rpl.control.cor.northeast)


mnl.control.south <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|0,
                            reflevel = 3,
                            model="mnl",
                            data=ml.wtp.1,
                            subset = ml.wtp.1$treat==0 & ml.wtp.1$region==3,
                            print.init = 2,
                            print.level =2
)

wtp.control.south <-coef(mnl.control.south)/coef(mnl.control.south)[1]
start.control.cor.south <- c(wtp.control.south,0.1,0.1,0.1,0.1,0.1,0)
wtp.rpl.control.cor.south <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|1,
                                    reflevel = 3,
                                    model="gmnl",
                                    data=ml.wtp.1,
                                    subset = ml.wtp.1$treat==0& ml.wtp.1$region==3,
                                    ranp = c(label = "n", method = "n"),
                                    correlation = T,
                                    fixed= c(T, rep(F,3), rep(F,3), F,F,T),
                                    panel = T, 
                                    haltons = NA,
                                    start = start.control.cor.south,
                                    print.init = 2,
                                    print.level =2,
                                    method = "bhhh",
                                    rterlim = 500
                                    
)
summary(wtp.rpl.control.cor.south)
save(wtp.rpl.control.cor.south, file = "wtp.rpl.control.cor.south.rdata")
AIC(wtp.rpl.control.cor.south)
BIC(wtp.rpl.control.cor.south)

mnl.control.west <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|0,
                              reflevel = 3,
                              model="mnl",
                              data=ml.wtp.1,
                              subset = ml.wtp.1$treat==0 & ml.wtp.1$region==4,
                              print.init = 2,
                              print.level =2
)

wtp.control.west <-coef(mnl.control.west)/coef(mnl.control.west)[1]
start.control.cor.west <- c(wtp.control.west,0.1,0.1,0.1,0.1,0.1,0)
wtp.rpl.control.cor.west <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|1,
                                      reflevel = 3,
                                      model="gmnl",
                                      data=ml.wtp.1,
                                      subset = ml.wtp.1$treat==0& ml.wtp.1$region==4,
                                      ranp = c(label = "n", method = "n"),
                                      correlation = T,
                                      fixed= c(T, rep(F,3), rep(F,3), F,F,T),
                                      panel = T, 
                                      haltons = NA,
                                      start = start.control.cor.west,
                                      print.init = 2,
                                      print.level =2,
                                      method = "bhhh",
                                      rterlim = 500
                                      
)
summary(wtp.rpl.control.cor.west)
save(wtp.rpl.control.cor.west, file = "wtp.rpl.control.cor.west.rdata")
AIC(wtp.rpl.control.cor.west)
BIC(wtp.rpl.control.cor.west)

#### T1 ####
mnl.t1 <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|0,
                    reflevel = 3,
                    model="mnl",
                    data=ml.wtp.1,
                    subset = ml.wtp.1$treat==1,
                    print.init = 2,
                    print.level =2
)

wtp.t1 <-coef(mnl.t1)/coef(mnl.t1)[1]
start.t1.cor <- c(wtp.t1,0.1,0.1,0.1,0.1,0.1,0)
wtp.rpl.t1.cor <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|1,
                            reflevel = 3,
                            model="gmnl",
                            data=ml.wtp.1,
                            subset = ml.wtp.1$treat==1,
                            ranp = c(label = "n", method = "n"),
                            correlation = T,
                            fixed= c(T, rep(F,3), rep(F,3), F,F,T),
                            panel = T, 
                            haltons = NA,
                            start = start.t1.cor,
                            print.init = 2,
                            print.level =2,
                            method = "bhhh",
                            rterlim = 500
                            
)
summary(wtp.rpl.t1.cor)
save(wtp.rpl.t1.cor, file = "wtp.rpl.t1.cor.rdata")
load("wtp.rpl.t1.cor.rdata")
AIC(wtp.rpl.t1.cor)
BIC(wtp.rpl.t1.cor)

# age
mnl.t1.young <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|0,
                          reflevel = 3,
                          model="mnl",
                          data=ml.wtp.1,
                          subset = ml.wtp.1$treat==1 & ml.wtp.1$young==1,
                          print.init = 2,
                          print.level =2
)

wtp.t1.young <-coef(mnl.t1.young)/coef(mnl.t1.young)[1]
start.t1.cor.young <- c(wtp.t1.young,0.1,0.1,0.1,0.1,0.1,0)
wtp.rpl.t1.cor.young <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|1,
                                  reflevel = 3,
                                  model="gmnl",
                                  data=ml.wtp.1,
                                  subset = ml.wtp.1$treat==1& ml.wtp.1$young==1,
                                  ranp = c(label = "n", method = "n"),
                                  correlation = T,
                                  fixed= c(T, rep(F,3), rep(F,3), F,F,T),
                                  panel = T, 
                                  haltons = NA,
                                  start = start.t1.cor.young,
                                  print.init = 2,
                                  print.level =2,
                                  method = "bhhh",
                                  rterlim = 500
                                  
)
summary(wtp.rpl.t1.cor.young)
save(wtp.rpl.t1.cor.young, file = "wtp.rpl.t1.cor.young.rdata")
load("wtp.rpl.t1.cor.young.rdata")
AIC(wtp.rpl.t1.cor.young)
BIC(wtp.rpl.t1.cor.young)

mnl.t1.old <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|0,
                        reflevel = 3,
                        model="mnl",
                        data=ml.wtp.1,
                        subset = ml.wtp.1$treat==1 & ml.wtp.1$young==0,
                        print.init = 2,
                        print.level =2
)

wtp.t1.old <-coef(mnl.t1.old)/coef(mnl.t1.old)[1]
start.t1.cor.old <- c(wtp.t1.old,0.1,0.1,0.1,0.1,0.1,0)
wtp.rpl.t1.cor.old <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|1,
                                reflevel = 3,
                                model="gmnl",
                                data=ml.wtp.1,
                                subset = ml.wtp.1$treat==1& ml.wtp.1$young==0,
                                ranp = c(label = "n", method = "n"),
                                correlation = T,
                                fixed= c(T, rep(F,3), rep(F,3), F,F,T),
                                panel = T, 
                                haltons = NA,
                                start = start.t1.cor.old,
                                print.init = 2,
                                print.level =2,
                                method = "bhhh",
                                rterlim = 500
                                
)
summary(wtp.rpl.t1.cor.old)
save(wtp.rpl.t1.cor.old, file = "wtp.rpl.t1.cor.old.rdata")
load("wtp.rpl.t1.cor.old.rdata")
AIC(wtp.rpl.t1.cor.old)
BIC(wtp.rpl.t1.cor.old)

## gender
mnl.t1.female <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|0,
                           reflevel = 3,
                           model="mnl",
                           data=ml.wtp.1,
                           subset = ml.wtp.1$treat==1 & ml.wtp.1$female==1,
                           print.init = 2,
                           print.level =2
)

wtp.t1.female <-coef(mnl.t1.female)/coef(mnl.t1.female)[1]
start.t1.cor.female <- c(wtp.t1.female,0.1,0.1,0.1,0.1,0.1,0)
wtp.rpl.t1.cor.female <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|1,
                                   reflevel = 3,
                                   model="gmnl",
                                   data=ml.wtp.1,
                                   subset = ml.wtp.1$treat==1& ml.wtp.1$female==1,
                                   ranp = c(label = "n", method = "n"),
                                   correlation = T,
                                   fixed= c(T, rep(F,3), rep(F,3), F,F,T),
                                   panel = T, 
                                   haltons = NA,
                                   start = start.t1.cor.female,
                                   print.init = 2,
                                   print.level =2,
                                   method = "bhhh",
                                   rterlim = 500
                                   
)
summary(wtp.rpl.t1.cor.female)
save(wtp.rpl.t1.cor.female, file = "wtp.rpl.t1.cor.female.rdata")
load("wtp.rpl.t1.cor.female.rdata")
AIC(wtp.rpl.t1.cor.female)
BIC(wtp.rpl.t1.cor.female)

mnl.t1.male <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|0,
                         reflevel = 3,
                         model="mnl",
                         data=ml.wtp.1,
                         subset = ml.wtp.1$treat==1 & ml.wtp.1$female==0,
                         print.init = 2,
                         print.level =2
)

wtp.t1.male <-coef(mnl.t1.male)/coef(mnl.t1.male)[1]
start.t1.cor.male <- c(wtp.t1.male,0.1,0.1,0.1,0.1,0.1,0)
wtp.rpl.t1.cor.male <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|1,
                                 reflevel = 3,
                                 model="gmnl",
                                 data=ml.wtp.1,
                                 subset = ml.wtp.1$treat==1& ml.wtp.1$female==0,
                                 ranp = c(label = "n", method = "n"),
                                 correlation = T,
                                 fixed= c(T, rep(F,3), rep(F,3), F,F,T),
                                 panel = T, 
                                 haltons = NA,
                                 start = start.t1.cor.male,
                                 print.init = 2,
                                 print.level =2,
                                 method = "bhhh",
                                 rterlim = 500
                                 
)
summary(wtp.rpl.t1.cor.male)
save(wtp.rpl.t1.cor.male, file = "wtp.rpl.t1.cor.male.rdata")
load("wtp.rpl.t1.cor.male.rdata")
AIC(wtp.rpl.t1.cor.male)
BIC(wtp.rpl.t1.cor.male)

## race
mnl.t1.white <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|0,
                          reflevel = 3,
                          model="mnl",
                          data=ml.wtp.1,
                          subset = ml.wtp.1$treat==1 & ml.wtp.1$white==1,
                          print.init = 2,
                          print.level =2
)

wtp.t1.white <-coef(mnl.t1.white)/coef(mnl.t1.white)[1]
start.t1.cor.white <- c(wtp.t1.white,0.1,0.1,0.1,0.1,0.1,0)
wtp.rpl.t1.cor.white <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|1,
                                  reflevel = 3,
                                  model="gmnl",
                                  data=ml.wtp.1,
                                  subset = ml.wtp.1$treat==1& ml.wtp.1$white==1,
                                  ranp = c(label = "n", method = "n"),
                                  correlation = T,
                                  fixed= c(T, rep(F,3), rep(F,3), F,F,T),
                                  panel = T, 
                                  haltons = NA,
                                  start = start.t1.cor.white,
                                  print.init = 2,
                                  print.level =2,
                                  method = "bhhh",
                                  rterlim = 500
                                  
)
summary(wtp.rpl.t1.cor.white)
save(wtp.rpl.t1.cor.white, file = "wtp.rpl.t1.cor.white.rdata")
load("wtp.rpl.t1.cor.white.rdata")
AIC(wtp.rpl.t1.cor.white)
BIC(wtp.rpl.t1.cor.white)

mnl.t1.non_white <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|0,
                              reflevel = 3,
                              model="mnl",
                              data=ml.wtp.1,
                              subset = ml.wtp.1$treat==1 & ml.wtp.1$white==0,
                              print.init = 2,
                              print.level =2
)

wtp.t1.non_white <-coef(mnl.t1.non_white)/coef(mnl.t1.non_white)[1]
start.t1.cor.non_white <- c(wtp.t1.non_white,0.1,0.1,0.1,0.1,0.1,0)
wtp.rpl.t1.cor.non_white <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|1,
                                      reflevel = 3,
                                      model="gmnl",
                                      data=ml.wtp.1,
                                      subset = ml.wtp.1$treat==1& ml.wtp.1$white==0,
                                      ranp = c(label = "n", method = "n"),
                                      correlation = T,
                                      fixed= c(T, rep(F,3), rep(F,3), F,F,T),
                                      panel = T, 
                                      haltons = NA,
                                      start = start.t1.cor.non_white,
                                      print.init = 2,
                                      print.level =2,
                                      method = "bhhh",
                                      rterlim = 500
                                      
)
summary(wtp.rpl.t1.cor.non_white)
save(wtp.rpl.t1.cor.non_white, file = "wtp.rpl.t1.cor.non_white.rdata")
load("wtp.rpl.t1.cor.non_white.rdata")
AIC(wtp.rpl.t1.cor.non_white)
BIC(wtp.rpl.t1.cor.non_white)

# region
mnl.t1.midwest <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|0,
                            reflevel = 3,
                            model="mnl",
                            data=ml.wtp.1,
                            subset = ml.wtp.1$treat==1 & ml.wtp.1$region==1,
                            print.init = 2,
                            print.level =2
)

wtp.t1.midwest <-coef(mnl.t1.midwest)/coef(mnl.t1.midwest)[1]
start.t1.cor.midwest <- c(wtp.t1.midwest,0.1,0.1,0.1,0.1,0.1,0)
wtp.rpl.t1.cor.midwest <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|1,
                                    reflevel = 3,
                                    model="gmnl",
                                    data=ml.wtp.1,
                                    subset = ml.wtp.1$treat==1& ml.wtp.1$region==1,
                                    ranp = c(label = "n", method = "n"),
                                    correlation = T,
                                    fixed= c(T, rep(F,3), rep(F,3), F,F,T),
                                    panel = T, 
                                    haltons = NA,
                                    start = start.t1.cor.midwest,
                                    print.init = 2,
                                    print.level =2,
                                    method = "bhhh",
                                    rterlim = 500
                                    
)
summary(wtp.rpl.t1.cor.midwest)
save(wtp.rpl.t1.cor.midwest, file = "wtp.rpl.t1.cor.midwest.rdata")
AIC(wtp.rpl.t1.cor.midwest)
BIC(wtp.rpl.t1.cor.midwest)

mnl.t1.northeast <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|0,
                              reflevel = 3,
                              model="mnl",
                              data=ml.wtp.1,
                              subset = ml.wtp.1$treat==1 & ml.wtp.1$region==2,
                              print.init = 2,
                              print.level =2
)

wtp.t1.northeast <-coef(mnl.t1.northeast)/coef(mnl.t1.northeast)[1]
start.t1.cor.northeast <- c(wtp.t1.northeast,0.1,0.1,0.1,0.1,0.1,0)
wtp.rpl.t1.cor.northeast <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|1,
                                      reflevel = 3,
                                      model="gmnl",
                                      data=ml.wtp.1,
                                      subset = ml.wtp.1$treat==1& ml.wtp.1$region==2,
                                      ranp = c(label = "n", method = "n"),
                                      correlation = T,
                                      fixed= c(T, rep(F,3), rep(F,3), F,F,T),
                                      panel = T, 
                                      haltons = NA,
                                      start = start.t1.cor.northeast,
                                      print.init = 2,
                                      print.level =2,
                                      method = "bhhh",
                                      rterlim = 500
                                      
)
summary(wtp.rpl.t1.cor.northeast)
save(wtp.rpl.t1.cor.northeast, file = "wtp.rpl.t1.cor.northeast.rdata")
AIC(wtp.rpl.t1.cor.northeast)
BIC(wtp.rpl.t1.cor.northeast)


mnl.t1.south <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|0,
                          reflevel = 3,
                          model="mnl",
                          data=ml.wtp.1,
                          subset = ml.wtp.1$treat==1 & ml.wtp.1$region==3,
                          print.init = 2,
                          print.level =2
)

wtp.t1.south <-coef(mnl.t1.south)/coef(mnl.t1.south)[1]
start.t1.cor.south <- c(wtp.t1.south,0.1,0.1,0.1,0.1,0.1,0)
wtp.rpl.t1.cor.south <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|1,
                                  reflevel = 3,
                                  model="gmnl",
                                  data=ml.wtp.1,
                                  subset = ml.wtp.1$treat==1& ml.wtp.1$region==3,
                                  ranp = c(label = "n", method = "n"),
                                  correlation = T,
                                  fixed= c(T, rep(F,3), rep(F,3), F,F,T),
                                  panel = T, 
                                  haltons = NA,
                                  start = start.t1.cor.south,
                                  print.init = 2,
                                  print.level =2,
                                  method = "bhhh",
                                  rterlim = 500
                                  
)
summary(wtp.rpl.t1.cor.south)
save(wtp.rpl.t1.cor.south, file = "wtp.rpl.t1.cor.south.rdata")
AIC(wtp.rpl.t1.cor.south)
BIC(wtp.rpl.t1.cor.south)

mnl.t1.west <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|0,
                         reflevel = 3,
                         model="mnl",
                         data=ml.wtp.1,
                         subset = ml.wtp.1$treat==1 & ml.wtp.1$region==4,
                         print.init = 2,
                         print.level =2
)

wtp.t1.west <-coef(mnl.t1.west)/coef(mnl.t1.west)[1]
start.t1.cor.west <- c(wtp.t1.west,0.1,0.1,0.1,0.1,0.1,0)
wtp.rpl.t1.cor.west <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|1,
                                 reflevel = 3,
                                 model="gmnl",
                                 data=ml.wtp.1,
                                 subset = ml.wtp.1$treat==1& ml.wtp.1$region==4,
                                 ranp = c(label = "n", method = "n"),
                                 correlation = T,
                                 fixed= c(T, rep(F,3), rep(F,3), F,F,T),
                                 panel = T, 
                                 haltons = NA,
                                 start = start.t1.cor.west,
                                 print.init = 2,
                                 print.level =2,
                                 method = "bhhh",
                                 rterlim = 500
                                 
)
summary(wtp.rpl.t1.cor.west)
save(wtp.rpl.t1.cor.west, file = "wtp.rpl.t1.cor.west.rdata")
AIC(wtp.rpl.t1.cor.west)
BIC(wtp.rpl.t1.cor.west)

#### T2 ####
mnl.t2 <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|0,
                    reflevel = 3,
                    model="mnl",
                    data=ml.wtp.1,
                    subset = ml.wtp.1$treat==2,
                    print.init = 2,
                    print.level =2
)

wtp.t2 <-coef(mnl.t2)/coef(mnl.t2)[1]
start.t2.cor <- c(wtp.t2,0.1,0.1,0.1,0.1,0.1,0)
wtp.rpl.t2.cor <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|1,
                            reflevel = 3,
                            model="gmnl",
                            data=ml.wtp.1,
                            subset = ml.wtp.1$treat==2,
                            ranp = c(label = "n", method = "n"),
                            correlation = T,
                            fixed= c(T, rep(F,3), rep(F,3), F,F,T),
                            panel = T, 
                            haltons = NA,
                            start = start.t2.cor,
                            print.init = 2,
                            print.level =2,
                            method = "bhhh",
                            rterlim = 500
                            
)
summary(wtp.rpl.t2.cor)
save(wtp.rpl.t2.cor, file = "wtp.rpl.t2.cor.rdata")
load("wtp.rpl.t2.cor.rdata")
AIC(wtp.rpl.t2.cor)
BIC(wtp.rpl.t2.cor)

# age
mnl.t2.young <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|0,
                          reflevel = 3,
                          model="mnl",
                          data=ml.wtp.1,
                          subset = ml.wtp.1$treat==2 & ml.wtp.1$young==1,
                          print.init = 2,
                          print.level =2
)

wtp.t2.young <-coef(mnl.t2.young)/coef(mnl.t2.young)[1]
start.t2.cor.young <- c(wtp.t2.young,0.1,0.1,0.1,0.1,0.1,0)
wtp.rpl.t2.cor.young <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|1,
                                  reflevel = 3,
                                  model="gmnl",
                                  data=ml.wtp.1,
                                  subset = ml.wtp.1$treat==2& ml.wtp.1$young==1,
                                  ranp = c(label = "n", method = "n"),
                                  correlation = T,
                                  fixed= c(T, rep(F,3), rep(F,3), F,F,T),
                                  panel = T, 
                                  haltons = NA,
                                  start = start.t2.cor.young,
                                  print.init = 2,
                                  print.level =2,
                                  method = "bhhh",
                                  rterlim = 500
                                  
)
summary(wtp.rpl.t2.cor.young)
save(wtp.rpl.t2.cor.young, file = "wtp.rpl.t2.cor.young.rdata")
load("wtp.rpl.t2.cor.young.rdata")
AIC(wtp.rpl.t2.cor.young)
BIC(wtp.rpl.t2.cor.young)

mnl.t2.old <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|0,
                        reflevel = 3,
                        model="mnl",
                        data=ml.wtp.1,
                        subset = ml.wtp.1$treat==2 & ml.wtp.1$young==0,
                        print.init = 2,
                        print.level =2
)

wtp.t2.old <-coef(mnl.t2.old)/coef(mnl.t2.old)[1]
start.t2.cor.old <- c(wtp.t2.old,0.1,0.1,0.1,0.1,0.1,0)
wtp.rpl.t2.cor.old <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|1,
                                reflevel = 3,
                                model="gmnl",
                                data=ml.wtp.1,
                                subset = ml.wtp.1$treat==2& ml.wtp.1$young==0,
                                ranp = c(label = "n", method = "n"),
                                correlation = T,
                                fixed= c(T, rep(F,3), rep(F,3), F,F,T),
                                panel = T, 
                                haltons = NA,
                                start = start.t2.cor.old,
                                print.init = 2,
                                print.level =2,
                                method = "bhhh",
                                rterlim = 500
                                
)
summary(wtp.rpl.t2.cor.old)
save(wtp.rpl.t2.cor.old, file = "wtp.rpl.t2.cor.old.rdata")
load("wtp.rpl.t2.cor.old.rdata")
AIC(wtp.rpl.t2.cor.old)
BIC(wtp.rpl.t2.cor.old)

## gender
mnl.t2.female <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|0,
                           reflevel = 3,
                           model="mnl",
                           data=ml.wtp.1,
                           subset = ml.wtp.1$treat==2 & ml.wtp.1$female==1,
                           print.init = 2,
                           print.level =2
)

wtp.t2.female <-coef(mnl.t2.female)/coef(mnl.t2.female)[1]
start.t2.cor.female <- c(wtp.t2.female,0.1,0.1,0.1,0.1,0.1,0)
wtp.rpl.t2.cor.female <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|1,
                                   reflevel = 3,
                                   model="gmnl",
                                   data=ml.wtp.1,
                                   subset = ml.wtp.1$treat==2& ml.wtp.1$female==1,
                                   ranp = c(label = "n", method = "n"),
                                   correlation = T,
                                   fixed= c(T, rep(F,3), rep(F,3), F,F,T),
                                   panel = T, 
                                   haltons = NA,
                                   start = start.t2.cor.female,
                                   print.init = 2,
                                   print.level =2,
                                   method = "bhhh",
                                   rterlim = 500
                                   
)
summary(wtp.rpl.t2.cor.female)
save(wtp.rpl.t2.cor.female, file = "wtp.rpl.t2.cor.female.rdata")
load("wtp.rpl.t2.cor.female.rdata")
AIC(wtp.rpl.t2.cor.female)
BIC(wtp.rpl.t2.cor.female)

mnl.t2.male <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|0,
                         reflevel = 3,
                         model="mnl",
                         data=ml.wtp.1,
                         subset = ml.wtp.1$treat==2 & ml.wtp.1$female==0,
                         print.init = 2,
                         print.level =2
)

wtp.t2.male <-coef(mnl.t2.male)/coef(mnl.t2.male)[1]
start.t2.cor.male <- c(wtp.t2.male,0.1,0.1,0.1,0.1,0.1,0)
wtp.rpl.t2.cor.male <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|1,
                                 reflevel = 3,
                                 model="gmnl",
                                 data=ml.wtp.1,
                                 subset = ml.wtp.1$treat==2& ml.wtp.1$female==0,
                                 ranp = c(label = "n", method = "n"),
                                 correlation = T,
                                 fixed= c(T, rep(F,3), rep(F,3), F,F,T),
                                 panel = T, 
                                 haltons = NA,
                                 start = start.t2.cor.male,
                                 print.init = 2,
                                 print.level =2,
                                 method = "bhhh",
                                 rterlim = 500
                                 
)
summary(wtp.rpl.t2.cor.male)
save(wtp.rpl.t2.cor.male, file = "wtp.rpl.t2.cor.male.rdata")
load("wtp.rpl.t2.cor.male.rdata")
AIC(wtp.rpl.t2.cor.male)
BIC(wtp.rpl.t2.cor.male)

## race
mnl.t2.white <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|0,
                          reflevel = 3,
                          model="mnl",
                          data=ml.wtp.1,
                          subset = ml.wtp.1$treat==2 & ml.wtp.1$white==1,
                          print.init = 2,
                          print.level =2
)

wtp.t2.white <-coef(mnl.t2.white)/coef(mnl.t2.white)[1]
start.t2.cor.white <- c(wtp.t2.white,0.1,0.1,0.1,0.1,0.1,0)
wtp.rpl.t2.cor.white <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|1,
                                  reflevel = 3,
                                  model="gmnl",
                                  data=ml.wtp.1,
                                  subset = ml.wtp.1$treat==2& ml.wtp.1$white==1,
                                  ranp = c(label = "n", method = "n"),
                                  correlation = T,
                                  fixed= c(T, rep(F,3), rep(F,3), F,F,T),
                                  panel = T, 
                                  haltons = NA,
                                  start = start.t2.cor.white,
                                  print.init = 2,
                                  print.level =2,
                                  method = "bhhh",
                                  rterlim = 500
                                  
)
summary(wtp.rpl.t2.cor.white)
save(wtp.rpl.t2.cor.white, file = "wtp.rpl.t2.cor.white.rdata")
load("wtp.rpl.t2.cor.white.rdata")
AIC(wtp.rpl.t2.cor.white)
BIC(wtp.rpl.t2.cor.white)

mnl.t2.non_white <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|0,
                              reflevel = 3,
                              model="mnl",
                              data=ml.wtp.1,
                              subset = ml.wtp.1$treat==2 & ml.wtp.1$white==0,
                              print.init = 2,
                              print.level =2
)

wtp.t2.non_white <-coef(mnl.t2.non_white)/coef(mnl.t2.non_white)[1]
start.t2.cor.non_white <- c(wtp.t2.non_white,0.1,0.1,0.1,0.1,0.1,0)
wtp.rpl.t2.cor.non_white <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|1,
                                      reflevel = 3,
                                      model="gmnl",
                                      data=ml.wtp.1,
                                      subset = ml.wtp.1$treat==2& ml.wtp.1$white==0,
                                      ranp = c(label = "n", method = "n"),
                                      correlation = T,
                                      fixed= c(T, rep(F,3), rep(F,3), F,F,T),
                                      panel = T, 
                                      haltons = NA,
                                      start = start.t2.cor.non_white,
                                      print.init = 2,
                                      print.level =2,
                                      method = "bhhh",
                                      rterlim = 500
                                      
)
summary(wtp.rpl.t2.cor.non_white)
save(wtp.rpl.t2.cor.non_white, file = "wtp.rpl.t2.cor.non_white.rdata")
load("wtp.rpl.t2.cor.non_white.rdata")
AIC(wtp.rpl.t2.cor.non_white)
BIC(wtp.rpl.t2.cor.non_white)

# region
mnl.t2.midwest <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|0,
                            reflevel = 3,
                            model="mnl",
                            data=ml.wtp.1,
                            subset = ml.wtp.1$treat==2 & ml.wtp.1$region==1,
                            print.init = 2,
                            print.level =2
)

wtp.t2.midwest <-coef(mnl.t2.midwest)/coef(mnl.t2.midwest)[1]
start.t2.cor.midwest <- c(wtp.t2.midwest,0.1,0.1,0.1,0.1,0.1,0)
wtp.rpl.t2.cor.midwest <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|1,
                                    reflevel = 3,
                                    model="gmnl",
                                    data=ml.wtp.1,
                                    subset = ml.wtp.1$treat==2& ml.wtp.1$region==1,
                                    ranp = c(label = "n", method = "n"),
                                    correlation = T,
                                    fixed= c(T, rep(F,3), rep(F,3), F,F,T),
                                    panel = T, 
                                    haltons = NA,
                                    start = start.t2.cor.midwest,
                                    print.init = 2,
                                    print.level =2,
                                    method = "bhhh",
                                    rterlim = 500
                                    
)
summary(wtp.rpl.t2.cor.midwest)
save(wtp.rpl.t2.cor.midwest, file = "wtp.rpl.t2.cor.midwest.rdata")
load("wtp.rpl.t2.cor.midwest.rdata")
AIC(wtp.rpl.t2.cor.midwest)
BIC(wtp.rpl.t2.cor.midwest)

mnl.t2.northeast <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|0,
                              reflevel = 3,
                              model="mnl",
                              data=ml.wtp.1,
                              subset = ml.wtp.1$treat==2 & ml.wtp.1$region==2,
                              print.init = 2,
                              print.level =2
)

wtp.t2.northeast <-coef(mnl.t2.northeast)/coef(mnl.t2.northeast)[1]
start.t2.cor.northeast <- c(wtp.t2.northeast,0.1,0.1,0.1,0.1,0,0)
wtp.rpl.t2.cor.northeast <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|1,
                                      reflevel = 3,
                                      model="gmnl",
                                      data=ml.wtp.1,
                                      subset = ml.wtp.1$treat==2& ml.wtp.1$region==2,
                                      ranp = c(label = "n", method = "n"),
                                      correlation = T,
                                      fixed= c(T, rep(F,3), rep(F,3), F,F,T),
                                      panel = T, 
                                      haltons = NA,
                                      start = start.t2.cor.northeast,
                                      print.init = 2,
                                      print.level =2,
                                      method = "bhhh",
                                      rterlim = 500
                                      
)
summary(wtp.rpl.t2.cor.northeast)
save(wtp.rpl.t2.cor.northeast, file = "wtp.rpl.t2.cor.northeast.rdata")
load("wtp.rpl.t2.cor.northeast.rdata")
AIC(wtp.rpl.t2.cor.northeast)
BIC(wtp.rpl.t2.cor.northeast)


mnl.t2.south <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|0,
                          reflevel = 3,
                          model="mnl",
                          data=ml.wtp.1,
                          subset = ml.wtp.1$treat==2 & ml.wtp.1$region==3,
                          print.init = 2,
                          print.level =2
)

wtp.t2.south <-coef(mnl.t2.south)/coef(mnl.t2.south)[1]
start.t2.cor.south <- c(wtp.t2.south,0.1,0.1,0.1,0.1,0,0)
wtp.rpl.t2.cor.south <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|1,
                                  reflevel = 3,
                                  model="gmnl",
                                  data=ml.wtp.1,
                                  subset = ml.wtp.1$treat==2& ml.wtp.1$region==3,
                                  ranp = c(label = "n", method = "n"),
                                  correlation = T,
                                  fixed= c(T, rep(F,3), rep(F,3), F,F,T),
                                  panel = T, 
                                  haltons = NA,
                                  start = start.t2.cor.south,
                                  print.init = 2,
                                  print.level =2,
                                  method = "bhhh",
                                  rterlim = 500
                                  
)
summary(wtp.rpl.t2.cor.south)
save(wtp.rpl.t2.cor.south, file = "wtp.rpl.t2.cor.south.rdata")
load("wtp.rpl.t2.cor.south.rdata")
AIC(wtp.rpl.t2.cor.south)
BIC(wtp.rpl.t2.cor.south)

mnl.t2.west <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|0,
                         reflevel = 3,
                         model="mnl",
                         data=ml.wtp.1,
                         subset = ml.wtp.1$treat==2 & ml.wtp.1$region==4,
                         print.init = 2,
                         print.level =2
)

wtp.t2.west <-coef(mnl.t2.west)/coef(mnl.t2.west)[1]
start.t2.cor.west <- c(wtp.t2.west,0.1,0.1,0.1,0.1,0,0)
wtp.rpl.t2.cor.west <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|1,
                                 reflevel = 3,
                                 model="gmnl",
                                 data=ml.wtp.1,
                                 subset = ml.wtp.1$treat==2& ml.wtp.1$region==4,
                                 ranp = c(label = "n", method = "n"),
                                 correlation = T,
                                 fixed= c(T, rep(F,3), rep(F,3), F,F,T),
                                 panel = T, 
                                 haltons = NA,
                                 start = start.t2.cor.west,
                                 print.init = 2,
                                 print.level =2,
                                 method = "bhhh",
                                 rterlim = 500
                                 
)
summary(wtp.rpl.t2.cor.west)
save(wtp.rpl.t2.cor.west, file = "wtp.rpl.t2.cor.west.rdata")
load("wtp.rpl.t2.cor.west.rdata")
AIC(wtp.rpl.t2.cor.west)
BIC(wtp.rpl.t2.cor.west)

#### T3 ####
mnl.t3 <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|0,
                    reflevel = 3,
                    model="mnl",
                    data=ml.wtp.1,
                    subset = ml.wtp.1$treat==3,
                    print.init = 2,
                    print.level =2
)

wtp.t3 <-coef(mnl.t3)/coef(mnl.t3)[1]
start.t3.cor <- c(wtp.t3,0.1,0.1,0.1,0.1,0.1,0)
wtp.rpl.t3.cor <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|1,
                            reflevel = 3,
                            model="gmnl",
                            data=ml.wtp.1,
                            subset = ml.wtp.1$treat==3,
                            ranp = c(label = "n", method = "n"),
                            correlation = T,
                            fixed= c(T, rep(F,3), rep(F,3), F,F,T),
                            panel = T, 
                            haltons = NA,
                            start = start.t3.cor,
                            print.init = 2,
                            print.level =2,
                            method = "bhhh",
                            rterlim = 500
                            
)
summary(wtp.rpl.t3.cor)
save(wtp.rpl.t3.cor, file = "wtp.rpl.t3.cor.rdata")
load("wtp.rpl.t3.cor.rdata")
AIC(wtp.rpl.t3.cor)
BIC(wtp.rpl.t3.cor)

# age
mnl.t3.young <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|0,
                          reflevel = 3,
                          model="mnl",
                          data=ml.wtp.1,
                          subset = ml.wtp.1$treat==3 & ml.wtp.1$young==1,
                          print.init = 2,
                          print.level =2
)

wtp.t3.young <-coef(mnl.t3.young)/coef(mnl.t3.young)[1]
start.t3.cor.young <- c(wtp.t3.young,0.1,0.1,0.1,0.1,0.1,0)
wtp.rpl.t3.cor.young <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|1,
                                  reflevel = 3,
                                  model="gmnl",
                                  data=ml.wtp.1,
                                  subset = ml.wtp.1$treat==3& ml.wtp.1$young==1,
                                  ranp = c(label = "n", method = "n"),
                                  correlation = T,
                                  fixed= c(T, rep(F,3), rep(F,3), F,F,T),
                                  panel = T, 
                                  haltons = NA,
                                  start = start.t3.cor.young,
                                  print.init = 2,
                                  print.level =2,
                                  method = "bhhh",
                                  rterlim = 500
                                  
)
summary(wtp.rpl.t3.cor.young)
save(wtp.rpl.t3.cor.young, file = "wtp.rpl.t3.cor.young.rdata")
load("wtp.rpl.t3.cor.young.rdata")
AIC(wtp.rpl.t3.cor.young)
BIC(wtp.rpl.t3.cor.young)

mnl.t3.old <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|0,
                        reflevel = 3,
                        model="mnl",
                        data=ml.wtp.1,
                        subset = ml.wtp.1$treat==3 & ml.wtp.1$young==0,
                        print.init = 2,
                        print.level =2
)

wtp.t3.old <-coef(mnl.t3.old)/coef(mnl.t3.old)[1]
start.t3.cor.old <- c(wtp.t3.old,0.1,0.1,0.1,0.1,0.1,0)
wtp.rpl.t3.cor.old <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|1,
                                reflevel = 3,
                                model="gmnl",
                                data=ml.wtp.1,
                                subset = ml.wtp.1$treat==3& ml.wtp.1$young==0,
                                ranp = c(label = "n", method = "n"),
                                correlation = T,
                                fixed= c(T, rep(F,3), rep(F,3), F,F,T),
                                panel = T, 
                                haltons = NA,
                                start = start.t3.cor.old,
                                print.init = 2,
                                print.level =2,
                                method = "bhhh",
                                rterlim = 500
                                
)
summary(wtp.rpl.t3.cor.old)
save(wtp.rpl.t3.cor.old, file = "wtp.rpl.t3.cor.old.rdata")
load("wtp.rpl.t3.cor.old.rdata")
AIC(wtp.rpl.t3.cor.old)
BIC(wtp.rpl.t3.cor.old)

## gender
mnl.t3.female <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|0,
                           reflevel = 3,
                           model="mnl",
                           data=ml.wtp.1,
                           subset = ml.wtp.1$treat==3 & ml.wtp.1$female==1,
                           print.init = 2,
                           print.level =2
)

wtp.t3.female <-coef(mnl.t3.female)/coef(mnl.t3.female)[1]
start.t3.cor.female <- c(wtp.t3.female,0.1,0.1,0.1,0.1,0.1,0)
wtp.rpl.t3.cor.female <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|1,
                                   reflevel = 3,
                                   model="gmnl",
                                   data=ml.wtp.1,
                                   subset = ml.wtp.1$treat==3& ml.wtp.1$female==1,
                                   ranp = c(label = "n", method = "n"),
                                   correlation = T,
                                   fixed= c(T, rep(F,3), rep(F,3), F,F,T),
                                   panel = T, 
                                   haltons = NA,
                                   start = start.t3.cor.female,
                                   print.init = 2,
                                   print.level =2,
                                   method = "bhhh",
                                   rterlim = 500
                                   
)
summary(wtp.rpl.t3.cor.female)
save(wtp.rpl.t3.cor.female, file = "wtp.rpl.t3.cor.female.rdata")
load("wtp.rpl.t3.cor.female.rdata")
AIC(wtp.rpl.t3.cor.female)
BIC(wtp.rpl.t3.cor.female)

mnl.t3.male <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|0,
                         reflevel = 3,
                         model="mnl",
                         data=ml.wtp.1,
                         subset = ml.wtp.1$treat==3 & ml.wtp.1$female==0,
                         print.init = 2,
                         print.level =2
)

wtp.t3.male <-coef(mnl.t3.male)/coef(mnl.t3.male)[1]
start.t3.cor.male <- c(wtp.t3.male,0.1,0.1,0.1,0.1,0.1,0)
wtp.rpl.t3.cor.male <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|1,
                                 reflevel = 3,
                                 model="gmnl",
                                 data=ml.wtp.1,
                                 subset = ml.wtp.1$treat==3& ml.wtp.1$female==0,
                                 ranp = c(label = "n", method = "n"),
                                 correlation = T,
                                 fixed= c(T, rep(F,3), rep(F,3), F,F,T),
                                 panel = T, 
                                 haltons = NA,
                                 start = start.t3.cor.male,
                                 print.init = 2,
                                 print.level =2,
                                 method = "bhhh",
                                 rterlim = 500
                                 
)
summary(wtp.rpl.t3.cor.male)
save(wtp.rpl.t3.cor.male, file = "wtp.rpl.t3.cor.male.rdata")
load("wtp.rpl.t3.cor.male.rdata")
AIC(wtp.rpl.t3.cor.male)
BIC(wtp.rpl.t3.cor.male)

## race
mnl.t3.white <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|0,
                          reflevel = 3,
                          model="mnl",
                          data=ml.wtp.1,
                          subset = ml.wtp.1$treat==3 & ml.wtp.1$white==1,
                          print.init = 2,
                          print.level =2
)

wtp.t3.white <-coef(mnl.t3.white)/coef(mnl.t3.white)[1]
start.t3.cor.white <- c(wtp.t3.white,0.1,0.1,0.1,0.1,0.1,0)
wtp.rpl.t3.cor.white <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|1,
                                  reflevel = 3,
                                  model="gmnl",
                                  data=ml.wtp.1,
                                  subset = ml.wtp.1$treat==3& ml.wtp.1$white==1,
                                  ranp = c(label = "n", method = "n"),
                                  correlation = T,
                                  fixed= c(T, rep(F,3), rep(F,3), F,F,T),
                                  panel = T, 
                                  haltons = NA,
                                  start = start.t3.cor.white,
                                  print.init = 2,
                                  print.level =2,
                                  method = "bhhh",
                                  rterlim = 500
                                  
)
summary(wtp.rpl.t3.cor.white)
save(wtp.rpl.t3.cor.white, file = "wtp.rpl.t3.cor.white.rdata")
load("wtp.rpl.t3.cor.white.rdata")
AIC(wtp.rpl.t3.cor.white)
BIC(wtp.rpl.t3.cor.white)

mnl.t3.non_white <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|0,
                              reflevel = 3,
                              model="mnl",
                              data=ml.wtp.1,
                              subset = ml.wtp.1$treat==3 & ml.wtp.1$white==0,
                              print.init = 2,
                              print.level =2
)

wtp.t3.non_white <-coef(mnl.t3.non_white)/coef(mnl.t3.non_white)[1]
start.t3.cor.non_white <- c(wtp.t3.non_white,0.1,0.1,0.1,0.1,0.1,0)
wtp.rpl.t3.cor.non_white <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|1,
                                      reflevel = 3,
                                      model="gmnl",
                                      data=ml.wtp.1,
                                      subset = ml.wtp.1$treat==3& ml.wtp.1$white==0,
                                      ranp = c(label = "n", method = "n"),
                                      correlation = T,
                                      fixed= c(T, rep(F,3), rep(F,3), F,F,T),
                                      panel = T, 
                                      haltons = NA,
                                      start = start.t3.cor.non_white,
                                      print.init = 2,
                                      print.level =2,
                                      method = "bhhh",
                                      rterlim = 500
                                      
)
summary(wtp.rpl.t3.cor.non_white)
save(wtp.rpl.t3.cor.non_white, file = "wtp.rpl.t3.cor.non_white.rdata")
load("wtp.rpl.t3.cor.non_white.rdata")
AIC(wtp.rpl.t3.cor.non_white)
BIC(wtp.rpl.t3.cor.non_white)

# region
mnl.t3.midwest <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|0,
                            reflevel = 3,
                            model="mnl",
                            data=ml.wtp.1,
                            subset = ml.wtp.1$treat==3 & ml.wtp.1$region==1,
                            print.init = 2,
                            print.level =2
)

wtp.t3.midwest <-coef(mnl.t3.midwest)/coef(mnl.t3.midwest)[1]
start.t3.cor.midwest <- c(wtp.t3.midwest,0.1,0.1,0.1,0.1,0.1,0)
wtp.rpl.t3.cor.midwest <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|1,
                                    reflevel = 3,
                                    model="gmnl",
                                    data=ml.wtp.1,
                                    subset = ml.wtp.1$treat==3& ml.wtp.1$region==1,
                                    ranp = c(label = "n", method = "n"),
                                    correlation = T,
                                    fixed= c(T, rep(F,3), rep(F,3), F,F,T),
                                    panel = T, 
                                    haltons = NA,
                                    start = start.t3.cor.midwest,
                                    print.init = 2,
                                    print.level =2,
                                    method = "bhhh",
                                    rterlim = 500
                                    
)
summary(wtp.rpl.t3.cor.midwest)
save(wtp.rpl.t3.cor.midwest, file = "wtp.rpl.t3.cor.midwest.rdata")
AIC(wtp.rpl.t3.cor.midwest)
BIC(wtp.rpl.t3.cor.midwest)

mnl.t3.northeast <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|0,
                              reflevel = 3,
                              model="mnl",
                              data=ml.wtp.1,
                              subset = ml.wtp.1$treat==3 & ml.wtp.1$region==2,
                              print.init = 2,
                              print.level =2
)

wtp.t3.northeast <-coef(mnl.t3.northeast)/coef(mnl.t3.northeast)[1]
start.t3.cor.northeast <- c(wtp.t3.northeast,0.1,0.1,0.1,0.1,0.1,0)
wtp.rpl.t3.cor.northeast <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|1,
                                      reflevel = 3,
                                      model="gmnl",
                                      data=ml.wtp.1,
                                      subset = ml.wtp.1$treat==3& ml.wtp.1$region==2,
                                      ranp = c(label = "n", method = "n"),
                                      correlation = T,
                                      fixed= c(T, rep(F,3), rep(F,3), F,F,T),
                                      panel = T, 
                                      haltons = NA,
                                      start = start.t3.cor.northeast,
                                      print.init = 2,
                                      print.level =2,
                                      method = "bhhh",
                                      rterlim = 500
                                      
)
summary(wtp.rpl.t3.cor.northeast)
save(wtp.rpl.t3.cor.northeast, file = "wtp.rpl.t3.cor.northeast.rdata")
AIC(wtp.rpl.t3.cor.northeast)
BIC(wtp.rpl.t3.cor.northeast)


mnl.t3.south <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|0,
                          reflevel = 3,
                          model="mnl",
                          data=ml.wtp.1,
                          subset = ml.wtp.1$treat==3 & ml.wtp.1$region==3,
                          print.init = 2,
                          print.level =2
)

wtp.t3.south <-coef(mnl.t3.south)/coef(mnl.t3.south)[1]
start.t3.cor.south <- c(wtp.t3.south,0.1,0.1,0.1,0.1,0.1,0)
wtp.rpl.t3.cor.south <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|1,
                                  reflevel = 3,
                                  model="gmnl",
                                  data=ml.wtp.1,
                                  subset = ml.wtp.1$treat==3& ml.wtp.1$region==3,
                                  ranp = c(label = "n", method = "n"),
                                  correlation = T,
                                  fixed= c(T, rep(F,3), rep(F,3), F,F,T),
                                  panel = T, 
                                  haltons = NA,
                                  start = start.t3.cor.south,
                                  print.init = 2,
                                  print.level =2,
                                  method = "bhhh",
                                  rterlim = 500
                                  
)
summary(wtp.rpl.t3.cor.south)
save(wtp.rpl.t3.cor.south, file = "wtp.rpl.t3.cor.south.rdata")
AIC(wtp.rpl.t3.cor.south)
BIC(wtp.rpl.t3.cor.south)

mnl.t3.west <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|0,
                         reflevel = 3,
                         model="mnl",
                         data=ml.wtp.1,
                         subset = ml.wtp.1$treat==3 & ml.wtp.1$region==4,
                         print.init = 2,
                         print.level =2
)

wtp.t3.west <-coef(mnl.t3.west)/coef(mnl.t3.west)[1]
start.t3.cor.west <- c(wtp.t3.west,0.1,0.1,0.1,0.1,0.1,0)
wtp.rpl.t3.cor.west <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|1,
                                 reflevel = 3,
                                 model="gmnl",
                                 data=ml.wtp.1,
                                 subset = ml.wtp.1$treat==3& ml.wtp.1$region==4,
                                 ranp = c(label = "n", method = "n"),
                                 correlation = T,
                                 fixed= c(T, rep(F,3), rep(F,3), F,F,T),
                                 panel = T, 
                                 haltons = NA,
                                 start = start.t3.cor.west,
                                 print.init = 2,
                                 print.level =2,
                                 method = "bhhh",
                                 rterlim = 500
                                 
)
summary(wtp.rpl.t3.cor.west)
save(wtp.rpl.t3.cor.west, file = "wtp.rpl.t3.cor.west.rdata")
AIC(wtp.rpl.t3.cor.west)
BIC(wtp.rpl.t3.cor.west)

#### T4 ####
mnl.t4 <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|0,
                    reflevel = 3,
                    model="mnl",
                    data=ml.wtp.1,
                    subset = ml.wtp.1$treat==4,
                    print.init = 2,
                    print.level =2
)

wtp.t4 <-coef(mnl.t4)/coef(mnl.t4)[1]
start.t4.cor <- c(wtp.t4,0.1,0.1,0.1,0.1,0.1,0)
wtp.rpl.t4.cor <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|1,
                            reflevel = 3,
                            model="gmnl",
                            data=ml.wtp.1,
                            subset = ml.wtp.1$treat==4,
                            ranp = c(label = "n", method = "n"),
                            correlation = T,
                            fixed= c(T, rep(F,3), rep(F,3), F,F,T),
                            panel = T, 
                            haltons = NA,
                            start = start.t4.cor,
                            print.init = 2,
                            print.level =2,
                            method = "bhhh",
                            rterlim = 500
                            
)
summary(wtp.rpl.t4.cor)
save(wtp.rpl.t4.cor, file = "wtp.rpl.t4.cor.rdata")
load("wtp.rpl.t4.cor.rdata")
AIC(wtp.rpl.t4.cor)
BIC(wtp.rpl.t4.cor)

# age
mnl.t4.young <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|0,
                          reflevel = 3,
                          model="mnl",
                          data=ml.wtp.1,
                          subset = ml.wtp.1$treat==4 & ml.wtp.1$young==1,
                          print.init = 2,
                          print.level =2
)

wtp.t4.young <-coef(mnl.t4.young)/coef(mnl.t4.young)[1]
start.t4.cor.young <- c(wtp.t4.young,0.1,0.1,0.1,0.1,0.1,0)
wtp.rpl.t4.cor.young <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|1,
                                  reflevel = 3,
                                  model="gmnl",
                                  data=ml.wtp.1,
                                  subset = ml.wtp.1$treat==4& ml.wtp.1$young==1,
                                  ranp = c(label = "n", method = "n"),
                                  correlation = T,
                                  fixed= c(T, rep(F,3), rep(F,3), F,F,T),
                                  panel = T, 
                                  haltons = NA,
                                  start = start.t4.cor.young,
                                  print.init = 2,
                                  print.level =2,
                                  method = "bhhh",
                                  rterlim = 500
                                  
)
summary(wtp.rpl.t4.cor.young)
save(wtp.rpl.t4.cor.young, file = "wtp.rpl.t4.cor.young.rdata")
load("wtp.rpl.t4.cor.young.rdata")
AIC(wtp.rpl.t4.cor.young)
BIC(wtp.rpl.t4.cor.young)

mnl.t4.old <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|0,
                        reflevel = 3,
                        model="mnl",
                        data=ml.wtp.1,
                        subset = ml.wtp.1$treat==4 & ml.wtp.1$young==0,
                        print.init = 2,
                        print.level =2
)

wtp.t4.old <-coef(mnl.t4.old)/coef(mnl.t4.old)[1]
start.t4.cor.old <- c(wtp.t4.old,0.1,0.1,0.1,0.1,0.1,0)
wtp.rpl.t4.cor.old <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|1,
                                reflevel = 3,
                                model="gmnl",
                                data=ml.wtp.1,
                                subset = ml.wtp.1$treat==4& ml.wtp.1$young==0,
                                ranp = c(label = "n", method = "n"),
                                correlation = T,
                                fixed= c(T, rep(F,3), rep(F,3), F,F,T),
                                panel = T, 
                                haltons = NA,
                                start = start.t4.cor.old,
                                print.init = 2,
                                print.level =2,
                                method = "bhhh",
                                rterlim = 500
                                
)
summary(wtp.rpl.t4.cor.old)
save(wtp.rpl.t4.cor.old, file = "wtp.rpl.t4.cor.old.rdata")
load("wtp.rpl.t4.cor.old.rdata")
AIC(wtp.rpl.t4.cor.old)
BIC(wtp.rpl.t4.cor.old)

## gender
mnl.t4.female <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|0,
                           reflevel = 3,
                           model="mnl",
                           data=ml.wtp.1,
                           subset = ml.wtp.1$treat==4 & ml.wtp.1$female==1,
                           print.init = 2,
                           print.level =2
)

wtp.t4.female <-coef(mnl.t4.female)/coef(mnl.t4.female)[1]
start.t4.cor.female <- c(wtp.t4.female,0.1,0.1,0.1,0.1,0.1,0)
wtp.rpl.t4.cor.female <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|1,
                                   reflevel = 3,
                                   model="gmnl",
                                   data=ml.wtp.1,
                                   subset = ml.wtp.1$treat==4& ml.wtp.1$female==1,
                                   ranp = c(label = "n", method = "n"),
                                   correlation = T,
                                   fixed= c(T, rep(F,3), rep(F,3), F,F,T),
                                   panel = T, 
                                   haltons = NA,
                                   start = start.t4.cor.female,
                                   print.init = 2,
                                   print.level =2,
                                   method = "bhhh",
                                   rterlim = 500
                                   
)
summary(wtp.rpl.t4.cor.female)
save(wtp.rpl.t4.cor.female, file = "wtp.rpl.t4.cor.female.rdata")
load("wtp.rpl.t4.cor.female.rdata")
AIC(wtp.rpl.t4.cor.female)
BIC(wtp.rpl.t4.cor.female)

mnl.t4.male <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|0,
                         reflevel = 3,
                         model="mnl",
                         data=ml.wtp.1,
                         subset = ml.wtp.1$treat==4 & ml.wtp.1$female==0,
                         print.init = 2,
                         print.level =2
)

wtp.t4.male <-coef(mnl.t4.male)/coef(mnl.t4.male)[1]
start.t4.cor.male <- c(wtp.t4.male,0.1,0.1,0.1,0.1,0.1,0)
wtp.rpl.t4.cor.male <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|1,
                                 reflevel = 3,
                                 model="gmnl",
                                 data=ml.wtp.1,
                                 subset = ml.wtp.1$treat==4& ml.wtp.1$female==0,
                                 ranp = c(label = "n", method = "n"),
                                 correlation = T,
                                 fixed= c(T, rep(F,3), rep(F,3), F,F,T),
                                 panel = T, 
                                 haltons = NA,
                                 start = start.t4.cor.male,
                                 print.init = 2,
                                 print.level =2,
                                 method = "bhhh",
                                 rterlim = 500
                                 
)
summary(wtp.rpl.t4.cor.male)
save(wtp.rpl.t4.cor.male, file = "wtp.rpl.t4.cor.male.rdata")
load("wtp.rpl.t4.cor.male.rdata")
AIC(wtp.rpl.t4.cor.male)
BIC(wtp.rpl.t4.cor.male)

## race
mnl.t4.white <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|0,
                          reflevel = 3,
                          model="mnl",
                          data=ml.wtp.1,
                          subset = ml.wtp.1$treat==4 & ml.wtp.1$white==1,
                          print.init = 2,
                          print.level =2
)

wtp.t4.white <-coef(mnl.t4.white)/coef(mnl.t4.white)[1]
start.t4.cor.white <- c(wtp.t4.white,0.1,0.1,0.1,0.1,0.1,0)
wtp.rpl.t4.cor.white <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|1,
                                  reflevel = 3,
                                  model="gmnl",
                                  data=ml.wtp.1,
                                  subset = ml.wtp.1$treat==4& ml.wtp.1$white==1,
                                  ranp = c(label = "n", method = "n"),
                                  correlation = T,
                                  fixed= c(T, rep(F,3), rep(F,3), F,F,T),
                                  panel = T, 
                                  haltons = NA,
                                  start = start.t4.cor.white,
                                  print.init = 2,
                                  print.level =2,
                                  method = "bhhh",
                                  rterlim = 500
                                  
)
summary(wtp.rpl.t4.cor.white)
save(wtp.rpl.t4.cor.white, file = "wtp.rpl.t4.cor.white.rdata")
load("wtp.rpl.t4.cor.white.rdata")
AIC(wtp.rpl.t4.cor.white)
BIC(wtp.rpl.t4.cor.white)

mnl.t4.non_white <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|0,
                              reflevel = 3,
                              model="mnl",
                              data=ml.wtp.1,
                              subset = ml.wtp.1$treat==4 & ml.wtp.1$white==0,
                              print.init = 2,
                              print.level =2
)

wtp.t4.non_white <-coef(mnl.t4.non_white)/coef(mnl.t4.non_white)[1]
start.t4.cor.non_white <- c(wtp.t4.non_white,0.1,0.1,0.1,0.1,0.1,0)
wtp.rpl.t4.cor.non_white <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|1,
                                      reflevel = 3,
                                      model="gmnl",
                                      data=ml.wtp.1,
                                      subset = ml.wtp.1$treat==4& ml.wtp.1$white==0,
                                      ranp = c(label = "n", method = "n"),
                                      correlation = T,
                                      fixed= c(T, rep(F,3), rep(F,3), F,F,T),
                                      panel = T, 
                                      haltons = NA,
                                      start = start.t4.cor.non_white,
                                      print.init = 2,
                                      print.level =2,
                                      method = "bhhh",
                                      rterlim = 500
                                      
)
summary(wtp.rpl.t4.cor.non_white)
save(wtp.rpl.t4.cor.non_white, file = "wtp.rpl.t4.cor.non_white.rdata")
load("wtp.rpl.t4.cor.non_white.rdata")
AIC(wtp.rpl.t4.cor.non_white)
BIC(wtp.rpl.t4.cor.non_white)

# region
mnl.t4.midwest <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|0,
                            reflevel = 3,
                            model="mnl",
                            data=ml.wtp.1,
                            subset = ml.wtp.1$treat==4 & ml.wtp.1$region==1,
                            print.init = 2,
                            print.level =2
)
summary(mnl.t4.midwest)
wtp.t4.midwest <-coef(mnl.t4.midwest)/coef(mnl.t4.midwest)[1]
start.t4.cor.midwest <- c(wtp.t4.midwest,0.1,0.1,0.1,0.1,0,0)
wtp.rpl.t4.cor.midwest <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|1,
                                    reflevel = 3,
                                    model="gmnl",
                                    data=ml.wtp.1,
                                    subset = ml.wtp.1$treat==4& ml.wtp.1$region==1,
                                    ranp = c(label = "n", method = "n"),
                                    correlation = T,
                                    fixed= c(T, rep(F,3), rep(F,3), F,F,T),
                                    panel = T, 
                                    haltons = NA,
                                    start = start.t4.cor.midwest,
                                    print.init = 2,
                                    print.level =2,
                                    method = "bhhh",
                                    rterlim = 500
                                    
)
summary(wtp.rpl.t4.cor.midwest)
save(wtp.rpl.t4.cor.midwest, file = "wtp.rpl.t4.cor.midwest.rdata")
AIC(wtp.rpl.t4.cor.midwest)
BIC(wtp.rpl.t4.cor.midwest)

mnl.t4.northeast <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|0,
                              reflevel = 3,
                              model="mnl",
                              data=ml.wtp.1,
                              subset = ml.wtp.1$treat==4 & ml.wtp.1$region==2,
                              print.init = 2,
                              print.level =2
)

wtp.t4.northeast <-coef(mnl.t4.northeast)/coef(mnl.t4.northeast)[1]
start.t4.cor.northeast <- c(wtp.t4.northeast,0.1,0.1,0.1,0.1,0,0)
wtp.rpl.t4.cor.northeast <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|1,
                                      reflevel = 3,
                                      model="gmnl",
                                      data=ml.wtp.1,
                                      subset = ml.wtp.1$treat==4& ml.wtp.1$region==2,
                                      ranp = c(label = "n", method = "n"),
                                      correlation = T,
                                      fixed= c(T, rep(F,3), rep(F,3), F,F,T),
                                      panel = T, 
                                      haltons = NA,
                                      start = start.t4.cor.northeast,
                                      print.init = 2,
                                      print.level =2,
                                      method = "bhhh",
                                      rterlim = 500
                                      
)
summary(wtp.rpl.t4.cor.northeast)
save(wtp.rpl.t4.cor.northeast, file = "wtp.rpl.t4.cor.northeast.rdata")
AIC(wtp.rpl.t4.cor.northeast)
BIC(wtp.rpl.t4.cor.northeast)


mnl.t4.south <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|0,
                          reflevel = 3,
                          model="mnl",
                          data=ml.wtp.1,
                          subset = ml.wtp.1$treat==4 & ml.wtp.1$region==3,
                          print.init = 2,
                          print.level =2
)

wtp.t4.south <-coef(mnl.t4.south)/coef(mnl.t4.south)[1]
start.t4.cor.south <- c(wtp.t4.south,0.1,0.1,0.1,0.1,0.1,0)
wtp.rpl.t4.cor.south <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|1,
                                  reflevel = 3,
                                  model="gmnl",
                                  data=ml.wtp.1,
                                  subset = ml.wtp.1$treat==4& ml.wtp.1$region==3,
                                  ranp = c(label = "n", method = "n"),
                                  correlation = T,
                                  fixed= c(T, rep(F,3), rep(F,3), F,F,T),
                                  panel = T, 
                                  haltons = NA,
                                  start = start.t4.cor.south,
                                  print.init = 2,
                                  print.level =2,
                                  method = "bhhh",
                                  rterlim = 500
                                  
)
summary(wtp.rpl.t4.cor.south)
save(wtp.rpl.t4.cor.south, file = "wtp.rpl.t4.cor.south.rdata")
AIC(wtp.rpl.t4.cor.south)
BIC(wtp.rpl.t4.cor.south)

mnl.t4.west <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|0,
                         reflevel = 3,
                         model="mnl",
                         data=ml.wtp.1,
                         subset = ml.wtp.1$treat==4 & ml.wtp.1$region==4,
                         print.init = 2,
                         print.level =2
)

wtp.t4.west <-coef(mnl.t4.west)/coef(mnl.t4.west)[1]
start.t4.cor.west <- c(wtp.t4.west,0.1,0.1,0.1,0.1,0.1,0)
wtp.rpl.t4.cor.west <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|1,
                                 reflevel = 3,
                                 model="gmnl",
                                 data=ml.wtp.1,
                                 subset = ml.wtp.1$treat==4& ml.wtp.1$region==4,
                                 ranp = c(label = "n", method = "n"),
                                 correlation = T,
                                 fixed= c(T, rep(F,3), rep(F,3), F,F,T),
                                 panel = T, 
                                 haltons = NA,
                                 start = start.t4.cor.west,
                                 print.init = 2,
                                 print.level =2,
                                 method = "bhhh",
                                 rterlim = 500
                                 
)
summary(wtp.rpl.t4.cor.west)
save(wtp.rpl.t4.cor.west, file = "wtp.rpl.t4.cor.west.rdata")
AIC(wtp.rpl.t4.cor.west)
BIC(wtp.rpl.t4.cor.west)

#### T5 ####
mnl.t5 <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|0,
                    reflevel = 3,
                    model="mnl",
                    data=ml.wtp.1,
                    subset = ml.wtp.1$treat==5,
                    print.init = 2,
                    print.level =2
)

wtp.t5 <-coef(mnl.t5)/coef(mnl.t5)[1]
start.t5.cor <- c(wtp.t5,0.1,0.1,0.1,0.1,0.1,0)
wtp.rpl.t5.cor <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|1,
                            reflevel = 3,
                            model="gmnl",
                            data=ml.wtp.1,
                            subset = ml.wtp.1$treat==5,
                            ranp = c(label = "n", method = "n"),
                            correlation = T,
                            fixed= c(T, rep(F,3), rep(F,3), F,F,T),
                            panel = T, 
                            haltons = NA,
                            start = start.t5.cor,
                            print.init = 2,
                            print.level =2,
                            method = "bhhh",
                            rterlim = 500
                            
)
summary(wtp.rpl.t5.cor)
save(wtp.rpl.t5.cor, file = "wtp.rpl.t5.cor.rdata")
load("wtp.rpl.t5.cor.rdata")
AIC(wtp.rpl.t5.cor)
BIC(wtp.rpl.t5.cor)

# age
mnl.t5.young <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|0,
                          reflevel = 3,
                          model="mnl",
                          data=ml.wtp.1,
                          subset = ml.wtp.1$treat==5 & ml.wtp.1$young==1,
                          print.init = 2,
                          print.level =2
)

wtp.t5.young <-coef(mnl.t5.young)/coef(mnl.t5.young)[1]
start.t5.cor.young <- c(wtp.t5.young,0.1,0.1,0.1,0.1,0.1,0)
wtp.rpl.t5.cor.young <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|1,
                                  reflevel = 3,
                                  model="gmnl",
                                  data=ml.wtp.1,
                                  subset = ml.wtp.1$treat==5& ml.wtp.1$young==1,
                                  ranp = c(label = "n", method = "n"),
                                  correlation = T,
                                  fixed= c(T, rep(F,3), rep(F,3), F,F,T),
                                  panel = T, 
                                  haltons = NA,
                                  start = start.t5.cor.young,
                                  print.init = 2,
                                  print.level =2,
                                  method = "bhhh",
                                  rterlim = 500
                                  
)
summary(wtp.rpl.t5.cor.young)
save(wtp.rpl.t5.cor.young, file = "wtp.rpl.t5.cor.young.rdata")
load("wtp.rpl.t5.cor.young.rdata")
AIC(wtp.rpl.t5.cor.young)
BIC(wtp.rpl.t5.cor.young)

mnl.t5.old <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|0,
                        reflevel = 3,
                        model="mnl",
                        data=ml.wtp.1,
                        subset = ml.wtp.1$treat==5 & ml.wtp.1$young==0,
                        print.init = 2,
                        print.level =2
)

wtp.t5.old <-coef(mnl.t5.old)/coef(mnl.t5.old)[1]
start.t5.cor.old <- c(wtp.t5.old,0.1,0.1,0.1,0.1,0.1,0)
wtp.rpl.t5.cor.old <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|1,
                                reflevel = 3,
                                model="gmnl",
                                data=ml.wtp.1,
                                subset = ml.wtp.1$treat==5& ml.wtp.1$young==0,
                                ranp = c(label = "n", method = "n"),
                                correlation = T,
                                fixed= c(T, rep(F,3), rep(F,3), F,F,T),
                                panel = T, 
                                haltons = NA,
                                start = start.t5.cor.old,
                                print.init = 2,
                                print.level =2,
                                method = "bhhh",
                                rterlim = 500
                                
)
summary(wtp.rpl.t5.cor.old)
save(wtp.rpl.t5.cor.old, file = "wtp.rpl.t5.cor.old.rdata")
load("wtp.rpl.t5.cor.old.rdata")
AIC(wtp.rpl.t5.cor.old)
BIC(wtp.rpl.t5.cor.old)

## gender
mnl.t5.female <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|0,
                           reflevel = 3,
                           model="mnl",
                           data=ml.wtp.1,
                           subset = ml.wtp.1$treat==5 & ml.wtp.1$female==1,
                           print.init = 2,
                           print.level =2
)

wtp.t5.female <-coef(mnl.t5.female)/coef(mnl.t5.female)[1]
start.t5.cor.female <- c(wtp.t5.female,0.1,0.1,0.1,0.1,0.1,0)
wtp.rpl.t5.cor.female <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|1,
                                   reflevel = 3,
                                   model="gmnl",
                                   data=ml.wtp.1,
                                   subset = ml.wtp.1$treat==5& ml.wtp.1$female==1,
                                   ranp = c(label = "n", method = "n"),
                                   correlation = T,
                                   fixed= c(T, rep(F,3), rep(F,3), F,F,T),
                                   panel = T, 
                                   haltons = NA,
                                   start = start.t5.cor.female,
                                   print.init = 2,
                                   print.level =2,
                                   method = "bhhh",
                                   rterlim = 500
                                   
)
summary(wtp.rpl.t5.cor.female)
save(wtp.rpl.t5.cor.female, file = "wtp.rpl.t5.cor.female.rdata")
load("wtp.rpl.t5.cor.female.rdata")
AIC(wtp.rpl.t5.cor.female)
BIC(wtp.rpl.t5.cor.female)

mnl.t5.male <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|0,
                         reflevel = 3,
                         model="mnl",
                         data=ml.wtp.1,
                         subset = ml.wtp.1$treat==5 & ml.wtp.1$female==0,
                         print.init = 2,
                         print.level =2
)

wtp.t5.male <-coef(mnl.t5.male)/coef(mnl.t5.male)[1]
start.t5.cor.male <- c(wtp.t5.male,0.1,0.1,0.1,0.1,0.1,0)
wtp.rpl.t5.cor.male <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|1,
                                 reflevel = 3,
                                 model="gmnl",
                                 data=ml.wtp.1,
                                 subset = ml.wtp.1$treat==5& ml.wtp.1$female==0,
                                 ranp = c(label = "n", method = "n"),
                                 correlation = T,
                                 fixed= c(T, rep(F,3), rep(F,3), F,F,T),
                                 panel = T, 
                                 haltons = NA,
                                 start = start.t5.cor.male,
                                 print.init = 2,
                                 print.level =2,
                                 method = "bhhh",
                                 rterlim = 500
                                 
)
summary(wtp.rpl.t5.cor.male)
save(wtp.rpl.t5.cor.male, file = "wtp.rpl.t5.cor.male.rdata")
load("wtp.rpl.t5.cor.male.rdata")
AIC(wtp.rpl.t5.cor.male)
BIC(wtp.rpl.t5.cor.male)

## race
mnl.t5.white <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|0,
                          reflevel = 3,
                          model="mnl",
                          data=ml.wtp.1,
                          subset = ml.wtp.1$treat==5 & ml.wtp.1$white==1,
                          print.init = 2,
                          print.level =2
)

wtp.t5.white <-coef(mnl.t5.white)/coef(mnl.t5.white)[1]
start.t5.cor.white <- c(wtp.t5.white,0.1,0.1,0.1,0.1,0.1,0)
wtp.rpl.t5.cor.white <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|1,
                                  reflevel = 3,
                                  model="gmnl",
                                  data=ml.wtp.1,
                                  subset = ml.wtp.1$treat==5& ml.wtp.1$white==1,
                                  ranp = c(label = "n", method = "n"),
                                  correlation = T,
                                  fixed= c(T, rep(F,3), rep(F,3), F,F,T),
                                  panel = T, 
                                  haltons = NA,
                                  start = start.t5.cor.white,
                                  print.init = 2,
                                  print.level =2,
                                  method = "bhhh",
                                  rterlim = 500
                                  
)
summary(wtp.rpl.t5.cor.white)
save(wtp.rpl.t5.cor.white, file = "wtp.rpl.t5.cor.white.rdata")
load("wtp.rpl.t5.cor.white.rdata")
AIC(wtp.rpl.t5.cor.white)
BIC(wtp.rpl.t5.cor.white)

mnl.t5.non_white <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|0,
                              reflevel = 3,
                              model="mnl",
                              data=ml.wtp.1,
                              subset = ml.wtp.1$treat==5 & ml.wtp.1$white==0,
                              print.init = 2,
                              print.level =2
)

wtp.t5.non_white <-coef(mnl.t5.non_white)/coef(mnl.t5.non_white)[1]
start.t5.cor.non_white <- c(wtp.t5.non_white,0.1,0.1,0.1,0.1,0.1,0)
wtp.rpl.t5.cor.non_white <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|1,
                                      reflevel = 3,
                                      model="gmnl",
                                      data=ml.wtp.1,
                                      subset = ml.wtp.1$treat==5& ml.wtp.1$white==0,
                                      ranp = c(label = "n", method = "n"),
                                      correlation = T,
                                      fixed= c(T, rep(F,3), rep(F,3), F,F,T),
                                      panel = T, 
                                      haltons = NA,
                                      start = start.t5.cor.non_white,
                                      print.init = 2,
                                      print.level =2,
                                      method = "bhhh",
                                      rterlim = 500
                                      
)
summary(wtp.rpl.t5.cor.non_white)
save(wtp.rpl.t5.cor.non_white, file = "wtp.rpl.t5.cor.non_white.rdata")
load("wtp.rpl.t5.cor.non_white.rdata")
AIC(wtp.rpl.t5.cor.non_white)
BIC(wtp.rpl.t5.cor.non_white)

# region
mnl.t5.midwest <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|0,
                            reflevel = 3,
                            model="mnl",
                            data=ml.wtp.1,
                            subset = ml.wtp.1$treat==5 & ml.wtp.1$region==1,
                            print.init = 2,
                            print.level =2
)

wtp.t5.midwest <-coef(mnl.t5.midwest)/coef(mnl.t5.midwest)[1]
start.t5.cor.midwest <- c(wtp.t5.midwest,0.1,0.1,0.1,0.1,0.1,0)
wtp.rpl.t5.cor.midwest <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|1,
                                    reflevel = 3,
                                    model="gmnl",
                                    data=ml.wtp.1,
                                    subset = ml.wtp.1$treat==5& ml.wtp.1$region==1,
                                    ranp = c(label = "n", method = "n"),
                                    correlation = T,
                                    fixed= c(T, rep(F,3), rep(F,3), F,F,T),
                                    panel = T, 
                                    haltons = NA,
                                    start = start.t5.cor.midwest,
                                    print.init = 2,
                                    print.level =2,
                                    method = "bhhh",
                                    rterlim = 500
                                    
)
summary(wtp.rpl.t5.cor.midwest)
save(wtp.rpl.t5.cor.midwest, file = "wtp.rpl.t5.cor.midwest.rdata")
AIC(wtp.rpl.t5.cor.midwest)
BIC(wtp.rpl.t5.cor.midwest)

mnl.t5.northeast <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|0,
                              reflevel = 3,
                              model="mnl",
                              data=ml.wtp.1,
                              subset = ml.wtp.1$treat==5 & ml.wtp.1$region==2,
                              print.init = 2,
                              print.level =2
)

wtp.t5.northeast <-coef(mnl.t5.northeast)/coef(mnl.t5.northeast)[1]
start.t5.cor.northeast <- c(wtp.t5.northeast,0.1,0.1,0.1,0.1,0.1,0)
wtp.rpl.t5.cor.northeast <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|1,
                                      reflevel = 3,
                                      model="gmnl",
                                      data=ml.wtp.1,
                                      subset = ml.wtp.1$treat==5& ml.wtp.1$region==2,
                                      ranp = c(label = "n", method = "n"),
                                      correlation = T,
                                      fixed= c(T, rep(F,3), rep(F,3), F,F,T),
                                      panel = T, 
                                      haltons = NA,
                                      start = start.t5.cor.northeast,
                                      print.init = 2,
                                      print.level =2,
                                      method = "bhhh",
                                      rterlim = 500
                                      
)
summary(wtp.rpl.t5.cor.northeast)
save(wtp.rpl.t5.cor.northeast, file = "wtp.rpl.t5.cor.northeast.rdata")
AIC(wtp.rpl.t5.cor.northeast)
BIC(wtp.rpl.t5.cor.northeast)


mnl.t5.south <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|0,
                          reflevel = 3,
                          model="mnl",
                          data=ml.wtp.1,
                          subset = ml.wtp.1$treat==5 & ml.wtp.1$region==3,
                          print.init = 2,
                          print.level =2
)

wtp.t5.south <-coef(mnl.t5.south)/coef(mnl.t5.south)[1]
start.t5.cor.south <- c(wtp.t5.south,0.1,0.1,0.1,0.1,0.1,0)
wtp.rpl.t5.cor.south <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|1,
                                  reflevel = 3,
                                  model="gmnl",
                                  data=ml.wtp.1,
                                  subset = ml.wtp.1$treat==5& ml.wtp.1$region==3,
                                  ranp = c(label = "n", method = "n"),
                                  correlation = T,
                                  fixed= c(T, rep(F,3), rep(F,3), F,F,T),
                                  panel = T, 
                                  haltons = NA,
                                  start = start.t5.cor.south,
                                  print.init = 2,
                                  print.level =2,
                                  method = "bhhh",
                                  rterlim = 500
                                  
)
summary(wtp.rpl.t5.cor.south)
save(wtp.rpl.t5.cor.south, file = "wtp.rpl.t5.cor.south.rdata")
AIC(wtp.rpl.t5.cor.south)
BIC(wtp.rpl.t5.cor.south)

mnl.t5.west <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|0,
                         reflevel = 3,
                         model="mnl",
                         data=ml.wtp.1,
                         subset = ml.wtp.1$treat==5 & ml.wtp.1$region==4,
                         print.init = 2,
                         print.level =2
)

wtp.t5.west <-coef(mnl.t5.west)/coef(mnl.t5.west)[1]
start.t5.cor.west <- c(wtp.t5.west,0.1,0.1,0.1,0.1,0.1,0)
wtp.rpl.t5.cor.west <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|1,
                                 reflevel = 3,
                                 model="gmnl",
                                 data=ml.wtp.1,
                                 subset = ml.wtp.1$treat==5& ml.wtp.1$region==4,
                                 ranp = c(label = "n", method = "n"),
                                 correlation = T,
                                 fixed= c(T, rep(F,3), rep(F,3), F,F,T),
                                 panel = T, 
                                 haltons = NA,
                                 start = start.t5.cor.west,
                                 print.init = 2,
                                 print.level =2,
                                 method = "bhhh",
                                 rterlim = 500
                                 
)
summary(wtp.rpl.t5.cor.west)
save(wtp.rpl.t5.cor.west, file = "wtp.rpl.t5.cor.west.rdata")
AIC(wtp.rpl.t5.cor.west)
BIC(wtp.rpl.t5.cor.west)

#### T6 ####
mnl.t6 <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|0,
                    reflevel = 3,
                    model="mnl",
                    data=ml.wtp.1,
                    subset = ml.wtp.1$treat==6,
                    print.init = 2,
                    print.level =2
)

wtp.t6 <-coef(mnl.t6)/coef(mnl.t6)[1]
start.t6.cor <- c(wtp.t6,0.1,0.1,0.1,0.1,0.1,0)
wtp.rpl.t6.cor <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|1,
                            reflevel = 3,
                            model="gmnl",
                            data=ml.wtp.1,
                            subset = ml.wtp.1$treat==6,
                            ranp = c(label = "n", method = "n"),
                            correlation = T,
                            fixed= c(T, rep(F,3), rep(F,3), F,F,T),
                            panel = T, 
                            haltons = NA,
                            start = start.t6.cor,
                            print.init = 2,
                            print.level =2,
                            method = "bhhh",
                            rterlim = 500
                            
)
summary(wtp.rpl.t6.cor)
save(wtp.rpl.t6.cor, file = "wtp.rpl.t6.cor.rdata")
load("wtp.rpl.t6.cor.rdata")
AIC(wtp.rpl.t6.cor)
BIC(wtp.rpl.t6.cor)

# age
mnl.t6.young <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|0,
                          reflevel = 3,
                          model="mnl",
                          data=ml.wtp.1,
                          subset = ml.wtp.1$treat==6 & ml.wtp.1$young==1,
                          print.init = 2,
                          print.level =2
)

wtp.t6.young <-coef(mnl.t6.young)/coef(mnl.t6.young)[1]
start.t6.cor.young <- c(wtp.t6.young,0.1,0.1,0.1,0.1,0.1,0)
wtp.rpl.t6.cor.young <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|1,
                                  reflevel = 3,
                                  model="gmnl",
                                  data=ml.wtp.1,
                                  subset = ml.wtp.1$treat==6& ml.wtp.1$young==1,
                                  ranp = c(label = "n", method = "n"),
                                  correlation = T,
                                  fixed= c(T, rep(F,3), rep(F,3), F,F,T),
                                  panel = T, 
                                  haltons = NA,
                                  start = start.t6.cor.young,
                                  print.init = 2,
                                  print.level =2,
                                  method = "bhhh",
                                  rterlim = 500
                                  
)
summary(wtp.rpl.t6.cor.young)
save(wtp.rpl.t6.cor.young, file = "wtp.rpl.t6.cor.young.rdata")
load("wtp.rpl.t6.cor.young.rdata")
AIC(wtp.rpl.t6.cor.young)
BIC(wtp.rpl.t6.cor.young)

mnl.t6.old <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|0,
                        reflevel = 3,
                        model="mnl",
                        data=ml.wtp.1,
                        subset = ml.wtp.1$treat==6 & ml.wtp.1$young==0,
                        print.init = 2,
                        print.level =2
)

wtp.t6.old <-coef(mnl.t6.old)/coef(mnl.t6.old)[1]
start.t6.cor.old <- c(wtp.t6.old,0.1,0.1,0.1,0.1,0.1,0)
wtp.rpl.t6.cor.old <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|1,
                                reflevel = 3,
                                model="gmnl",
                                data=ml.wtp.1,
                                subset = ml.wtp.1$treat==6& ml.wtp.1$young==0,
                                ranp = c(label = "n", method = "n"),
                                correlation = T,
                                fixed= c(T, rep(F,3), rep(F,3), F,F,T),
                                panel = T, 
                                haltons = NA,
                                start = start.t6.cor.old,
                                print.init = 2,
                                print.level =2,
                                method = "bhhh",
                                rterlim = 500
                                
)
summary(wtp.rpl.t6.cor.old)
save(wtp.rpl.t6.cor.old, file = "wtp.rpl.t6.cor.old.rdata")
load("wtp.rpl.t6.cor.old.rdata")
AIC(wtp.rpl.t6.cor.old)
BIC(wtp.rpl.t6.cor.old)

## gender
mnl.t6.female <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|0,
                           reflevel = 3,
                           model="mnl",
                           data=ml.wtp.1,
                           subset = ml.wtp.1$treat==6 & ml.wtp.1$female==1,
                           print.init = 2,
                           print.level =2
)

wtp.t6.female <-coef(mnl.t6.female)/coef(mnl.t6.female)[1]
start.t6.cor.female <- c(wtp.t6.female,0.1,0.1,0.1,0.1,0.1,0)
wtp.rpl.t6.cor.female <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|1,
                                   reflevel = 3,
                                   model="gmnl",
                                   data=ml.wtp.1,
                                   subset = ml.wtp.1$treat==6& ml.wtp.1$female==1,
                                   ranp = c(label = "n", method = "n"),
                                   correlation = T,
                                   fixed= c(T, rep(F,3), rep(F,3), F,F,T),
                                   panel = T, 
                                   haltons = NA,
                                   start = start.t6.cor.female,
                                   print.init = 2,
                                   print.level =2,
                                   method = "bhhh",
                                   rterlim = 500
                                   
)
summary(wtp.rpl.t6.cor.female)
save(wtp.rpl.t6.cor.female, file = "wtp.rpl.t6.cor.female.rdata")
load("wtp.rpl.t6.cor.female.rdata")
AIC(wtp.rpl.t6.cor.female)
BIC(wtp.rpl.t6.cor.female)

mnl.t6.male <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|0,
                         reflevel = 3,
                         model="mnl",
                         data=ml.wtp.1,
                         subset = ml.wtp.1$treat==6 & ml.wtp.1$female==0,
                         print.init = 2,
                         print.level =2
)

wtp.t6.male <-coef(mnl.t6.male)/coef(mnl.t6.male)[1]
start.t6.cor.male <- c(wtp.t6.male,0.1,0.1,0.1,0.1,0.1,0)
wtp.rpl.t6.cor.male <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|1,
                                 reflevel = 3,
                                 model="gmnl",
                                 data=ml.wtp.1,
                                 subset = ml.wtp.1$treat==6& ml.wtp.1$female==0,
                                 ranp = c(label = "n", method = "n"),
                                 correlation = T,
                                 fixed= c(T, rep(F,3), rep(F,3), F,F,T),
                                 panel = T, 
                                 haltons = NA,
                                 start = start.t6.cor.male,
                                 print.init = 2,
                                 print.level =2,
                                 method = "bhhh",
                                 rterlim = 500
                                 
)
summary(wtp.rpl.t6.cor.male)
save(wtp.rpl.t6.cor.male, file = "wtp.rpl.t6.cor.male.rdata")
load("wtp.rpl.t6.cor.male.rdata")
AIC(wtp.rpl.t6.cor.male)
BIC(wtp.rpl.t6.cor.male)

## race
mnl.t6.white <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|0,
                          reflevel = 3,
                          model="mnl",
                          data=ml.wtp.1,
                          subset = ml.wtp.1$treat==6 & ml.wtp.1$white==1,
                          print.init = 2,
                          print.level =2
)

wtp.t6.white <-coef(mnl.t6.white)/coef(mnl.t6.white)[1]
start.t6.cor.white <- c(wtp.t6.white,0.1,0.1,0.1,0.1,0.1,0)
wtp.rpl.t6.cor.white <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|1,
                                  reflevel = 3,
                                  model="gmnl",
                                  data=ml.wtp.1,
                                  subset = ml.wtp.1$treat==6& ml.wtp.1$white==1,
                                  ranp = c(label = "n", method = "n"),
                                  correlation = T,
                                  fixed= c(T, rep(F,3), rep(F,3), F,F,T),
                                  panel = T, 
                                  haltons = NA,
                                  start = start.t6.cor.white,
                                  print.init = 2,
                                  print.level =2,
                                  method = "bhhh",
                                  rterlim = 500
                                  
)
summary(wtp.rpl.t6.cor.white)
save(wtp.rpl.t6.cor.white, file = "wtp.rpl.t6.cor.white.rdata")
load("wtp.rpl.t6.cor.white.rdata")
AIC(wtp.rpl.t6.cor.white)
BIC(wtp.rpl.t6.cor.white)

mnl.t6.non_white <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|0,
                              reflevel = 3,
                              model="mnl",
                              data=ml.wtp.1,
                              subset = ml.wtp.1$treat==6 & ml.wtp.1$white==0,
                              print.init = 2,
                              print.level =2
)

wtp.t6.non_white <-coef(mnl.t6.non_white)/coef(mnl.t6.non_white)[1]
start.t6.cor.non_white <- c(wtp.t6.non_white,0.1,0.1,0.1,0.1,0.1,0)
wtp.rpl.t6.cor.non_white <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|1,
                                      reflevel = 3,
                                      model="gmnl",
                                      data=ml.wtp.1,
                                      subset = ml.wtp.1$treat==6& ml.wtp.1$white==0,
                                      ranp = c(label = "n", method = "n"),
                                      correlation = T,
                                      fixed= c(T, rep(F,3), rep(F,3), F,F,T),
                                      panel = T, 
                                      haltons = NA,
                                      start = start.t6.cor.non_white,
                                      print.init = 2,
                                      print.level =2,
                                      method = "bhhh",
                                      rterlim = 500
                                      
)
summary(wtp.rpl.t6.cor.non_white)
save(wtp.rpl.t6.cor.non_white, file = "wtp.rpl.t6.cor.non_white.rdata")
load("wtp.rpl.t6.cor.non_white.rdata")
AIC(wtp.rpl.t6.cor.non_white)
BIC(wtp.rpl.t6.cor.non_white)

# region
mnl.t6.midwest <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|0,
                            reflevel = 3,
                            model="mnl",
                            data=ml.wtp.1,
                            subset = ml.wtp.1$treat==6 & ml.wtp.1$region==1,
                            print.init = 2,
                            print.level =2
)

wtp.t6.midwest <-coef(mnl.t6.midwest)/coef(mnl.t6.midwest)[1]
start.t6.cor.midwest <- c(wtp.t6.midwest,0.1,0.1,0.1,0.1,0.1,0)
wtp.rpl.t6.cor.midwest <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|1,
                                    reflevel = 3,
                                    model="gmnl",
                                    data=ml.wtp.1,
                                    subset = ml.wtp.1$treat==6& ml.wtp.1$region==1,
                                    ranp = c(label = "n", method = "n"),
                                    correlation = T,
                                    fixed= c(T, rep(F,3), rep(F,3), F,F,T),
                                    panel = T, 
                                    haltons = NA,
                                    start = start.t6.cor.midwest,
                                    print.init = 2,
                                    print.level =2,
                                    method = "bhhh",
                                    rterlim = 500
                                    
)
summary(wtp.rpl.t6.cor.midwest)
save(wtp.rpl.t6.cor.midwest, file = "wtp.rpl.t6.cor.midwest.rdata")
AIC(wtp.rpl.t6.cor.midwest)
BIC(wtp.rpl.t6.cor.midwest)

mnl.t6.northeast <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|0,
                              reflevel = 3,
                              model="mnl",
                              data=ml.wtp.1,
                              subset = ml.wtp.1$treat==6 & ml.wtp.1$region==2,
                              print.init = 2,
                              print.level =2
)

wtp.t6.northeast <-coef(mnl.t6.northeast)/coef(mnl.t6.northeast)[1]
start.t6.cor.northeast <- c(wtp.t6.northeast,0.1,0.1,0.1,0.1,0.1,0)
wtp.rpl.t6.cor.northeast <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|1,
                                      reflevel = 3,
                                      model="gmnl",
                                      data=ml.wtp.1,
                                      subset = ml.wtp.1$treat==6& ml.wtp.1$region==2,
                                      ranp = c(label = "n", method = "n"),
                                      correlation = T,
                                      fixed= c(T, rep(F,3), rep(F,3), F,F,T),
                                      panel = T, 
                                      haltons = NA,
                                      start = start.t6.cor.northeast,
                                      print.init = 2,
                                      print.level =2,
                                      method = "bhhh",
                                      rterlim = 500
                                      
)
summary(wtp.rpl.t6.cor.northeast)
save(wtp.rpl.t6.cor.northeast, file = "wtp.rpl.t6.cor.northeast.rdata")
AIC(wtp.rpl.t6.cor.northeast)
BIC(wtp.rpl.t6.cor.northeast)


mnl.t6.south <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|0,
                          reflevel = 3,
                          model="mnl",
                          data=ml.wtp.1,
                          subset = ml.wtp.1$treat==6 & ml.wtp.1$region==3,
                          print.init = 2,
                          print.level =2
)

wtp.t6.south <-coef(mnl.t6.south)/coef(mnl.t6.south)[1]
start.t6.cor.south <- c(wtp.t6.south,0.1,0.1,0.1,0.1,0.1,0)
wtp.rpl.t6.cor.south <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|1,
                                  reflevel = 3,
                                  model="gmnl",
                                  data=ml.wtp.1,
                                  subset = ml.wtp.1$treat==6& ml.wtp.1$region==3,
                                  ranp = c(label = "n", method = "n"),
                                  correlation = T,
                                  fixed= c(T, rep(F,3), rep(F,3), F,F,T),
                                  panel = T, 
                                  haltons = NA,
                                  start = start.t6.cor.south,
                                  print.init = 2,
                                  print.level =2,
                                  method = "bhhh",
                                  rterlim = 500
                                  
)
summary(wtp.rpl.t6.cor.south)
save(wtp.rpl.t6.cor.south, file = "wtp.rpl.t6.cor.south.rdata")
AIC(wtp.rpl.t6.cor.south)
BIC(wtp.rpl.t6.cor.south)

mnl.t6.west <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|0,
                         reflevel = 3,
                         model="mnl",
                         data=ml.wtp.1,
                         subset = ml.wtp.1$treat==6 & ml.wtp.1$region==4,
                         print.init = 2,
                         print.level =2
)

wtp.t6.west <-coef(mnl.t6.west)/coef(mnl.t6.west)[1]
start.t6.cor.west <- c(wtp.t6.west,0.1,0.1,0.1,0.1,0.1,0)
wtp.rpl.t6.cor.west <- gmnl(formula = Choice ~ price + nobuy +  label + method |0|0|0|1,
                                 reflevel = 3,
                                 model="gmnl",
                                 data=ml.wtp.1,
                                 subset = ml.wtp.1$treat==6& ml.wtp.1$region==4,
                                 ranp = c(label = "n", method = "n"),
                                 correlation = T,
                                 fixed= c(T, rep(F,3), rep(F,3), F,F,T),
                                 panel = T, 
                                 haltons = NA,
                                 start = start.t6.cor.west,
                                 print.init = 2,
                                 print.level =2,
                                 method = "bhhh",
                                 rterlim = 500
                                 
)
summary(wtp.rpl.t6.cor.west)
save(wtp.rpl.t6.cor.west, file = "wtp.rpl.t6.cor.west.rdata")
AIC(wtp.rpl.t6.cor.west)
BIC(wtp.rpl.t6.cor.west)

######kr poe test ####
source("C:\\Users\\wei.yang\\OneDrive - Texas A&M AgriLife\\Wei\\Documentation\\Documentation\\R_codes\\function\\kr.poe.test.R")

#compare pooled models

model1 <- c(rep("wtp.rpl.control.cor",6), rep("wtp.rpl.t1.cor",5),rep("wtp.rpl.t2.cor",4),rep("wtp.rpl.t3.cor",3),
            rep("wtp.rpl.t4.cor",2),"wtp.rpl.t5.cor")
model2 <- c("wtp.rpl.t1.cor", "wtp.rpl.t2.cor","wtp.rpl.t3.cor","wtp.rpl.t4.cor","wtp.rpl.t5.cor","wtp.rpl.t6.cor",
            "wtp.rpl.t2.cor","wtp.rpl.t3.cor","wtp.rpl.t4.cor","wtp.rpl.t5.cor","wtp.rpl.t6.cor",
            "wtp.rpl.t3.cor","wtp.rpl.t4.cor","wtp.rpl.t5.cor","wtp.rpl.t6.cor",
            "wtp.rpl.t4.cor","wtp.rpl.t5.cor","wtp.rpl.t6.cor",
            "wtp.rpl.t5.cor","wtp.rpl.t6.cor",
            "wtp.rpl.t6.cor")
vari_compair <- c("label", "method")

kr.poe(model1, model2, vari_compair)

# compare within models
a <- c("control","t1", "t2","t3","t4","t5","t6")

demo1 <- c("young","female", "white")
demo2 <- c("old", "male", "non_white")
i=3

model1 <- paste("wtp.rpl.", a, ".cor.", demo1[i], sep = "")
model2 <- paste("wtp.rpl.", a, ".cor.",demo2[i], sep = "")

vari_compair <- c("label", "method")

kr.poe(model1, model2, vari_compair)





# compare between models
a <- c(rep("control",6),
       rep("t1",5), 
       rep("t2",4),
       rep("t3",3),
       rep("t4",2),
       "t5")

b <- c("t1", "t2","t3","t4","t5","t6",
       "t2","t3","t4","t5","t6",
       "t3","t4","t5","t6",
       "t4","t5","t6",
       "t5","t6",
       "t6")

demo <- c("young","old", "female","male", "white","non_white")

i=6

model1 <- paste("wtp.rpl.", a, ".cor.", demo[i], sep = "")
model2 <- paste("wtp.rpl.", b, ".cor.",demo[i], sep = "")

vari_compair <- c("label", "method")

kr.poe(model1, model2, vari_compair)


