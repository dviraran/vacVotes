### read libraries. TODO: check what is really required

library(stargazer)
library(forestplot)
library(ggplot2)
library(ggpubr)
library(reshape2)
library(plyr)
library(dplyr)
library(stringr)
library(ggrepel)
library(ggeffects)
library(cowplot)
library(robust)
library(lme4)
library(afex) 
library(sjmisc)

####
eps = 0.0001
working.dir ='~/Documents/covid_analyses/vacVotes/'
source(paste0(working.dir,'vacVotesFunctions.R'))

####

parties.all = c("Avoda", "Yamina","ZionutDatit","KaholLavan" ,  
                "IsraelBeitenu", "Likud",  "Meretz", "YeshAtid", "Shas" ,"TikvaHadasha","Raam","YahadutHatora",'Aravit')
parties.group.all = c('Left','Center','Right','Center','Center','Right','Left','Left','Haredi','Center','Arab','Haredi','Arab')

###

election=2021

votes = readVotes(working.dir,election=election)
votes = votes[,c('code','BZB','N',parties.all)]
votes = ddply(votes,"code",numcolwise(sum))
votes[,intersect(parties.all,colnames(votes))] = votes[,intersect(parties.all,colnames(votes))]/votes$BZB

vac = read.csv(paste0(working.dir,'data/geographic-sum-per-day-ver_00714_small.csv'))
agas = str_pad(as.numeric(trimws(vac$agas_code)),4,pad = "0")
agas[is.na(agas)]='0000'
vac$code = paste0(vac$town_code,agas)
vac$date = as.Date(vac$date)

## choose rows every 30 days from dose 1, and every 30 days from dose 3
vac = subset(vac,vac$date %in% c(seq(from=as.Date('2020-11-21'),to=as.Date('2021-07-30'),by='30 day'),
                                 seq(from=as.Date('2021-07-01'),to=as.Date('2021-11-30'),by='30 day')))

socioeconomic = readSE(working.dir)
ages = getAreasAges(working.dir)

df = merge(vac,votes,by='code')
df = merge(df,socioeconomic,by='code')
df = merge(df,ages,by='code')

#if deidentified (<10) make it 5
df$accumulated_vaccination_first_dose[is.na(as.numeric(df$accumulated_vaccination_first_dose))] = 5
df$accumulated_vaccination_second_dose[is.na(as.numeric(df$accumulated_vaccination_second_dose))] = 5
df$accumulated_vaccination_third_dose[is.na(as.numeric(df$accumulated_vaccination_third_dose))] = 5
df$accumulated_cases[is.na(as.numeric(df$accumulated_cases))] = 5
df$accumulated_recoveries[is.na(as.numeric(df$accumulated_recoveries))] = 5

df$time = as.numeric(as.Date(df$date))
df$time = df$time-min(df$time)


### order
df = df[order(df$code,df$time),]

# define vote groups
df$ProBibi = df$Likud+df$Shas+df$YahadutHatora+df$ZionutDatit
df$AntiBibi = df$Meretz+df$YeshAtid+df$Avoda+df$TikvaHadasha+df$IsraelBeitenu
df$Other = 1-df$Likud-df$Shas-df$Avoda-df$Meretz-df$YeshAtid-df$TikvaHadasha-df$IsraelBeitenu-df$YahadutHatora-df$ZionutDatit
df$OtherNoVote = 1-rowSums(df[,parties.all])

## log relative to ref of votes
df$AntiBibiL = log((df$AntiBibi+eps)/(df$ProBibi+eps))
df$OtherL = log((df$Other+0.0001)/(df$ProBibi+eps))

df$reference = df$Likud
df$ShasL = log((df$Shas+eps)/(df$reference+eps))
df$YahadutHatoraL = log((df$YahadutHatora+eps)/(df$reference+eps))
df$ZionutDatitL = log((df$ZionutDatit+eps)/(df$reference+eps))
df$KaholLavanL = log((df$KaholLavan+eps)/(df$reference+eps))
df$MeretzL = log((df$Meretz+eps)/(df$reference+eps))
df$YeshAtidL = log((df$YeshAtid+eps)/(df$reference+eps))
df$AvodaL = log((df$Avoda+eps)/(df$reference+eps))
df$YaminaL = log((df$Yamina+eps)/(df$reference+eps))
df$TikvaHadashaL = log((df$TikvaHadasha+eps)/(df$reference+eps))
df$IsraelBeitenuL = log((df$IsraelBeitenu+eps)/(df$reference+eps))
df$RaamL = log((df$Raam+eps)/(df$reference+eps))
df$AravitL = log((df$Aravit+eps)/(df$reference+eps))
df$OtherNoVoteL = log((df$OtherNoVote+eps)/(df$reference+eps))
df$LikudL = log((df$Likud+eps)/(df$reference+eps))

# change - date after coalition change
df$change = as.numeric(as.Date(df$date,origin='1899-12-21')>=as.Date('2021-06-13'))

df$code = factor(df$code)
df$AGE_SV = as.numeric(df$AGE_SV)
#df$accumulated_cases = as.numeric(df$accumulated_cases)

accumulated_cases1_start = as.numeric(df$accumulated_cases[df$time==0])
names(accumulated_cases1_start) = df$code[df$time==0]
accumulated_cases3_start = as.numeric(df$accumulated_cases[df$time==222])
names(accumulated_cases3_start) = df$code[df$time==222]

df$accumulated_cases1 = as.numeric(df$accumulated_cases)-accumulated_cases1_start[df$code]
df$accumulated_cases3 = as.numeric(df$accumulated_cases)-accumulated_cases3_start[df$code]
df$accumulated_cases1 = c(0,df$accumulated_cases1[-nrow(df)])
df$accumulated_cases3 = c(0,df$accumulated_cases3[-nrow(df)])

df$vax1 = as.numeric(df$accumulated_vaccination_first_dose)
df$vax3 = as.numeric(df$accumulated_vaccination_third_dose)

df$vax_potential1 = df$Nsize
vax_potential3 = as.numeric(df$accumulated_vaccination_first_dose[df$time==150])
names(vax_potential3) = df$code[df$time==150]
df$vax_potential3 = vax_potential3[df$code]

df$weights = df$Nsize/max(df$Nsize)

createDataTimeFromStart = function(df,n.months) {
  out1 = subset(df,df$time==n.months*30+30)
  out1$vax = out1$vax1/out1$vax_potential1
  out1$vax_potential = out1$vax_potential1/out1$Nsize
  out1$accumulated_cases = out1$accumulated_cases1/out1$Nsize
  
  out2 = subset(df,df$time==222+n.months*30+30)
  out2$vax = out2$vax3/out2$vax_potential3
  out2$vax_potential = out2$vax_potential3/out2$Nsize
  out2$accumulated_cases = out2$accumulated_cases3/out2$Nsize
  
  out = rbind(out1,out2)
  out
}

df = assignSector(df,2021)

df.general = subset(df,sector=='General')
#df.general = subset(df.general,OtherNoVote<0.5)

df1 = createDataTimeFromStart(df.general, n.months=1)
df2 = createDataTimeFromStart(df.general, n.months=2)
df3 = createDataTimeFromStart(df.general, n.months=3)

#### create models (not working now, need to choose how to do them)


model1 = lmer(vax ~ change*(SE_INDEX + age20 + age60 + accumulated_cases + vax_potential + OtherL + AntiBibiL) +  (1|code),
              weights = weights, data=df1)

model2 = lmer(vax ~ change*(SE_INDEX + age20 + age60 + accumulated_cases + vax_potential + OtherL + AntiBibiL) +  (1|code),
              weights = weights, data=df2)

model3 = lmer(vax ~ change*(SE_INDEX + age20 + age60 + accumulated_cases + vax_potential + OtherL + AntiBibiL) +  (1|code),
              weights = weights, data=df3)


sjPlot::tab_model(list(model1,model2,model3),digits=4,collapse.ci =T,  show.intercept = F,p.style='stars')

sjPlot::plot_model(model2,show.values=TRUE, show.p=TRUE)

sjPlot::plot_models(model3,model2,model1,show.values=TRUE, show.p=TRUE,grid = T)+geom_hline(yintercept = 0,linetype='dashed')


###### model per party

model3 = lmer(vax ~ 
                SE_INDEX + 
                age20 + 
                age60 +
                change*(

                    accumulated_cases + 
                    vax_potential + 
                    
                    ShasL + ShasL:change + 
                    YahadutHatoraL + YahadutHatoraL:change + 
                    ZionutDatitL + ZionutDatitL:change + 
                    KaholLavanL + KaholLavanL:change + 
                    MeretzL + MeretzL:change +
                    YeshAtidL + YeshAtidL:change + 
                    AvodaL + AvodaL:change + 
                    YaminaL + YaminaL:change + 
                    IsraelBeitenuL + IsraelBeitenuL:change + 
                    TikvaHadashaL + TikvaHadashaL:change +
                    RaamL + RaamL:change +
                    AravitL + AravitL:change +
                    OtherNoVoteL + OtherNoVoteL:change) + 
                (1|code), 
              weights = weights,data=df3)

sjPlot::plot_model(model3,show.values=TRUE, show.p=TRUE,
                   terms = c('change:ShasL','change:YahadutHatoraL','change:ZionutDatitL',
                             'change:YaminaL','change:TikvaHadashaL','change:IsraelBeitenuL',
                             'change:KaholLavanL','change:YeshAtidL','change:AvodaL',
                             'change:MeretzL','change:RaamL','change:AravitL','change:OtherNoVoteL'
                   )) + ylim(c(-0.025,0.025)) + geom_hline(yintercept = 0,linetype='dashed')


sjPlot::plot_models(model3,model2,model1,show.values=TRUE, show.p=TRUE,
                    terms = c('change:Shas','change:YahadutHatora','change:ZionutDatit',
                              'change:Yamina','change:TikvaHadasha','change:IsraelBeitenu',
                              'change:KaholLavan','change:YeshAtid','change:Avoda',
                              'change:Meretz','change:Raam','change:Aravit'
                    ),
                    axis.lim = c(-0.1,0.1))+
  geom_hline(yintercept = 0,linetype='dashed')

