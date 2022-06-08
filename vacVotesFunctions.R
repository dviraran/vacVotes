
readVaccination = function(file,date.vac2,date.vac3) {
  
  v = read.csv(file)
  
  A = v$date==date.vac2
  B = v$date==date.vac3
  vac = data.frame(town_code=v$town_code[A],town=v$town[A],agas_code=v$agas_code[A])
  
  vac$dose2=as.numeric(v$accumulated_vaccination_second_dose[A])
  vac$dose3=as.numeric(v$accumulated_vaccination_third_dose[B])
  vac$recovered2=as.numeric(v$accumulated_recoveries[A])
  vac$recovered3=as.numeric(v$accumulated_recoveries[B])
  vac$tests2=as.numeric(v$accumulated_diagnostic_tests[A])
  vac$tests3=as.numeric(v$accumulated_diagnostic_tests[B])
  vac$recovered.after = as.numeric(vac$recovered3)-as.numeric(vac$recovered2)
  vac$tests.after = as.numeric(vac$tests3)-as.numeric(vac$tests2)
  
  agas = str_pad(as.numeric(trimws(vac$agas_code)),4,pad = "0")
  agas[is.na(agas)]='0000'
  vac$code = paste0(vac$town_code,agas)
  vac
}

readSE = function(working.dir) {
  se1 = openxlsx::read.xlsx(paste0(working.dir,'data/lamas-t12.xlsx'))
  se2 = openxlsx::read.xlsx(paste0(working.dir,'data/lamas-t01.xlsx'))
  se3 = openxlsx::read.xlsx(paste0(working.dir,'data/lamas-t07.xlsx'))
  
  se = rbind(se1,se2[se2$TOWN_CODE %in% setdiff(se2$TOWN_CODE,se1$TOWN_CODE),])
  se = rbind.fill(se,se3[se3$TOWN_CODE %in% setdiff(se3$TOWN_CODE,se$TOWN_CODE),])
  
  agas = str_pad(as.numeric(trimws(se$AGAS_CODE)),4,pad = "0")
  agas[is.na(agas)]='0000'
  se$code = paste0(se$TOWN_CODE,agas)
  se
}

readVotes = function(working.dir,election=2020) {
  
  if (election==2021) {
    print(election)
    votes = openxlsx::read.xlsx(paste0(working.dir,'data/bechirot2021.xlsx'))
    votes$kalfi.code = paste0((votes$town_code),'.',unlist(lapply(as.character(votes$Kalfi_id),FUN=function(x) strsplit(x,'[.]')[[1]][1])))
    
    map.votes.stat = openxlsx::read.xlsx(paste0(working.dir,'data/kalpi_january2021_stat2011_6010ד.xlsx'))
    kalfi.map = openxlsx::read.xlsx(paste0(working.dir,'data/kalfi_2021_transform.xlsx'))
    for (i in 1:ncol(kalfi.map)) {
      id = map.votes.stat$town_code==kalfi.map$town_code[i] & map.votes.stat$kalfi_id==kalfi.map$orig_id[i]
      if(sum(id)==1) {
        map.votes.stat$kalfi_id[id] =  kalfi.map$new_id[i]
      }
      if (sum(id)==0) {
        print(i)
      }
    }
    map.votes.stat$kalfi.code = paste0(map.votes.stat$town_code,'.',map.votes.stat$kalfi_id)
  } else if (election==2020) {
    print(election)

    votes = openxlsx::read.xlsx(paste0(working.dir,'data/bechirot2020.xlsx'))
    votes$kalfi.code = paste0((votes$town_code),'.',unlist(lapply(as.character(votes$Kalfi_id),FUN=function(x) strsplit(x,'[.]')[[1]][1])))
    map.votes.stat = openxlsx::read.xlsx(paste0(working.dir,'data/kalpi_March2020_stat2011.xlsx'))
    map.votes.stat$kalfi.code = paste0(map.votes.stat$semel_yishuv,'.',map.votes.stat$kalpi2019)
    map.votes.stat$code = map.votes.stat$yishuv_stat2011
  }
  
  votes = merge(votes,map.votes.stat,by='kalfi.code')
  votes
}

assignSector = function(m,election) {
  
  cities = read.csv(paste0(working.dir,'data/cities.csv'))
  
  arabs = unique(cities$city[cities$דסק ==  'דסק ערבי'])
  ultraorthodox = unique(cities$city[cities$דסק ==  'דסק חרדי'])
  
  m$sector = 'General'
  m$sector[m$town %in% arabs] = 'Arab'
  m$sector[m$town %in% ultraorthodox] = 'Haredi'
  
  if (election==2021) {
    arabs = m$Raam+m$Aravit>0.25
    
  } else if (election==2020) {
    arabs = m$ReshimaAravit>0.25
  }
  
  ultraorthodox = m$YahadutHatora+m$Shas>0.25
  
  
  m$sector[arabs] = 'Arab'
  m$sector[ultraorthodox] = 'Haredi'
  
  mahoz = cities$מחוז
  mahoz = mapvalues(mahoz,from = unique(mahoz), to = c('Missing','Jerusalem','North','South','Haifa','Ashkelon','Merkaz','Tel Aviv','Unkown','empty'))
  m$region = 'Missing'
  umahoz = unique(mahoz)
  for (i in 1:length(umahoz)) {
    
    m$region[m$town %in% cities$city[mahoz==umahoz[i]]] = umahoz[i]
    
  }
  
  m
}

toCityLevel = function(vac,votes) {
  
  socioeconomic = read.csv(paste0(working.dir,'data/socioeconomic.cities.csv'))
  
  
  vac.city = ddply(vac,"town",numcolwise(sum))
  
  mcity = merge(vac.city,socioeconomic,by.x='town',by.y='city')
  
  votes2 = votes[,c('town_code','BZB','N',intersect(parties,colnames(votes)))]
  votes2 = ddply(votes2,"town_code",numcolwise(sum))
  votes2[,intersect(parties,colnames(votes))] = votes2[,intersect(parties,colnames(votes))]/votes2$BZB
  
  mcity = merge(mcity,votes2,by='town_code')
  
  
  mcity$dose2.perc = as.numeric(mcity$dose2)/mcity$BZB
  mcity$dose3.perc = as.numeric(mcity$dose3)/mcity$BZB
  
  mcity$diff = 100*(mcity$dose2.perc-mcity$dose3.perc)
  mcity$avoid = 100*(1-mcity$dose3.perc/mcity$dose2.perc)
  mcity$dose3.dose2 = (100-mcity$avoid)/100
  
  mcity
}

getAreasAges = function(working.dir) {
  
  ages = openxlsx::read.xlsx(paste0(working.dir,'data/deciles.xlsx'),1)
  colnames(ages) = c('town_ages','town_code_ages','code','Nsize','p10','p20','p30','p40','p50','p60','p70','p80','p90')
  
  for (i in 1:nrow(ages)) {
    if (ages[i,13]>60) {
      j = min(which(ages[i,5:13]>50))
      percent = (ages[i,(j+5)]-ages[i,(j+5)-1])/10
      ages$age60[i] = min(100,100-(j*10 + (60-ages[i,(j+5)-1])/percent))
    } else if (ages[i,13]>50) {
      ages$age60[i] = ages[i,13]-50
    } else {
      ages$age60[i] = 0
    }
  }
  
  for (i in 1:nrow(ages)) {
    if (ages[i,13]>20) {
      j = min(which(ages[i,5:13]>20))
      percent = (ages[i,(j+5)]-ages[i,(j+5)-1])/10
      ages$age20[i] = min(100,100-(j*10 + (20-ages[i,(j+5)-1])/percent))
    } else if (ages[i,13]>20) {
      ages$age20[i] = ages[i,13]-20
    } else {
      ages$age20[i] = 0
    }
  }
  
  ages$age20 = (ages$age20-ages$age60)/100
  ages$age60 = ages$age60/100
  
  ages
}