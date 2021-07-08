

#### conect to SOAR package
fstoreconnect = function(subdir){
  oldLC = Sys.getenv("R_LOCAL_CACHE", unset = ".R_Cache")
  Sys.setenv(R_LOCAL_CACHe = subdir)
}
fstoreconnect("rstore")
tmp = Objects()

map.func2 = function(x, y = 1){
  map = x %>% 
    sapply(FUN = function(x){strsplit(x, '[.]')[[1]][y]}) %>% 
    # sub("[[:punct:]]", '',.) %>% 
    sub(" ",'',.) %>% as.vector() #%>% as.numeric()
  return(map)
}


map.name = function(x, y = 1:4){
  map = x %>% 
    sapply(FUN = function(x){strsplit(x, '[_]')[[1]][y]}) %>% 
    # sub("[[:punct:]]", '',.) %>% 
    sub(" ",'',.) %>% as.vector() #%>% as.numeric()
  return(map)
}
###
freq.encode = function(x ,xnew = x){
  if(is.factor(x) || is.character(x)){
    return(as.numeric(factor(xnew, levels = names(sort(table(x))))))
  }else{
    return(approxfun(density(x[!is.na(x)],n=length(x)/100))(xnew))
  }
}

nd = function(red, nir) {
  n = (nir-red)/(nir+red)
  return(n)
}

quantile5 = function(x){
  q = quantile(x,0.05,na.rm = T)
  return(q)
}
quantile25 = function(x){
  q = quantile(x,0.25,na.rm = T)
  return(q)
}
quantile75 = function(x){
  q = quantile(x,0.75,na.rm = T)
  return(q)
}

quantile95 = function(x){
  q = quantile(x,0.95,na.rm = T)
  return(q)
}

collectStats <- function(x, quant = c(0.1,0.2,0.5,0.8, 0.95,0.9)){
  data.frame(t(quantile(x,quant))) %>%
    set_names(paste0("Q",quant))   %>%
    mutate(mean = mean(x), sd = sd(x),
      min  = min(x), max = max(x),
      IQR  = IQR(x), RMS = sqrt(mean(x^2)))
}

# mean_abs_change = function(x){
#   ch = mean(abs(diff(x)))
#   return(ch)
# }
# 
# min_abs= function(x){
#   ch = min(abs(x))
#   return(ch)
# }
# 
# max_abs = function(x){
#   ch = max(abs(x))
#   return(ch)
# }
# SRAV = function(x){
#   SRA = sum(sqrt(abs(x)))
#   ch = (SRA / length(x))^2
#   # ch= (ch)^2
#   return(ch)
# }
#ndvi 



#### my f2 cnt
my.f2cnt = function(th2,vn1,vn2, filter = TRUE){
  data = data.frame(f1= th2[,vn1],f2=th2[,vn2], filter = filter)
  colnames(data) = c("f1","f2","filter")
  sum1 = sqldf::sqldf("select f1,f2, count(*) as cnt from data where filter=1 group by 1,2")
  tmp = sqldf::sqldf("select b.cnt from data a left join sum1 b on a.f1=b.f1 and a.f2=b.f2")
  tmp$cnt[is.na(tmp$cnt)] = 0
  return(tmp$cnt)
  
}


#### my f2 cnt
my.f3cnt = function(th2,vn1,vn2,vn3, filter = TRUE){
  data = data.frame(f1= th2[,vn1],f2=th2[,vn2],f3=th2[,vn3], filter = filter)
  colnames(data) = c("f1","f2","f3","filter")
  sum1 = sqldf::sqldf("select f1,f2,f3, count(*) as cnt from data where filter=1 group by 1,2,3")
  tmp = sqldf::sqldf("select b.cnt from data a left join sum1 b on a.f1=b.f1 and a.f2=b.f2 and a.f3=b.f3")
  tmp$cnt[is.na(tmp$cnt)] = 0
  return(tmp$cnt)
  
}


# CUBIST training function for stacking
cubist.model <- function(intrain, label, intest){
  
  set.seed(1235)
  cub.mod <- cubist(x = as.matrix(intrain), y = label, committees = cub.param[1],
    neighbors = cub.param[2],
    control = cubistControl(unbiased = T, rules = cub.param[3], 
      sample = 0)) 
  
  pred <- predict(cub.mod, newdata = as.matrix(intest[,colnames(intrain)]))
  
  return(pred)
}


group_pr = function(x){
  if(x<=50){
    return("Bin1")
  }else if(x>50 & x<=100){
    return("Bin2")
  }else if(x>100 & x<=150){
    return("Bin3")
  }else if(x>150){
    return("Bin4")
    }
}
