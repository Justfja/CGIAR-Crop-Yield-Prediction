options(warn = -1)

library(lightgbm)
library(plyr)
library(tidyverse)
library(raster)
library(caret)
library(SOAR)
library(reticulate)
np = import("numpy")

source("utils.R")
#####
path.dir = getwd()
data.dir = paste0(path.dir,"/Data")

######
#dir.create(paste0(path.dir,"/tmp"))
subm.dir = paste0(path.dir,"/subm")
save.files.dir = paste0(path.dir,"/tmp")


df = read.csv(paste0(data.dir,"/train.csv"))
samp = read.csv(paste0(data.dir,"/SampleSubmission.csv"))
info = read.csv(paste0(data.dir,"/fields_w_additional_info.csv"))
year = read.csv(paste0(data.dir,"/test_field_ids_with_year.csv"))

inf = info %>% select(starts_with("climate"),Field_ID)
inf = pivot_longer(inf,cols = starts_with("clima"))
inf.m = matrix(unlist(purrr::map(ff$name,map.name)),ncol = 4, byrow = T)
colnames(inf.m) = c("type","year","month","clim_prop")
inf = data.frame(inf,inf.m[,2:ncol(inf.m)])
inf = inf %>% mutate(month= as.numeric(month)-1)
inf$month = inf$month -1


#df = df %>% filter(Yield>=1 &Yield<9)
df = df %>% filter(Quality != 2)
#df = df %>% filter(Yield<9)
train.id = df$Field_ID
label = df$Yield
test.id = samp$Field_ID

##
Store(label)
Store(train.id)
Store(test.id)

#3@df= df %>% filter(Year != 2018)
# ####
# train.files <- sapply(list.files(paste0(data.dir,"/image_arrays_train")),map.func2) %>% as.character()
# test.files <- sapply(list.files(paste0(data.dir,"/image_arrays_test")),map.func2) %>% as.character()

###
ba = c("S2_B1","S2_B2","S2_B3","S2_B4","S2_B5","S2_B6","S2_B7","S2_B8","S2_B8A","S2_B9","S2_B10","S2_B11","S2_B12","S2_QA10","S2_QA20","S2_QA60","CLIM_aet","CLIM_def","CLIM_pdsi","CLIM_pet","CLIM_pr","CLIM_ro","CLIM_soil","CLIM_srad","CLIM_swe","CLIM_tmmn","CLIM_tmmx","CLIM_vap","CLIM_vpd","CLIM_vs")
all = c()
for(i in 0:11){
all = c(all,paste0(i,"_",ba))
}
df_train = data.frame()
for(i in 1:length(df$Field_ID)){
  x = df$Field_ID[i] %>% as.character()
  aa = np$load(paste0(data.dir,"/image_arrays_train/",x,".npy"))
  stats = collectStats(aa)
#  colnames(stats) = paste0("Stats_",colnames(stats))
  #m = matrix(aa, ncol = (41*41))
  dd = matrix()
  for(j in 2:7){
    bns = paste0(j,"_",ba)
    idxs = np$where(np$isin(all,bns))
    vs = data.frame(matrix(aa[idxs[[1]]+1,20,20], ncol = 30))
    colnames(vs) = bns
    # vs2 = data.frame(matrix(m[idxs[[1]]+1,841], ncol = 30))
    # colnames(vs2) = paste0("new_",bns)
    ndvi = (vs[8] - vs[4])/ (vs[8] + vs[4])
    colnames(ndvi) = paste0(j,"_NDVI")
    cc = ((vs[8]-vs[5])/(vs[8]+vs[5]))/((vs[8]-vs[4])/(vs[8]+vs[4]))
    colnames(cc) = paste0(j,"_CC")
    rel = vs[28] / (vs[28]+vs[29]); colnames(rel) = paste0(j,"_RH")
    ah = (vs[26]+vs[27]/2); colnames(ah) = paste0(j,"_Temp")
    et = (vs[17]+vs[20])/2;  colnames(et) = paste0(j,"_ET")
    bi = (vs[21] * vs[30])/100; colnames(bi) = paste0(j,"_GVI")
    bi2 = (1/vs[3]) - (1/vs[5]); colnames(bi2) = paste0(j,"_AR")
    bi3 = vs[8]  *vs[4] / (vs[3]^2);colnames(bi3) = paste0(j,"_CVI")
    br= ((1/vs[3])-(1/vs[5]))/(vs[8]);colnames(br) = paste0(j,"_BR")
    gr = (vs[7]/vs[3])^(-1);colnames(gr) = paste0(j,"_GR")
    ci = (vs[4] - vs[2])/ vs[4];colnames(ci) = paste0(j,"_CI")
    evi = 2.5*(vs[8]- vs[4])/ ((vs[8] + 6.0*vs[4] - 7.5*vs[2]) + 1.0)  
    colnames(evi) = paste0(j,"_EVI")
    evi2 = 2.4 * (vs[8] - vs[4]) / (vs[8] + vs[4] + 1.0)
    colnames(evi2) = paste0(j,"_EVI2")
    avi = (vs[8] * (1 - vs[4]) * (vs[8] - vs[4]))
    colnames(avi) = paste0(j,"_AVI")
    tci =1.2 * (vs[5] - vs[3])- 1.5 *(vs[4] - vs[3])*sqrt(vs[5]/vs[4])
    colnames(tci) = paste0(j,"_TCI")
    tvi = sqrt((((vs[4] - vs[3]) / (vs[4] + vs[3]))) + 0.5)
    colnames(tvi) = paste0(j,"_TVI")
    cred = (vs[7]/vs[5])^(-1);colnames(cred) = paste0(j,"_CRED")
    RECI = (vs[8] / vs[4])-1;colnames(RECI) = paste0(j,"_RECI")
    RECI2 = (vs[7]/vs[3])-1;colnames(RECI2) = paste0(j,"_RECI2")
    # epi = (0.3 * vs[4]) / (vs[3] * vs[5])^0.3
    # colnames(epi) = paste0(j,"_EPI")
    # ivi =  (vs[8] - 0.8) / (0.4 * vs[4])
    # colnames(ivi) = paste0(j,"_IVI")
    # lci = (vs[8] - vs[5]) / (vs[8] + vs[4])
    # colnames(lci) = paste0(j,"_LCI")
    # int = (1.0 / (30.5)) * (vs[4] + vs[3] + vs[2])
    # colnames(int) = paste0(j,"_INT")
    # eq = 2*vs[10]-vs[4];colnames(eq) = paste0(j,"_EQ")
    # eq2 = (vs[8] / vs[5])-1;colnames(eq2) = paste0(j,"_EQ2")
    # eq3 = (0.1*vs[8]-vs[2])/(0.1*vs[8]+vs[2])
    # colnames(eq3) = paste0(j,"_EQ3")
    # #b = vs[8]  / vs[3];colnames(b) = paste0(j,"_GSAN")
    # d = 1.5*(vs[8]-vs[4])/(vs[8]+vs[4]+0.5);colnames(d)= paste0(j,"_MSS")
    #   ndsi = (vs[7] - vs[4])/vs[7] + vs[4];colnames(ndsi) = paste0(j,"_NDSI")
    #   ndwi = (vs[3] - vs[8]) / (vs[3] + vs[8])
    #  colnames(ndwi) = paste0(j,"_NDWI")
    dd = cbind(dd,vs,ndvi,cc,rel,ah,et,bi,bi2,bi3,br,gr,ci,evi,evi2,avi,tci,tvi,cred,RECI,RECI2)
    
    
  }
  dd = cbind(dd,stats)
  df_train = rbind(df_train,dd)
}
#NDRE	(NIR1-RE)/(NIR1+RE)	

##############

df_test = data.frame()
for(i in 1:length(samp$Field_ID)){
  x = samp$Field_ID[i] %>% as.character()
  aa = np$load(paste0(data.dir,"/image_arrays_test/",x,".npy"))
 # m = matrix(aa, ncol = (41*41))
  stats = collectStats(aa)
 # colnames(stats) = paste0("Stats_",colnames(stats))
  dd = matrix()
  for(j in 2:7){
    stats = collectStats(aa)
    bns = paste0(j,"_",ba)
    idxs = np$where(np$isin(all,bns))
    vs = data.frame(matrix(aa[idxs[[1]]+1,20,20], ncol = 30))
    colnames(vs) = bns
    # vs2 = data.frame(matrix(m[idxs[[1]]+1,841], ncol = 30))
    # colnames(vs2) = paste0("new_",bns)
    ndvi = (vs[8] - vs[4])/ (vs[8] + vs[4])
    colnames(ndvi) = paste0(j,"_NDVI")
    cc = ((vs[8]-vs[5])/(vs[8]+vs[5]))/((vs[8]-vs[4])/(vs[8]+vs[4]))
    colnames(cc) = paste0(j,"_CC")
    rel = vs[28] / (vs[28]+vs[29]); colnames(rel) = paste0(j,"_RH")
    ah = (vs[26]+vs[27]/2); colnames(ah) = paste0(j,"_Temp")
    et = (vs[17]+vs[20])/2;  colnames(et) = paste0(j,"_ET")
    bi = (vs[21] * vs[30])/100; colnames(bi) = paste0(j,"_GVI")
    bi2 = (1/vs[3]) - (1/vs[5]); colnames(bi2) = paste0(j,"_AR")
    bi3 = vs[8]  *vs[4] / (vs[3]^2);colnames(bi3) = paste0(j,"_CVI")
    br= ((1/vs[3])-(1/vs[5]))/(vs[8]);colnames(br) = paste0(j,"_BR")
    gr = (vs[7]/vs[3])^(-1);colnames(gr) = paste0(j,"_GR")
    ci = (vs[4] - vs[2])/ vs[4];colnames(ci) = paste0(j,"_CI")
    evi = 2.5*(vs[8]- vs[4])/ ((vs[8] + 6.0*vs[4] - 7.5*vs[2]) + 1.0)  
    colnames(evi) = paste0(j,"_EVI")
    evi2 = 2.4 * (vs[8] - vs[4]) / (vs[8] + vs[4] + 1.0)
    colnames(evi2) = paste0(j,"_EVI2")
    avi = (vs[8] * (1 - vs[4]) * (vs[8] - vs[4]))
    colnames(avi) = paste0(j,"_AVI")
    tci =1.2 * (vs[5] - vs[3])- 1.5 *(vs[4] - vs[3])*sqrt(vs[5]/vs[4])
    colnames(tci) = paste0(j,"_TCI")
    tvi = sqrt((((vs[4] - vs[3]) / (vs[4] + vs[3]))) + 0.5)
    colnames(tvi) = paste0(j,"_TVI")
    cred = (vs[7]/vs[5])^(-1);colnames(cred) = paste0(j,"_CRED")
    RECI = (vs[8] / vs[4])-1;colnames(RECI) = paste0(j,"_RECI")
    RECI2 = (vs[7]/vs[3])-1;colnames(RECI2) = paste0(j,"_RECI2")
    # epi = (0.3 * vs[4]) / (vs[3] * vs[5])^0.3
    # colnames(epi) = paste0(j,"_EPI")
    # ivi =  (vs[8] - 0.8) / (0.4 * vs[4])
    # colnames(ivi) = paste0(j,"_IVI")
    # lci = (vs[8] - vs[5]) / (vs[8] + vs[4])
    # colnames(lci) = paste0(j,"_LCI")
    # int = (1.0 / (30.5)) * (vs[4] + vs[3] + vs[2])
    # colnames(int) = paste0(j,"_INT")
    # eq = 2*vs[10]-vs[4];colnames(eq) = paste0(j,"_EQ")
    # eq2 = (vs[8] / vs[5])-1;colnames(eq2) = paste0(j,"_EQ2")
    # eq3 = (0.1*vs[8]-vs[2])/(0.1*vs[8]+vs[2])
    # colnames(eq3) = paste0(j,"_EQ3")
    # #b = vs[8]  / vs[3];colnames(b) = paste0(j,"_GSAN")
    # d = 1.5*(vs[8]-vs[4])/(vs[8]+vs[4]+0.5);colnames(d)= paste0(j,"_MSS")
    #   ndsi = (vs[7] - vs[4])/vs[7] + vs[4];colnames(ndsi) = paste0(j,"_NDSI")
    #   ndwi = (vs[3] - vs[8]) / (vs[3] + vs[8])
    #  colnames(ndwi) = paste0(j,"_NDWI")
    dd = cbind(dd,vs,ndvi,cc,rel,ah,et,bi,bi2,bi3,br,gr,ci,evi,evi2,avi,tci,tvi,cred,RECI,RECI2)
    
  }
  dd = cbind(dd,stats)
  df_test = rbind(df_test,dd)
}


df_train$Field_ID = train.id
df_test$Field_ID = test.id
info = info %>% select(Field_ID,starts_with("soil"))
#info$row_mean = rowMeans(info[,-1])
df_train = df_train %>% left_join(info, by = "Field_ID")
df_test = df_test %>% left_join(info)

df_train$Field_ID = NULL
# train.id = df$Field_ID
# label = df$Yield
# test.id = samp$Field_ID
# 
# ##
# Store(label)
# Store(train.id)
# Store(test.id)
# 
# df_train$l = label
# df_train = df_train %>% filter(l>=1)
# df_train = df_train %>% filter(l<9)
# label = df_train$l
# df_train$l = NULL

###
# m2 = df_test %>% as.data.frame() %>% select(contains("pr")) %>%
#   mutate(pr_min = apply(.,1,FUN = min),
#     pr_max = apply(.,1,FUN = max),
#     pr_mean = apply(.,1,FUN = sd)) %>%
#   select(pr_min,pr_max,pr_mean)
# 
# m = df_train %>% as.data.frame() %>% select(contains("pr")) %>%
#   mutate(pr_min = apply(.,1,FUN = min),
#     pr_max = apply(.,1,FUN = max),
#     pr_mean = apply(.,1,FUN = sd)) %>%
#   select(pr_min,pr_max,pr_mean)



###
m = round(rowMeans(df_train %>% select(contains("pr"))))
m2 = round(rowMeans(df_test %>% select(contains("pr"))))
df2 = cbind(df,m)
df2= df2 %>% group_by(m) %>% summarise(mim = min(Yield),mam = max(Yield),msd = sd(Yield))

df_train$m =m
df_test$m =m2

df_train = df_train %>% left_join(df2)
df_test = df_test %>% left_join(df2)

df_train = df_train %>% select(-ends_with(c("vap","vpd","B8A","Temp","swe","pdsi")))

df_train$m=NULL
df_test$m = NULL

#xgboost
df_train = df_train %>% select(-ends_with(c("vap","vpd","B8A","Temp","swe","pdsi","B10","B11")))

# 
# ####
# m = cbind(round(rowMeans(df_train %>% select(contains("pr")))),round(rowMeans(df_train %>% select(contains("tmmx")))))
# colnames(m) = c("m","m2")
# m2 = cbind(round(rowMeans(df_test %>% select(contains("pr")))),round(rowMeans(df_test %>% select(contains("tmmx")))))
# colnames(m2) = c("m","m2")
# df2 = cbind(df,m)
# df2= df2 %>% group_by(m,m2) %>% summarise(mim2 = min(Yield),mam2 = max(Yield),msd2 = sd(Yield))
# 
# df_train =cbind(df_train,m)
# df_test =cbind(df_test,m2)
# 
# df_train = df_train %>% left_join(df2)
# df_test = df_test %>% left_join(df2)
# 
# df_train = df_train %>% select(-ends_with(c("vap","vpd","B8A","Temp")))
# 

## humdity, temp, wind,precipitation,water quality,solar radiation,soil moisture,soil quality, fallow length, rainfall, topography
#Wind Chill = (10*sqrt(df_train$wind_spd) - df_train$wind_spd+10.5) * (33- df_train$temp)
# l = 2.453e6
# rv = 461
# df['eqn'] = (l / rv) * ((1/273) - (1 / (df['temp'] + 273.15)))
##
# data["NDVI"]=(data["B08"]-data["B04"])/(data["B08"]+data["B04"])
# data["NDSI"]=(data["B07"]-data["B04"])/(data["B07"]+data["B04"])
# ndvi = (b8 - b4) / (b8 + b4)
# gndvi = (b8 - b3) / (b8 + b3)
# evi = 2.5 * (b8 - b4) / ((b8 + 6.0 * b4 - 7.5 * b2) + 1.0)    
# evi2 = 2.4 * (b8 - b4) / (b8 + b4 + 1.0)
# avi = (b8 * (1 - b4) * (b8 - b4))
# bsi = ((b11 + b4) - (b8 + b2)) / ((b11 + b4) + (b8 + b2))
# si = ((1 - b2) * (1 - b3) * (1 - b4))
# ndwi = (b3 - b8) / (b3 + b8)
# ndmi = (b8 - b11) / (b8 + b11)
# npcri = (b4 - b2) / (b4 + b2) 
# SAVI :1.5*(B8A-B04)/(B8A+B04+0.5)
#RECI :(B08/B04)-1
# RECI :(B07/B03)-1
# MSI :B11/B8A
# NDRE :(B09-B05)/(B09+B05)
# ARVI :(B08-(B04-1*(B02-B04)))/(B08+(B04-1*(B02-B04)))
# FIDET : B12/(B8A*B09)
#SIPI : (B08-B02)/(B08-B04)
### qq = c("(Red-Blue)/Green")
#Chlred-edge
### fieldimager
# BI	sqrt((R^2+G^2+B^2)/3)	
# BIM	sqrt((R*2+G*2+B*2)/3)	
# SCI	(R-G)/(R+G)	
# GLI	(2*G-R-B)/(2*G+R+B)	
# HI	(2*R-G-B)/(G-B)	
# NGRDI	(G-R)/(G+R)	
# SI	(R-B)/(R+B)	
# VARI	(G-R)/(G+R-B)
# HUE	atan(2*(B-G-R)/30.5*(G-R))	
# BGI	B/G	
# PSRI	(R-G)/(RE)	
# NDVI	(NIR1-R)/(NIR1+R)	
# GNDVI	(NIR1-G)/(NIR1+G)	
# RVI	NIR1/R	
# NDRE	(NIR1-RE)/(NIR1+RE)	
# TVI	(0.5*(120*(NIR1-G)-200*(R-G)))	
# CVI	(NIR1*R)/(G^2)	
# CIG	(NIR1/G)-1	
# CIRE	(NIR1/RE)-1	
# DVI	NIR1-RE	
# EVI	2.5*(NIR1-R)/(NIR1+6*R-7.5*B+1)	

## MTCI
#Exblue train- (2B2- B3 - B4)Fairly good
#Exgreen train- (2B3- B4 - B2)Fairly good
###

########
#@@@@  TODO re-arrange by field id
########


ftrs = data.frame(
  type = unlist(lapply(df_train[1:length(train.id),],class)),
  n.unique = unlist(lapply(df_train[1:length(train.id),],function(x)length(unique(x)))),
  f.missing = unlist(lapply(df_train[1:length(train.id),],function(x)mean(is.na(x)))),
  spear.cor = unlist(lapply(df_train[1:length(train.id),],function(x){idx = !is.na(x);
  if(is.factor(x)) x = as.numeric(x);
  if(is.character(x)) x = as.numeric(as.factor(x));
  if(is.integer(x)) x = as.numeric(x);
  if(is.logical(x)) x = as.numeric(x);
  cor(x[idx],y = label[idx], method = "spearman")
  }))
)


ftrs$name = rownames(ftrs)
ftrs = ftrs %>% drop_na()
df_train = df_train[,names(df_train) %in% ftrs$name]


########
##
#######
df_train = df[1:length(train.id),]
df_test = df[(length(train.id)+1):nrow(df),]


m = df_train %>% select(contains("soil"))


frac = 0.3
frac2 = 0.5
frac3 = 1
set.seed(6043)
dt = rbindlist(list(df[is.na(label) | label==7], 
  df[label==1][sample(1:.N,.N*frac)],
  df[label==2][sample(1:.N,.N*frac3)],
  df[label==3][sample(1:.N,.N*frac3)],
  df[label==4][sample(1:.N,.N*frac3)],
  df[label==5][sample(1:.N,.N*frac3)],
  df[label==6][sample(1:.N,.N*frac3)]))


#########
df.dat = rbind(df_train,df_test[,colnames(df_train)])
sumsq = NULL
for (i in 1:15) {
  set.seed(1234)
 sumsq[i] = sum(kmeans(df.dat,centers = i, iter.max = 1000,
                       algorithm = "Lloyd")$withinss)
}
plot(1:15,sumsq,type= "b")
###
set.seed(1234)
kmns = kmeans(df.dat,6,iter.max = 1000,
              algorithm = "Lloyd",trace = T)

cnts = kmns$centers
kmeans.distance = NULL
for (i in 1:nrow(cnts)) {
  kmeans.distance = cbind(kmeans.distance, sqrt(colSums((t(df.dat)-unlist(cnts[i,]))^2)))

}
table(kmns$cluster)
save(kmeans.distance, file = paste(save.files.dir,"/kmeans_features.RData"))


# ndvi <- VI(, 8, 4)  band 8 - band4



#####
duplicated.x = findCorrelation(cor(randomForest::na.roughfix(df_train)),
                               cutoff = 0.999, names = T, verbose = F)
length(duplicated.x)
df_train = df_train[, !names(df_train) %in% duplicated.x]

#### PCA Reduction













### Kmeans on Top features.
library(Rtsne)
#dat = df %>% dplyr::select(starts_with("stack"))
set.seed(1234)
tsne.res = Rtsne(df.dat, check_duplicates = T, max_iter=1000,
                 perplexity = 50, #pca = TRUE,
                 theta = 0.5, dims = 2, verbose =T)
save(tsne.res, file = paste0(save.files.dir,"/tsne.RData"))


### Kmeans on top features
sumsq = NULL
for (i in 1:15) {
  set.seed(1234)
  sumsq[i] = sum(kmeans(umap_data2,centers = i, iter.max = 1000,
                        algorithm = "Lloyd")$withinss)
}
plot(1:15,sumsq,type= "b")
###
set.seed(1234)
kmns = kmeans(dr.dat,8,nstart = 17,iter.max = 1000,
              algorithm = "Lloyd",trace = T)







# Ht = function(DF1, ntimes = 1){
#   w = function(k){
#     s1 = dwt(k, filter = "haar")
#     return(s1@V[[1]])
#   }
#   smt = DF1
#   for (i in 1:ntimes) {
#     smt = t(apply(smt,1,w))
#   }
#   return(data.frame(smt))
# }
# 
# a = Ht(as.matrix(dat,9))

dat = df %>% dplyr::select(ends_with("_5"))
der = function(x,d=1){
  df = t(diff(t(x), differences = d))
  return(df)
}

a5 = der(a,1)
colnames(a5)  = paste0("der5_",1:ncol(a5))
set.seed(1234)
s = ssvd(as.matrix(q), k = 10,n =10,maxit = 1000)
colnames(s$u) = paste0("svd",1:ncol(s$u))
svd.features = data.frame(s$u)



q = rbind(df_train,df_test)





### AUToencoder
library(h2o)
h2o.init(nthreads = -1)
dr.dat.h2o = as.h2o(umap_data2)
m.aec = h2o.deeplearning(
  x = names(dr.dat),
  training_frame= dr.dat.h2o,
  autoencoder = T,
  activation = "Tanh",
  model_id = "autoenc1",
  hidden = c(5,2,5),
  epochs =100,
  seed = 1234
)

deep.fea = as.data.frame(h2o.deepfeatures(m.aec, dr.dat.h2o, layer = 2))
save(deep.fea, file = paste0(save.files.dir,"/autoenc_output.RData"))

h2o.shutdown()

prostate.anon = h2o.anomaly(m.aec, dr.dat.h2o, per_feature=FALSE)
head(prostate.anon)
err <- as.data.frame(prostate.anon)



### DBSCAN
set.seed(1234)
# fpc package
res.fpc <- fpc::dbscan(df.dat, eps = 4, MinPts = 8)
summary(as.factor(res.fpc$cluster))
save(res.fpc, file = paste0(save.files.dir,"/dbscan_clus.RData"))



#############
#### TWO WAY INTERACTIONS
#############
num.features = rownames(ftrs)[ftrs$type != 'factor'][1:6]
######
start.time = Sys.time()
# Split the data set
#df_train <- df[1:6249,]
df_train$y = label
num.pairs = data.frame(t(combn(as.character(num.features), 2)), stringsAsFactors = F)
num.pairs$pv = NA
Store(num.pairs)
for (i in 1:nrow(num.pairs)) {
  frmla.alte=as.formula(paste('y ~', num.pairs$X1[i], '+', num.pairs$X2[i]))
  frmla.mul=as.formula(paste('y ~', num.pairs$X1[i], '*', num.pairs$X2[i]))
  num.pairs$pv[i] = anova(glm(frmla.alte, df_train, family = "gaussian"),
    glm(frmla.mul, df_train, family = "gaussian"),
    test = 'Chisq')$`Pr(>Chi)`[2]
}
head(num.pairs)
gc(reset = T)

total.time = Sys.time() - start.time
total.time

num.pairs$log = log(num.pairs$pv)
nn = num.pairs %>% filter(log < -50)

##########
start.time = Sys.time()
w = data.frame(id =c(1:2977))
w2 = data.frame(id =c(1:1055))
for (i in 1:nrow(nn)){
  name = paste0(nn$X1[i], "_", nn$X2[i], "_cnt2") 
  #  tmp = my.f2cnt(df, nn$X1[i], nn$X2[i])
  
  # df[,name] = tmp
  
  ######
  var1 = nn[i,1]
  var2 = nn[i,2]
  var = paste(var1,var2,"Mult" ,sep = "_")
  w[,var] = as.numeric(df_train[,var1]) * as.numeric(df_train[,var2])
  w2[,var] = as.numeric(df_test[,var1]) * as.numeric(df_test[,var2])
  #var = paste(var1,var2,"Add" ,sep = "_")
 3# w[,var] = as.numeric(df_train[,var1]) + as.numeric(df_train[,var2])
  #w2[,var] = as.numeric(df_test[,var1]) + as.numeric(df_test[,var2])
}
rm(tmp) ;gc(reset = T)
total.time = Sys.time() - start.time
total.time







dd = data.frame()
for(i in 1:length(df$Field_ID)){
  x = df$Field_ID[i] %>% as.character()
  y = df$Year[i] %>% as.character()
  m = info %>% filter(Field_ID==x) %>% select(contains(y))
  colnames(m) = paste0("Feat_",1:ncol(m))
  dd = rbind(dd,m)
}

dd2 = data.frame()
for(i in 1:length(year$Field_ID)){
  x = year$Field_ID[i] %>% as.character()
  y = year$Year[i] %>% as.character()
  m = info %>% filter(Field_ID==x) %>% select(contains(y))
  colnames(m) = paste0("Feat_",1:ncol(m))
  dd2 = rbind(dd2,m)
}