
require(data.table)
require(ggplot2)
require(repr)

options(repr.plot.width=15, repr.plot.height=8)
# assuming you have the data folder in your working directory in the following format:
# 'working_directory/ClassificationData/dataset_name/'
current_folder=getwd()
dataset='CBF'

train_data_path=sprintf('%s/ClassificationData/%s/%s_TRAIN.txt',current_folder,dataset,dataset)

train_data=fread(train_data_path)

# the data in this repository is structured in the following way:
# each row is a time-series
# first column (V1) is the class variable
# V2 to last column represent each time steps

# This is also referred to as wide-format in general for data analysis problems
head(train_data)

# We will work with long format for easier visualization and analysis
# first add id variable (data.table notation)
# and rename column name "V1" with "class"
#sort based on class first
setnames(train_data,'V1','class')
train_data=train_data[order(class)]
train_data[,class:=as.character(class)]
train_data[,id:=1:.N]
# check
head(train_data)



# melt the data for long format
long_train=melt(train_data,id.vars=c('id','class'))
head(long_train)

# need to get numerical part of the variable (represents time)
# using gsub to set the nonnumerical part to zero length
long_train[,time:=as.numeric(gsub("\\D", "", variable))-1]

# check
head(long_train)

# remove variable
long_train=long_train[,list(id,class,time,value)]
long_train=long_train[order(id,time)]

# check
head(long_train)


# visualize time series based on class
ggplot(long_train, aes(time,value)) + geom_line(aes(color=as.character(id))) +
     facet_wrap(~class)

# another representation (heatmap)
long_train=long_train[order(class,id,time)]
ggplot(long_train, aes(time,id, fill= value)) + 
  geom_tile()


# let's start with broad overview. Boxplot for each class
ggplot(long_train, aes(x=class, y=value, color=class)) +
  geom_boxplot()

# calculate some stats for each series
stats=long_train[,list(mean=mean(value),median=median(value),stdev=sd(value)),by=list(class,id)]
head(stats)
# mean and stdevs are the same
summary(stats)

# check median
ggplot(stats, aes(x=class, y=median, color=class)) +
  geom_boxplot()

#long_train[,interval_id:=NULL]
long_train[,interval_id:=cut(time,2, ordered_result=T),by=list(id)]
str(long_train)
long_train[,interval_id:=as.numeric(interval_id)]

#long_train[id==1]
stats=long_train[,list(mean=mean(value),median=median(value),stdev=sd(value)),by=list(class,id,interval_id)]
head(stats)

# represent each series with the interval means
interval_stats=dcast(stats,id+class~paste0('int_',interval_id),value.var='mean')
head(interval_stats)

ggplot(interval_stats,aes(x=int_1,y=int_2,color=class)) + geom_point(size = 3)

ggplot(data=interval_stats, aes(x=id, y=int_1,fill=class)) +
  geom_bar(stat="identity")



# represent each series with the interval median
interval_stats=dcast(stats,id+class~paste0('int_',interval_id),value.var='median')
head(interval_stats)

ggplot(interval_stats,aes(x=int_1,y=int_2,color=class)) + geom_point(size = 3)

ggplot(data=interval_stats, aes(x=id, y=int_1,fill=class)) +
  geom_bar(stat="identity")

ar_long_train=copy(long_train)
ar_long_train=ar_long_train[order(id,time)]
ar_long_train[,lag1_val:=shift(value,1),by=list(id)]
ar_long_train[,lag2_val:=shift(value,2),by=list(id)]

## below will do the same
#lags=c(1:2)
#ar_long_train[,paste0('lag',lags,'_val'):=shift(value,lags)]

head(ar_long_train)

series_id=unique(ar_long_train$id)

# AR 2 model as a function to get the coefficients
fit_ar2=function(dat){
    fit=lm(value~lag1_val+lag2_val,dat)
    return(data.frame(t(coef(fit))))
}
fitted_coef=lapply(series_id,function(x) fit_ar2(ar_long_train[id==x]))

#str(fitted_coef)

coef_dt=rbindlist(fitted_coef)
                   
head(coef_dt)
                   
coef_dt[,id:=series_id] 
# add class information, use earlier data table (train data)
coef_dt=merge(coef_dt,train_data[,list(id,class)],by='id')
head(coef_dt)
     
ggplot(coef_dt,aes(x=lag1_val,y=lag2_val,color=class)) + geom_point(size = 3)


class_id=unique(ar_long_train$class)

# AR 2 model as a function to get the model
fit_ar2=function(dat){
    fit=lm(value~lag1_val+lag2_val,dat)
    return(fit)
}
fitted_models=lapply(class_id,function(x) fit_ar2(ar_long_train[class==x]))

ar_train_with_predictions=copy(ar_long_train)                   
for(i in 1:length(class_id)){
    current_class=class_id[i]
    ar_train_with_predictions[,paste0('residual_',current_class):=value-predict(fitted_models[[i]],ar_train_with_predictions)] 
}
                     
head(ar_train_with_predictions)

residual_stats=ar_train_with_predictions[,list(m1=mean(residual_1,na.rm=T),
                                m2=mean(residual_2,na.rm=T),
                                m3=mean(residual_3,na.rm=T)),by=list(id,class)]
head(residual_stats)

residual_stats=melt(residual_stats,id.vars=c('id','class'))

ggplot(residual_stats, aes(x=variable, y=value, color=variable)) +
  geom_boxplot() + facet_wrap(~class)
