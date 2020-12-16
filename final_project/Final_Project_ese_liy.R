#Final Project For ESE5023
#Project Title：Data analysis of microorganism with Raman
#Author       ：Li-Yuan
#Time         ：2020-12-09

#--------------------------------------------
#           1.Data manipulate
#--------------------------------------------
###1.1 Load Origin Data [param-1]
files <- list.files(path = "./data/",full.names = T)

###1.2 select first column as identify column
wavenumber <- read.table(files[1], 
                              header = FALSE, sep = "\t")[, 1]


###1.3 Integrate Data
for (filename in files){
  if(gsub(".txt$","",filename)!=filename){ #select the TXT file
    data <- read.table(filename, header = FALSE, sep="\t")[2]
    colnames(data) <- gsub(".*/|.txt","",filename)
    wavenumber <- cbind(wavenumber, data)
  }
}
colnames(wavenumber)[1] <- "Wavenumber"

###1.4 Clean and Transform Data
#check NA
sum(is.na(wavenumber))/dim(wavenumber)[1]/dim(wavenumber)[2] #0

#wide to long
library("dplyr")
library("tidyr")
wavenumber_long_tib<-as_tibble(wavenumber) %>% gather("Wavenumber") %>% 
    mutate(group=gsub("_.*$","",Wavenumber))
wavenumber_long_tib$wavenumber <-rep(wavenumber$Wavenumber,dim(wavenumber)[2]-1)
wavenumber_summarize <- wavenumber_long_tib %>% group_by(group,wavenumber) %>% 
  summarize(average = mean(value), stdev = sd(value),
            error = sd(value)/sqrt(length(value)))

library("ggplot2")
SpectraPlot <- ggplot(wavenumber_summarize, aes(x = wavenumber, y = average, group = group)) + 
  geom_line(aes(color = group)) + 
  geom_ribbon(aes(ymin = average - stdev, 
                  ymax = average + stdev, 
                  fill = group),
              alpha = 0.5) +
  labs(title="",x=expression(Wavenumber/cm^{"-1"}), y = "Intensity") +
  scale_x_continuous(breaks=seq(400,4000,400)) +
  scale_y_continuous(breaks=seq(-0.2,1.5,0.3)) +
  coord_cartesian(xlim = c(400, 4000),ylim=c(-0.2,1.5)) +
  theme_linedraw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "right",
        legend.background = element_blank()
  ) +
  ggtitle("Different intensity of three bacteria groups")

 pdf(file="1-secptra_plot.pdf",width = 10,height = 5)
 SpectraPlot
 dev.off()
#--------------------------------------------
#           2.Data analysis in statistic
#--------------------------------------------
###2.1 Normal Distribution or not [hist plot]
###2.2 Check Difference among different groups using Anonva algorithm
### boxplot
SpectraBoxPlot<-plot.boxplot(wavenumber_summarize,wavenumber_summarize$group,
                wavenumber_summarize$average,wavenumber_summarize$group
                )
pdf(file="2-secptra_boxplot.pdf",width = 10,height = 5)
SpectraBoxPlot
dev.off() 

plot.boxplot <- function(data,x,y,type,filename,title="boxplot"){
  a <- ggplot(data=data, aes(x =x, y =y ,color=type,group=type)) +
    geom_jitter(alpha = 0.3,size=3) +
    geom_boxplot(alpha = .5,size=1)+
    labs(x="Bacterial Groups",y="Average Intensity",fill= "Groups")+
    ggtitle(title)+
    theme_bw() + 
    theme(panel.border = element_blank())+
    theme(axis.line = element_line(size=1, colour = "black")) +
    theme(panel.grid =element_blank())+  
    theme(axis.text = element_text(size = 15,colour = "black"),text = element_text(size = 15,colour = "black"))+
    theme(axis.text.x = element_text( hjust = 1,angle = 45))
  #ggsave(paste0(filename, ".pdf"),plot=a,width=8,height=8)
    return(a)
}

### pca
library(devtools)
#install_github("vqv/ggbiplot")
library(ggbiplot)
x<-t(wavenumber)
y<-as.data.frame(x)
colnames(y)<-y[1,]
y<-y[-1,]
wavenumber.pca<-prcomp(y, center = TRUE,scale. = TRUE)
summary(wavenumber.pca)
###plot the distribution of PCA
#ggbiplot(wavenumber.pca, groups=row.names(y),)
# Visualize
# Use habillage to specify groups for coloring note to use as.factor
spectraPCAPlot <- ggbiplot(wavenumber.pca, 
         groups = gsub("_.*$","",row.names(y)),
         ellipse = TRUE,var.axes = F)

pdf(file="3-spectra_pcaplot.pdf",width = 10,height = 5)
spectraPCAPlot
dev.off() 
#--------------------------------------------------------
#           3. Annova to find the most signfiture feature
#--------------------------------------------------------
anova_df <- data.frame(H1=wavenumber_summarize[1:1024,3],
                       H7=wavenumber_summarize[1025:2048,3],
                       SCM1=wavenumber_summarize[2049:3072,3])
colnames(anova_df)=c("H1","H7","SCM1")
anova_one_way1 <- aov(H1 ~ H7,data = anova_df)
summary(anova_one_way) #significant

anova_one_way2 <- aov(H7 ~ SCM1,data = anova_df)
summary(anova_one_way2) #significant

anova_one_way3 <- aov(H1 ~ SCM1,data = anova_df)
summary(anova_one_way3) #significant

#--------------------------------------------
#           4.Build model to train and validate using Machine Learning
#--------------------------------------------
### 4.1 data split
### 4.2 train
### 4.3 test
### 4.4 roc curve
### 4.5 confuse matrix
### 4.6 Pheatmap

##### Create balanced splits of the data into training set and test set
library ( caret )
set.seed (329)
wavenumber_ml<-wavenumber %>% t() %>% as_data_frame()
colnames(wavenumber_ml) <- wavenumber_ml[1,] #set col names with first row
wavenumber_ml <- wavenumber_ml[-1,] #remove first row
group <- gsub("_.*$","",colnames(wavenumber)[-1])
wavenumber_ml$group <- group
row.names(wavenumber_ml) <- colnames(wavenumber)[-1] #set row names

trainIndex <- createDataPartition ( wavenumber_ml$group , p = .8,
                                    list = FALSE ,
                                    times = 1)
head (trainIndex)
dataTrain <- wavenumber_ml [ trainIndex ,]
row.names(dataTrain)<- row.names(wavenumber_ml)[ trainIndex]

dataTest <- wavenumber_ml [- trainIndex ,]
row.names(dataTest)<- row.names(wavenumber_ml)[ -trainIndex]

# Parameter tuning
fitControl <- trainControl (## 10- fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated 5 times
  repeats = 5,
  allowParallel = TRUE )
#---------------------------------------------------------------#
#                 3,train model
#---------------------------------------------------------------#
svmFitLin <- train ( group ~ ., data = dataTrain,
                     method = "svmLinear2", # or svmLinear
                     trControl = fitControl ,probability=T)
#---------------------------------------------------------------#
#                4,validate model
#---------------------------------------------------------------#
svmPred <- predict ( svmFitLin , dataTest,type = "prob") #预测值
predict<-c()
predict[which(svmPred[[1]]>0.5)] <-"H1" #H1
predict[which(svmPred[[2]]>0.5)] <-"H7" #H2
predict[which(svmPred[[3]]>0.5)] <-"SCM1" #H3

svmPred$true <- dataTest$group #true label
svmPred$predict <- predict #predict label
anno_row <-data.frame(pred=svmPred$predict,true=svmPred$true)
row.names(anno_row)<-row.names(svmPred)
colnames(anno_row)<-c("Predict","True")
#---------------------------------------------------------------#
#                5,pheatmap
#---------------------------------------------------------------#
library(pheatmap)
SVM_PheatmapPlot<-pheatmap(svmPred[,1:3],
         cluster_cols=F,
         cluster_rows=F,
         display_numbers=T,
         width = 10,
         height = 16,
         cellwidth = 80,
         cellheight= 30,
         show_colnames=T,
         show_rownames=T,
         border_color="black",
         annotation_row = anno_row,
         main = "The result of Predict and True",
         fontsize = 18
        
)
pdf(file="4-svm_pheatmapplot.pdf",width =8,height = 10)
SVM_PheatmapPlot
dev.off() 
#--------------------------------------------
#           5.Data Save,submit to Git 
#--------------------------------------------
save(file="final_project.Rdata",list = ls())
load(file="final_project.Rdata")

