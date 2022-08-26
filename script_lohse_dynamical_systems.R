library(car); library(patchwork); library(ez)
library(ggExtra); library(mediation); library(tidyverse)

getwd()
setwd("~/GitHub/taylor_2022_CI_sequential_effects")
list.files("./Data/")

# 1.0 Visualizing Data ---------------------------------------------------------
ACQ<-read.csv("./Data/data_ACQ_MASTER.csv", header = TRUE, sep=",",  
              na.strings=c("NA","NaN"," ",""), stringsAsFactors = TRUE)
head(ACQ)
ACQ$participant <-factor(ACQ$participant)
ACQ$Target<-factor(ACQ$target)
ACQ$block <- factor(ACQ$block)

# Removing Outliers
ACQ$absolute_error <- ifelse(ACQ$absolute_error < 1000, ACQ$absolute_error, NA)
ACQ$constant_error <- ifelse(ACQ$absolute_error < 1000, ACQ$constant_error, NA)
ACQ$hold_time <- ifelse(ACQ$absolute_error < 1000, ACQ$hold_time, NA)

ACQ$absolute_error <- ACQ$absolute_error/1000
ACQ$constant_error <- ACQ$constant_error/1000
ACQ$hold_time <- ACQ$hold_time/1000

head(ACQ)

# Sorting the data so that lag_error refers to the previous time they had a 
# target of that type:
ACQ2<-ACQ %>% group_by(participant, target) %>% 
  arrange(trial_total) %>%
  mutate(target_lag_absolute_error=lag(absolute_error),
         target_lag_2_absolute_error=lag(absolute_error, 2),
         target_lag_3_absolute_error=lag(absolute_error, 3),
         target_lag_4_absolute_error=lag(absolute_error, 4),
         target_lag_constant_error=lag(constant_error),
         target_lag_2_constant_error=lag(constant_error, 2),
         target_lag_3_constant_error=lag(constant_error, 3),
         target_lag_4_constant_error=lag(constant_error, 4),
         target_lag_trial_same_target=lag(trial_total),
         target_lag_hold_time=lag(hold_time),
         target_lag_2_hold_time=lag(hold_time, 2),
         target_lag_3_hold_time=lag(hold_time, 3),
         target_lag_4_hold_time=lag(hold_time, 4),
         target_trial = seq(1:length(absolute_error)),
         target_ave_hold_time=mean(hold_time, na.rm=TRUE)
         )%>%
  ungroup() %>%
  arrange(participant, trial_total)

ACQ3 <- ACQ2 %>% group_by(participant) %>% 
  arrange(trial_total) %>%
  mutate(trial_lag_absolute_error=lag(absolute_error),
         trial_lag_2_absolute_error=lag(absolute_error, 2),
         trial_lag_3_absolute_error=lag(absolute_error, 3),
         trial_lag_4_absolute_error=lag(absolute_error, 4),
         trial_lag_constant_error=lag(constant_error),
         trial_lag_2_constant_error=lag(constant_error, 2),
         trial_lag_3_constant_error=lag(constant_error, 3),
         trial_lag_4_constant_error=lag(constant_error, 4),
         trial_lag_hold_time=lag(hold_time),
         trial_lag_2_hold_time=lag(hold_time, 2),
         trial_lag_3_hold_time=lag(hold_time, 3),
         trial_lag_4_hold_time=lag(hold_time, 4),
         trial_ave_hold_time=mean(hold_time, na.rm=TRUE)
  )%>%
  ungroup() %>%
  arrange(participant, trial_total)

# Calculating Difference Scores
ACQ3$target_CE_diff <- with(ACQ3, constant_error-target_lag_constant_error)
ACQ3$target_AE_diff <- with(ACQ3, absolute_error-target_lag_absolute_error)

ACQ3$trial_CE_diff <- with(ACQ3, constant_error-trial_lag_constant_error)
ACQ3$trial_AE_diff <- with(ACQ3, absolute_error-trial_lag_absolute_error)

# Aboslute Change in Constant Error
ACQ3$target_absolute_change <- abs(ACQ3$target_CE_diff)
ACQ3$trial_absolute_change <- abs(ACQ3$trial_CE_diff)

table(ACQ3$participant, ACQ3$Target)

ACQ3$rand.c <- as.numeric(ACQ3$group)-1.5

# QA Plots to make sure sorting worked correctly:
plot(y=ACQ3$constant_error, 
     x=ACQ3$hold_time)
plot(y=ACQ3$absolute_error, 
     x=ACQ3$hold_time)
plot(y=ACQ3$target_lag_constant_error, 
     x=ACQ3$target_lag_hold_time)
plot(y=ACQ3$target_lag_absolute_error, 
     x=ACQ3$target_lag_hold_time)

# CE differences should be identical for the blocked group
plot(y=ACQ3[ACQ3$group=="Blocked",]$target_CE_diff, 
     x=ACQ3[ACQ3$group=="Blocked",]$trial_CE_diff)
# CE differences should almost all be different for the random group
plot(y=ACQ3[ACQ3$group=="Random",]$target_CE_diff, 
     x=ACQ3[ACQ3$group=="Random",]$trial_CE_diff)

head(ACQ3)


# Import Post Test Data
list.files("./Data/")
POST<-read.csv("./Data/data_post_MASTER.csv", header = TRUE, sep=",",  
              na.strings=c("NA","NaN"," ",""), stringsAsFactors = TRUE)
head(POST)
str(POST)

# Removing Outliers
POST$absolute_error <- ifelse(POST$absolute_error < 1000, POST$absolute_error, NA)
POST$absolute_error <- POST$absolute_error/1000
POST$constant_error <- POST$constant_error/1000
POST$hold_time <- POST$hold_time/1000

POST <- subset(POST, is.na(POST$absolute_error)==FALSE)
POST$Target <- factor(POST$target)
head(POST)



# Plotting Sequential Effects on Constant Error 
head(ACQ)
ggplot(ACQ[ACQ$participant==132,], 
       aes(x=trial_total, y = constant_error)) +
  geom_line(col="black", alpha=1, lwd=0.5)+
  geom_point(aes(fill=Target), shape=21, size=2) +
  #facet_wrap(~Target)+
  scale_x_continuous(name = "Trial Number") +
  scale_y_continuous(name = "Constant Error", limits=c(-0.7,0.7)) +
  scale_fill_manual(values=c("grey90", "grey60", "grey30"))+
  #labs(fill = "Group", col="Group", shape="Estimation", lty="Estimation")+ 
  theme_bw()+
  theme(axis.text=element_text(size=12, color="black"), 
        legend.text=element_text(size=12, color="black"),
        legend.title=element_text(size=12, face="bold"),
        axis.title=element_text(size=12, face="bold"),
        plot.title=element_text(size=12, face="bold", hjust=0.5),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=12, face="bold"),
        legend.position = "none")


# 2.0 Determinant Approach: Comparing Previous Trial to Previous Target ----
# Ellipses: Hold Time
ggplot(ACQ3, aes(x =target_lag_hold_time, y = hold_time)) +
  stat_ellipse(aes(group=paste(participant, Target), col=Target), alpha=0.5, level=0.80)+
  facet_wrap(~group)+
  #scale_fill_colorblind()+
  #scale_color_grey()+
  scale_x_continuous(name = expression(bold(Hold~Time~n[k]-1~(s)))) +
  scale_y_continuous(name = expression(bold(Hold~Time~n[k]~(s)))) +
  #labs(fill = "Group", col="Group", shape="Estimation", lty="Estimation")+ 
  theme_bw()+
  theme(axis.text=element_text(size=10, color="black"), 
        legend.text=element_text(size=12, color="black"),
        legend.title=element_text(size=12, face="bold"),
        axis.title=element_text(size=12, face="bold"),
        plot.title=element_text(size=12, face="bold", hjust=0.5),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=12, face="bold"),
        legend.position = "none")

# Ellipses: Constant Error
ggplot(ACQ3, aes(x =target_lag_constant_error, y = constant_error)) +
  stat_ellipse(aes(group=paste(participant)), alpha=0.5, level=0.80)+
  facet_wrap(~group)+
  #scale_fill_colorblind()+
  #scale_color_grey()+
  scale_x_continuous(name = expression(bold(Constant~Error~n[k]-1~(s)))) +
  scale_y_continuous(name = expression(bold(Constant~Error~n[k]~(s)))) +
  #labs(fill = "Group", col="Group", shape="Estimation", lty="Estimation")+ 
  theme_bw()+
  theme(axis.text=element_text(size=10, color="black"), 
        legend.text=element_text(size=12, color="black"),
        legend.title=element_text(size=12, face="bold"),
        axis.title=element_text(size=12, face="bold"),
        plot.title=element_text(size=12, face="bold", hjust=0.5),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=12, face="bold"),
        legend.position = "none")


# Ellipses: Absolute Error
ggplot(ACQ3, aes(x =target_lag_absolute_error, y = absolute_error)) +
  stat_ellipse(aes(group=paste(participant)), alpha=0.5, level=0.80)+
  facet_wrap(~group)+
  #scale_fill_colorblind()+
  #scale_color_grey()+
  scale_x_continuous(name = expression(bold(Absolute~Error~n[k]-1~(s)))) +
  scale_y_continuous(name = expression(bold(Absolute~Error~n[k]~(s)))) +
  #labs(fill = "Group", col="Group", shape="Estimation", lty="Estimation")+ 
  theme_bw()+
  theme(axis.text=element_text(size=10, color="black"), 
        legend.text=element_text(size=12, color="black"),
        legend.title=element_text(size=12, face="bold"),
        axis.title=element_text(size=12, face="bold"),
        plot.title=element_text(size=12, face="bold", hjust=0.5),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=12, face="bold"),
        legend.position = "none")

# Figure 3 D and C: Autocorrelations ----
# Graphs for individual participants, 101 and 107 blocked, 102 and 126, random
ggplot(ACQ[ACQ$participant==107,], 
       aes(x=trial_total, y = constant_error)) +
  geom_line(col="black", alpha=1)+
  geom_point(aes(fill=Target), shape=21, size=2) +
  scale_fill_manual(values=c("grey90", "grey60", "grey30"))+
  facet_wrap(~Target, scales="free")+
  scale_x_continuous(name = "Trial Number") +
  scale_y_continuous(name = "Constant Error", limits=c(-0.5,0.5)) +
  #labs(fill = "Group", col="Group", shape="Estimation", lty="Estimation")+ 
  theme_bw()+
  theme(axis.text=element_text(size=10, color="black"), 
        legend.text=element_text(size=12, color="black"),
        legend.title=element_text(size=12, face="bold"),
        axis.title=element_text(size=12, face="bold"),
        plot.title=element_text(size=12, face="bold", hjust=0.5),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=12, face="bold"),
        legend.position = "none")

# Graphs for individual participants, 101 and 107 blocked, 102 and 126, random

cor(x =ACQ3[ACQ3$participant==107,]$target_lag_constant_error, 
          y = ACQ3[ACQ3$participant==107,]$constant_error, use="complete.obs")

ggplot(ACQ3[ACQ3$participant==107,], 
       aes(x =target_lag_constant_error, y = constant_error)) +
  geom_path(aes(group=participant), col="black", alpha=0.5)+
  geom_point(aes(group=Target, fill=Target), col="black", shape=21) +
  stat_ellipse(aes(group=participant), col="black", lwd=1.5, level=0.95)+
  scale_fill_manual(values=c("grey90", "grey60", "grey30"))+
  scale_color_manual(values=c("grey90", "grey60", "grey30"))+
  #scale_fill_colorblind()+
  #scale_colour_colorblind()+
  scale_x_continuous(name = expression(bold(Constant~Error~n[k]-1~(s))), limits=c(-0.5, 0.5)) +
  scale_y_continuous(name = expression(bold(Constant~Error~n[k]~(s))), limits=c(-0.5, 0.5)) +
  #labs(fill = "Group", col="Group", shape="Estimation", lty="Estimation")+ 
  theme_bw()+
  theme(axis.text=element_text(size=10, color="black"), 
        legend.text=element_text(size=12, color="black"),
        legend.title=element_text(size=12, face="bold"),
        axis.title=element_text(size=12, face="bold"),
        plot.title=element_text(size=12, face="bold", hjust=0.5),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=12, face="bold"),
        legend.position = "none")


# Calculating the Determinant of the Data ----
summary(ACQ3$target_lag_constant_error)
summary(ACQ3$trial_lag_constant_error)

# Excluding NAs for different lags
# The NAs will be different in the different calculations
# Excluding NAs
ACQ_by_TARGET <- subset(ACQ3, is.na(ACQ3$absolute_error)==FALSE)
ACQ_by_TARGET <- subset(ACQ_by_TARGET, is.na(ACQ_by_TARGET$target_lag_absolute_error)==FALSE)

ACQ_by_TRIAL <- subset(ACQ3, is.na(ACQ3$absolute_error)==FALSE)
ACQ_by_TRIAL <- subset(ACQ_by_TRIAL, is.na(ACQ_by_TRIAL$trial_lag_absolute_error)==FALSE)


TWOBACK_TRIAL <- subset(ACQ_by_TRIAL, is.na(trial_lag_2_constant_error)==FALSE)
TWOBACK_TARGET <- subset(ACQ_by_TARGET, is.na(target_lag_2_constant_error)==FALSE)
summary(TWOBACK_TRIAL$trial_lag_2_constant_error)
summary(TWOBACK_TARGET$target_lag_2_constant_error)

THREEBACK_TRIAL <- subset(TWOBACK_TRIAL, is.na(trial_lag_3_constant_error)==FALSE)
THREEBACK_TARGET <- subset(TWOBACK_TARGET, is.na(target_lag_3_constant_error)==FALSE)
summary(THREEBACK_TRIAL$trial_lag_3_constant_error)
summary(THREEBACK_TARGET$target_lag_3_constant_error)

FOURBACK_TRIAL <- subset(THREEBACK_TRIAL, is.na(trial_lag_4_constant_error)==FALSE)
FOURBACK_TARGET <- subset(THREEBACK_TARGET, is.na(target_lag_4_constant_error)==FALSE)
summary(FOURBACK_TRIAL$trial_lag_4_constant_error)
summary(FOURBACK_TARGET$target_lag_4_constant_error)


participant <- NULL
group <- NULL

det_1back_target <- NULL
det_2back_target <- NULL
det_3back_target <- NULL
det_4back_target <- NULL

det_1back_trial <- NULL
det_2back_trial <- NULL
det_3back_trial <- NULL
det_4back_trial <- NULL

sub_list <- unique(ACQ_by_TARGET$participant)
length(sub_list)


for (i in seq(1:length(sub_list))){
  print(i)
  
  participant[i] <- as.character(ACQ_by_TARGET[ACQ_by_TARGET$participant==sub_list[i],]$participant[1])
  group[i] <- as.character(ACQ_by_TARGET[ACQ_by_TARGET$participant==sub_list[i],]$group[1])
  
  # 1 back determinants
  det_1back_target[i] <- det(cor(ACQ_by_TARGET[ACQ_by_TARGET$participant==sub_list[i],
                                  c("constant_error", 
                                    "target_lag_constant_error")]))
  det_1back_trial[i] <- det(cor(ACQ_by_TRIAL[ACQ_by_TRIAL$participant==sub_list[i],
                                               c("constant_error", 
                                                 "trial_lag_constant_error")]))
  # 2 back determinants
  det_2back_target[i] <- det(cor(TWOBACK_TARGET[TWOBACK_TARGET$participant==sub_list[i],
                                               c("constant_error", 
                                                 "target_lag_constant_error", 
                                                 "target_lag_2_constant_error")]))
  det_2back_trial[i] <- det(cor(TWOBACK_TRIAL[TWOBACK_TRIAL$participant==sub_list[i],
                                              c("constant_error", 
                                                "trial_lag_constant_error", 
                                                "trial_lag_2_constant_error")]))
  
  # 3 back determinants
  det_3back_target[i] <- det(cor(THREEBACK_TARGET[THREEBACK_TARGET$participant==sub_list[i],
                                                c("constant_error", 
                                                  "target_lag_constant_error", 
                                                  "target_lag_2_constant_error",
                                                  "target_lag_3_constant_error")]))
  det_3back_trial[i] <- det(cor(THREEBACK_TRIAL[THREEBACK_TRIAL$participant==sub_list[i],
                                              c("constant_error", 
                                                "trial_lag_constant_error", 
                                                "trial_lag_2_constant_error",
                                                "trial_lag_3_constant_error")]))
  # 4 back determinants
  det_4back_target[i] <- det(cor(FOURBACK_TARGET[FOURBACK_TARGET$participant==sub_list[i],
                                                  c("constant_error", 
                                                    "target_lag_constant_error", 
                                                    "target_lag_2_constant_error",
                                                    "target_lag_3_constant_error", 
                                                    "target_lag_4_constant_error")]))
  det_4back_trial[i] <- det(cor(FOURBACK_TRIAL[FOURBACK_TRIAL$participant==sub_list[i],
                                                c("constant_error", 
                                                  "trial_lag_constant_error", 
                                                  "trial_lag_2_constant_error",
                                                  "trial_lag_3_constant_error", 
                                                  "trial_lag_4_constant_error")]))
}

DATA_DET <- data.frame(participant, group, 
                       det_1back_target, 
                       det_2back_target,  
                       det_3back_target,
                       det_4back_target,
                       det_1back_trial,
                       det_2back_trial,
                       det_3back_trial,
                       det_4back_trial,
                       stringsAsFactors = TRUE)
head(DATA_DET)

write.csv(DATA_DET, "./data_DET_WIDE.csv")

DATA_DET_LONG <- DATA_DET %>% pivot_longer(cols=3:10, names_to = "label", values_to = "det")
head(DATA_DET_LONG, 10)

str_split(DATA_DET_LONG$label, "_", simplify = TRUE)

DATA_DET_LONG$phase_space <- str_split(DATA_DET_LONG$label, "_", simplify = TRUE)[,3]
DATA_DET_LONG$lag <- str_split(DATA_DET_LONG$label, "_", simplify = TRUE)[,2]
head(DATA_DET_LONG, 10)

write.csv(DATA_DET_LONG, "./data_DET_LONG.csv")


# DATA_DET <- DATA_DET_LONG %>% select(-label) %>%
#   pivot_wider(names_from = phase_space, values_from = det)
# 
# write.csv(DATA_DET, "./data_DET.csv")

head(DATA_DET_LONG)
DATA_DET_LONG$phase_space  <- factor(DATA_DET_LONG$phase_space, 
                                     levels = c("target", "trial"),
                                     labels = c("Target", "Trial"))
ggplot(DATA_DET_LONG, 
       aes(x=lag, y = det)) +
  geom_point(aes(group=group, fill=group), col="black", shape=21, alpha=0.5, 
             position=position_jitterdodge())+
  geom_boxplot(aes(group=paste(lag, group), fill=group), alpha=0.5,
               position=position_dodge(), outlier.shape = NA)+
  facet_wrap(~phase_space)+
  scale_fill_manual(values=c("grey90", "grey30"))+
  scale_color_manual(values=c("grey90", "grey30"))+
  scale_x_discrete(name = "Lag") +
  scale_y_continuous(name = "Determinant") +
  labs(fill = "Group", col="Group")+ 
  theme_bw()+
  theme(axis.text=element_text(size=12, color="black"), 
        legend.text=element_text(size=14, color="black"),
        legend.title=element_text(size=14, face="bold"),
        axis.title=element_text(size=14, face="bold"),
        plot.title=element_text(size=14, face="bold", hjust=0.5),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=14, face="bold"),
        legend.position = "bottom")


DATA_DET_LONG$explained_variance <- 1-DATA_DET_LONG$det
DATA_DET_LONG$log_exp_var <- log(DATA_DET_LONG$explained_variance+1)
head(DATA_DET_LONG)

DATA_DET <- DATA_DET_LONG %>% dplyr::select(-label) %>%
  pivot_wider(names_from = phase_space, values_from = c(det, explained_variance, log_exp_var))

head(DATA_DET)

DATA_DET$var_diff <- with(DATA_DET, log_exp_var_Target-log_exp_var_Trial)
DATA_DET$det_diff <- with(DATA_DET, log_exp_var_Target-log_exp_var_Trial)
write.csv(DATA_DET, "./data_DET.csv")


# Analyses of the Determinants ----
head(DATA_DET)
head(DATA_DET_LONG)

ezANOVA(
  data=DATA_DET_LONG
  , dv = .(det)
  , wid =.(participant)
  , within = .(lag, phase_space)
  , between = .(group)
  , type = 3
  , white.adjust = FALSE
  , detailed = FALSE
  , return_aov = FALSE
)


# Post-Hoc ANOVAs in Trial space or Target space
ezANOVA(
  data=DATA_DET_LONG[DATA_DET_LONG$phase_space=="Target",]
  , dv = .(det)
  , wid =.(participant)
  , within = .(lag)
  , between = .(group)
  , type = 3
  , white.adjust = FALSE
  , detailed = FALSE
  , return_aov = FALSE
)



head(DATA_DET)
DATA_DET %>% group_by(group, lag) %>%
  summarize(
    subs = length(unique(participant)),      
    ave_det_target = mean(det_Target),
    SD_det_target = sd(det_Target),
    ave_det_trial = mean(det_Trial),
    SD_det_trial = sd(det_Trial),
    ave_det_diff = mean(det_diff),
    SD_det_diff = sd(det_diff),)


# Merging Determinant Data into Post-Test Data
ACQ %>% group_by(participant, group) %>% summarize(n=n()) %>% 
  group_by(group) %>% summarize(n=n())
POST %>% group_by(participant, group) %>% summarize(n=n()) %>% 
  group_by(group) %>% summarize(n=n())

head(POST)
POST_AVE <- POST %>% group_by(participant, group, test_type) %>%
  summarize(ave_ae = mean(absolute_error, na.rm=TRUE),
            ave_ce = mean(constant_error, na.rm=TRUE),
            ave_ve = sd(constant_error, na.rm=TRUE))
head(POST_AVE)
POST_AVE<-POST_AVE %>% pivot_wider(names_from=test_type, 
                                   values_from=c(ave_ae, ave_ce, ave_ve))
head(POST_AVE)

head(DATA_DET)

MERGED <- merge(POST_AVE, DATA_DET[DATA_DET$lag=="4back",
                                   c("participant", "det_Target", "det_Trial", "det_diff")], 
                by="participant")
head(MERGED)

write.csv(MERGED, "./data_DYNAMIC_MERGED.csv")


# 3.0. How do the groups differ in their correlation structure? ----

# Compute correlation matrix within each person, get average correlation
# matrix in each group in Target:Trial space
# Correlation Matrices in Target Space ----
head(ACQ_by_TARGET)
DAT1<- ACQ_by_TARGET %>% ungroup() %>% 
  dplyr::select(c(participant, group, constant_error, 
           target_lag_constant_error, target_lag_2_constant_error,
           target_lag_3_constant_error, target_lag_4_constant_error)) 
DAT1 <- na.omit(DAT1)
head(DAT1)
cor(DAT1[DAT1$participant==102,3:7])  


# Correlation Matrices for Random Participants
RAND <- subset(DAT1, group=="Random")
sub_list <- unique(RAND$participant)
length(sub_list)
RAND_MAT <- vector(mode="list", length=0)

for (i in seq(1:length(sub_list))){
  print(i)
  
  RAND_MAT[[i]] <- cor(RAND[RAND$participant==sub_list[i],3:7])  
}



# Correlation Matrices for Blocked Participants
BLOCK <- subset(DAT1, group=="Blocked")
sub_list <- unique(BLOCK$participant)
length(sub_list)
BLOCK_MAT <- vector(mode="list", length=0)

for (i in seq(1:length(sub_list))){
  print(i)
  
  BLOCK_MAT[[i]] <- cor(BLOCK[BLOCK$participant==sub_list[i],3:7])  
}

BLOCK_MAT

# Correlation Matrix, Random Group, Target Space
length(RAND_MAT)
RAND_ARRAY <- array( unlist(RAND_MAT) , c(5,5,length(RAND_MAT)))
rowMeans(RAND_ARRAY, dims = 2 )
RAND_COR_MAT <- apply(RAND_ARRAY, 1:2, mean)
colnames(RAND_COR_MAT) <- paste0("N",seq(0:4)-1)
rownames(RAND_COR_MAT) <- paste0("N",seq(0:4)-1)
RAND_COR_MAT

# Correlation Matrix Blocked GRoup, Trial Space
BLOCK_ARRAY <- array( unlist(BLOCK_MAT) , c(5,5,length(BLOCK_MAT)))
rowMeans(BLOCK_ARRAY, dims = 2 )
BLOCK_COR_MAT <- apply(BLOCK_ARRAY, 1:2, mean)
colnames(BLOCK_COR_MAT) <- paste0("N",seq(0:4)-1)
rownames(BLOCK_COR_MAT) <- paste0("N",seq(0:4)-1)
BLOCK_COR_MAT





# Correlation Matrices in Trial Space ----
head(ACQ_by_TRIAL)
DAT2<- ACQ_by_TRIAL %>% ungroup() %>% 
  dplyr::select(c(participant, group, constant_error, 
           trial_lag_constant_error, trial_lag_2_constant_error,
           trial_lag_3_constant_error, trial_lag_4_constant_error)) 
DAT2 <- na.omit(DAT2)
head(DAT2)
cor(DAT2[DAT2$participant==102,3:7])  


# Correlation Matrices for Random Participants
RAND <- subset(DAT2, group=="Random")
sub_list <- unique(RAND$participant)
length(sub_list)
RAND_MAT <- vector(mode="list", length=0)

for (i in seq(1:length(sub_list))){
  print(i)
  
  RAND_MAT[[i]] <- cor(RAND[RAND$participant==sub_list[i],3:7])  
}



# Correlation Matrices for Blocked Participants
BLOCK <- subset(DAT2, group=="Blocked")
sub_list <- unique(BLOCK$participant)
length(sub_list)
BLOCK_MAT <- vector(mode="list", length=0)

for (i in seq(1:length(sub_list))){
  print(i)
  
  BLOCK_MAT[[i]] <- cor(BLOCK[BLOCK$participant==sub_list[i],3:7])  
}

BLOCK_MAT

# Correlation Matrix, Random Group, Target Space
length(RAND_MAT)
RAND_ARRAY <- array( unlist(RAND_MAT) , c(5,5,length(RAND_MAT)))
rowMeans(RAND_ARRAY, dims = 2 )
RAND_COR_MAT <- apply(RAND_ARRAY, 1:2, mean)
colnames(RAND_COR_MAT) <- paste0("N",seq(0:4)-1)
rownames(RAND_COR_MAT) <- paste0("N",seq(0:4)-1)
RAND_COR_MAT

# Correlation Matrix Blocked GRoup, Trial Space
BLOCK_ARRAY <- array( unlist(BLOCK_MAT) , c(5,5,length(BLOCK_MAT)))
rowMeans(BLOCK_ARRAY, dims = 2 )
BLOCK_COR_MAT <- apply(BLOCK_ARRAY, 1:2, mean)
colnames(BLOCK_COR_MAT) <- paste0("N",seq(0:4)-1)
rownames(BLOCK_COR_MAT) <- paste0("N",seq(0:4)-1)
BLOCK_COR_MAT






# 4.0. Multilevel Model Approach to Change ----
library(lme4); library(lmerTest)

# Change following Correct versus KR Trials ----
head(ACQ_by_TARGET)

ACQ_by_TARGET$change_p <- ACQ_by_TARGET$target_absolute_change/ACQ_by_TARGET$target_lag_absolute_error 
plot(y=ACQ_by_TARGET$change_p, x=ACQ_by_TARGET$target_lag_absolute_error )
plot(y=ACQ_by_TARGET$target_absolute_change, x=ACQ_by_TARGET$target_lag_absolute_error )

ACQ_by_TARGET$lag_KR <- factor(ifelse(ACQ_by_TARGET$target_lag_absolute_error <= 0.050,
                      "No KR",
                      ifelse(ACQ_by_TARGET$target_lag_absolute_error == "NA",
                      NA,
                      "KR")),
                      levels=c("No KR", "KR"))

summary(ACQ_by_TARGET$lag_KR)
table(ACQ_by_TARGET$lag_KR, ACQ_by_TARGET$group)

ACQ_by_TARGET %>% group_by(participant, group) %>%
  summarize(No_KR = sum(lag_KR=="No KR"),
          KR = sum(lag_KR=="KR"),
          Total = n()) %>%
  mutate(No_KR_p = No_KR/Total,
         KR_p = KR/Total) %>%
  group_by(group) %>%
  summarize(ave_No_KR_p=mean(No_KR_p),
            ave_KR_p=mean(KR_p))

ggplot(ACQ_by_TARGET %>% filter(is.na(lag_KR)== FALSE), 
       aes(x =lag_KR, y = target_absolute_change)) +
  geom_point(aes(group=group, col=group), shape=16, size=1, alpha=0.2, 
             position=position_jitterdodge(dodge.width=0.5)) +
  geom_boxplot(aes(fill=group), outlier.shape = NA,
               position=position_dodge(width=0.5), width=0.5)+
  scale_fill_manual(values=c("grey60", "grey30"))+
  scale_color_manual(values=c("grey60", "grey30"))+
  facet_wrap(~block, ncol=1)+
  stat_smooth(method="lm", formula = y~x+I(x^2)+I(x^3), se=FALSE, col="blue")+
  scale_x_discrete(name = "Feedback Received") +
  scale_y_continuous(name = expression(bold(Change~n[k]+1~(s))), limits=c(0,1)) +
  theme_bw()+
  labs(fill="Group", col="Group")+
  theme(axis.text=element_text(size=12, color="black"), 
        legend.text=element_text(size=12, color="black"),
        legend.title=element_text(size=12, face="bold"),
        axis.title=element_text(size=12, face="bold"),
        plot.title=element_text(size=12, face="bold", hjust=0.5),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=12, face="bold"),
        legend.position = "bottom")


ACQ_by_KR <- ACQ_by_TARGET %>% filter(is.na(lag_KR)== FALSE) %>%
  group_by(participant, block, lag_KR) %>%
  summarize(group = group[1],
    mean_change = mean(target_absolute_change))
head(ACQ_by_KR)

KR_RE_mod <- lmer(mean_change~
       # Fixed Effects 
       group*lag_KR*block+
       # Random Effects
       (1|participant)+(1|block:participant)+(1|lag_KR:participant),
     data=ACQ_by_KR, 
     REML=TRUE,
     control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=5e5)))

anova(KR_RE_mod)
summary(KR_RE_mod)

with(ACQ_by_KR, aggregate(x=mean_change, by=list(group), FUN=mean))
with(ACQ_by_KR, aggregate(x=mean_change, by=list(group, lag_KR), FUN=mean))
with(ACQ_by_KR, aggregate(x=mean_change, by=list(lag_KR, block), FUN=mean))

          
# Change Following KR Trials -----
ACQ4 <- ACQ_by_TARGET %>% filter(target_lag_absolute_error > 0.050)

# Graphs for individual participants, 101 and 107 blocked, 102 and 126, random
ggplot(ACQ4[ACQ4$participant==126,], 
       aes(x =target_lag_absolute_error, y = target_absolute_change)) +
  geom_line(col="grey") +
  geom_point(shape=16, col="grey") +
  facet_wrap(~block)+
  stat_smooth(method="lm", formula = y~x+I(x^2), 
              se=FALSE, col="black", lty=2)+
  scale_x_continuous(name = expression(bold(Absolute~Error~n[k]~(s))), limits=c(0,1)) +
  scale_y_continuous(name = expression(bold(Change~n[k]+1~(s))), limits=c(0,1)) +
  theme_bw()+
  theme(axis.text=element_text(size=12, color="black"), 
        legend.text=element_text(size=12, color="black"),
        legend.title=element_text(size=12, face="bold"),
        axis.title=element_text(size=12, face="bold"),
        plot.title=element_text(size=12, face="bold", hjust=0.5),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=12, face="bold"),
        legend.position = "bottom")


head(ACQ4)
mean(ACQ4$target_lag_absolute_error)
ACQ4$target_lag_absolute_error.c <- ACQ4$target_lag_absolute_error - 0.500

RE_mod_CHANGE_linear <- lmer(target_absolute_change~
                            # Fixed Effects 
                            1+target_lag_absolute_error+
                            # Random Effects
                            (1+target_lag_absolute_error|participant)+
                            (1|block),
                          data=ACQ4, REML=FALSE,
                          control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=5e5)))


RE_mod_CHANGE_quad <- lmer(target_absolute_change~
                            # Fixed Effects 
                            1+target_lag_absolute_error+
                             I(target_lag_absolute_error^2)+
                             # Random Effects
                            (1+target_lag_absolute_error+
                               I(target_lag_absolute_error^2)|participant)+
                            (1|block),
                          data=ACQ4, REML=FALSE,
                          control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=5e5)))



RE_mod_CHANGE_cube <- lmer(target_absolute_change~
                             # Fixed Effects 
                             1+target_lag_absolute_error+
                             I(target_lag_absolute_error^2)+
                             I(target_lag_absolute_error^3)+
                             # Random Effects
                             (1+target_lag_absolute_error+
                                I(target_lag_absolute_error^2)|participant)+
                             (1|block),
                           data=ACQ4, REML=FALSE,
                           control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=5e5)))

summary(RE_mod_CHANGE_cube)

anova(RE_mod_CHANGE_linear, RE_mod_CHANGE_quad, RE_mod_CHANGE_cube)


target_mod_CHANGE <- lmer(target_absolute_change~
                        # Fixed Effects 
                        block*group*target_lag_absolute_error+
                        block*group*I(target_lag_absolute_error^2)+
                        block*group*I(target_lag_absolute_error^3)+
                        # Random Effects
                        (1+target_lag_absolute_error+
                           I(target_lag_absolute_error^2)|participant)+
                        (1|Target),
                      data=ACQ4, REML=TRUE,
                      control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=5e5)))

summary(target_mod_CHANGE)
anova(target_mod_CHANGE)

# Blocked 
1.099e-01
1.099e-01-4.306e-03
1.099e-01+1.225e-02

# Random 
1.099e-01+7.465e-02
1.099e-01-4.306e-03+7.465e-02+5.288e-03
1.099e-01+1.225e-02+7.465e-02-6.534e-02

target_mod_CHANGE.c <- lmer(target_absolute_change~
                            # Fixed Effects 
                            block*group*target_lag_absolute_error.c+
                            block*group*I(target_lag_absolute_error.c^2)+
                            block*group*I(target_lag_absolute_error.c^3)+
                            # Random Effects
                            (1+target_lag_absolute_error.c+
                               I(target_lag_absolute_error.c^2)|participant)+
                            (1|Target),
                          data=ACQ4, REML=TRUE,
                          control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=5e5)))

summary(target_mod_CHANGE.c)
anova(target_mod_CHANGE)

# Blocked
4.617e-01
4.617e-01+8.530e-03
4.617e-01-1.908e-02

# Random
4.617e-01-3.705e-03
4.617e-01+8.530e-03-3.705e-03-5.803e-03
4.617e-01-1.908e-02-3.705e-03-3.520e-02


block <- c(rep(c(rep("1", 21), rep("2", 21), rep("3", 21)),2))
lag1_error <- c(rep(c(rep(seq(from=0, to=1, by=0.05),3)),2))
group <- c(rep("Blocked", 63), rep("Random", 63))

pred_AE <- c(1:length(block))

TARGET_PRED_DATA <- data.frame(group, block, lag1_error, pred_AE)

LIST <- unique(block)

for (i in 1:length(TARGET_PRED_DATA$pred_AE)){
  if(TARGET_PRED_DATA$block[i]=="1" && TARGET_PRED_DATA$group[i]=="Blocked"){
    TARGET_PRED_DATA$pred_AE[i] <- (1.099e-01)+(6.977e-01)*TARGET_PRED_DATA$lag1_error[i]+(-1.457e-01)*(TARGET_PRED_DATA$lag1_error[i]^2)+(3.149e-01)*(TARGET_PRED_DATA$lag1_error[i]^3)
  } 
  
  else if(TARGET_PRED_DATA$block[i]=="2" && TARGET_PRED_DATA$group[i]=="Blocked"){
    TARGET_PRED_DATA$pred_AE[i] <- (1.099e-01-4.306e-03)+(6.977e-01-2.225e-01)*TARGET_PRED_DATA$lag1_error[i]+(-1.457e-01+9.527e-01)*(TARGET_PRED_DATA$lag1_error[i]^2)+(3.149e-01-9.129e-01)*(TARGET_PRED_DATA$lag1_error[i]^3)
  } 
  
  else if(TARGET_PRED_DATA$block[i]=="3" && TARGET_PRED_DATA$group[i]=="Blocked"){
    TARGET_PRED_DATA$pred_AE[i] <- (1.099e-01+1.225e-02)+(6.977e-01-4.219e-01)*TARGET_PRED_DATA$lag1_error[i]+(-1.457e-01+1.054e+00)*(TARGET_PRED_DATA$lag1_error[i]^2)+(3.149e-01-6.705e-01)*(TARGET_PRED_DATA$lag1_error[i]^3)
  } 
  
  else if(TARGET_PRED_DATA$block[i]=="1" && TARGET_PRED_DATA$group[i]=="Random"){
    TARGET_PRED_DATA$pred_AE[i] <- (1.099e-01+7.465e-02)+(6.977e-01-7.324e-01)*TARGET_PRED_DATA$lag1_error[i]+(-1.457e-01+1.766e+00)*(TARGET_PRED_DATA$lag1_error[i]^2)+(3.149e-01-1.230e+00)*(TARGET_PRED_DATA$lag1_error[i]^3)
  } 
  
  else if(TARGET_PRED_DATA$block[i]=="2" && TARGET_PRED_DATA$group[i]=="Random"){
    TARGET_PRED_DATA$pred_AE[i] <- (1.099e-01-4.306e-03+7.465e-02+5.288e-03)+(6.977e-01-2.225e-01-7.324e-01-1.585e-02)*TARGET_PRED_DATA$lag1_error[i]+(-1.457e-01+9.527e-01+1.766e+00-1.144e-01)*(TARGET_PRED_DATA$lag1_error[i]^2)+(3.149e-01-9.129e-01-1.230e+00+2.035e-01)*(TARGET_PRED_DATA$lag1_error[i]^3)
  } 
  
  else if(TARGET_PRED_DATA$block[i]=="3" && TARGET_PRED_DATA$group[i]=="Random"){
    TARGET_PRED_DATA$pred_AE[i] <- (1.099e-01+1.225e-02+7.465e-02-6.534e-02)+(6.977e-01-4.219e-01-7.324e-01+8.522e-01)*TARGET_PRED_DATA$lag1_error[i]+(-1.457e-01+1.054e+00+1.766e+00-2.433e+00)*(TARGET_PRED_DATA$lag1_error[i]^2)+(3.149e-01-6.705e-01-1.230e+00+1.698e+00)*(TARGET_PRED_DATA$lag1_error[i]^3)
  } 
  
  else {TARGET_PRED_DATA$pred_AE[i] <- NA}
  
}

ggplot(ACQ4, aes(x =target_lag_absolute_error, y = target_absolute_change)) +
  #geom_point(aes(group=group, col=group), alpha=0.2, shape=16)+
  geom_abline(intercept=0, slope=1, lty=1, col="red", lwd=0.5)+
  stat_smooth(aes(group=participant), method="lm",
              formula = y~x+I(x^2), alpha=0.8, se=FALSE, lwd=0.5, col="grey80") +
  geom_line(data= TARGET_PRED_DATA, aes(x =lag1_error, y = pred_AE, lty=group), 
            lwd=1, col="black") +
  facet_wrap(~group+block)+
  scale_x_continuous(name = expression(bold(Absolute~Error~n[k]~(s))), limits=c(0,1)) +
  scale_y_continuous(name = expression(bold(Change~n[k]+1~(s))), limits=c(0,1)) +
  theme_bw()+
  scale_fill_manual(values=c("grey90", "grey30"))+
  scale_color_manual(values=c("grey90", "grey30"))+
  labs(col="Group", lty="Group")+
  theme(axis.text=element_text(size=12, color="black"), 
        legend.text=element_text(size=12, color="black"),
        axis.title=element_text(size=12, face="bold"),
        plot.title=element_text(size=12, face="bold", hjust=0.5),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=12, face="bold"),
        legend.title=element_blank(),
        legend.position = "bottom")





RE_mod <- lmer(target_absolute_change~
                             # Fixed Effects 
                             1+
                             # Random Effects
                             (1+target_lag_absolute_error+
                                I(target_lag_absolute_error^2)|participant)+
                             (1|block),
                           data=ACQ4, REML=FALSE,
                           control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=5e5)))

RE_MOD <- rownames_to_column(ranef(RE_mod)$participant)
head(RE_MOD)
head(MERGED)
MERGED <- merge(x=MERGED, y=RE_MOD, by.x="participant", by.y="rowname")
head(MERGED)




# 5.0 Correlations with long-term learning ----
# Contrast coding group and det_target
MERGED$rand.c <-as.numeric(MERGED$group)-1.5
MERGED$det_Target.c <- MERGED$det_Target - mean(MERGED$det_Target)
MERGED$intercept.c <- MERGED$`(Intercept)` - mean(MERGED$`(Intercept)`)
MERGED$slope.c <- MERGED$target_lag_absolute_error - mean(MERGED$target_lag_absolute_error)


head(ACQ3)
PRAC <- ACQ3 %>% group_by(participant) %>%
  summarize(acq_CE = mean(constant_error, na.rm=TRUE),
            acq_AE = mean(absolute_error, na.rm=TRUE),
            acq_TE = sd(constant_error, na.rm=TRUE))

MERGED <- merge(MERGED, PRAC, by="participant")


head(ACQ_by_TARGET)
AFTER_KR <- ACQ_by_TARGET %>% select(participant, lag_KR, target_absolute_change) %>%
  group_by(participant, lag_KR) %>%
  summarize(mean_Change = mean(abs(target_absolute_change), na.rm=TRUE),
            SD_Change = sd(target_absolute_change, na.rm=TRUE)) %>%
  pivot_wider(names_from = lag_KR, values_from=mean_Change:SD_Change)

MERGED <- merge(MERGED, AFTER_KR, by="participant")
head(MERGED)

plot(x=MERGED$`SD_Change_No KR`, y=MERGED$acq_TE)
plot(x=MERGED$`mean_Change_No KR`, y=MERGED$acq_AE)

plot(x=MERGED$`SD_Change_No KR`, y=MERGED$ave_ae_Retention)
plot(x=MERGED$`mean_Change_No KR`, y=MERGED$ave_ae_Retention)



# Regression Analysis: Det on AE Retention -------------------------------------
mod1 <- lm(det_Target~rand.c+ave_ae_Retention,
           data=MERGED)
summary(mod1)

mod2 <- lm(ave_ae_Retention~rand.c+det_Target.c,
           data=MERGED)
summary(mod2)
vif(mod2)

head(MERGED)
p <- ggplot(MERGED, aes(x = det_Target , y = ave_ae_Retention)) +
  geom_point(aes(col=group, shape=group))+ 
  stat_smooth(aes(lty=group, col=group), method="lm", se=FALSE, 
              lwd=0.75) + 
  #stat_ellipse(aes(group=group, col=group), lwd=0.75) +
  scale_color_manual(values=c("grey70", "grey30"))+
  scale_fill_manual(values=c("grey70", "grey30"))+
  labs(col="Group", fill="Group", lty="Group", shape="Group")+
  scale_x_continuous(name = "Determinant over Five Trials") +
  scale_y_continuous(name = "Average Retention AE (s)", limits = c(0,0.75)) +
  theme_bw()+
  theme(axis.text=element_text(size=12, color="black"), 
        legend.text=element_text(size=12, color="black"),
        axis.title=element_text(size=12, face="bold"),
        plot.title=element_text(size=12, face="bold", hjust=0.5),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=12, face="bold"),
        legend.title=element_blank(),
        legend.position = "bottom")

ggMarginal(p,
           type = 'boxplot',
           margins = 'both',
           size = 5, colour = 'black',
           groupFill = TRUE)



# Regression Analysis: Mean CHANGE following No KR on Retention ---------------------------
head(MERGED)
mod1 <- lm(`mean_Change_No KR`~rand.c+ave_ae_Retention,
           data=MERGED)
summary(mod1)

mod2 <- lm(ave_ae_Retention~rand.c+`mean_Change_No KR`,
           data=MERGED)
summary(mod2)
vif(mod2)


head(MERGED)
p<-ggplot(MERGED, aes(x = `mean_Change_No KR`, y = ave_ae_Retention)) +
  geom_point(aes(col=group, shape=group))+ 
  stat_smooth(aes(lty=group, col=group), method="lm", se=FALSE, 
              lwd=0.75) + 
  #stat_ellipse(aes(group=group, col=group), lwd=0.75) +
  scale_color_manual(values=c("grey70", "grey30"))+
  scale_fill_manual(values=c("grey70", "grey30"))+
  labs(col="Group", fill="Group", lty="Group", shape="Group")+
  scale_x_continuous(name = "Mean Change following no KR") +
  scale_y_continuous(name = "Average Retention AE (s)") +
  theme_bw()+
  theme(axis.text=element_text(size=12, color="black"), 
        legend.text=element_text(size=12, color="black"),
        axis.title=element_text(size=12, face="bold"),
        plot.title=element_text(size=12, face="bold", hjust=0.5),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=12, face="bold"),
        legend.title=element_blank(),
        legend.position = "bottom")

ggMarginal(p,
           type = 'boxplot',
           margins = 'both',
           size = 5, colour = 'black',
           groupFill = TRUE)





# Regression Analysis: Slope CHANGE on Retention ---------------------------
mod1 <- lm(slope.c~rand.c+ave_ae_Retention,
           data=MERGED)
summary(mod1)

mod2 <- lm(ave_ae_Retention~rand.c+slope.c,
           data=MERGED)
summary(mod2)
vif(mod2)


head(MERGED)
p<-ggplot(MERGED, aes(x = target_lag_absolute_error, y = ave_ae_Retention)) +
  geom_point(aes(col=group, shape=group))+ 
  stat_smooth(aes(lty=group, col=group), method="lm", se=FALSE, 
              lwd=0.75) + 
  #stat_ellipse(aes(group=group, col=group), lwd=0.75) +
  scale_color_manual(values=c("grey70", "grey30"))+
  scale_fill_manual(values=c("grey70", "grey30"))+
  labs(col="Group", fill="Group", lty="Group", shape="Group")+
  scale_x_continuous(name = "Slope in Mixed-Model") +
  scale_y_continuous(name = "Average Retention AE (s)") +
  theme_bw()+
  theme(axis.text=element_text(size=12, color="black"), 
        legend.text=element_text(size=12, color="black"),
        axis.title=element_text(size=12, face="bold"),
        plot.title=element_text(size=12, face="bold", hjust=0.5),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=12, face="bold"),
        legend.title=element_blank(),
        legend.position = "bottom")

ggMarginal(p,
           type = 'boxplot',
           margins = 'both',
           size = 5, colour = 'black',
           groupFill = TRUE)


head(MERGED)
mod2 <- lm(ave_ae_Retention~rand.c+det_Target+acq_AE,
           data=MERGED)
summary(mod2)

mod2 <- lm(ave_ae_Retention~rand.c+intercept.c+acq_AE,
           data=MERGED)
summary(mod2)

mod2 <- lm(ave_ae_Retention~rand.c+slope.c+acq_AE,
           data=MERGED)
summary(mod2)
