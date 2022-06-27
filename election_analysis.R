### PART 2
library(car)
library(dplyr)  
library(ggplot2)
library(tidyverse)
library(GGally)
library(magrittr)
election <-"~/Desktop/anes_timeseries_2020_csv_20220210.csv"
election_df<-read.csv(election)
#summary(election_df)
var.keep <- c("V202119", "V202066", "V202121", "V201228", "V202117") 
election_df_new <-election_df [ ,var.keep]
colnames(election_df_new) <- c('voting_difficulty_origin','voting_status_origin','waiting_time_origin','party_of_registration_origin','voting_mode_origin')

election_df_new$voting_difficulty_origin<-ifelse(election_df_new$voting_difficulty_origin==-9,'Refused',
                                                 ifelse(election_df_new$voting_difficulty_origin==-7,'No post-election data, deleted due to incomplete interview',
                                                        ifelse(election_df_new$voting_difficulty_origin==-6,'No post-election interview',
                                                               ifelse(election_df_new$voting_difficulty_origin==-5,'Interview breakoff (sufficient partial IW)',
                                                                      ifelse(election_df_new$voting_difficulty_origin==-1,'Inapplicable',
                                                                             ifelse(election_df_new$voting_difficulty_origin==1,'Not difficult at all',
                                                                                    ifelse(election_df_new$voting_difficulty_origin==2,'A little difficult',
                                                                                           ifelse(election_df_new$voting_difficulty_origin==3,'Moderately difficult',
                                                                                                  ifelse(election_df_new$voting_difficulty_origin==4,'Very difficult','Extremely difficult')))))))))

election_df_new$voting_status_origin<-ifelse(election_df_new$voting_status_origin==-9,'Refused',
                                             ifelse(election_df_new$voting_status_origin==-7,'No post-election data, deleted due to incomplete interview',
                                                    ifelse(election_df_new$voting_status_origin==-6,'No post-election interview',
                                                           ifelse(election_df_new$voting_status_origin==-1,'Inapplicable',
                                                                  ifelse(election_df_new$voting_status_origin==1,'I did not vote (in the election this November)',
                                                                         ifelse(election_df_new$voting_status_origin==2,'I thought about voting this time, but didn’t',
                                                                                ifelse(election_df_new$voting_status_origin==3,'I usually vote, but didn’t this time', 'I am sure I voted')))))))


election_df_new$waiting_time_origin<-ifelse(election_df_new$waiting_time_origin==-9,'Refused',
                                            ifelse(election_df_new$waiting_time_origin==-7,'No post-election data, deleted due to incomplete interview',
                                                   ifelse(election_df_new$waiting_time_origin==-6,'No post-election interview',
                                                          ifelse(election_df_new$voting_difficulty_origin==-5,'Interview breakoff (sufficient partial IW)',
                                                                 ifelse(election_df_new$waiting_time_origin==-1,'Inapplicable',
                                                                        ifelse(election_df_new$waiting_time_origin==1,'0-15 minutes',
                                                                               ifelse(election_df_new$waiting_time_origin==2,'16-30 minutes',
                                                                                      ifelse(election_df_new$waiting_time_origin==3,'31-59 minutes',
                                                                                             ifelse(election_df_new$waiting_time_origin==4,'1-2 hours','More than 2 hours')))))))))

election_df_new$party_of_registration_origin<-ifelse(election_df_new$party_of_registration_origin==-9,'Refused',
                                                     ifelse(election_df_new$party_of_registration_origin==-8,'Don’t know',
                                                            ifelse(election_df_new$party_of_registration_origin==-4,'Technical error',
                                                                   ifelse(election_df_new$party_of_registration_origin==0,'No preference {VOL - video/phone only}',
                                                                          ifelse(election_df_new$party_of_registration_origin==1,'Democrat',
                                                                                 ifelse(election_df_new$party_of_registration_origin==2,'Republican',
                                                                                        ifelse(election_df_new$party_of_registration_origin==3,'Independen','Other party')))))))


election_df_new$voting_mode_origin<-ifelse(election_df_new$voting_mode_origin==-9,'Refused',
                                           ifelse(election_df_new$voting_mode_origin==-8,'Don’t know',
                                                  ifelse(election_df_new$voting_mode_origin==-7,'No post-election data, deleted due to incomplete interview',
                                                         ifelse(election_df_new$voting_mode_origin==-6,'No post-election interview',
                                                                ifelse(election_df_new$voting_mode_origin==-5,'Interview breakoff (sufficient partial IW)',
                                                                       ifelse(election_df_new$voting_mode_origin==-1,'Inapplicable',
                                                                              ifelse(election_df_new$voting_mode_origin==1,'In person, at the polling place','By mail or absentee ballot')))))))

# only focus on repulican  and democratic party 

df_democratic_republican <-filter(election_df_new,party_of_registration_origin=='Democrat' | party_of_registration_origin=='Republican')

dat <- data.frame(table(df_democratic_republican$voting_difficulty_origin,df_democratic_republican $party_of_registration_origin))
names(dat) <- c("voting_difficulty","party_of_registration","Count")
ggplot(data=dat, aes(y=voting_difficulty, x=Count, fill=party_of_registration)) + geom_bar(stat="identity")+ 
  scale_fill_manual("legend", values = c("Republican" = "red", "Democrat" = "blue"))+  ggtitle('Voting Difficulty Between Republican & Democratic')

dat2 <- data.frame(table(df_democratic_republican$voting_status_origin,df_democratic_republican $party_of_registration_origin))
names(dat2) <- c("voting_status","party_of_registration","Count")
ggplot(data=dat2, aes(x= Count, y=voting_status, fill=party_of_registration)) + geom_bar(stat="identity")+ 
  scale_fill_manual("legend", values = c("Republican" = "red", "Democrat" = "blue"))+  ggtitle('Voting Status Between Republican & Democratic')

dat3 <- data.frame(table(df_democratic_republican$waiting_time_origin,df_democratic_republican $party_of_registration_origin))
names(dat3) <- c("waiting_time","party_of_registration","Count")
ggplot(data=dat3, aes(y= waiting_time, x=Count, fill=party_of_registration)) + geom_bar(stat="identity")+ 
  scale_fill_manual("legend", values = c("Republican" = "red", "Democrat" = "blue"))+  ggtitle('Voting Waiting Time Between Republican & Democratic')

dat4 <- data.frame(table(df_democratic_republican$voting_mode_origin,df_democratic_republican $party_of_registration_origin))
names(dat4) <- c("voting_mode","party_of_registration","Count")
ggplot(data=dat4, aes( x=voting_mode,y= Count, fill=party_of_registration)) + geom_bar(stat="identity")+ 
  scale_fill_manual("legend", values = c("Republican party" = "red", "Democratic party" = "blue"))+  ggtitle('Voting Mode Time Between Republican & Democratic')


# only focus on those I usually vote, but didn’t this time  , we want to see if there is any difficulities that people who always vote but didn't vote had but we didn't find anything there
filter(df_democratic_republican,voting_status_origin=='I usually vote, but didn’t this time')
# only focus on those who didn't vote , we wanted to know what happened to people who din't vote if the wait time was intolerable or if their voting mode was different but we didn't find anything
filter(df_democratic_republican,voting_status_origin=='I did not vote (in the election this November)')
# people who voted 
filter(df_democratic_republican,voting_status_origin=='I am sure I voted')


