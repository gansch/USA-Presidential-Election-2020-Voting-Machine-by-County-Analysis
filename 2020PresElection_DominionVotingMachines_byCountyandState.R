########################## #3#####
#
#     2020 Presidential Dominion Voting Machine Analysis
#           by,
#           Benjamin Ganschow
#
#############################3 ##
options(scipen=999)
library(tidyverse)
library(lme4)
library(lattice)

df.votes <- read_csv("https://raw.githubusercontent.com/tonmcg/US_County_Level_Election_Results_08-20/master/2020_US_County_Level_Presidential_Results.csv")
# df.votes2 <- read_csv("https://github.com/kjhealy/us_elections_2020_csv") #retrieve alternate dataset, haven't tried

#voting machine data retrieved 21/12/20 at https://verifiedvoting.org/verifier/#mode/navigate/map/ppEquip/mapType/normal/year/2020
df.machines <- read_csv("https://raw.githubusercontent.com/gansch/USA-Presidential-Election-2020-Voting-Machine-by-County-Analysis/main/verifier-machines.csv", skip=1)


# What are the machines? How many are there? --------------------------------------------------

#prevalence?
sort(summary(factor(df.machines$Make))) ##three types of dominion: "Dominion Voting Systems", "Premier/Diebold (Dominion), "Sequoia (Dominion)"
#most is election systems and software
#second most is "not applicable"...

#what does it mean when make == "Not Applicable"
df.whyNA<-df.machines[df.machines$Make == "Not Applicable",] #not applicable means the ballots are hand-counted

#what type of machines are from dominion?
df.dominion<-df.machines[df.machines$Make == "Dominion Voting Systems",] # Dominion means these are all ballot marking devices


#df.machines$county_fips <- factor(substring(df.machines$FIPS.code,1,5)) #county FIPS data, this is broken...

#how many machines are used per state?
machine_state <- df.machines %>%
  group_by(State)%>%
  count()#why does michigan need 5800 voting machines?

machine_county <-df.machines %>%
  group_by(State,County)%>%
  count() #why do some counties need over 200 machines?

#Are the machine types uniform by state and by county (i.e. where is the variance of DOminion/non-Domininon)?
#lump together top ten machine makers
df.machines$MostMake <- fct_lump(df.machines$Make,10) #make sure:"Dominion Voting Systems", "Premier/Diebold (Dominion), "Sequoia (Dominion)"

county_company <-df.machines %>%
  group_by(State,County) %>% 
  count(MostMake)%>%
  pivot_wider(names_from=MostMake, values_from=n,
              values_fill=0) ### Georgia is uniform

#Conclusion: Wisconsin needs almost 4-5 times more than the next largest state
#NA is paper count
#three types of dominion systems


# Dominion Analysis Dataset Merge -------------------------------------------------------
# make Dominion/non-dominion factor
df.machines$Dominion <- fct_collapse(df.machines$MostMake,
                                     Paper = "Not Applicable",
                                     Other = c("Democracy Live", "Election Systems & Software", "KNOWiNK", "Other", "VotingWorks", "Unisyn Voting Solutions"),
                                     Hart = "Hart InterCivic",
                                     Dominion = c("Dominion Voting Systems", "Premier/Diebold (Dominion)", "Sequoia (Dominion)"))
summary(df.machines$Dominion)

usa_county_company <-df.machines %>%
  group_by(State,County) %>% 
  count(Dominion)%>%
  pivot_wider(names_from=Dominion, values_from=n,
              values_fill=0)

#the machines dataset has some odd spaces in the names
usa_county_company$County <- factor(str_replace_all(usa_county_company$County, " ", "")) 
#usa_county_company$State <- factor(str_replace_all(usa_county_company$State," ",""))

#the vote dataset has name + "county/parish/city"
county <- str_split(df.votes$county_name, c(" County"),simplify=TRUE, n=5)
county <- str_split(county[,1], c(" Parish"),simplify=TRUE)
county <- str_split(county[,1], c(" city"),simplify=TRUE)
county <- str_replace_all(county[,1], " ", "")

df.votes$County <- factor(county)
df.votes$State <-  factor(df.votes$state_name)

#there are some missing data: 10 or so counties with weird names, Alaska "districts", a few wierder names, all non-states like DC, Puerto Rico, Somoa
df.usa <- full_join(df.votes, usa_county_company, by = c("State","County")) #by = c("state_name","county_name")) #good enough, missing Alaska, some Miss, some Ill

df.usa$Dom <- ifelse(df.usa$Dominion==0, "No", "Yes") #Purely dominion or not

# County wide voting due to dominion --------------------------------------

#percentage of voting democratic
mod.county.per <- lm(data=df.usa, 
                    per_dem ~ Dom) 
summary(mod.county.per) #having a dominion voting machine adds 7-8% per county

#pure difference scores (diff= Trump Votes - Biden Votes)
mod.county.diff <- lm(data=df.usa, 
              diff ~ Dom) 
summary(mod.county.diff) # dominion adds 6278 +/- 2243 for Biden


# Within/Between State County Analysis by Dominion Machine ----------------

#percentage of democratic vote of counties grouped into states
mod.state.per <- lmer(data=df.usa, 
                    per_dem ~ Dom + (1+Dom|State)) 

summary(mod.state.per) 
ranef(mod.state.per)
dotplot(ranef(mod.state.per)) #states without dominion machines have reliable "intercept"/unreliable "DomYes"
#the states with reliable intercept have no dominion machines, or in Georgia's case, all counties have dominion machines.

#difference of democratic vote of counties grouped into states
mod.state.diff <- lmer(data=df.usa, 
                      diff ~ Dom + (1+Dom|State)) 

summary(mod.state.diff) 
ranef(mod.state.diff)
dotplot(ranef(mod.state.diff)) #states without dominion machines have reliable "intercept"/unreliable "DomYes"
#the states with reliable intercept have no dominion machines, or in Georgia's case, all counties have dominion machines.
###hmmm lots of difference betweeen states in unexpected direction - Dominion helps Trump in CA and NY???


# States with Equal Dom/Non-Dom Counties -------------------------------------------------
#finding states that have more than one county with dominion voting machines
states_diffvote <-df.usa %>%
  group_by(State)%>%
  count(Dom) %>%
  pivot_wider(names_from=Dom, values_from=n,
              values_fill=0)
  
#these are states with more than 1 county using dominion
statesdom <- c("Illinois", "California", "Indiana", "Iowa", "Kansas", "Michigan", "Mississippi", "New York", "Ohio", "Wisconsin", "Tennessee")

df.usa.filterdom <- df.usa %>%
  filter(State %in% statesdom) #select these states with differences

#to find differences, we would expect dominion voting machines in battleground states to add to the Biden percentage or raw vote count in comparison to other states

#percentage democrat between states using dominion + non-dominion machines
mod.state.filterdom.per <- lmer(data=df.usa.filterdom,
                                 per_dem ~ Dom + (1+Dom|State))
summary(mod.state.filterdom.per) 
ranef(mod.state.filterdom.per)# the dominion machines greatly benefit Trump in California and New York, while tend to skew Biden in the other states
dotplot(ranef(mod.state.filterdom.per)) 

#voting difference between states using dominion + non-dominion machines
#this would mean that certain countie are really influential for the state election as a whole if the 
mod.state.filterdom.diff <- lmer(data=df.usa.filterdom,
                        diff ~ Dom + (1+Dom|State))
summary(mod.state.filterdom.diff) 
ranef(mod.state.filterdom.diff)# the dominion machines greatly benefit Trump in California and New York, while tend to skew Biden in the other states
dotplot(ranef(mod.state.filterdom.diff)) #odd democratic boost from machines in Tennessee and Mississippi



