library(dplyr)
library(tidyr)
library(ggplot2)
library(anytime)
library(forecast)

options(scipen = 999)

setwd('C:/Users/mjoy/Documents/MarissaJCKMChallenge/')
comps = read.csv('./CompData.csv')

nrow(comps)
head(comps)

is.ts(comps$change_ts)
#start(comps$change_ts)
#end(comps$change_ts)

#frequency(comps$change_ts)

#autoplot(comps$change_ts, facets = FALSE)

#comps$change_ts <- msts(comps$change_ts, start = 1514764800000, frequency = 24)

is.ts(comps$change_ts)


 # comps$component_name <- gsub('COMPONENT', "",x = comps$component_name)
# comps$component_name <- gsub("(^|[^0-9])0+", "\\1", comps$component_name) 
comps$component_name <- as.character(comps$component_name)

#sort file by component names and it's timestamp
ordered = comps %>%
  arrange(component_name, change_ts)
#write.csv(ordered, "Ordered.csv")

##from this i can see the process that each component goes through
# component0: YBSFJ_00_01 -> JX3UO_01_01 -> ZZU3X_02_00 -> B57X3_03_00 -> ASSEMBLY
# component1: TG61A_00_02 -> MRX5B_01_00 -> HUX1L_02_02 -> 29MSJ_03_01 -> ASSEMBLY
# component2: VUFVH_00_00 -> 5YU1V_01_02 -> ZAENM_02_01 -> 7EFLOP_03_02 -> ASSEMBLY
# component3: GH9CV_00_03 -> 5YU1V_01_02 -> ZZU3X_02_00 -> 29MSJ_03_01 -> ASSEMBLY

target <- c("YBSFJ_00_01", "TG6IA_00_02", "VUFVH_00_00", "GH9CV_00_03", "assembly room")
target1 <- c("YBSFJ_00_01", "TG6IA_00_02", "VUFVH_00_00", "GH9CV_00_03")
target2 <- c(" JX3UO_01_01", "MRX5B_01_00", "5YU1V_01_02")
target3 <- c("ZZU3X_02_00", "HUX1L_02_02", "ZAENM_02_01")
target4 <- c("B57X3_03_00", "29MSJ_03_01", "7EFLOP_03_02")
target5 <- "assembly room"


#get rate of ouput per component 
roo <- ordered %>%
  filter(., location %in% target) %>%
  group_by(., component_type, location) %>%
  count(., component_type) %>%
  mutate(., stat = ifelse(location == "assembly room", "end", "initial")) %>%
  arrange(., component_type, n) 

roo$location <- NULL

rates <- roo %>%
  ungroup() %>%
  spread(., stat, n) %>%
  mutate(., rate_output = (end/initial) * 100)
rates
##component 3 has worst rate of output at 5.23%


# #convert change_ts to timestamps
comps$times <- as.numeric(substr(comps$change_ts, 1, nchar(comps$change_ts) - 3))
comps$times <- anytime(comps$times)

#get average time it takes specific component to 
#go from first machine to assembly room 
name_diff <- comps %>%
  group_by(., component_name) %>%
  mutate(., diff = as.numeric(difftime(max(times), min(times))))

diffs <- name_diff %>%
  group_by(., component_type) %>%
  summarise(., avg = mean(diff))
diffs
#ZZU3X_02_00 is the only machine that is shared by 0 and 3
#so it seems like there is a slow down there 


l <- comps %>%
  select(., component_name, component_type, location, times) %>%
  arrange(., component_name)

dput(head(l, 13))



loc_diff <- comps %>%
          group_by(., component_type, location) %>%
          mutate(., diff = as.numeric(difftime(max(times), min(times))))
ld <- loc_diff %>%
      group_by(., location) %>%
      summarise(., avg = mean(diff))


dts <- decompose(comps$change_ts, type = "additive")
plot(dts)
stlRes <- stl(comps$change_ts, s.window = "periodic")
stlRes




#dataset separated by each component
comp0 <- comps %>%
  filter(., component_type == "component_0")

comp1 <- comps %>%
  filter(., component_type == "component_1")

comp2 <- comps %>%
  filter(., component_type == "component_2")

comp3 <- comps %>%
  filter(., component_type == "component_3")

colSums(is.na(diff))


#view how many compoenents go through each location
cl_ct = ordered %>%
  group_by(., component_type, location) %>%
  count(., component_type)

ggplot(cl_ct, aes(component_type, n)) + 
  geom_bar(aes(fill = location), position = "dodge", stat = "identity")

cl <- cl_ct %>% spread(., location, n)

#view how many waiting/in progress/complete components at each location
loc_stat = ordered %>%
    group_by(., location) %>%
    count(., status)

ggplot(loc_stat, aes(location, n)) + 
  geom_bar(aes(fill = status), position = "dodge", stat = "identity")

ls <- loc_stat %>% spread(., status, n)

status_count <- comps %>%
  group_by(., component_type) %>%
  count(., status)

#bar plot of components and their status 
ggplot(status_count, aes(component_type, n)) + 
  geom_bar(aes(fill = status), position = "dodge", stat = "identity")

#component 0 and 3 spend the most time in waiting/inprogress 

#location count by compenent
location_count <- comps %>%
  group_by(., component_type) %>%
  count(., location)

ggplot(location_count, aes(component_type, n)) + 
  geom_bar(aes(fill = location), position = "dodge", stat = "identity")


#total location count 
l2 <- comps %>%
  group_by(., location) %>%
  count(., location)

ggplot(l2, aes(location, n)) + 
  geom_bar(aes(fill = location), position = "dodge", stat = "identity")

by_loc = comps %>%
  group_by(., component_name) %>%
  mutate(., id = 1:n()) %>%
 # reshape::melt(., id = c("id", "location", "component_type"))
  spread(., status, location) %>%
  

u <- lapply(in_progf, function(x) unique(x))

ggplot(completed, aes(location, n)) + 
  geom_bar(aes(fill = location), position = "dodge", stat = "identity")
