library(dplyr)
library(tidyr)
library(ggplot2)
library(anytime)

options(scipen = 999)

setwd('C:/Users/mjoy/Documents/MarissaJCKMChallenge/')
comps = read.csv('./CompData.csv')

nrow(comps)
head(comps)

#convert change_ts to timestamps
comps$change_ts <- substr(comps$change_ts, 1, nchar(comps$change_ts) - 3)
comps$change_ts <- as.numeric(comps$change_ts)
comps$change_ts <- anytime(comps$change_ts)


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

t <- c(target)
#filter by starting and final location to get difference
times = ordered1 %>%
  group_by(component_type) %>%
  filter(., location %in% target) 

times2 = as.data.frame(times)
times2$component_type <- NULL
times2$location <- NULL
#times2 = times2 %>% tibble::rowid_to_column(., "ID")
times2 <- times2[!duplicated(times2), ]
colSums(is.na(times2))

#spread to get status as columns to get difference btw first and final
r <- times2 %>% 
  group_by(., component_name) %>%
  spread(., status, change_ts) %>%
  mutate(., tot_time = as.numeric(difftime(complete, `in progress`)))

sum(is.na(r$tot_time))

r$complete <- NULL
r$`in progress` <- NULL

comp = comps[, c('component_name', "component_type")]

diff <- merge(comp, r, by = "component_name", all.x = TRUE)
diff = diff[!duplicated(diff),]
diff$component_name <- NULL

colSums(is.na(diff))

diff_g <- diff %>%
            group_by(., component_type) %>%
            summarise(., avg = mean(as.numeric(tot_time), na.rm = TRUE))

times %>% group_by(., component_type) %>%
  mutate()

countbyloc = ordered %>%
  group_by(., component_type, location) %>%
  count(., component_type)



ggplot(cl_ct, aes(component_type, n)) + 
  geom_bar(aes(fill = location), position = "dodge", stat = "identity")


loc_stat = ordered %>%
    group_by(., location) %>%
    count(., status)

ggplot(loc_stat, aes(location, n)) + 
  geom_bar(aes(fill = status), position = "dodge", stat = "identity")


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
