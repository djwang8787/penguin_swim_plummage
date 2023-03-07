pacman::p_load(dplyr, reshape2, lubridate, ggplot2, ggpubr, lme4, MuMIn)

####1. Baseline data ####
bl = read.csv("baseline-data.csv")
bl$Date = bl[,1]
bl[,1] = NULL
bl$Date = as.POSIXct(bl$Date, format = "%m/%d/%Y")
bl$SUBJECT = recode(bl$SUBJECT,
                    "PI" = "Pierre",
                    "Y" = "Yellow",
                    "B" = "Blue",
                    "P" = "Purple",
                    "Z" = "Zoro",
                    "G" = "Gloria",
                    "GL" = "Gloria",
                    "H" = "Hansel",
                    'PE' = 'Percy',
                    'A' = 'Arlo',
                    'AI' = 'Aidan',
                    'BR' = 'Brix',
                    'S' = 'Sora',
                    "PA" = "Patricia")

bl$LOCATION = recode(bl$LOCATION,
                     "I" = 'Ice',
                     'PW' = 'Pebble Wash',
                     'R' = 'Rock',
                     'W' = 'Water',
                     'C' = 'Cliff')

bl$Locomotion = recode(bl$Locomotion,
                       "S" = "Stand",
                       "W" = "Walk",
                       "SL" = "Sleep",
                       "SW" = "Swim",
                       "CL" = "Climb",
                       "L" = "Lie",
                       "P" = "Preen")

bl$Behaviour = recode(bl$Behaviour,
                      "SL" = "Sleep",
                      'E' = 'Eat',
                      "P" = "Preen",
                      "D" = "Dive",
                      "W" = "Walk",
                      "IN-P" = "Interact.Penguin",
                      "IN-C" = "Interact.Cliff",
                      "IN-R" = "Interact.Rock",
                      "IN-M" = "Interact.Mirror")

bl = bl %>%
  dplyr::mutate(Species = ifelse(SUBJECT %in% c("Blue", "Yellow", "Purple"), "Gentoo",
                                 ifelse(SUBJECT %in% c("Pierre"), "NRH", "King")),
                Month = month(Date, label = T))

bl$Behaviour = as.factor(bl$Behaviour)
bl$Locomotion = as.factor(bl$Locomotion)

bl.summary = bl %>%
  group_by(Species, SUBJECT) %>%
  dplyr::summarise(Sleeping = sum(Behaviour == "Sleep"),
                   Preening = sum(Behaviour == "Preen"),
                   Standing = sum(Locomotion == "Stand"),
                   Lying = sum(Locomotion == "Lie"),
                   Total = sum(Sleeping, Preening, Standing, Lying))

bl.summary.prop = bl.summary %>%
  group_by(Species) %>%
  dplyr::mutate(Sleeping = Sleeping/Total*100,
                Preening = Preening/Total*100,
                Standing = Standing/Total*100,
                Lying = Lying/Total*100) %>%
  select(!Total)

bl.summary.prop = melt(bl.summary.prop, variable.name = "Behaviour", value.name = "Prop", id.var = c("Species"))

# Baseline data, occurrence % by counts of behaviours > 1% occurrence.
ggplot(bl.summary.prop, aes(x = Behaviour, y = Prop, fill = Species)) +
  geom_bar(stat = "identity",
           position = position_dodge(),
           colour = "black") +
  theme_minimal() +
  xlab('Behaviour') +
  ylab('Time spent (%)') +
  ggtitle("Baseline data") +
  theme(legend.position = "bottom")

####2. Trial data ####
data = read.csv("penguin_zoomonitor-data.csv")

new.data = data %>%
  filter(Edited == "No", Observer == "Yanti") %>%
  dplyr::mutate(Species = ifelse(Focal.Name %in% c("Blue", "Yellow", "Purple"), "Gentoo",
                                 ifelse(Focal.Name %in% c("Pierre"), "NRH", "King"))) %>%
  dplyr::select(SessionID, Observer, DateTime, Time, Month, Year, Hour,
                Species, Focal.Name, Location, Location.Duration, Locomotion,
                Locomotion.Duration, Behaviours,
                Behaviour.Duration)

####2.1 Locomotion analysis ####
locomotion.data = new.data %>%
  dplyr::select(SessionID, Observer, DateTime, Time, Month, Year, Hour,
                Species, Focal.Name, Locomotion,
                Locomotion.Duration)  %>%
  dplyr::mutate(Species = as.factor(Species),
                Focal.Name = as.factor(Focal.Name),
                Locomotion = as.factor(Locomotion)) %>%
  filter(Locomotion != "")

loc.data = locomotion.data %>%
  group_by(Species) %>%
  dplyr::summarise(Sleeping = sum(Locomotion == "Sleeping"),
                   Lying = sum(Locomotion == "Lying"),
                   Standing = sum(Locomotion == "Standing"))

####2.2 Behaviour analysis ####
behaviour.data = new.data %>%
  dplyr::select(SessionID, Observer, DateTime, Time, Month, Year, Hour,
                Species, Focal.Name,  Behaviours,
                Behaviour.Duration)%>%
  dplyr::mutate(Species = as.factor(Species),
                Focal.Name = as.factor(Focal.Name),
                Behaviours = as.factor(Behaviours)) %>%
  filter(Behaviours != "", Behaviours != "Pooping")

behav.data = behaviour.data %>%
  group_by(Species) %>%
  dplyr::summarise(Preening = sum(Behaviour == "Preening"))

merged.data = cbind(loc.data, behav.data$Preening)
merged.data$Preening = merged.data$`behav.data$Preening`

merged.data = merged.data %>%
  group_by(Species) %>%
  dplyr::mutate(Total = sum(Sleeping, Lying, Standing, Preening))

merged.data = merged.data %>%
  dplyr::summarise(Sleeping = Sleeping/Total*100,
                   Preening = Preening/Total*100,
                   Standing = Standing/Total*100,
                   Lying = Lying/Total*100) %>%
  melt(., variable.name = "Behaviour", id.var = c("Species"), value.name = "Prop")

# Trial data, occurrence % by counts of behaviours > 1% occurrence.
ggplot(merged.data, aes(x = Behaviour, y = Prop, fill = Species)) +
  geom_bar(stat = "identity", position = position_dodge(), colour = "black") +
  theme_minimal() +
  xlab('Behaviour') +
  ylab('Time spent (%)') +
  ggtitle("Trial data") +
  theme(legend.position = "bottom")

####2.3 activity budget comparison####
names(bl)
names(behaviour.data) # take Species, Focal.Name, Behaviours, Month, DateTime,
bl2 = bl  %>%
  rename(Focal.Name = SUBJECT,
         Behaviours = Behaviour) %>%
  mutate(Year)
  select(Focal.Name, Behaviours, Species, Month, Date)

behav.d2 = behaviour.data %>%
  select(Focal.Name, Behaviours, Species, Month, Date)

####3. BCS analysis ####
bcs = read.csv("PG-Study-Subjects.csv")
bcs$Date = bcs[,1]
bcs$Date = as.POSIXct(bcs$Date, format = "%m/%d/%Y")

bcs = bcs %>%
  dplyr::mutate(Species = as.factor(COMMON.NAME),
                Name = as.factor(NAME),
                Gender = as.factor(GENDER),
                Weight = bcs[,6],
                BCS = bcs[,7],
                Plumage = as.factor(PLUMAGE),
                Success = ifelse(Name %in% c("Pierre", "Gloria", "Hansel", "Patricia", "Brix",
                                                   "Aidan", "Arlo"), "Success", "Fail"),
                Month = month(Date, label = TRUE)) %>%
  select(Date, Species, Name, Gender, Weight, BCS, Plumage, Month, Success)

bcs = bcs %>%
  mutate(Month.index = month(Date, label = FALSE),
         Month.no = ifelse(Month %in% c("Jan", "Feb", "Mar"), Month.index+5, Month.index - 7))

w = bcs %>%
  group_by(Success, Month, Species) %>%
  dplyr::summarise(mean.w = mean(Weight),
                sd.w = sd(Weight),
                lower.w = mean.w - sd.w,
                higher.w = mean.w + sd.w,
                mean.bcs = mean(BCS),
                sd.bcs = sd(BCS),
                lower.bcs = mean.bcs-sd.bcs,
                higher.bcs = mean.bcs + sd.bcs) %>%
  replace(is.na(.), 0)

w %>% filter(Species == "King penguin", Success == "Success") %>%
ggplot() +
  geom_point(aes(x = Month, y = mean.w), size = 2) +
  geom_errorbar(aes(x = Month, ymin = lower.w, ymax = higher.w), size = 0.75, width = 0.3)+
  theme_minimal() +
  xlab('Date') +
  ylab('Average weight') +
  ggtitle('King penguins that fed while swimming') +
  scale_x_discrete(limits = c("Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar"))

w %>% filter(Species == "King penguin", Success == "Fail") %>%
  ggplot() +
  geom_point(aes(x = Month, y = mean.w), size = 2) +
  geom_errorbar(aes(x = Month, ymin = lower.w, ymax = higher.w), size = 0.75, width = 0.3)+
  theme_minimal() +
  xlab('Date') +
  ylab('Average weight') +
  ggtitle('King penguins that do not feed while swimming') +
  scale_x_discrete(limits = c("Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar"))

w %>% filter(Species == "Gentoo penguin") %>%
  ggplot() +
  geom_point(aes(x = Month, y = mean.w), size = 2) +
  geom_errorbar(aes(x = Month, ymin = lower.w, ymax = higher.w), size = 0.75, width = 0.3)+
  theme_minimal() +
  xlab('Date') +
  ylab('Average weight') +
  ggtitle('Gentoo penguins that do not feed while swimming')+
  scale_x_discrete(limits = c("Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar"))


w %>% filter(Species == "Northen Rockhopper penguin") %>%
  ggplot() +
  geom_point(aes(x = Month, y = mean.w), size = 2) +
  geom_line(aes(x = Month, y = mean.w, group = Species)) +
  theme_minimal() +
  xlab('Date') +
  ylab('Average weight') +
  ggtitle('Northern Rockhopper penguin that feeds while swimming') +
  scale_x_discrete(limits = c("Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar"))

####3.1 Linear regression ####
# Can't do linear regression because unequal sample sizes across species.
# e.g., no gentoo's participated, only a few king penguins did, and the northern rockhopper definitely did.
bcs.m0 = lmer(Weight~1 + (1|Name),
              data = bcs)

bcs.m1 = lmer(Weight~Success + (1|Name),
              data = bcs)

bcs.m2 = lmer(Weight~Month.no + (1|Name),
              data = bcs)

bcs.m3 = lmer(Weight~Species + (1|Name),
              data = bcs)

bcs.m4 = lmer(Weight~Species * Month.no + (1|Name),
              data = bcs)

bcs.m5 = lmer(Weight~Success * Species + (1|Name),
              data = bcs)
model.sel(bcs.m0, bcs.m1, bcs.m2, bcs.m3, bcs.m4, bcs.m5)
bcs.m3
summary(bcs.m3)

bcs.m0 = bcs %>% filter(Species %in% c("King penguin", "Northen Rockhopper penguin")) %>%
  lmer(Weight~1 + (1|Name),
              data = .)

bcs.m1 = bcs %>% filter(Species %in% c("King penguin", "Northen Rockhopper penguin")) %>%
  lmer(Weight~Success + (1|Name),
              data = .)

bcs.m2 = bcs %>% filter(Species %in% c("King penguin", "Northen Rockhopper penguin")) %>%
  lmer(Weight~Month.no + (1|Name),
              data = .)

bcs.m3 = bcs %>% filter(Species %in% c("King penguin", "Northen Rockhopper penguin")) %>%
  lmer(Weight~Month.no + Success + (1|Name),
       data = .)

model.sel(bcs.m0, bcs.m1, bcs.m2, bcs.m3)

bcs %>%
  filter(Species == "King penguin") %>%
  ggplot(data = ., aes(x = Date, y = Weight, color = Name)) +
  geom_line() +
  facet_grid(.~Success)

####4. Fish consumption rate ####
fish = read.csv("fish-consumption.csv")
fish$Date = fish[,1]
fish$Date = as.POSIXct(fish$Date, format = "%m/%d/%Y")
fish[,1] = NULL
fish[is.na(fish)] = 0
fish = melt(fish, variable.name = 'Focal.Name', value.name = "Consumed", id.vars = c("Date")) %>%
  dplyr::mutate(Species = ifelse(Focal.Name %in% c("BLUE", "YELLOW", "PURPLE"), "Gentoo",
                                 ifelse(Focal.Name %in% c("PIERRE"), "NRH", "King")),
                Success = ifelse(Focal.Name %in% c("PIERRE", "GLORIA", "HANSEL", "PATRICIA", "BRIX",
                                                  "AIDAN", "ARLO"), "Success", "Fail"),
                Week = week(Date),
                Month = month(Date, label = TRUE))

fish = fish %>%
  dplyr::mutate(Month = factor(Month, levels = c("Nov", "Dec", "Jan", "Feb", "Mar")))

fish.lm0 = fish %>%
  filter(Success == "Success") %>%
  lmer(Consumed~1 + (1|Focal.Name),
       data = .)

fish.lm1 = fish %>%
  filter(Success == "Success") %>%
  lmer(Consumed~ Week + (1|Focal.Name),
       data = .)

fish.bouts = fish %>%
  filter(Success == "Success") %>%
  group_by(Species, Month) %>%
  summarise(total.consumed = sum(Consumed),
            counts = sum(Consumed > 0, na.rm = TRUE),
            average.consumed = total.consumed/counts,
            total = n(),
            prop = counts/total*100)


fish.bouts %>%
ggplot(., aes(x = Month, y = prop,group = Species)) +
  geom_line(stat = "identity", size = 0.8, aes(linetype = Species)) +
  theme_minimal() +
  xlab('Month') +
  ylab('Occurrence (%)') +
  ggtitle("The relative presence of swim-feed bouts observed in King and Northern Rockhopper penguins.") +
  theme(legend.position = "bottom")

fish.consumed = fish %>%
  filter(Success == "Success") %>%
  group_by(Month, Species) %>%
  summarise(counts = mean(Consumed))

fish.consumed %>%
  ggplot(., aes(x = Month, y = counts,group = Species)) +
  geom_line(stat = "identity", size = 0.8, aes(linetype = Species)) +
  theme_minimal() +
  xlab('Month') +
  ylab('Average # of fish consumed') +
  ggtitle("The average number of capelins consumed by swimming King and Northern Rockhopper penguins.") +
  theme(legend.position = "bottom")


####5. Plumage score ####
plum = read.csv('plumage-score.csv')
plum$Date = as.POSIXct(plum$Date, format = "%m/%d/%Y")

plum = melt(plum, variable.name = "Focal.Name", value.name = "Score", id.vars = c("Date"))

plum = plum %>%
  group_by(Name) %>%
  mutate(Month = month(Date, label = TRUE),
         Species = ifelse(Name %in% c("Blue", "Yellow", "Purple"), "Gentoo",
                          ifelse(Name %in% c("Pierre"), "NRH", "King")),
         Success = ifelse(Name %in% c("Pierre", "Gloria", "Hansel", "Patricia", "Brix",
                                            "Aidan", "Arlo"), "Success", "Fail")) %>%
  filter(Score > 0)

plum.summary = plum %>%
  group_by(Success, Species, Month) %>%
  summarise(mean = mean(Score))

ggarrange(
plum.summary %>%
  filter(Success == "Success") %>%
  ggplot() +
  geom_point(aes(x = Month, y = mean, group = Species), size = 2) +
  geom_line(aes(x = Month, y = mean, group = Species)) +
  theme_minimal() +
  xlab('Date') +
  ylab('Average plumage score') +
  facet_wrap(.~Species, nrow = 1) +
  ggtitle('The plumage scores of penguins that do swim-feed') +
  scale_x_discrete(limits = c("Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar"))
,

plum.summary %>%
  filter(Success == "Fail") %>%
  ggplot() +
  geom_point(aes(x = Month, y = mean, group = Species), size = 2) +
  geom_line(aes(x = Month, y = mean, group = Species)) +
  theme_minimal() +
  xlab('Date') +
  ylab('Average plumage score') +
  facet_wrap(.~Species, nrow = 1) +
  ggtitle('The plumage scores of penguins that do not swim-feed') +
  scale_x_discrete(limits = c("Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar"))
, ncol = 1)


