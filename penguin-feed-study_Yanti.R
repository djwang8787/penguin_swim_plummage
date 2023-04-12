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
                Lying = Lying/Total*100,
                Treatment = "BL") %>%
    group_by(Species) %>%
    summarise(sleep = mean(Sleeping),
              preen = mean(Preening),
              stand = mean(Standing),
              lie = mean(Lying),
              sleep.se = plotrix::std.error(Sleeping, na.rm = T),
              preen.se = plotrix::std.error(Preening),
              stand.se = plotrix::std.error(Standing),
              lie.se = plotrix::std.error(Lying),
              treatment = first(Treatment))


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
  group_by(Species, Focal.Name) %>%
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
  group_by(Species, Focal.Name) %>%
  dplyr::summarise(Preening = sum(Behaviours == "Preening")) %>%
  ungroup() %>%
  add_row(Species = "King", Focal.Name = "Gretel", Preening = 0, .before = 8)

merged.data = loc.data
merged.data$Preening = as.numeric(behav.data[, 3])

merged.data = merged.data %>%
  mutate(Treatment = "treatment") %>%
  group_by(Species) %>%
  dplyr::summarise(sleep = mean(Sleeping),
                   preen = mean(Preening),
                   stand = mean(Standing),
                   lie = mean(Lying),
                   sleep.se = plotrix::std.error(Sleeping, na.rm = T),
                   preen.se = plotrix::std.error(Preening),
                   stand.se = plotrix::std.error(Standing),
                   lie.se = plotrix::std.error(Lying),
                   treatment = first(Treatment)) %>%
  rbind(bl.summary.prop)

merged.data.means = merged.data %>%
  select(Species, sleep, preen, stand, lie, treatment) %>%
  tidyr::pivot_longer(cols = (sleep:lie), names_to = "behaviour", values_to = "mean" )

merged.data.se = merged.data %>%
  select(Species, sleep.se, preen.se, stand.se, lie.se, treatment) %>%
  tidyr::pivot_longer(cols = (sleep.se:lie.se), names_to = "behaviour", values_to = "se" ) %>%
  tidyr::replace_na(list(se = 0))

merged.data.means$se = merged.data.se$se


# Trial data, occurrence % by counts of behaviours > 1% occurrence.
ggplot(merged.data.means, aes(x = behaviour, y = mean, fill = treatment)) +
  geom_bar(stat = "identity", position = position_dodge(), colour = "black") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,
                position=position_dodge(.9)) +
  theme_minimal() +
  facet_grid(.~Species) +
  xlab('Behaviours') +
  ylab('Counts') +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("red", "blue"))

####2.3 activity budget comparison####
names(bl)
names(behaviour.data) # take Species, Focal.Name, Behaviours, Month, DateTime,
bl2 = bl  %>%
  rename(Focal.Name = SUBJECT,
         Behaviours = Behaviour) %>%
  mutate(Year = lubridate::year(Date)) %>%
  select(Focal.Name, Behaviours, Species, Month, Date)

behav.d2 = behaviour.data %>%
  select(Focal.Name, Behaviours, Species, Month, DateTime)

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
                Month = month(Date, label = TRUE),
                Month.index = month(Date, label = FALSE),
                Month.no = ifelse(Month %in% c("Jan", "Feb", "Mar"), Month.index+5, Month.index - 7),
                Treatment = ifelse(Month %in% c("Aug", "Sep", "Oct"), "Baseline", "Treatment"))

bl.vs.t = bcs %>%
  group_by(Treatment, Species) %>%
  dplyr::summarise(mean.w = mean(Weight),
                   sd.w = plotrix::std.error(Weight),
                   lower.w = mean.w - sd.w,
                   higher.w = mean.w + sd.w,
                   mean.bcs = mean(BCS),
                   sd.bcs = plotrix::std.error(BCS),
                   lower.bcs = mean.bcs-sd.bcs,
                   higher.bcs = mean.bcs + sd.bcs) %>%
  replace(is.na(.), 0)

# bl vs treatment of all penguin weights
ggarrange(
  bl.vs.t %>%
  filter(Species == "King penguin") %>%
  ggplot() +
  geom_point(aes(x = Species, y = mean.w, colour = Treatment), size = 2, position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(x = Species, ymin = lower.w, ymax = higher.w, colour = Treatment),
                size = 0.75, width = 0.3, position = position_dodge(width = 0.9)) +
  theme_minimal() +
  xlab(' ') +
  ylab('Weight') +
  theme(legend.position = "none") +
  scale_colour_manual(values = c("red", "blue"))
,
bl.vs.t %>%
  filter(Species == "Gentoo penguin") %>%
  ggplot() +
  geom_point(aes(x = Species, y = mean.w, colour = Treatment), size = 2, position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(x = Species, ymin = lower.w, ymax = higher.w, colour = Treatment),
                size = 0.75, width = 0.3, position = position_dodge(width = 0.9)) +
  theme_minimal() +
  xlab(' ') +
  ylab(' ') +
  theme(legend.position = "none")+
  scale_colour_manual(values = c("red", "blue"))
,
bl.vs.t %>%
  filter(Species == "Northen Rockhopper penguin") %>%
  ggplot() +
  geom_point(aes(x = Species, y = mean.w, colour = Treatment), size = 2, position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(x = Species, ymin = lower.w, ymax = higher.w, colour = Treatment),
                size = 0.75, width = 0.3, position = position_dodge(width = 0.9)) +
  theme_minimal() +
  xlab(' ') +
  ylab(' ') +
  theme(legend.position = "none")+
  scale_colour_manual(values = c("red", "blue"))
, legend = "none", common.legend = T, ncol = 3)

# bl vs treat for only king penguins, further differentiated by those that swim and those that don't.
w = bcs %>%
  group_by(Treatment, Success, Species) %>%
  dplyr::summarise(mean.w = mean(Weight),
                sd.w = plotrix::std.error(Weight),
                lower.w = mean.w - sd.w,
                higher.w = mean.w + sd.w,
                mean.bcs = mean(BCS),
                sd.bcs = plotrix::std.error(BCS),
                lower.bcs = mean.bcs-sd.bcs,
                higher.bcs = mean.bcs + sd.bcs) %>%
  replace(is.na(.), 0)

w %>% filter(Species == "King penguin") %>%
ggplot() +
  geom_point(aes(x = Success, y = mean.w, colour = Success), size = 2, position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(x = Success, ymin = lower.w, ymax = higher.w, colour = Success),
                size = 0.75, width = 0.3, position = position_dodge(width = 0.9))+
  theme_minimal() +
  facet_grid(.~Treatment) +
  scale_colour_manual(values = c("red", "blue")) +
  theme(legend.position = "none") +
  ylab("Weight") +
  xlab(" ")

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
fish$Date = as.POSIXct(fish$Date, format = "%d/%m/%y")
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


# feed.bouts = fish %>%
#   filter(Success == "Success") %>%
#   group_by(Species, Month) %>%
#   summarise(total.consumed = sum(Consumed),
#             counts = sum(Consumed > 0, na.rm = TRUE),
#             average.consumed = total.consumed/counts,
#             total = n(),
#             prop = counts/total*100)

feed.bouts.king = fish %>%
  filter(Success == "Success", Species == "King") %>%
  group_by(Focal.Name, Month) %>%
  summarise(total.consumed = sum(Consumed),
            feed.bouts = sum(Consumed > 0, na.rm = TRUE),
            average.consumed = total.consumed/feed.bouts) %>%
  tidyr::replace_na(list(average.consumed = 0)) %>%
  group_by(Focal.Name) %>%
  mutate(cumsum(total.consumed))
feed.bouts.king


feed.bouts.king %>%
ggplot(., aes(x = Month, y = prop, group = Species)) +
  geom_line(stat = "identity", size = 0.8, aes(linetype = Species)) +
  theme_minimal() +
  xlab('Month') +
  ylab('Occurrence (%)') +
  ggtitle("The relative occurrence of swim-feed bouts observed in King and Northern Rockhopper penguins.") +
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
plum = plum %>%
  group_by(Name) %>%
  mutate(Month = month(Date, label = TRUE),
         Species = as.factor(ifelse(Name %in% c("Blue", "Yellow", "Purple"), "Gentoo",
                          ifelse(Name %in% c("Pierre"), "NRH", "King"))),
         Success = ifelse(Name %in% c("Pierre", "Gloria", "Hansel", "Patricia", "Brix",
                                            "Aidan", "Arlo"), "Success", "Fail"),
         Treatment = ifelse(Month %in% c("Aug", "Sep", "Oct"), "Baseline", "Treatment")) %>%
  filter(Score > 0)

plum.summary = plum %>%
  group_by(Treatment, Success, Species) %>%
  summarise(mean = mean(Score),
            se = plotrix::std.error(Score))

plum.summary %>%
  filter(Species == "King") %>%
ggplot() +
  geom_point(aes(x = Treatment, y = mean, colour = Success), size = 2, position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(x = Treatment, ymin = mean-se, ymax = mean+se, colour = Success),
                size = 0.75, width = 0.3, position = position_dodge(width = 0.9))+
  theme_minimal() +
  scale_colour_manual(values = c("red", "blue")) +
  theme(legend.position = "none") +
  ylab("Plumage score") +
  xlab(" ")

# month by month examination; requires re-mutation of the month variable
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


