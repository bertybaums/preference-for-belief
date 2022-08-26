library(tidyverse)

d = read.csv("IdeoEpiChamber6-IdeologyFilterUniformBias-table.csv", header = TRUE, skip = 6
)#this is the case of uniform bias
#Here it makes sense to compare ideology for different levels of motivated bias

#Data clearning of division by zero cases:
rem <- which(d$Yes.con == 999999999)
rem <- c(rem,which(d$Yes.vcon == 999999999))
rem <- c(rem,which(d$Yes.m == 999999999))
rem <- c(rem,which(d$Yes.lib == 999999999))
rem <- c(rem,which(d$Yes.vlib == 999999999))
d <- d[-rem,]

group_by(d, small.world., motivated.start.) %>%
summarise(echo = mean(average.bubble),
          count = n())

Yes_by_ideo <- d %>%
group_by(small.world., motivated.start., motivated.bias) %>%
  summarise(Yes.vl = mean(Yes.vlib),
            Yes.l = mean(Yes.lib),
            Yes.m = mean(Yes.m),
            Yes.c = mean(Yes.con),
            Yes.vc = mean(Yes.vcon), 
            count = n())

colors <- c("Yes.vl" = "purple3", "Yes.l" = "purple", "Yes.m" = "brown", "Yes.c" = "orange", "Yes.vc" = "orange3")
ggplot(data = Yes_by_ideo) +
  geom_smooth(mapping = aes(x = motivated.bias, y = Yes.vl, color = "Yes.vl")) +
  geom_smooth(mapping = aes(x = motivated.bias, y = Yes.l, color = "Yes.l")) +
  geom_smooth(mapping = aes(x = motivated.bias, y = Yes.m, color = "Yes.m")) +
  geom_smooth(mapping = aes(x = motivated.bias, y = Yes.c, color = "Yes.c")) +
  geom_smooth(mapping = aes(x = motivated.bias, y = Yes.vc, color = "Yes.vc")) +
  labs(x = "Motivated Bias",
       y = "Probability of Yes Given Ideology",
       color = "Ideology") +
  scale_color_manual(name = "Ideology",
                     breaks = c("Yes.vl","Yes.l", "Yes.m","Yes.c","Yes.vc"),
                     labels = c("Very Left","Left","Moderate","Right","Very Right"),
                                values=colors)+
  facet_grid(small.world. ~ motivated.start. , 
             labeller = labeller(
               small.world. = c(`false` = "Random Graph", `true` = "Small World"),
               motivated.start. = c(`false` = "Arbitrary Start", `true` = "Arranged Start")
             ) 
  ) 




d = read.csv("IdeoEpiChamber9StartTypesStartScenarios.csv", header = TRUE, skip = 6
)#this explores an exhaustive number of different start types, with no bias and one level of bias
#Data clearning of division by zero cases:
rem <- which(d$Yes.con == 999999999)
rem <- c(rem,which(d$Yes.vcon == 999999999))
rem <- c(rem,which(d$Yes.m == 999999999))
rem <- c(rem,which(d$Yes.lib == 999999999))
rem <- c(rem,which(d$Yes.vlib == 999999999))
d <- d[-rem,]
Yes_by_ideo <- d %>%
  group_by(StartType, max.motivated.bias) %>%
  summarise(Yes.vl = mean(Yes.vlib),
            Yes.l = mean(Yes.lib),
            Yes.m = mean(Yes.m),
            Yes.c = mean(Yes.con),
            Yes.vc = mean(Yes.vcon), 
            count = n())

#We're going to recode so that upper case letters mean yes, and lower case means no
Yes_by_ideo$StartType <- recode(Yes_by_ideo$StartType, Myynn = "MMmm", Oyynn = "OOoo", PynOyn = "PpOo", Pyynn = "PPpp", PyyOnn = "PPoo", PnnOyy = "ppOO",
                                Myyyn = "MMMm", Oyyyn = "OOOo", PnOyyy = "pOOO", PyOyyn = "POOo", PyyOyn = "PPOo", Pyyyn = "PPPp", PyyyOn = "PPPo",
                                Mynnn = "Mmmm", Oynnn = "Oooo", Pynnn = "Pppp")


#d.plot <- filter(Yes_by_ideo, max.motivated.bias == 0)
colors <- c("Yes.vl" = "purple3", "Yes.l" = "purple", "Yes.m" = "brown", "Yes.c" = "orange", "Yes.vc" = "orange3")
ggplot(data = Yes_by_ideo) +
  geom_jitter(mapping = aes(x = StartType, y = Yes.vl, color="Yes.vl"), width=0.05)  +
  geom_jitter(mapping = aes(x = StartType, y = Yes.l, color = "Yes.l"), width=0.05) +
  geom_jitter(mapping = aes(x = StartType, y = Yes.m, color = "Yes.m"), width=0.05) +
  geom_jitter(mapping = aes(x = StartType, y = Yes.c, color = "Yes.c"), width=0.05) +
  geom_jitter(mapping = aes(x = StartType, y = Yes.vc, color = "Yes.vc"), width=0.05) +
labs(x = "Start Type",
     y = "Probability of Yes Given Ideology",
     color = "Ideology") +
  scale_color_manual(name = "Ideology",
                     breaks = c("Yes.vl","Yes.l", "Yes.m","Yes.c","Yes.vc"),
                     labels = c("Very Left","Left","Moderate","Right","Very Right"),
                     values=colors)+
  facet_wrap(~ max.motivated.bias, nrow = 2,
             labeller = labeller(
               max.motivated.bias = c(`0` = "No Bias", `0.25` = "Bias")
             )
  ) 

#Let's look at just the different starts where there are two ys and two ns


Yes_by_ideo%>%
  #Note the old filter was filter(StartType == "Myynn" | StartType == "Oyynn" | StartType == "PynOyn" | StartType == "Pyynn" | StartType == "PyyOnn" | StartType == "PnnOyy") %>%
    filter(StartType == "MMmm" | StartType == "OOoo" | StartType == "PpOo" | StartType == "PPpp" | StartType == "PPoo" | StartType == "ppOO") %>%
ggplot() +
  geom_jitter(mapping = aes(x = StartType, y = Yes.vl, color="Yes.vl"), width=0.05)  +
  geom_jitter(mapping = aes(x = StartType, y = Yes.l, color = "Yes.l"), width=0.05) +
  geom_jitter(mapping = aes(x = StartType, y = Yes.m, color = "Yes.m"), width=0.05) +
  geom_jitter(mapping = aes(x = StartType, y = Yes.c, color = "Yes.c"), width=0.05) +
  geom_jitter(mapping = aes(x = StartType, y = Yes.vc, color = "Yes.vc"), width=0.05) +
  labs(x = "Start Type",
       y = "Probability of Yes Given Ideology",
       color = "Ideology") +
  scale_color_manual(name = "Ideology",
                     breaks = c("Yes.vl","Yes.l", "Yes.m","Yes.c","Yes.vc"),
                     labels = c("Very Left","Left","Moderate","Right","Very Right"),
                     values=colors)+
  facet_wrap(~ max.motivated.bias, nrow = 2,
             labeller = labeller(
               max.motivated.bias = c(`0` = "No Bias", `0.25` = "Bias")
             )
  ) 

#Now let's look at when there are 3 ys and 1 n
#(#The cases with 3 n's and 1 y is the same but with symmytric inversion on ideology)
Yes_by_ideo%>%
  #filter(StartType == "Myyyn" | StartType == "Oyyyn" | StartType == "PnOyyy" | StartType == "PyOyyn" | StartType == "PyyOyn" | StartType == "Pyyyn" | StartType == "PyyyOn") %>%
  filter(StartType == "MMMm" | StartType == "OOOo" | StartType == "pOOO" | StartType == "POOo" | StartType == "PPOo" | StartType == "PPPp" | StartType == "PPPo") %>%
ggplot() +
  geom_jitter(mapping = aes(x = StartType, y = Yes.vl, color="Yes.vl"), width=0.05)  +
  geom_jitter(mapping = aes(x = StartType, y = Yes.l, color = "Yes.l"), width=0.05) +
  geom_jitter(mapping = aes(x = StartType, y = Yes.m, color = "Yes.m"), width=0.05) +
  geom_jitter(mapping = aes(x = StartType, y = Yes.c, color = "Yes.c"), width=0.05) +
  geom_jitter(mapping = aes(x = StartType, y = Yes.vc, color = "Yes.vc"), width=0.05) +
  labs(x = "Start Type",
       y = "Probability of Yes Given Ideology",
       color = "Ideology") +
  scale_color_manual(name = "Ideology",
                     breaks = c("Yes.vl","Yes.l", "Yes.m","Yes.c","Yes.vc"),
                     labels = c("Very Left","Left","Moderate","Right","Very Right"),
                     values=colors)+
  facet_wrap(~ max.motivated.bias, nrow = 2,
             labeller = labeller(
               max.motivated.bias = c(`0` = "No Bias", `0.25` = "Bias")
             )
  ) 


#Now lets see for the case of PnnOyy (and PnOyyy) whether any level of max motivated bias would reverse the starting seed distribution
d1 = read.csv("IdeoEpiChamber9StartTypesPnnOyyMotivated.csv", header = TRUE, skip = 6
)# this is for PnnOyy
d2 = read.csv("IdeoEpiChamber9StartTypesPnOyyyMotivated.csv", header = TRUE, skip = 6
)# this is for PnOyyy
d <- full_join(d1,d2)
rem <- which(d$Yes.con == 999999999)
rem <- c(rem,which(d$Yes.vcon == 999999999))
rem <- c(rem,which(d$Yes.m == 999999999))
rem <- c(rem,which(d$Yes.lib == 999999999))
rem <- c(rem,which(d$Yes.vlib == 999999999))
d <- d[-rem,]
d$StartType <- recode(d$StartType, PnnOyy = "ppOO", PnOyyy = "pOOO")
Yes_by_ideo <- d %>%
  group_by(StartType, max.motivated.bias) %>%
  summarise(Yes.vl = mean(Yes.vlib),
            Yes.l = mean(Yes.lib),
            Yes.m = mean(Yes.m),
            Yes.c = mean(Yes.con),
            Yes.vc = mean(Yes.vcon), 
            count = n())%>%
  mutate(StartType = factor(StartType,levels=c("ppOO","pOOO")))
colors <- c("Yes.vl" = "purple3", "Yes.l" = "purple", "Yes.m" = "brown", "Yes.c" = "orange", "Yes.vc" = "orange3")
ggplot(data = Yes_by_ideo) +
  geom_smooth(mapping = aes(x = max.motivated.bias, y = Yes.vl, color = "Yes.vl")) +
  geom_smooth(mapping = aes(x = max.motivated.bias, y = Yes.l, color = "Yes.l")) +
  geom_smooth(mapping = aes(x = max.motivated.bias, y = Yes.m, color = "Yes.m")) +
  geom_smooth(mapping = aes(x = max.motivated.bias, y = Yes.c, color = "Yes.c")) +
  geom_smooth(mapping = aes(x = max.motivated.bias, y = Yes.vc, color = "Yes.vc")) +
  labs(#title = "PnnOyy Start",
       x = "Motivated Bias",
       y = "Probability of Yes Given Ideology",
       color = "Ideology") +
  scale_color_manual(name = "Ideology",
                     breaks = c("Yes.vl","Yes.l", "Yes.m","Yes.c","Yes.vc"),
                     labels = c("Very Left","Left","Moderate","Right","Very Right"),
                     values=colors)+
  facet_wrap(~ StartType)
  #facet_wrap(~ rev(StartType))


###
###
### AVOWALS
#Now we're going to consider the idea that people listen to avowals and then update their beliefs, which they then avow the next round


d = read.csv("IdeoEpiChamberAvowalStartScenarios-table.csv", header = TRUE, skip = 6
)#this explores an exhaustive number of different start types, with no bias and one level of bias
#Data clearning of division by zero cases:
rem <- which(d$Yes.con == 999999999)
rem <- c(rem,which(d$Yes.vcon == 999999999))
rem <- c(rem,which(d$Yes.m == 999999999))
rem <- c(rem,which(d$Yes.lib == 999999999))
rem <- c(rem,which(d$Yes.vlib == 999999999))
d <- d[-rem,]
Yes_by_ideo <- d %>%
  group_by(StartType, max.motivated.bias) %>%
  summarise(Yes.vl = mean(Yes.vlib),
            Yes.l = mean(Yes.lib),
            Yes.m = mean(Yes.m),
            Yes.c = mean(Yes.con),
            Yes.vc = mean(Yes.vcon), 
            count = n())

#We're gonig to recode so that upper case letters mean yes, and lower case means no
Yes_by_ideo$StartType <- recode(Yes_by_ideo$StartType, Myynn = "MMmm", Oyynn = "OOoo", PynOyn = "PpOo", Pyynn = "PPpp", PyyOnn = "PPoo", PnnOyy = "ppOO",
                                Myyyn = "MMMm", Oyyyn = "OOOo", PnOyyy = "pOOO", PyOyyn = "POOo", PyyOyn = "PPOo", Pyyyn = "PPPp", PyyyOn = "PPPo",
                                Mynnn = "Mmmm", Oynnn = "Oooo", Pynnn = "Pppp")

#Let's look at just the different starts where there are two ys and two ns

Yes_by_ideo%>%
  #Note the old filter was filter(StartType == "Myynn" | StartType == "Oyynn" | StartType == "PynOyn" | StartType == "Pyynn" | StartType == "PyyOnn" | StartType == "PnnOyy") %>%
  filter(StartType == "MMmm" | StartType == "OOoo" | StartType == "PpOo" | StartType == "PPpp" | StartType == "PPoo" | StartType == "ppOO") %>%
  ggplot() +
  geom_jitter(mapping = aes(x = StartType, y = Yes.vl, color="Yes.vl"), width=0.05)  +
  geom_jitter(mapping = aes(x = StartType, y = Yes.l, color = "Yes.l"), width=0.05) +
  geom_jitter(mapping = aes(x = StartType, y = Yes.m, color = "Yes.m"), width=0.05) +
  geom_jitter(mapping = aes(x = StartType, y = Yes.c, color = "Yes.c"), width=0.05) +
  geom_jitter(mapping = aes(x = StartType, y = Yes.vc, color = "Yes.vc"), width=0.05) +
  labs(x = "Start Type",
       y = "Probability of Yes Given Ideology",
       color = "Ideology") +
  scale_color_manual(name = "Ideology",
                     breaks = c("Yes.vl","Yes.l", "Yes.m","Yes.c","Yes.vc"),
                     labels = c("Very Left","Left","Moderate","Right","Very Right"),
                     values=colors)+
  facet_wrap(~ max.motivated.bias, nrow = 2,
             labeller = labeller(
               max.motivated.bias = c(`0` = "No Bias", `0.25` = "Bias")
             )
  ) 

#Now let's look at when there are 3 ys and 1 n
#(#The cases with 3 n's and 1 y is the same but with symmytric inversion on ideology)
Yes_by_ideo%>%
  #filter(StartType == "Myyyn" | StartType == "Oyyyn" | StartType == "PnOyyy" | StartType == "PyOyyn" | StartType == "PyyOyn" | StartType == "Pyyyn" | StartType == "PyyyOn") %>%
  filter(StartType == "MMMm" | StartType == "OOOo" | StartType == "pOOO" | StartType == "POOo" | StartType == "PPOo" | StartType == "PPPp" | StartType == "PPPo") %>%
  ggplot() +
  geom_jitter(mapping = aes(x = StartType, y = Yes.vl, color="Yes.vl"), width=0.05)  +
  geom_jitter(mapping = aes(x = StartType, y = Yes.l, color = "Yes.l"), width=0.05) +
  geom_jitter(mapping = aes(x = StartType, y = Yes.m, color = "Yes.m"), width=0.05) +
  geom_jitter(mapping = aes(x = StartType, y = Yes.c, color = "Yes.c"), width=0.05) +
  geom_jitter(mapping = aes(x = StartType, y = Yes.vc, color = "Yes.vc"), width=0.05) +
  labs(x = "Start Type",
       y = "Probability of Yes Given Ideology",
       color = "Ideology") +
  scale_color_manual(name = "Ideology",
                     breaks = c("Yes.vl","Yes.l", "Yes.m","Yes.c","Yes.vc"),
                     labels = c("Very Left","Left","Moderate","Right","Very Right"),
                     values=colors)+
  facet_wrap(~ max.motivated.bias, nrow = 2,
             labeller = labeller(
               max.motivated.bias = c(`0` = "No Bias", `0.25` = "Bias")
             )
  ) 



###
### Again, now in the context of the avowal model
#Now lets see for the case of PnnOyy (and PnOyyy) whether any level of max motivated bias would reverse the starting seed distribution
d1 = read.csv("IdeoEpiChamberAvowalIncongruentPnnOyy-table.csv", header = TRUE, skip = 6
)# this is for PnnOyy
d2 = read.csv("IdeoEpiChamberAvowalIncongruentPnOyyy-table.csv", header = TRUE, skip = 6
)# this is for PnOyyy
d <- full_join(d1,d2)
rem <- which(d$Yes.con == 999999999)
rem <- c(rem,which(d$Yes.vcon == 999999999))
rem <- c(rem,which(d$Yes.m == 999999999))
rem <- c(rem,which(d$Yes.lib == 999999999))
rem <- c(rem,which(d$Yes.vlib == 999999999))
d <- d[-rem,]
d$StartType <- recode(d$StartType, PnOyyy = "pOOO", PnnOyy = "ppOO")
Yes_by_ideo <- d %>%
  group_by(StartType, max.motivated.bias) %>%
  summarise(Yes.vl = mean(Yes.vlib),
            Yes.l = mean(Yes.lib),
            Yes.m = mean(Yes.m),
            Yes.c = mean(Yes.con),
            Yes.vc = mean(Yes.vcon), 
            count = n())%>%
  mutate(StartType = factor(StartType,levels=c("ppOO","pOOO")))
colors <- c("Yes.vl" = "purple3", "Yes.l" = "purple", "Yes.m" = "brown", "Yes.c" = "orange", "Yes.vc" = "orange3")
ggplot(data = Yes_by_ideo) +
  geom_smooth(mapping = aes(x = max.motivated.bias, y = Yes.vl, color = "Yes.vl")) +
  geom_smooth(mapping = aes(x = max.motivated.bias, y = Yes.l, color = "Yes.l")) +
  geom_smooth(mapping = aes(x = max.motivated.bias, y = Yes.m, color = "Yes.m")) +
  geom_smooth(mapping = aes(x = max.motivated.bias, y = Yes.c, color = "Yes.c")) +
  geom_smooth(mapping = aes(x = max.motivated.bias, y = Yes.vc, color = "Yes.vc")) +
  labs(#title = "PnnOyy Start",
    x = "Motivated Bias",
    y = "Probability of Yes Given Ideology",
    color = "Ideology") +
  scale_color_manual(name = "Ideology",
                     breaks = c("Yes.vl","Yes.l", "Yes.m","Yes.c","Yes.vc"),
                     labels = c("Very Left","Left","Moderate","Right","Very Right"),
                     values=colors)+
  facet_wrap( ~ StartType
            )










