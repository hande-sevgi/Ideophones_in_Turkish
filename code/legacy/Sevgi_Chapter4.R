##### Chapter 4 #####

# Description: This study investigates how speakers interpret pragmatic alternatives 
# introduced by different types of adverbials, with a focus on Turkish. Across three 
# acceptability judgment experiments, we compare manner adverbials with temporal and 
# locative adverbials, and also explore differences within manner adverbials 
# (lexical vs. ideophonic). Our findings show that manner adverbials form a distinct 
# class in how they contribute discourse alternatives, helping to explain their distribution 
# under negation. These results offer a more nuanced view of event modification and contribute 
# to cross-linguistic and multimodal research on how meaning is shaped by different kinds of 
# event modifiers.

# Load Required Libraries
install.packages("dplyr")
install.packages("magrittr")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("tidyverse")
install.packages("lme4")
install.packages("emmeans")
install.packages("effects")
install.packages("lmerTest")
install.packages("ordinal")
install.packages("MASS")

required_packages <- c("dplyr", "magrittr", "ggplot2", "tidyr", "MASS", "tidyverse", "ordinal", "lme4", "emmeans", "effects", "lmerTest")
invisible(lapply(required_packages, library, character.only = TRUE))

# Set seed for reproducibility
set.seed(123)

####  Experiment I: Adverbials under the scope of negation ####
### General Look ###

# Load Data
data_Cont <- read_excel("Experiment_I.xlsx") # You can find the file under C >> Contrastive Contexts >> Experiment I

# Data Preparation
data_Cont <- data_Cont %>%
  mutate(
    Polarity = ifelse(grepl("Aff", Trial), "Negation in the 2nd clause", "Negation in the 1st clause"),
    Scenario = case_when(
      grepl("1..", Trial) ~ "1",
      grepl("2..", Trial) ~ "2",
      grepl("3..", Trial) ~ "3",
      grepl("4..", Trial) ~ "4",
      grepl("5..", Trial) ~ "5",
      TRUE ~ "6"
    ),
    Type =case_when(
      grepl("CT", Trial) ~ "CatchTrial",
      grepl("F", Trial) ~ "Filler",
      TRUE ~ "Experimental"),
    Condition =case_when(
      grepl("Bad", Trial) ~ "Infelicitious",
      grepl("Good", Trial) ~ "Felicitious",
      grepl("InDirOb", Trial) ~ "Indirect Object",
      grepl("DirObj", Trial) ~ "Direct Object",
      grepl("Loc", Trial) ~ "Mismatch (Locative)",
      TRUE ~ "Match"
    ),
    AdverbType=case_when(
      grepl("Time_", Trial) ~ "Time Adverb",
      grepl("Manner_", Trial) ~ "Manner Adverb",
      grepl("Red_", Trial) ~ "Reduplication",
      TRUE ~ "Nonexperimental"
    ),
    Rating  =  as.factor(Rating),
    Response_ID = as.factor(Response_ID))

View(data_Cont)


data_Cont <- data_Cont %>% mutate(
  Scenario = factor(Scenario, levels = c("1","2", "3","4", "5", "6")),
  Polarity = factor(Polarity, levels = c("Negation in the 1st clause", "Negation in the 2nd clause")),
  Type = factor(Type, levels = c("Filler", "CatchTrial", "Experimental")),
  Condition = factor(Condition, levels = c("Infelicitious", "Felicitious","Match", "Mismatch (Locative)", "Indirect Object", "Direct Object")),
  AdverbType = factor(AdverbType, levels = c("Nonexperimental","Time Adverb", "Manner Adverb", "Reduplication")),
  Rating = factor(Rating, levels = c("5", "4", "3", "2", "1"))
)

# Check Data Structure
str(data_Cont)

# Visualization: Count data
ggplot(data_Cont, aes(x = Rating, color = Type, fill = Type)) +
  geom_bar(stat = "count", position = "dodge") +
  facet_wrap(~Polarity) +
  scale_color_brewer(palette = "Set1") + 
  scale_fill_brewer(palette = "Set1") +
  labs(
    subtitle = "Comparison of Ratings Across Polarity and Event Types",
    x = "Polarity", y = "Rating"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.position = "top",
    legend.title = element_text(face = "bold")
  )

# Visualization: Proportion
ggplot(data_Cont, aes(x = Rating, fill = Condition)) +
  geom_bar(position = "fill") +
  facet_wrap(~Polarity) +
  scale_fill_brewer(palette = "Set1") +
  labs(
    subtitle = "Proportion of Ratings Across Polarity and Event Types",
    x = "Extra", 
    y = "Proportion",
    fill = "Rating"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal(base_size = 14) +
  theme(
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.position = "top",
    legend.title = element_text(face = "bold")
  )

# Visualization: Proportions in a more comprehensive way
# The acceptability ratings
ggplot(data_Cont, aes(x = Condition, fill = Rating)) +
  geom_bar(position = "fill", color = "black") +
  scale_y_continuous() +
  scale_fill_manual(values = c("#008080", "#70A494", "#EDC9AF", "#FF9B71","#E63946"))+
  labs(
    subtitle = "Proportion of Ratings",
    x = "",
    y = "Proportion",
    fill = "Rating"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.position = "top",
    legend.title = element_text(face = "bold"))

####  Analysis I: Adverbials under the scope of negation ####
### Manner vs Time in Experiment I ###

data_Cont_MT <- data_Cont %>%
  filter(
    Type == "Experimental",
    AdverbType %in% c("Time Adverb", "Manner Adverb")
  ) %>% droplevels()

View(data_Cont_MT)
str(data_Cont_MT)

data_Cont_MT <- data_Cont_MT %>% mutate(
  Rating = factor(Rating, levels = c("5", "4", "3", "2", "1"), ordered = TRUE),
  Polarity = factor(Polarity, levels = c("Negation in the 2nd clause", "Negation in the 1st clause")),
  Condition = factor(Condition, levels = c("Match", "Mismatch (Locative)")),
  AdverbType = factor(AdverbType, levels = c("Time Adverb", "Manner Adverb"))
)

# Ratings for Manner and Time Adverbs
ggplot(data_Cont_MT, aes(x = AdverbType, fill = Rating)) +
  geom_bar(position = "fill", color = "black") +
  scale_y_continuous() +
  scale_fill_manual(values = c("#008080", "#70A494", "#EDC9AF", "#FF9B71", "#E63946"))+
  labs(
    subtitle = "Proportion of Ratings",
    x = "Polarity",
    y = "Proportion",
    fill = "Rating"
  ) +
  facet_wrap(~ Polarity + Condition, ncol = 2) +
  theme_minimal(base_size = 14) +
  theme(
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.position = "top",
    legend.title = element_text(face = "bold"))


# Descriptive data overview
data_avg_MT <- data_Cont_MT %>%
  group_by(Condition, Polarity, AdverbType) %>%
  summarise(
    Mean = mean(as.numeric(as.character(Rating)), na.rm = TRUE),
    SE = sd(as.numeric(as.character(Rating)), na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

Mean_plot_MT <- ggplot(data_avg_MT, aes(x = Condition, y = Mean, group = AdverbType, shape = AdverbType, fill = AdverbType)) +
  geom_line(aes(linetype = AdverbType), size = 0.6, color = "black") +  # fixed color for lines
  geom_line(aes(linetype = AdverbType), size = 0.6, color = "black") +  # fixed color for lines
  geom_point(size = 3, color = "black") +  # black outline, fill from AdverbType
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), width = 0.15) +
  facet_wrap(~ Polarity) +
  scale_shape_manual(values = c(21, 24, 22)) +  # filled shapes
  theme_minimal(base_size = 16) +
  theme(legend.position = "bottom") +
  coord_cartesian(ylim = c(2.25, 4.75))+
  xlab("Continuation Type") +
  ylab("Mean Ratings") +
  scale_fill_manual(values = c("Time Adverb" = "salmon", "Manner Adverb" = "darkturquoise"))

Mean_plot_MT

# Modelling
data_Cont_MT$Rating <- factor(data_Cont_MT$Rating, levels = rev(levels(factor(data_Cont_MT$Rating))))

options(contrasts = c("contr.sum", "contr.poly"))

data_Cont_MT_clmm <- clmm(Rating ~ Condition * AdverbType * Polarity + (1 | Response_ID) + (1 | Scenario), 
                          data = data_Cont_MT, 
                          link = "logit")

# Results of clmm analysis 
print(summary(data_Cont_MT_clmm))

# Visualization of the model results
plot(allEffects(data_Cont_MT_clmm), style = "stacked",
     main = "Ratings - Polarity and Condition",
     key.args = list(space="right"),
     xlab = "Continuation Type" ,
     lattice=list(strip=list(factor.names=FALSE,values=TRUE,cex=1)),
     colors=rev(hcl.colors(5, palette="TealRose")))


# As expected, Condition had a significant effect on the acceptability. However, there is 
# no fixed effect of AdverbType and Position of Negation. However, two two-way interactions
# were significant. (i) ConditionXAdverb Type and (ii)ConditionXPosition of Negation. 
# (i) The Match–Mismatch contrast is larger for Time adverb. (ii) The mismatch penalty was 
# greater when negation occurred in the first clause than in the second clause. However, no
# three-way interactions occurred.

####  Analysis II: Adverbials under the scope of negation ####
### Manner vs Time vs Ideophones (Reduplication) in Experiment I ###

data_Cont_RMT <- data_Cont %>%
  filter(
    Type == "Experimental",
    AdverbType %in% c("Time Adverb", "Manner Adverb", "Reduplication")
  ) %>% droplevels()

View(data_Cont_RMT)
str(data_Cont_RMT)

data_Cont_RMT <- data_Cont_RMT %>% mutate(
  Rating = as.factor(Rating),
  Polarity = factor(Polarity, levels = c("Negation in the 2nd clause", "Negation in the 1st clause")),
  Condition = factor(Condition, levels = c("Match", "Mismatch (Locative)")),
  AdverbType = factor(AdverbType, levels = c("Time Adverb","Manner Adverb", "Reduplication"))
)

# Ratings for Manner and Time Adverbs
ggplot(data_Cont_RMT, aes(x = AdverbType, fill = Rating)) +
  geom_bar(position = "fill", color = "black") +
  scale_y_continuous() +
  scale_fill_manual(values = c("#008080", "#70A494", "#EDC9AF", "#FF9B71", "#E63946"))+
  labs(
    subtitle = "Proportion of Ratings",
    x = "Polarity",
    y = "Proportion",
    fill = "Rating"
  ) +
  facet_wrap(~ Polarity + Condition, ncol = 2) +
  theme_minimal(base_size = 14) +
  theme(
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.position = "top",
    legend.title = element_text(face = "bold"))

# Descriptive visualization of the data
data_avg_RMT <- data_Cont_RMT %>%
  group_by(Condition, Polarity, AdverbType) %>%
  summarise(
    Mean = mean(as.numeric(as.character(Rating)), na.rm = TRUE),
    SE = sd(as.numeric(as.character(Rating)), na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

Mean_plot_RMT <- ggplot(data_avg_RMT, aes(x = Condition, y = Mean, group = AdverbType, shape = AdverbType, fill = AdverbType)) +
  geom_line(aes(linetype = AdverbType), size = 0.6, color = "black") +  # fixed color for lines
  geom_point(size = 3, color = "black") +  # black outline, fill from AdverbType
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), width = 0.15) +
  facet_wrap(~ Polarity) +
  scale_shape_manual(values = c(21, 24, 22)) +  # filled shapes
  theme_minimal(base_size = 16) +
  theme(legend.position = "bottom") +
  coord_cartesian(ylim = c(2.25, 4.75))+
  xlab("Continuation Type") +
  ylab("Mean Ratings") +
  scale_fill_manual(values = c("Time Adverb" = "salmon", "Manner Adverb" = "darkturquoise", "Reduplication" = "darkgreen"))

Mean_plot_RMT


# Modelling
data_Cont_RMT$Rating <- factor(data_Cont_RMT$Rating, levels = rev(levels(factor(data_Cont_RMT$Rating))))

options(contrasts = c("contr.sum", "contr.poly"))

data_Cont_RMT_clmm <- clmm(Rating ~ Condition * AdverbType * Polarity + (1 | Response_ID) + (1 | Scenario), 
                           data = data_Cont_RMT, 
                           link = "logit")

# Results of clmm analysis 
print(summary(data_Cont_RMT_clmm))


# Visualization of the model results
plot(allEffects(data_Cont_RMT_clmm), style = "stacked",
     main = "Ratings - Polarity and Condition",
     key.args = list(space="right"),
     lattice=list(strip=list(factor.names=FALSE,values=TRUE,cex=1)),
     colors=rev(hcl.colors(5, palette="TealRose")))

# Pairwise comparisons to investigate each Adverb Type
pairs(emmeans(data_Cont_RMT_clmm, ~ AdverbType | Condition ), adjust = "none")

#### Additional toy analyses for sanity check ####
# Does Reduplication show similar behavior to Time? #
data_Cont_Toy1 <- data_Cont %>%
  filter(
    Type == "Experimental",
    AdverbType %in% c("Time Adverb", "Reduplication")
  ) %>% droplevels()

data_Cont_Toy1 <- data_Cont_Toy1 %>% mutate(
  Rating = as.factor(Rating),
  Polarity = factor(Polarity, levels = c("Negation in the 1st clause", "Negation in the 2nd clause")),
  Condition = factor(Condition, levels = c("Match", "Mismatch (Locative)")),
  AdverbType = factor(AdverbType, levels = c("Time Adverb", "Reduplication"))
)

# Ratings for Manner and Time Adverbs
ggplot(data_Cont_Toy1, aes(x = AdverbType, fill = Rating)) +
  geom_bar(position = "fill", color = "black") +
  scale_y_continuous() +
  scale_fill_manual(values = c("#008080", "#70A494", "#EDC9AF", "#FF9B71", "#E63946"))+
  labs(
    subtitle = "Proportion of Ratings",
    x = "Polarity",
    y = "Proportion",
    fill = "Rating"
  ) +
  facet_wrap(~ Polarity + Condition, ncol = 2) +
  theme_minimal(base_size = 14) +
  theme(
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.position = "top",
    legend.title = element_text(face = "bold"))


# Modelling
data_Cont_Toy1$Rating <- factor(data_Cont_Toy1$Rating, levels = rev(levels(factor(data_Cont_Toy1$Rating))))

options(contrasts = c("contr.sum", "contr.poly"))


data_Cont_Toy1_clmm <- clmm(Rating ~ Condition * AdverbType * Polarity + (1 | Response_ID) + (1 | Scenario), 
                           data = data_Cont_Toy1, 
                           link = "logit")

# Results of clmm analysis 
print(summary(data_Cont_Toy1_clmm))

# Visualization of the model results
plot(allEffects(data_Cont_Toy1_clmm), style = "stacked",
     main = "Ratings - Polarity and Condition",
     key.args = list(space="right"),
     lattice=list(strip=list(factor.names=FALSE,values=TRUE,cex=1)),
     colors=rev(hcl.colors(5, palette="TealRose")))

# Does Reduplication show similar behavior to Manner? #
data_Cont_Toy2 <- data_Cont %>%
  filter(
    Type == "Experimental",
    AdverbType %in% c("Manner Adverb", "Reduplication")
  ) %>% droplevels()

data_Cont_Toy2 <- data_Cont_Toy2 %>% mutate(
  Rating = as.factor(Rating),
  Polarity = factor(Polarity, levels = c("Negation in the 1st clause", "Negation in the 2nd clause")),
  Condition = factor(Condition, levels = c("Match", "Mismatch (Locative)")),
  AdverbType = factor(AdverbType, levels = c("Manner Adverb", "Reduplication"))
)

# Ratings for Manner and Time Adverbs
ggplot(data_Cont_Toy2, aes(x = AdverbType, fill = Rating)) +
  geom_bar(position = "fill", color = "black") +
  scale_y_continuous() +
  scale_fill_manual(values = c("#008080", "#70A494", "#EDC9AF", "#FF9B71", "#E63946"))+
  labs(
    subtitle = "Proportion of Ratings",
    x = "Polarity",
    y = "Proportion",
    fill = "Rating"
  ) +
  facet_wrap(~ Polarity + Condition, ncol = 2) +
  theme_minimal(base_size = 14) +
  theme(
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.position = "top",
    legend.title = element_text(face = "bold"))


# Modelling
data_Cont_Toy2$Rating <- factor(data_Cont_Toy2$Rating, levels = rev(levels(factor(data_Cont_Toy2$Rating))))

options(contrasts = c("contr.sum", "contr.poly"))

data_Cont_Toy2_clmm <- clmm(Rating ~ Condition * AdverbType * Polarity + (1 | Response_ID) + (1 | Scenario), 
                            data = data_Cont_Toy2, 
                            link = "logit")

# Results of clmm analysis 
print(summary(data_Cont_Toy2_clmm))

# Visualization of the model results
plot(allEffects(data_Cont_Toy2_clmm), style = "stacked",
     main = "Ratings - Polarity and Condition",
     key.args = list(space="right"),
     lattice=list(strip=list(factor.names=FALSE,values=TRUE,cex=1)),
     colors=rev(hcl.colors(5, palette="TealRose")))

#####  Experiment II: Information structure and adverbials #####
### General Look ###

# Load Data
data_Cont_NC <- read_excel("Experiment_II.xlsx") # You can find the file in Datasets folder

# Data Preparation
data_Cont_NC <- data_Cont_NC %>%
  mutate(
    Polarity = ifelse(grepl("Aff", Trial), "Negation in the 2nd clause", "Negation in the 1st clause"),
    Scenario = case_when(
      grepl("1..", Trial) ~ "1",
      grepl("2..", Trial) ~ "2",
      grepl("3..", Trial) ~ "3",
      grepl("4..", Trial) ~ "4",
      grepl("5..", Trial) ~ "5",
      TRUE ~ "6"
    ),
    Type =case_when(
      grepl("CT", Trial) ~ "CatchTrial",
      grepl("F", Trial) ~ "Filler",
      TRUE ~ "Experimental"),
    Condition =case_when(
      grepl("Bad", Trial) ~ "Infelicitious",
      grepl("Good", Trial) ~ "Felicitious",
      grepl("InDirOb", Trial) ~ "Indirect Object",
      grepl("DirObj", Trial) ~ "Direct Object",
      grepl("Loc", Trial) ~ "Mismatch (Locative)",
      TRUE ~ "Match"
    ),
    AdverbType=case_when(
      grepl("Time_", Trial) ~ "Time Adverb",
      grepl("Manner_", Trial) ~ "Manner Adverb",
      grepl("Red_", Trial) ~ "Reduplication",
      TRUE ~ "Nonexperimental"
    ),
    Rating  = as.factor(Rating),
    Response_ID = as.factor(Response_ID))

View(data_Cont_NC)
str(data_Cont_NC)

data_Cont_NC <- data_Cont_NC %>% mutate(
  Scenario = factor(Scenario, levels = c("1","2", "3","4", "5", "6")),
  Polarity = factor(Polarity, levels = c("Negation in the 1st clause", "Negation in the 2nd clause")),
  Type = factor(Type, levels = c("Filler", "CatchTrial", "Experimental")),
  Condition = factor(Condition, levels = c("Infelicitious", "Felicitious","Match", "Mismatch (Locative)", "Indirect Object", "Direct Object")),
  AdverbType = factor(AdverbType, levels = c("Nonexperimental","Time Adverb", "Manner Adverb", "Reduplication")),
  Rating = factor(Rating, levels = c("5", "4", "3", "2", "1"))
)

# Check Data Structure
str(data_Cont_NC)
View(data_Cont_NC)

# Visualization: Count data
ggplot(data_Cont_NC, aes(x = Rating, color = Type, fill = Type)) +
  geom_bar(stat = "count", position = "dodge") +
  facet_wrap(~Polarity) +
  scale_color_brewer(palette = "Set1") + 
  scale_fill_brewer(palette = "Set1") +
  labs(
    subtitle = "Comparison of Ratings Across Polarity and Event Types",
    x = "Polarity", y = "Rating"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.position = "top",
    legend.title = element_text(face = "bold")
  )

# Visualization: Proportion
ggplot(data_Cont_NC, aes(x = Rating, fill = Condition)) +
  geom_bar(position = "fill") +
  facet_wrap(~Polarity) +
  scale_fill_brewer(palette = "Set1") +
  labs(
    subtitle = "Proportion of Ratings Across Polarity and Event Types",
    x = "Extra", 
    y = "Proportion",
    fill = "Rating"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal(base_size = 14) +
  theme(
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.position = "top",
    legend.title = element_text(face = "bold")
  )

# Visualization: Proportions once again
ggplot(data_Cont_NC, aes(x = Condition, fill = Rating)) +
  geom_bar(position = "fill", color = "black") +
  scale_y_continuous() +
  scale_fill_manual(values = c("#008080", "#70A494", "#EDC9AF", "#FF9B71","#E63946"))+
  labs(
    subtitle = "Proportion of Ratings",
    x = "",
    y = "Proportion",
    fill = "Rating"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.position = "top",
    legend.title = element_text(face = "bold"))

####  Analysis I: Adverbials under the scope of negation ####
## Manner vs Time in Experiment II ##

data_Cont_NC_MT <- data_Cont_NC %>%
  filter(
    Type == "Experimental",
    AdverbType %in% c("Time Adverb", "Manner Adverb")
  ) %>% droplevels()

View(data_Cont_NC_MT)

levels(data_Cont_NC_MT$Response_ID)
str(data_Cont_NC_MT)

data_Cont_NC_MT <- data_Cont_NC_MT %>% mutate(
  Rating = as.factor(Rating),
  Polarity = factor(Polarity, levels = c("Negation in the 2nd clause", "Negation in the 1st clause")),
  Condition = factor(Condition, levels = c("Match", "Mismatch (Locative)")),
  AdverbType = factor(AdverbType, levels = c("Time Adverb", "Manner Adverb"))
)

# Ratings for Manner and Time Adverbs
ggplot(data_Cont_NC_MT, aes(x = AdverbType, fill = Rating)) +
  geom_bar(position = "fill", color = "black") +
  scale_y_continuous() +
  scale_fill_manual(values = c("#008080", "#70A494", "#EDC9AF", "#FF9B71", "#E63946"))+
  labs(
    subtitle = "Proportion of Ratings",
    x = "Polarity",
    y = "Proportion",
    fill = "Rating"
  ) +
  facet_wrap(~ Polarity + Condition, ncol = 2) +
  theme_minimal(base_size = 14) +
  theme(
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.position = "top",
    legend.title = element_text(face = "bold"))

summary(data_Cont_NC_MT$Trial)

# Descriptive visualization of the data
data_avg_NC_MT <- data_Cont_NC_MT %>%
  group_by(Condition, Polarity, AdverbType) %>%
  summarise(
    Mean = mean(as.numeric(as.character(Rating)), na.rm = TRUE),
    SE = sd(as.numeric(as.character(Rating)), na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

Mean_plot_NC_MT <- ggplot(data_avg_NC_MT, aes(x = Condition, y = Mean, group = AdverbType, shape = AdverbType, fill = AdverbType)) +
  geom_line(aes(linetype = AdverbType), size = 0.6, color = "black") +  # fixed color for lines
  geom_point(size = 3, color = "black") +  # black outline, fill from AdverbType
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), width = 0.15) +
  facet_wrap(~ Polarity) +
  scale_shape_manual(values = c(21, 24, 22)) +  # filled shapes
  theme_minimal(base_size = 16) +
  theme(legend.position = "bottom") +
  coord_cartesian(ylim = c(2.25, 4.75))+
  xlab("Continuation Type") +
  ylab("Mean Ratings") +
  scale_fill_manual(values = c("Time Adverb" = "salmon", "Manner Adverb" = "darkturquoise"))

Mean_plot_NC_MT

# Modelling
data_Cont_NC_MT$Rating <- factor(data_Cont_NC_MT$Rating, levels = rev(levels(factor(data_Cont_NC_MT$Rating))))

options(contrasts = c("contr.sum", "contr.poly"))
contrasts(data_Cont_NC_MT$AdverbType)
data_Cont_NC_MT_clmm <- clmm(Rating ~ Condition * AdverbType * Polarity + (1 | Response_ID) + (1 | Scenario), 
                             data = data_Cont_NC_MT, 
                             link = "logit")

# Results of clmm analysis 
print(summary(data_Cont_NC_MT_clmm))

# Visualization of the model results
plot(allEffects(data_Cont_NC_MT_clmm), style = "stacked",
     main = "Ratings - Polarity and Condition",
     key.args = list(space="right"),
     xlab = "Continuation Type",
     lattice=list(strip=list(factor.names=FALSE,values=TRUE,cex=1)),
     colors=rev(hcl.colors(5, palette="TealRose")))

####  Analysis II: Adverbials under the scope of negation ####
### Manner vs Time vs Ideophones (Reduplication) in Experiment II ###

data_Cont_NC_RMT <- data_Cont_NC %>%
  filter(
    Type == "Experimental",
    AdverbType %in% c("Time Adverb", "Manner Adverb", "Reduplication")
  ) %>% droplevels()

View(data_Cont_NC_RMT)

levels(data_Cont_NC_RMT$Response_ID)
str(data_Cont_NC_RMT)

data_Cont_NC_RMT <- data_Cont_NC_RMT %>% mutate(
  Rating = as.factor(Rating),
  Polarity = factor(Polarity, levels = c("Negation in the 2nd clause","Negation in the 1st clause")),
  Condition = factor(Condition, levels = c("Match", "Mismatch (Locative)")),
  AdverbType = factor(AdverbType, levels = c("Time Adverb", "Manner Adverb", "Reduplication"))
)

# Ratings for AdverbTypes
ggplot(data_Cont_NC_RMT, aes(x = AdverbType, fill = Rating)) +
  geom_bar(position = "fill", color = "black") +
  scale_y_continuous() +
  scale_fill_manual(values = c("#008080", "#70A494", "#EDC9AF", "#FF9B71", "#E63946"))+
  labs(
    subtitle = "Proportion of Ratings",
    x = "Polarity",
    y = "Proportion",
    fill = "Rating"
  ) +
  facet_wrap(~ Polarity + Condition, ncol = 2) +
  theme_minimal(base_size = 14) +
  theme(
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.position = "top",
    legend.title = element_text(face = "bold"))


# Modelling
options(contrasts = c("contr.sum", "contr.poly"))

data_Cont_NC_RMT$Rating <- factor(data_Cont_NC_RMT$Rating, levels = rev(levels(factor(data_Cont_NC_RMT$Rating))))

data_Cont_NC_RMT_clmm <- clmm(Rating ~ Condition * AdverbType * Polarity + (1 | Response_ID) + (1 | Scenario), 
                              data = data_Cont_NC_RMT, 
                              link = "logit")

# Results of clmm analysis 
print(summary(data_Cont_NC_RMT_clmm))

# Visualization of the model results
plot(allEffects(data_Cont_NC_RMT_clmm), style = "stacked",
     main = "Ratings - Polarity and Condition",
     key.args = list(space="right"),
     lattice=list(strip=list(factor.names=FALSE,values=TRUE,cex=1)),
     colors=rev(hcl.colors(5, palette="TealRose")))

pairs(emmeans(data_Cont_NC_RMT_clmm, ~ AdverbType), adjust = "none")

#### Additional toy analyses for sanity check ####
# Does Reduplication show similar behavior to Time? #
data_Cont_Toy3 <- data_Cont_NC %>%
  filter(
    Type == "Experimental",
    AdverbType %in% c("Time Adverb", "Reduplication")
  ) %>% droplevels()

data_Cont_Toy3 <- data_Cont_Toy3 %>% mutate(
  Rating = as.factor(Rating),
  Polarity = factor(Polarity, levels = c("Negation in the 1st clause", "Negation in the 2nd clause")),
  Condition = factor(Condition, levels = c("Match", "Mismatch (Locative)")),
  AdverbType = factor(AdverbType, levels = c("Time Adverb", "Reduplication"))
)

# Ratings for Manner and Time Adverbs
ggplot(data_Cont_Toy3, aes(x = AdverbType, fill = Rating)) +
  geom_bar(position = "fill", color = "black") +
  scale_y_continuous() +
  scale_fill_manual(values = c("#008080", "#70A494", "#EDC9AF", "#FF9B71", "#E63946"))+
  labs(
    subtitle = "Proportion of Ratings",
    x = "Polarity",
    y = "Proportion",
    fill = "Rating"
  ) +
  facet_wrap(~ Polarity + Condition, ncol = 2) +
  theme_minimal(base_size = 14) +
  theme(
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.position = "top",
    legend.title = element_text(face = "bold"))


# Modelling
data_Cont_Toy3$Rating <- factor(data_Cont_Toy3$Rating, levels = rev(levels(factor(data_Cont_Toy3$Rating))))

contrasts(data_Cont_Toy3$Polarity) <- contr.sum(levels(data_Cont_Toy3$Polarity))
contrasts(data_Cont_Toy3$AdverbType) <- contr.sum(levels(data_Cont_Toy3$AdverbType))
contrasts(data_Cont_Toy3$Condition) <- contr.sum(levels(data_Cont_Toy3$Condition))


data_Cont_Toy3_clmm <- clmm(Rating ~ Condition * AdverbType * Polarity + (1 | Response_ID) + (1 | Scenario), 
                            data = data_Cont_Toy3, 
                            link = "logit")

# Results of clmm analysis 
print(summary(data_Cont_Toy3_clmm))

# Visualization of the model results
plot(allEffects(data_Cont_Toy3_clmm), style = "stacked",
     main = "Ratings - Polarity and Condition",
     key.args = list(space="right"),
     lattice=list(strip=list(factor.names=FALSE,values=TRUE,cex=1)),
     colors=rev(hcl.colors(5, palette="TealRose")))
## No!

# Does Reduplication show similar behavior to Manner? #
data_Cont_Toy4 <- data_Cont_NC %>%
  filter(
    Type == "Experimental",
    AdverbType %in% c("Manner Adverb", "Reduplication")
  ) %>% droplevels()

data_Cont_Toy4 <- data_Cont_Toy4 %>% mutate(
  Rating = as.factor(Rating),
  Polarity = factor(Polarity, levels = c("Negation in the 1st clause", "Negation in the 2nd clause")),
  Condition = factor(Condition, levels = c("Match", "Mismatch (Locative)")),
  AdverbType = factor(AdverbType, levels = c("Manner Adverb", "Reduplication"))
)

# Ratings for Manner and Time Adverbs
ggplot(data_Cont_Toy4, aes(x = AdverbType, fill = Rating)) +
  geom_bar(position = "fill", color = "black") +
  scale_y_continuous() +
  scale_fill_manual(values = c("#008080", "#70A494", "#EDC9AF", "#FF9B71", "#E63946"))+
  labs(
    subtitle = "Proportion of Ratings",
    x = "Polarity",
    y = "Proportion",
    fill = "Rating"
  ) +
  facet_wrap(~ Polarity + Condition, ncol = 2) +
  theme_minimal(base_size = 14) +
  theme(
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.position = "top",
    legend.title = element_text(face = "bold"))


# Modelling
data_Cont_Toy4$Rating <- factor(data_Cont_Toy4$Rating, levels = rev(levels(factor(data_Cont_Toy4$Rating))))

contrasts(data_Cont_Toy4$Polarity) <- contr.sum(levels(data_Cont_Toy4$Polarity))
contrasts(data_Cont_Toy4$AdverbType) <- contr.sum(levels(data_Cont_Toy4$AdverbType))
contrasts(data_Cont_Toy4$Condition) <- contr.sum(levels(data_Cont_Toy4$Condition))


data_Cont_Toy4_clmm <- clmm(Rating ~ Condition * AdverbType * Polarity + (1 | Response_ID) + (1 | Scenario), 
                            data = data_Cont_Toy4, 
                            link = "logit")

# Results of clmm analysis 
print(summary(data_Cont_Toy4_clmm))

# Visualization of the model results
plot(allEffects(data_Cont_Toy4_clmm), style = "stacked",
     main = "Ratings - Polarity and Condition",
     key.args = list(space="right"),
     lattice=list(strip=list(factor.names=FALSE,values=TRUE,cex=1)),
     colors=rev(hcl.colors(5, palette="TealRose")))

######## Experiment III #################
# Load Data
data_OneClause<- read_excel("Experiment_III.xlsx")
View(data_OneClause)

# Data Preparation
data_OneClause <- data_OneClause %>%
  mutate(
    Polarity = ifelse(grepl("Aff", Trial), "Affirmative", "Negative"),
    Scenario = case_when(
      grepl("1..", Trial) ~ "1",
      grepl("2..", Trial) ~ "2",
      grepl("3..", Trial) ~ "3",
      grepl("4..", Trial) ~ "4",
      grepl("5..", Trial) ~ "5",
      grepl("6..", Trial) ~ "6",
      grepl("7..", Trial) ~ "7",
      grepl("8..", Trial) ~ "8",
      TRUE ~ "Filler"
    ),
    Type =case_when(
      grepl("_UnGr_", Trial) ~ "Ungrammatical Catch Trials",
      grepl("_Gr_", Trial) ~ "Grammatical Catch Trials",
      TRUE ~ "Experimental Trials"),
    Condition =case_when(
      grepl("NonIde", Trial) ~ "Nonideophone",
      grepl("_Ide_", Trial) ~ "Ideophone",
      TRUE ~ "Fillers"),
    AdverbType=case_when(
      grepl("Cont", Trial) ~ "Time Adverb",
      grepl("Red", Trial) ~ "Reduplication",
      grepl("Cvb", Trial) ~ "Converbial",
      grepl("RegMod", Trial) ~ "Manner Adverb",
      grepl("_Gr_", Trial) ~ "Grammatical Catch Trials",
      TRUE ~ "Ungrammatical Catch Trials"),
    Response_ID = as.factor(Response_ID))

View(data_OneClause)

data_OneClause <- data_OneClause %>% mutate(
  Scenario = factor(Scenario, levels = c("1","2", "3","4", "5", "6", "7", "8","Fillers")),
  Polarity = factor(Polarity, levels = c("Affirmative", "Negative")),
  Type = factor(Type, levels = c("Experimental Trials", "Grammatical Catch Trials", "Ungrammatical Catch Trials")),
  Condition = factor(Condition, levels = c("Ideophone", "Nonideophone", "Fillers")),
  AdverbType = factor(AdverbType, levels = c("Manner Adverb", "Converbial", "Reduplication", "Time Adverb","Grammatical Catch Trials", "UnGrammatical Catch Trials"))
)

# Check Data Structure
str(data_OneClause$Response_ID) # 116 participants
levels(data_OneClause$AdverbType)

## Cleaning the data
data_sanity_check <- data_OneClause %>% filter(
  Condition == "Fillers",
) %>% droplevels()

data_sanity_check_summary <- data_sanity_check %>%
  group_by(Response_ID, Type) %>%
  summarise(
    mean_rating = mean(Rating, na.rm = TRUE),
    .groups = "drop"
  )

data_sanity_check_wide <- data_sanity_check_summary %>%
  tidyr::pivot_wider(
    names_from = Type,
    values_from = mean_rating
  ) %>% View()

# Two participants to exclude
# R_8PZCLwD9BXz3phF
# R_27xHK3MlKo8kLRv

exclude_ids <- c("R_8PZCLwD9BXz3phF", "R_27xHK3MlKo8kLRv")

# Cleaned data

data_OneClause_Final <- data_OneClause %>% 
    filter(!Response_ID %in% exclude_ids) %>%
  droplevels() 

str(data_OneClause_Final$Response_ID) # 114 participants

# How does the updated data look?

ggplot(data_OneClause_Final, aes(x = Polarity, y = Rating, color = Type, fill = Type)) +
  geom_violin(
    trim = TRUE,        # Cut tails to min/max of data
    bw = 3,             # Adjust this to control smoothness (try 2–5)
    scale = "width",    # Make violins uniform width
    alpha = 0.3         # Transparent fill for layering
  ) +
  stat_summary(
    fun = mean,
    geom = "point",
    position = position_dodge(0.9),
    size = 2,
    color = "black"
  ) +
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               position = position_dodge(width = 0.9), 
               width = 0.5, color = "black")+
  labs(y = "Mean Rating", x = "Polarity") +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank())



##### Analysis I - Comparison of ideophonic reduplication and converbial ####
data_Exp_III_Ide <- data_OneClause_Final %>% filter(
  Condition == "Ideophone") %>% droplevels()

str(data_Exp_III_Ide)
View(data_Exp_III_Ide)

# Data aggregation here
data_Exp_III_Ide_Agr <- data_Exp_III_Ide %>%
  group_by(AdverbType, Polarity) %>%
  summarise(MeanRating = mean(Rating),
            SE = sd(Rating, na.rm = TRUE) / sqrt(n()),
            .groups = "drop")

data_Exp_III_Ide_Agr %<>% as.data.frame

plot_data_Exp_III_Ide_Agr <- ggplot(data_Exp_III_Ide_Agr, aes(x = Polarity, y = MeanRating, group = AdverbType, 
shape = AdverbType, fill = AdverbType)) +
  geom_point(size= 3) +
  geom_errorbar(aes(ymin = MeanRating - SE, ymax = MeanRating + SE), width = 0.15) +
  scale_shape_manual(values = c(21, 24, 22, 23, 25)) +  # filled vs. open circles
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")

plot_data_Exp_III_Ide_Agr

# lmer model and its summary
contrasts(data_Exp_III_Ide$AdverbType) <- contr.treatment(levels(data_Exp_III_Ide$AdverbType))
contrasts(data_Exp_III_Ide$Polarity) <- contr.treatment(levels(data_Exp_III_Ide$Polarity))

model_data_Exp_III_Ide <- lmer(Rating ~ AdverbType * Polarity+ (1|Response_ID) + (1|Scenario) , data= data_Exp_III_Ide)
summary(model_data_Exp_III_Ide)

vif(model_data_Exp_III_Ide)
predicted <- predict(model_data_Exp_III_Ide)

predicted_capped <- pmin(pmax(predicted, 0), 100)

# Add them to your original data frame for plotting
data_Exp_III_Ide$predicted <- predicted_capped
# 2. Plot predicted values
ggplot(data_Exp_III_Ide, 
       aes(x = Polarity, y = predicted, color = AdverbType, fill = AdverbType)) +
  geom_boxplot(notch = TRUE, outlier.shape = NA, alpha = 0.7) +
  theme_minimal(base_size = 14) +
  labs(
    x = "Polarity",
    y = "Predicted Ratings"  ) +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

# Pairwise comparison
pairs(emmeans(model_data_Exp_III_Ide, ~ AdverbType | Polarity), adjust = "tukey")


##### Analysis II - Non-ideophones integrated - The full picture####

data_Exp_III_All <- data_OneClause_Final %>% filter(
  Condition %in% c("Ideophone", "Nonideophone")) %>% droplevels()

plot_data_Exp_III_All <- ggplot(data_Exp_III_All, aes(x = Polarity, y = Rating, color = AdverbType, fill = AdverbType)) +
  geom_boxplot(notch = TRUE) + facet_wrap(Condition~Type) + theme_minimal() +   labs(x = "Response", y = "Mean Rating")

plot_data_Exp_III_All

data_Exp_III_All_Agr <- data_Exp_III_All %>%
  group_by(AdverbType, Polarity, Type, Condition) %>%
  summarise(MeanRating = mean(Rating),
            SE = sd(Rating, na.rm = TRUE) / sqrt(n()),
            .groups = "drop"
  )

data_Exp_III_All_Agr %<>% as.data.frame

plot_data_Exp_III_All_Agr <- ggplot(data = data_Exp_III_All_Agr, aes(x = Polarity, y = MeanRating, color = AdverbType, fill = AdverbType)) +
  geom_bar(stat = "identity", position = "dodge") + theme_minimal() +   labs(x = "Adverbial Types", y = "Ratings") + facet_wrap(~Condition)

plot_data_Exp_III_All_Agr

## Analysis

contrasts(data_Exp_III_All$AdverbType) <- contr.treatment(levels(data_Exp_III_All$AdverbType))
contrasts(data_Exp_III_All$Condition) <- contr.treatment(levels(data_Exp_III_All$Condition))
contrasts(data_Exp_III_All$Polarity) <- contr.treatment(levels(data_Exp_III_All$Polarity))

model_Exp_III_All <- lmer(Rating ~ AdverbType * Condition * Polarity+ (1|Response_ID) + (1|Scenario) , data= data_Exp_III_All)
print(summary(model_Exp_III_All))

vif(model_Exp_III_All)
predicted <- predict(model_Exp_III_All)

predicted_capped <- pmin(pmax(predicted, 0), 100)

# Add them to your original data frame for plotting
data_Exp_III_All$predicted <- predicted_capped

# Plot predicted values
ggplot(data_Exp_III_All, 
       aes(x = Polarity, y = predicted, color = AdverbType, fill = AdverbType)) +
  facet_wrap(~Condition) +
  geom_boxplot(notch = TRUE, outlier.shape = NA, alpha = 0.7) +
  theme_minimal(base_size = 14) +
  labs(
    x = "Polarity",
    y = "Predicted Ratings"  ) +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

## What about non-ideophones on their own?
data_Exp_III_NonIde <- data_OneClause_Final %>% filter(
  Condition == "Nonideophone") %>% droplevels()

data_Exp_III_NonIde_Agr <- data_Exp_III_NonIde %>%
  group_by(AdverbType, Polarity) %>%
  summarise(MeanRating = mean(Rating),
            SE = sd(Rating, na.rm = TRUE) / sqrt(n()),
            .groups = "drop")

data_Exp_III_NonIde_Agr %<>% as.data.frame

plot_data_Exp_III_NonIde_Agr <- ggplot(data_Exp_III_NonIde_Agr, aes(x = Polarity, y = MeanRating, group = AdverbType, 
                                                              shape = AdverbType, fill = AdverbType)) +
  geom_point(size= 3) +
  geom_errorbar(aes(ymin = MeanRating - SE, ymax = MeanRating + SE), width = 0.15) +
  scale_shape_manual(values = c(21, 24, 22, 23, 25)) +  # filled vs. open circles
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")

plot_data_Exp_III_NonIde_Agr

# lmer model and its summary

contrasts(data_Exp_III_NonIde$AdverbType) <- contr.treatment(levels(data_Exp_III_NonIde$AdverbType))
contrasts(data_Exp_III_NonIde$Polarity) <- contr.treatment(levels(data_Exp_III_NonIde$Polarity))

model_data_Exp_III_NonIde <- lmer(Rating ~ AdverbType * Polarity+ (1|Response_ID) + (1|Scenario) , data= data_Exp_III_NonIde)
summary(model_data_Exp_III_NonIde)

vif(model_data_Exp_III_NonIde)
predicted <- predict(model_data_Exp_III_NonIde)

predicted_capped <- pmin(pmax(predicted, 0), 100)

# Add them to your original data frame for plotting
data_Exp_III_NonIde$predicted <- predicted_capped
# 2. Plot predicted values
ggplot(data_Exp_III_NonIde, 
       aes(x = Polarity, y = predicted, color = AdverbType, fill = AdverbType)) +
  geom_boxplot(notch = TRUE, outlier.shape = NA, alpha = 0.7) +
  theme_minimal(base_size = 14) +
  labs(
    x = "Polarity",
    y = "Predicted Ratings"  ) +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

# Pairwise comparison
pairs(emmeans(model_data_Exp_III_NonIde, ~ AdverbType | Polarity), adjust = "tukey")

########  Fillers of Experiment I and II ######## 
## You can find this analysis in Appendix C. The aim is to presenting the general picture rather than
## providing a theoretical discussion.

Filler_Exp_I <- data_Cont %>%
  filter(
    Type == "Filler"
  ) %>% droplevels()

Filler_Exp_II <- data_Cont_NC %>%
  filter(
    Type == "Filler",
  ) %>% droplevels()

data_Fillers <- bind_rows(Filler_Exp_I, Filler_Exp_II) %>% as.data.frame()
View(data_Fillers)

data_Fillers$Order <- ifelse(grepl("NC", data_Fillers$Trial), "DO>>IO (Exp II)", "IO>>DO (Exp I)")
data_Fillers$Condition <- ifelse(grepl("Indirect Object", data_Fillers$Condition), "Continuation with Indirect Object", "Continuation with Direct Object")

data_Fillers <- data_Fillers %>% mutate(
  Polarity = factor(Polarity, levels = c("Negation in the 2nd clause", "Negation in the 1st clause")),
  Condition = factor(Condition, levels = c("Continuation with Direct Object", "Continuation with Indirect Object")),
  Order = factor(Order, levels = c("IO>>DO (Exp I)", "DO>>IO (Exp II)")),
  Rating = factor(Rating, levels = c("5", "4", "3", "2", "1"), ordered = TRUE))


str(data_Fillers)

data_Fillers$Rating <- factor(data_Fillers$Rating, levels = rev(levels(factor(data_Fillers$Rating))))


ggplot(data_Fillers, aes(x = Order, fill = Rating)) +
  geom_bar(position = "fill", color = "black") +
  scale_y_continuous() +
  scale_fill_manual(values = c("#008080", "#70A494", "#EDC9AF", "#FF9B71", "#E63946"))+
  labs(
    subtitle = "Proportion of Ratings",
    x = "Polarity",
    y = "Proportion",
    fill = "Rating"
  ) +
  facet_wrap(Polarity ~ Condition) +
  theme_minimal(base_size = 14) +
  theme(
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.position = "top",
    legend.title = element_text(face = "bold"))

options(contrasts = c("contr.sum", "contr.poly"))

data_Fillers_clmm <- clmm(Rating ~ Order * Condition * Polarity + (1 | Response_ID) + (1 | Scenario), 
                            data = data_Fillers, 
                            link = "logit")

summary(data_Fillers_clmm)

plot(allEffects(data_Fillers_clmm), style = "stacked",
     key.args = list(space="right"),
     xlab = "Word Order",
     lattice=list(strip=list(factor.names=FALSE,values=TRUE,cex=1)),
     colors=rev(hcl.colors(5, palette="TealRose")))

