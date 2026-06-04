# Salty P Analysis
# G. Wilkinson, code adapted from E. Albright
library(tidyverse)
library(ggpubr)

raw <- read_csv("saltyP_allData_allSeasons_final.csv") 

# REASSEMBLE THE DATA FRAME, CALCULATE P MASS
delta = raw %>%
  mutate(srp_flag = case_when(is.na(.$srp_flag) ~ "keep",
                              TRUE ~ srp_flag)) %>%
  filter(!srp_flag == "remove") %>%

  # Calculate the difference in P after shaking, units = mg/L
  mutate(deltaSRPconc = (InitialP/1000) - (srp/1000)) %>%
  # Calculate change in SRP as mass, Units = mg
  mutate(deltaSRPmass = deltaSRPconc*0.04) %>% #40 mL solution
  # Calculate the change in SRP mass per gram of dry sediment.
  # Units=mgP/g dry sediment
  mutate(deltaP = deltaSRPmass/sediment_mass) %>%
  
  # Calculate the difference in chloride after shaking, units = mg/L 
  mutate(deltaChlorideconc = chloride - InitialCl) %>%
  # Calculate change in chloride as mass, Units = mg
  mutate(deltaChloridemass = deltaChlorideconc*0.04) %>% #40 mL solution 
  # Calculate the change in chloride mass per gram of dry sediment. 
  # Units=mg Cl/g dry sediment
  mutate(deltaCl = deltaChloridemass/sediment_mass) %>%
  
  #select down to the important columns
  select(pond, season, InitialP, InitialCl, deltaP, deltaCl) %>%
  mutate(season = case_when(season == 'fall' ~ "Fall 2022",
                            season == 'winter' ~ "Winter 2023",
                            TRUE ~ 'Spring 2023')) %>%
  mutate(season = factor(season, levels = c("Fall 2022", "Winter 2023", "Spring 2023")))

# # FIGURE =======================================
# ## Sediment P sorption capacity among ponds and seasons - the data that EPC is calculated from
# ggplot(delta, aes(x = InitialP/1000, y = deltaP)) +
#   geom_point(size = 2, alpha = 0.75, aes(colour = as.factor(InitialCl))) + 
#   geom_line(size = 1, alpha = 0.75, aes(colour = as.factor(InitialCl))) +
#   scale_color_brewer(palette = "Oranges") +
#   theme_bw() +
#   guides(color = guide_legend(title = "Chloride (mg/L)")) +
#   geom_hline(yintercept = 0, color = 'white' , linetype = 'dashed') +
#   facet_grid(season~pond) +
#   ylab("P in Solution after Incubation (mg P/g dry sediment)") + 
#   xlab("Initial P added to Solution (mg/L)")


#===================================================
# Calculate EPC
## Need to create a data frame of the intercept and slope for each pond, season, and initial chloride
epc = delta %>%
  mutate(InitialP = InitialP/1000) %>%
  group_by(pond, season, InitialCl) %>% 
  summarise(slope = as.numeric(lm(deltaP ~ InitialP)$coefficients[2]),
            slope_error = as.numeric(summary(lm(deltaP ~ InitialP))$coefficients[2,'Std. Error']),
            intercept = as.numeric(lm(deltaP ~ InitialP)$coefficients[1]),
            r2 = summary(lm(deltaP ~ InitialP))$r.squared,
            epc = ((0 - intercept)/slope)*1000) %>%
  ungroup() %>%
  mutate(pond = factor(pond, levels = c("elver", "lakeview", "orchid", 
                                        "strickers", "tiedemans", 
                                        "wingra shallow", "wingra deep")))

# Delta P regressions
# ggplot(delta %>% filter(pond == "wingra deep"), 
#        aes(y = deltaP, x = InitialP, color = season)) + 
#   geom_point(size = 2) + geom_line() +
#   facet_grid(InitialCl~season, scale = "free_x") + 
#   theme_bw() + 
#   geom_smooth(method = "lm")

## EPC vs Initial Chloride -- organized by season, pond = color
gg_epc_season =
  ggplot(epc, aes(x = InitialCl, y = epc, color = pond)) + 
  geom_line(linewidth = 1) + geom_point(size = 2) + 
  scale_color_manual(labels = c("Elver", "Lakeview", "Orchid", 
                                "Stricker's", "Tiedeman's", 
                                "Wingra, shallow", "Wingra, deep"),
                     values = c("#20753D", "#44AA99", "#48A0CC", 
                                "#D0BC5A", "#CC6677", "#A9539A", "#882255")) +
  # scale_y_continuous(transform = "log10") +
  theme_bw() +
  facet_wrap(~season) +
  theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "inches")) +
  xlab("Chloride Added (mg/L)") + ylab('EPC (ug/L)') 


### Comparing Spring EPC to Bottom Water SRP
bottomSRP = read_csv("epc_coordinates.csv")

epc_comparison = left_join(epc, bottomSRP, 
                           by = c("pond", "season")) %>%
  mutate(epc_effect = epc - bottomSRP,
         season = factor(season, 
                         levels = c('Fall 2022', 
                                    "Winter 2023", 
                                    "Spring 2023")),
         pond = factor(pond, levels = c("elver", "lakeview", "orchid", 
                                        "strickers", "tiedemans", 
                                        "wingra shallow", "wingra deep")),
         InitialCl_label = case_when(InitialCl == 0 ~ "Initial Cl = 0",
                                     InitialCl == 50 ~ "Initial Cl = 50",
                                     InitialCl == 100 ~ "Initial Cl = 100",
                                     InitialCl == 500 ~ "Initial Cl = 500"),
         InitialCl_label = factor(InitialCl_label, 
                                  levels = c("Initial Cl = 0", "Initial Cl = 50", 
                                             "Initial Cl = 100", "Initial Cl = 500")))

gg_epc_minus_hypoSRP = 
  ggplot(epc_comparison, 
       aes(x = InitialCl, y = epc_effect, color = pond)) +
  geom_line(linewidth = 1) + geom_point(size = 2) + 
  scale_color_manual(labels = c("Elver", "Lakeview", "Orchid", 
                                "Stricker's", "Tiedeman's", 
                                "Wingra, shallow", "Wingra, deep"),
                     values = c("#20753D", "#44AA99", "#48A0CC", 
                                "#D0BC5A", "#CC6677", "#A9539A", "#882255")) +
  geom_hline(yintercept = 0, 
             linetype = 'dashed', 
             linewidth = 0.75) +
  facet_wrap(facet = "season") +
  theme_bw() + theme(legend.title = element_blank()) +
  xlab("Chloride Added (mg/L)") + 
  ylab("EPC – bottom SRP (ug/L)")

ggsave("epc_season_vs_bottomSRP.pdf",
  ggarrange(gg_epc_season, gg_epc_minus_hypoSRP, nrow = 2, align = "hv"), 
  height = 4.4, width = 6.5, units = "in")

# =====================================
gg_epc_chloride = 
  ggplot(epc_comparison, 
       aes(x = chloride, y = epc)) + 
  geom_point(size = 3, alpha = 0.7, aes(color = pond, shape = season)) + 
  scale_color_manual(
    labels = c("Elver", "Lakeview", "Orchid", 
               "Stricker's", "Tiedeman's", 
               "Wingra, shallow", "Wingra, deep"),
    values = c("#20753D", "#44AA99", 
               "#48A0CC", "#D0BC5A", 
               "#CC6677", "#A9539A", "#882255")) +
  facet_wrap(~InitialCl_label, nrow = 1) +
  theme_bw() + 
  ylab("EPC") + xlab("Chloride at Time of Sampling (mg/L)") 

gg_epc_srp = 
  ggplot(epc_comparison, 
         aes(x = bottomSRP, y = epc)) + 
  geom_point(size = 3, alpha = 0.7, aes(color = pond, shape = season)) + 
  scale_color_manual(
    labels = c("Elver", "Lakeview", "Orchid", 
               "Stricker's", "Tiedeman's", 
               "Wingra, shallow", "Wingra, deep"),
    values = c("#20753D", "#44AA99", 
               "#48A0CC", "#D0BC5A", 
               "#CC6677", "#A9539A", "#882255")) +
  facet_wrap(~InitialCl_label, nrow = 1) +
  theme_bw() + 
  ylab("EPC") + xlab("SRP at Time of Sampling (ug/L)")

gg_epc_sulfate = 
  ggplot(epc_comparison, 
         aes(x = sulfate, y = epc)) + 
  geom_point(size = 3, alpha = 0.7, aes(color = pond, shape = season)) + 
  scale_color_manual(
    labels = c("Elver", "Lakeview", "Orchid", 
               "Stricker's", "Tiedeman's", 
               "Wingra, shallow", "Wingra, deep"),
    values = c("#20753D", "#44AA99", 
               "#48A0CC", "#D0BC5A", 
               "#CC6677", "#A9539A", "#882255")) +
  facet_wrap(~InitialCl_label, nrow = 1) +
  theme_bw() + 
  ylab("EPC") + xlab("Sulfate at Time of Sampling (mg/L)")

gg_epc_pH = 
  ggplot(epc_comparison, 
         aes(x = pH, y = epc)) + 
  geom_point(size = 3, alpha = 0.7, aes(color = pond, shape = season)) + 
  scale_color_manual(
    labels = c("Elver", "Lakeview", "Orchid", 
               "Stricker's", "Tiedeman's", 
               "Wingra, shallow", "Wingra, deep"),
    values = c("#20753D", "#44AA99", 
               "#48A0CC", "#D0BC5A", 
               "#CC6677", "#A9539A", "#882255")) +
  facet_wrap(~InitialCl_label, nrow = 1) +
  theme_bw() + 
  ylab("EPC") + xlab("pH at Time of Sampling")

gg_epc_dic = 
  ggplot(epc_comparison, 
         aes(x = dic, y = epc)) + 
  geom_point(size = 3, alpha = 0.7, aes(color = pond, shape = season)) + 
  scale_color_manual(
    labels = c("Elver", "Lakeview", "Orchid", 
               "Stricker's", "Tiedeman's", 
               "Wingra, shallow", "Wingra, deep"),
    values = c("#20753D", "#44AA99", 
               "#48A0CC", "#D0BC5A", 
               "#CC6677", "#A9539A", "#882255")) +
  facet_wrap(~InitialCl_label, nrow = 1) +
  theme_bw() + 
  ylab("EPC") + xlab("Dissolved Inorganic C at Time of Sampling (mg/L)")

ggarrange(gg_epc_chloride, gg_epc_srp, gg_epc_sulfate, gg_epc_pH, gg_epc_dic, 
          nrow = 5, legend = "right", common.legend = TRUE)


# # # # FIGURE ==========================================
# # # # Chloride exchange ===============================
# combo = left_join(delta, epc, by = c("pond", "season", "InitialCl")) %>%
#   filter(!is.na(deltaCl))
#   # mutate(InitialCl = factor(InitialCl, levels = c("0", "50", "100", "500")))
# # filter(!deltaCl > 2)
# 
# 
# ## EPC vs Initial Chloride -- organized by pond, season = color
# ggplot(combo, aes(x = deltaCl, y = epc, color = pond, shape = season, size = InitialCl)) +
#   geom_point() +
#   geom_polygon(aes(color = pond, fill = pond), alpha = 0.5) +
#   scale_fill_manual(labels = c("Elver", "Lakeview", "Orchid",
#                                 "Stricker's", "Tiedeman's",
#                                 "Wingra, shallow", "Wingra, deep"),
#                      values = c("#20753D", "#44AA99", "#48A0CC",
#                                 "#D0BC5A", "#CC6677", "#A9539A", "#882255")) +
#   scale_color_manual(values = c("#20753D", "#44AA99", "#48A0CC",
#                                 "#D0BC5A", "#CC6677", "#A9539A", "#882255")) +
#   theme_bw() + guides(color = FALSE) +
#   xlab("Change in Chloride (mg Cl/g dry sediment)") +
#   ylab("Equilibrium P Concentration (ug/L)") +
#   # region of low P sorption, chloride sorbed or low exchange
#   annotate(geom = "text", label = "1", x = -0.4, y = 70, color = "black", size = 6) +
#   # region of high P sorption, chloride sorbed or low exchange
#   annotate(geom = "text", label = "2", x = -0.4, y = 3, color = "black", size = 6) +
#   # region of High P sorption, chloride release from sediments
#   annotate(geom = "text", label = "3", x = 9.5, y = 3, color = "black", size = 6)
# 
# 


epc_chloride_effect = epc %>%
  select(pond, season, InitialCl, epc) %>%
  pivot_wider(., names_from = "InitialCl", values_from = "epc") %>%
  rename(initial0 = `0`, initial50 = `50`, initial100 = `100`, initial500 = `500`) %>%
  mutate(epc50_diff = initial50 - initial0,
         epc100_diff = initial100 - initial0, 
         epc500_diff = initial500 - initial0) %>%
  left_join(., bottomSRP, by = c('pond', 'season')) 

epc_diff = epc_chloride_effect %>% 
  select(pond, season, epc50_diff:epc500_diff) %>%
  pivot_longer(., cols = epc50_diff:epc500_diff, names_to = "diff_category", values_to = "diff_value") %>%
  mutate(diff_category = factor(case_when(diff_category == "epc50_diff" ~ "Diff in EPC at 50 mg/L Cl Added",
                                          diff_category == "epc100_diff" ~ "Diff in EPC at 100 mg/L Cl Added",
                                          diff_category == "epc500_diff" ~ "Diff in EPC at 500 mg/L Cl Added"),
                                levels = c("Diff in EPC at 50 mg/L Cl Added", 
                                           "Diff in EPC at 100 mg/L Cl Added", 
                                           "Diff in EPC at 500 mg/L Cl Added")))


ggplot(epc_diff, aes(x = diff_value)) +
  geom_histogram(aes(y = after_stat(density)), fill = "#DAA2CA") +
  geom_density(linewidth = 1.5, color = "#9E4083") +
  facet_wrap(~ diff_category, scales = "free_y") +
  theme_bw() + 
  xlab("Difference in EPC (ug/L) from 0 mg/L Chloride Added Treatment") + 
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 1)



