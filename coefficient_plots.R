### 
# Plotting the coefficients cmputed with the STATA code
# (manual import of estimates and confidence intervals)
# author: WB Schmal
#

# NOTE to add your desired path to store the plots if wanted
### General set-up ----
# load packages 
library(ggplot2)
library(tibble)

# Relabel Coefficient column values: 
# 1 Control Group
getNewCoefficient1 <- function(row) {
  if (row %% 3 == 1) {
    return("ANID")
  } else if (row %% 3 == 2) {
    return("MBIE")
  } else {
    return("NSFC")
  }
}

# 2 Comparison Group
getNewCoefficient2 <- function(row) {
  if (row %% 3 == 1) {
    return("DFG")
  } else if (row %% 3 == 2) {
    return("FWO")
  } else {
    return("NCI")
  }
}



### Pooled Regressions ----
# 4.96 x 3.95 in image
# OA ALL
data <- tribble(
  ~Group, ~Coefficient, ~Estimate, ~CI_Lower_95, ~CI_Upper_95, ~CI_Lower_99, ~CI_Upper_99,
  1, "r1", -0.07450199, -0.09942395, -0.04958004, -0.10725661, -0.04174738,
  2, "r2", -0.08562363, -0.10165528, -0.06959198, -0.10669395, -0.06455331
)

# Rename groups
data$Group <- factor(data$Group, levels = c(1, 2), labels = c("Control group", "Comparison group"))


ggplot(data, aes(x = as.factor(1:nrow(data)), y = Estimate, color = factor(Group))) +
  geom_point(position = position_dodge(width = 0.5), size = 2) +
  geom_errorbar(aes(ymin = CI_Lower_99, ymax = CI_Upper_99), 
                position = position_dodge(width = 0.5), 
                width = 0.3,  linewidth = .8, alpha = 0.5) +  # Set alpha for 99% CI
  geom_errorbar(aes(ymin = CI_Lower_95, ymax = CI_Upper_95), 
                position = position_dodge(width = 0.5), 
                width = 0.2, linewidth = .9) +  # 95% CI with full opacity
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Pooled Comparison with 'Non Plan S' Funders",
    subtitle = "All Open Access Types",
    x = "Compared to 'Non Plan S' funders",
    y = "Point Coefficients",
    color = "Relative to:"
  ) +
  theme_bw() +  ylim(-0.205, 0.05) +
  theme(axis.text.x = element_blank(), legend.position = "bottom") +
  scale_x_discrete(labels = data$Coefficient)
ggsave(filename = "pooled_oa_all.png", plot = last_plot(), path = "", scale = 1, dpi = 600)



# Gold OA WITH exclusion
data <- tribble(
  ~Group, ~Coefficient, ~Estimate, ~CI_Lower_95, ~CI_Upper_95, ~CI_Lower_99, ~CI_Upper_99,
  1, "r1", -0.03489988, -0.07208503, 0.00228527, -0.08377217, 0.01397242,
  2, "r2", -0.12424828, -0.14971567, -0.09878089, -0.15772035, -0.09077621
)


# Rename groups
data$Group <- factor(data$Group, levels = c(1, 2), labels = c("Control group", "Comparison group"))


ggplot(data, aes(x = as.factor(1:nrow(data)), y = Estimate, color = factor(Group))) +
  geom_point(position = position_dodge(width = 0.5), size = 2) +
  geom_errorbar(aes(ymin = CI_Lower_99, ymax = CI_Upper_99), 
                position = position_dodge(width = 0.5), 
                width = 0.3,  linewidth = .8, alpha = 0.5) +  # Set alpha for 99% CI
  geom_errorbar(aes(ymin = CI_Lower_95, ymax = CI_Upper_95), 
                position = position_dodge(width = 0.5), 
                width = 0.2, linewidth = .9) +  # 95% CI with full opacity
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Pooled Comparison with 'Non Plan S' Funders",
    subtitle = "Only Gold Open Access",
    x = "Compared to 'Non Plan S' funders",
    y = "Point Coefficients",
    color = "Relative to:"
  ) +
  theme_bw() +  ylim(-0.205, 0.05) +
  theme(axis.text.x = element_blank(), legend.position = "bottom") +
  scale_x_discrete(labels = data$Coefficient)
ggsave(filename = "pooled_oa_gold_excl.png", plot = last_plot(), path = "", scale = 1, dpi = 600)




# Hybrid OA WITH exclusion
data <- tribble(
  ~Group, ~Coefficient, ~Estimate, ~CI_Lower_95, ~CI_Upper_95, ~CI_Lower_99, ~CI_Upper_99,
  1, "r1", 0.01705856, 0.004921, 0.02919611, 0.00110614, 0.03301097,
  2, "r1", -0.16939065, -0.19474424, -0.14403706, -0.20271331, -0.136068
)

# Rename groups
data$Group <- factor(data$Group, levels = c(1, 2), labels = c("Control group", "Comparison group"))


ggplot(data, aes(x = as.factor(1:nrow(data)), y = Estimate, color = factor(Group))) +
  geom_point(position = position_dodge(width = 0.5), size = 2) +
  geom_errorbar(aes(ymin = CI_Lower_99, ymax = CI_Upper_99), 
                position = position_dodge(width = 0.5), 
                width = 0.3,  linewidth = .8, alpha = 0.5) +  # Set alpha for 99% CI
  geom_errorbar(aes(ymin = CI_Lower_95, ymax = CI_Upper_95), 
                position = position_dodge(width = 0.5), 
                width = 0.2, linewidth = .9) +  # 95% CI with full opacity
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Pooled Comparison with 'Non Plan S' Funders",
    subtitle = "Only Hybrid (Gold) Open Access",
    x = "Compared to 'Non Plan S' funders",
    y = "Point Coefficients",
    color = "Relative to:"
  ) +
  theme_bw() +  ylim(-0.205, 0.05) +
  theme(axis.text.x = element_blank(), legend.position = "bottom") +
  scale_x_discrete(labels = data$Coefficient)
ggsave(filename = "pooled_oa_hybrid_excl.png", plot = last_plot(), path = "", scale = 1, dpi = 600)



# Green OA WITH exclusion
data <- tribble(
  ~Group, ~Coefficient, ~Estimate, ~CI_Lower_95, ~CI_Upper_95, ~CI_Lower_99, ~CI_Upper_99,
  1, "r1", -0.1574577, -0.18157189, -0.13334351, -0.18915113, -0.12576427,
  2, "r1", -0.14537343, -0.17230233, -0.11844452, -0.18076666, -0.1099802
)

# Rename groups
data$Group <- factor(data$Group, levels = c(1, 2), labels = c("Control group", "Comparison group"))


ggplot(data, aes(x = as.factor(1:nrow(data)), y = Estimate, color = factor(Group))) +
  geom_point(position = position_dodge(width = 0.5), size = 2) +
  geom_errorbar(aes(ymin = CI_Lower_99, ymax = CI_Upper_99), 
                position = position_dodge(width = 0.5), 
                width = 0.3,  linewidth = .8, alpha = 0.5) +  # Set alpha for 99% CI
  geom_errorbar(aes(ymin = CI_Lower_95, ymax = CI_Upper_95), 
                position = position_dodge(width = 0.5), 
                width = 0.2, linewidth = .9) +  # 95% CI with full opacity
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Pooled Comparison with 'Non Plan S' Funders",
    subtitle = "Only Green Open Access",
    x = "Compared to 'Non Plan S' funders",
    y = "Point Coefficients",
    color = "Relative to:"
  ) +
  theme_bw() +  ylim(-0.205, 0.05) +
  theme(axis.text.x = element_blank(), legend.position = "bottom") +
  scale_x_discrete(labels = data$Coefficient)
ggsave(filename = "pooled_oa_green_excl.png", plot = last_plot(), path = "", scale = 1, dpi = 600)


################################################################################
### Pairwise comparisons: ------
# 7.84 x 3.95 in image 
# Step 1 - All OA types ----

# Control group
data <- tribble(
  ~Group, ~Coefficient, ~Estimate, ~CI_Lower_95, ~CI_Upper_95, ~CI_Lower_99, ~CI_Upper_99,
  1, "r1", -.03035862, -.06509238, .00437514, -.07601185, .01529462,
  1, "r1", -.09465845, -.15674266, -.03257424, -.1762635, -.01305339,
  1, "r1", -.01638298, -.04501577, .01224982, -.05401509, .02124914,
  2, "r1", -.14659143, -.19163102, -.10155183, -.20579245, -.08739041,
  2, "r1", -.21089126, -.28082456, -.14095796, -.30282771, -.11895482,
  2, "r1", -.13261579, -.16777669, -.09745489, -.1788279, -.08640368,
  3, "r1", .09491585, .06464374, .12518796, .05512738, .13470431,
  3, "r1", .03061602, -.02785997, .089092, -.04624403, .10747606,
  3, "r1", .10889149, .07250077, .1452822, .06106318, .1567198,
  4, "r1", -.05164523, -.08699363, -.01629682, -.09810581, -.00518464,
  4, "r1", -.11594506, -.17997899, -.05191113, -.200111, -.03177912,
  4, "r1", -.03766959, -.06600446, -.00933471, -.07491008, -.0004291,
  5, "r1", -.1095469, -.14665894, -.07243487, -.15832383, -.06076998,
  5, "r1", -.17384674, -.23881302, -.10888046, -.25923329, -.08846019,
  5, "r1", -.09557126, -.12150359, -.06963894, -.1296539, -.06148863
)


# Apply the function to update Coefficient values
data$Coefficient <- sapply(1:nrow(data), getNewCoefficient1)

# Rename groups
data$Group <- factor(data$Group, levels = c(1, 2, 3, 4, 5), labels = c("FWF", "HHMI", "NCN", "NWO", "UKRI"))


ggplot(data, aes(x = as.factor(1:nrow(data)), y = Estimate, color = factor(Group))) +
  geom_point(position = position_dodge(width = 0.5), size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_errorbar(aes(ymin = CI_Lower_99, ymax = CI_Upper_99), 
                position = position_dodge(width = 0.5), 
                width = 0.3,  linewidth = .8, alpha = 0.5) +  # Set alpha for 99% CI
  geom_errorbar(aes(ymin = CI_Lower_95, ymax = CI_Upper_95), 
                position = position_dodge(width = 0.5), 
                width = 0.2, linewidth = .9) +  # 95% CI with full opacity
  labs(
    title = "Pairwise Comparison with 'Non Plan S' Funders",
    subtitle = "All Open Access Types",
    x = "Compared to 'Non Plan S' funders",
    y = "Point Coefficients",
    color = "Plan S Funder"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(labels = data$Coefficient)
ggsave(filename = "pairwise_comp_oaall_control.png", plot = last_plot(), path = "", scale = 1, dpi = 600)


## Comparison Group

data <- tribble(
  ~Group, ~Coefficient, ~Estimate, ~CI_Lower_95, ~CI_Upper_95, ~CI_Lower_99, ~CI_Upper_99,
  1, "r1", -.10379169, -.12331876, -.08426461, -.12945662, -.07812675,
  1, "r1", .08049733, .06221881, .09877586, .0564725, .10452216,
  1, "r1", .17167436, .14803803, .19531068, .14060739, .20274132,
  2, "r1", -.2200245, -.253784, -.186265, -.26439558, -.17565341,
  2, "r1", -.03573548, -.06494568, -.00652528, -.07412969, .00265873,
  2, "r1", .05544155, .0284421, .08244099, .01995193, .09093116,
  3, "r1", .02148278, -.00553594, .0485015, -.01402844, .056994,
  3, "r1", .2057718, .17717137, .23437223, .16818068, .24336292,
  3, "r1", .29694882, .2618124, .33208524, .2507672, .34313045,
  4, "r1", -.1250783, -.14504059, -.105116, -.15131517, -.09884142,
  4, "r1", .05921072, .04138564, .07703581, .03578205, .0826394,
  4, "r1", .15038775, .12766621, .17310929, .12052346, .18025204,
  5, "r1", -.18297997, -.20584621, -.16011374, -.21303319, -.15292676,
  5, "r1", .00130905, -.01524753, .01786562, -.02045154, .02306963,
  5, "r1", .09248607, .07338205, .11159009, .06737741, .11759473
)


# Apply the function to update Coefficient values
data$Coefficient <- sapply(1:nrow(data), getNewCoefficient2)

# Rename groups
data$Group <- factor(data$Group, levels = c(1, 2, 3, 4, 5), labels = c("FWF", "HHMI", "NCN", "NWO", "UKRI"))

# Plot coefficients with confidence intervals, colored by groups
ggplot(data, aes(x = as.factor(1:nrow(data)), y = Estimate, color = factor(Group))) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_point(position = position_dodge(width = 0.5), size = 2) +
  geom_errorbar(aes(ymin = CI_Lower_99, ymax = CI_Upper_99), 
                position = position_dodge(width = 0.5), 
                width = 0.3,  linewidth = .8, alpha = 0.5) +  # Set alpha for 99% CI
  geom_errorbar(aes(ymin = CI_Lower_95, ymax = CI_Upper_95), 
                position = position_dodge(width = 0.5), 
                width = 0.2, linewidth = .9) +  # 95% CI with full opacity
  labs(
    title = "Pairwise Comparison with 'OA focused' Funders",
    subtitle = "All Open Access Types",
    x = "Compared to 'Non Plan S but OA focused' funders",
    y = "Point Coefficients",
    color = "Plan S Funder"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(labels = data$Coefficient)
ggsave(filename = "pairwise_comparison_oaall_comp.png", plot = last_plot(), path = "", scale = 1, dpi = 600)

### GOLD OA only ----

# main control group

data <- tribble(
  ~Group, ~Coefficient, ~Estimate, ~CI_Lower_95, ~CI_Upper_95, ~CI_Lower_99, ~CI_Upper_99,
  1, "r1", -.0230563, -.07381157, .02769897, -.08977056, .04365796,
  1, "r1", .02273629, -.05616061, .1016332, -.08097856, .12645115,
  1, "r1", .0776866, .01729159, .13808162, -.00169104, .15706424,
  2, "r1", -.27000827, -.35509856, -.18491797, -.38185664, -.1581599,
  2, "r1", -.22421567, -.34463192, -.10379942, -.38253808, -.06589326,
  2, "r1", -.16926536, -.23970841, -.09882232, -.26184929, -.07668143,
  3, "r1", .07048882, .02805434, .1129233, .01471334, .1262643,
  3, "r1", .11628142, .03433609, .19822674, .00856981, .22399302,
  3, "r1", .17123172, .11313928, .22932417, .0948805, .24758294,
  4, "r1", -.08023775, -.13047338, -.03000211, -.14626853, -.01420696,
  4, "r1", -.03444515, -.12384695, .05495665, -.15196751, .08307721,
  4, "r1", .02050516, -.03230227, .07331259, -.04890002, .08991034,
  5, "r1", -.20625998, -.25786481, -.15465515, -.27408702, -.13843294,
  5, "r1", -.16046738, -.25936946, -.0615653, -.29046208, -.03047269,
  5, "r1", -.10551708, -.13842469, -.07260946, -.14876759, -.06226656
)

# Apply the function to update Coefficient values
data$Coefficient <- sapply(1:nrow(data), getNewCoefficient1)

# Rename groups
data$Group <- factor(data$Group, levels = c(1, 2, 3, 4, 5), labels = c("FWF", "HHMI", "NCN", "NWO", "UKRI"))


ggplot(data, aes(x = as.factor(1:nrow(data)), y = Estimate, color = factor(Group))) +
  geom_point(position = position_dodge(width = 0.5), size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_errorbar(aes(ymin = CI_Lower_99, ymax = CI_Upper_99), 
                position = position_dodge(width = 0.5), 
                width = 0.3,  linewidth = .8, alpha = 0.5) +  # Set alpha for 99% CI
  geom_errorbar(aes(ymin = CI_Lower_95, ymax = CI_Upper_95), 
                position = position_dodge(width = 0.5), 
                width = 0.2, linewidth = .9) +  # 95% CI with full opacity
  labs(
    title = "Pairwise Comparison with 'Non Plan S' Funders",
    subtitle = "Only Gold Open Access",
    x = "Compared to 'Non Plan S' funders",
    y = "Point Coefficients",
    color = "Plan S Funder"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(labels = data$Coefficient)
ggsave(filename = "pairwise_comp_goldoa_control.png", plot = last_plot(), path = "", scale = 1, dpi = 600)

## Comparison Group

data <- tribble(
  ~Group, ~Coefficient, ~Estimate, ~CI_Lower_95, ~CI_Upper_95, ~CI_Lower_99, ~CI_Upper_99,
  1, "r1", -.07064649, -.11279691, -.02849606, -.1260468, -.01524618,
  1, "r1", .10926024, .07255308, .14596741, .06101036, .15751013,
  1, "r1", .28246311, .2160536, .34887262, .19517006, .36975616,
  2, "r1", -.31759845, -.38176717, -.25342973, -.40193861, -.23325829,
  2, "r1", -.13769172, -.21158685, -.06379659, -.23482687, -.04055657,
  2, "r1", .03551115, -.03636931, .1073916, -.05898045, .13000274,
  3, "r1", .02289863, -.02134386, .06714113, -.03525089, .08104816,
  3, "r1", .20280537, .16224425, .24336648, .14949202, .25611871,
  3, "r1", .37600823, .30968735, .44232911, .28883662, .46317984,
  4, "r1", -.12782793, -.16356419, -.09209168, -.17479774, -.08085813,
  4, "r1", .0520788, .01724151, .08691609, .00628698, .09787062,
  4, "r1", .22528167, .16520147, .28536186, .14630868, .30425465,
  5, "r1", -.25385016, -.28194442, -.22575591, -.29077523, -.2169251,
  5, "r1", -.07394343, -.11238237, -.0355045, -.12446617, -.0234207,
  5, "r1", .09925943, .04896463, .14955423, .03315396, .16536491
)


# Apply the function to update Coefficient values
data$Coefficient <- sapply(1:nrow(data), getNewCoefficient2)

# Rename groups
data$Group <- factor(data$Group, levels = c(1, 2, 3, 4, 5), labels = c("FWF", "HHMI", "NCN", "NWO", "UKRI"))

# Plot coefficients with confidence intervals, colored by groups
ggplot(data, aes(x = as.factor(1:nrow(data)), y = Estimate, color = factor(Group))) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_point(position = position_dodge(width = 0.5), size = 2) +
  geom_errorbar(aes(ymin = CI_Lower_99, ymax = CI_Upper_99), 
                position = position_dodge(width = 0.5), 
                width = 0.3,  linewidth = .8, alpha = 0.5) +  # Set alpha for 99% CI
  geom_errorbar(aes(ymin = CI_Lower_95, ymax = CI_Upper_95), 
                position = position_dodge(width = 0.5), 
                width = 0.2, linewidth = .9) +  # 95% CI with full opacity
  labs(
    title = "Pairwise Comparison with 'OA focused' Funders: Only Gold OA",
    subtitle = "Only Gold Open Access",
    x = "Compared to 'Non Plan S but OA focused' funders",
    y = "Point Coefficients",
    color = "Plan S Funder"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(labels = data$Coefficient)
ggsave(filename = "pairwise_comparison_goldoa_comp.png", plot = last_plot(), path = "", scale = 1, dpi = 600)


### HYBRID OA only ----

# control group

data <- tribble(
  ~Group, ~Coefficient, ~Estimate, ~CI_Lower_95, ~CI_Upper_95, ~CI_Lower_99, ~CI_Upper_99,
  1, "r1", .06538502, .03294716, .09782288, .02274775, .10802228,
  1, "r1", -.08912276, -.13477226, -.04347326, -.14912892, -.02911661,
  1, "r1", .08848416, .06713878, .10982954, .06042951, .1165388,
  2, "r1", -.07073429, -.16105588, .0195873, -.1894624, .04799383,
  2, "r1", -.22524207, -.32515033, -.12533381, -.35660727, -.09387687,
  2, "r1", -.04763515, -.13529482, .04002452, -.16284828, .06757798,
  3, "r1", .17724128, .14564874, .20883382, .13571595, .21876661,
  3, "r1", .0227335, -.02484371, .07031071, -.03980373, .08527073,
  3, "r1", .20034042, .17294058, .22774026, .16432832, .23635252,
  4, "r1", .05399466, .02374048, .08424884, .01422817, .09376114,
  4, "r1", -.10051312, -.14509343, -.05593282, -.15911232, -.04191393,
  4, "r1", .07709379, .05535747, .09883011, .04852539, .1056622,
  5, "r1", -.04411435, -.07329345, -.01493526, -.08246586, -.00576284,
  5, "r1", -.19862213, -.24258413, -.15466013, -.25640393, -.14084034,
  5, "r1", -.02101521, -.03211431, -.00991612, -.03560284, -.00642758
)


# Apply the function to update Coefficient values
data$Coefficient <- sapply(1:nrow(data), getNewCoefficient1)

# Rename groups
data$Group <- factor(data$Group, levels = c(1, 2, 3, 4, 5), labels = c("FWF", "HHMI", "NCN", "NWO", "UKRI"))


ggplot(data, aes(x = as.factor(1:nrow(data)), y = Estimate, color = factor(Group))) +
  geom_point(position = position_dodge(width = 0.5), size = 2) +
  geom_errorbar(aes(ymin = CI_Lower_99, ymax = CI_Upper_99), 
                position = position_dodge(width = 0.5), 
                width = 0.3,  linewidth = .8, alpha = 0.5) +  # Set alpha for 99% CI
  geom_errorbar(aes(ymin = CI_Lower_95, ymax = CI_Upper_95), 
                position = position_dodge(width = 0.5), 
                width = 0.2, linewidth = .9) +  # 95% CI with full opacity
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Pairwise Comparison with 'Non Plan S' Funders",
    subtitle = "Only Hybrid (Gold) Open Access",
    x = "Compared to 'Non Plan S' funders",
    y = "Point Coefficients",
    color = "Plan S Funder"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(labels = data$Coefficient)
ggsave(filename = "pairwise_comp_hybridoa_control.png", plot = last_plot(), path = "", scale = 1, dpi = 600)

## Comparison Group

data <- tribble(
  ~Group, ~Coefficient, ~Estimate, ~CI_Lower_95, ~CI_Upper_95, ~CI_Lower_99, ~CI_Upper_99,
  1, "r1", -.17374187, -.20082755, -.1466562, -.20934212, -.13814163,
  1, "r1", .07758853, .04830459, .10687246, .03909655, .1160805,
  1, "r1", .28999598, .22682327, .3531687, .20695864, .37303333,
  2, "r1", -.30986118, -.39807048, -.22165188, -.42580022, -.19392214,
  2, "r1", -.05853078, -.1484142, .03135264, -.17668481, .05962325,
  2, "r1", .15387668, .06945356, .23829979, .04289041, .26486294,
  3, "r1", -.06188561, -.09388488, -.02988634, -.10394381, -.01982742,
  3, "r1", .18944479, .15905729, .21983229, .14950334, .22938624,
  3, "r1", .40185225, .33469703, .46900746, .31358305, .49012144,
  4, "r1", -.18513224, -.21316321, -.15710126, -.22197481, -.14828966,
  4, "r1", .06619816, .03738763, .0950087, .02832881, .10406752,
  4, "r1", .27860562, .21427236, .34293888, .19404415, .36316709,
  5, "r1", -.28324124, -.30979021, -.25669228, -.31813538, -.24834711,
  5, "r1", -.03191084, -.06078338, -.0030383, -.06985954, .00603785,
  5, "r1", .18049661, .11868712, .24230611, .09925735, .26173588
)


# Apply the function to update Coefficient values
data$Coefficient <- sapply(1:nrow(data), getNewCoefficient2)

# Rename groups
data$Group <- factor(data$Group, levels = c(1, 2, 3, 4, 5), labels = c("FWF", "HHMI", "NCN", "NWO", "UKRI"))

# Plot coefficients with confidence intervals, colored by groups
ggplot(data, aes(x = as.factor(1:nrow(data)), y = Estimate, color = factor(Group))) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_point(position = position_dodge(width = 0.5), size = 2) +
  geom_errorbar(aes(ymin = CI_Lower_99, ymax = CI_Upper_99), 
                position = position_dodge(width = 0.5), 
                width = 0.3,  linewidth = .8, alpha = 0.5) +  # Set alpha for 99% CI
  geom_errorbar(aes(ymin = CI_Lower_95, ymax = CI_Upper_95), 
                position = position_dodge(width = 0.5), 
                width = 0.2, linewidth = .9) +  # 95% CI with full opacity
  labs(
    title = "Pairwise Comparison with 'OA focused' Funders",
    subtitle = "Only Hybrid (Gold) Open Access",
    x = "Compared to 'Non Plan S but OA focused' funders",
    y = "Point Coefficients",
    color = "Plan S Funder"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(labels = data$Coefficient)
ggsave(filename = "pairwise_comparison_hybridoa_comp.png", plot = last_plot(), path = "", scale = 1, dpi = 600)


#
### GREEN OA only ----
#

# Control group

data <- tribble(
  ~Group, ~Coefficient, ~Estimate, ~CI_Lower_95, ~CI_Upper_95, ~CI_Lower_99, ~CI_Upper_99,
  1, "r1", .09044481, .03518583, .14570379, .01780947, .16308015,
  1, "r1", .03626783, -.01283886, .08537451, -.02828734, .10082299,
  1, "r1", .05462808, .01689791, .09235824, .00503848, .10421767,
  2, "r1", -.23011647, -.31141077, -.14882216, -.33697748, -.12325545,
  2, "r1", -.28429345, -.35604097, -.21254593, -.3786313, -.1899556,
  2, "r1", -.2659332, -.33043348, -.20143292, -.35070746, -.18115894,
  3, "r1", .10027836, .05308443, .1474723, .03824598, .16231074,
  3, "r1", .04610138, .00819941, .08400334, -.00371939, .09592215,
  3, "r1", .06446163, .04150291, .08742034, .03428654, .09463672,
  4, "r1", .02563862, -.02053029, .07180753, -.03504763, .08632487,
  4, "r1", -.02853836, -.0754053, .01832858, -.09014739, .03307067,
  4, "r1", -.01017811, -.04600621, .02564999, -.05726773, .03691151,
  5, "r1", -.17993496, -.21768327, -.14218664, -.22954979, -.13032012,
  5, "r1", -.23411194, -.27968106, -.18854281, -.29400677, -.17421711,
  5, "r1", -.21575169, -.25124383, -.18025955, -.2623994, -.16910398
)


# Apply the function to update Coefficient values
data$Coefficient <- sapply(1:nrow(data), getNewCoefficient1)

# Rename groups
data$Group <- factor(data$Group, levels = c(1, 2, 3, 4, 5), labels = c("FWF", "HHMI", "NCN", "NWO", "UKRI"))


ggplot(data, aes(x = as.factor(1:nrow(data)), y = Estimate, color = factor(Group))) +
  geom_point(position = position_dodge(width = 0.5), size = 2) +
  geom_errorbar(aes(ymin = CI_Lower_99, ymax = CI_Upper_99), 
                position = position_dodge(width = 0.5), 
                width = 0.3,  linewidth = .8, alpha = 0.5) +  # Set alpha for 99% CI
  geom_errorbar(aes(ymin = CI_Lower_95, ymax = CI_Upper_95), 
                position = position_dodge(width = 0.5), 
                width = 0.2, linewidth = .9) +  # 95% CI with full opacity
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Pairwise Comparison with 'Non Plan S' Funders",
    subtitle = "Only Green Open Access",
    x = "Compared to 'Non Plan S' funders",
    y = "Point Coefficients",
    color = "Plan S Funder"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(labels = data$Coefficient)
ggsave(filename = "pairwise_comp_greenoa_control.png", plot = last_plot(), path = "", scale = 1, dpi = 600)

## Comparison Group

data <- tribble(
  ~Group, ~Coefficient, ~Estimate, ~CI_Lower_95, ~CI_Upper_95, ~CI_Lower_99, ~CI_Upper_99,
  1, "r1", -.01384822, -.05079214, .02309569, -.06240615, .0347097,
  1, "r1", .1701731, .13058654, .20975967, .11813872, .22220748,
  1, "r1", .36480242, .32207344, .4075314, .30863662, .42096822,
  2, "r1", -.3344095, -.40441678, -.26440222, -.42642515, -.24239385,
  2, "r1", -.15038817, -.21829578, -.08248057, -.23965124, -.06112511,
  2, "r1", .04424114, -.02360431, .11208659, -.04494629, .13342857,
  3, "r1", -.00401467, -.03431015, .02628081, -.04383378, .03580444,
  3, "r1", .18000665, .15045235, .20956096, .1411603, .21885301,
  3, "r1", .37463597, .34337034, .4059016, .33354023, .41573171,
  4, "r1", -.07865441, -.11291418, -.04439464, -.12368431, -.03362451,
  4, "r1", .10536691, .07027768, .14045615, .05924416, .15148967,
  4, "r1", .29999623, .25920744, .34078502, .24638129, .35361117,
  5, "r1", -.28422799, -.31996673, -.24848924, -.33120081, -.23725516,
  5, "r1", -.10020666, -.13765067, -.06276266, -.14942157, -.05099176,
  5, "r1", .09442265, .05320214, .13564316, .04024425, .14860105
)

# Apply the function to update Coefficient values
data$Coefficient <- sapply(1:nrow(data), getNewCoefficient2)

# Rename groups
data$Group <- factor(data$Group, levels = c(1, 2, 3, 4, 5), labels = c("FWF", "HHMI", "NCN", "NWO", "UKRI"))

# Plot coefficients with confidence intervals, colored by groups
ggplot(data, aes(x = as.factor(1:nrow(data)), y = Estimate, color = factor(Group))) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_point(position = position_dodge(width = 0.5), size = 2) +
  geom_errorbar(aes(ymin = CI_Lower_99, ymax = CI_Upper_99), 
                position = position_dodge(width = 0.5), 
                width = 0.3,  linewidth = .8, alpha = 0.5) +  # Set alpha for 99% CI
  geom_errorbar(aes(ymin = CI_Lower_95, ymax = CI_Upper_95), 
                position = position_dodge(width = 0.5), 
                width = 0.2, linewidth = .9) +  # 95% CI with full opacity
  labs(
    title = "Pairwise Comparison with 'OA focused' Funders",
    subtitle = "Only Green Open Access",
    x = "Compared to 'Non Plan S but OA focused' funders",
    y = "Point Coefficients",
    color = "Plan S Funder"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(labels = data$Coefficient)
ggsave(filename = "pairwise_comparison_greenoa_comp.png", plot = last_plot(), path = "", scale = 1, dpi = 600)

################################################################################

#### Pooled regressions WITHIN OA
# relative change of one OA type relative to the others
# >> excluding restricted access publications

# gold - hybrid - green

data <- tribble(
  ~Group, ~Coefficient, ~Estimate, ~CI_Lower_95, ~CI_Upper_95, ~CI_Lower_99, ~CI_Upper_99,
  1, "r1",  -.02625636,  -.07810204,   .02558933,  -.09439685,   .04188414,
  1, "r1",  -.00895955,  -.03323397,   .01531488,  -.04086343,   .02294434,
  2, "r1",   .10808898,   .07952807,   .13664989,   .07055153,   .14562643,
  2, "r1",  -.02817421,    -.049079,  -.00726942,  -.05564939, -.00069903,
  3, "r1",	-.08527349,	-.10717778,	-.06336921,	-.11406217,	-.05648481,
  3, "r1",	-.00384508,	-.01990132,	.01221117,	-.02494781,	.01725765
)


# Rename groups
data$Group <- factor(data$Group, levels = c(1, 2, 3), labels = c("Gold", "Hybrid", "Green"))

# 1 Control Group
getNewCoefficient3 <- function(row) {
  if (row %% 2 == 1) {
    return("Control")
  } else {
    return("Comparison")
  }
}

data$Coefficient <- sapply(1:nrow(data), getNewCoefficient3)

ggplot(data, aes(x = as.factor(1:nrow(data)), y = Estimate, color = factor(Group))) +
  geom_point(position = position_dodge(width = 0.5), size = 2) +
  geom_errorbar(aes(ymin = CI_Lower_99, ymax = CI_Upper_99), 
                position = position_dodge(width = 0.5), 
                width = 0.3,  linewidth = .8, alpha = 0.5) +  # Set alpha for 99% CI
  geom_errorbar(aes(ymin = CI_Lower_95, ymax = CI_Upper_95), 
                position = position_dodge(width = 0.5), 
                width = 0.2, linewidth = .9) +  # 95% CI with full opacity
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Pooled Comparison Among Open Access Flavors",
    subtitle = "Change among Plan S funders relative to others (without restricted access)",
   x = "",
    y = "Point Coefficients",
    color = "Relative to:"
  ) +
  theme_bw() +  ylim(-0.155, 0.155) +
  theme(legend.position = "bottom") +
  scale_x_discrete(labels = data$Coefficient)
ggsave(filename = "pooled_oa_flavors.png", plot = last_plot(), path = "", scale = 1, dpi = 600)

###############################################################################


#     E N D   O F   S C R I P T     

