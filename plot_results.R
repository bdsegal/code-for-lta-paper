# ACL Latent Transition Analysis
# Make plots (run process_results.R first)

library(reshape2)
library(ggplot2)
library(gtools)

load("post_process_final.RData")

# item response probabilities -------------------------------------------------
itemRespMaleQuants <- t(apply(itemRespMaleBoot, 1, quantile, c(0.025, 0.975),
                              type = 6))
colnames(irpSingMale) <- c("group", "variable", "response", "status", "value")

# make sure variables, response, and status aligned
cbind(irpMale[, 2:4], irpSingMale[, 2:4])

itemRespMaleAll <- irpSingMale
itemRespMaleAll$lower = pmax(itemRespMaleQuants[, 1], 0)
itemRespMaleAll$upper = pmin(itemRespMaleQuants[, 2], 1)

colnames(itemRespMaleAll) <- c("group","variable", "response", "status",
                               "value", "lower", "upper")
itemRespMaleAll$status <- paste("Profile", itemRespMaleAll$status)

unique(itemRespMaleAll$status)

irSub <- itemRespMaleAll[which(!itemRespMaleAll$status %in% "Profile 6" &
                               !itemRespMaleAll$variable %in% "Dead" &
                               !itemRespMaleAll$response %in% "Dead (BMI)" &
                               !itemRespMaleAll$response %in% "Dead (smoking)" &
                               !itemRespMaleAll$response %in% "Dead (drinking)"), ]
dev.new(height = 7.5, width = 7)
ggplot(aes(x = response, y = value), data = irSub) +
  geom_bar(stat = "identity")+
  geom_errorbar(aes(ymin = lower, ymax = upper), color = "red")+
  facet_grid(status ~ variable, scale = "free")+
  theme_bw(16)+
  scale_y_continuous(lim = c(0, 1), breaks = c(0, 0.5, 1))+
  labs(y = "Item-response probability", x = "", title = "Males")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        plot.title = element_text(hjust = 0.5))
ggsave("plots/itemResp_group_male_boot_95.png")

itemRespFemaleQuants <- t(apply(itemRespFemaleBoot, 1, quantile, c(0.025, 0.975),
                          type = 6))
colnames(irpSingFemale) <- c("group", "variable", "response", "status", "value")

# make sure variables, response, and status aligned
cbind(irpFemale[, 2:4], irpSingFemale[, 2:4])

itemRespFemaleAll <- irpSingFemale
itemRespFemaleAll$lower = pmax(itemRespFemaleQuants[, 1], 0)
itemRespFemaleAll$upper = pmin(itemRespFemaleQuants[, 2], 1)

colnames(itemRespFemaleAll) <- c("group","variable", "response", "status",
                                 "value", "lower", "upper")
itemRespFemaleAll$status <- paste("Profile", itemRespFemaleAll$status)


irSub <- itemRespFemaleAll[which(!itemRespFemaleAll$status %in% "Profile 6" &
                               !itemRespFemaleAll$variable %in% "Dead" &
                               !itemRespFemaleAll$response %in% "Dead (BMI)" &
                               !itemRespFemaleAll$response %in% "Dead (smoking)" &
                               !itemRespFemaleAll$response %in% "Dead (drinking)"), ]

dev.new(height = 7.5, width = 7)
ggplot(aes(x = response, y = value), data = irSub)+
  geom_bar(stat = "identity")+
  geom_errorbar(aes(ymin = lower, ymax = upper), color = "red")+
  facet_grid(status ~ variable, scale = "free")+
  theme_bw(16)+
  scale_y_continuous(lim = c(0, 1), breaks = c(0, 0.5, 1))+
  labs(y = "Item-response probability", x = "", title = "Females")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        plot.title = element_text(hjust = 0.5))
ggsave("plots/itemResp_group_female_boot_95.png")

# difference
itemRespDiffAll <- itemRespFemaleAll[, c("variable", "response", "status")]
itemRespDiffQuants <- t(apply(itemRespMaleBoot - itemRespFemaleBoot, 1,
                              quantile, c(0.025, 0.975), type = 6))

itemRespDiffAll$value <- irpSingMale$value - irpSingFemale$value
itemRespDiffAll$lower = itemRespDiffQuants[, 1]
itemRespDiffAll$upper = itemRespDiffQuants[, 2]


irSub <- itemRespDiffAll[which(!itemRespDiffAll$status %in% "Profile 6" &
                               !itemRespDiffAll$variable %in% "Dead" &
                               !itemRespDiffAll$response %in% "Dead (BMI)" &
                               !itemRespDiffAll$response %in% "Dead (smoking)" &
                               !itemRespDiffAll$response %in% "Dead (drinking)"), ]

dev.new(height = 7.5, width = 7)
ggplot(aes(x = response, y = value), data = irSub)+
  geom_point(stat = "identity", size = 2)+
  geom_segment(aes(y = lower, yend = upper, x = response, xend = response),
               color = "black")+
  facet_grid(status ~ variable, scale = "free")+
  theme_bw(16)+
  geom_hline(yintercept = 0, linetype = "dashed")+
  labs(y = "Item-response probability (males - females)", x = "",
       title = "Males - Females")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        plot.title = element_text(hjust = 0.5))
ggsave("plots/itemResp_group_diff_boot_95.png")

# transition probabilities ----------------------------------------------------
tranProbMaleQuants <- t(apply(tranProbMaleBoot, 1, quantile, c(0.025, 0.975),
                        type = 6))

# cbind(tpMaleNewM[,c("facet", "from", "to")], tpSingMaleNewM[, c("facet", "from", "to")])

tranProbMaleAll <- tpSingMaleNewM
tranProbMaleAll$lower = pmax(tranProbMaleQuants[, 1], 0)
tranProbMaleAll$upper = pmin(tranProbMaleQuants[, 2], 1)

tranProbMaleAll$from <- ordered(tranProbMaleAll$from,
                                levels = rev(levels(tranProbMaleAll$from)))
tranProbMaleAll <- tranProbMaleAll[with(tranProbMaleAll, order(from, to, facet)), ]
tranProbMaleAll$fromTo <- with(tranProbMaleAll, paste("Profile", from, "to", to))
tranProbMaleAll$fromTo <- ordered(tranProbMaleAll$fromTo)
tranProbMaleAll$fromTo <- ordered(tranProbMaleAll$fromTo,
                                  levels = rev(levels(tranProbMaleAll$fromTo)))

dev.new(width = 14, height = 18)
ggplot(aes(x = fromTo, xend = fromTo, y = lower, yend = upper),
       data= tranProbMaleAll)+
  geom_segment()+
  geom_point(aes(x = fromTo, y = value), size = 2)+
  coord_flip()+
  geom_vline(xintercept = seq(6, 30, 6) + 0.5, linetype = "dashed")+
  facet_wrap(~facet)+
  labs(x = "", y = "", title = "Males")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))
ggsave("plots/tranProb_group_male_boot_95.png")
dev.off()

# females
tranProbFemaleQuants <- t(apply(tranProbFemaleBoot, 1, quantile,
                                c(0.025, 0.975), type = 6))

# check to make sure alignment is correct
cbind(tpFemaleNewM[,c("facet", "from", "to")],
      tpSingFemaleNewM[, c("facet", "from", "to")])

tranProbFemaleAll <- tpSingFemaleNewM
tranProbFemaleAll$lower = pmax(tranProbFemaleQuants[, 1], 0)
tranProbFemaleAll$upper = pmin(tranProbFemaleQuants[, 2], 1)

tranProbFemaleAll$from <- ordered(tranProbFemaleAll$from,
                                  levels = rev(levels(tranProbFemaleAll$from)))
tranProbFemaleAll <- tranProbFemaleAll[with(tranProbFemaleAll,
                                            order(from, to, facet)), ]
tranProbFemaleAll$fromTo <- with(tranProbFemaleAll,
                                 paste("Profile", from, "to", to))
tranProbFemaleAll$fromTo <- ordered(tranProbFemaleAll$fromTo)
tranProbFemaleAll$fromTo <- ordered(tranProbFemaleAll$fromTo,
                                    levels = rev(levels(tranProbFemaleAll$fromTo)))

dev.new(width = 14, height = 18)
ggplot(aes(x = fromTo, xend = fromTo, y = lower, yend = upper),
       data= tranProbFemaleAll)+
  geom_segment()+
  geom_point(aes(x = fromTo, y = value), size = 2)+
  coord_flip()+
  geom_vline(xintercept = seq(6, 30, 6) + 0.5, linetype = "dashed")+
  facet_wrap(~facet)+
  labs(x = "", y = "", title = "Females")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))
ggsave("plots/tranProb_group_female_boot_95.png")
dev.off()

# males - females

tranProbDiffAll <- tpSingFemaleNewM[, c("wave", "from", "to", "facet")]
tranProbDiffAll$value <- tpSingMaleNewM$value - tpSingFemaleNewM$value
tranProbDiffQuants <- t(apply(tranProbMaleBoot - tranProbFemaleBoot, 1, 
                              quantile, c(0.025, 0.975), type = 6))
tranProbDiffAll$lower = tranProbDiffQuants[, 1]
tranProbDiffAll$upper = tranProbDiffQuants[, 2]

tranProbDiffAll$from <- ordered(tranProbDiffAll$from,
                                levels = rev(levels(tranProbDiffAll$from)))
tranProbDiffAll <- tranProbDiffAll[with(tranProbDiffAll,
                                        order(from, to, facet)), ]
tranProbDiffAll$fromTo <- with(tranProbDiffAll, paste("Profile", from, "to", to))
tranProbDiffAll$fromTo <- ordered(tranProbDiffAll$fromTo)
tranProbDiffAll$fromTo <- ordered(tranProbDiffAll$fromTo,
                                  levels = rev(levels(tranProbDiffAll$fromTo)))

dev.new(width = 14, height = 18)
ggplot(aes(x = fromTo, xend = fromTo, y = lower, yend = upper),
       data = tranProbDiffAll)+
  geom_segment()+
  geom_point(aes(x = fromTo, y = value), size = 2)+
  coord_flip()+
  geom_vline(xintercept = seq(6, 30, 6) + 0.5, linetype = "dashed")+
  geom_hline(yintercept = 0, linetype = "dashed")+
  facet_wrap(~facet)+
  labs(x = "", y = "", title = "Males - Females")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))
ggsave("plots/tranProb_group_diff_boot_95.png")
dev.off()

# heatmap version of average transition probs
tranProbMaleAll$fromPlot <- factor(tranProbMaleAll$from,
                                   levels = rev(levels(tranProbMaleAll$from)))
sub <- tranProbMaleAll[tranProbMaleAll$fromPlot != 6, ]

ggplot(aes(x = to, y = fromPlot, fill = value), data= sub)+
  geom_tile()+
  facet_wrap(~facet)+
  scale_fill_continuous("Prob", low = "white", high = "gray30")+
  labs(x = "To profile", y = "From profile", title = "Males")+
  theme_bw(18)+
  theme(plot.title = element_text(hjust = 0.5))
ggsave("plots/tranProb_group_male_boot_mean.png")

tranProbFemaleAll$fromPlot <- factor(tranProbFemaleAll$from,
                                     levels = rev(levels(tranProbFemaleAll$from)))
sub <- tranProbFemaleAll[tranProbFemaleAll$fromPlot != 6, ]

ggplot(aes(x = to, y = fromPlot, fill = value), data= sub)+
  geom_tile()+
  facet_wrap(~facet)+
  scale_fill_continuous("Prob", low = "white", high = "gray30")+
  labs(x = "To profile", y = "From profile", title = "Females")+
  theme_bw(18)+
  theme(plot.title = element_text(hjust = 0.5))
ggsave("plots/tranProb_group_female_boot_mean.png")

# male - female
tranProbDiffAll$fromPlot <- factor(tranProbDiffAll$from,
                                   levels = rev(levels(tranProbDiffAll$from)))
tranProbDiffAll$signif <- with(tranProbDiffAll, (upper*lower > 0))

# note: want reverse level for from scale (numeric value)
tranProbDiffAll$toSig <- as.numeric(levels(tranProbDiffAll$to)[tranProbDiffAll$to])
tranProbDiffAll$fromPlotSig <- as.numeric(rev(levels(tranProbDiffAll$fromPlot))[tranProbDiffAll$fromPlot])

ggplot(aes(x = to, y = fromPlot, fill = value), data = tranProbDiffAll)+
  geom_tile()+
  facet_wrap(~facet)+
  scale_fill_gradient2("Prob", high = "firebrick", mid = "white", low = "navy blue")+
  labs(x = "To profile", y = "From profile", title = "Males - Females")+
  theme_bw(18)+
  geom_text(aes(x = toSig + 0.15, y = fromPlotSig + 0.05), 
           data = tranProbDiffAll[tranProbDiffAll$signif,], label = "*", size = 8)+
  theme(plot.title = element_text(hjust = 0.5))
ggsave("plots/tranProb_group_diff_boot_mean.png")

#Gray scale version
tranProbDiffAll$direction <- NA
for (i in 1:nrow(tranProbDiffAll)) {
  if(tranProbDiffAll$value[i] == 0) {
    tranProbDiffAll$direction[i] <- ""
  } else if (tranProbDiffAll$value[i] > 0) {
    tranProbDiffAll$direction[i] <- "M"
  } else if (tranProbDiffAll$value[i] < 0) {
    tranProbDiffAll$direction[i] <- "F"
  }
}

sub <- tranProbDiffAll[tranProbDiffAll$fromPlot != 6, ]
ggplot(aes(x = to, y = fromPlot, fill = abs(value)), data = sub)+
  geom_tile()+
  facet_wrap(~facet)+
  scale_fill_gradient("Prob", high = "gray30", low = "white")+
  labs(x = "To profile", y = "From profile", title = "Difference between males and females")+
  theme_bw(18)+
  geom_text(aes(x = toSig + 0.15, y = fromPlotSig + 0.05 - 1), 
           data = sub[sub$signif,], label = "*", size = 8)+
  geom_text(aes(x = toSig - 0.15, y = fromPlotSig - 0.05 - 1, label = direction),
           data = sub, size = 5)+
  theme(plot.title = element_text(hjust = 0.5))
ggsave("plots/tranProb_group_diff_boot_mean_bw_nonZero.png")

ggplot(aes(x = to, y = fromPlot, fill = abs(value)), data = sub)+
  geom_tile()+
  facet_wrap(~facet)+
  scale_fill_gradient("Prob", high = "gray30", low = "white")+
  labs(x = "To profile", y = "From profile", title = "Difference between males and females")+
  theme_bw(18)+
  geom_text(aes(x = toSig + 0.15, y = fromPlotSig + 0.05 - 1), 
           data = sub[sub$signif,], label = "*", size = 8)+
  geom_text(aes(x = toSig - 0.15, y = fromPlotSig - 0.05 - 1, label = direction),
           data = sub[sub$direction == "F", ], size = 5)+
  theme(plot.title = element_text(hjust = 0.5))
ggsave("plots/tranProb_group_diff_boot_mean_bw_f_nonZero.png")

# plot initial probabilities and parameter estimates beta ---------------------

initProbMaleQuants <- t(apply(initProbMaleBoot, 1, quantile,
                              c(0.025, 0.975), type = 6))
initMalePoint <- do.call(c, initSingMaleNew[1, -(1:2)])
initProbMaleAll <- data.frame(status = as.numeric(names(initMalePoint)),
                              value = initMalePoint)                         
initProbMaleAll$lower <- pmax(initProbMaleQuants[, 1], 0)
initProbMaleAll$upper <- pmin(initProbMaleQuants[, 2], 1)

ggplot(aes(x = status, y = value),
       data = initProbMaleAll[which(initProbMaleAll$status != 6), ])+
  geom_bar(stat = "identity")+
  geom_errorbar(aes(ymin = lower, ymax = upper), color = "red")+
  theme_bw(18)+
  scale_y_continuous(lim = c(0, 1))+
  scale_x_continuous(breaks = 1:6)+
  labs(y = "Initial membership probability", x = "Profile", title = "Males")+
  theme(plot.title = element_text(hjust = 0.5))
ggsave("plots/initProbBoot_group_male_95.png")

initProbFemaleQuants <- t(apply(initProbFemaleBoot, 1, quantile,
                                c(0.025, 0.975), type = 6))
initFemalePoint <- do.call(c, initSingFemaleNew[1, -(1:2)])
initProbFemaleAll <- data.frame(status = as.numeric(names(initFemalePoint)),
                              value = initFemalePoint)                         
initProbFemaleAll$lower <- pmax(initProbFemaleQuants[, 1], 0)
initProbFemaleAll$upper <- pmin(initProbFemaleQuants[, 2], 1)

ggplot(aes(x = status, y = value), 
       data = initProbFemaleAll[which(initProbFemaleAll$status != 6), ])+
  geom_bar(stat = "identity")+
  geom_errorbar(aes(ymin = lower, ymax = upper), color = "red")+
  theme_bw(18)+
  scale_y_continuous(lim = c(0, 1))+
  scale_x_continuous(breaks = 1:6)+
  labs(y = "Initial membership probability", x = "Profile", title = "Females")+
  theme(plot.title = element_text(hjust = 0.5))
ggsave("plots/initProbBoot_group_female_95.png")

initProbDiffQuants <- t(apply(initProbMaleBoot - initProbFemaleBoot, 1,
                              quantile, c(0.025, 0.975), type = 6))
initProbDiffAll <- data.frame(status = initProbFemaleAll$status )
initProbDiffAll$value <- initMalePoint - initFemalePoint
initProbDiffAll$lower <- initProbDiffQuants[, 1]
initProbDiffAll$upper <- initProbDiffQuants[, 2]

ggplot(aes(x = status, y = value), 
       data = initProbDiffAll[which(initProbDiffAll$status != 6), ])+
  geom_point(stat = "identity", size = 2)+
  geom_segment(aes(y = lower, yend = upper, x = status, xend = status))+
  theme_bw(18)+
  scale_x_continuous(breaks = 1:6)+
  geom_hline(yintercept = 0, linetype = "dashed")+
  labs(y = "Initial membership probability (males - females)", x = "Profile",
       title = "Males - Females")+
  theme(plot.title = element_text(hjust = 0.5))
ggsave("plots/initProbBoot_group_diff_95.png")

# parameter estimates
initBetaMaleQuants <- t(apply(initBetaMaleBoot, 1, quantile, c(0.025, 0.975),
                              type = 6))
colnames(betaSingMaleM) <- c("covariate", "status", "value")
initBetaMaleAll <- betaSingMaleM
initBetaMaleAll$lower <- initBetaMaleQuants[, 1]
initBetaMaleAll$upper <- initBetaMaleQuants[, 2]

initBetaMaleAll[which(initBetaMaleAll$covariate == "age1Centered"), -(1:2)] <- 
  initBetaMaleAll[which(initBetaMaleAll$covariate == "age1Centered"), -(1:2)] * 10

initBetaMaleAll$covariate <- droplevels(initBetaMaleAll$covariate)
levs <- levels(initBetaMaleAll$covariate)
levels(initBetaMaleAll$covariate) <- c("Reference", "Education", "Black",
                                       "Age (per 10 years)")
  
ggplot(aes(x = status, xend = status, y = lower, yend = upper), 
       data= initBetaMaleAll[which(initBetaMaleAll$status != 6), ])+
  geom_segment()+
  geom_point(aes(x = status, y = value), size = 2)+
  facet_wrap(~covariate)+
  labs(x = "Profile", y = expression(beta), title = "Males")+
  scale_x_discrete(breaks = 1:6)+
  geom_hline(yintercept = 0, linetype = "dashed")+
  theme_bw(18)+
  theme(plot.title = element_text(hjust = 0.5))
ggsave("plots/betaBoot_group_male_95.png")

initBetaFemaleQuants <- t(apply(initBetaFemaleBoot, 1, quantile, c(0.025, 0.975),
                                type = 6))
colnames(betaSingFemaleM) <- c("covariate", "status", "value")
initBetaFemaleAll <- betaSingFemaleM
initBetaFemaleAll$lower <- initBetaFemaleQuants[, 1]
initBetaFemaleAll$upper <- initBetaFemaleQuants[, 2]

initBetaFemaleAll[which(initBetaFemaleAll$covariate == "age1Centered"), -(1:2)] <- 
  initBetaFemaleAll[which(initBetaFemaleAll$covariate == "age1Centered"), -(1:2)] * 10

initBetaFemaleAll$covariate <- droplevels(initBetaFemaleAll$covariate)
levs <- levels(initBetaFemaleAll$covariate)
levels(initBetaFemaleAll$covariate) <- c("Reference", "Education", "Black",
                                         "Age (per 10 years)")
  
ggplot(aes(x = status, xend = status, y = lower, yend = upper), 
       data= initBetaFemaleAll[which(initBetaFemaleAll$status != 6), ])+
  geom_segment()+
  geom_point(aes(x = status, y = value), size = 2)+
  facet_wrap(~covariate)+
  labs(x = "Profile", y = expression(beta), title = "Females")+
  scale_x_discrete(breaks = 1:6)+
  geom_hline(yintercept = 0, linetype = "dashed")+
  theme_bw(18)+
  theme(plot.title = element_text(hjust = 0.5))
ggsave("plots/betaBoot_group_female_95.png")

initBetaDiffQuants <- t(apply(initBetaMaleBoot - initBetaFemaleBoot, 1,
                                quantile, c(0.025, 0.975), type = 6))
initBetaDiffAll <- betaSingFemaleM[, c("covariate", "status")]
initBetaDiffAll$value <- betaSingMaleM$value - betaSingFemaleM$value
initBetaDiffAll$lower <- initBetaDiffQuants[, 1]
initBetaDiffAll$upper <- initBetaDiffQuants[, 2]

initBetaDiffAll[which(initBetaDiffAll$covariate == "age1Centered"), -(1:2)] <- 
  initBetaDiffAll[which(initBetaDiffAll$covariate == "age1Centered"), -(1:2)] * 10
  
initBetaDiffAll$covariate <- droplevels(initBetaDiffAll$covariate)
levs <- levels(initBetaDiffAll$covariate)
levels(initBetaDiffAll$covariate) <- c("Reference", "Education", "Black",
                                         "Age (per 10 years)")

ggplot(aes(x = status, xend = status, y = lower, yend = upper), 
       data= initBetaDiffAll[which(initBetaDiffAll$status != 6), ])+
  geom_segment()+
  geom_point(aes(x = status, y = value), size = 2)+
  facet_wrap(~covariate)+
  labs(x = "Profile", y = expression(beta), title = "Males - Females")+
  scale_x_discrete(breaks = 1:6)+
  geom_hline(yintercept = 0, linetype = "dashed")+
  theme_bw(18)+
  theme(plot.title = element_text(hjust = 0.5))
ggsave("plots/betaBoot_group_diff_95.png")
