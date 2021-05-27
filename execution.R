### Preliminaries ###
library(ggplot2)
library(ggmap)
library(osmdata)
library(scales)
library(readxl)
library(tidyverse)
library(gtools)
library(stringdist)
colours <- c("blue", "orange", "green", "cyan")



### Summary Statistics ###
apply(data$output_f, 2, function(x) 3958-sum(is.na(x)))
apply(data$output_f, 2, function(x) max(na.omit(x)))
apply(data$output_f, 2, function(x) min(na.omit(x)))
apply(data$output_f, 2, function(x) mean(as.numeric(x), na.rm = TRUE))
apply(data$output_f, 2, function(x) sd(na.omit(x)))

apply(data$output_h, 2, function(x) 8109-sum(is.na(x)))
apply(data$output_h, 2, function(x) max(na.omit(x)))
apply(data$output_h, 2, function(x) min(na.omit(x)))
apply(data$output_h, 2, function(x) mean(as.numeric(x), na.rm = TRUE))
apply(data$output_h, 2, function(x) sd(na.omit(x)))

apply(data$output_i, 2, function(x) 4287-sum(is.na(x)))
apply(data$output_i, 2, function(x) max(na.omit(x)))
apply(data$output_i, 2, function(x) min(na.omit(x)))
apply(data$output_i, 2, function(x) mean(as.numeric(x), na.rm = TRUE))
apply(data$output_i, 2, function(x) sd(na.omit(x)))

apply(data$output_p, 2, function(x) 2236-sum(is.na(x)))
apply(data$output_p, 2, function(x) max(na.omit(x)))
apply(data$output_p, 2, function(x) min(na.omit(x)))
apply(data$output_p, 2, function(x) mean(as.numeric(x), na.rm = TRUE))
apply(data$output_p, 2, function(x) sd(na.omit(x)))



### Short Typology Analysis ###
(table(data$output_f$PropertySubtype)*100/length(data$output_f$PropertySubtype))
(table(data$output_h$PropertySubtype)*100/length(data$output_h$PropertySubtype))
(table(data$output_i$PropertySubtype)*100/length(data$output_i$PropertySubtype))
(table(data$output_p$PropertySubtype)*100/length(data$output_p$PropertySubtype))

barplot(table(data$output_f$PropertySubtype)*100/length(data$output_f$PropertySubtype))
barplot(table(data$output_h$PropertySubtype)*100/length(data$output_h$PropertySubtype))
barplot(table(data$output_i$PropertySubtype)*100/length(data$output_i$PropertySubtype))
barplot(table(data$output_p$PropertySubtype)*100/length(data$output_p$PropertySubtype))



### Hypothesis Testing ###
# Kolmogorov-Smirnov #
# f with h #
ks.test(data$output_f$RentM + rnorm(length(data$output_f$RentM), sd = sd(data$output_f$RentM)/1000),
        data$output_h$RentM + rnorm(length(data$output_h$RentM), sd = sd(data$output_h$RentM)/1000))
# f with i #
ks.test(data$output_f$RentM + rnorm(length(data$output_f$RentM), sd = sd(data$output_f$RentM)/1000),
        data$output_i$RentM + rnorm(length(data$output_i$RentM), sd = sd(data$output_i$RentM)/1000))
# f with p #
ks.test(data$output_f$RentM + rnorm(length(data$output_f$RentM), sd = sd(data$output_f$RentM)/1000),
        data$output_p$RentM + rnorm(length(data$output_p$RentM), sd = sd(data$output_p$RentM)/1000))
# h with i #
ks.test(data$output_h$RentM + rnorm(length(data$output_h$RentM), sd = sd(data$output_h$RentM)/1000),
        data$output_i$RentM + rnorm(length(data$output_i$RentM), sd = sd(data$output_i$RentM)/1000))
# h with p #
ks.test(data$output_h$RentM + rnorm(length(data$output_h$RentM), sd = sd(data$output_h$RentM)/1000),
        data$output_p$RentM + rnorm(length(data$output_p$RentM), sd = sd(data$output_p$RentM)/1000))
# i with p #
ks.test(data$output_i$RentM + rnorm(length(data$output_i$RentM), sd = sd(data$output_i$RentM)/1000),
        data$output_p$RentM + rnorm(length(data$output_p$RentM), sd = sd(data$output_p$RentM)/1000))

# ANOVA testing #
datavec <- c(data$output_f$RentM,
             data$output_h$RentM,
             data$output_i$RentM,
             data$output_p$RentM)
group <- c(rep(0, length(data$output_f$RentM)),
           rep(1, length(data$output_h$RentM)),
           rep(2, length(data$output_i$RentM)),
           rep(3, length(data$output_p$RentM)))
anova <- aov(datavec ~ group)
summary(anova)

# t-testing #
# Data from https://ajuntament.barcelona.cat/estadistica/catala/Estadistiques_per_temes/Habitatge_i_mercat_immobiliari/Mercat_immobiliari/Habitatges_lloguer/evo/tllogem2.htm
t.test(data$output_f$RentM, mu = 14.3)
t.test(data$output_h$RentM, mu = 14.3)
t.test(data$output_i$RentM, mu = 14.3)
t.test(data$output_p$RentM, mu = 14.3)

t.test(data$output_f$Surface, mu = 73)
t.test(data$output_h$Surface, mu = 73)
t.test(data$output_i$Surface, mu = 73)
t.test(data$output_p$Surface, mu = 73)



### Density Estimation ###
# Monthly Rent Density #
minimum <- 300
maximum <- 4000
len <- 10000
h <- c(h_pi(data$output_h$RentPrice), h_pi(data$output_f$RentPrice),
       h_pi(data$output_p$RentPrice), h_pi(data$output_i$RentPrice))
grid <- seq(minimum, maximum, length.out = len)
dens_h <- dens_line(len, data$output_h$RentPrice, h[1], use.default.grid = FALSE, min = minimum, max = maximum)
dens_f <- dens_line(len, data$output_f$RentPrice, h[2], use.default.grid = FALSE, min = minimum, max = maximum)
dens_p <- dens_line(len, data$output_p$RentPrice, h[3], use.default.grid = FALSE, min = minimum, max = maximum)
dens_i <- dens_line(len, data$output_i$RentPrice, h[4], use.default.grid = FALSE, min = minimum, max = maximum)
data_dens <- data.frame(grid = grid, dens_h = dens_h$densities,
                        dens_f = dens_f$densities, dens_p = dens_p$densities,
                        dens_i = dens_i$densities)
pdf("rent.pdf", height = 4.5, width = 6.5)
ggplot(data_dens) +
  geom_line(aes(grid, dens_h, colour = "Habitaclia"), size = 1) +
  geom_line(aes(grid, dens_f, colour = "Fotocasa"), size = 1) +
  geom_line(aes(grid, dens_p, colour = "Pisos"), size = 1) +
  geom_line(aes(grid, dens_i, colour = "Idealista"), size = 1) + 
  scale_colour_manual(values = colours, name = "Dataset") +
  theme_light() +
  theme(legend.position = "left") +
  ggtitle("Density Distribution of Monthly Rent") +
  xlab("Monthly Rent, in Euros") + 
  ylab("Density")
dev.off()

# Monthly Rent Density, Undersmoothed #
minimum <- 300
maximum <- 4000
len <- 10000
h <- c(h_pi(data$output_h$RentPrice), h_pi(data$output_f$RentPrice),
       h_pi(data$output_p$RentPrice), h_pi(data$output_i$RentPrice))/4
grid <- seq(minimum, maximum, length.out = len)
dens_h <- dens_line(len, data$output_h$RentPrice, h[1], use.default.grid = FALSE, min = minimum, max = maximum)
dens_f <- dens_line(len, data$output_f$RentPrice, h[2], use.default.grid = FALSE, min = minimum, max = maximum)
dens_p <- dens_line(len, data$output_p$RentPrice, h[3], use.default.grid = FALSE, min = minimum, max = maximum)
dens_i <- dens_line(len, data$output_i$RentPrice, h[4], use.default.grid = FALSE, min = minimum, max = maximum)
data_dens <- data.frame(grid = grid, dens_h = dens_h$densities,
                        dens_f = dens_f$densities, dens_p = dens_p$densities,
                        dens_i = dens_i$densities)
pdf("rentu.pdf", height = 4.5, width = 6.5)
ggplot(data_dens) +
  geom_line(aes(grid, dens_h, colour = "Habitaclia"), size = 1) +
  geom_line(aes(grid, dens_f, colour = "Fotocasa"), size = 1) +
  geom_line(aes(grid, dens_p, colour = "Pisos"), size = 1) +
  geom_line(aes(grid, dens_i, colour = "Idealista"), size = 1) + 
  scale_colour_manual(values = colours, name = "Dataset") +
  theme_light() +
  theme(legend.position = "left") +
  ggtitle("Density Distribution of Monthly Rent", subtitle = "Undersmoothed") +
  xlab("Monthly Rent, in Euros") + 
  ylab("Density")
dev.off()

# Log Monthly Rent Density #
minimum <- log(300)
maximum <- 9
len <- 10000
h <- c(h_pi(log(data$output_h$RentPrice)), h_pi(log(data$output_f$RentPrice)),
       h_pi(log(data$output_p$RentPrice)), h_pi(log(data$output_i$RentPrice)))
grid <- seq(minimum, maximum, length.out = len)
dens_h <- dens_line(len, log(data$output_h$RentPrice), h[1], use.default.grid = FALSE, min = minimum, max = maximum)
dens_f <- dens_line(len, log(data$output_f$RentPrice), h[2], use.default.grid = FALSE, min = minimum, max = maximum)
dens_p <- dens_line(len, log(data$output_p$RentPrice), h[3], use.default.grid = FALSE, min = minimum, max = maximum)
dens_i <- dens_line(len, log(data$output_i$RentPrice), h[4], use.default.grid = FALSE, min = minimum, max = maximum)
data_dens <- data.frame(grid = grid, dens_h = dens_h$densities,
                        dens_f = dens_f$densities, dens_p = dens_p$densities,
                        dens_i = dens_i$densities)
pdf("rentl.pdf", height = 4.5, width = 6.5)
ggplot(data_dens) +
  geom_line(aes(grid, dens_h, colour = "Habitaclia"), size = 1) +
  geom_line(aes(grid, dens_f, colour = "Fotocasa"), size = 1) +
  geom_line(aes(grid, dens_p, colour = "Pisos"), size = 1) +
  geom_line(aes(grid, dens_i, colour = "Idealista"), size = 1) + 
  scale_colour_manual(values = colours, name = "Dataset") +
  theme_light() +
  theme(legend.position = "left") +
  ggtitle("Density Distribution of Log Monthly Rent") +
  xlab("Log Monthly Rent") + 
  ylab("Density")
dev.off()

# Log Monthly Rent Density, Undersmoothed #
minimum <- log(300)
maximum <- 9
len <- 10000
h <- c(h_pi(log(data$output_h$RentPrice)), h_pi(log(data$output_f$RentPrice)),
       h_pi(log(data$output_p$RentPrice)), h_pi(log(data$output_i$RentPrice)))/3
grid <- seq(minimum, maximum, length.out = len)
dens_h <- dens_line(len, log(data$output_h$RentPrice), h[1], use.default.grid = FALSE, min = minimum, max = maximum)
dens_f <- dens_line(len, log(data$output_f$RentPrice), h[2], use.default.grid = FALSE, min = minimum, max = maximum)
dens_p <- dens_line(len, log(data$output_p$RentPrice), h[3], use.default.grid = FALSE, min = minimum, max = maximum)
dens_i <- dens_line(len, log(data$output_i$RentPrice), h[4], use.default.grid = FALSE, min = minimum, max = maximum)
data_dens <- data.frame(grid = grid, dens_h = dens_h$densities,
                        dens_f = dens_f$densities, dens_p = dens_p$densities,
                        dens_i = dens_i$densities)
pdf("rentlu.pdf", height = 4.5, width = 6.5)
ggplot(data_dens) +
  geom_line(aes(grid, dens_h, colour = "Habitaclia"), size = 1) +
  geom_line(aes(grid, dens_f, colour = "Fotocasa"), size = 1) +
  geom_line(aes(grid, dens_p, colour = "Pisos"), size = 1) +
  geom_line(aes(grid, dens_i, colour = "Idealista"), size = 1) + 
  scale_colour_manual(values = colours, name = "Dataset") +
  theme_light() +
  theme(legend.position = "left") +
  ggtitle("Density Distribution of Log Monthly Rent", subtitle = "Undersmoothed") +
  xlab("Log Monthly Rent") + 
  ylab("Density")
dev.off()

# Monthly Rent per Square Meter Density #
minimum <- 5
maximum <- 50
len <- 10000
h <- c(h_pi(data$output_h$RentM), h_pi(data$output_f$RentM),
       h_pi(data$output_p$RentM), h_pi(data$output_i$RentM))
grid <- seq(minimum, maximum, length.out = len)
dens_h <- dens_line(len, data$output_h$RentM, h[1], use.default.grid = FALSE, min = minimum, max = maximum)
dens_f <- dens_line(len, data$output_f$RentM, h[2], use.default.grid = FALSE, min = minimum, max = maximum)
dens_p <- dens_line(len, data$output_p$RentM, h[3], use.default.grid = FALSE, min = minimum, max = maximum)
dens_i <- dens_line(len, data$output_i$RentM, h[4], use.default.grid = FALSE, min = minimum, max = maximum)
data_dens <- data.frame(grid = grid, dens_h = dens_h$densities,
                        dens_f = dens_f$densities, dens_p = dens_p$densities,
                        dens_i = dens_i$densities)
pdf("rentm.pdf", height = 4.5, width = 6.5)
ggplot(data_dens) +
  geom_line(aes(grid, dens_h, colour = "Habitaclia"), size = 1) +
  geom_line(aes(grid, dens_f, colour = "Fotocasa"), size = 1) +
  geom_line(aes(grid, dens_p, colour = "Pisos"), size = 1) +
  geom_line(aes(grid, dens_i, colour = "Idealista"), size = 1) + 
  scale_colour_manual(values = colours, name = "Dataset") +
  theme_light() +
  ggtitle("Density Distribution of Monthly Rent per Square Meter") +
  xlab("Monthly Rent per Square Meter, in Euros") + 
  ylab("Density")
dev.off()

# Monthly Rent per Square Meter Density, Undersmoothed #
minimum <- 5
maximum <- 50
len <- 10000
h <- c(h_pi(data$output_h$RentM), h_pi(data$output_f$RentM),
       h_pi(data$output_p$RentM), h_pi(data$output_i$RentM))/4
grid <- seq(minimum, maximum, length.out = len)
dens_h <- dens_line(len, data$output_h$RentM, h[1], use.default.grid = FALSE, min = minimum, max = maximum)
dens_f <- dens_line(len, data$output_f$RentM, h[2], use.default.grid = FALSE, min = minimum, max = maximum)
dens_p <- dens_line(len, data$output_p$RentM, h[3], use.default.grid = FALSE, min = minimum, max = maximum)
dens_i <- dens_line(len, data$output_i$RentM, h[4], use.default.grid = FALSE, min = minimum, max = maximum)
data_dens <- data.frame(grid = grid, dens_h = dens_h$densities,
                        dens_f = dens_f$densities, dens_p = dens_p$densities,
                        dens_i = dens_i$densities)
pdf("rentmu.pdf", height = 4.5, width = 6.5)
ggplot(data_dens) +
  geom_line(aes(grid, dens_h, colour = "Habitaclia"), size = 1) +
  geom_line(aes(grid, dens_f, colour = "Fotocasa"), size = 1) +
  geom_line(aes(grid, dens_p, colour = "Pisos"), size = 1) +
  geom_line(aes(grid, dens_i, colour = "Idealista"), size = 1) + 
  scale_colour_manual(values = colours, name = "Dataset") +
  theme_light() +
  ggtitle("Density Distribution of Monthly Rent per Square Meter", subtitle = "Undersmoothed") +
  xlab("Monthly Rent per Square Meter, in Euros") + 
  ylab("Density")
dev.off()

# Log Monthly Rent per Square Meter Density #
minimum <- log(5)
maximum <- log(50)
len <- 10000
h <- c(h_pi(log(data$output_h$RentM)), h_pi(log(data$output_f$RentM)),
       h_pi(log(data$output_p$RentM)), h_pi(log(data$output_i$RentM)))
grid <- seq(minimum, maximum, length.out = len)
dens_h <- dens_line(len, log(data$output_h$RentM), h[1], use.default.grid = FALSE, min = minimum, max = maximum)
dens_f <- dens_line(len, log(data$output_f$RentM), h[2], use.default.grid = FALSE, min = minimum, max = maximum)
dens_p <- dens_line(len, log(data$output_p$RentM), h[3], use.default.grid = FALSE, min = minimum, max = maximum)
dens_i <- dens_line(len, log(data$output_i$RentM), h[4], use.default.grid = FALSE, min = minimum, max = maximum)
data_dens <- data.frame(grid = grid, dens_h = dens_h$densities,
                        dens_f = dens_f$densities, dens_p = dens_p$densities,
                        dens_i = dens_i$densities)
pdf("rentml.pdf", height = 4.5, width = 6.5)
ggplot(data_dens) +
  geom_line(aes(grid, dens_h, colour = "Habitaclia"), size = 1) +
  geom_line(aes(grid, dens_f, colour = "Fotocasa"), size = 1) +
  geom_line(aes(grid, dens_p, colour = "Pisos"), size = 1) +
  geom_line(aes(grid, dens_i, colour = "Idealista"), size = 1) + 
  scale_colour_manual(values = colours, name = "Dataset") +
  theme_light() +
  ggtitle("Density Distribution of Log Monthly Rent per Square Meter") +
  xlab("Log Monthly Rent per Square Meter") + 
  ylab("Density")
dev.off()

# Log Monthly Rent per Square Meter Density, Undersmoothed #
minimum <- log(5)
maximum <- log(50)
len <- 10000
h <- c(h_pi(log(data$output_h$RentM)), h_pi(log(data$output_f$RentM)),
       h_pi(log(data$output_p$RentM)), h_pi(log(data$output_i$RentM)))/3
grid <- seq(minimum, maximum, length.out = len)
dens_h <- dens_line(len, log(data$output_h$RentM), h[1], use.default.grid = FALSE, min = minimum, max = maximum)
dens_f <- dens_line(len, log(data$output_f$RentM), h[2], use.default.grid = FALSE, min = minimum, max = maximum)
dens_p <- dens_line(len, log(data$output_p$RentM), h[3], use.default.grid = FALSE, min = minimum, max = maximum)
dens_i <- dens_line(len, log(data$output_i$RentM), h[4], use.default.grid = FALSE, min = minimum, max = maximum)
data_dens <- data.frame(grid = grid, dens_h = dens_h$densities,
                        dens_f = dens_f$densities, dens_p = dens_p$densities,
                        dens_i = dens_i$densities)
pdf("rentmlu.pdf", height = 4.5, width = 6.5)
ggplot(data_dens) +
  geom_line(aes(grid, dens_h, colour = "Habitaclia"), size = 1) +
  geom_line(aes(grid, dens_f, colour = "Fotocasa"), size = 1) +
  geom_line(aes(grid, dens_p, colour = "Pisos"), size = 1) +
  geom_line(aes(grid, dens_i, colour = "Idealista"), size = 1) + 
  scale_colour_manual(values = colours, name = "Dataset") +
  theme_light() +
  ggtitle("Density Distribution of Log Monthly Rent per Square Meter", subtitle = "Undersmoothed") +
  xlab("Log Monthly Rent per Square Meter") + 
  ylab("Density")
dev.off()

rm(grid, dens_h, dens_f, dens_p, dens_i,
   data_dens, h, len, maximum, minimum)

# Rank Analysis #
rank_h <- 8810 -700 - rank(data$output_h$RentM)
rank_f <- 3959 - rank(data$output_f$RentM)
rank_p <- 2237 - rank(data$output_p$RentM)
rank_i <- 4288 - rank(data$output_i$RentM)
pdf("rank.pdf", height = 4.5, width = 6.5)
ggplot() +
  geom_line(aes(x = (rank_f), y = data$output_f$RentM, colour = "Fotocasa")) +
  geom_line(aes(x = (rank_h), y = data$output_h$RentM, colour = "Habitaclia")) +
  geom_line(aes(x = (rank_i), y = data$output_i$RentM, colour = "Idealista")) +
  geom_line(aes(x = (rank_p), y = data$output_p$RentM, colour = "Pisos")) +
  theme_light() +
  theme(legend.position = "left") +
  scale_color_manual(values = colours, name = "Dataset") +
  xlab("Rank") +
  ylab("Rent per Square Meter, in Euros") +
  ggtitle("Rank-Size Plot")
dev.off()

pdf("rankl.pdf", height = 4.5, width = 6.5)
ggplot() +
  geom_line(aes(x = log(rank_f), y = log(data$output_f$RentM), colour = "Fotocasa")) +
  geom_line(aes(x = log(rank_h), y = log(data$output_h$RentM), colour = "Habitaclia")) +
  geom_line(aes(x = log(rank_i), y = log(data$output_i$RentM), colour = "Idealista")) +
  geom_line(aes(x = log(rank_p), y = log(data$output_p$RentM), colour = "Pisos")) +
  theme_light() +
  scale_color_manual(values = colours, name = "Dataset") +
  xlab("Log Rank") +
  ylab("Log Rent per Square Meter") +
  ggtitle("Log Rank-Size Plot")
dev.off()

s_f <- summary(lm(log(data$output_f$RentM)~log(rank_f)))
s_h <- summary(lm(log(data$output_h$RentM)~log(rank_h)))
s_i <- summary(lm(log(data$output_i$RentM)~log(rank_i)))
s_p <- summary(lm(log(data$output_p$RentM)~log(rank_p)))
print(c(s_f$r.squared, s_h$r.squared, s_i$r.squared, s_p$r.squared))
rm(s_f, s_h, s_i, s_p, rank_f, rank_h, rank_i, rank_p)



### Distance Study ###
# Distributions by Distance #
minimum <- log(5)
maximum <- log(50)
len <- 10000
group1 <- data$output_f %>% filter(DistanceToCenter < 1.4)
group2 <- data$output_f %>% filter(DistanceToCenter >= 1.4, DistanceToCenter < 3)
group34 <- data$output_f %>% filter(DistanceToCenter > 3)
h <- c(h_pi(log(group1$RentM)),
       h_pi(log(group2$RentM)),
       h_pi(log(group34$RentM)))
grid <- seq(minimum, maximum, length.out = len)
dens_1 <- dens_line(len, log(group1$RentM), h[1], use.default.grid = FALSE, min = minimum, max = maximum)
dens_2 <- dens_line(len, log(group2$RentM), h[2], use.default.grid = FALSE, min = minimum, max = maximum)
dens_34 <- dens_line(len, log(group34$RentM), h[3], use.default.grid = FALSE, min = minimum, max = maximum)
data_dens <- data.frame(grid = grid, dens_1 = dens_1$densities, 
                        dens_2 = dens_2$densities, dens_34 = dens_34$densities)
pdf("densf.pdf", height = 4.5, width = 6.5)
ggplot(data_dens) +
  geom_line(aes(x = grid, y = dens_1, linetype = "Centre", colour = "Centre"), size = 1) +
  geom_line(aes(x = grid, y = dens_2, linetype = "Middle ring", colour = "Middle ring"), size = 1) +
  geom_line(aes(x = grid, y = dens_34, linetype = "Periphery", colour = "Periphery"), size = 1) +
  scale_color_manual(values = rep("blue", 3)) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
  labs(color  = "Distance Group", linetype = "Distance Group", shape = "Distance Group") +
  theme_light() +
  theme(legend.position = "left") +
  ggtitle("Density Distribution depending on Distance Group", subtitle = "Fotocasa") +
  xlab("Log Rent per Square Meter") + 
  ylab("Density")
dev.off()
cat('Mean Center:', mean(group1$RentM), '\nMean Ring:', mean(group2$RentM), '\nMean Periphery:', mean(group34$RentM))
cat('SD Center:', sd(group1$RentM), '\nSD Ring:', sd(group2$RentM), '\nSD Periphery:', sd(group34$RentM))

group1 <- data$output_h %>% filter(DistanceToCenter < 1.4)
group2 <- data$output_h %>% filter(DistanceToCenter >= 1.4, DistanceToCenter < 3)
group34 <- data$output_h %>% filter(DistanceToCenter > 3)
h <- c(h_pi(log(group1$RentM)),
       h_pi(log(group2$RentM)),
       h_pi(log(group34$RentM)))
grid <- seq(minimum, maximum, length.out = len)
dens_1 <- dens_line(len, log(group1$RentM), h[1], use.default.grid = FALSE, min = minimum, max = maximum)
dens_2 <- dens_line(len, log(group2$RentM), h[2], use.default.grid = FALSE, min = minimum, max = maximum)
dens_34 <- dens_line(len, log(group34$RentM), h[3], use.default.grid = FALSE, min = minimum, max = maximum)
data_dens <- data.frame(grid = grid, dens_1 = dens_1$densities, 
                        dens_2 = dens_2$densities, dens_34 = dens_34$densities)
pdf("densh.pdf", height = 4.5, width = 6.5)
ggplot(data_dens) +
  geom_line(aes(x = grid, y = dens_1, linetype = "Centre", colour = "Centre"), size = 1) +
  geom_line(aes(x = grid, y = dens_2, linetype = "Middle ring", colour = "Middle ring"), size = 1) +
  geom_line(aes(x = grid, y = dens_34, linetype = "Periphery", colour = "Periphery"), size = 1) +
  scale_color_manual(values = rep("orange", 3)) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
  labs(color  = "Distance Group", linetype = "Distance Group", shape = "Distance Group") +
  theme_light() +
  ggtitle("Density Distribution depending on Distance Group", subtitle = "Habitaclia") +
  xlab("Log Rent per Square Meter") + 
  ylab("Density")
dev.off()
cat('Mean Center:', mean(group1$RentM), '\nMean Ring:', mean(group2$RentM), '\nMean Periphery:', mean(group34$RentM))
cat('SD Center:', sd(group1$RentM), '\nSD Ring:', sd(group2$RentM), '\nSD Periphery:', sd(group34$RentM))

group1 <- data$output_i %>% filter(DistanceToCenter < 1.4)
group2 <- data$output_i %>% filter(DistanceToCenter >= 1.4, DistanceToCenter < 3)
group34 <- data$output_i %>% filter(DistanceToCenter > 3)
h <- c(h_pi(log(group1$RentM)),
       h_pi(log(group2$RentM)),
       h_pi(log(group34$RentM)))
grid <- seq(minimum, maximum, length.out = len)
dens_1 <- dens_line(len, log(group1$RentM), h[1], use.default.grid = FALSE, min = minimum, max = maximum)
dens_2 <- dens_line(len, log(group2$RentM), h[2], use.default.grid = FALSE, min = minimum, max = maximum)
dens_34 <- dens_line(len, log(group34$RentM), h[3], use.default.grid = FALSE, min = minimum, max = maximum)
data_dens <- data.frame(grid = grid, dens_1 = dens_1$densities, 
                        dens_2 = dens_2$densities, dens_34 = dens_34$densities)
pdf("densi.pdf", height = 4.5, width = 6.5)
ggplot(data_dens) +
  geom_line(aes(x = grid, y = dens_1, linetype = "Centre", colour = "Centre"), size = 1) +
  geom_line(aes(x = grid, y = dens_2, linetype = "Middle ring", colour = "Middle ring"), size = 1) +
  geom_line(aes(x = grid, y = dens_34, linetype = "Periphery", colour = "Periphery"), size = 1) +
  scale_color_manual(values = rep("green", 3)) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
  labs(color  = "Distance Group", linetype = "Distance Group", shape = "Distance Group") +
  theme_light() +
  theme(legend.position = "left") +
  ggtitle("Density Distribution depending on Distance Group", subtitle = "Idealista") +
  xlab("Log Rent per Square Meter") + 
  ylab("Density")
dev.off()
cat('Mean Center:', mean(group1$RentM), '\nMean Ring:', mean(group2$RentM), '\nMean Periphery:', mean(group34$RentM))
cat('SD Center:', sd(group1$RentM), '\nSD Ring:', sd(group2$RentM), '\nSD Periphery:', sd(group34$RentM))

group1 <- data$output_p %>% filter(DistanceToCenter < 1.4)
group2 <- data$output_p %>% filter(DistanceToCenter >= 1.4, DistanceToCenter < 3)
group34 <- data$output_p %>% filter(DistanceToCenter > 3)
h <- c(h_pi(log(group1$RentM)),
       h_pi(log(group2$RentM)),
       h_pi(log(group34$RentM)))
grid <- seq(minimum, maximum, length.out = len)
dens_1 <- dens_line(len, log(group1$RentM), h[1], use.default.grid = FALSE, min = minimum, max = maximum)
dens_2 <- dens_line(len, log(group2$RentM), h[2], use.default.grid = FALSE, min = minimum, max = maximum)
dens_34 <- dens_line(len, log(group34$RentM), h[3], use.default.grid = FALSE, min = minimum, max = maximum)
data_dens <- data.frame(grid = grid, dens_1 = dens_1$densities, 
                        dens_2 = dens_2$densities, dens_34 = dens_34$densities)
pdf("densp.pdf", height = 4.5, width = 6.5)
ggplot(data_dens) +
  geom_line(aes(x = grid, y = dens_1, linetype = "Centre", colour = "Centre"), size = 1) +
  geom_line(aes(x = grid, y = dens_2, linetype = "Middle ring", colour = "Middle ring"), size = 1) +
  geom_line(aes(x = grid, y = dens_34, linetype = "Periphery", colour = "Periphery"), size = 1) +
  scale_color_manual(values = rep("cyan", 3)) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
  labs(color  = "Distance Group", linetype = "Distance Group", shape = "Distance Group") +
  theme_light() +
  ggtitle("Density Distribution depending on Distance Group", subtitle = "Pisos") +
  xlab("Log Rent per Square Meter") + 
  ylab("Density")
dev.off()
cat('Mean Center:', mean(group1$RentM), '\nMean Ring:', mean(group2$RentM), '\nMean Periphery:', mean(group34$RentM))
cat('SD Center:', sd(group1$RentM), '\nSD Ring:', sd(group2$RentM), '\nSD Periphery:', sd(group34$RentM))

rm(len, grid, h, maximum, minimum, data_dens, dens_1, dens_2, dens_34, 
   group1, group2, group34)

# Rent per Square Meter - Distance to Center Regression #
minimum <- 0
maximum <- 9
times <- 50
len <- 500
h1 <- c(LOOCV_ll(data$output_h$DistanceToCenter, data$output_h$RentM, times)$hst, # Attention: Extremely time-consuming.
        LOOCV_ll(data$output_f$DistanceToCenter, data$output_f$RentM, times)$hst,
        LOOCV_ll(data$output_p$DistanceToCenter, data$output_p$RentM, times)$hst,
        LOOCV_ll(data$output_i$DistanceToCenter, data$output_i$RentM, times)$hst)
h2 <- c(h_pi((data$output_h$DistanceToCenter)), h_pi((data$output_f$DistanceToCenter)),
       h_pi((data$output_p$DistanceToCenter)), h_pi((data$output_i$DistanceToCenter)))
grid <- seq(minimum, maximum, length.out = len)
line_h <- reg_line_ll(len, data$output_h$DistanceToCenter, data$output_h$RentM, h1[1], use.default.grid = FALSE, min = minimum, max = maximum)
line_f <- reg_line_ll(len, data$output_f$DistanceToCenter, data$output_f$RentM, h1[2], use.default.grid = FALSE, min = minimum, max = maximum)
line_p <- reg_line_ll(len, data$output_p$DistanceToCenter, data$output_p$RentM, h1[3], use.default.grid = FALSE, min = minimum, max = maximum)
line_i <- reg_line_ll(len, data$output_i$DistanceToCenter, data$output_i$RentM, h1[4], use.default.grid = FALSE, min = minimum, max = maximum)
dens_h <- dens_line(len, data$output_h$DistanceToCenter, h2[1], use.default.grid = FALSE, min = minimum, max = maximum)
dens_f <- dens_line(len, data$output_f$DistanceToCenter, h2[2], use.default.grid = FALSE, min = minimum, max = maximum)
dens_p <- dens_line(len, data$output_p$DistanceToCenter, h2[3], use.default.grid = FALSE, min = minimum, max = maximum)
dens_i <- dens_line(len, data$output_i$DistanceToCenter, h2[4], use.default.grid = FALSE, min = minimum, max = maximum)
data_reg <- data.frame(grid = grid, line_h = line_h$y, line_f = line_f$y, line_p = line_p$y, line_i = line_i$y,
                       dens_h = dens_h$densities, dens_f = dens_f$densities, 
                       dens_p = dens_p$densities, dens_i = dens_i$densities)
pdf("dist.pdf", height = 4, width = 7)
ggplot(data_reg) +
  geom_line(aes(grid, line_h, colour = "Habitaclia"), size = 1) +
  geom_line(aes(grid, line_f, colour = "Fotocasa"), size = 1) +
  geom_line(aes(grid, line_p, colour = "Pisos"), size = 1) +
  geom_line(aes(grid, line_i, colour = "Idealista"), size = 1) + 
  geom_line(aes(grid, (dens_h+1)/0.1, colour = "Habitaclia"), linetype = "dotted", size = 1) + 
  geom_line(aes(grid, (dens_f+1)/0.1, colour = "Fotocasa"), linetype = "dotted", size = 1) + 
  geom_line(aes(grid, (dens_p+1)/0.1, colour = "Pisos"), linetype = "dotted", size = 1) + 
  geom_line(aes(grid, (dens_i+1)/0.1, colour = "Idealista"), linetype = "dotted", size = 1) + 
  scale_y_continuous(limits=c(10, 20),
                     sec.axis = sec_axis(~ . *(0.1) - 1, name = "Densities")) +
  theme_light() +
  scale_colour_manual(values = colours, name = "Dataset") +
  ggtitle("Rent per Square Meter - Distance to Centre Relationship") +
  xlab("Distance to City Centre, in Km") + 
  ylab("Rent per Square Meter, in Euros")
dev.off()

rm(grid, line_h, line_f, line_p, line_i, line_h, dens_h, dens_f, dens_p, dens_i,
   data_reg, h1, h2, len, maximum, minimum, times)
   


### Geospatial Data ###
times <- 40
rad_neighborhood <- 0.01
pop_neighborhood <- 3
box <- c(left = 2.075, bottom = 41.35-0.025, right = 2.25, top = 41.46+0.01)
map <- get_stamenmap(bbox = box, maptype = "terrain-lines", zoom = 13)

output <- data.frame(Latitude = data$output_f$Latitude, Longitude = data$output_f$Longitude, 
                     DistanceToCenter = data$output_f$DistanceToCenter, RentM = (data$output_f$RentM))
locations_f <- as_tibble(output)
output <- data.frame(Latitude = data$output_h$Latitude, Longitude = data$output_h$Longitude, 
                     DistanceToCenter = data$output_h$DistanceToCenter, RentM = (data$output_h$RentM))
locations_h <- as_tibble(output)
data$output_i <- data$output_i %>% filter(is.na(Latitude) == FALSE)
output <- data.frame(Latitude = data$output_i$Latitude, Longitude = data$output_i$Longitude, 
                     DistanceToCenter = data$output_i$DistanceToCenter, RentM = (data$output_i$RentM))
locations_i <- as_tibble(output)
output <- data.frame(Latitude = data$output_p$Latitude, Longitude = data$output_p$Longitude, 
                     DistanceToCenter = data$output_p$DistanceToCenter, RentM = (data$output_p$RentM))
locations_p <- as_tibble(output)

h <- c(LOOCV3d_ll(locations_f$Longitude, locations_f$Latitude, locations_f$RentM, times)$hst, # Attention: extremely time-consuming
       LOOCV3d_ll(locations_h$Longitude, locations_h$Latitude, locations_h$RentM, times)$hst,
       LOOCV3d_ll(locations_i$Longitude, locations_i$Latitude, locations_i$RentM, times)$hst,
       LOOCV3d_ll(locations_p$Longitude, locations_p$Latitude, locations_p$RentM, times)$hst)
r_f <- reg_space_ll(250, locations_f$Longitude, locations_f$Latitude, locations_f$RentM, h[1], # Attention: extremely time-consuming
               FALSE, 2.075, 2.25, 41.35-0.025, 41.46+0.01)
r_h <- reg_space_ll(250, locations_h$Longitude, locations_h$Latitude, locations_h$RentM, h[2],
               FALSE, 2.075, 2.25, 41.35-0.025, 41.46+0.01)
r_i <- reg_space_ll(250, locations_i$Longitude, locations_i$Latitude, locations_i$RentM, h[3],
               FALSE, 2.075, 2.25, 41.35-0.025, 41.46+0.01)
r_p <- reg_space_ll(250, locations_p$Longitude, locations_p$Latitude, locations_p$RentM, h[4],
               FALSE, 2.075, 2.25, 41.35-0.025, 41.46+0.01)

r_f$z <- nullify(r_f$x, r_f$y, r_f$z, locations_f$Longitude, locations_f$Latitude, 
               rad_neighborhood, pop_neighborhood)
r_h$z <- nullify(r_h$x, r_h$y, r_h$z, locations_h$Longitude, locations_h$Latitude, 
               rad_neighborhood, pop_neighborhood)
r_i$z <- nullify(r_i$x, r_i$y, r_i$z, locations_i$Longitude, locations_i$Latitude, 
               rad_neighborhood, pop_neighborhood)
r_p$z <- nullify(r_p$x, r_p$y, r_p$z, locations_p$Longitude, locations_p$Latitude, 
               rad_neighborhood, pop_neighborhood)

mod_quant_f <- rescale(c(quantile(na.omit(r_f$z), c(0.01, 0.9), na.rm = TRUE), range(na.omit(r_f$z))))
mod_quant_h <- rescale(c(quantile(na.omit(r_h$z), c(0.01, 0.9), na.rm = TRUE), range(na.omit(r_h$z))))
mod_quant_i <- rescale(c(quantile(na.omit(r_i$z), c(0.01, 0.9), na.rm = TRUE), range(na.omit(r_i$z))))
mod_quant_p <- rescale(c(quantile(na.omit(r_p$z), c(0.01, 0.9), na.rm = TRUE), range(na.omit(r_p$z))))

colour_range <- 6
palette <- "turbo"

map_f <- ggmap(map) +
  geom_raster(data = r_f, aes(x = x, y = y, fill = z), alpha = 0.6, 
              hjust = 0, vjust = 0, interpolate = TRUE) +
  scale_fill_viridis_c(option = palette, na.value = NA, 
                       guide_legend(title = "Rent per Square\nMeter in Euros"),
                       values = c(0, seq(mod_quant_f[1], mod_quant_f[2], length.out = colour_range), 1),
                       breaks = c(10, 12.5, 15, 17.5, 20, 22.5, 25, 27.5, 30, 32.5)) +
  coord_equal() +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_light() +
  theme(legend.key.height = unit(3, "lines"), legend.position = "left") +
  ggtitle("Rent Prices - Fotocasa")

map_h <- ggmap(map) +
  geom_raster(data = r_h, aes(x = x, y = y, fill = z), alpha = 0.6, 
              hjust = 0, vjust = 0, interpolate = TRUE) +
  scale_fill_viridis_c(option = palette, na.value = NA, 
                       guide_legend(title = "Rent per Square\nMeter in Euros"),
                       values = c(0, seq(mod_quant_h[1], mod_quant_h[2], length.out = colour_range), 1),
                       breaks = c(7.5, 10, 12.5, 15, 17.5, 20, 22.5, 25, 27.5, 30)) +
  coord_equal() +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_light() +
  theme(legend.key.height = unit(3, "lines")) +
  ggtitle("Rent Prices - Habitaclia")

map_i <- ggmap(map) +
  geom_raster(data = r_i, aes(x = x, y = y, fill = z), alpha = 0.6, 
              hjust = 0, vjust = 0, interpolate = TRUE) +
  scale_fill_viridis_c(option = palette, na.value = NA, 
                       guide_legend(title = "Rent per Square\nMeter in Euros"),
                       values = c(0, seq(mod_quant_i[1], mod_quant_i[2], length.out = colour_range), 1),
                       breaks = c(10, 15, 20,  25, 30, 35, 40, 45, 50)) +
  coord_equal() +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_light() +
  theme(legend.key.height = unit(3, "lines"), legend.position = "left") +
  ggtitle("Rent Prices - Idealista")

map_p <- ggmap(map) +
  geom_raster(data = r_p, aes(x = x, y = y, fill = z), alpha = 0.6, 
              hjust = 0, vjust = 0, interpolate = TRUE) +
  scale_fill_viridis_c(option = palette, na.value = NA, 
                       guide_legend(title = "Rent per Square\nMeter in Euros"),
                       values = c(0, seq(mod_quant_p[1], mod_quant_p[2], length.out = colour_range), 1),
                       breaks = c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55)) +
  coord_equal() +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_light() +
  theme(legend.key.height = unit(3, "lines")) +
  ggtitle("Rent Prices - Pisos")

pdf("mapf.pdf", height = 5, width = 6.5)
map_f
dev.off()

pdf("maph.pdf", height = 5, width = 6.5)
map_h
dev.off()

pdf("mapi.pdf", height = 5, width = 6.5)
map_i
dev.off()

pdf("mapp.pdf", height = 5, width = 6.5)
map_p
dev.off()

rm(output, mod_quant_p, mod_quant_i, mod_quant_h, mod_quant_f, palette, colour_range,
   r_i, r_h, r_f, r_p, h, locations_i, locations_f, locations_h, locations_p, times,
   box, map, rad_neighborhood, pop_neighborhood)
