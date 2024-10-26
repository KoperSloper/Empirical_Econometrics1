data = killing_aej_5257

library(dplyr)
library(plm)
library(sandwich)
library(lmtest)
library(stargazer)
library(did)
library(tidyr)
library(ggplot2)
library(patchwork)
library(fixest)

# Making some adjustments to the data
data <- data %>%
  mutate(
    treatment = ifelse((year >= 1955 & timing == 1955) | (year >= 1956 & timing == 1956), 1, 0)
  )

data <- data %>%
  mutate(
    ldraft = log(draft),
    lpop = log(pop),
    loutput = log(output_grain),
    larea = log(area),
    ldistance = log(1 + distance)
  )

data <- data %>%
  mutate(
    ldif = ldraft - lag(ldraft),
    ldif_pop = lpop - lag(lpop),
    ldif_output = loutput - lag(loutput),
    ldif_area = larea - lag(larea)
  )

data <- data %>%
  mutate(
    treatment_output = ifelse((timing == 1955 & year >= 1956) | (timing == 1956 & year == 1957), 1, 0)
  )

data <- data %>%
  mutate(trend = year - 1951)

data <- data %>%
  mutate(
    timestan = case_when(
      (year == 1954 & timing == 1955) | (year == 1955 & timing == 1956) ~ -1,
      (year == 1953 & timing == 1955) | (year == 1954 & timing == 1956) ~ -2,
      (year == 1952 & timing == 1955) | (year == 1953 & timing == 1956) ~ -3,
      year == 1952 & timing == 1956 ~ -4,
      (year == 1956 & timing == 1955) | (year == 1957 & timing == 1956) ~ 1,
      year == 1957 & timing == 1955 ~ 2,
      TRUE ~ 0
    )
  )

data <- data %>%
  mutate(
    dmean_lsize = lsize_advance1957 - mean(lsize_advance1957, na.rm = TRUE),
    dmean_mid_house = mid_house - mean(mid_house, na.rm = TRUE),
    dmean_ldistance = ldistance - mean(ldistance, na.rm = TRUE)
  )

pdata <- pdata.frame(data, index = c("countycode", "year"))

# The different models
model1 <- plm(ldraft ~ treatment + factor(year)+factor(countycode), data = pdata, model = "within")
coeftest(model1, vcov = vcovHC(model1, type = "HC1", cluster = "group"))

model2 <- plm(ldraft ~ treatment + lpop + larea + flood + draught + factor(year), 
              data = pdata, model = "within")
coeftest(model2, vcov = vcovHC(model2, type = "HC1", cluster = "group"))

model3 <- plm(ldraft ~ treatment + lpop + larea + flood + draught + 
                factor(year) * factor(provincecode), 
              data = pdata, model = "within")
coeftest(model3, vcov = vcovHC(model3, type = "HC1", cluster = "group"))

model4 <- plm(ldraft ~ treatment + lpop + larea + flood + draught + 
                factor(year) + factor(countycode) * trend, 
              data = pdata, model = "within")
coeftest(model4, vcov = vcovHC(model4, type = "HC1", cluster = "group"))

model5 <- plm(ldraft ~ treatment + lpop + larea + flood + draught + 
                factor(year), 
              data = pdata[pdata$distance <= 100, ], model = "within")
coeftest(model5, vcov = vcovHC(model5, type = "HC1", cluster = "group"))

model6 <- plm(ldraft ~ treatment + lpop + larea + flood + draught + 
                factor(year) + factor(countycode) * trend, 
              data = pdata[pdata$distance <= 100, ], model = "within")
coeftest(model6, vcov = vcovHC(model6, type = "HC1", cluster = "group"))

stargazer(model1, model2, model3, model4, model5, model6,
          type = "latex",
          title = "TABLE 2—THE EFFECTS OF COLLECTIVIZATION ON THE INVENTORY OF DRAFT ANIMALS: LOG (draft animals)",
          dep.var.labels = "Log (Draft Animals)",
          covariate.labels = c("Collectivization", "g(rural population)", "g(arable lands)", 
                               "Flood", "Draught"),
          omit.stat = c("f", "ser"),
          no.space = TRUE,
          keep = c("treatment", "lpop", "larea", "flood", "draught"),
          add.lines = list(c("Province fixed effects > year fixed effects", "N", "N", "Y", "N", "N", "N"),
                           c("County fixed effects x trend", "N", "N", "N", "Y", "N", "Y"),
                           c("Distance to prov. capital < 100 km", "N", "N", "N", "N", "Y", "Y")))

# Visualizing variation in treatment
navy_color <- "#003366"
pink_color <- "#FF69B4"

plot_data <- data %>%
  group_by(year, timing) %>%
  summarize(mean_ldraft = mean(ldraft, na.rm = TRUE)) %>%
  ungroup()

p <- ggplot(plot_data, aes(x = year, y = mean_ldraft, color = factor(timing), group = timing)) +
  geom_vline(xintercept = 1954, linetype = "dashed", color = navy_color, size = 0.7, alpha = 0.7) +
  geom_vline(xintercept = 1955, linetype = "dashed", color = pink_color, size = 0.7, alpha = 0.7) +
  geom_line(size = 0.8) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c(navy_color, pink_color),
                     name = "Treatment Group",
                     labels = c("1955 Treatment", "1956 Treatment")) +
  labs(x = "Year",
       y = "Log(Number of Draft Animals)") +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray90"),
    panel.border = element_rect(color = "black", size = 0.7),
    axis.line = element_line(color = "black", size = 0.7),
    axis.ticks = element_line(color = "black", size = 0.7),
    axis.text = element_text(color = "black", size = 9),
    axis.title = element_text(size = 10, face = "plain"),
    legend.position = "bottom",
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 9),
    legend.box.spacing = unit(0.5, "lines"),
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "lines")
  ) +
  scale_x_continuous(breaks = seq(min(plot_data$year), max(plot_data$year), by = 1))

ggsave("diff_in_diff_plot.pdf", p, width = 8, height = 6, device = cairo_pdf)

# Common trends assumption
data1 <- subset(data, !(timestan %in% c(-7, 2)))

data_balanced <- subset(data1, timestan >= -3 & timestan <= 1)

model_draft <- plm(ldraft ~ factor(timestan)*timing_1956, 
                      data = data_balanced, 
                      index = c("countycode", "year"), 
                      model = "within",
                      effect = "individual")

model_pop <- plm(lpop ~ factor(timestan)*timing_1956, 
                 data = data_balanced, 
                 index = c("countycode", "year"), 
                 model = "within",
                 effect = "individual")

model_area <- plm(larea ~ factor(timestan)*timing_1956, 
                  data = data_balanced, 
                  index = c("countycode", "year"), 
                  model = "within",
                  effect = "individual")

stargazer(model_draft, model_pop, model_area,
          title = "Test for the Assumption of a Common Trend",
          column.labels = c("log(animal)", "log(population)", "log(land)"),
          dep.var.labels = "",
          model.numbers = FALSE,
          omit = c("factor\\(timestan\\)", "Constant"),
          keep = "timing_1956",
          covariate.labels = c("Group56 × two years before collect.",
                               "Group56 × one year before collect.",
                               "Group56 × year of collect.",
                               "Group56 × one year after collect."),
          add.lines = list(c("Sample years", "1952-1957", "1952-1957", "1952-1957")),
          se = list(sqrt(diag(vcovHC(model_draft, type = "HC1", cluster = "group"))),
                    sqrt(diag(vcovHC(model_pop, type = "HC1", cluster = "group"))),
                    sqrt(diag(vcovHC(model_area, type = "HC1", cluster = "group")))),
          type = "latex",
          style = "aer",
          notes = "Standard errors are clustered at the county level.",
          notes.append = TRUE,
          header = FALSE)



# ATT(g,t) method 
da.attgt <- att_gt(yname = "ldraft",
                   gname = "timing",
                   idname = "countycode",
                   tname = "year",
                   xformla = ~lpop + larea + flood + draught,
                   data = data,
                   control_group = "notyettreated"
                   )

da2.attgt <- att_gt(yname = "ldraft",
                   gname = "timing",
                   idname = "countycode",
                   tname = "year",
                   data = data,
                   control_group = "notyettreated"
)

summary(da.attgt)
summary(da2.attgt)

# Visualizing four different DiD plots
generate_data <- function(t, treatment_time = Inf, base_slope = 0.2, effect = 2) { 
  y <- 10 + base_slope * t 
  if (treatment_time != Inf) { 
    y[t == treatment_time] <- y[t == treatment_time] + effect  # Sharp jump
    y[t > treatment_time] <- y[t > treatment_time] + effect  # Keep the increase linear
  } 
  return(y)
}

t <- seq(1, 10, 1)
early_treatment_time <- 4
late_treatment_time <- 7
base_slope <- 0.2
treatment_effect <- 2
early_treated <- generate_data(t, early_treatment_time)
late_treated <- generate_data(t, late_treatment_time)
never_treated <- generate_data(t, Inf)

pink_color <- "#FF69B4"
blue_color <- "#003366"
green_color <- "#006400"
gray_color <- "#CCCCCC"

theme_custom <- theme_bw() +  
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray90"),
    panel.border = element_rect(color = "black", size = 0.7),
    axis.line = element_line(color = "black", size = 0.7),
    axis.ticks = element_line(color = "black", size = 0.7),
    axis.text = element_blank(),
    axis.title = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 9),
    legend.box.spacing = unit(0.5, "lines"),
    plot.title = element_text(size = 10, face = "plain", hjust = 0),
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "lines")
  )

p1 <- ggplot() +  
  geom_line(aes(x = t, y = early_treated, color = "Early Treated"), size = 0.8) +  
  geom_line(aes(x = t, y = never_treated, color = "Never Treated"), size = 0.8) +  
  geom_point(aes(x = t, y = early_treated, color = "Early Treated"), size = 1.5) +  
  geom_point(aes(x = t, y = never_treated, color = "Never Treated"), size = 1.5) +  
  geom_vline(xintercept = early_treatment_time, linetype = "dashed", 
             color = pink_color, size = 0.7, alpha = 0.7) +  
  scale_color_manual(values = c("Early Treated" = pink_color, 
                                "Never Treated" = green_color)) +  
  scale_x_continuous(limits = c(1, 10)) +  
  theme_custom +  
  labs(title = "Early vs Never Treated")

p2 <- ggplot() +  
  geom_line(aes(x = t, y = late_treated, color = "Late Treated"), size = 0.8) +  
  geom_line(aes(x = t, y = never_treated, color = "Never Treated"), size = 0.8) +  
  geom_point(aes(x = t, y = late_treated, color = "Late Treated"), size = 1.5) +  
  geom_point(aes(x = t, y = never_treated, color = "Never Treated"), size = 1.5) +  
  geom_vline(xintercept = late_treatment_time, linetype = "dashed", 
             color = blue_color, size = 0.7, alpha = 0.7) +  
  scale_color_manual(values = c("Late Treated" = blue_color, 
                                "Never Treated" = green_color)) +  
  scale_x_continuous(limits = c(1, 10)) +  
  theme_custom +  
  labs(title = "Late vs Never Treated")

p3 <- ggplot() +
  annotate("rect", xmin = 1, xmax = late_treatment_time - 1, ymin = -Inf, ymax = Inf,
           fill = "gray80", alpha = 0.5) +
  geom_line(aes(x = t, y = early_treated, color = "Early Treated"), size = 0.8) +
  geom_line(aes(x = t, y = late_treated, color = "Late Treated"), size = 0.8) +
  geom_point(aes(x = t, y = early_treated, color = "Early Treated"), size = 1.5) +
  geom_point(aes(x = t, y = late_treated, color = "Late Treated"), size = 1.5) +
  geom_vline(xintercept = early_treatment_time, linetype = "dashed", 
             color = pink_color, size = 0.7, alpha = 0.7) +
  geom_vline(xintercept = late_treatment_time, linetype = "dashed", 
             color = blue_color, size = 0.7, alpha = 0.7) +
  scale_color_manual(values = c("Early Treated" = pink_color, 
                                "Late Treated" = blue_color)) +
  scale_x_continuous(limits = c(1, 10)) +
  theme_custom +
  labs(title = "Early vs Late (Before Late Treatment)")

p4 <- ggplot() +
  annotate("rect", xmin = early_treatment_time, xmax = 10, ymin = -Inf, ymax = Inf,
           fill = "gray80", alpha = 0.5) +
  geom_line(aes(x = t, y = early_treated, color = "Early Treated"), size = 0.8) +
  geom_line(aes(x = t, y = late_treated, color = "Late Treated"), size = 0.8) +
  geom_point(aes(x = t, y = early_treated, color = "Early Treated"), size = 1.5) +
  geom_point(aes(x = t, y = late_treated, color = "Late Treated"), size = 1.5) +
  geom_vline(xintercept = early_treatment_time, linetype = "dashed", 
             color = pink_color, size = 0.7, alpha = 0.7) +
  geom_vline(xintercept = late_treatment_time, linetype = "dashed", 
             color = blue_color, size = 0.7, alpha = 0.7) +
  scale_color_manual(values = c("Early Treated" = pink_color, 
                                "Late Treated" = blue_color)) +
  scale_x_continuous(limits = c(1, 10)) +
  theme_custom +
  labs(title = "Late vs Early (After Early Treatment)")

combined_plot <- (p1 + p2) / (p3 + p4)

ggsave("combined_plot.pdf", plot = combined_plot, width = 8, height = 6)
