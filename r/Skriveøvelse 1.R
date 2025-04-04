#Indlæs pakker
library(tidyverse)
library(scales)
library(psych)
library(moments)
#working directory
getwd()
# Indlæs data
data_2001 <- read.csv("Valg 2001/Data/CSV/data12516.csv", sep = ";", encoding = "latin1", header = TRUE, quote = "\"")
data_2022 <- read.csv("Valg 2022/Data/CSV/Dataset.csv", encoding = "latin1")
#typetal funktion
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
# ------------------------------------------------------
# Politisk tillid 2022
# ------------------------------------------------------
#cleanup
data_2022 <- data_2022 |>
  mutate(across(c(Q55, Q53, Q52_1, Q52_2), ~na_if(., 99)))
# Reverse-kod kun de spørgsmål hvor 1 = høj tillid og 5 = lav tillid
data_2022 <- data_2022 |>
  mutate(
    q55_rev = 6 - Q55,     # Demokratitilfredshed
    q53_rev = 6 - Q53,     # Tillid til politikere
    q52b_rev = 6 - Q52_2,  # Tillid til embedsværk
    q52a = Q52_1           # Politkernes hesnsyn til borgernes mening, direkte brug
  )
# Skaler alle til 1-5 skala
data_2022 <- data_2022 |>
  mutate(
    tillid_q55 = scales::rescale(q55_rev, to = c(1, 5)),
    tillid_q53 = scales::rescale(q53_rev, to = c(1, 5)),
    tillid_q52b = scales::rescale(q52b_rev, to = c(1, 5)),
    tillid_q52a = scales::rescale(q52a, to = c(1, 5))
  )
# Konstruer indeks: gennemsnit hvis højst én NA
data_2022 <- data_2022 |>
  rowwise() |>
  mutate(
    politisk_tillid_rå = mean(c_across(c(tillid_q55, tillid_q53, tillid_q52b, tillid_q52a)), na.rm = TRUE),
    tillid_mangler = sum(is.na(c_across(c(tillid_q55, tillid_q53, tillid_q52b, tillid_q52a)))),
    politisk_tillid = ifelse(tillid_mangler <= 1, politisk_tillid_rå, NA)
  ) |>
  ungroup()
# Opsummering og visualisering
summary(data_2022$politisk_tillid)
ggplot(data_2022, aes(x = politisk_tillid)) +
  geom_histogram(binwidth = 0.25, fill = "#3B82F6", color = "black", boundary = 1) +
  scale_x_continuous(breaks = 1:5, limits = c(1, 5)) +
  labs(
    title = "Fordeling af politisk tillid (2022)",
    x = "Politisk tillid (1 = lav tillid, 5 = høj tillid)",
    y = "Antal respondenter"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5, margin = margin(b = 10)),
    axis.title = element_text(size = 12),
    panel.grid.major = element_line(color = "#E5E7EB"),
    panel.grid.minor = element_blank()
  ) +
  ylim(0, 400)
#typetal
typetal_tillid_2022 <- getmode(data_2022$politisk_tillid)
print(typetal_tillid_2022)
# Cronbach's alpha
psych::alpha(data_2022[, c("tillid_q55", "tillid_q53", "tillid_q52b", "tillid_q52a")])
# Beregn gennemsnit og standardafvigelse
middelværdi_tillid_2022 <- mean(data_2022$politisk_tillid, na.rm = TRUE)
spredning_tillid_2022 <- sd(data_2022$politisk_tillid, na.rm = TRUE)
# Beregn intervaller for ±1, ±2 og ±3 standardafvigelser
interval_1sd_tillid_2022 <- c(middelværdi_tillid_2022 - spredning_tillid_2022, middelværdi_tillid_2022 + spredning_tillid_2022)   # Ca. 68 % af observationer
interval_2sd_tillid_2022 <- c(middelværdi_tillid_2022 - 2 * spredning_tillid_2022, middelværdi_tillid_2022 + 2 * spredning_tillid_2022)  # Ca. 95 % af observationer
interval_3sd_tillid_2022 <- c(middelværdi_tillid_2022 - 3 * spredning_tillid_2022, middelværdi_tillid_2022 + 3 * spredning_tillid_2022)  # Ca. 99,7 % af observationer
# Udskriv intervaller
interval_1sd_tillid_2022
interval_2sd_tillid_2022
interval_3sd_tillid_2022
#skævhed
skew(data_2022$politisk_tillid, na.rm = TRUE)
# Beregn kurtosis for politisk tillid
kurtosis(data_2022$politisk_tillid, na.rm = TRUE)
# ------------------------------------------------------
# Politisk tillid 2001
# ------------------------------------------------------
#cleanup
data_2001 <- data_2001 |>
  mutate(across(c(V246, V171, V157, V155), ~na_if(., 8)))
# Reverse-kod de spørgsmål hvor 1 = høj tillid og 5 = lav tillid
data_2001 <- data_2001 |>
  mutate(
    V246_rev = 5 - V246,     # Demokratitilfredshed
    V171_rev = 5 - V171,     # Tillid til politikere
    V157_rev = 6 - V157,  # Tillid til embedsværk
    V155          # Direkte brugt, da den allerede er negativt formuleret
  )
# Skaler alle til 1-5 skala
data_2001 <- data_2001 |>
  mutate(
    tillid_V246 = scales::rescale(V246_rev, to = c(1, 5), from = c(1, 4)),
    tillid_V171 = scales::rescale(V171_rev, to = c(1, 5), from = c(1, 4)),
    tillid_V157 = scales::rescale(V157_rev, to = c(1, 5), from = c(1, 5)),
    tillid_V155 = scales::rescale(V155, to = c(1, 5), from = c(1, 5))
  )
cor(data_2001[, c("tillid_V246", "tillid_V171", "tillid_V157", "tillid_V155")], use = "complete.obs")
# Konstruér indeks: gennemsnit hvis højst én NA
data_2001 <- data_2001 |>
  rowwise() |>
  mutate(
    politisk_tillid_rå = mean(c_across(c(tillid_V246, tillid_V171, tillid_V157, tillid_V155)), na.rm = TRUE),
    tillid_mangler = sum(is.na(c_across(c(tillid_V246, tillid_V171, tillid_V157, tillid_V155)))),
    politisk_tillid = ifelse(tillid_mangler <= 1, politisk_tillid_rå, NA)
  ) |>
  ungroup()
# Opsummering og visualisering
summary(data_2001$politisk_tillid)
# Beregn typetal
typetal_2001 <- getmode(data_2001$politisk_tillid)
print(typetal_2001)
# Cronbach's alpha
psych::alpha(data_2001[, c("tillid_V246", "tillid_V171", "tillid_V157", "tillid_V155")])
# Visualisering
ggplot(data_2001, aes(x = politisk_tillid)) +
  geom_histogram(binwidth = 0.25, fill = "#3B82F6", color = "black", boundary = 1) +
  scale_x_continuous(breaks = 1:5, limits = c(1, 5)) +
  labs(
    title = "Fordeling af politisk tillid (2001)",
    x = "Politisk tillid (1 = lav tillid, 5 = høj tillid)",
    y = "Antal respondenter"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5, margin = margin(b = 10)),
    axis.title = element_text(size = 12),
    panel.grid.major = element_line(color = "#E5E7EB"),
    panel.grid.minor = element_blank()
  ) +
  ylim(0, 400)
# Beregn gennemsnit og standardafvigelse
middelvaerdi_2001 <- mean(data_2001$politisk_tillid, na.rm = TRUE)
spredning_2001 <- sd(data_2001$politisk_tillid, na.rm = TRUE)
# Beregn intervaller for ±1, ±2 og ±3 standardafvigelser
interval_1sd_2001 <- c(middelvaerdi_2001 - spredning_2001, middelvaerdi_2001 + spredning_2001)   # Ca. 68 % af observationer
interval_2sd_2001 <- c(middelvaerdi_2001 - 2 * spredning_2001, middelvaerdi_2001 + 2 * spredning_2001)  # Ca. 95 % af observationer
interval_3sd_2001 <- c(middelvaerdi_2001 - 3 * spredning_2001, middelvaerdi_2001 + 3 * spredning_2001)  # Ca. 99,7 % af observationer
# Udskriv intervaller
interval_1sd_2001
interval_2sd_2001
interval_3sd_2001
#skævhed
skew(data_2001$politisk_tillid, na.rm = TRUE)
# Beregn kurtosis for politisk tillid
kurtosis(data_2001$politisk_tillid, na.rm = TRUE)
# ------------------------------------------------------
# Intern Efficacy 2022
# ------------------------------------------------------
#cleanup
data_2022 <- data_2022 |>
  mutate(across(c(Q52_3, Q52_4, Q52_5), ~na_if(., 99)))
# Reverse-kod de spørgsmål hvor 1 = høj effektivitet og 5 = lav effektivitet
data_2022 <- data_2022 |>
  mutate(
    q52c = Q52_3, # Direkte brugt, da den allerede er negativt formuleret
    q52d = Q52_4, # Direkte brugt, da den allerede er negativt formuleret
    q52e = 6-Q52_5 #reverse
  )
# Konstruér indeks: gennemsnit hvis højst én NA
data_2022 <- data_2022 |>
  rowwise() |>
  mutate(
    intern_effektivitet_rå = mean(c_across(c(q52c, q52d, q52e)), na.rm = TRUE),
    intern_effektivitet_mangler = sum(is.na(c_across(c(q52c, q52d, q52e)))),
    intern_effektivitet = ifelse(intern_effektivitet_mangler <= 1,intern_effektivitet_rå, NA)
  ) |>
  ungroup()
# Opsummering og visualisering
summary(data_2022$intern_effektivitet)
# Beregn typetal for politisk tillid
typetal_effektivitet_2022 <- getmode(data_2022$intern_effektivitet)
print(typetal_effektivitet_2022)
# Cronbach's alpha
psych::alpha(data_2022[, c("q52c", "q52d", "q52e")])
# Visualisering
ggplot(data_2022, aes(x = intern_effektivitet)) +
  geom_histogram(binwidth = 0.25, fill = "#3B82F6", color = "black", boundary = 1) +
  scale_x_continuous(breaks = 1:5, limits = c(1, 5)) +
  labs(
    title = "Fordeling af intern politisk effektivitet (2022)",
    x = "Intern effektivitet (1 = lav, 5 = høj)",
    y = "Antal respondenter"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5, margin = margin(b = 10)),
    axis.title = element_text(size = 12),
    panel.grid.major = element_line(color = "#E5E7EB"),
    panel.grid.minor = element_blank()
  ) +
  ylim(0, 400)
# Beregn gennemsnit og standardafvigelse
middelværdi_effektivitet_2022 <- mean(data_2022$intern_effektivitet, na.rm = TRUE)
spredning_effektivitet_2022 <- sd(data_2022$intern_effektivitet, na.rm = TRUE)
# Beregn intervaller for ±1, ±2 og ±3 standardafvigelser
interval_1sd_effektivitet_2022 <- c(middelværdi_effektivitet_2022 - spredning_effektivitet_2022, middelværdi_effektivitet_2022 + spredning_effektivitet_2022)   # Ca. 68 % af observationer
interval_2sd_effektivitet_2022 <- c(middelværdi_effektivitet_2022 - 2 * spredning_effektivitet_2022, middelværdi_effektivitet_2022 + 2 * spredning_effektivitet_2022)  # Ca. 95 % af observationer
interval_3sd_effektivitet_2022 <- c(middelværdi_effektivitet_2022 - 3 * spredning_effektivitet_2022, middelværdi_effektivitet_2022 + 3 * spredning_effektivitet_2022)  # Ca. 99,7 % af observationer
# Udskriv intervaller
interval_1sd_effektivitet_2022
interval_2sd_effektivitet_2022
interval_3sd_effektivitet_2022
#skævhed
skew(data_2022$intern_effektivitet, na.rm = TRUE)
# Beregn kurtosis for politisk tillid
kurtosis(data_2022$intern_effektivitet, na.rm = TRUE)