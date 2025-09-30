# ============================================================
# Almsee passerines — time-of-day effects on counts
# Data columns used: Tagesabschnitt (Früh/Mittag/Nachmittag),
# Uhrzeit (character times like '8:10'...), species *_scan / *_number,
# All_Individuen_scan / All_Individuen_number
# ============================================================

library(readr)
library(ggplot2)
library(ggpubr)
library(scales)
library(dplyr)

# --- Load ----------------------------------------------------
data_Almsee <- read.csv("Ergebnisse.csv", header = TRUE, sep = ",")

# --- Basic checks (avoid View/attach for reproducibility) ----
names(data_Almsee)
head(data_Almsee, 10)

# --- Factor settings -----------------------------------------
# Fix intended order for Tagesabschnitt and Uhrzeit so plots are stable
data_Almsee <- data_Almsee %>%
  mutate(
    Tagesabschnitt = factor(Tagesabschnitt, levels = c("Früh", "Mittag", "Nachmittag")),
    Uhrzeit = factor(
      Uhrzeit,
      levels = c(
        '8:10','8:20','8:30','8:40','8:50','9:00','9:10','9:20','9:30','9:40','9:50','10:00',
        '11:40','11:50','11:55','12:00','12:05','12:10','12:15','12:20','12:25','12:30','12:35','12:45',
        '14:55','15:05','15:10','15:15','15:20','15:25','15:30','15:35','15:40','15:45','15:50','15:55',
        '16:00','16:05','16:10','16:15','16:20','16:25','16:30','16:35','16:40','16:45','16:50','17:00'
      )
    )
  )

# --- Global: omnibus test (nonparametric) --------------------
# Kruskal–Wallis across time-of-day for total individuals
kruskal.test(All_Individuen_number ~ Tagesabschnitt, data = data_Almsee)

# ---------- Helper: tidy boxplot with optional time color ----
box_by_tagesabschnitt <- function(df, y, ylab_scan, color_by_time = TRUE) {
  ggplot(df, aes(x = Tagesabschnitt, y = .data[[y]])) +
    geom_boxplot(aes(color = if (color_by_time) Uhrzeit else NULL)) +
    labs(x = "", y = ylab_scan, color = if (color_by_time) "Uhrzeit" else NULL) +
    theme_minimal()
}

# ============================================================
# Kohlmeise (Great tit): scan vs continuous
# ============================================================

# Boxplots with Uhrzeit color overlay (scan / continuous)
q_scan <- box_by_tagesabschnitt(
  data_Almsee, "Kohlmeise_scan", "Kohlmeisen Anzahl (scan)", color_by_time = TRUE
)
q <- box_by_tagesabschnitt(
  data_Almsee, "Kohlmeise_number", "Kohlmeisen Anzahl (continuous)", color_by_time = TRUE
) + guides(color = "none")

# Clean ggpubr versions with Kruskal–Wallis annotation
a_scan <- ggboxplot(
  data_Almsee, x = "Tagesabschnitt", y = "Kohlmeise_scan",
  order = c("Früh", "Mittag", "Nachmittag"), fill = "Tagesabschnitt"
) +
  stat_compare_means(method = "kruskal.test") +
  theme(legend.position = "none") +
  labs(x = "", y = "Kohlmeisen Anzahl (scan)") +
  guides(color = "none", fill = "none")

a <- ggboxplot(
  data_Almsee, x = "Tagesabschnitt", y = "Kohlmeise_number",
  order = c("Früh", "Mittag", "Nachmittag"), fill = "Tagesabschnitt"
) +
  stat_compare_means(method = "kruskal.test") +
  theme(legend.position = "none") +
  labs(x = "", y = "Kohlmeisen Anzahl (continuous)") +
  guides(color = "none", fill = "none")

ggarrange(q, a, q_scan, a_scan, common.legend = TRUE)

# ============================================================
# Blaumeise (Blue tit): scan vs continuous
# ============================================================

w_scan <- box_by_tagesabschnitt(
  data_Almsee, "Blaumeise_scan", "Blaumeisen Anzahl (scan)", color_by_time = TRUE
)
w <- box_by_tagesabschnitt(
  data_Almsee, "Blaumeise_number", "Blaumeisen Anzahl (continuous)", color_by_time = TRUE
) + guides(color = "none")

b_scan <- ggboxplot(
  data_Almsee, x = "Tagesabschnitt", y = "Blaumeise_scan",
  order = c("Früh", "Mittag", "Nachmittag"), fill = "Tagesabschnitt"
) +
  stat_compare_means(method = "kruskal.test") +
  theme(legend.position = "none") +
  labs(x = "", y = "Blaumeisen Anzahl (scan)")

b <- ggboxplot(
  data_Almsee, x = "Tagesabschnitt", y = "Blaumeise_number",
  order = c("Früh", "Mittag", "Nachmittag"), fill = "Tagesabschnitt"
) +
  stat_compare_means(method = "kruskal.test") +
  theme(legend.position = "none") +
  labs(x = "", y = "Blaumeisen Anzahl (continuous)") +
  guides(color = "none", fill = "none")

ggarrange(w, b, w_scan, b_scan, common.legend = TRUE)

# ============================================================
# Sumpfmeise (Marsh tit): scan vs continuous
# ============================================================

e_scan <- box_by_tagesabschnitt(
  data_Almsee, "Sumpfmeise_scan", "Sumpfmeisen Anzahl (scan)", color_by_time = TRUE
)
e <- box_by_tagesabschnitt(
  data_Almsee, "Sumpfmeise_number", "Sumpfmeisen Anzahl (continuous)", color_by_time = TRUE
) + guides(color = "none")

c_scan <- ggboxplot(
  data_Almsee, x = "Tagesabschnitt", y = "Sumpfmeise_scan",
  order = c("Früh", "Mittag", "Nachmittag"), fill = "Tagesabschnitt"
) +
  stat_compare_means(method = "kruskal.test") +
  theme(legend.position = "none") +
  labs(x = "", y = "Sumpfmeisen Anzahl (scan)")

c <- ggboxplot(
  data_Almsee, x = "Tagesabschnitt", y = "Sumpfmeise_number",
  order = c("Früh", "Mittag", "Nachmittag"), fill = "Tagesabschnitt"
) +
  stat_compare_means(method = "kruskal.test") +
  theme(legend.position = "none") +
  labs(x = "", y = "Sumpfmeisen Anzahl (continuous)") +
  guides(color = "none", fill = "none")

ggarrange(e, c, e_scan, c_scan, common.legend = TRUE)

# ============================================================
# All species combined: scan vs continuous
# ============================================================

r_scan <- box_by_tagesabschnitt(
  data_Almsee, "All_Individuen_scan", "Alle Anzahl (scan)", color_by_time = TRUE
)
r <- box_by_tagesabschnitt(
  data_Almsee, "All_Individuen_number", "Alle Anzahl (continuous)", color_by_time = TRUE
) + guides(color = "none")

d_scan <- ggboxplot(
  data_Almsee, x = "Tagesabschnitt", y = "All_Individuen_scan",
  order = c("Früh", "Mittag", "Nachmittag"), fill = "Tagesabschnitt"
) +
  stat_compare_means(method = "kruskal.test") +
  theme(legend.position = "none") +
  labs(x = "", y = "Alle Anzahl (scan)") +
  guides(color = "none", fill = "none")

d <- ggboxplot(
  data_Almsee, x = "Tagesabschnitt", y = "All_Individuen_number",
  order = c("Früh", "Mittag", "Nachmittag"), fill = "Tagesabschnitt"
) +
  stat_compare_means(method = "kruskal.test") +
  theme(legend.position = "none") +
  labs(x = "", y = "Alle Anzahl (continuous)") +
  guides(color = "none", fill = "none")

ggarrange(r, d, r_scan, d_scan, common.legend = TRUE)

# ============================================================
# Compact Kruskal panel for continuous sampling counts
# ============================================================

a <- ggboxplot(
  data_Almsee, x = "Tagesabschnitt", y = "Kohlmeise_number",
  order = c("Früh", "Mittag", "Nachmittag"), fill = "Tagesabschnitt"
) +
  stat_compare_means(method = "kruskal.test") +
  theme(legend.position = "none") +
  labs(x = "", y = "Kohlmeisen Anzahl (continuous sampling)")

b <- ggboxplot(
  data_Almsee, x = "Tagesabschnitt", y = "Blaumeise_number",
  order = c("Früh", "Mittag", "Nachmittag"), fill = "Tagesabschnitt"
) +
  stat_compare_means(method = "kruskal.test") +
  theme(legend.position = "none") +
  labs(x = "", y = "Blaumeisen Anzahl (continuous sampling)")

c <- ggboxplot(
  data_Almsee, x = "Tagesabschnitt", y = "Sumpfmeise_number",
  order = c("Früh", "Mittag", "Nachmittag"), fill = "Tagesabschnitt"
) +
  stat_compare_means(method = "kruskal.test") +
  theme(legend.position = "none") +
  labs(x = "", y = "Sumpfmeisen Anzahl (continuous sampling)")

d <- ggboxplot(
  data_Almsee, x = "Tagesabschnitt", y = "All_Individuen_number",
  order = c("Früh", "Mittag", "Nachmittag"), fill = "Tagesabschnitt"
) +
  stat_compare_means(method = "kruskal.test") +
  theme(legend.position = "none") +
  labs(x = "", y = "Alle Anzahl (continuous sampling)")

ggarrange(a, b, c, d, common.legend = TRUE)
