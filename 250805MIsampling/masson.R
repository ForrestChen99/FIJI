# ── 1.  Load packages ─────────────────────────────────────────────
library(readr)    # for read_csv()
library(dplyr)    # for mutate(), across(), etc.
library(tibble)   # for rowwise() (included with dplyr ≥1.1)
library(readxl)


df <- read_xlsx("masson_250801.xlsx")   # <- put the correct filename / path here

angle_small <- function(mx, my, p1x, p1y, p2x, p2y) {
  v1 <- cbind(p1x - mx, p1y - my)
  v2 <- cbind(p2x - mx, p2y - my)
  dot   <- rowSums(v1 * v2)
  mag   <- sqrt(rowSums(v1^2)) * sqrt(rowSums(v2^2))
  ratio <- pmin(pmax(dot / mag, -1), 1)        # clamp for safety
  acos(ratio) * 180 / pi
}

# ── compute both the small-arc angle and the final angle ───────────
df_angle <- df %>% 
  mutate(
    angle_small = angle_small(`middle X`, `middle Y`,
                              `P1 X`,     `P1 Y`,
                              `P2 X`,     `P2 Y`),
    
    # if largeArc == TRUE, take the complement
    angle_final = if_else(largeArc, 360 - angle_small, angle_small),
    
    percent_circle = angle_final / 360 * 100
  )

# ── preview ────────────────────────────────────────────────────────
df_angle %>% 
  select(ID, angle_small, largeArc, angle_final, percent_circle) %>% 
  print()

write_csv(df_angle, "heart_angles_results.csv")

###statistics
## ── 2. Quick sanity check ────────────────────────────────────────────────
df = df_angle

df %>% count(Group)
summary(df$percent_circle)

## ── 3.  Choose a global test  ────────────────────────────────────────────
# Normality in each group (Shapiro).  Small n?  keep it descriptive only.
norm_p <- df %>% 
  group_by(Group) %>% 
  summarise(p = shapiro.test(percent_circle)$p.value)

use_kw <- any(norm_p$p < 0.05)    # TRUE  ⇒  switch to Kruskal-Wallis

if (use_kw) {
  # Kruskal-Wallis
  global_p <- kruskal.test(percent_circle ~ Group, data = df)$p.value
  
  pair_p <- pairwise.wilcox.test(df$percent_circle, df$Group,
                                 p.adjust.method = "BH")
  
  cat("\n▶ Kruskal-Wallis  p =", signif(global_p, 3), "\n")
  print(pair_p$p.value)
  
  comparison_method <- "wilcox.test"
} else {
  # One-way ANOVA
  fit  <- aov(percent_circle ~ Group, data = df)
  global_p <- summary(fit)[[1]][["Pr(>F)"]][1]
  
  tuk  <- TukeyHSD(fit)
  print(tuk)
  
  comparison_method <- "tukey"
}

