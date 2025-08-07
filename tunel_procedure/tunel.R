list.files()
df = read.csv('mepp_tunel_count.csv')


# ── 1.  packages ───────────────────────────────────────────────
library(readr)    # read_csv
library(dplyr)    # data wrangling
library(tidyr)    # pivot_wider
library(ggplot2)  # plotting
library(readxl)
library(stringr)

# ── 2.  load the table ─────────────────────────────────────────
df <- read_csv("mepp_tunel_count.csv")      # columns: sample, count
df = read_xlsx('250807_mepp_tunel_count.xlsx')

# ── 3.  parse sample name and channel (dapi / fitc) ────────────
df <- df %>% 
  mutate(
    sample   = sub("_.*$", "", Slice), 
    group   = str_extract(Slice, "^[A-Za-z]+"),                     # fk1 / mepp2 / th1 …
    channel = ifelse(grepl("dapi", Slice, TRUE), "dapi",
                     ifelse(grepl("fitc", Slice, TRUE), "fitc", NA))
  )

# ── 4.  summarise counts per group and compute %FITC ───────────
summary_tbl <- df %>% 
  group_by(sample, channel) %>% 
  summarise(total = sum(Count), .groups = "drop") %>% 
  pivot_wider(names_from = channel, values_from = total, values_fill = 0) %>% 
  mutate(proportion = 100 * fitc / dapi)                    # percent FITC+ nuclei

print(summary_tbl)
write.csv(summary_tbl,'summary.csv')
