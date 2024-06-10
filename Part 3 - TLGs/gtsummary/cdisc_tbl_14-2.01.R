library(tidyverse)
library(gtsummary)
library(gt)

# Obtain the .xpt data from the CDISC SDTM-ADAM Pilot Project
# - use `haven::read_xpt()` to generate a data frame from the file
adsl_orig <-
  haven::read_xpt(
    "https://raw.githubusercontent.com/cdisc-org/sdtm-adam-pilot-project/master/updated-pilot-submission-package/900172/m5/datasets/cdiscpilot01/analysis/adam/datasets/adsl.xpt"
  )


# Generate factors for some variables
# - 'TRT01P', 'SEX', 'AGEGR1', 'RACE', 'BMIBLGR1', and 'DURDSGR1'
adsl <-
  adsl_orig |>
  dplyr::mutate(
    TRT01P = factor(
      TRT01P,
      levels = c("Placebo","Xanomeline Low Dose","Xanomeline High Dose"),
      labels = c("Placebo", "Low Dose","High Dose")
    ),
    SEX = forcats::fct_rev(factor(SEX, labels = c("Female","Male"))),
    AGEGR1 = factor(
      AGEGR1,
      level = c("<65","65-80",">80"),
      labels = c("<65 yrs","65-80 yrs",">80 yrs")
    ),
    RACE = stringr::str_to_title(factor(RACE)),
    BMIBLGR1 = factor(BMIBLGR1, levels = c("<25","25-<30",">=30") ),
    DURDSGR1 = factor(DURDSGR1, labels = c("<12 months", ">=12 months")
    )
  )

# Use `gtsummary::tbl_summary()` along with its specialized
# table-building functions to generate a basic summary of
# demographic and baseline characteristics
gtsummary_tbl <-
  tbl_summary(
    data = adsl,
    by = TRT01P,
    include = c(
      AGE, AGEGR1, SEX, RACE, MMSETOT, DURDIS, DURDSGR1,
      EDUCLVL, WEIGHTBL, HEIGHTBL, BMIBL, BMIBLGR1
    ),
    type = list(all_continuous() ~ "continuous2"),
    statistic = list(
      all_continuous() ~ c(
        "{N_nonmiss}",
        "{mean}",
        "{sd}",
        "{median}",
        "{min}",
        "{max}"
      ),
      all_categorical() ~ c("{n} ({p}%)")
    ),
    label = list(
      AGE ~ "Age (y)",
      AGEGR1 ~ "",
      SEX ~ "Sex",
      RACE ~ "Race (Origin)",
      MMSETOT ~ "MMSE",
      DURDIS ~ "Duration of disease",
      DURDSGR1 ~ "",
      BMIBL ~ "Baseline BMI",
      BMIBLGR1 ~ ""
    ),
    digits = list(
      all_continuous() ~ c(0, 1, 2, 1, 1, 1),
      all_categorical() ~ c(0, 0)
    ),
    sort = list(RACE ~ "frequency"),
    missing = "no",
    missing_text = "Missing"
  ) |>
  modify_header(
    label = "",
    all_stat_cols(stat_0 = FALSE) ~ "**{level}<br>(N={n})**"
  ) |>
  add_overall(
    last = TRUE,
    col_label =  "**Total<br>(N={N})**"
  ) |>
  add_p(
    test = list(
      all_continuous() ~ "aov",
      all_categorical() ~ "chisq.test"
    ),
    pvalue_fun = function(x) style_pvalue(x, digits = 3)
  ) |>
  modify_spanning_header(c(stat_2, stat_3) ~ "**Xanomeline**") |>
  modify_footnote(
    all_stat_cols() ~ NA,
    p.value ~ "P-values are results of ANOVA treatment group comparison for
    continuous variable and Pearsons chisquare test for categorical variables."
  ) |>
  modify_table_styling(
    columns = label,
    rows = variable == "DURDIS" & row_type == "label",
    footnote = "Duration of disease is computed as months between date of
    enrollment and date of onset of the first definite symptoms of Alzheimers
    disease."
  ) |>
  modify_column_alignment(all_stat_cols(), align = "right") |>
  modify_table_body(
    ~ .x |>
      mutate(label = case_match(
        label,
        "Minimum" ~ "Min",
        "Maximum" ~ "Max",
        "N" ~ "n",
        .default = label
      )
    )
  ) |>
  modify_caption(
    caption = "**Table 14-2.01<br>Summary of Demographic and Baseline
    Characteristics**"
  )

# Have a look at the table
gtsummary_tbl

# Once you are finished making the gtsummary table, you can continue
# to make adjustments using gt()
gt_tbl <-
  gtsummary_tbl |>
  as_gt() |> # <- using this allows you to make more changes via gt
  opt_vertical_padding(scale = 0.5) |>
  opt_table_font(font = google_font("IBM Plex Sans")) |>
  opt_stylize(
    style = 1,
    color = "cyan",
    add_row_striping = FALSE
  ) |>
  text_transform(
    locations = cells_body(),
    fn = function(x) {
      gsub("-", "&mdash;", x)
    }
  ) |>
  rm_caption() |>
  tab_header(
    title = "Table 14-2.01",
    subtitle = "Summary of Demographic and Baseline Characteristics"
  ) |>
  tab_options(
    heading.title.font.size = px(24),
    heading.subtitle.font.size = px(18)
  ) |>
  opt_align_table_header(align = "left") |>
  tab_style_body(
    style = cell_fill(color = "bisque"),
    columns = p.value,
    fn = function(x) !is.na(x)
  )

# Print the gt table
gt_tbl

# If you need to have the finalized cells checked through a
# separate process, you can extract the cells into a new
# data frame object

df_from_gt <-
  gt_tbl |>
  extract_body()

df_from_gt
