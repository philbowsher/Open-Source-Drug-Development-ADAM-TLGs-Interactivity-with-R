#' Name: DM domain
#'
#' Label: R program to create DM Domain
#' 
#' Input 
#' raw data: 
#' pharmaverseraw::dm_raw
#' pharmaverseraw::ec_raw
#' pharmaverseraw::ds_raw
#' SDTM aCRF - 
#' https://github.com/pharmaverse/pharmaverseraw/blob/main/vignettes/articles/aCRFs/Demographics_aCRF.pdf
#' https://github.com/pharmaverse/pharmaverseraw/blob/main/vignettes/articles/aCRFs/Exposure_as_collected_aCRF.pdf
#' https://github.com/pharmaverse/pharmaverseraw/blob/main/vignettes/articles/aCRFs/Subject_Disposition_aCRF.pdf 
#' 
#' study_controlled_terminology : sdtm_ct.csv
#'
library(sdtm.oak)
library(dplyr)

# Read CT Specification
study_ct <- read.csv("./Part 1 - SDTM/sdtm-specs/sdtm_ct.csv")

# Read in raw data
ds_raw <- pharmaverseraw::ds_raw 
ex_raw <- pharmaverseraw::ec_raw
dm_raw <- pharmaverseraw::dm_raw

ds_raw <- admiral::convert_blanks_to_na(ds_raw)
ex_raw <- admiral::convert_blanks_to_na(ex_raw)
dm_raw <- admiral::convert_blanks_to_na(dm_raw)

# Derive oak_id_vars
ds_raw <- ds_raw %>%
  generate_oak_id_vars(
    pat_var = "PATNUM",
    raw_src = "ds_raw"
  )

ex_raw <- ex_raw %>%
  generate_oak_id_vars(
    pat_var = "PATNUM",
    raw_src = "ex_raw"
  )

dm_raw <- dm_raw %>%
  generate_oak_id_vars(
    pat_var = "PATNUM",
    raw_src = "dm_raw"
  )

# Create reference dates configuration file
# Data frame which has the details of the variables to be used for the 
# calculation of reference dates. 
# Should have columns listed below: 
#
# raw_dataset_name : Name of the raw dataset. 
# date_var : Date variable name from the raw dataset. 
# time_var : Time variable name from the raw dataset. 
# dformat : Format of the date collected in raw data. 
# tformat: Format of the time collected in raw data. 
# sdtm_var_name : Reference variable name.

ref_date_conf_df <- tibble::tribble(
  ~raw_dataset_name, ~date_var,     ~time_var,      ~dformat,      ~tformat, ~sdtm_var_name,
  "ex_raw",       "IT.ECSTDAT", NA_character_, "dd-mmm-yyyy", NA_character_,     "RFXSTDTC",
  "ex_raw",       "IT.ECENDAT", NA_character_, "dd-mmm-yyyy", NA_character_,     "RFXENDTC",
  "ds_raw",       "IT.DSSTDAT", NA_character_,  "mm-dd-yyyy", NA_character_,      "RFSTDTC",
  "ex_raw",       "IT.ECENDAT", NA_character_, "dd-mmm-yyyy", NA_character_,      "RFENDTC",
  "dm_raw",            "IC_DT", NA_character_,  "mm/dd/yyyy", NA_character_,      "RFICDTC",
  "ds_raw",          "DSDTCOL",     "DSTMCOL",  "mm-dd-yyyy",         "H:M",     "RFPENDTC",
  "ds_raw",          "DEATHDT", NA_character_,  "mm/dd/yyyy", NA_character_,       "DTHDTC"
)

# Create DM domain.
dm <-
  # Map Topic variable SUBJID using assign_no_ct
  assign_no_ct(
    raw_dat = dm_raw,
    raw_var = "PATNUM",
    tgt_var = "SUBJID",
    id_vars = oak_id_vars()
  ) %>%
  #dplyr::mutate(SUBJID = substr(dm_raw$PATNUM, 5, 8)) %>%
  # Map AGE using assign_no_ct
  assign_no_ct(
    raw_dat = dm_raw,
    raw_var = "IT.AGE",
    tgt_var = "AGE",
    id_vars = oak_id_vars()
  ) %>%
  # Map AGEU using hardcode_ct
  hardcode_ct(
    raw_dat = dm_raw,
    raw_var = "IT.AGE",
    tgt_var = "AGEU",
    tgt_val = "Year",
    ct_spec = study_ct,
    ct_clst = "C66781",
    id_vars = oak_id_vars()
  ) %>%
  # Map SEX using assign_ct
  assign_ct(
    raw_dat = dm_raw,
    raw_var = "IT.SEX",
    tgt_var = "SEX",
    ct_spec = study_ct,
    ct_clst = "C66731",
    id_vars = oak_id_vars()
  ) %>%
  # Map ETHNIC using assign_ct
  assign_ct(
    raw_dat = dm_raw,
    raw_var = "IT.ETHNIC",
    tgt_var = "ETHNIC",
    ct_spec = study_ct,
    ct_clst = "C66790",
    id_vars = oak_id_vars()
  ) %>%
  # Map RACE using assign_ct
  assign_ct(
    raw_dat = dm_raw,
    raw_var = "IT.RACE",
    tgt_var = "RACE",
    ct_spec = study_ct,
    ct_clst = "C74457",
    id_vars = oak_id_vars()
  ) %>%
  # Map ARM using assign_ct
  assign_ct(
    raw_dat = dm_raw,
    raw_var = "PLANNED_ARM",
    tgt_var = "ARM",
    ct_spec = study_ct,
    ct_clst = "ARM",
    id_vars = oak_id_vars()
  ) %>%
  # Map ARMCD using assign_no_ct
  assign_no_ct(
    raw_dat = dm_raw,
    raw_var = "PLANNED_ARMCD",
    tgt_var = "ARMCD",
    id_vars = oak_id_vars()
  ) %>%
  # Map ACTARM using assign_ct
  assign_ct(
    raw_dat = dm_raw,
    raw_var = "ACTUAL_ARM",
    tgt_var = "ACTARM",
    ct_spec = study_ct,
    ct_clst = "ARM",
    id_vars = oak_id_vars()
  ) %>%
  # Map ACTARMCD using assign_no_ct
  assign_no_ct(
    raw_dat = dm_raw,
    raw_var = "ACTUAL_ARMCD",
    tgt_var = "ACTARMCD",
    id_vars = oak_id_vars()
  ) %>%
  # Map DMDTC using assign_datetime
  assign_datetime(
    raw_dat = dm_raw,
    raw_var = "COL_DT",
    tgt_var = "DMDTC",
    raw_fmt = c("m/d/y"),
    id_vars = oak_id_vars()
  ) %>%
  mutate(STUDYID = dm_raw$STUDY,
         DOMAIN = "DM",
         USUBJID = paste0("01-", dm_raw$PATNUM),
         COUNTRY = dm_raw$COUNTRY) %>%
  # Derive RFSTDTC using oak_cal_ref_dates
  # Variable `RFSTDTC` is the reference Start Date/time for the subject in ISO 8601 
  # character format. Usually equivalent to date/time when subject was first exposed
  # to study treatment. So as specified in the reference date configuration file, 
  # we need to calculate the minimum date of  the `IT.ECSTDAT` for each subject 
  # from the `ec_raw` dataset. Therefore, in `min_max` parameter, "min" is selected 
  # for the calculation.
  
  # Users can pass all applicable raw datasets to raw_source parameter
  oak_cal_ref_dates(ds_in = .,
                    der_var = "RFSTDTC",
                    min_max = "min",
                    ref_date_config_df = ref_date_conf_df,
                    raw_source = list(
                      ex_raw = ex_raw,
                      ds_raw = ds_raw,
                      dm_raw = dm_raw
                    )
  ) %>%
  # Derive RFENDTC using oak_cal_ref_dates
  # Equivalent to the date/time when subject was determined to have ended the trial,
  # and often equivalent to date/time of last exposure to study treatment. we need 
  # to calculate the  maximum date of the `IT.ECENDAT` for each subject from the 
  # `ec_raw` dataset. Therefore, in `min_max` parameter, "max" is selected for the calculation.
  
  # Users can pass just pass the one applicable raw datasets to raw_source parameter
  oak_cal_ref_dates(ds_in = .,
                    der_var = "RFENDTC",
                    min_max = "max",
                    ref_date_config_df = ref_date_conf_df,
                    raw_source = list(
                      ex_raw = ex_raw
                    )
  ) %>%
  # Derive RFXSTDTC using oak_cal_ref_dates
  # oak_cal_ref_dates(ds_in = .,
  #                   der_var = ,
  #                   min_max = ,
  #                   ref_date_config_df = ref_date_conf_df,
  #                   raw_source = list(
  #                     ex_raw = ex_raw,
  #                     ds_raw = ds_raw,
  #                     dm_raw = dm_raw
  #                   )
  # ) %>%
  # Derive RFXENDTC using oak_cal_ref_dates
  # oak_cal_ref_dates(ds_in = .,
  #                   der_var = ,
  #                   min_max = ,
  #                   ref_date_config_df = ref_date_conf_df,
  #                   raw_source = list(
  #                     ex_raw = ex_raw,
  #                     ds_raw = ds_raw,
  #                     dm_raw = dm_raw
  #                   )
  # ) %>%
  # Derive RFICDTC using oak_cal_ref_dates
  oak_cal_ref_dates(ds_in = .,
                    der_var = "RFICDTC",
                    min_max = "min",
                    ref_date_config_df = ref_date_conf_df,
                    raw_source = list(
                      ex_raw = ex_raw,
                      ds_raw = ds_raw,
                      dm_raw = dm_raw
                    )
  ) %>%
  # Derive RFPENDTC using oak_cal_ref_dates
  oak_cal_ref_dates(ds_in = .,
                    der_var = "RFPENDTC",
                    min_max = "max",
                    ref_date_config_df = ref_date_conf_df,
                    raw_source = list(
                      ex_raw = ex_raw,
                      ds_raw = ds_raw,
                      dm_raw = dm_raw
                    )
  ) %>%
  # Map DTHDTC using oak_cal_ref_dates
  oak_cal_ref_dates(ds_in = .,
                    der_var = "DTHDTC",
                    min_max = "min",
                    ref_date_config_df = ref_date_conf_df,
                    raw_source = list(
                      ex_raw = ex_raw,
                      ds_raw = ds_raw,
                      dm_raw = dm_raw
                    )
  ) %>%
  dplyr::mutate(DTHFL = dplyr::if_else(is.na(DTHDTC), NA_character_, "Y")) %>%
  # Derive DMDY
  # derive_study_day(
  #   sdtm_in = .,
  #   dm_domain = .,
  #   tgdt = ,
  #   refdt = ,
  #   study_day_var = 
  # ) %>%
  select(
    "STUDYID", "DOMAIN", "USUBJID", "SUBJID",  "RFSTDTC", "RFENDTC", "RFXSTDTC", "RFXENDTC", "RFICDTC", "RFPENDTC", 
    "DTHDTC", "DTHFL", "AGE", "AGEU", "SEX", "RACE", "ETHNIC", "ARMCD", "ARM", "ACTARMCD", "ACTARM",
    "COUNTRY", "DMDTC", "DMDY"
  )

