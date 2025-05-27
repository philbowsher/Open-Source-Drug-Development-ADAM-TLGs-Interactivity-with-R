#' Name: AE domain
#'
#' Label: R program to create AE Domain
#'
#' Input
#' raw data: pharmaverseraw::ae_raw
#' study_controlled_terminology : sdtm_ct.csv
#' dm domain : pharmaversesdtm::dm
#'
library(sdtm.oak)
library(dplyr)


# Read CT Specification
study_ct <- read.csv("./Part 1 - SDTM/sdtm-specs/sdtm_ct.csv")

# Read in raw data
ae_raw <- pharmaverseraw::ae_raw

ae_raw <- admiral::convert_blanks_to_na(ae_raw)

# Derive oak_id_vars
ae_raw <- ae_raw %>%
  generate_oak_id_vars(
    pat_var = "PATNUM",
    raw_src = "ae_raw"
  )

# Read in DM domain to derive study day
dm <- pharmaversesdtm::dm

dm <- admiral::convert_blanks_to_na(dm)

# Create AE domain. The first step in creating AE domain is to create the topic variable
ae <-
  # Derive topic variable
  # Map AETERM using assign_no_ct, raw_var=IT.AETERM, tgt_var=AETERM
  assign_no_ct(
    raw_dat = ae_raw,
    raw_var = "IT.AETERM",
    tgt_var = "AETERM",
    id_vars = oak_id_vars()
  ) %>%
  # Map AEOUT using assign_ct, raw_var=AEOUTCOME, tgt_var=AEOUT
  assign_ct(
    raw_dat = ae_raw,
    raw_var = "AEOUTCOME",
    tgt_var = "AEOUT",
    ct_spec = study_ct,
    ct_clst = "C66768",
    id_vars = oak_id_vars()
  ) %>%
  # Map AESEV using hardcode_ct and condition_add, raw_var=IT.AESEV, tgt_var=AESEV
  # if "Mild Adverse Event" then AE.AESEV = "MILD" 
  # else if "Moderate Adverse Event" then AE.AESEV = "MODERATE" , 
  # else if "Severe Adverse Event" then AE.AESEV = "SEVERE"
  hardcode_ct(
    raw_dat = condition_add(ae_raw, IT.AESEV == "Mild Adverse Event"),
    raw_var = "IT.AESEV",
    tgt_var = "AESEV",
    tgt_val = "MILD",
    ct_spec = study_ct,
    ct_clst = "C66769",
    id_vars = oak_id_vars()
  )  %>%
  hardcode_ct(
    raw_dat = condition_add(ae_raw, IT.AESEV == "Moderate Adverse Event"),
    raw_var = "IT.AESEV",
    tgt_var = "AESEV",
    tgt_val = "MODERATE",
    ct_spec = study_ct,
    ct_clst = "C66769",
    id_vars = oak_id_vars()
  )  %>%
  hardcode_ct(
    raw_dat = condition_add(ae_raw, IT.AESEV == "Severe Adverse Event"),
    raw_var = "IT.AESEV",
    tgt_var = "AESEV",
    tgt_val = "SEVERE",
    ct_spec = study_ct,
    ct_clst = "C66769",
    id_vars = oak_id_vars()
  )  %>%
  # Map AESER using assign_ct, raw_var=IT.AESER, tgt_var=AESER
  assign_ct(
    raw_dat = ae_raw,
    raw_var = "IT.AESER",
    tgt_var = "AESER",
    ct_spec = study_ct,
    ct_clst = "C66742",
    id_vars = oak_id_vars()
  ) %>%
  # Map AEACN using assign_ct, raw_var=IT.AEACN, tgt_var=AEACN
  assign_ct(
    raw_dat = ae_raw,
    raw_var = "IT.AEACN",
    tgt_var = "AEACN",
    ct_spec = study_ct,
    ct_clst = "C66767",
    id_vars = oak_id_vars()
  ) %>%
  # Map AEREL using assign_ct, raw_var=IT.AEREL, tgt_var=AEREL
  # User-added codelist is in the ct,
  assign_ct(
    raw_dat = ae_raw,
    raw_var = "IT.AEREL",
    tgt_var = "AEREL",
    ct_spec = study_ct,
    ct_clst = "AEREL",
    id_vars = oak_id_vars()
  ) %>%
  # Map AESCAN using assign_ct, raw_var=AESCAN, tgt_var=AESCAN
  assign_ct(
    raw_dat = ae_raw,
    raw_var = "AESCAN",
    tgt_var = "AESCAN",
    ct_spec = study_ct,
    ct_clst = "C66742",
    id_vars = oak_id_vars()
  ) %>%
  # Map AESCNO using assign_ct, raw_var=AESCNO, tgt_var=AESCNO
  assign_ct(
    raw_dat = ae_raw,
    raw_var = "AESCNO",
    tgt_var = "AESCONG",
    ct_spec = study_ct,
    ct_clst = "C66742",
    id_vars = oak_id_vars()
  ) %>%
  # Map AEDIS using assign_ct, raw_var=AEDIS, tgt_var=AEDIS
  assign_ct(
    raw_dat = ae_raw,
    raw_var = "AEDIS",
    tgt_var = "AESDISAB",
    ct_spec = study_ct,
    ct_clst = "C66742",
    id_vars = oak_id_vars()
  ) %>%
  # Map AESDTH using assign_ct, raw_var=IT.AESDTH, tgt_var=AESDTH
  # If Yes then AESDTH = Y else Not submitted
  hardcode_no_ct(
    raw_dat = condition_add(ae_raw, IT.AESDTH == "Yes"),
    raw_var = "IT.AESDTH",
    tgt_var = "AESDTH",
    tgt_val = "Y",
    id_vars = oak_id_vars()
  ) %>%
  hardcode_no_ct(
    raw_dat = condition_add(ae_raw, IT.AESDTH != "Yes"),
    raw_var = "IT.AESDTH",
    tgt_var = "AESDTH",
    tgt_val = "Not Submitted",
    id_vars = oak_id_vars()
  ) %>%
  # Map AESHOSP using assign_ct, raw_var=IT.AESHOSP, tgt_var=AESHOSP
  assign_ct(
    raw_dat = ae_raw,
    raw_var = "IT.AESHOSP",
    tgt_var = "AESHOSP",
    ct_spec = study_ct,
    ct_clst = "C66742",
    id_vars = oak_id_vars()
  ) %>%
  # Map AESLIFE using assign_ct, raw_var=IT.AESLIFE, tgt_var=AESLIFE
  assign_ct(
    raw_dat = ae_raw,
    raw_var = "IT.AESLIFE",
    tgt_var = "AESLIFE",
    ct_spec = study_ct,
    ct_clst = "C66742",
    id_vars = oak_id_vars()
  ) %>%
  # Map AESOD using assign_ct, raw_var=AESOD, tgt_var=AESOD
  assign_ct(
    raw_dat = ae_raw,
    raw_var = "AESOD",
    tgt_var = "AESOD",
    ct_spec = study_ct,
    ct_clst = "C66742",
    id_vars = oak_id_vars()
  ) %>%
  # Map AEDTC using assign_datetime, raw_var=AEDTCOL
  assign_datetime(
    raw_dat = ae_raw,
    raw_var = "AEDTCOL",
    tgt_var = "AEDTC",
    raw_fmt = c("m/d/y")
  ) %>%
  # Map AESTDTC using assign_datetime, raw_var=IT.AESTDAT
  assign_datetime(
    raw_dat = ae_raw,
    raw_var = "IT.AESTDAT",
    tgt_var = "AESTDTC",
    raw_fmt = c(list(c("m/d/y", "yyyy"))),
    id_vars = oak_id_vars()
  ) %>%
  # Map AEENDTC using assign_datetime, raw_var=IT.AEENDAT
  assign_datetime(
    raw_dat = ae_raw,
    raw_var = "IT.AEENDAT",
    tgt_var = "AEENDTC",
    raw_fmt = c("m/d/y"),
    id_vars = oak_id_vars()
  ) %>%
  dplyr::mutate(
    STUDYID = ae_raw$STUDY,
    DOMAIN = "AE",
    USUBJID = paste0("01-", ae_raw$PATNUM),
    AELLT = ae_raw$AELLT,
    AELLTCD = ae_raw$AELLTCD,
    AEDECOD = ae_raw$AEDECOD,
    AEPTCD = ae_raw$AEPTCD,
    AEHLT = ae_raw$AEHLT,
    AEHLTCD = ae_raw$AEHLTCD,
    AEHLGT = ae_raw$AEHLGT,
    AEHLGTCD = ae_raw$AEHLGTCD,
    AEBODSYS = ae_raw$AEBODSYS,
    AEBDSYCD = ae_raw$AEBDSYCD,
    AESOC = ae_raw$AESOC,
    AESOCCD = ae_raw$AESOCCD,
    AETERM = toupper(AETERM)
  ) %>%
  derive_seq(tgt_var = "AESEQ",
             rec_vars= c("USUBJID", "AETERM")) %>%
  derive_study_day(
    sdtm_in = .,
    dm_domain = dm,
    tgdt = "AESTDTC",
    refdt = "RFXSTDTC",
    study_day_var = "AESTDY"
  ) %>%
  derive_study_day(
    sdtm_in = .,
    dm_domain = dm,
    tgdt = "AEENDTC",
    refdt = "RFXENDTC",
    study_day_var = "AEENDY"
  ) %>%
  select("STUDYID", "DOMAIN", "USUBJID", "AESEQ", "AETERM", "AELLT", "AELLTCD", "AEDECOD", "AEPTCD", "AEHLT", "AEHLTCD", "AEHLGT",
         "AEHLGTCD", "AEBODSYS", "AEBDSYCD", "AESOC", "AESOCCD", "AESEV", "AESER", "AEACN", "AEREL", "AEOUT", "AESCAN", "AESCONG", "AESDISAB",
         "AESDTH", "AESHOSP", "AESLIFE", "AESOD", "AEDTC", "AESTDTC", "AEENDTC", "AESTDY", "AEENDY")
