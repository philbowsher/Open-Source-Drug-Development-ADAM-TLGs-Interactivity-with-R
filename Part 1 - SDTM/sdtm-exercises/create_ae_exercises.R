#' Name: AE domain
#'
#' Label: R program to create AE Domain
#'
#' Input
#' raw data: pharmaverseraw::ae_raw
#' study_controlled_terminology : sdtm_ct.csv
#' dm domain : pharmaversesdtm::dm
#' SDTM aCRF - 
#' https://github.com/pharmaverse/pharmaverseraw/blob/main/vignettes/articles/aCRFs/AdverseEvent_aCRF.pdf
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
    # pat_var = ,
    # raw_src =
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
  # Map AESDTH using assign_ct, raw_var=IT.AESDTH, tgt_var=AESDTH
  # If Yes then AESDTH = Y else Not submitted
  hardcode_no_ct(
    raw_dat = condition_add(ae_raw, IT.AESDTH == "Yes"),
    raw_var = "IT.AESDTH",
    tgt_var = "AESDTH",
    tgt_val = "Y",
    id_vars = oak_id_vars()
  ) %>%
  # hardcode_no_ct(
  #   raw_dat = ,
  #   raw_var = ,
  #   tgt_var = ,
  #   tgt_val = ,
  #   id_vars = oak_id_vars()
  # ) %>%
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
  # Map AESER using assign_ct, raw_var=IT.AESER, tgt_var=AESER
  # assign_ct(
  #   raw_dat = ,
  #   raw_var = ,
  #   tgt_var = ,
  #   ct_spec = ,
  #   ct_clst = ,
  #   id_vars =
  # ) %>%
  # Map AEACN using assign_ct, raw_var=IT.AEACN, tgt_var=AEACN
  # assign_ct(
  #   raw_dat = ,
  #   raw_var = ,
  #   tgt_var = ,
  #   ct_spec = ,
  #   ct_clst = ,
  #   id_vars =
  # ) %>%
  # Map AEREL using assign_ct, raw_var=IT.AEREL, tgt_var=AEREL
  # User-added codelist is in the ct,
  # assign_ct(
  #   raw_dat = ,
  #   raw_var = ,
  #   tgt_var = ,
  #   ct_spec = ,
  #   ct_clst = ,
  #   id_vars =
  # ) %>%
  # Map AESCAN using assign_ct, raw_var=AESCAN, tgt_var=AESCAN
  # assign_ct(
  #   raw_dat = ,
  #   raw_var = ,
  #   tgt_var = ,
  #   ct_spec = ,
  #   ct_clst = ,
  #   id_vars =
  # ) %>%
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
  # assign_datetime(
  #   raw_dat = ,
  #   raw_var = ,
  #   tgt_var = ,
  #   raw_fmt =
  # ) %>%
  # Map all coded terms using assign_no_ct algorithm
  assign_no_ct(
    raw_dat = ae_raw,
    raw_var = "AELLT",
    tgt_var = "AELLT",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = ae_raw,
    raw_var = "AELLTCD",
    tgt_var = "AELLTCD",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = ae_raw,
    raw_var = "AEDECOD",
    tgt_var = "AEDECOD",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = ae_raw,
    raw_var = "AEPTCD",
    tgt_var = "AEPTCD",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = ae_raw,
    raw_var = "AEHLT",
    tgt_var = "AEHLT",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = ae_raw,
    raw_var = "AEHLTCD",
    tgt_var = "AEHLTCD",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = ae_raw,
    raw_var = "AEHLGT",
    tgt_var = "AEHLGT",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = ae_raw,
    raw_var = "AEHLGTCD",
    tgt_var = "AEHLGTCD",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = ae_raw,
    raw_var = "AEBODSYS",
    tgt_var = "AEBODSYS",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = ae_raw,
    raw_var = "AEBDSYCD",
    tgt_var = "AEBDSYCD",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = ae_raw,
    raw_var = "AESOC",
    tgt_var = "AESOC",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = ae_raw,
    raw_var = "AESOCCD",
    tgt_var = "AESOCCD",
    id_vars = oak_id_vars()
  ) %>%
  dplyr::mutate(
    STUDYID = ae_raw$STUDY,
    DOMAIN = "AE",
    USUBJID = paste0("01-", ae_raw$PATNUM),
    AETERM = toupper(AETERM)
  ) %>%
  derive_seq(tgt_var = "AESEQ",
             rec_vars= c("USUBJID", "AETERM")) %>%
  # derive_study_day(
  #   sdtm_in = .,
  #   dm_domain = dm,
  #   tgdt = ,
  #   refdt = ,
  #   study_day_var =
  # ) %>%
  # derive_study_day(
  #   sdtm_in = .,
  #   dm_domain = dm,
  #   tgdt = ,
  #   refdt = ,
  #   study_day_var =
  # ) %>%
  select("STUDYID", "DOMAIN", "USUBJID", "AESEQ", "AETERM", "AELLT", "AELLTCD", "AEDECOD", "AEPTCD", "AEHLT", "AEHLTCD", "AEHLGT",
         "AEHLGTCD", "AEBODSYS", "AEBDSYCD", "AESOC", "AESOCCD", "AESEV", "AESER", "AEACN", "AEREL", "AEOUT", "AESCAN", "AESCONG", "AESDISAB",
         "AESDTH", "AESHOSP", "AESLIFE", "AESOD", "AEDTC", "AESTDTC", "AEENDTC", "AESTDY", "AEENDY")
