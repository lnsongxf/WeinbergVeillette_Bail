if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, broom, argparse, AER, magrittr, lubridate)

## Arguments are provided from the command line

parser <- ArgumentParser()
parser$add_argument("--input", type="character")
parser$add_argument("--output", type="character")

arguments <- parser$parse_args()


## Import Data ----
df <- read_csv(arguments$input,
               col_types = cols(
                 FACILITY = col_factor(NULL),
                 OFFENSE = col_factor(NULL),
                 RACE = col_factor(NULL),
                 GENDER = col_factor(NULL))) %>%
  rename(bond_amount = `BOND AMOUNT`, download_date = `DOWNLOAD DATE`, admit_date = `LATEST ADMISSION DATE`) %>%
  mutate(admit_date = mdy(admit_date), download_date = mdy(download_date), year = year(admit_date))


## Clean Data----
print("Sorting by date...")
ct <- df %>%
  # Sort by download date
  arrange(download_date) %>%
  # Number of days in detention
  group_by(IDENTIFIER, admit_date) %>%
  mutate(detention_duration = n(), log_bond_amount = log(bond_amount)) %>%
  ungroup()

# Keep only one observation for each arrest
ct <- distinct(ct, IDENTIFIER, admit_date, .keep_all = TRUE)

# number of times arrested
ct <- ct %>%
  group_by(IDENTIFIER) %>%
  mutate(times_arrested = n()) %>%
  ungroup()

# Create variable for class of offense
print("Classifying Offense...")
ct <- ct %>%
  filter(OFFENSE != "PURGE CIVIL COMMITMENT") %>% # Contempt of court cant skip trial https://law.justia.com/codes/connecticut/2011/title52/chap915/Sec52-468.html
  mutate(offense_class = str_trim(str_sub(OFFENSE, -3, -1))) %>%
  mutate(offense_class = ifelse(str_length(offense_class) == 3,
                                NA,
                                offense_class),
         # Violation is less than a misdemeanor    # https://www.lawserver.com/law/state/connecticut/ct-laws/connecticut_statutes_53a-27
         # so is infraction https://www.jud.ct.gov/faq/traffic.html
         offense_class = ifelse(str_detect(OFFENSE, "\\bVIOLATION"),
                                "VorI", # violation or infraction
                                offense_class),
         # now we hard code some unclassified offenses as UF (unclassified felony) or UM (unclassified misdemeanor)
         # we also hard code some classifications (like AM) that were originally missing
         offense_class = ifelse(OFFENSE == "POSSESSION OF NARCOTICS", "UF", offense_class), # https://www.cga.ct.gov/PS98/rpt%5Colr%5Chtm/98-R-1003.htm
         offense_class = ifelse(OFFENSE == "CARRYING OR SALE OF DANGEROUS WEAPON", "UF", offense_class), # https://www.cga.ct.gov/2006/rpt/2006-R-0807.htm
         offense_class = ifelse(OFFENSE == "SALE OF HEROIN, COC BY NON-DEPENDENT", "UF", offense_class), # https://statelaws.findlaw.com/connecticut-law/connecticut-heroin-laws.html
         offense_class = ifelse(OFFENSE == "PROHIB ACTS RE: DRUG PARAPHERNALIA", "UM", offense_class), # https://law.justia.com/codes/connecticut/2011/title21a/chap420b/Sec21a-267.html
         offense_class = ifelse(OFFENSE == "STRANGULATION 3RD DEGREE", "AM", offense_class), # https://www.cga.ct.gov/current/pub/chap_952.htm#sec_53a-64cc
         offense_class = ifelse(OFFENSE == "CARRY PIST/RVOLV W/O PERMIT", "UF", offense_class), # https://www.cga.ct.gov/2006/rpt/2006-R-0807.htm
         offense_class = ifelse(OFFENSE == "ILLEGAL MANUFACTUAL OR SALE OF DRUGS", "UF", offense_class), # https://www.cga.ct.gov/PS98/rpt%5Colr%5Chtm/98-R-1003.htm
         offense_class = ifelse(OFFENSE == "CREDIT CARD THEFT", "AM", offense_class), # https://statelaws.findlaw.com/connecticut-law/connecticut-credit-and-debit-card-fraud.html
         offense_class = ifelse(OFFENSE == "HINDERING PROSECUTION 2ND DEG", "CF", offense_class), # https://www.lawserver.com/law/state/connecticut/ct-laws/connecticut_statutes_53a-166
         offense_class = ifelse(OFFENSE == "53A021", "CF", offense_class), # https://law.justia.com/codes/connecticut/2011/title53/chap939/Sec53-21.html
         offense_class = ifelse(OFFENSE == "CREATING A PUBLIC DISTURBANCE", "VorI", offense_class), # https://law.justia.com/codes/connecticut/2005/title53a/sec53a-181a.html
         offense_class = ifelse(OFFENSE == "CRUELTY TO ANIMALS", "UM", offense_class), # https://www.cga.ct.gov/2006/rpt/2006-R-0807.htm
         offense_class = ifelse(OFFENSE == "ILLEGAL REFUSAL OF DNA TESTING", "DF", offense_class), # https://www.cga.ct.gov/2011/act/pa/2011PA-00207-R00HB-06489-PA.htm
         offense_class = fct_explicit_na(as.factor(offense_class), na_level = "other"))


## Create Instrument and Outcome variables ----

print("Creating new instrument...")
ct <- group_by(ct, FACILITY) %>% mutate(n_facility = n()) %>% ungroup()

ct <- ct %>%
  group_by(FACILITY, offense_class) %>%
  mutate(avg_fac_off_bond = mean(bond_amount), n_fac_off = n()) %>%
  arrange(admit_date) %>%
  mutate(trailing_count = row_number(),
         trailing_bond_sum = lag(cumsum(bond_amount)), default=0) %>%
  ungroup() %>%
  # Create facility leave-out average
  mutate(leaveout_avg_fac_off_bond = ((avg_fac_off_bond * n_fac_off) - bond_amount) / (n_fac_off - 1),
         trailing_leaveout_avg_bond = trailing_bond_sum / (trailing_count - 1),
         log_leaveout_avg_fac_off_bond = log(leaveout_avg_fac_off_bond),
         log_trailing_leaveout_bond = log(trailing_leaveout_avg_bond))

ct <- ct %>%
  # Same as above but now it is year-facility-offense specific
  group_by(FACILITY, offense_class, year) %>%
  mutate(year_avg_fac_off_bond = mean(bond_amount), year_n_fac_off = n()) %>%
  arrange(admit_date) %>%
  mutate(year_trailing_count = row_number(),
         year_trailing_bond_sum = lag(cumsum(bond_amount)), default=0) %>%
  ungroup() %>%
  # Create facility leave-out average
  mutate(year_leaveout_avg_fac_off_bond = ((year_avg_fac_off_bond * year_n_fac_off) - bond_amount) / (year_n_fac_off - 1),
         year_trailing_leaveout_avg_bond = year_trailing_bond_sum / (year_trailing_count - 1),
         year_log_leaveout_avg_fac_off_bond = log(year_leaveout_avg_fac_off_bond),
         year_log_trailing_leaveout_bond = log(year_trailing_leaveout_avg_bond))


# Create Outcome Variable
print("Creating outcome variable...")
ct <- ct %>%
  group_by(IDENTIFIER) %>%
  arrange(admit_date) %>%
  mutate(prior_arrests = row_number() - 1,
         is_first_arrest = row_number() == 1,
  		   is_M = str_sub(offense_class, -1) == "M",
         is_MV = is_M | offense_class == "VorI",
         is_F = str_sub(offense_class, -1) == "F",
         is_FTA = OFFENSE %in% c("FAILURE TO APPEAR, SECOND DEGREE      AM", "FAILURE TO APPEAR, FIRST DEGREE       DF"),
         prior_FTAs = lag(cumsum(is_FTA), default = 0), # Including the current arrests
         prior_Fs = lag(cumsum(is_F), default = 0),
         prior_Ms = lag(cumsum(is_M), default = 0),
         prior_MVs = lag(cumsum(is_MV), default = 0),
         # Generate indicator for whether next arrest is for fleeing
         outcome = lead(is_FTA, default=FALSE)) %>%
  ungroup()


## Create variables to drop ----
# Drop if in jail for very long time
cutoff <- ct %>%
  ungroup() %>%
  filter(outcome == TRUE) %>%
  summarise(mean = mean(detention_duration),
            std = sd(detention_duration),
            cutoff = mean + 2*std) %>% pull()


## Export data with dropping conditions ----
print("Exporting clean dataset...")
fac <- c("YORK CI", "HARTFORD CC", "BRIDGEPORT CC", "NEW HAVEN CC", "CORRIGAN CI", "MANSON YI", "OSBORN CI", "GARNER")

constrained <- ct %>%
  mutate(long_detain = detention_duration > cutoff,
         before_data_starts = admit_date < mdy("07/01/2016"),
         pre_reform = admit_date < mdy("07/01/2017"),
         non_standard_rules = DETAINER != "NONE",
         small_facility = !FACILITY %in% fac,
         minor = AGE < 18,
         few_race = RACE %in% c("ASIAN", "AMER IND"),
         youth_offense = OFFENSE == "YOUTHFUL OFFENDER",
         # One indicator for whether to drop
         drop_me = youth_offense | long_detain | non_standard_rules |
           small_facility | minor | few_race| before_data_starts)

save(constrained, file = arguments$output)
