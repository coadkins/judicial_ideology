# Load packages
library(tidyverse)
library(data.table)
library(jsonlite)
library(janitor)
library(lubridate)
library(httr)

setDTthreads(threads = 0)

# Token for API

secret_token <- "Token a4a7a6f6be318b55e50c38a501df60767f21139d"

# Load FJC

fjc_data_1988 <- data.table::fread("Civil-2.txt") %>%
                 clean_names()

# Load FJC Biographical

fjc_biographical <- data.table::fread("fjc_judge_bio.csv") %>%
                    clean_names()

# Load Freelaw database

# Docket entries

json_dockets <- list.files(path = "dockets/", pattern = "*.json") %>%
              as.list(.) %>%
              map(~read_json(path = paste("dockets/", ., sep = ""))) %>%
              map(~rlist::list.remove(., "tags"))

cl_dt <- data.table::rbindlist(json_dockets, fill = TRUE)

# Filter out rows with no judge,
# and outside the time window
# and save resulting data

cl_dt[is.na(assigned_to) == FALSE] %>%
.[, date_filed := parse_date(date_filed, "%Y-%m-%d")] %>%
.[date_filed > ymd(19880101)] %>%
.[, .(id, court, assigned_to, date_filed,
      docket = docket_number_core, nature_of_suit)] %>%
write_csv("cl_dt_filter.csv")

# Load judge entries

json_judges <- list.files(path = "judges/", pattern = "*.json") %>%
              as.list(.) %>%
              map(~read_json(path = paste("judges/", ., sep = ""))) %>%
              map(~rlist::list.remove(., c("educations",
                                           "sources",
                                           "political_affiliations",
                                           "aba_ratings",
                                           "positions")))

cl_judges_dt <- rbindlist(json_judges, fill = TRUE) %>%
                .[is.na(fjc_id) == FALSE] %>%
                select(resource_uri, id, fjc_id)

write_csv(cl_judges_dt, "cl_judges_filter.csv")

# Match Judges with FJC Appointing President

fjc_biographical <- fjc_biographical %>%
              rename(fjc_id = jid) %>%
              select(fjc_id, court_name_1, party_of_appointing_president_1,
                     ayes_nays_1) %>%
              right_join(., cl_judges_dt, by = "fjc_id")
              
# Match Dockets with FJC Cases

## Replace "court" column with relevant court id using API

get_courts <- function (x) {
as.numeric(
        content(GET(
            url = x,
            config = add_headers(Authorization = secret_token)
        ))$fjc_court_id
)
}

courts_key <- data.frame(url = unique(cl_dt[,"court"])) %>%
            mutate(court_id = map_dbl(court, get_courts))

## Use court_key to give Courtlistener data the court ids

cl_dt <- as.data.table(courts_key)[cl_dt, on = .(court)]

## Filter the FJC data to the relevant districts

fjc_data_1988 <- fjc_data_1988 %>%
                  .[, .(district, docket, origin,
                         nos, classact, plt, def, procprog,
                         disp, noj, amtrec, judgment)] %>%
                  .[, court_id := as.numeric(district)] %>%
                  .[court_id %in% courts_key$court_id]

# Right join Courtlistener data to FJC data using docket and court ids
# Keep only the first match from cl_dt

cl_dt <- cl_dt[fjc_data_1988, on = .(docket, court_id), mult = "first"]

write_csv(cl_dt, "cl_fjc_dt.csv")

# Match Judges with Dockets

get_judges <- function (x) {
fjc_id <- as.numeric(
        content(GET(
            url = x,
            config = add_headers(Authorization = secret_token)
        ))$fjc_id
)
if (length(fjc_id) == 0)
        return(NA)
    return(fjc_id)
}

judges_key <- data.frame(url = unique(cl_dt[, "assigned_to"])) %>%
            mutate(fjc_id = map_dbl(assigned_to, get_judges))

## Use judges_key to give Courtlistener data the judge ids

cl_dt <- as.data.table(judges_key)[cl_dt, on = .(assigned_to)]

# Right join fjc_biographical to cl_dt to complete data!

cl_dt <- fjc_biographical[cl_dt, on = .(fjc_id)] %>%
        setcolorder(., c("court_id", "docket", "nature_of_suit",
                    "fjc_id", "party_of_appointing_president_1",
                     "classact", "judgment",
                    "disp", "procprog"))

write_csv(cl_dt, "cl_fjc_dt.csv")
