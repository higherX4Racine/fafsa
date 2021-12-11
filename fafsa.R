library(tidyverse)

.FAFSA_SITE <- "https://studentaid.gov"
.FAFSA_PATH <- "sites/default/files/fsawg/datacenter/library/fafsabyhs"
.extension <- "xls"


fafsa_end_of_year_file_name <- function(year) {
    glue::glue("HS_ARCHIVE1231{year}.{.extension}")
}


fafsa_remote_file <- function(year) {
    
    paste(.FAFSA_SITE,
          .FAFSA_PATH,
          fafsa_end_of_year_file_name(year),
          sep = "/")
}


read_fafsa_file <- function(path) {
    readxl::read_excel(path,
                       skip = 3,
                       col_types = "text") %>%
        tidyr::pivot_longer(cols = !(Name:State),
                            values_to = "Applications") %>%
        dplyr::mutate(
            Applications = readr::parse_integer(Applications,
                                                na = c("<5", "NA"))) %>%
        dplyr::filter(!is.na(Applications)) %>%
        tidyr::separate(name,
                        c(NA,
                          "Status",
                          "Date"),
                        sep = "\n",
                        convert = TRUE) %>%
        tidyr::pivot_wider(id_cols = c(Name, City, State, Date),
                           names_from = Status,
                           values_from = Applications) %>%
        tidyr::unnest(c("Submitted",
                        "Complete")) %>%
        tidyr::extract(Date,
                       into = c("Month",
                                "Day",
                                "Year"),
                       regex = "([[:alpha:]]{3})(\\S*)\\s+(.+)",
                       convert = TRUE) %>%
        dplyr::mutate(Rate = Complete / Submitted,
                      Date = lubridate::ymd(paste(Year,
                                                  Month,
                                                  if_else(is.na(Day),
                                                          1L,
                                                          Day)))
        ) %>%
        dplyr::distinct() %>%
        invisible()
}