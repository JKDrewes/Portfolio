library(tidycensus)
tidycensus_key = "63f9c2c91c77329028fc773366c870d57ab325b7"
census_api_key(tidycensus_key)

il_acs = get_pums(
  variables = c("PUMA",
                "HICOV",
                "MULTG",
                "COW",
                "SCH",
                "SEX",
                "DIS",
                "ESP",
                "HINCP",
                "AGEP",
                "WKHP"),
  survey = "acs5",
  year = 2021,
  recode = TRUE,
  show_call = TRUE,
  state = "17",
  rep_weights = "person" #We're not interested in estimating housing, so we'll avoid it to cut down on DL
)

rm(tidycensus_key)

print("Downloaded Data")