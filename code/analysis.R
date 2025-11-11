library(here)
library(tidyverse)
library(RColorBrewer)

fed_funds <- here("ntd-data",
                  "federal.csv") |>
  read_csv() 



service_by_mode <- here("ntd-data",
                        "service-by-mode.csv") |>
  read_csv() |>
  rename(reporter_type = max_reporter_type) |>
  rename(ntd_id = `_5_digit_ntd_id`)|>
  rename(agency = max_agency) |>   
  mutate(agency = str_replace_all(agency, "  ", " ")) |>
  mutate(agency = str_replace_all(agency, " ,", ",")) |>
  mutate(agency = ifelse(agency == "Belknap-Merrimack CAP/Concord Area Transit, dba: Community Action Program Belknap-Merrimack Countie",
                         "Belknap-Merrimack CAP/Concord Area Transit",
                         agency)) |>
  mutate(agency = ifelse(agency == "Southwestern Community Services Transportation, dba: Southwestern Community Services",
                         "Southwestern Community Services Transportation",
                         agency)) |>
  mutate(agency = ifelse(agency %in% c("MTA New York City Transit",
                                       "Metro-North Commuter Railroad Company, dba: MTA Metro-North Railroad",
                                       "MTA Bus Company",
                                       "MTA Long Island Rail Road",
                                       "Staten Island Rapid Transit Operating Authority, dba: MTA Staten Island Railway"),
                                                          "New York MTA",
                         agency)) |>
  mutate(ntd_id = ifelse(agency == "New York MTA", "99999", ntd_id)) |>
  mutate(agency = ifelse(agency == "Washington Metropolitan Area Transit Authority, dba: Washington Metro",
                         "WMATA",
                         agency)) |>
  mutate(agency = ifelse(agency == "Los Angeles County Metropolitan Transportation Authority, dba: Metro",
                         "LA Metro",
                         agency)) |>
  mutate(agency = ifelse(agency == "San Francisco Bay Area Rapid Transit District, dba: SF BART",
                         "SF BART",
                         agency)) |>
  group_by(agency, ntd_id, mode, reporter_type) |>
  summarise(VRM = sum(sum_actual_vehicles_passenger_car_revenue_miles))

all_agencies <- service_by_mode |>
  group_by(agency, ntd_id, reporter_type) |>
  summarise(VRM = sum(VRM)) 



fixed_route_agencies <- service_by_mode |>
  filter(mode != "DR") |>
  group_by(agency, ntd_id) |>
  summarise(VRM = sum(VRM)) |>
  ungroup() |>
  mutate(pct_of_total = VRM / sum(VRM)) |>
  arrange(-VRM) |>
  mutate(cum_pct = cumsum(pct_of_total)) |>
  mutate(remaining = 1- cum_pct)

dr_only <- service_by_mode |>
  filter(! ntd_id %in% fixed_route_agencies$ntd_id)

agency_types <- fed_funds |>
  mutate(dr_only = ntd_id %in% dr_only$ntd_id) |>
  group_by(organization_type, dr_only) |>
  summarize(num = n()) |>
  pivot_wider(names_from = dr_only, values_from = num)
  
revenue_types <- c("Fares and other\ndirectly-generated revenue",
                   "Taxes and fees\nlevied by transit agency",
                   "Transfers from\nlocal government",
                   "Transfers from\nstate government",
                   "Transfers from\nfederal government")


funds_by_type <- here("ntd-data",
                      "funds-by-type.csv") |>
  read_csv() |> 
  mutate(agency = str_replace_all(agency, "  ", " ")) |>
  mutate(agency = str_replace_all(agency, " ,", ",")) |>
  mutate(agency = ifelse(agency == "Washington Metropolitan Area Transit Authority, dba: Washington Metro",
                         "WMATA",
                         agency)) |>
  mutate(agency = ifelse(agency == "Los Angeles County Metropolitan Transportation Authority, dba: Metro",
                         "LA Metro",
                         agency)) |>
  mutate(agency = ifelse(agency == "San Francisco Bay Area Rapid Transit District, dba: SF BART",
                         "SF BART",
                         agency)) |>
  select("agency",
         "ntd_id",
         "state",
         "uza_name",
         "primary_uza_population",
         "agency_voms",
         "fund_expenditure_type",
         "fares_and_other_directly",
         "taxes_fees_levied_by_transit",
         "local",
         "state_1",
         "federal",
         "reporter_type") |>
  pivot_longer(cols = c("fares_and_other_directly",
                        "taxes_fees_levied_by_transit",
                        "local",
                        "state_1",
                        "federal"),
               names_to = "revenue_type") |>
  mutate(agency = ifelse(agency %in% c("MTA New York City Transit",
                                       "Metro-North Commuter Railroad Company, dba: MTA Metro-North Railroad",
                                       "MTA Bus Company",
                                       "MTA Long Island Rail Road",
                                       "Staten Island Rapid Transit Operating Authority, dba: MTA Staten Island Railway"),
                         "New York MTA",
                         agency)) |>
  mutate(ntd_id = ifelse(agency == "New York MTA", "99999", ntd_id)) |>
  inner_join(fixed_route_agencies) |>
  mutate(big_agency = ifelse(agency %in% fixed_route_agencies$agency[1:6],
                             agency, "Other")) |>
  mutate(big_agency = factor(big_agency, 
                             levels = c(fixed_route_agencies$agency[1:6], "Other"))) |>
  mutate(revenue_type = 
           case_when(
             revenue_type == "fares_and_other_directly" ~ revenue_types[1],
             revenue_type == "taxes_fees_levied_by_transit" ~ revenue_types[2],
             revenue_type == "local" ~ revenue_types[3],
             revenue_type == "state_1" ~ revenue_types[4],
             revenue_type == "federal" ~ revenue_types[5])) |>
  mutate(revenue_type = factor(revenue_type, levels = revenue_types)) |>
  mutate(f5307_11 = ifelse(primary_uza_population > 200000, "5307", "5311")) |>
  replace_na(list(f5307_11 = "5311"))

ggplot(funds_by_type) +
  geom_bar(stat = "sum",
           aes(x = fund_expenditure_type,
               y = value,
               fill = big_agency)) +
  scale_size_continuous(guide = "none") +
  scale_fill_manual(values = c(brewer.pal(n = 6, name = "Set3"), "gray"),
                    name = "Operator") +
  scale_y_continuous(breaks = breaks <- seq(0, 70000000000, by = 10000000000),
                     labels = paste0("$", breaks / 1000000000),
    name = "2024 expenditures by U.S. transit operators\noffering fixed-route service (billions)") +
  scale_x_discrete(name = "", labels = c("Capital", "Operating")) +
  theme_minimal()

here("figures",
     "cap_op_agency.png") |>
  ggsave(height = 5, width = 5, units = "in", dpi = 600)

ggplot(funds_by_type) +
  geom_bar(stat = "sum",
           aes(x = fund_expenditure_type,
               y = value,
               fill = revenue_type)) +
  scale_size_continuous(guide = "none") +
  scale_fill_manual(values = c(brewer.pal(n = 6, name = "Set3"), "gray"),
                    name = "Revenue source") +
  scale_y_continuous(breaks = breaks <- seq(0, 70000000000, by = 10000000000),
                     labels = paste0("$", breaks / 1000000000),
                     name = "2024 expenditures by U.S. transit operators\noffering fixed-route service (billions)") +
  scale_x_discrete(name = "", labels = c("Capital", "Operating")) +
  theme_minimal()

here("figures",
     "cap_op_rev.png") |>
  ggsave(height = 5, width = 5, units = "in", dpi = 600)

# Eligibilty for federal operating expences is rural areas and UZAs with popln
# less than 200,000

f5307 <- fed_funds |>
  filter(fta_urbanized_area_formula > 0) |>
  select(ntd_id) |>
  left_join(funds_by_type) |>
  filter(!is.na(value))

f5311 <- fed_funds |>
  filter(fta_rural_progam_5311 > 0) |>
  select(ntd_id) |>
  left_join(funds_by_type) |>
  filter(!is.na(value))

f5307_only <- fed_funds |>
  filter(fta_urbanized_area_formula > 0 & fta_rural_progam_5311 == 0) |>
  select(ntd_id) |>
  left_join(funds_by_type) |>
  filter(!is.na(value))

f5311_only <- fed_funds |>
  filter(fta_rural_progam_5311 > 0 & fta_urbanized_area_formula == 0) |>
  select(ntd_id) |>
  left_join(funds_by_type) |>
  filter(!is.na(value))



ggplot(f5311) +
  geom_bar(stat = "sum",
           aes(x = fund_expenditure_type,
               y = value,
               fill = revenue_type)) +
  scale_size_continuous(guide = "none") +
  scale_fill_manual(values = c(brewer.pal(n = 6, name = "Set3"), "gray"),
                    name = "Revenue source") +
   scale_y_continuous(breaks = breaks <- seq(0, 10000000000, by = 2000000000),
                      labels = paste0("$", breaks / 1000000000),
                      name = "2024 expenditures by fixed-route U.S. transit operators\nwith 5311 funding (billions)") +
  scale_x_discrete(name = "", labels = c("Capital", "Operating")) +
  theme_minimal()

here("figures",
     "rev_5311.png") |>
  ggsave(height = 5, width = 5, units = "in", dpi = 600)

ggplot(f5307) +
  geom_bar(stat = "sum",
           aes(x = fund_expenditure_type,
               y = value,
               fill = revenue_type)) +
  scale_size_continuous(guide = "none") +
  scale_fill_manual(values = c(brewer.pal(n = 6, name = "Set3"), "gray"),
                    name = "Revenue source") +
  scale_y_continuous(breaks = breaks <- seq(0, 60000000000, by = 10000000000),
                      labels = paste0("$", breaks / 1000000000),
                      name = "2024 expenditures by fixed-route U.S. transit operators\nwith for 5307 funding (billions)") +
  scale_x_discrete(name = "", labels = c("Capital", "Operating")) +
  theme_minimal()

here("figures",
     "rev_5307.png") |>
  ggsave(height = 5, width = 5, units = "in", dpi = 600)



ggplot(f5311_only) +
  geom_bar(stat = "sum",
           aes(x = fund_expenditure_type,
               y = value,
               fill = revenue_type)) +
  scale_size_continuous(guide = "none") +
  scale_fill_manual(values = c(brewer.pal(n = 6, name = "Set3"), "gray"),
                    name = "Revenue source") +
  scale_y_continuous(breaks = breaks <- seq(0, 1000000000, by = 100000000),
                     labels = paste0("$", breaks / 1000000),
                     name = "2024 expenditures by fixed-route U.S. transit operators\nwith 5311 funding (millions)") +
  scale_x_discrete(name = "", labels = c("Capital", "Operating")) +
  theme_minimal()

here("figures",
     "rev_5311_only.png") |>
  ggsave(height = 5, width = 5, units = "in", dpi = 600)

ggplot(f5307_only) +
  geom_bar(stat = "sum",
           aes(x = fund_expenditure_type,
               y = value,
               fill = revenue_type)) +
  scale_size_continuous(guide = "none") +
  scale_fill_manual(values = c(brewer.pal(n = 6, name = "Set3"), "gray"),
                    name = "Revenue source") +
  scale_y_continuous(breaks = breaks <- seq(0, 60000000000, by = 10000000000),
                     labels = paste0("$", breaks / 1000000000),
                     name = "2024 expenditures by fixed-route U.S. transit operators\nwith for 5307 funding (billions)") +
  scale_x_discrete(name = "", labels = c("Capital", "Operating")) +
  theme_minimal()

here("figures",
     "rev_5307_only.png") |>
  ggsave(height = 5, width = 5, units = "in", dpi = 600)




reporter_types <- all_agencies |>
  group_by(reporter_type) |>
  summarize(num_agencies = n(),
            total_VRM = sum(VRM))

ggplot(reporter_types, aes(x = 2, y = total_VRM, fill = reporter_type)) +
  geom_col(stat = "identity") +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette = "Set3",
                    name = "Type of reporter") +
  theme_void()

here("figures",
     "type_vrm.png") |>
  ggsave(height = 4, width = 4, units = "in", dpi = 600)

ggplot(reporter_types, aes(x = 2, y = num_agencies, fill = reporter_type)) +
  geom_col(stat = "identity") +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette = "Set3",
                    name = "Type of reporter") +
  theme_void()

here("figures",
     "type_n_agencies.png") |>
  ggsave(height = 4, width = 4, units = "in", dpi = 600)


state <- here("ntd-data",
              "state.csv") |> 
  read_csv() |>
  filter(reporter_type == "Full Reporter") |>
  rename(state_general = general_funds,
         state_transpo = transportation_funds) |>
  select(ntd_id, agency, state_general, state_transpo) |>
  pivot_longer(cols = c(state_general,
                        state_transpo),
               names_to = "fund_type")

local <- here("ntd-data",
              "local.csv") |> 
  read_csv() |>
  filter(reporter_type == "Full Reporter") |>
  rename(local_general = general_fund,
         local_income_tax = income_tax,
         local_sales_tax = sales_tax,
         local_property_tax = property_tax,
         local_fuel_tax = fuel_tax,
         local_other_tax = other_taxes,
         local_tolls = tolls,
         other_local = other_funds) |>
  select(ntd_id, 
         agency,
         local_general,
         local_income_tax,
         local_sales_tax,
         local_property_tax,
         local_fuel_tax,
         local_other_tax,
         local_tolls,
         other_local) |>
  pivot_longer(cols = c(local_general,
                        local_income_tax,
                        local_sales_tax,
                        local_property_tax,
                        local_fuel_tax,
                        local_other_tax,
                        local_tolls,
                        other_local),
               names_to = "fund_type")

agency_taxes <- here("ntd-data",
                     "agency-taxes.csv") |>
  read_csv() |>
  filter(reporter_type == "Full Reporter",
         !is.na(total),
         total > 0) |>
  pivot_longer(cols = c(income_tax,
                        sales_tax,
                        property_tax,
                        fuel_tax,
                        other_tax,
                        tolls,
                        other_funds),
               names_to = "type") |>
  filter(!is.na(value)) |>
  mutate(pct_of_total = value / total) |>
  mutate(type = factor(type, levels = c("income_tax",
                                        "sales_tax",
                                        "property_tax",
                                        "fuel_tax",
                                        "other_tax",
                                        "tolls")))

state_local <- rbind(state, local) |>
  mutate(general_funds = fund_type %in% c("state_general", "local_general"))

state_local_gen_pct <- state_local |>
  group_by(ntd_id) |>
  mutate(total_state_local = sum(value)) |>
  ungroup() |>
  group_by(ntd_id, general_funds) |>
  summarise(agency = min(agency),
            value = sum(value),
            total_state_local = mean(total_state_local)) |>
  filter(general_funds) |>
  mutate(pct_general = value / total_state_local)

ggplot(state_local) +
  geom_col(aes(x = 1, y = value, fill = general_funds)) +
  scale_y_continuous(name = "Local/state funds spent on public transit\n(billions) (full-reporters only)",
                     breaks = breaks <- seq(0, 50000000000, by = 10000000000),
                     labels = paste0("$", breaks / 1000000000)) +
  scale_x_continuous(name = "", breaks = c()) +
  scale_fill_brewer(palette = "Set3",
                    name = "Funding type",
                    labels = c("Dedicated transportation funds",
                              "General funds")) +
  theme_minimal()

here("figures",
     "gen_dedic.png") |>
  ggsave(height = 5, width = 5, units = "in", dpi = 600)

state_local_dedicated <- state_local |>
  filter(!general_funds) |>
  mutate(fund_type = factor(fund_type, 
                            levels = c("local_income_tax",
                                       "local_sales_tax",
                                       "local_property_tax",
                                       "local_fuel_tax",
                                       "local_other_tax",
                                       "local_tolls",
                                       "other_local",
                                       "state_transpo")))

dedicated_summary <- state_local_dedicated |>
  pivot_wider(names_from = fund_type,
              values_from = value) |>
  mutate(total_dedicated = 
           local_income_tax +
           local_sales_tax +
           local_property_tax +
           local_fuel_tax +
           local_other_tax +
           local_tolls +
           other_local +
           state_transpo) |>
  filter(total_dedicated > 0) |>
  mutate(pct_income_tax = local_income_tax / total_dedicated,
         pct_sales_tax = local_sales_tax / total_dedicated,
         pct_prop_tax = local_property_tax / total_dedicated,
         pct_fuel_tax = local_fuel_tax / total_dedicated,
         pct_other_tax = local_other_tax / total_dedicated,
         pct_tolls = local_tolls / total_dedicated,
         pct_other_local = other_local / total_dedicated,
         pct_state = state_transpo / total_dedicated)
  
ggplot(state_local_dedicated) +
  geom_col(aes(x = 1, y = value, fill = fund_type)) +
  scale_y_continuous(name = "Local/state funds spent on public transit\n(billions) (full-reporters only)",
                     breaks = breaks <- seq(0, 50000000000, by = 10000000000),
                     labels = paste0("$", breaks / 1000000000)) +
  scale_x_continuous(name = "", breaks = c()) +
  scale_fill_brewer(palette = "Set3",
                    name = "Type of dedicated\ntransportation funds",
                    labels = c("Local income tax",
                               "Local sales tax",
                               "Local property tax",
                               "Local fuel tax",
                               "Other local taxes",
                               "Tolls",
                               "Other local funds",
                               "State transportation funds")) +
  theme_minimal()

here("figures",
     "dedic_types.png") |>
  ggsave(height = 5, width = 5, units = "in", dpi = 600)

ggplot(agency_taxes) +
  geom_col(aes(x = 1, y = value, fill = type)) +
  scale_y_continuous(name = "Revenue from taxes levied\nby transit agencies (billions)",
                     breaks = breaks <- seq(0, 8000000000, by = 1000000000),
                     labels = paste0("$", breaks / 1000000000)) +
  scale_fill_brewer(palette = "Set3",
                    name = "Type of tax",
                    labels = c("Income tax",
                               "Sales tax",
                               "Property tax",
                               "Local fuel tax",
                               "Other local taxes",
                               "Tolls")) +
  scale_x_continuous(name = "", breaks = c()) +
  theme_minimal()

here("figures",
     "transit_tax.png") |>
  ggsave(height = 5, width = 5, units = "in", dpi = 600)

fares <- here("ntd-data",
                     "direct-gen.csv") |>
  read_csv() |>
  select(agency,
         ntd_id,
         fares,
         park_and_ride,
         concessions,
         advertising,
         other,
         purchased_transportation) |>
  pivot_longer(cols = c(-agency, -ntd_id),
               names_to = "rev_type") |>
  replace_na(list(value = 0)) |>
  mutate(rev_type = factor(rev_type,
                           levels = c("fares",
                                      "advertising",
                                      "park_and_ride",
                                      "concessions",
                                      "purchased_transportation",
                                      "other")))

ggplot(fares) +
  geom_col(aes(x = 1, y = value, fill = rev_type)) +
  scale_y_continuous(name = "Directly-generated revenue\nfrom transit agencies (billions)",
                     breaks = breaks <- seq(0, 13000000000, by = 1000000000),
                     labels = paste0("$", breaks / 1000000000)) +
  scale_fill_brewer(palette = "Set3",
                    name = "Revenue source",
                    labels = c("Fares",
                               "Advertising",
                               "Park-and-ride",
                               "Concessions",
                               "Non-fare revenue from contractors",
                               "Other")) +
  scale_x_continuous(name = "", breaks = c()) +
  theme_minimal()

here("figures",
     "direct-gen.png") |>
  ggsave(height = 5, width = 5, units = "in", dpi = 600)

fare_free <- fares |>
  filter(rev_type == "fares",
         value == 0) |>
  select(ntd_id)

fare_free_funds_by_type <- funds_by_type |>
  filter(ntd_id %in% fare_free$ntd_id)

ggplot(fare_free_funds_by_type) +
  geom_bar(stat = "sum",
           aes(x = fund_expenditure_type,
               y = value,
               fill = revenue_type)) +
  scale_size_continuous(guide = "none") +
  scale_fill_manual(values = c(brewer.pal(n = 6, name = "Set3"), "gray"),
                    name = "Revenue source") +
  scale_y_continuous(breaks = breaks <- seq(0, 800000000, by = 100000000),
                     labels = paste0("$", breaks / 1000000),
                     name = "2024 expenditures by fare-free\nU.S. transit operators offering\nfixed-route service (millions)") +
  scale_x_discrete(name = "", labels = c("Capital", "Operating")) +
  theme_minimal()

here("figures",
     "fare-free-rev.png") |>
  ggsave(height = 5, width = 5, units = "in", dpi = 600)

ff_state_local <- state_local |>
  filter(ntd_id %in% fare_free$ntd_id)

ggplot(ff_state_local) +
  geom_col(aes(x = 1, y = value, fill = general_funds)) +
  scale_y_continuous(name = "Local/state funds spent on fare-free public transit\n(millions) (full-reporters only)",
                     breaks = breaks <- seq(0, 500000000, by = 100000000),
                     labels = paste0("$", breaks / 1000000)) +
  scale_x_continuous(name = "", breaks = c()) +
  scale_fill_brewer(palette = "Set3",
                    name = "Funding type",
                    labels = c("Dedicated transportation funds",
                               "General funds")) +
  theme_minimal()

here("figures",
     "ff_gen_dedic.png") |>
  ggsave(height = 5, width = 5, units = "in", dpi = 600)

ff_state_local_dedicated <- state_local_dedicated |>
  filter(ntd_id %in% fare_free$ntd_id)

ggplot(ff_state_local_dedicated) +
  geom_col(aes(x = 1, y = value, fill = fund_type)) +
  scale_y_continuous(name = "Dedicated local/state funds spent\non fare-free public transit\n(billions) (full-reporters only)",
  breaks = breaks <- seq(0, 500000000, by = 100000000),
labels = paste0("$", breaks / 1000000)) +
  scale_x_continuous(name = "", breaks = c()) +
  scale_fill_brewer(palette = "Set3",
                    name = "Type of dedicated\ntransportation funds",
                    labels = c("Local income tax",
                               "Local sales tax",
                               "Local property tax",
                               "Local fuel tax",
                               "Other local taxes",
                               "Tolls",
                               "Other local funds",
                               "State transportation funds")) +
  theme_minimal()


here("figures",
     "ff_type_dedic.png") |>
  ggsave(height = 5, width = 5, units = "in", dpi = 600)

ff_agency_taxes <- agency_taxes |>
  filter(ntd_id %in% fare_free$ntd_id)

ff_fares <- fares |>
  filter(ntd_id %in% fare_free$ntd_id) |>
  filter(value > 0)

ggplot(ff_fares) +
  geom_col(aes(x = 1, y = value, fill = rev_type)) +
  scale_y_continuous(name = "Directly-generated revenue from\nfare-free transit agencies (millions)",
                     breaks = breaks <- seq(0, 30000000, by = 5000000),
                     labels = paste0("$", breaks / 1000000)) +
  scale_fill_manual(values =  brewer.pal(6, "Set3")[2:6],
                    name = "Revenue source",
                    labels = c("Advertising",
                               "Park-and-ride",
                               "Concessions",
                               "Non-fare revenue from contractors",
                               "Other")) +
  scale_x_continuous(name = "", breaks = c()) +
  theme_minimal()

here("figures",
     "ff_fares.png") |>
  ggsave(height = 5, width = 5, units = "in", dpi = 600)
