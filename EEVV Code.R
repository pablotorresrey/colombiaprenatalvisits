library(haven)
library(readr)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(fixest)
library(stargazer)
library(did)
library(readxl)

projectpath <- "/Users/pablotorresr/Desktop/Classes_Columbia/7th Semester/Senior Thesis/Data/raw/"
nac_2008_name <- paste0(projectpath, "EEVV_Nacidos/nac2008/nac2008.csv")
nac_2009_name <- paste0(projectpath, "EEVV_Nacidos/nac2009/nac2009.csv")
nac_2010_name <- paste0(projectpath, "EEVV_Nacidos/nac2010/nac2010.csv")
nac_2011_name <- paste0(projectpath, "EEVV_Nacidos/nac2011/nac2011.sav")
nac_2012_name <- paste0(projectpath, "EEVV_Nacidos/nac2012/nac2012.sav")
nac_2013_name <- paste0(projectpath, "EEVV_Nacidos/nac2013/nac2013.sav")
nac_2014_name <- paste0(projectpath, "EEVV_Nacidos/nac2014/nac2014.sav")
nac_2015_name <- paste0(projectpath, "EEVV_Nacidos/nac2015/nac2015.sav")
codes_data_name <- paste0(projectpath, "rural_dnp/def_rurality.xlsx")
afiliados_name <- paste0(projectpath, "BDUA/afiliados.csv")
fetal_2008_name <- paste0(projectpath, "EEVV_fetal_deaths/fetal2008/fetal2008.csv")
fetal_2009_name <- paste0(projectpath, "EEVV_fetal_deaths/fetal2009/fetal2009.csv")
fetal_2010_name <- paste0(projectpath, "EEVV_fetal_deaths/fetal2010/fetal2010.csv")
fetal_2011_name <- paste0(projectpath, "EEVV_fetal_deaths/fetal2011/fetal2011.csv")
fetal_2012_name <- paste0(projectpath, "EEVV_fetal_deaths/fetal2012/fetal2012.sav")
fetal_2013_name <- paste0(projectpath, "EEVV_fetal_deaths/fetal2013/fetal2013.sav")
fetal_2014_name <- paste0(projectpath, "EEVV_fetal_deaths/fetal2014/fetal2014.sav")
fetal_2015_name <- paste0(projectpath, "EEVV_fetal_deaths/fetal2015/fetal2015.sav")
crosswalk_pcodes_name <- paste0(projectpath, "ICD_crosswalks/icd_pcodes.csv")
crosswalk_qcodes_name <- paste0(projectpath, "ICD_crosswalks/icd_qcodes.csv")

nac2008 <- read.csv(nac_2008_name)
nac2009 <- read.csv(nac_2009_name)
nac2010 <- read.csv(nac_2010_name)
nac2011 <- read_sav(nac_2011_name)
nac2012 <- read_sav(nac_2012_name, encoding = "ISO-8859-1")
nac2013 <- read_sav(nac_2013_name)
nac2014 <- read_sav(nac_2014_name)
nac2015 <- read_sav(nac_2015_name)
codes_data <- read_xlsx(codes_data_name, sheet = "codes")
afiliados <- read.csv(afiliados_name)
fetal2008 <- read.csv(fetal_2008_name)
fetal2009 <- read.csv(fetal_2009_name)
fetal2010 <- read.csv(fetal_2010_name)
fetal2011 <- read.csv(fetal_2011_name)
fetal2012 <- read_sav(fetal_2012_name, encoding = "ISO-8859-1")
fetal2013 <- read_sav(fetal_2013_name)
fetal2014 <- read_sav(fetal_2014_name)
fetal2015 <- read_sav(fetal_2015_name)
crosswalk_pcodes <- read.csv(crosswalk_pcodes_name)
crosswalk_qcodes <- read.csv(crosswalk_qcodes_name)
# Define a function to process each year
process_data <- function(data) {
  data %>%
    select(COD_DPTO, COD_MUNIC, AREANAC, CODPTORE, CODMUNRE, AREA_RES, NUMCONSUL) %>%
    filter(AREA_RES %in% c("1", "2", "3")) %>%  # Filter for valid residential areas only
    mutate(NUMCONSUL = as.character(NUMCONSUL)) %>%  # Directly convert NUMCONSUL to character
    filter(!is.na(NUMCONSUL) & NUMCONSUL != "") %>%  # Filter out missing or empty values
    mutate(NUMCONSUL = case_when(
      NUMCONSUL == "00" ~ 0L,
      NUMCONSUL == "01" ~ 1L,
      NUMCONSUL == "02" ~ 2L,
      NUMCONSUL == "03" ~ 3L,
      NUMCONSUL == "04" ~ 4L,
      NUMCONSUL == "05" ~ 5L,
      NUMCONSUL == "06" ~ 6L,
      NUMCONSUL == "07" ~ 7L,
      NUMCONSUL == "08" ~ 8L,
      NUMCONSUL == "09" ~ 9L,
      TRUE ~ as.integer(NUMCONSUL)
    )) %>%
    filter(!is.na(NUMCONSUL) & NUMCONSUL != 99) %>%  # Exclude rows where NUMCONSUL is 99 (no information)
    group_by(AREA_RES, NUMCONSUL) %>%
    summarise(total_women = n(), .groups = "drop")  # Summarise and ungroup
}

# Apply the function to each dataset
nac2008_processed <- process_data(nac2008)
nac2009_processed <- process_data(nac2009)
nac2010_processed <- process_data(nac2010)
nac2011_processed <- process_data(nac2011)
nac2012_processed <- process_data(nac2012)
nac2013_processed <- process_data(nac2013)
nac2014_processed <- process_data(nac2014)
nac2015_processed <- process_data(nac2015)

#Process Codes Data
codes_data <- codes_data %>% 
  rename(COD_UNIQ = `Código DANE`)

# Calculate total women per residential area for each year
calculate_totals <- function(data) {
  data %>%
    filter(AREA_RES %in% c("1", "2", "3")) %>%
    group_by(AREA_RES) %>%
    summarise(total_women_area = sum(total_women))  # Sum of total women in each area, excluding NAs and 99s
}

#Process Afiliados Data
afiliados_processed <- afiliados %>% 
  filter(ano <= 2015) %>% 
  mutate(
    COD_DPTO = substr(Municipio, 1, 2),   
    COD_UNIQ = substr(Municipio, 1, 5)    
  ) %>% 
  filter(COD_DPTO %in% sprintf("%02d", 1:99)) %>% 
  rename(SEG_SOCIAL = Regimen) %>% 
  filter(SEG_SOCIAL %in% c("CONTRIBUTIVO", "SUBSIDIADO")) %>% 
  mutate(
    SEG_SOCIAL = ifelse(SEG_SOCIAL == "CONTRIBUTIVO", 1,
                        ifelse(SEG_SOCIAL == "SUBSIDIADO", 0, SEG_SOCIAL))
  )

market_share <- afiliados_processed %>%
  mutate(
    month_year = paste0(ano, "-", mes)  # Combine year and month into a single column
  ) %>%
  group_by(COD_UNIQ, SEG_SOCIAL, month_year, NomAdministradora) %>% # Group by COD_UNIQ, SEG_SOCIAL, month_year, NomAdministradora
  summarise(
    total_individuals = sum(Afiliados, na.rm = TRUE),  # Sum Afiliados for each NomAdministradora
    .groups = "drop"                                  # Drop grouping after summarising
  ) %>%
  group_by(COD_UNIQ, SEG_SOCIAL, month_year) %>% # Group again by COD_UNIQ, SEG_SOCIAL, month_year
  mutate(
    market_share = total_individuals / sum(total_individuals, na.rm = TRUE)  # Calculate market share
  ) %>%
  ungroup() 

market_share <- market_share %>%
  filter(COD_UNIQ %in% codes_data$COD_UNIQ)

market_share_with_change <- market_share %>%
  group_by(COD_UNIQ, SEG_SOCIAL, NomAdministradora) %>% # Group by COD_UNIQ, SEG_SOCIAL, and NomAdministradora
  arrange(month_year) %>%                              # Ensure data is ordered by time
  mutate(
    change_in_market_share = market_share - lag(market_share) # Compute month-over-month change
  ) %>%
  ungroup() 

hhi_data <- market_share_with_change %>%
  group_by(COD_UNIQ, month_year, SEG_SOCIAL) %>% # Group by COD_UNIQ, month_year, SEG_SOCIAL
  summarise(
    HHI = 10000*(sum((market_share)^2, na.rm = TRUE)),  # Calculate HHI
    .groups = "drop"                            # Drop grouping after summarising
  )

# Step 2: Calculate the change in HHI across time periods for the same COD_UNIQ and SEG_SOCIAL
hhi_data_with_change <- hhi_data %>%
  group_by(COD_UNIQ, SEG_SOCIAL) %>%       # Group by COD_UNIQ and SEG_SOCIAL
  arrange(month_year) %>%                  # Ensure data is ordered by time
  mutate(
    change_in_HHI = HHI - lag(HHI)         # Calculate change in HHI over time
  ) %>%
  ungroup()                                # Remove grouping for final output


nac2008_totals <- calculate_totals(nac2008_processed)
nac2009_totals <- calculate_totals(nac2009_processed)
nac2010_totals <- calculate_totals(nac2010_processed)
nac2011_totals <- calculate_totals(nac2011_processed)
nac2012_totals <- calculate_totals(nac2012_processed)
nac2013_totals <- calculate_totals(nac2013_processed)
nac2014_totals <- calculate_totals(nac2014_processed)
nac2015_totals <- calculate_totals(nac2015_processed)

# Join processed data with totals and calculate percentages and cumulative percentages for each year
calculate_percentages_and_cumulative <- function(processed, totals) {
  processed %>%
    left_join(totals, by = "AREA_RES") %>%
    mutate(percentage = (total_women / total_women_area) * 100) %>%
    complete(NUMCONSUL = 0:25, fill = list(total_women = 0)) %>%  # Ensure all values from 0 to 25 are present
    arrange(AREA_RES, NUMCONSUL) %>%
    group_by(AREA_RES) %>%
    mutate(cumulative_women = cumsum(total_women),
           cumulative_percentage = (cumulative_women / total_women_area) * 100) %>%  # Calculate cumulative percentage
    ungroup()
}

percentages_cumulative_2008 <- calculate_percentages_and_cumulative(nac2008_processed, nac2008_totals)
percentages_cumulative_2009 <- calculate_percentages_and_cumulative(nac2009_processed, nac2009_totals)
percentages_cumulative_2010 <- calculate_percentages_and_cumulative(nac2010_processed, nac2010_totals)
percentages_cumulative_2011 <- calculate_percentages_and_cumulative(nac2011_processed, nac2011_totals)
percentages_cumulative_2012 <- calculate_percentages_and_cumulative(nac2012_processed, nac2012_totals)
percentages_cumulative_2013 <- calculate_percentages_and_cumulative(nac2013_processed, nac2013_totals)
percentages_cumulative_2014 <- calculate_percentages_and_cumulative(nac2014_processed, nac2014_totals)
percentages_cumulative_2015 <- calculate_percentages_and_cumulative(nac2015_processed, nac2015_totals)
# Convert AREA_RES to character for consistency across all datasets
percentages_cumulative_2008 <- percentages_cumulative_2008 %>% mutate(AREA_RES = as.character(AREA_RES))
percentages_cumulative_2009 <- percentages_cumulative_2009 %>% mutate(AREA_RES = as.character(AREA_RES))
percentages_cumulative_2010 <- percentages_cumulative_2010 %>% mutate(AREA_RES = as.character(AREA_RES))
percentages_cumulative_2011 <- percentages_cumulative_2011 %>% mutate(AREA_RES = as.character(AREA_RES))
percentages_cumulative_2012 <- percentages_cumulative_2012 %>% mutate(AREA_RES = as.character(AREA_RES))
percentages_cumulative_2013 <- percentages_cumulative_2013 %>% mutate(AREA_RES = as.character(AREA_RES))
percentages_cumulative_2014 <- percentages_cumulative_2014 %>% mutate(AREA_RES = as.character(AREA_RES))
percentages_cumulative_2015 <- percentages_cumulative_2015 %>% mutate(AREA_RES = as.character(AREA_RES))
# Combine all dataframes into one for plotting
plotted_data <- bind_rows(
  percentages_cumulative_2008 %>% mutate(YEAR = 2008),
  percentages_cumulative_2009 %>% mutate(YEAR = 2009),
  percentages_cumulative_2010 %>% mutate(YEAR = 2010),
  percentages_cumulative_2011 %>% mutate(YEAR = 2011),
  percentages_cumulative_2012 %>% mutate(YEAR = 2012),
  percentages_cumulative_2013 %>% mutate(YEAR = 2013),
  percentages_cumulative_2014 %>% mutate(YEAR = 2014),
  percentages_cumulative_2015 %>% mutate(YEAR = 2015)
)

# Loop through each year and save the graph as a PDF
years <- unique(plotted_data$YEAR)

for (year in years) {
  # Filter the data for the current year
  year_data <- plotted_data %>% filter(YEAR == year)
  
  # Create the plot
  p <- ggplot(year_data, aes(x = NUMCONSUL, y = cumulative_percentage, color = as.factor(AREA_RES))) +
    geom_line(size = 1) +
    labs(
      title = paste("Cumulative Percentage of Pre-Natal Visits by Residential Area -", year),
      x = "Number of Pre-Natal Visits",
      y = "Cumulative Percentage (%)",
      color = "Residential Area"
    ) +
    theme_minimal() +
    theme(legend.position = "right") +
    scale_color_manual(
      values = c("blue", "green", "red"),
      labels = c("Municipal Center", "Populated Center", "Dispersed Rural Area")  # Custom legend labels
    ) +
    scale_x_continuous(limits = c(0, 25)) +
    scale_y_continuous(limits = c(0, 100))
  
  # Save the plot as a PDF
  pdf_filename <- paste0("Cumulative_Percentage_PreNatal_Visits_", year, ".pdf")
  ggsave(pdf_filename, plot = p, device = "pdf", width = 8, height = 6)
}


#BASIC REGRESSION


# Function to ensure consistent data types across all datasets
convert_columns <- function(data) {
  data %>%
    mutate(
      COD_DPTO = as.character(COD_DPTO),
      COD_MUNIC = as.character(COD_MUNIC),
      AREANAC = as.character(AREANAC),
      SIT_PARTO = as.character(SIT_PARTO),
      SEXO = as.character(SEXO),
      ANO = as.character(ANO),
      MES = as.character(MES),
      ATEN_PAR = as.character(ATEN_PAR),
      T_GES = as.character(T_GES),
      # Convert NUMCONSUL safely, handling problematic values before converting
      NUMCONSUL = as.numeric(as.character(NUMCONSUL)),  # Direct conversion without `as_factor`
      TIPO_PARTO = as.character(TIPO_PARTO),
      MUL_PARTO = as.character(MUL_PARTO),
      APGAR1 = as.character(APGAR1),
      APGAR2 = as.character(APGAR2),
      IDHEMOCLAS = as.character(IDHEMOCLAS),
      IDFACTORRH = as.character(IDFACTORRH),
      EST_CIVM = as.character(EST_CIVM),
      NIV_EDUM = as.numeric(as.character(NIV_EDUM)),  # Direct conversion if numeric
      CODPRES = as.character(CODPRES),
      CODPTORE = as.character(CODPTORE),
      CODMUNRE = as.character(CODMUNRE),
      AREA_RES = as.character(AREA_RES),
      FECHA_NACM = as.Date(FECHA_NACM, format = "%Y-%m-%d"),
      SEG_SOCIAL = as.character(SEG_SOCIAL),
      IDCLASADMI = as.character(IDCLASADMI),
      NIV_EDUP = as.character(NIV_EDUP),
      PROFESION = as.character(PROFESION)
    )
}

# Apply the conversion function to each dataset
nac2008 <- convert_columns(nac2008)
nac2009 <- convert_columns(nac2009)
nac2010 <- convert_columns(nac2010)
nac2011 <- convert_columns(nac2011)
nac2012 <- convert_columns(nac2012)
nac2013 <- convert_columns(nac2013)
nac2014 <- convert_columns(nac2014)
nac2015 <- convert_columns(nac2015)

# Combine all datasets into one with a 'year' column
combined_data <- bind_rows(
  nac2008 %>% mutate(year = 2008),
  nac2009 %>% mutate(year = 2009),
  nac2010 %>% mutate(year = 2010),
  nac2011 %>% mutate(year = 2011),
  nac2012 %>% mutate(year = 2012),
  nac2013 %>% mutate(year = 2013),
  nac2014 %>% mutate(year = 2014),
  nac2015 %>% mutate(year = 2015)
)

combined_data <- combined_data %>% 
  select(COD_DPTO, COD_MUNIC, AREANAC, AREA_RES, PESO_NAC, ANO, MES, T_GES, TALLA_NAC,
         NUMCONSUL, NOM_INST, COD_INST, TIPO_PARTO, EDAD_MADRE, NIV_EDUM, SEG_SOCIAL)

combined_data$COD_DPTO <- sprintf("%02s", combined_data$COD_DPTO)

# Filter out NA and specific unwanted values in AREA_RES and NUMCONSUL
combined_data <- combined_data %>%
  filter(AREA_RES  %in% c("1", "2", "3")) %>% 
  filter(!is.na(NUMCONSUL) & NUMCONSUL != 99) %>% 
  mutate(
    AREA_RES = case_when(
      AREA_RES %in% c("2", "3") ~ "1",  # Recode 2 and 3 as 1 (rural)
      AREA_RES == "1" ~ "0"             # Recode 1 as 0 (non-rural)
    ),
    Post = ifelse(ANO >= 2011, 1, 0),  # Indicator for post-treatment period
    Rural = as.numeric(AREA_RES),       # Rural indicator: 1 for rural, 0 for non-rural
    SEG_SOCIAL = as.numeric(SEG_SOCIAL)          # Interaction term for DiD
  )
combined_data <- combined_data %>%
  mutate(
    COD_DPTO = if_else(nchar(COD_DPTO) == 1, sprintf("%02d", as.numeric(COD_DPTO)), COD_DPTO),
    COD_MUNIC = sprintf("%03d", as.numeric(COD_MUNIC)),
    COD_UNIQ = paste0(COD_DPTO, COD_MUNIC)
    )
combined_data <- combined_data %>% 
  filter(COD_MUNIC != 999) %>% 
  mutate(LOGNUMCONSUL = log(NUMCONSUL + 1))


#Insurer Control (Filter out uninsured)
combined_data_insured <- combined_data %>% 
  filter(SEG_SOCIAL %in% c("1", "2")) %>% 
  mutate(SEG_SOCIAL = case_when(
    SEG_SOCIAL == "1" ~ "1",
    SEG_SOCIAL == "2" ~ "0",
  ))
did_log_model_insured <- feols(LOGNUMCONSUL ~  Post*Rural*SEG_SOCIAL | ANO + COD_DPTO,  # Add other control variables if needed
                               data = combined_data_insured, cluster = ~ COD_DPTO)
etable(did_log_model_insured, tex = TRUE, file = "Did_Log_Model_Insured_Latex_Output.tex")
summary(did_log_model_insured)

#Model for women above 18 -- Unificacion PBS
combined_data_joined_initial <- combined_data_insured %>% 
  filter(EDAD_MADRE >= 3) 
combined_data_joined_initial <- combined_data_joined_initial %>% 
  mutate(
    Post = if_else((ANO > 2012) | (ANO == 2012 & as.numeric(MES) >= 7), 1, 0)
  )
pbs_initial_did <- feols(LOGNUMCONSUL ~ Post*SEG_SOCIAL*Rural | ANO + COD_DPTO , data = combined_data_joined_initial, cluster = ~ COD_DPTO)
etable(pbs_initial_did, tex = TRUE, file = "PBS_Initial_Model_Latex_Output.tex")
summary(pbs_initial_did)

#Model for women under 18 
combined_data_joined_minors <- combined_data_insured %>% 
  filter(EDAD_MADRE <= 2)
combined_data_joined_minors <- combined_data_joined_minors %>% 
  mutate(
    Post = ifelse(ANO >= 2010,1,0)
  )
pbs_minors_did <- feols(LOGNUMCONSUL ~ Post*SEG_SOCIAL*Rural | ANO + COD_DPTO , data = combined_data_joined_minors, cluster = ~ COD_DPTO)
etable(pbs_minors_did, tex = TRUE, file = "PBS_Minors_Initial_Model_Latex_Output.tex")
summary(pbs_minors_did)

#Two conditions being satified
data <- combined_data_insured %>%
  mutate(period = as.numeric(case_when(
    ANO < 2010 ~ "0",                                        # Pre-2010: No treatment
    ANO >= 2010 & (ANO < 2012 | (ANO == 2012 & MES <= 7)) ~ "1", # 2010–July 2012
    ANO > 2012 | (ANO == 2012 & MES > 7) ~ "2"               # Post-July 2012
  )),
  G = as.numeric(case_when(
    SEG_SOCIAL == "1" ~ "0",                                       # No one treated in Period 0
    SEG_SOCIAL == "0" & EDAD_MADRE < 3 ~ "1",  # Treated in 2010 (Period 1)
    SEG_SOCIAL == "0" & EDAD_MADRE >= 3 ~ "2", # Treated in 2012 (Period 2)
  ))
  )

#DiD LOGNUMCONSUL -- Never treated vs. not treated yet

example_did_both_periods <- att_gt(
  yname = "LOGNUMCONSUL",
  tname = "period",
  idname = NULL,
  gname = "G",
  data = data,
  control_group = c("nevertreated", "notyettreated"),
  panel = FALSE
)
summary(example_did_both_periods)
ggdid(example_did_both_periods)
agg.es_1 <- aggte(example_did_both_periods, type = "dynamic")
summary(agg.es_1)
save(example_did_both_periods, agg.es_1, file = "log_ATT_results.RData")

did_both_periods_non_log <- att_gt(
  yname = "NUMCONSUL",
  tname = "period",
  idname = NULL,
  gname = "G",
  data = data,
  control_group = c("nevertreated", "notyettreated"),
  panel = FALSE
)
summary(did_both_periods_non_log)
ggdid(did_both_periods_non_log)
agg.es_1 <- aggte(did_both_periods_non_log, type = "dynamic")
summary(agg.es_1)
save(did_both_periods_non_log, agg.es_1, file = "ATT_results.RData")

#Only with never treated as control group 
non_treated_example_did_both_periods <- att_gt(
  yname = "LOGNUMCONSUL",
  tname = "period",
  idname = NULL,
  gname = "G",
  data = data,
  control_group = "nevertreated",
  panel = FALSE
)
summary(non_treated_example_did_both_periods)
ggdid(non_treated_example_did_both_periods)
nt_agg.es_1 <- aggte(non_treated_example_did_both_periods, type = "dynamic")
summary(nt_agg.es_1)
save(non_treated_example_did_both_periods, nt_agg.es_1, file = "nt_log_ATT_results.RData")
#NON LOG
non_treated_did_both_periods_non_log <- att_gt(
  yname = "NUMCONSUL",
  tname = "period",
  idname = NULL,
  gname = "G",
  data = data,
  control_group = "nevertreated",
  panel = FALSE
)
summary(non_treated_did_both_periods_non_log)
ggdid(non_treated_did_both_periods_non_log)
nt_agg.es_1_nl <- aggte(non_treated_did_both_periods_non_log, type = "dynamic")
summary(nt_agg.es_1_nl)
save(non_treated_did_both_periods_non_log, nt_agg.es_1_nl , file = "ATT_results.RData")

#Join Combined_data with codes_data
codes_data <- codes_data %>% 
  mutate(COD_UNIQ = str_pad(COD_UNIQ, width = 5, pad = "0"))

combined_data <- left_join(combined_data,
                           codes_data, by = "COD_UNIQ"
)

#Initial Exploration of Codes_data
percentage_data <- combined_data %>%
  filter(!is.na(TIPO)) %>%
  group_by(ANO, TIPO) %>%
  summarise(
    Percentage_Less_Than_4 = mean(NUMCONSUL < 4, na.rm = TRUE) * 100
  ) %>%
  arrange(ANO, TIPO)

#Graph for low visits
percentage_data <- combined_data %>%
  filter(!is.na(TIPO)) %>%
  group_by(ANO, TIPO) %>%
  summarise(
    Percentage_Less_Than_4 = mean(NUMCONSUL <= 4, na.rm = TRUE) * 100
  ) %>%
  mutate(ANO = as.numeric(ANO)) %>%
  arrange(ANO, TIPO)

# Create the line graph
# Convert TIPO to a factor with the appropriate labels
percentage_data$TIPO <- factor(percentage_data$TIPO, 
                               levels = c(1, 2, 3, 4), 
                               labels = c("Urban", "Intermediate", "Rural", "Scattered Rural"))

# Updated ggplot code
ggplot(percentage_data, aes(x = ANO, y = Percentage_Less_Than_4, color = TIPO)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Percentage of Women with Low Pre-Natal Care Over Time",
    x = "Year",
    y = "Percentage",
    color = "Type"
  ) +
  scale_x_continuous(breaks = unique(percentage_data$ANO)) +
  scale_color_manual(
    values = c("Urban" = "blue", "Intermediate" = "green", "Rural" = "red", "Scattered Rural" = "purple")
  ) +
  theme_minimal()

#CDF For Codes_data
unique_years <- unique(combined_data$ANO)

# Loop through each year and create a graph
for (year in unique_years) {
  # Filter data for the specific year
  year_data_tipo <- combined_data %>%
    filter(ANO == year) %>%
    group_by(NUMCONSUL, TIPO) %>%
    summarise(count = n()) %>%
    ungroup() %>%
    group_by(TIPO) %>%
    mutate(
      cumulative_count = cumsum(count),
      total_count = sum(count),
      cumulative_percentage = (cumulative_count / total_count) * 100
    )
  
  # Create the plot
  plot_tipo <- ggplot(year_data_tipo, aes(x = NUMCONSUL, y = cumulative_percentage, color = as.factor(TIPO))) +
    geom_line(size = 1) +
    labs(
      title = paste("Cumulative Percentage of Pre-Natal Visits by TIPO -", year),
      x = "Number of Pre-Natal Visits",
      y = "Cumulative Percentage (%)",
      color = "TIPO"
    ) +
    theme_minimal() +
    theme(legend.position = "right") +
    scale_color_manual(values = c("blue", "green", "red", "purple"), labels = c("Urban", "Intermediate", "Rural", "Scattered Rural")) +  # Adjust colors as needed
    scale_x_continuous(limits = c(0, 25)) +
    scale_y_continuous(limits = c(0, 100))
  
  # Save the plot as a PDF
  pdf_filename <- paste0("Tipo_Cumulative_Percentage_PreNatal_Visits", year, ".pdf")
  ggsave(pdf_filename, plot = plot_tipo, device = "pdf", width = 8, height = 6)
}

#Use Crosswalk to Analyze NUMCONSUL as the dependent variable

#Model with TIPO as factor
did_type_all <- feols(LOGNUMCONSUL ~ Post *SEG_SOCIAL* as.factor(TIPO) | ANO + COD_DPTO,
                      data = combined_data_insured, cluster = ~ COD_DPTO)

summary(did_type_all)
etable(did_log_model_insured, did_type_all, tex = TRUE, file = "/Users/pablotorresr/Desktop/Classes_Columbia/7th Semester/Senior Thesis/Did_2011Law_Output.tex")


#Model for women above 18 -- Unificacion PBS 
pbs_tipo_did <- feols(LOGNUMCONSUL ~ Post*SEG_SOCIAL*as.factor(TIPO) | ANO +COD_DPTO, data = combined_data_joined_initial, cluster = ~ COD_DPTO)
summary(pbs_tipo_did)


#Model for women under 18 
pbs_minors_tipo_did <- feols(LOGNUMCONSUL ~ Post*SEG_SOCIAL*as.factor(TIPO) | COD_DPTO + ANO, data = combined_data_joined_minors, cluster = ~ COD_DPTO)
summary(pbs_minors_tipo_did)
etable(pbs_minors_did, pbs_minors_tipo_did, tex = TRUE, file = "/Users/pablotorresr/Desktop/Classes_Columbia/7th Semester/Senior Thesis/PBS_Minors_Output.tex")

#Join HHI and Combined Data
month_lookup <- c(
  "Enero" = "01", "Febrero" = "02", "Marzo" = "03", "Abril" = "04",
  "Mayo" = "05", "Junio" = "06", "Julio" = "07", "Agosto" = "08",
  "Septiembre" = "09", "Octubre" = "10", "Noviembre" = "11", "Diciembre" = "12"
)

# Split month_year into mes and ano, and map mes to numeric format
hhi_data_with_change <- hhi_data_with_change %>%
  separate(
    col = month_year,
    into = c("ANO", "MES"),
    sep = "-"
  ) %>%
  mutate(
    MES = recode(MES, !!!month_lookup)  # Convert month names to two-digit numbers
  )

# Join with combined_data
hhi_data_with_change <- hhi_data_with_change %>%
  mutate(SEG_SOCIAL = as.character(SEG_SOCIAL))
combined_hhi_data <- left_join(combined_data_insured, hhi_data_with_change, by = c("COD_UNIQ", "MES", "ANO", "SEG_SOCIAL"))
combined_hhi_data_adults <- combined_hhi_data %>% 
  filter(EDAD_MADRE >= 3) 
combined_hhi_data_minors <- combined_hhi_data %>% 
  filter(EDAD_MADRE < 3) 
combined_hhi_data_adults <- combined_hhi_data_adults %>% 
  mutate(Post = if_else((ANO > 2012) | (ANO == 2012 & as.numeric(MES) >= 7), 1, 0))
pbs_hhi_adult_did <- feols(LOGNUMCONSUL ~ Post*SEG_SOCIAL*Rural*HHI | COD_DPTO + ANO, data = combined_hhi_data_adults, cluster = ~ COD_DPTO)
pbs_change_in_hhi_adult_did <- feols(LOGNUMCONSUL ~ Post*SEG_SOCIAL*Rural*change_in_HHI | COD_DPTO + ANO, data = combined_hhi_data_adults, cluster = ~ COD_DPTO)

etable(pbs_initial_did, pbs_tipo_did, pbs_hhi_adult_did, tex = TRUE, file = "/Users/pablotorresr/Desktop/Classes_Columbia/7th Semester/Senior Thesis/PBS_Adults_Output.tex")


#Visualization of Parallel Trends
combined_data_insured <- combined_data_insured %>%
  mutate(Less_Than_4 = ifelse(NUMCONSUL < 4, 1, 0))

# Aggregate the data to compute the percentage of individuals with NUMCONSUL < 4
percentage_less_than_4 <- combined_data_insured %>%
  group_by(ANO, SEG_SOCIAL) %>%
  summarise(Percent_Less_Than_4 = mean(Less_Than_4, na.rm = TRUE) * 100)

# Map SEG_SOCIAL values to meaningful labels
percentage_less_than_4 <- percentage_less_than_4 %>%
  mutate(SEG_SOCIAL_Label = ifelse(SEG_SOCIAL == 0, "Subsidized", "Contributory"))

# Create the graph
ggplot(percentage_less_than_4, aes(x = ANO, y = Percent_Less_Than_4, color = SEG_SOCIAL_Label, group = SEG_SOCIAL)) +
  geom_line(size = 1) +
  geom_point(size = 2) + 
  geom_vline(xintercept = 2012, linetype = "dashed", color = "black") +
  labs(
    title = "Percentage of Women with Low Prenatal care over time",
    x = "Year",
    y = "Percentage with < 4 Pre-Natal Visits",
    color = "Insurance Scheme"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10)
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1))

#Gestation, Size, Weight
data_ges <- combined_data %>%
  filter(!T_GES %in% c(6, 9)) %>%
  group_by(ANO, T_GES) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(total_count_year = sum(count, na.rm = TRUE), .by = "ANO") %>%
  mutate(percentage = round((count / total_count_year) * 100, 4))

# For TALLA_NAC (filtering out 9)
data_size <- combined_data %>%
  filter(TALLA_NAC != 9) %>%
  group_by(ANO, TALLA_NAC) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(total_count_year = sum(count, na.rm = TRUE), .by = "ANO") %>%
  mutate(percentage = round((count / total_count_year) * 100, 4))

# For PESO_NAC (filtering out 9)
data_weight <- combined_data %>%
  filter(PESO_NAC != 9) %>%
  group_by(ANO, PESO_NAC) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(total_count_year = sum(count, na.rm = TRUE), .by = "ANO") %>%
  mutate(percentage = round((count / total_count_year) * 100, 4))

a <- combined_data %>%
  filter(!T_GES %in% c(6, 9)) %>%
  filter(AREA_RES %in% c(1,2,3)) %>% 
  group_by(ANO, AREA_RES, T_GES) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(total_count_year_area = sum(count, na.rm = TRUE), .by = c("ANO", "AREA_RES")) %>%
  mutate(percentage = round((count / total_count_year_area) * 100, 4))

b <- combined_data %>%
  filter(TALLA_NAC != 9) %>%
  filter(AREA_RES %in% c(1,2,3)) %>% 
  group_by(ANO, AREA_RES, TALLA_NAC) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(total_count_year_area = sum(count, na.rm = TRUE), .by = c("ANO", "AREA_RES")) %>%
  mutate(percentage = round((count / total_count_year_area) * 100, 4))

c <- combined_data %>%
  filter(PESO_NAC != 9) %>%
  filter(AREA_RES %in% c(1,2,3)) %>% 
  group_by(ANO, AREA_RES, PESO_NAC) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(total_count_year_area = sum(count, na.rm = TRUE), .by = c("ANO", "AREA_RES")) %>%
  mutate(percentage = round((count / total_count_year_area) * 100, 4))

#CDF by TIPO for the three variables
calculate_percentages_and_cumulative <- function(data, variable, max_value) {
  variable_sym <- sym(variable)  # Create a symbol from the variable name
  
  data %>%
    mutate(!!variable_sym := as.character(!!variable_sym)) %>%  # Convert variable to character for processing
    filter(!is.na(!!variable_sym)) %>%
    filter(as.numeric(!!variable_sym) <= max_value) %>%
    filter(TIPO %in% c(1, 2, 3, 4)) %>%
    group_by(ANO, TIPO, !!variable_sym) %>%
    summarise(count = n(), .groups = "drop") %>%
    complete(
      !!variable_sym := as.character(seq(0, max_value, by = 1)),
      nesting(ANO, TIPO),
      fill = list(count = 0)
    ) %>%
    group_by(ANO, TIPO) %>%
    mutate(total_count_year_tipo = sum(count, na.rm = TRUE)) %>%  # Calculate totals here
    ungroup() %>%
    mutate(
      percentage = ifelse(total_count_year_tipo > 0, (count / total_count_year_tipo) * 100, 0)
    ) %>%
    arrange(ANO, TIPO, !!variable_sym) %>%
    group_by(ANO, TIPO) %>%
    mutate(
      cumulative_count = cumsum(count),
      cumulative_percentage = ifelse(total_count_year_tipo > 0, (cumulative_count / total_count_year_tipo) * 100, 0)
    ) %>%
    mutate(!!variable_sym := as.numeric(!!variable_sym)) %>%  # Convert back to numeric
    ungroup()
}

# Process the data for each variable
processed_t_ges <- calculate_percentages_and_cumulative(combined_data, "T_GES", 5)
processed_talla_nac <- calculate_percentages_and_cumulative(combined_data, "TALLA_NAC", 6)
processed_peso_nac <- calculate_percentages_and_cumulative(combined_data, "PESO_NAC", 8)

# Combine data across years
plot_data <- bind_rows(
  processed_t_ges %>% mutate(variable = "T_GES"),
  processed_talla_nac %>% mutate(variable = "TALLA_NAC"),
  processed_peso_nac %>% mutate(variable = "PESO_NAC")
)

# Debug: Check the combined data
print("DEBUG: Combined Plot Data")
print(head(plot_data))

# Generate plots
for (year in unique(plot_data$ANO)) {
  year_data <- plot_data %>% filter(ANO == year)
  for (var in unique(year_data$variable)) {
    var_data <- year_data %>% filter(variable == var)
    
    # Debug: Print var_data for verification
    print("DEBUG: var_data for plotting")
    print(head(var_data))
    
    # Create the plot
    p <- ggplot(var_data, aes(x = !!sym(var), y = cumulative_percentage, color = as.factor(TIPO))) +
      geom_line(size = 1) +
      labs(
        title = paste("Cumulative Percentage for", var, "-", year),
        x = var,
        y = "Cumulative Percentage (%)",
        color = "TIPO"
      ) +
      theme_minimal() +
      theme(legend.position = "right") +
      scale_color_manual(values = c("blue", "green", "red", "purple"), labels = c("Urban", "Intermediate", "Rural", "Scattered Rural"))
    
    # Save the plot as a PDF
    pdf_filename <- paste0("Cumulative_Percentage_", var, "_", year, ".pdf")
    ggsave(pdf_filename, plot = p, device = "pdf", width = 8, height = 6)
  }
}

#Do by Seg_Social

calculate_percentages_and_cumulative <- function(data, variable, max_value) {
  variable_sym <- sym(variable)  # Create a symbol from the variable name
  
  data %>%
    mutate(!!variable_sym := as.character(!!variable_sym)) %>%  # Convert variable to character for processing
    filter(!is.na(!!variable_sym)) %>%
    filter(as.numeric(!!variable_sym) <= max_value) %>%
    filter(SEG_SOCIAL %in% c(0, 1)) %>%  # Filter valid SEG_SOCIAL values
    group_by(ANO, SEG_SOCIAL, !!variable_sym) %>%
    summarise(count = n(), .groups = "drop") %>%
    complete(
      !!variable_sym := as.character(seq(0, max_value, by = 1)),
      nesting(ANO, SEG_SOCIAL),
      fill = list(count = 0)
    ) %>%
    group_by(ANO, SEG_SOCIAL) %>%
    mutate(total_count_year_scheme = sum(count, na.rm = TRUE)) %>%  # Calculate totals here
    ungroup() %>%
    mutate(
      percentage = ifelse(total_count_year_scheme > 0, (count / total_count_year_scheme) * 100, 0)
    ) %>%
    arrange(ANO, SEG_SOCIAL, !!variable_sym) %>%
    group_by(ANO, SEG_SOCIAL) %>%
    mutate(
      cumulative_count = cumsum(count),
      cumulative_percentage = ifelse(total_count_year_scheme > 0, (cumulative_count / total_count_year_scheme) * 100, 0)
    ) %>%
    mutate(!!variable_sym := as.numeric(!!variable_sym)) %>%  # Convert back to numeric
    ungroup()
}
# Process T_GES with bounds 0 to 5
processed_t_ges <- calculate_percentages_and_cumulative(combined_data_insured, "T_GES", 5)

# Process TALLA_NAC with bounds (adjust these if needed)
processed_talla_nac <- calculate_percentages_and_cumulative(combined_data_insured, "TALLA_NAC", 6)

# Process PESO_NAC with bounds (adjust these if needed)
processed_peso_nac <- calculate_percentages_and_cumulative(combined_data_insured, "PESO_NAC", 8)
plot_data <- bind_rows(
  processed_t_ges %>% mutate(variable = "T_GES"),
  processed_talla_nac %>% mutate(variable = "TALLA_NAC"),
  processed_peso_nac %>% mutate(variable = "PESO_NAC")
)
for (year in unique(plot_data$ANO)) {
  year_data <- plot_data %>% filter(ANO == year)
  for (var in unique(year_data$variable)) {
    var_data <- year_data %>% filter(variable == var)
    # Create the plot
    p <- ggplot(var_data, aes(x = !!sym(var), y = cumulative_percentage, color = as.factor(SEG_SOCIAL))) +
      geom_line(size = 1) +
      labs(
        title = paste("Cumulative Percentage for", var, "-", year),
        x = var,
        y = "Cumulative Percentage (%)",
        color = "Insurance Scheme"
      ) +
      theme_minimal() +
      theme(legend.position = "right") +
      scale_color_manual(values = c("blue", "red"), labels = c("Subsidized", "Contributory"))
    
    # Save the plot as a PDF
    pdf_filename <- paste0("Cumulative_Percentage_", var, "_", year, ".pdf")
    ggsave(pdf_filename, plot = p, device = "pdf", width = 8, height = 6)
  }
}
#Normal vs not Normal Weight
prepare_data_by_social <- function(data) {
  data %>%
    mutate(
      normal_weight = ifelse(PESO_NAC %in% c(5, 6, 7), "Normal", "Not Normal")
    ) %>%
    group_by(ANO, SEG_SOCIAL, normal_weight) %>%
    summarise(count = n(), .groups = "drop") %>%
    group_by(ANO, SEG_SOCIAL) %>%
    summarise(
      total_count = sum(count),
      normal_count = sum(count[normal_weight == "Normal"]),
      .groups = "drop"
    ) %>%
    mutate(percentage = (normal_count / total_count) * 100)
}

normal_weight_by_social <- prepare_data_by_social(combined_data_insured)
prepare_data_by_tipo <- function(data) {
  data %>%
    filter(!is.na(TIPO)) %>%  # Exclude rows with NA TIPO
    mutate(
      normal_weight = ifelse(PESO_NAC %in% c(5, 6, 7), "Normal", "Not Normal")
    ) %>%
    group_by(ANO, TIPO, normal_weight) %>%
    summarise(count = n(), .groups = "drop") %>%
    group_by(ANO, TIPO) %>%
    summarise(
      total_count = sum(count),
      normal_count = sum(count[normal_weight == "Normal"]),
      .groups = "drop"
    ) %>%
    mutate(percentage = (normal_count / total_count) * 100)
}

normal_weight_by_tipo <- prepare_data_by_tipo(combined_data_insured)

p_social <- ggplot(normal_weight_by_social, aes(x = ANO, y = percentage, color = as.factor(SEG_SOCIAL), group = SEG_SOCIAL)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Percentage of Infants with Normal Weight by Insurance Scheme",
    x = "Year",
    y = "Percentage of Infants with Normal Weight (%)",
    color = "Insurance Scheme"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("blue", "red"), labels = c("Subsidized", "Contributory"))

ggsave("Percentage_Normal_Weight_SEG_SOCIAL.pdf", plot = p_social, device = "pdf", width = 8, height = 6)
p_tipo <- ggplot(normal_weight_by_tipo, aes(x = ANO, y = percentage, color = as.factor(TIPO), group = TIPO)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Percentage of Infants with Normal Weight by Birth Type",
    x = "Year",
    y = "Percentage of Infants with Normal Weight (%)",
    color = "Birth Type (TIPO)"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("blue", "green", "red", "purple"), labels = c("Urban", "Intermediate", "Rural", "Scattered Rural"))

ggsave("Percentage_Normal_Weight_TIPO.pdf", plot = p_tipo, device = "pdf", width = 8, height = 6)


#For TALLA- size
prepare_length_data_by_social <- function(data) {
  data %>%
    mutate(
      normal_length = ifelse(TALLA_NAC %in% c(4, 5), "Normal", "Not Normal")
    ) %>%
    group_by(ANO, SEG_SOCIAL, normal_length) %>%
    summarise(count = n(), .groups = "drop") %>%
    group_by(ANO, SEG_SOCIAL) %>%
    summarise(
      total_count = sum(count),
      normal_count = sum(count[normal_length == "Normal"]),
      .groups = "drop"
    ) %>%
    mutate(percentage = (normal_count / total_count) * 100)
}

prepare_length_data_by_tipo <- function(data) {
  data %>%
    filter(!is.na(TIPO)) %>%
    mutate(
      normal_length = ifelse(TALLA_NAC %in% c(4, 5), "Normal", "Not Normal")
    ) %>%
    group_by(ANO, TIPO, normal_length) %>%
    summarise(count = n(), .groups = "drop") %>%
    group_by(ANO, TIPO) %>%
    summarise(
      total_count = sum(count),
      normal_count = sum(count[normal_length == "Normal"]),
      .groups = "drop"
    ) %>%
    mutate(percentage = (normal_count / total_count) * 100)
}

length_by_social <- prepare_length_data_by_social(combined_data_insured)

p_social_length <- ggplot(length_by_social, aes(x = ANO, y = percentage, color = as.factor(SEG_SOCIAL), group = SEG_SOCIAL)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Percentage of Infants with Normal Length at Birth by Insurance Scheme",
    x = "Year",
    y = "Percentage of Infants with Normal Length at Birth (%)",
    color = "Insurance Scheme"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("blue", "red"), labels = c("Subsidized", "Contributory"))

ggsave("Percentage_Normal_Length_SEG_SOCIAL.pdf", plot = p_social_length, device = "pdf", width = 8, height = 6)

length_by_tipo <- prepare_length_data_by_tipo(combined_data_insured)

p_tipo_length <- ggplot(length_by_tipo, aes(x = ANO, y = percentage, color = as.factor(TIPO), group = TIPO)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Percentage of Infants with Normal Length at Birth by Rurality Category",
    x = "Year",
    y = "Percentage of Infants with Normal Length at Birth (%)",
    color = "Rurality Category"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("blue", "green", "red", "purple"),
                     labels = c("Urban", "Intermediate", "Rural", "Scattered Rural"))

ggsave("Percentage_Normal_Length_TIPO.pdf", plot = p_tipo_length, device = "pdf", width = 8, height = 6)

#Gestation Time Graphs
prepare_gestation_data_by_social <- function(data) {
  data %>%
    mutate(
      normal_gestation = ifelse(T_GES %in% c(4, 5), "Normal", "Not Normal")
    ) %>%
    group_by(ANO, SEG_SOCIAL, normal_gestation) %>%
    summarise(count = n(), .groups = "drop") %>%
    group_by(ANO, SEG_SOCIAL) %>%
    summarise(
      total_count = sum(count),
      normal_count = sum(count[normal_gestation == "Normal"]),
      .groups = "drop"
    ) %>%
    mutate(percentage = (normal_count / total_count) * 100)
}

prepare_gestation_data_by_tipo <- function(data) {
  data %>%
    filter(!is.na(TIPO)) %>%
    mutate(
      normal_gestation = ifelse(T_GES %in% c(4, 5), "Normal", "Not Normal")
    ) %>%
    group_by(ANO, TIPO, normal_gestation) %>%
    summarise(count = n(), .groups = "drop") %>%
    group_by(ANO, TIPO) %>%
    summarise(
      total_count = sum(count),
      normal_count = sum(count[normal_gestation == "Normal"]),
      .groups = "drop"
    ) %>%
    mutate(percentage = (normal_count / total_count) * 100)
}

gestation_by_social <- prepare_gestation_data_by_social(combined_data_insured)

p_social_gestation <- ggplot(gestation_by_social, aes(x = ANO, y = percentage, color = as.factor(SEG_SOCIAL), group = SEG_SOCIAL)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Percentage of Newborns with Normal Gestation Time by Insurance Scheme",
    x = "Year",
    y = "Percentage of Newborns with Normal Gestation Time (%)",
    color = "Insurance Scheme"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("blue", "red"), labels = c("Subsidized", "Contributory"))

ggsave("Percentage_Normal_Gestation_SEG_SOCIAL.pdf", plot = p_social_gestation, device = "pdf", width = 8, height = 6)

gestation_by_tipo <- prepare_gestation_data_by_tipo(combined_data_insured)

p_tipo_gestation <- ggplot(gestation_by_tipo, aes(x = ANO, y = percentage, color = as.factor(TIPO), group = TIPO)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Percentage of Newborns with Normal Gestation Time by Rurality Category",
    x = "Year",
    y = "Percentage of Newborns with Normal Gestation Time (%)",
    color = "Rurality Category"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("blue", "green", "red", "purple"),
                     labels = c("Urban", "Intermediate", "Rural", "Scattered Rural"))

ggsave("Percentage_Normal_Gestation_TIPO.pdf", plot = p_tipo_gestation, device = "pdf", width = 8, height = 6)




#Fetal Deaths Data Set
fetal2008 <- fetal2008 %>%
  mutate(C_DIR1 = if_else(C_DIR1 == "P95X", "P95", C_DIR1))

count_icd <- function(data, year) {
  count(data %>% group_by(C_DIR1)) %>% 
    rename(!!paste0("Year_", year) := n)
}
icd2008 <- count_icd(fetal2008, 2008)
icd2009 <- count_icd(fetal2009, 2009)
icd2010 <- count_icd(fetal2010, 2010)
icd2011 <- count_icd(fetal2011, 2011)
icd2012 <- count_icd(fetal2012, 2012)
icd2013 <- count_icd(fetal2013, 2013)
icd2014 <- count_icd(fetal2014, 2014)
icd2015 <- count_icd(fetal2015, 2015)
icd_list <- list(icd2008, icd2009, icd2010, icd2011, icd2012, icd2013, icd2014, icd2015)
# Use reduce() to perform sequential full joins
icd_all <- reduce(icd_list, full_join, by = "C_DIR1")

icd_all <- icd_all %>%
  ungroup() %>%  # Remove any accidental grouping
  mutate(Total = rowSums(select(., where(is.numeric)), na.rm = TRUE)) 
icd_all <- icd_all %>% 
  mutate(Percentage = (Total / sum(Total, na.rm = TRUE)) * 100)
c_dir1_codes_vector <- icd_all %>%
  filter(Percentage >= 0.5) %>%
  pull(C_DIR1)

#Crosswalks for p,q codes
crosswalk_pcodes <- crosswalk_pcodes %>% 
  rename(C_DIR1 = code)
crosswalk_qcodes <- crosswalk_qcodes %>% 
  rename(C_DIR1 = code)
crosswalk_icd <- bind_rows(crosswalk_pcodes, crosswalk_qcodes)
expand_icd_range <- function(code_range) {
  if (!str_detect(code_range, "-")) {
    return(code_range)  # Keep single codes unchanged
  }
  
  # Split range (e.g., "P00-P04" -> "P00", "P04")
  split_range <- strsplit(code_range, "-")[[1]]
  prefix <- substr(split_range[1], 1, 1)  # Extract prefix (P or Q)
  start_num <- as.numeric(substr(split_range[1], 2, nchar(split_range[1])))
  end_num <- as.numeric(substr(split_range[2], 2, nchar(split_range[2])))
  
  # If parsing fails, return original value
  if (is.na(start_num) | is.na(end_num)) {
    return(code_range)
  }
  
  # Generate full ICD-10 codes (P00, P01, ..., P04)
  expanded_codes <- paste0(prefix, sprintf("%02d", start_num:end_num))
  return(expanded_codes)
}

# Apply function only to range-based codes
crosswalk_icd_expanded <- crosswalk_icd %>%
  rowwise() %>%
  mutate(C_DIR1 = list(expand_icd_range(C_DIR1))) %>%  # Expand ranges into lists
  unnest(C_DIR1)

crosswalk_icd_expanded <- crosswalk_icd_expanded %>%
  mutate(C_DIR1 = if_else(C_DIR1 == "XVI", "P97" , C_DIR1)) %>% 
  filter(C_DIR1 != "XVII")

icd_all <- icd_all %>% 
  left_join(crosswalk_icd_expanded, by = "C_DIR1")

#Data with all deaths
select_columns <- function(data) {
  data %>%
    select(C_DIR1, COD_DPTO, COD_MUNIC, ANO, MES, AREA_RES, SEG_SOCIAL, PMAN_MUER, T_GES, PESO_NAC,NIV_EDUM, EDAD_MADRE)  # Replace with actual column names
}

fetal2008 <- select_columns(fetal2008)
fetal2009 <- select_columns(fetal2009)
fetal2010 <- select_columns(fetal2010)
fetal2011 <- select_columns(fetal2011)
fetal2012 <- select_columns(fetal2012)
fetal2013 <- select_columns(fetal2013)
fetal2014 <- select_columns(fetal2014)
fetal2015 <- select_columns(fetal2015)


prepare_data <- function(data, year) {
  data %>%
    mutate(
      year = year,
      COD_DPTO = as.character(COD_DPTO),
      COD_MUNIC = as.character(COD_MUNIC),
      ANO = as.character(ANO),
      MES = as.character(MES),
      AREA_RES = as.character(AREA_RES),
      SEG_SOCIAL = as.character(SEG_SOCIAL),
      PMAN_MUER = as.character(PMAN_MUER),
      T_GES = as.character(T_GES),
      NIV_EDUM = as.character(NIV_EDUM),
      EDAD_MADRE = as.character(EDAD_MADRE) # Ensure consistent type
    )
}

# Bind all datasets with standardized `COD_DPTO`
combined_data_deaths <- bind_rows(
  prepare_data(fetal2008, 2008),
  prepare_data(fetal2009, 2009),
  prepare_data(fetal2010, 2010),
  prepare_data(fetal2011, 2011),
  prepare_data(fetal2012, 2012),
  prepare_data(fetal2013, 2013),
  prepare_data(fetal2014, 2014),
  prepare_data(fetal2015, 2015)
)

combined_data_deaths <- combined_data_deaths %>% 
  mutate(COD_DPTO = if_else(COD_DPTO == "5", "05" , COD_DPTO)) %>% 
  mutate(COD_DPTO = if_else(COD_DPTO == "8", "08" , COD_DPTO)) %>% 
  mutate(COD_MUNIC = sprintf("%03d", as.numeric(COD_MUNIC))) %>% 
  filter(COD_MUNIC != "999") %>% 
  filter(EDAD_MADRE != "99") %>% 
  mutate(COD_UNIQ = paste0(COD_DPTO, COD_MUNIC)) %>% 
  mutate(MES = sub("^0", "", MES)) %>% 
  filter(AREA_RES %in% c("1", "2", "3")) 

combined_data_deaths <- left_join(combined_data_deaths,
                                 codes_data, by = "COD_UNIQ"
)
#Filter out abortions and pregnancy terminations
combined_data_deaths <- combined_data_deaths %>% 
  filter(C_DIR1 != "P964")

combined_data_deaths_insured <- combined_data_deaths %>% 
  filter(SEG_SOCIAL %in% c("1", "2"))

combined_data_deaths_insured_reg <- combined_data_deaths_insured %>% 
  filter(C_DIR1 != "P018")

adults_death_reg <- combined_data_deaths_insured_reg %>% 
  filter(EDAD_MADRE >= 3) 

pbs_adults_death_counts <- adults_death_reg %>%
  group_by(COD_UNIQ, ANO, MES, SEG_SOCIAL, COD_DPTO) %>%
  summarise(deaths = n(), .groups = "drop") %>%
  mutate(
    ANO = as.numeric(ANO),
    Post = if_else((ANO > 2012) | (ANO == 2012 & as.numeric(MES) >= 7), 1, 0)
  ) %>% 
  mutate(LOGDEATHS = log(deaths +1))

pbs_deaths_adult_did <- feols(LOGDEATHS ~ Post*SEG_SOCIAL | ANO +COD_DPTO, data = pbs_adults_death_counts, cluster = ~ COD_DPTO)
summary(pbs_deaths_adult_did)

minors_death_reg <- combined_data_deaths_insured_reg %>% 
  filter(EDAD_MADRE < 3)

pbs_minors_death_counts <- minors_death_reg %>%
  group_by(COD_UNIQ, ANO, MES, SEG_SOCIAL, COD_DPTO) %>%
  summarise(deaths = n(), .groups = "drop") %>%
  mutate(
    ANO = as.numeric(ANO),
    Post = if_else((ANO >= 2010), 1, 0)
  ) %>% 
  mutate(LOGDEATHS = log(deaths +1))

pbs_deaths_minors_did <- feols(LOGDEATHS ~ Post*SEG_SOCIAL | ANO +COD_DPTO, data = pbs_minors_death_counts, cluster = ~ COD_DPTO)
summary(pbs_deaths_minors_did)


rural_death_counts <- adults_death_reg %>%
  group_by(COD_UNIQ, ANO, AREA_RES, MES, SEG_SOCIAL, COD_DPTO) %>%
  summarise(deaths = n(), .groups = "drop") %>%
  mutate(
    Rural = case_when(
      AREA_RES %in% c(2, 3) ~ 1,  # Rural if AREA_RES is 2 or 3
      AREA_RES == 1 ~ 0  # Non-Rural if AREA_RES is 1
    )
  ) %>% 
  mutate(
    ANO = as.numeric(ANO),  
    Post = if_else(ANO >= 2011, 1, 0)) %>% 
  mutate(LOGDEATHS = log(deaths +1))

rural_deaths_did <- feols(LOGDEATHS ~ Post*SEG_SOCIAL*Rural | ANO +COD_DPTO, data = rural_death_counts, cluster = ~ COD_DPTO)
summary(rural_deaths_did)


# Function to plot yearly evolution of total deaths for any grouping variable
plot_yearly_trend <- function(data, grouping_var) {
  grouping_var <- rlang::ensym(grouping_var)  # Convert input to symbol for dplyr
  
  # Aggregate total deaths per year per chosen grouping variable
  yearly_deaths <- data %>%
    group_by(year, !!grouping_var) %>%
    summarise(total_cases = n(), .groups = "drop")  # Count the number of deaths
  
  # Create the plot
  ggplot(yearly_deaths, aes(x = year, y = total_cases, color = as.factor(!!grouping_var), group = !!grouping_var)) +
    geom_line(size = 1) +        # Line plot
    geom_point(size = 2) +       # Add points for visibility
    scale_y_log10() +            # Use log scale for y-axis
    labs(title = paste("Yearly Evolution of Total Cases by", deparse(substitute(grouping_var)), "(Log Scale)"),
         x = "Year",
         y = "Total Deaths (log scale)",
         color = as.character(grouping_var)) +  # Legend title
    theme_minimal() +
    scale_x_continuous(breaks = unique(yearly_deaths$year))  # Ensure years appear properly
}

# Example usage:
plot_yearly_trend(combined_data_deaths, AREA_RES)  # Plots for AREA_RES
plot_yearly_trend(combined_data_deaths, COD_DPTO)  # Plots for COD_DPTO instead
plot_yearly_trend(combined_data_deaths, TIPO)
plot_yearly_trend(combined_data_deaths_insured, SEG_SOCIAL)

#Drop those Municipalities were Deaths increase threefold
yearly_munic_cases <- combined_data_deaths %>%
  group_by(year, COD_UNIQ) %>%
  summarise(total_cases = n(), .groups = "drop")  # Count total deaths

# Filter for 2008 and 2015 and reshape for comparison
munic_2008_2015 <- yearly_munic_cases %>%
  filter(year %in% c(2008, 2015)) %>%
  pivot_wider(names_from = year, values_from = total_cases, names_prefix = "cases_") %>%
  mutate(ratio = cases_2015 / cases_2008) %>%  # Calculate ratio
  filter(ratio >= 3)  # Keep only those where 2015 cases are at least 3x 2008 cases

# Extract the list of COD_MUNIC values that satisfy the condition
cod_munic_list <- munic_2008_2015 %>%
  pull(COD_UNIQ)

# View result
print(cod_munic_list)

combined_data_deaths_filtered <- combined_data_deaths %>% 
  filter(!(COD_UNIQ %in% cod_munic_list))
plot_yearly_trend(combined_data_deaths_filtered, AREA_RES)
plot_yearly_trend(combined_data_deaths_filtered, TIPO)
combined_data_deaths_filtered_insured <- combined_data_deaths_filtered %>% 
  filter(SEG_SOCIAL %in% c("1", "2"))
plot_yearly_trend(combined_data_deaths_filtered_insured, SEG_SOCIAL)

top_20_d_values <- icd_all %>%
  arrange(desc(Percentage)) %>% 
  slice_head(n = 20) %>%  # Take the top 20 values
  pull(C_DIR1)  # Extract only C_DIR1 values as a vector



#Look at important numbers -- 4,5,8,10,15,17
selected_codes <- top_20_d_values[c(4,8,10)]
combined_data_selected_illnesses <- combined_data_deaths %>% 
  filter(C_DIR1 %in% selected_codes)
combined_data_selected_illnesses <- combined_data_selected_illnesses %>% 
  left_join(crosswalk_icd, by = "C_DIR1")
#Merge P200 and P209
merged_terms <- combined_data_selected_illnesses %>%
  filter(C_DIR1 %in% c("P200", "P209")) %>%
  summarise(term = paste(unique(term), collapse = " / ")) %>%  # Combine unique term descriptions
  mutate(C_DIR1 = "P200")  # Set the new merged C_DIR1

# Step 2: Reassign P209 to P200 in the dataset
combined_data_selected_illnesses <- combined_data_selected_illnesses %>%
  mutate(C_DIR1 = if_else(C_DIR1 == "P209", "P200", C_DIR1))  # Convert P209 to P200
combined_data_selected_illnesses <- combined_data_selected_illnesses %>%
  mutate(term = if_else(C_DIR1 == "P200", "Intrauterine hypoxia", term))

combined_data <- combined_data %>%
  mutate(NUMCONSUL = as.numeric(NUMCONSUL)) %>%  # Ensure numeric type
  filter(NUMCONSUL <= 50)

# Function to prepare data for mortality analysis based on a chosen grouping variable
prepare_mortality_data <- function(data_births, data_deaths_selected, data_deaths_all, group_var) {
  group_var <- rlang::ensym(group_var)
  
  # Total deaths for true mortality denominator
  total_deaths_all <- data_deaths_all %>%
    group_by(ANO, !!group_var) %>%
    summarise(total_deaths_all = n(), .groups = "drop")
  
  # Total births and prenatal visits
  births_summary <- data_births %>%
    group_by(ANO, !!group_var) %>%
    summarise(
      total_births = n(),
      avg_consultas = mean(NUMCONSUL, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Deaths from selected illnesses
  deaths_summary <- data_deaths_selected %>%
    group_by(ANO, !!group_var, C_DIR1, term) %>%
    summarise(total_deaths = n(), .groups = "drop")
  
  # Merge and calculate correct mortality rate
  merged_data <- deaths_summary %>%
    left_join(births_summary, by = c("ANO", as.character(group_var))) %>%
    left_join(total_deaths_all, by = c("ANO", as.character(group_var))) %>%
    mutate(mortality_rate = total_deaths / (total_deaths_all + total_births))
  
  return(merged_data)
}

#combined_data_deaths <- combined_data_deaths %>%
  #mutate(TIPO = coalesce(TIPO.x, TIPO.y)) %>%  # take TIPO.x if not NA, otherwise TIPO.y
  #select(-TIPO.x, -TIPO.y)  # remove the duplicate columns

# Apply new function
mortality_by_seg_social <- prepare_mortality_data(combined_data, combined_data_selected_illnesses, combined_data_deaths, SEG_SOCIAL)
mortality_by_tipo <- prepare_mortality_data(combined_data, combined_data_selected_illnesses, combined_data_deaths, TIPO)
# Function to create mortality trend graphs
plot_mortality_trend <- function(data, group_var) {
  group_var_sym <- ensym(group_var)  # Convert variable name to symbol
  seg_social_labels <- c("1" = "Contributory", "2" = "Subsidized")
  
  # Define custom colors and labels for TIPO
  tipo_labels <- c("1" = "Urban", "2" = "Intermediate", "3" = "Rural", "4" = "Scattered Rural")
  tipo_colors <- c("1" = "blue", "2" = "green", "3" = "red", "4" = "purple")
  custom_title <- if (as_label(group_var_sym) == "SEG_SOCIAL") {
    "Mortality by Health Insurance Regime"
  } else if (as_label(group_var_sym) == "TIPO") {
    "Mortality by Rurality Type"
  } else {
    paste("Incidence Trend by", as_label(group_var_sym))
  }
  
  p <- ggplot(data, aes(x = as.numeric(ANO), y = mortality_rate,                         
                        color = factor(!!group_var_sym), group = !!group_var_sym)) +  
    geom_line(size = 1) +   
    geom_point(size = 2) +   
    facet_wrap(~ term, scales = "free_y") +  # Facet by illness (C_DIR1)   
    labs(     
      title = custom_title,  # Use the custom title    
      x = "Year",     
      y = "Mortality Rate",     
      color = as_label(group_var_sym)   
    ) +   
    theme_minimal() +   
    scale_x_continuous(breaks = unique(as.numeric(data$ANO)))    
  
  # Apply custom label adjustments only when necessary   
  if (as_label(group_var_sym) == "SEG_SOCIAL") {     
    p <- p + scale_color_manual(values = c("red", "blue"), labels = seg_social_labels)   
  } else if (as_label(group_var_sym) == "TIPO") {     
    p <- p + scale_color_manual(values = tipo_colors, labels = tipo_labels)   
  }    
  
  return(p) 
}

# Generate graphs for different groupings
mortality_by_seg_social_insured <- mortality_by_seg_social %>% 
  filter(SEG_SOCIAL %in% c(1,2))
plot_mortality_trend(mortality_by_seg_social_insured, SEG_SOCIAL)
plot_mortality_trend(mortality_by_tipo, TIPO)

#Regressions regarding 

prepare_mortality_data_munic_year <- function(data_births, data_deaths_selected, data_deaths_all, group_var) {
  group_var <- rlang::ensym(group_var)
  
  # Births summary: total births & avg prenatal consultations
  births_summary <- data_births %>%
    group_by(COD_UNIQ, ANO, !!group_var) %>%
    summarise(
      total_births = n(),
      avg_consultas = mean(as.numeric(NUMCONSUL), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(!!group_var := as.character(!!group_var))
  
  # Deaths from selected illnesses
  deaths_selected_summary <- data_deaths_selected %>%
    group_by(COD_UNIQ, ANO, !!group_var, C_DIR1, term) %>%
    summarise(total_deaths = n(), .groups = "drop") %>%
    mutate(!!group_var := as.character(!!group_var))
  
  # Total deaths (all causes)
  deaths_all_summary <- data_deaths_all %>%
    group_by(COD_UNIQ, ANO, !!group_var) %>%
    summarise(total_deaths_all = n(), .groups = "drop") %>%
    mutate(!!group_var := as.character(!!group_var))
  
  # Merge everything
  merged_data <- deaths_selected_summary %>%
    left_join(births_summary, by = c("COD_UNIQ", "ANO", as.character(group_var))) %>%
    left_join(deaths_all_summary, by = c("COD_UNIQ", "ANO", as.character(group_var))) %>%
    mutate(
      mortality_rate = total_deaths / (total_births + total_deaths_all)
    )
  
  return(merged_data)
}


mortality_munic_year_by_seg_social <- prepare_mortality_data_munic_year(
  combined_data, combined_data_selected_illnesses, combined_data_deaths, SEG_SOCIAL)

mortality_munic_year_by_area_res <- prepare_mortality_data_munic_year(
  combined_data, combined_data_selected_illnesses, combined_data_deaths, AREA_RES)

mortality_munic_year_by_tipo <- prepare_mortality_data_munic_year(
  combined_data, combined_data_selected_illnesses, combined_data_deaths, TIPO)
# Filter datasets to include only Hypoxia cases (P200)
hypoxia_munic_year_by_seg_social <- mortality_munic_year_by_seg_social %>%
  filter(C_DIR1 == "P200") %>% 
  filter(SEG_SOCIAL %in% c("1", "2"))

hypoxia_munic_year_by_area_res <- mortality_munic_year_by_area_res %>%
  filter(C_DIR1 == "P200")

hypoxia_munic_year_by_tipo <- mortality_munic_year_by_tipo %>%
  filter(C_DIR1 == "P200")

run_feols_hypoxia <- function(data, group_var) {
  group_var_sym <- ensym(group_var)  # Convert variable name to symbol
  group_var_str <- as.character(group_var_sym)  # Convert symbol to string for formula
  
  # Construct formula dynamically
  formula <- as.formula(paste("mortality_rate ~ avg_consultas + i(", group_var_str, ") | COD_UNIQ + ANO"))
  
  # Run fixed effects regression
  fe_model <- feols(formula, data = data)
  
  cat("\nFixed Effects Regression Results for Hypoxia (P200) - Grouped by:", as_label(group_var_sym), "\n")
  print(summary(fe_model))
  
  return(fe_model)  # Return the model object
}


# Run fixed-effects regressions for Hypoxia
hypoxia_seg_model <- run_feols_hypoxia(hypoxia_munic_year_by_seg_social, "SEG_SOCIAL")
hypoxia_area_model <- run_feols_hypoxia(hypoxia_munic_year_by_area_res, "AREA_RES")
hypoxia_tipo_model <- run_feols_hypoxia(hypoxia_munic_year_by_tipo, "TIPO")

setFixest_dict(c("avg_consultas" = "Avg. Prenatal Visits",
                 "SEG_SOCIAL::2" = "Subsidized Health Insurance"))

etable(hypoxia_seg_model, tex = TRUE, file = "Did_Hypoxia_Model.tex")

plot_scatter_mortality <- function(data, group_var) {
  group_var_sym <- ensym(group_var)  # Convert to symbol
  group_var_name <- as_label(group_var_sym)
  
  data <- data %>% 
    filter(total_births >= 10)
  # Define custom labels for SEG_SOCIAL
  seg_social_labels <- c("1" = "Contributory", "2" = "Subsidized")
  
  # Define custom colors and labels for TIPO
  tipo_labels <- c("1" = "Urban", "2" = "Intermediate", "3" = "Rural", "4" = "Scattered Rural")
  tipo_colors <- c("1" = "blue", "2" = "green", "3" = "red", "4" = "purple")
  
  # Determine title based on group_var
  title_text <- if (group_var_name == "SEG_SOCIAL") {
    "Insurance Scheme"
  } else if (group_var_name %in% c("TIPO", "AREA_RES")) {
    "Rurality Type"
  } else {
    group_var_name  # Fallback (if you ever add another variable)
  }
  
  # Base scatter plot with best-fit regression lines
  p <- ggplot(data, aes(x = avg_consultas, y = mortality_rate, color = as.factor(!!group_var_sym))) +
    geom_point(size = 3, alpha = 0.7) +  # Scatter points
    geom_smooth(method = "lm", se = FALSE, aes(group = as.factor(!!group_var_sym)), linewidth = 1) +  # Best-fit line per group
    labs(
      title = paste("Municipality-Level Mortality associated to Hypoxia vs. Prenatal Visits by", title_text),
      x = "Average Prenatal Visits per Municipality",
      y = "Mortality Rate",
      color = title_text
    ) +
    theme_minimal() +
    scale_y_continuous(limits = c(0, 0.2))  # Adjust y-axis limits
  
  # Apply custom color and label adjustments
  if (group_var_name == "SEG_SOCIAL") {
    p <- p + scale_color_manual(values = c("red", "blue"), labels = seg_social_labels)
  } else if (group_var_name == "TIPO") {
    p <- p + scale_color_manual(values = tipo_colors, labels = tipo_labels)
  } else if (group_var_name == "AREA_RES") {
    p <- p + scale_color_manual(values = tipo_colors[1:3], labels = tipo_labels[1:3])  # Assuming AREA_RES only uses 1, 2, 3
  }
  
  return(p)
}
plot_scatter_mortality(hypoxia_munic_year_by_seg_social, SEG_SOCIAL)
plot_scatter_mortality(hypoxia_munic_year_by_area_res, AREA_RES)
plot_scatter_mortality(hypoxia_munic_year_by_tipo, TIPO)



#CODE FOR Panel model
# Step 1: Select codes and define illness category
selected_codes <- top_20_d_values[c(4, 5, 8, 10, 17)]

combined_data_selected_illnesses <- combined_data_deaths %>%
  filter(C_DIR1 %in% selected_codes) %>%
  left_join(crosswalk_icd, by = "C_DIR1") %>%
  mutate(
    C_DIR1 = if_else(C_DIR1 == "P209", "P200", C_DIR1),
    term = if_else(C_DIR1 == "P200", "Intrauterine hypoxia", term),
    Rural = case_when(
      AREA_RES %in% c(2, 3) ~ 1,
      AREA_RES == 1 ~ 0
    )
  ) %>%
  filter(!is.na(Rural))

# Step 2: Clean and filter births
combined_data_insured <- combined_data_insured %>%
  mutate(NUMCONSUL = as.numeric(NUMCONSUL)) %>%
  filter(NUMCONSUL <= 50)

# Step 3: Updated mortality prep function (corrected denominator)
prepare_mortality_data_munic_year <- function(data_births, data_deaths_selected, data_deaths_all) {
  # Total births and covariates
  births_summary <- data_births %>%
    group_by(COD_UNIQ, ANO) %>%
    summarise(
      total_births = n(),
      avg_consultas = mean(as.numeric(NUMCONSUL), na.rm = TRUE),
      Rural = first(Rural),
      SEG_SOCIAL = first(SEG_SOCIAL),
      .groups = "drop"
    )
  
  # Total deaths from selected causes
  deaths_selected_summary <- data_deaths_selected %>%
    group_by(COD_UNIQ, ANO, C_DIR1, term, Rural) %>%
    summarise(total_deaths = n(), .groups = "drop")
  
  # Total deaths from all causes
  deaths_all_summary <- data_deaths_all %>%
    group_by(COD_UNIQ, ANO) %>%
    summarise(total_deaths_all = n(), .groups = "drop")
  
  # Merge everything
  merged_data <- deaths_selected_summary %>%
    left_join(births_summary, by = c("COD_UNIQ", "ANO", "Rural")) %>%
    left_join(deaths_all_summary, by = c("COD_UNIQ", "ANO")) %>%
    mutate(
      mortality_rate = total_deaths / (total_births + total_deaths_all),
      SEG_SOCIAL = as.character(SEG_SOCIAL)
    ) %>%
    filter(!is.na(mortality_rate), !is.infinite(mortality_rate))
  
  return(merged_data)
}

combined_data_insured <- combined_data_insured %>%
  mutate(NUMCONSUL = as.numeric(NUMCONSUL)) %>%
  filter(NUMCONSUL <= 50)

# Step 3: Updated mortality prep function (corrected denominator)
prepare_mortality_data_munic_year <- function(data_births, data_deaths_selected, data_deaths_all) {
  # Total births and covariates
  births_summary <- data_births %>%
    group_by(COD_UNIQ, ANO) %>%
    summarise(
      total_births = n(),
      avg_consultas = mean(as.numeric(NUMCONSUL), na.rm = TRUE),
      Rural = first(Rural),
      SEG_SOCIAL = first(SEG_SOCIAL),
      .groups = "drop"
    )
  
  # Total deaths from selected causes
  deaths_selected_summary <- data_deaths_selected %>%
    group_by(COD_UNIQ, ANO, C_DIR1, term, Rural) %>%
    summarise(total_deaths = n(), .groups = "drop")
  
  # Total deaths from all causes
  deaths_all_summary <- data_deaths_all %>%
    group_by(COD_UNIQ, ANO) %>%
    summarise(total_deaths_all = n(), .groups = "drop")
  
  # Merge everything
  merged_data <- deaths_selected_summary %>%
    left_join(births_summary, by = c("COD_UNIQ", "ANO", "Rural")) %>%
    left_join(deaths_all_summary, by = c("COD_UNIQ", "ANO")) %>%
    mutate(
      mortality_rate = total_deaths / (total_births + total_deaths_all),
      SEG_SOCIAL = as.character(SEG_SOCIAL)
    ) %>%
    filter(!is.na(mortality_rate), !is.infinite(mortality_rate))
  
  return(merged_data)
}
mortality_datasets <- list()
for (icd_code in selected_codes) {
  if (icd_code != "P209") {
    mortality_datasets[[icd_code]] <- prepare_mortality_data_munic_year(
      combined_data_insured,
      combined_data_selected_illnesses %>% filter(C_DIR1 == icd_code),
      combined_data_deaths
    )
  }
}


# Function to run fixed-effects regressions for mortality rates
run_feols_mortality <- function(data, icd_code) {
  formula <- as.formula("mortality_rate ~ avg_consultas * SEG_SOCIAL * Rural | COD_UNIQ + ANO")
  fe_model <- feols(formula, data = data, cluster = ~COD_UNIQ)
  
  cat("\nFixed Effects Regression Results for ICD-10:", icd_code, "\n")
  print(summary(fe_model))
  
  return(fe_model)
}

# Run regression for each ICD-10 illness
mortality_models <- list()
for (icd_code in selected_codes) {
  if (icd_code != "P209") {  # Exclude P209 since it was merged into P200
    mortality_models[[icd_code]] <- run_feols_mortality(mortality_datasets[[icd_code]], icd_code)
  }
}

# Create a LaTeX regression table summarizing all illnesses
etable(mortality_models, tex = TRUE, file = "/Users/pablotorresr/Desktop/Classes_Columbia/7th Semester/Senior Thesis/Mortality_Analysis_by_Illness.tex")

#Map for HHI by Department

colombia_map <- ne_states(country = "Colombia", returnclass = "sf")

# Ensure a (crosswalk) has COD_DPTO as a character with leading zeros
a <- a %>%
  mutate(COD_DPTO = as.character(sprintf("%02d", as.numeric(COD_DPTO))))  # Force character format

# Step 1: Extract COD_DPTO from COD_UNIQ (first two characters) as character with leading zeros
afiliados_processed <- afiliados_processed %>%
  mutate(
    COD_DPTO = as.character(str_sub(COD_UNIQ, 1, 2)),  # Extract first two characters as character
    year = as.character(ano)  # Convert `ano` to character
  )

# Step 2: Compute total affiliates per year per COD_DPTO
affiliates_by_year_dpto <- afiliados_processed %>%
  group_by(COD_DPTO, year, SEG_SOCIAL) %>%
  summarise(total_afiliados_year = sum(Afiliados, na.rm = TRUE), .groups = "drop")  # Total per year

# Step 3: Compute the average total affiliates per department over years
avg_affiliates_by_dpto <- affiliates_by_year_dpto %>%
  group_by(COD_DPTO, SEG_SOCIAL) %>%
  summarise(avg_afiliados = mean(total_afiliados_year, na.rm = TRUE), .groups = "drop")  # Average over years

# Step 4: Compute total average affiliates per department across both SEG_SOCIAL categories
total_avg_affiliates_per_dpto <- avg_affiliates_by_dpto %>%
  group_by(COD_DPTO) %>%
  summarise(total_avg_afiliados_dpto = sum(avg_afiliados, na.rm = TRUE), .groups = "drop")

# Step 5: Compute the yearly average HHI per department (Using `month_year` from `hhi_data`)
hhi_avg_per_year <- hhi_data %>%
  mutate(year = as.character(str_sub(month_year, 1, 4))) %>%  # Extract year from month_year
  group_by(COD_DPTO, year, SEG_SOCIAL) %>%
  summarise(avg_HHI = mean(HHI, na.rm = TRUE), .groups = "drop")

# Step 6: Merge with department names from a
hhi_avg_per_year <- hhi_avg_per_year %>%
  left_join(a, by = "COD_DPTO") %>%
  mutate(woe_name = tolower(trimws(woe_name)))  # Standardize department names

# Standardize department names in colombia_map for merging
colombia_map <- colombia_map %>%
  mutate(woe_name = tolower(trimws(woe_name)))

# Step 7: Merge affiliate data into HHI data
hhi_with_affiliates <- hhi_avg_per_year %>%
  left_join(total_avg_affiliates_per_dpto, by = "COD_DPTO")

# Step 8: Generate separate maps for SUBSIDIZED and CONTRIBUTORY
for (seg in c(0, 1)) {
  hhi_filtered <- hhi_with_affiliates %>%
    filter(SEG_SOCIAL == seg)
  
  # Merge with map data
  colombia_map_with_hhi <- colombia_map %>%
    left_join(hhi_filtered, by = "woe_name")
  
  # Define title manually
  title_label <- ifelse(seg == 0, "SUBSIDIZED", "CONTRIBUTORY")
  
  # Create the plot and store it in a variable
  p <- ggplot(colombia_map_with_hhi) +
    geom_sf(aes(fill = avg_HHI), color = "white") +  # Keep the color map
    scale_fill_gradient(low = "yellow", high = "red", na.value = "grey80", name = "Avg. HHI") +  # **Keep color legend**
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),  # Remove major grid lines
      panel.grid.minor = element_blank(),  # Remove minor grid lines
      axis.title.x = element_blank(),  # Remove X-axis title
      axis.text.x = element_blank(),  # Remove X-axis text
      axis.ticks.x = element_blank(),  # Remove X-axis ticks
      axis.title.y = element_blank(),  # Remove Y-axis title
      axis.text.y = element_blank(),  # Remove Y-axis text
      axis.ticks.y = element_blank(),  # Remove Y-axis ticks
      legend.position = "right"  # **Keep only the color legend on the side**
    ) +
    labs(
      title = paste("Average HHI per Department (Yearly) -", title_label),
      subtitle = "Annual average based on monthly HHI values"
    )
  
  # Save the plot as a PNG file
  ggsave(filename = paste0("HHI_Map_", title_label, ".png"), plot = p, width = 8, height = 6)
}

# Law 1438 2011
# Define normal health outcomes and Post-treatment indicator
combined_data_insured <- combined_data_insured %>%
  filter(PESO_NAC != 9) %>% 
  filter(TALLA_NAC != 9) %>% 
  filter(T_GES != 9) %>% 
  mutate(
    normal_weight_rural = ifelse(PESO_NAC %in% c(5, 6, 7), 1, 0),  # Normal birth weight
    normal_size_rural = ifelse(TALLA_NAC %in% c(4, 5), 1, 0),  # Normal birth size
    normal_gestation_rural = ifelse(T_GES == 4, 1, 0),  # Normal gestation time
    Post = if_else(ANO >= 2011, 1, 0)  # Post-policy dummy (2011 law)
  )
# First stage: Effect of the policy on pre-natal visits
first_stage_weight_rural <- feols(NUMCONSUL ~ Rural * Post * SEG_SOCIAL | COD_DPTO + ANO,
                                  data = combined_data_insured, cluster = ~COD_DPTO)

first_stage_size_rural <- feols(NUMCONSUL ~ Rural * Post * SEG_SOCIAL | COD_DPTO + ANO,
                                data = combined_data_insured, cluster = ~COD_DPTO)

first_stage_gestation_rural <- feols(NUMCONSUL ~ Rural * Post * SEG_SOCIAL | COD_DPTO + ANO,
                                     data = combined_data_insured, cluster = ~COD_DPTO)
# Store fitted values from the first stage for use in second stage regression
combined_data_insured <- combined_data_insured %>%
  mutate(fitted_numnconsul_rural = predict(first_stage_weight_rural, type = "response"))
# Second stage: Effect of predicted prenatal visits on birth outcomes
second_stage_weight_rural <- feglm(normal_weight_rural ~ fitted_numnconsul_rural * SEG_SOCIAL | COD_DPTO + ANO,
                                   data = combined_data_insured, family = binomial("logit"),
                                   cluster = ~COD_DPTO)

second_stage_size_rural <- feglm(normal_size_rural ~ fitted_numnconsul_rural * SEG_SOCIAL | COD_DPTO + ANO,
                                 data = combined_data_insured, family = binomial("logit"),
                                 cluster = ~COD_DPTO)

second_stage_gestation_rural <- feglm(normal_gestation_rural ~ fitted_numnconsul_rural * SEG_SOCIAL | COD_DPTO + ANO ,
                                      data = combined_data_insured, family = binomial("logit"),
                                      cluster = ~COD_DPTO)
# Reduced form: Effect of the policy directly on birth outcomes
reduced_form_weight_rural <- feglm(normal_weight_rural ~ Rural * Post * SEG_SOCIAL | COD_DPTO + ANO,
                                   data = combined_data_insured, family = binomial("logit"),
                                   cluster = ~COD_DPTO)

reduced_form_size_rural <- feglm(normal_size_rural ~ Rural * Post * SEG_SOCIAL | COD_DPTO + ANO,
                                 data = combined_data_insured, family = binomial("logit"),
                                 cluster = ~COD_DPTO)

reduced_form_gestation_rural <- feglm(normal_gestation_rural ~ Rural * Post * SEG_SOCIAL | COD_DPTO + ANO,
                                      data = combined_data_insured, family = binomial("logit"),
                                      cluster = ~COD_DPTO)
# Save results as LaTeX tables
etable(first_stage_weight_rural,
       tex = TRUE, file = "/Users/pablotorresr/Desktop/Classes_Columbia/7th Semester/Senior Thesis/First_Stage_Law2011.tex")

etable(second_stage_weight_rural, second_stage_size_rural, second_stage_gestation_rural,
       tex = TRUE, file = "/Users/pablotorresr/Desktop/Classes_Columbia/7th Semester/Senior Thesis/Second_Stage_Law2011.tex")

etable(reduced_form_weight_rural, reduced_form_size_rural, reduced_form_gestation_rural,
       tex = TRUE, file = "/Users/pablotorresr/Desktop/Classes_Columbia/7th Semester/Senior Thesis/Reduced_Form_Law2011.tex")



#Regressions for Health Outcomes -- Health Benefits Plan

combined_hhi_data_adults <- combined_hhi_data_adults %>%
  filter(PESO_NAC != 9) %>% 
  filter(TALLA_NAC != 9) %>% 
  filter(T_GES != 9) %>% 
  mutate(
    normal_weight = ifelse(PESO_NAC %in% c(5, 6, 7), 1, 0),  # Normal birth weight
    normal_size = ifelse(TALLA_NAC %in% c(4, 5), 1, 0),  # Normal birth size
    normal_gestation = ifelse(T_GES == 4, 1, 0),  # Normal gestation time
    Post = if_else((ANO > 2012) | (ANO == 2012 & as.numeric(MES) >= 7), 1, 0)  # Post-policy dummy
  )
# First stage for each outcome
first_stage_weight <- feols(NUMCONSUL ~ Rural * Post * SEG_SOCIAL | COD_DPTO + ANO,
                            data = combined_hhi_data_adults, cluster = ~COD_DPTO)

first_stage_size <- feols(NUMCONSUL ~ Rural * Post * SEG_SOCIAL | COD_DPTO + ANO,
                          data = combined_hhi_data_adults, cluster = ~COD_DPTO)

first_stage_gestation <- feols(NUMCONSUL ~ Rural * Post * SEG_SOCIAL | COD_DPTO + ANO,
                               data = combined_hhi_data_adults, cluster = ~COD_DPTO)

# Store fitted values for second stage
combined_hhi_data_adults <- combined_hhi_data_adults %>%
  mutate(fitted_numnconsul = predict(first_stage_weight, type = "response"))
second_stage_weight <- feglm(normal_weight ~ fitted_numnconsul * SEG_SOCIAL | COD_DPTO + ANO,
                             data = combined_hhi_data_adults, family = binomial("logit"),
                             cluster = ~COD_DPTO)

second_stage_size <- feglm(normal_size ~ fitted_numnconsul * SEG_SOCIAL | COD_DPTO + ANO,
                           data = combined_hhi_data_adults, family = binomial("logit"),
                           cluster = ~COD_DPTO)

second_stage_gestation <- feglm(normal_gestation ~ fitted_numnconsul * SEG_SOCIAL | COD_DPTO + ANO ,
                                data = combined_hhi_data_adults, family = binomial("logit"),
                                cluster = ~COD_DPTO)
reduced_form_weight <- feglm(normal_weight ~ Rural * Post * SEG_SOCIAL | COD_DPTO + ANO,
                             data = combined_hhi_data_adults, family = binomial("logit"),
                             cluster = ~COD_DPTO)

reduced_form_size <- feglm(normal_size ~ Rural * Post * SEG_SOCIAL | COD_DPTO + ANO,
                           data = combined_hhi_data_adults, family = binomial("logit"),
                           cluster = ~COD_DPTO)

reduced_form_gestation <- feglm(normal_gestation ~ Rural * Post * SEG_SOCIAL | COD_DPTO + ANO,
                                data = combined_hhi_data_adults, family = binomial("logit"),
                                cluster = ~COD_DPTO)
etable(first_stage_weight,
       tex = TRUE, file = "/Users/pablotorresr/Desktop/Classes_Columbia/7th Semester/Senior Thesis/First_Stage_All.tex")

etable(second_stage_weight, second_stage_size, second_stage_gestation,
       tex = TRUE, file = "/Users/pablotorresr/Desktop/Classes_Columbia/7th Semester/Senior Thesis/Second_Stage_All.tex")

etable(reduced_form_weight, reduced_form_size, reduced_form_gestation,
       tex = TRUE, file = "/Users/pablotorresr/Desktop/Classes_Columbia/7th Semester/Senior Thesis/Reduced_Form_All.tex")

# Create unique department-municipality mapping
unique_dept_muni <- combined_hhi_data_adults %>%
  select(COD_DPTO, COD_UNIQ) %>%
  distinct()

# Create a sequence of pre-natal visits
num_visits <- seq(0, 10, 1)

# Expand grid over all years, municipalities, and insurance types
newdata <- expand.grid(
  fitted_numnconsul = num_visits,
  SEG_SOCIAL = factor(c(0, 1)),  # Insurance type
  ANO = unique(combined_hhi_data_adults$ANO),  # All years
  COD_UNIQ = unique(combined_hhi_data_adults$COD_UNIQ)  # All municipalities
)

# Merge to include department info
newdata <- newdata %>%
  left_join(unique_dept_muni, by = "COD_UNIQ") %>%
  mutate(
    EDAD_MADRE = mean(combined_hhi_data_adults$EDAD_MADRE, na.rm = TRUE),
    NIV_EDUM = mean(combined_hhi_data_adults$NIV_EDUM, na.rm = TRUE)
  )

# Predict probabilities for all outcomes
newdata$predicted_prob_weight <- predict(second_stage_weight, newdata, type = "response")
newdata$predicted_prob_size <- predict(second_stage_size, newdata, type = "response")
newdata$predicted_prob_gestation <- predict(second_stage_gestation, newdata, type = "response")

# Aggregate: Take the mean predicted probability per `fitted_numnconsul` & `SEG_SOCIAL`
average_predictions_weight <- newdata %>%
  group_by(fitted_numnconsul, SEG_SOCIAL) %>%
  summarise(mean_predicted_prob = mean(predicted_prob_weight, na.rm = TRUE), .groups = "drop")

average_predictions_size <- newdata %>%
  group_by(fitted_numnconsul, SEG_SOCIAL) %>%
  summarise(mean_predicted_prob = mean(predicted_prob_size, na.rm = TRUE), .groups = "drop")

average_predictions_gestation <- newdata %>%
  group_by(fitted_numnconsul, SEG_SOCIAL) %>%
  summarise(mean_predicted_prob = mean(predicted_prob_gestation, na.rm = TRUE), .groups = "drop")

# Rename SEG_SOCIAL labels for plots
average_predictions_weight$SEG_SOCIAL <- factor(average_predictions_weight$SEG_SOCIAL, labels = c("Subsidized", "Contributory"))
average_predictions_size$SEG_SOCIAL <- factor(average_predictions_size$SEG_SOCIAL, labels = c("Subsidized", "Contributory"))
average_predictions_gestation$SEG_SOCIAL <- factor(average_predictions_gestation$SEG_SOCIAL, labels = c("Subsidized", "Contributory"))

# Function to plot the results
plot_predicted_prob <- function(data, title) {
  ggplot(data, aes(x = fitted_numnconsul, y = mean_predicted_prob, color = SEG_SOCIAL)) +
    geom_line(size = 1.2) +
    scale_color_manual(values = c("blue", "red")) +  # Subsidized = Blue, Contributory = Red
    labs(
      title = title,
      x = "Number of Pre-Natal Visits",
      y = "Average Predicted Probability",
      color = "Insurance Type"
    ) +
    theme_minimal()
}

# Plot all three graphs
plot1 <- plot_predicted_prob(average_predictions_weight, "Predicted Probability of Normal Birth Weight (Adult Mothers)")
plot2 <- plot_predicted_prob(average_predictions_size, "Predicted Probability of Normal Birth Size (Adult Mothers)")
plot3 <- plot_predicted_prob(average_predictions_gestation, "Predicted Probability of Normal Gestation Time (Adult Mothers)")

# Display plots
print(plot1)
print(plot2)
print(plot3)

combined_hhi_data_minors <- combined_hhi_data_minors %>%
  filter(PESO_NAC != 9) %>% 
  filter(TALLA_NAC != 9) %>% 
  filter(T_GES != 9) %>% 
  mutate(
    normal_weight_minor = ifelse(PESO_NAC %in% c(5, 6, 7), 1, 0),  # Normal = 1
    normal_size_minor = ifelse(TALLA_NAC %in% c(4, 5), 1, 0),  # Normal Size = 1
    normal_gestation_minor = ifelse(T_GES == 4, 1, 0),  # Normal Gestation = 1
    Post_minor = if_else((ANO > 2010) | (ANO == 2010 & as.numeric(MES) >= 1), 1, 0)  # Post-policy dummy for minors
  )

# FIRST STAGE REGRESSION
first_stage_weight_minor <- feols(NUMCONSUL ~ Rural * Post_minor * SEG_SOCIAL | COD_DPTO + ANO, 
                                  data = combined_hhi_data_minors, cluster = ~COD_DPTO)

first_stage_size_minor <- feols(NUMCONSUL ~ Rural * Post_minor * SEG_SOCIAL | COD_DPTO + ANO, 
                                data = combined_hhi_data_minors, cluster = ~COD_DPTO)

first_stage_gestation_minor <- feols(NUMCONSUL ~ Rural * Post_minor * SEG_SOCIAL | COD_DPTO + ANO, 
                                     data = combined_hhi_data_minors, cluster = ~COD_DPTO)

# Store fitted values for second stage
combined_hhi_data_minors <- combined_hhi_data_minors %>%
  mutate(fitted_numnconsul_minor = predict(first_stage_weight_minor, type = "response"))

# SECOND STAGE REGRESSIONS
second_stage_weight_minor <- feglm(normal_weight_minor ~ fitted_numnconsul_minor * SEG_SOCIAL | COD_DPTO + ANO,
                                   data = combined_hhi_data_minors, family = binomial("logit"), cluster = ~COD_DPTO)

second_stage_size_minor <- feglm(normal_size_minor ~ fitted_numnconsul_minor * SEG_SOCIAL | COD_DPTO + ANO,
                                 data = combined_hhi_data_minors, family = binomial("logit"), cluster = ~COD_DPTO)

second_stage_gestation_minor <- feglm(normal_gestation_minor ~ fitted_numnconsul_minor * SEG_SOCIAL | COD_DPTO + ANO,
                                      data = combined_hhi_data_minors, family = binomial("logit"), cluster = ~COD_DPTO)

# REDUCED FORM REGRESSION
reduced_form_weight_minor <- feglm(normal_weight_minor ~ Rural * Post_minor * SEG_SOCIAL | COD_DPTO + ANO,
                                   data = combined_hhi_data_minors, family = binomial("logit"), cluster = ~COD_DPTO)

reduced_form_size_minor <- feglm(normal_size_minor ~ Rural * Post_minor * SEG_SOCIAL | COD_DPTO + ANO,
                                 data = combined_hhi_data_minors, family = binomial("logit"), cluster = ~COD_DPTO)

reduced_form_gestation_minor <- feglm(normal_gestation_minor ~ Rural * Post_minor * SEG_SOCIAL | COD_DPTO + ANO,
                                      data = combined_hhi_data_minors, family = binomial("logit"), cluster = ~COD_DPTO)

# EXPORT REGRESSION TABLES
etable(first_stage_weight_minor,
       tex = TRUE, file = "/Users/pablotorresr/Desktop/Classes_Columbia/7th Semester/Senior Thesis/First_Stage_All_Minors.tex")

etable(second_stage_weight_minor, second_stage_size_minor, second_stage_gestation_minor, 
       tex = TRUE, file = "/Users/pablotorresr/Desktop/Classes_Columbia/7th Semester/Senior Thesis/Second_Stage_All_Minors.tex")

etable(reduced_form_weight_minor, reduced_form_size_minor, reduced_form_gestation_minor, 
       tex = TRUE, file = "/Users/pablotorresr/Desktop/Classes_Columbia/7th Semester/Senior Thesis/Reduced_Form_All_Minors.tex")

# CREATE PREDICTION DATASE
# Create unique department-municipality mapping
unique_dept_muni_minor <- combined_hhi_data_minors %>%
  select(COD_DPTO, COD_UNIQ) %>%
  distinct()

# Create a sequence of pre-natal visits
num_visits <- seq(0, 10, 1)

# Expand grid over all years, municipalities, and insurance types
newdata_minor <- expand.grid(
  fitted_numnconsul_minor = num_visits,
  SEG_SOCIAL = factor(c(0, 1)),  # Insurance type
  ANO = unique(combined_hhi_data_minors$ANO),  # All years
  COD_UNIQ = unique(combined_hhi_data_minors$COD_UNIQ)  # All municipalities
)

# Merge to include department info
newdata_minor <- newdata_minor %>%
  left_join(unique_dept_muni_minor, by = "COD_UNIQ") %>%
  mutate(
    EDAD_MADRE = mean(combined_hhi_data_minors$EDAD_MADRE, na.rm = TRUE),
    NIV_EDUM = mean(combined_hhi_data_minors$NIV_EDUM, na.rm = TRUE)
  )

# Predict probabilities for all outcomes
newdata_minor$predicted_prob_weight_minor <- predict(second_stage_weight_minor, newdata_minor, type = "response")
newdata_minor$predicted_prob_size_minor <- predict(second_stage_size_minor, newdata_minor, type = "response")
newdata_minor$predicted_prob_gestation_minor <- predict(second_stage_gestation_minor, newdata_minor, type = "response")

# Aggregate: Take the mean predicted probability per `fitted_numnconsul_minor` & `SEG_SOCIAL`
average_predictions_weight_minor <- newdata_minor %>%
  group_by(fitted_numnconsul_minor, SEG_SOCIAL) %>%
  summarise(mean_predicted_prob = mean(predicted_prob_weight_minor, na.rm = TRUE), .groups = "drop")

average_predictions_size_minor <- newdata_minor %>%
  group_by(fitted_numnconsul_minor, SEG_SOCIAL) %>%
  summarise(mean_predicted_prob = mean(predicted_prob_size_minor, na.rm = TRUE), .groups = "drop")

average_predictions_gestation_minor <- newdata_minor %>%
  group_by(fitted_numnconsul_minor, SEG_SOCIAL) %>%
  summarise(mean_predicted_prob = mean(predicted_prob_gestation_minor, na.rm = TRUE), .groups = "drop")

# Rename SEG_SOCIAL labels for plots
average_predictions_weight_minor$SEG_SOCIAL <- factor(average_predictions_weight_minor$SEG_SOCIAL, labels = c("Subsidized", "Contributory"))
average_predictions_size_minor$SEG_SOCIAL <- factor(average_predictions_size_minor$SEG_SOCIAL, labels = c("Subsidized", "Contributory"))
average_predictions_gestation_minor$SEG_SOCIAL <- factor(average_predictions_gestation_minor$SEG_SOCIAL, labels = c("Subsidized", "Contributory"))

# FUNCTION TO PLOT RESULTS
plot_predicted_prob_minor <- function(data, title) {
  ggplot(data, aes(x = fitted_numnconsul_minor, y = mean_predicted_prob, color = SEG_SOCIAL)) +
    geom_line(size = 1.2) +
    scale_color_manual(values = c("blue", "red")) +  # Subsidized = Blue, Contributory = Red
    labs(
      title = title,
      x = "Number of Pre-Natal Visits",
      y = "Average Predicted Probability",
      color = "Insurance Type"
    ) +
    theme_minimal()
}

#  PLOT ALL THREE GRAPHS 
plot1_minor <- plot_predicted_prob_minor(average_predictions_weight_minor, "Predicted Probability of Normal Birth Weight (Teenage Mothers)")
plot2_minor <- plot_predicted_prob_minor(average_predictions_size_minor, "Predicted Probability of Normal Birth Size (Teenage Mothers)")
plot3_minor <- plot_predicted_prob_minor(average_predictions_gestation_minor, "Predicted Probability of Normal Gestation Time (Teenage Mothers)")

# Display plots
print(plot1_minor)
print(plot2_minor)
print(plot3_minor)

#REAL PRENATAL and Gestation Time
# Step 1: Prepare data
combined_data_insured <- combined_data_insured %>%
  filter(PESO_NAC != 9) %>%
  filter(TALLA_NAC != 9) %>%
  filter(T_GES != 9) %>%
  mutate(
    normal_weight = ifelse(PESO_NAC %in% c(5, 6, 7), "Normal", "Not Normal"),
    normal_length = ifelse(TALLA_NAC %in% c(4, 5), "Normal", "Not Normal"),
    normal_gestation = ifelse(T_GES %in% c(4), "Normal", "Not Normal"),
    insurance_type = case_when(
      SEG_SOCIAL == 1 ~ "Contributory",
      SEG_SOCIAL == 0 ~ "Subsidized",
      TRUE ~ NA_character_
    )
  )

# Step 2: Function to prepare summary
prepare_normal_share_by_visits <- function(data, outcome_var) {
  data %>%
    filter(NUMCONSUL %in% 0:20, !is.na(insurance_type)) %>%
    group_by(NUMCONSUL, insurance_type, .data[[outcome_var]]) %>%
    summarise(n = n(), .groups = "drop_last") %>%
    mutate(share = n / sum(n)) %>%
    filter(.data[[outcome_var]] == "Normal") %>%
    ungroup()
}

# Step 3: Prepare each dataset
weight_share_seg <- prepare_normal_share_by_visits(combined_data_insured, "normal_weight")
length_share_seg <- prepare_normal_share_by_visits(combined_data_insured, "normal_length")
gestation_share_seg <- prepare_normal_share_by_visits(combined_data_insured, "normal_gestation")

# Step 4: Create plots
seg_prenatal_weight <- ggplot(weight_share_seg, aes(x = NUMCONSUL, y = share, color = insurance_type)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c("Contributory" = "red", "Subsidized" = "blue")) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Normal Birth Weight by Prenatal Visits and Insurance Scheme",
    x = "Number of Prenatal Visits",
    y = "Share of Newborns with Normal Weight",
    color = "Insurance Scheme"
  ) +
  theme_minimal()

seg_prenatal_length <- ggplot(length_share_seg, aes(x = NUMCONSUL, y = share, color = insurance_type)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c("Contributory" = "red", "Subsidized" = "blue")) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Normal Birth Length by Prenatal Visits and Insurance Scheme",
    x = "Number of Prenatal Visits",
    y = "Share of Newborns with Normal Length",
    color = "Insurance Scheme"
  ) +
  theme_minimal()

seg_prenatal_ges <- ggplot(gestation_share_seg, aes(x = NUMCONSUL, y = share, color = insurance_type)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c("Contributory" = "red", "Subsidized" = "blue")) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Normal Gestation Time by Prenatal Visits and Insurance Scheme",
    x = "Number of Prenatal Visits",
    y = "Share of Newborns with Normal Gestation",
    color = "Insurance Scheme"
  ) +
  theme_minimal()

# Step 5: Save to file
ggsave(
  filename = "/Users/pablotorresr/Desktop/Classes_Columbia/7th Semester/Senior Thesis/seg_prenatal_weight.png",
  plot = seg_prenatal_weight,
  width = 7, height = 5, dpi = 300
)

ggsave(
  filename = "/Users/pablotorresr/Desktop/Classes_Columbia/7th Semester/Senior Thesis/seg_prenatal_length.png",
  plot = seg_prenatal_length,
  width = 7, height = 5, dpi = 300
)

ggsave(
  filename = "/Users/pablotorresr/Desktop/Classes_Columbia/7th Semester/Senior Thesis/seg_prenatal_ges.png",
  plot = seg_prenatal_ges,
  width = 7, height = 5, dpi = 300
)



