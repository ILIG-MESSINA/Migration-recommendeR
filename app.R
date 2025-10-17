# --------------------------------------------------------------
# Load required packages
# --------------------------------------------------------------
library(dplyr)           # Data manipulation
library(countrycode)     # Country code conversion
library(reactable)       # Interactive tables
library(shiny)           # Web app framework
library(htmltools)       # HTML helpers
library(ggplot2)         # Data visualization
library(stringr)         # String manipulation
library(readr)           # Read/write data
library(leaflet)         # Interactive maps
library(sparkline)       # Inline charts
library(purrr)           # Functional programming
library(DT)              # DataTables in Shiny
library(commonmark)      # Markdown parsing
library(shinyBS)         # Bootstrap components
library(shinyjs)         # JS integration in Shiny

# --------------------------------------------------------------
# Load datasets
# --------------------------------------------------------------

Literacy_Rate <- read_csv("www/DATA/Literacy Rate.csv", show_col_types = FALSE)
Macro1 <- read_csv("www/DATA/Macro1.csv", show_col_types = FALSE)
Capital <- read_csv("www/DATA/Capital.csv", show_col_types = FALSE)
growth_inflation <- read_csv("www/DATA/growth_inflation.csv", show_col_types = FALSE)

result <- readRDS("www/DATA/result.rds")
pays_continent <- readRDS("www/DATA/pays_continent.rds")
pyramide_plot<-readRDS("www/DATA/pyramide_plot.rds")

# --------------------------------------------------------------
# Flag colors by country (hex values) to fill age pyramid 
# --------------------------------------------------------------

flag_colors_hex <- list(
  AD = c("#0041FF", "#FFD700", "#000000"),
  AE = c("#00732F", "#FFFFFF", "#DA1212"),
  AF = c("#000000", "#CE1126", "#009640"),
  AG = c("#009B3A", "#FFFF00", "#0000FF"),
  AI = c("#006747", "#FFD100", "#F1A800"),
  AL = c("#E41C25", "#FFFFFF", "#000000"),
  AM = c("#D90012", "#0033A0", "#F2A800"),
  AO = c("#000000", "#CE1126", "#F9D616"),
  AR = c("#74ACDF", "#FFFFFF", "#F6B40E"),
  AS = c("#002B7F", "#CE1126", "#FFFFFF"),
  AT = c("#ED2939", "#FFFFFF", "#000000"),
  AU = c("#00008B", "#FF0000", "#FFFFFF"),
  AW = c("#00A9E0", "#F9A800", "#F4C300"),
  AZ = c("#00B9E4", "#3F9C35", "#ED2939"),
  BA = c("#002F6C", "#FFFFFF", "#EF3340"),
  BB = c("#00267F", "#FFFFFF", "#FFD100"),
  BD = c("#006A4E", "#F42A41", "#FFFFFF"),
  BE = c("#000000", "#FFD100", "#ED2939"),
  BF = c("#00966E", "#EF2B2D"),
  BG = c("#00966E", "#FFD100", "#D62612"),
  BH = c("#CE1126", "#FFFFFF", "#000000"),
  BI = c("#EF2B2D", "#FFD100", "#000000"),
  BJ = c("#FCD116", "#009E49", "#EF2B2D"),
  BL = c("#00267F", "#FFFFFF", "#FFDD00"),
  BM = c("#00247D", "#FFD100", "#EF3340"),
  BN = c("#00205B", "#FFD100", "#FFFFFF"),
  BO = c("#D52B1E", "#007934", "#FFD100"),
  BR = c("#009B3A", "#FEDF00", "#002776"),
  BS = c("#006AB3", "#FDB813", "#FFFFFF"),
  BT = c("#FF0000", "#FFDD00", "#000000"),
  BW = c("#006AB3", "#000000"),
  BY = c("#D52B1E", "#FFFFFF", "#00A651"),
  BZ = c("#006847", "#FFD100", "#EF2B2D"),
  CA = c("#FF0000", "#FFFFFF", "#000000"),
  CD = c("#007A5E", "#F3D03E", "#E30613"),
  CF = c("#E30B17", "#FCD116", "#000000"),
  CG = c("#009A44", "#FCD116", "#E30B17"),
  CH = c("#FF0000", "#FFFFFF", "#000000"),
  CI = c("#F77F00", "#FFFFFF", "#009E49"),
  CK = c("#00247D", "#FFFFFF", "#000000"),
  CL = c("#0033A0", "#FFFFFF", "#D52B1E"),
  CM = c("#007A5E", "#EF2B2D", "#FFD100"),
  CN = c("#DE2910", "#FFDE00", "#000000"),
  CO = c("#FCD116", "#0033A0", "#CE1126"),
  CR = c("#002B7F", "#FFFFFF", "#D62828"),
  CU = c("#002A8F", "#FFFFFF", "#CE1126"),
  CV = c("#003893", "#FCD116", "#EF7C00"),
  CW = c("#0072C6", "#FFB612", "#EF3340"),
  CY = c("#FFFFFF", "#003366", "#D62828"),
  CZ = c("#11457E", "#FFFFFF", "#D7141A"),
  DE = c("#000000", "#DD0000", "#FFCE00"),
  DJ = c("#006847", "#FCD116", "#CE1126"),
  DK = c("#C60C30", "#FFFFFF", "#000000"),
  DM = c("#00853F", "#FFD100", "#EF3340"),
  DO = c("#002D62", "#FFFFFF", "#D21034"),
  DZ = c("#007229", "#FFFFFF", "#D21034"),
  EC = c("#FF0000", "#FDEF42", "#000000"),
  EE = c("#0000FF", "#FFDD00", "#000000"),
  EG = c("#CE1126", "#FFFFFF", "#000000"),
  ER = c("#007A3D", "#F9DC00", "#CE1126"),
  ES = c("#AA151B", "#F1BF00", "#000000"),
  ET = c("#008000", "#FEEF27", "#DF0000"),
  FI = c("#003580", "#FFFFFF", "#000000"),
  FJ = c("#002B7F", "#FFFFFF", "#EF3340"),
  FM = c("#75B2DD", "#FFFFFF", "#000000"),
  FO = c("#D71920", "#FFFFFF", "#003087"),
  FR = c("#0055A4", "#FFFFFF", "#EF4135"),
  GA = c("#009E49", "#FCDD09", "#CE1126"),
  GB = c("#012169", "#FFFFFF", "#C8102E"),
  GD = c("#CE1126", "#008000", "#FFD100"),
  GE = c("#D90012", "#FFFFFF", "#000000"),
  GG = c("#FFFFFF", "#FF0000", "#000000"),
  GH = c("#006B3F", "#FCD116", "#CE1126"),
  GI = c("#FFFFFF", "#000000", "#FF0000"),
  GL = c("#003087", "#FFFFFF", "#D21034"),
  GM = c("#009E49", "#FFD100", "#CE1126"),
  GN = c("#EF2B2D", "#FCDD09", "#009460"),
  GQ = c("#007A5E", "#FCD116", "#CE1126"),
  GR = c("#0D5EAF", "#FFFFFF", "#000000"),
  GT = c("#006847", "#FFFFFF", "#00A2E8"),
  GU = c("#00205B", "#FFFFFF", "#FF0000"),
  GW = c("#009E49", "#FFD100", "#EF2B2D"),
  GY = c("#009739", "#FFD100", "#EF2B2D"),
  HK = c("#E4002B", "#FFFFFF", "#000000"),
  HN = c("#0073C0", "#FFFFFF", "#000000"),
  HR = c("#FF0000", "#FFFFFF", "#0093DD"),
  HT = c("#00209F", "#FFFFFF", "#EF2B2D"),
  HU = c("#CD2A3E", "#FFFFFF", "#436F4D"),
  ID = c("#FFFFFF", "#DA251D", "#000000"),
  IE = c("#009B48", "#FFFFFF", "#FF7900"),
  IL = c("#0038B8", "#FFFFFF", "#000000"),
  IM = c("#FFFFFF", "#000000", "#FF0000"),
  IN = c("#FF9933", "#FFFFFF", "#138808"),
  IQ = c("#CE1126", "#FFFFFF", "#000000"),
  IR = c("#239F40", "#FFFFFF", "#DA0000"),
  IS = c("#003897", "#FFFFFF", "#D72828"),
  IT = c("#008C45", "#FFFFFF", "#CD212A"),
  JE = c("#EE3227", "#134A7C", "#FFFFFF"),
  JM = c("#009B3A", "#FFD100", "#000000"),
  JO = c("#000000", "#FFFFFF", "#CE1126"),
  JP = c("#BC002D", "#FFFFFF", "#000000"),
  KE = c("#FF0000", "#000000", "#008000"),
  KG = c("#D21034", "#FFFFFF", "#008000"),
  KH = c("#032EA1", "#FFFFFF", "#D52B1E"),
  KI = c("#0000FF", "#FFFFFF", "#FF0000"),
  KM = c("#0C6848", "#FFFFFF", "#E30613"),
  KN = c("#009E49", "#FFD100", "#EF3340"),
  KP = c("#024FA2", "#FFFFFF", "#ED1C27"),
  KR = c("#003478", "#FFFFFF", "#CD2A3E"),
  KW = c("#007A3D", "#FFFFFF", "#CE1126"),
  KY = c("#00247D", "#FFFFFF", "#FF0000"),
  KZ = c("#00A859", "#FFFFFF", "#FEDD00"),
  LA = c("#DA251D", "#002868", "#FECB00"),
  LB = c("#ED1C24", "#FFFFFF", "#007A3D"),
  LC = c("#007A5E", "#FFD100", "#EF3340"),
  LI = c("#FF0000", "#FFFFFF", "#000000"),
  LK = c("#FFB300", "#1E3D59", "#FFFFFF"),
  LR = c("#002868", "#FFFFFF", "#EF3340"),
  LS = c("#003082", "#FFD100", "#EF3340"),
  LT = c("#006A44", "#FEDD00", "#C1272D"),
  LU = c("#E4002B", "#FFD500", "#000000"),
  LV = c("#9E3039", "#FFFFFF", "#000000"),
  LY = c("#E70013", "#000000", "#FFFFFF"),
  MA = c("#C1272D", "#006233", "#FFD100"),
  MC = c("#E30A17", "#FFFFFF", "#000000"),
  MD = c("#0033A0", "#FFFFFF", "#EF2B2D"),
  ME = c("#007A33", "#FCD116", "#000000"),
  MF = c("#00267F", "#FFFFFF", "#FFDD00"),
  MG = c("#FFFFFF", "#FF0000", "#007A5E"),
  MH = c("#00247D", "#FFFFFF", "#EF3340"),
  MK = c("#D20000", "#FFD700", "#000000"),
  ML = c("#00853F", "#FDEF42", "#E31B23"),
  MM = c("#EF2B2D", "#FECB00", "#007A33"),
  MN = c("#0033A0", "#D90012", "#FFD100"),
  MO = c("#009639", "#FFFFFF", "#E31B23"),
  MP = c("#00247D", "#FFFFFF", "#EF3340"),
  MR = c("#006233", "#FDEF42", "#E31B23"),
  MS = c("#00247D", "#FFFFFF", "#EF3340"),
  MT = c("#CE1126", "#FFFFFF", "#000000"),
  MU = c("#F48120", "#003B5C", "#FFFFFF"),
  MV = c("#003897", "#FFFFFF", "#EF3340"),
  MW = c("#D21034", "#008000", "#000000"),
  MX = c("#006847", "#FFFFFF", "#CE1126"),
  MY = c("#012749", "#FEDF00", "#ED1C24"),
  MZ = c("#006A4E", "#FCD116", "#EF2B2D"),
  "NA" = c("#003580", "#FFB612", "#000000"),
  NC = c("#002B7F", "#FF0000", "#FFFFFF"),
  NE = c("#FCDD09", "#009E49", "#EF2B2D"),
  NG = c("#008751", "#FFFFFF", "#000000"),
  NI = c("#0033A0", "#FFFFFF", "#FFD100"),
  NL = c("#21468B", "#FFFFFF", "#AE1C28"),
  NO = c("#BA0C2F", "#FFFFFF", "#00205B"),
  NP = c("#DC143C", "#FFFFFF", "#003893"),
  NR = c("#0033A0", "#FFFFFF", "#EF3340"),
  NZ = c("#00247D", "#FFFFFF", "#CC142B"),
  OM = c("#007A3D", "#FFFFFF", "#D91023"),
  PA = c("#FF0000", "#FFFF00", "#000000"),
  PE = c("#D91023", "#FFFFFF", "#003893"),
  PF = c("#00267F", "#FFFFFF", "#ED2939"),
  PG = c("#007A3D", "#FFD100", "#CE1126"),
  PH = c("#0038A8", "#CE1126", "#FFFFFF"),
  PK = c("#01411C", "#FFFFFF", "#000000"),
  PL = c("#DC143C", "#FFFFFF", "#000000"),
  PM = c("#00267F", "#FFFFFF", "#FFDD00"),
  PR = c("#0055B3", "#FFFFFF", "#CE1126"),
  PT = c("#006600", "#FF0000", "#FFFFFF"),
  PW = c("#0099FF", "#FFFF00", "#000000"),
  PY = c("#D52B1E", "#FFFFFF", "#0038A8"),
  QA = c("#8A1538", "#FFFFFF", "#000000"),
  RO = c("#002B7F", "#FCD116", "#CE1126"),
  RS = c("#C6363C", "#0C4076", "#FFFFFF"),
  RU = c("#FFFFFF", "#0039A6", "#D52B1E"),
  RW = c("#009E49", "#FCDD09", "#007A5E"),
  SA = c("#006C35", "#FFFFFF", "#000000"),
  SB = c("#0051BA", "#FFFFFF", "#FCD116"),
  SC = c("#003F87", "#FCD856", "#D62828"),
  SD = c("#006233", "#FF0000", "#000000"),
  SE = c("#006AA7", "#FECC00", "#000000"),
  SG = c("#EF2B2D", "#FFFFFF", "#000000"),
  SH = c("#0000FF", "#FFFFFF", "#8B4513"),
  SI = c("#0057B8", "#FFFFFF", "#EF3340"),
  SK = c("#0B4EA2", "#FFFFFF", "#EF3340"),
  SL = c("#009E49", "#FCD116", "#EF3340"),
  SM = c("#009246", "#FFFFFF", "#CE2B37"),
  SN = c("#00853F", "#FDEF42", "#E31B23"),
  SO = c("#4189DD", "#FFFFFF", "#E30B17"),
  SR = c("#009B3A", "#FED100", "#EF2B2D"),
  SS = c("#0F47AF", "#FCDD09", "#000000"),
  ST = c("#009E49", "#FCD116", "#CE1126"),
  SV = c("#004B87", "#FFFFFF", "#FFD100"),
  SX = c("#DC1016", "#002688", "#62B0E2"),
  SY = c("#CE1126", "#FFFFFF", "#000000"),
  SZ = c("#3E5EB9", "#FFD900", "#B10C0C"),
  TC = c("#CF0820", "#002368", "#FFFFFF"),
  TD = c("#007A5E", "#EF2B2D", "#FCDD09"),
  TG = c("#009E49", "#FCDD09", "#CE1126"),
  TH = c("#0A1E52", "#FFFFFF", "#DC241F"),
  TJ = c("#009739", "#EF2B2D", "#FFFFFF"),
  TL = c("#000000", "#FFFFFF", "#DC241F"),
  TM = c("#00A651", "#E30B17", "#FFFFFF"),
  TN = c("#E70013", "#FFFFFF", "#008000"),
  TO = c("#CE1126", "#FFFFFF", "#000000"),
  TR = c("#E30A17", "#FFFFFF", "#000000"),
  TT = c("#000000", "#FFDD00", "#EF3340"),
  TV = c("#007A3D", "#FFFFFF", "#000000"),
  TW = c("#000099", "#FFFFFF", "#FF0000"),
  TZ = c("#006600", "#FFCC00", "#000000"),
  UA = c("#0057B7", "#FFD700", "#000000"),
  UG = c("#006A4E", "#FCD116", "#000000"),
  US = c("#3C3B6E", "#FFFFFF", "#BF0A30"),
  UY = c("#0099B5", "#FFFFFF", "#F6B40E"),
  UZ = c("#1EB53A", "#FFFFFF", "#000000"),
  VC = c("#009B3A", "#FFD100", "#EF3340"),
  VE = c("#002868", "#FED100", "#EF3340"),
  VG = c("#D00C27", "#001F7E", "#FFFFFF"),
  VI = c("#F4C63B", "#339541", "#FFFFFF"),
  VN = c("#DA251D", "#FFFF00", "#000000"),
  VU = c("#000000", "#FDCE12", "#D21034"),
  WF = c("#0033A0", "#FFFFFF", "#FF0000"),
  WS = c("#002B7F", "#FFFFFF", "#CE1126"),
  XG = c("#CE1126", "#000000", "#FFFFFF"),
  XK = c("#244AA5", "#FFFFFF", "#D0A650"),
  XW = c("#CE1126", "#000000", "#FFFFFF"),
  YE = c("#CE1126", "#FFFFFF", "#000000"),
  ZA = c("#000000", "#FFB612", "#007A4D"),
  ZM = c("#008000", "#FFD700", "#000000"),
  ZW = c("#009E49", "#FFD100", "#000000")
)

# ----------------------------------------------------------------------------
# International phone codes by ISO2 country to enrich country information's
# ----------------------------------------------------------------------------

phone_codes <- c(
  "AF" = "+93",   "AL" = "+355",  "DZ" = "+213",  "AS" = "+1-684", "AD" = "+376",  "AO" = "+244",
  "AI" = "+1-264","AQ" = "+672",  "AG" = "+1-268", "AR" = "+54",   "AM" = "+374",  "AW" = "+297",
  "AU" = "+61",   "AT" = "+43",   "AZ" = "+994",  "BS" = "+1-242", "BH" = "+973",  "BD" = "+880",
  "BB" = "+1-246","BY" = "+375",  "BE" = "+32",   "BZ" = "+501",  "BJ" = "+229",  "BM" = "+1-441",
  "BT" = "+975",  "BO" = "+591",  "BA" = "+387",  "BW" = "+267",  "BR" = "+55",   "IO" = "+246",
  "VG" = "+1-284","BN" = "+673",  "BG" = "+359",  "BF" = "+226",  "BI" = "+257",  "KH" = "+855",
  "CM" = "+237",  "CA" = "+1",    "CV" = "+238",  "KY" = "+1-345", "CF" = "+236",  "TD" = "+235",
  "CL" = "+56",   "CN" = "+86",   "CX" = "+61",   "CC" = "+61",   "CO" = "+57",   "KM" = "+269",
  "CK" = "+682",  "CR" = "+506",  "HR" = "+385",  "CU" = "+53",   "CW" = "+599",  "CY" = "+357",
  "CZ" = "+420",  "CD" = "+243",  "DK" = "+45",   "DJ" = "+253",  "DM" = "+1-767", "DO" = "+1-809",
  "EC" = "+593",  "EG" = "+20",   "SV" = "+503",  "GQ" = "+240",  "ER" = "+291",  "EE" = "+372",
  "SZ" = "+268",  "ET" = "+251",  "FK" = "+500",  "FO" = "+298",  "FJ" = "+679",  "FI" = "+358",
  "FR" = "+33",   "PF" = "+689",  "GA" = "+241",  "GM" = "+220",  "GE" = "+995",  "DE" = "+49",
  "GH" = "+233",  "GI" = "+350",  "GR" = "+30",   "GL" = "+299",  "GD" = "+1-473", "GU" = "+1-671",
  "GT" = "+502",  "GG" = "+44-1481","GN" = "+224", "GW" = "+245",  "GY" = "+592",  "HT" = "+509",
  "HN" = "+504",  "HK" = "+852",  "HU" = "+36",   "IS" = "+354",  "IN" = "+91",   "ID" = "+62",
  "IR" = "+98",   "IQ" = "+964",  "IE" = "+353",  "IM" = "+44-1624","IL" = "+972", "IT" = "+39",
  "CI" = "+225",  "JM" = "+1-876", "JP" = "+81",  "JE" = "+44-1534","JO" = "+962", "KZ" = "+7",
  "KE" = "+254",  "KI" = "+686",  "KP" = "+850",  "KR" = "+82",   "KW" = "+965",  "KG" = "+996",
  "LA" = "+856",  "LV" = "+371",  "LB" = "+961",  "LS" = "+266",  "LR" = "+231",  "LY" = "+218",
  "LI" = "+423",  "LT" = "+370",  "LU" = "+352",  "MO" = "+853",  "MG" = "+261",  "MW" = "+265",
  "MY" = "+60",   "MV" = "+960",  "ML" = "+223",  "MT" = "+356",  "MH" = "+692",  "MR" = "+222",
  "MU" = "+230",  "YT" = "+262",  "MX" = "+52",   "FM" = "+691",  "MD" = "+373",  "MC" = "+377",
  "MN" = "+976",  "ME" = "+382",  "MS" = "+1-664", "MA" = "+212", "MZ" = "+258",  "MM" = "+95",
  "NA" = "+264",  "NR" = "+674",  "NP" = "+977",  "NL" = "+31",   "NC" = "+687",  "NZ" = "+64",
  "NI" = "+505",  "NE" = "+227",  "NG" = "+234",  "NU" = "+683",  "NF" = "+672",  "MP" = "+1-670",
  "NO" = "+47",   "OM" = "+968",  "PK" = "+92",   "PW" = "+680",  "PS" = "+970",  "PA" = "+507",
  "PG" = "+675",  "PY" = "+595",  "PE" = "+51",   "PH" = "+63",   "PL" = "+48",   "PT" = "+351",
  "PR" = "+1-787","QA" = "+974",  "RO" = "+40",   "RU" = "+7",    "RW" = "+250",  "BL" = "+590",
  "SH" = "+290",  "KN" = "+1-869","LC" = "+1-758","MF" = "+590",  "PM" = "+508",  "VC" = "+1-784",
  "WS" = "+685",  "SM" = "+378",  "ST" = "+239",  "SA" = "+966",  "SN" = "+221",  "RS" = "+381",
  "SC" = "+248",  "SL" = "+232",  "SG" = "+65",   "SX" = "+1-721", "SK" = "+421",  "SI" = "+386",
  "SB" = "+677",  "SO" = "+252",  "ZA" = "+27",   "SS" = "+211",  "ES" = "+34",   "LK" = "+94",
  "SD" = "+249",  "SR" = "+597",  "SE" = "+46",   "CH" = "+41",   "SY" = "+963",  "TW" = "+886",
  "TJ" = "+992",  "TZ" = "+255",  "TH" = "+66",   "TL" = "+670",  "TG" = "+228",  "TO" = "+676",
  "TT" = "+1-868","TN" = "+216",  "TR" = "+90",   "TM" = "+993",  "TC" = "+1-649", "TV" = "+688",
  "UG" = "+256",  "UA" = "+380",  "AE" = "+971",  "GB" = "+44",   "US" = "+1",    "UY" = "+598",
  "UZ" = "+998",  "VU" = "+678",  "VA" = "+379",  "VE" = "+58",   "VN" = "+84",   "WF" = "+681",
  "EH" = "+212",  "YE" = "+967",  "ZM" = "+260",  "ZW" = "+263"
)

# --------------------------------------------------------------
# Function: Recommend countries based on filters
# --------------------------------------------------------------
recommander_pays <- function(continent_filter = NULL, country_filter, age, sexe, raison) {
  
  # Get origin country by name or ISO2
  data_origine <- result %>%
    filter(tolower(country) == tolower(country_filter) |
             tolower(iso2c) == tolower(country_filter))
  
  if (nrow(data_origine) == 0) {
    stop("Origin country not found in dataset.")
  }
  
  # Select age column based on age group
  if (age < 19) {
    age_col <- "Child migrants (<19)"
  } else if (age >= 19 & age <= 64) {
    age_col <- "Working age migrants (20-64)"
  } else {
    age_col <- "Older migrants (>65)"
  }
  
  # Select migration reason column
  raison_col <- switch(raison,
                       "scholarship" = "International students (destination country/region)",
                       "residence"   = "Permanent migration inflows",
                       "asylum"      = "Asylum seekers (host country/region)",
                       "refugees"    = "Refugees (host country/region)",
                       age_col)
  
  # Gender weight based on female share
  female_share <- as.numeric(data_origine$`Share of female immigrants`[1])
  if (is.na(female_share)) female_share <- 50
  
  # Extract migration flows to each country
  migration_cols <- names(data_origine)[grepl("^Migration_to_", names(data_origine))]
  flux <- as.numeric(data_origine[1, migration_cols])
  
  raison_val <- as.numeric(data_origine[[raison_col]][1])
  if (is.na(raison_val)) raison_val <- 1
  
  # Compute scores
  scores <- flux * raison_val
  if (tolower(sexe) == "f") {
    scores <- scores * (female_share / 100)
  } else {
    scores <- scores * (1 - female_share / 100)
  }
  
  # Build dataframe of destination scores
  destinations <- gsub("Migration_to_", "", migration_cols)
  df_destinations <- data.frame(destination = destinations, score = scores, stringsAsFactors = FALSE)
  
  # Match destinations to continents
  continents_df <- result %>%
    select(country, iso2c, continent) %>%
    distinct() %>%
    filter(country %in% destinations)
  
  result_df <- df_destinations %>%
    left_join(continents_df, by = c("destination" = "country"))
  
  # Warn for missing continent
  if (any(is.na(result_df$continent))) {
    warning("Missing continent for: ", paste(result_df$destination[is.na(result_df$continent)], collapse = ", "))
  }
  
  # Clean continent names
  result_df$continent <- trimws(result_df$continent)
  result_df$continent <- str_to_title(result_df$continent)
  
  # 🧹 Exclude missing continent
  result_df <- result_df %>% filter(!is.na(continent))
  
  # Filter by continent if specified
  if (!is.null(continent_filter)) {
    result_df <- result_df %>% filter(continent == continent_filter)
  }
  
  # Top 1 per continent
  top_par_continent <- result_df %>%
    group_by(continent) %>%
    slice_max(order_by = score, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    arrange(desc(score))
  
  # Top overall
  top_global <- top_par_continent %>%
    slice_max(order_by = score, n = 1, with_ties = FALSE)
  
  return(list(
    top_par_continent = top_par_continent,
    top_global = top_global
  ))
}

# --------------------------------------------------------------
# Gemini API: Chat history + call function
# --------------------------------------------------------------
chatHistory <- list()  # store conversation globally

# Clé API définie en dur
GEMINI_API_KEY <- "AIzaSyDrI53CFnUqeNZNZmWY1V_a5s92Gt4e3b8"

gemini <- function(prompt, 
                   temperature = 1,
                   max_output_tokens = 1024,
                   api_key = GEMINI_API_KEY,
                   model = "gemini-2.0-flash") {
  
  # ➕ Add user prompt to history
  chatHistory <<- append(chatHistory, list(list(
    role = "user",
    parts = list(list(text = prompt))
  )))
  
  # Build body with full history
  body_list <- list(
    contents = chatHistory,
    generationConfig = list(
      temperature = temperature,
      maxOutputTokens = max_output_tokens
    )
  )
  
  # Send POST request
  response <- httr::POST(
    url = paste0("https://generativelanguage.googleapis.com/v1beta/models/", model, ":generateContent"),
    query = list(key = api_key),
    httr::content_type_json(),
    encode = "json",
    body = jsonlite::toJSON(body_list, auto_unbox = TRUE)
  )
  
  # Error handling
  if (response$status_code > 200) {
    chatHistory <<- chatHistory[-length(chatHistory)]
    stop(paste("Error -", httr::content(response)$error$message))
  }
  
  # Extract model response
  answer <- httr::content(response)$candidates[[1]]$content$parts[[1]]$text
  
  # Add answer to history
  chatHistory <<- append(chatHistory, list(list(
    role = "model",
    parts = list(list(text = answer))
  )))
  
  return(answer)
}
# --------------------------------------------------------------
# Population Pyramid 
# --------------------------------------------------------------

# --------------------------------------------------------------
# Total population per country
# --------------------------------------------------------------
population_code <- pyramide_plot |>
  group_by(code) |>
  summarise(total_population = sum(population, na.rm = TRUE)) |>
  dplyr::filter(!is.na(total_population))

# --------------------------------------------------------------
# Gradient generator from flag colors
# --------------------------------------------------------------
get_country_gradient <- function(colors, n) {
  colorRampPalette(colors)(n)
}

# --------------------------------------------------------------
# Pyramid Plot Function
# --------------------------------------------------------------
plot_pyramide_par_pays <- function(data, country_code = NULL, country_name = NULL, gap_ratio = 0.1) {
  
  age_levels <- c("0_4", "5_9", "10_14", "15_19", "20_24", "25_29", "30_34", "35_39",
                  "40_44", "45_49", "50_54", "55_59", "60_64", "65_69", "70_74",
                  "75_79", "80_84", "85_89", "90_94", "95_99", "100_")
  
  # 🇨🇺 Filter data by country code or name
  if (!is.null(country_code)) {
    df <- data %>% filter(code == country_code)
    flag_cols <- flag_colors_hex[[country_code]]
  } else if (!is.null(country_name)) {
    df <- data %>% filter(name == country_name)
    code <- unique(df$code)
    flag_cols <- flag_colors_hex[[code]]
  } else {
    stop("Specify either 'country_code' or 'country_name'")
  }
  
  if (nrow(df) == 0) stop("No data for this country.")
  
  # Interpolate colors for all age segments
  n_segments <- nrow(df)
  palette_colors <- get_country_gradient(flag_cols, n_segments)
  df <- df %>% mutate(segment_id = row_number())
  
  # Define age categories
  df <- df %>%
    mutate(
      age_lower = as.numeric(str_extract(AGE, "^\\d+")),
      age_category = case_when(
        age_lower < 15 ~ "Children (0-14)",
        age_lower < 25 ~ "Youth (15-24)",
        age_lower < 45 ~ "Young Adult (25-44)",
        age_lower < 65 ~ "Adult (45-64)",
        TRUE          ~ "Seniors (65+)"
      )
    )
  
  # Compute share per age group
  age_group_props <- df %>%
    group_by(age_category) %>%
    summarise(pop = sum(pop_abs, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(
      age_category = factor(age_category, 
                            levels = c("Children (0-14)", "Youth (15-24)", "Young Adult (25-44)", "Adult (45-64)", "Seniors (65+)")),
      total = sum(pop),
      pct = round(100 * pop / total, 1),
      label = paste0(age_category, ": ", pct, "%")
    ) %>%
    arrange(age_category)
  
  age_group_title <- paste(age_group_props$label, collapse = " | ")
  
  # Build rectangles for plotting
  rect_width <- 0.8
  gap <- max(df$pop_abs, na.rm = TRUE) * gap_ratio
  
  pyramide_df <- df %>%
    mutate(
      AGE = factor(AGE, levels = age_levels),
      age_num = as.numeric(AGE),
      xmin = age_num - rect_width / 2,
      xmax = age_num + rect_width / 2,
      ymin = ifelse(SEX == "Male", -gap - pop_abs, gap),
      ymax = ifelse(SEX == "Male", -gap, gap + pop_abs)
    ) %>%
    mutate(
      total_pop = sum(pop_abs, na.rm = TRUE),
      percent = round(100 * pop_abs / total_pop, 1)
    )
  
  # Plot
  ggplot(pyramide_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
    geom_rect(aes(fill = factor(segment_id)), color = "grey", linewidth = 0.2) +
    geom_text(
      aes(
        x = age_num,
        y = ifelse(SEX == "Male", ymin - gap * 0.05, ymax + gap * 0.05),
        label = paste0(percent, "%")
      ),
      size = 3.5,
      hjust = ifelse(pyramide_df$SEX == "Male", 1, 0),
      fontface = "bold"
    ) +
    annotate("text", x = max(pyramide_df$age_num) + 0.1, y = -max(pyramide_df$pop_abs) * 0.7,
             label = "Male", hjust = 0, size = 5, fontface = "bold") +
    annotate("text", x = max(pyramide_df$age_num) + 0.1, y =  max(pyramide_df$pop_abs) * 0.7,
             label = "Female", hjust = 1, size = 5, fontface = "bold") +
    geom_text(
      data = pyramide_df %>% filter(SEX == "Male"),
      aes(x = age_num, y = 0, label = AGE),
      size = 4,
      hjust = 0.5
    ) +
    scale_x_continuous(breaks = 1:length(age_levels), labels = NULL, expand = expansion(add = 0.5)) +
    scale_y_continuous(labels = function(x) format(abs(x), big.mark = " ", scientific = FALSE)) +
    scale_fill_manual(values = setNames(palette_colors, as.character(1:n_segments)), guide = "none") +
    coord_flip() +
    theme_void() +
    theme(
      axis.text.y = element_blank(),
      axis.text.x = element_blank(),
      legend.position = "none",
      plot.margin = unit(c(10, 10, 10, 10), "pt"),
      plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 12, face = "italic")
    ) +
    labs(title = paste0(" ", unique(pyramide_df$name)), subtitle = age_group_title)
}

# --------------------------------------------------------------
# Country / Continent dataset preparation
# --------------------------------------------------------------
countries <- data.frame(
  country = countrycode::codelist$country.name.en,
  continent = countrycode::codelist$continent
) %>%
  dplyr::filter(!is.na(continent)) %>%
  filter(!is.na(continent), !is.na(country)) %>%
  distinct() %>%
  filter(country %in% pays_continent$country) %>%
  mutate(
    code_pays = countrycode(country, origin = "country.name", destination = "iso2c")
  ) %>%
  filter(continent != "Antarctica")

# --------------------------------------------------------------
# Summary of countries by continent
# --------------------------------------------------------------
# Count the number of countries per continent (excluding Antarctica)
continents_summary <- countries %>%
  group_by(continent) %>%
  filter(continent != "Antarctica") %>%
  summarise(nb_pays = n()) %>%
  arrange(continent)

# --------------------------------------------------------------
# Gender distribution summary per country
# --------------------------------------------------------------
# Aggregate population by gender (Male/Female) for each country
gender_summary <- pyramide_plot %>%
  group_by(name, code, SEX) %>%
  summarise(total = sum(population), .groups = "drop") %>%
  filter(total > 0) %>%
  tidyr::pivot_wider(
    names_from = SEX,
    values_from = total,
    values_fill = 0
  ) %>%
  mutate(
    total_pop = Female + Male,
    female_percent = round(100 * Female / total_pop),
    male_percent = 100 - female_percent
  )

# --------------------------------------------------------------
# Add gender HTML badges for display in the reactable
# --------------------------------------------------------------
# The column "gender" will contain a small inline icon bar showing the percentage
gender_summary <- gender_summary %>%
  mutate(
    gender = paste0(
      "<div style='display: flex; align-items: center; gap: 1px; font-size: 14px;'>
         <span style='color: black; font-size:13px;'>", female_percent,
      "% <i class='fa fa-female' style='color:#66cc66; font-size:16px;'></i></span>
         <span>|</span>
         <span style='color: black; font-size:13px;'>
           <i class='fa fa-male' style='color:#238823; font-size:16px;'></i> ", male_percent, "%
         </span>
       </div>"
    )
  )

# --------------------------------------------------------------
# Literacy data: select latest info and clean numeric fields
# --------------------------------------------------------------
Literacy_Rate_latest <- Literacy_Rate %>%
  ungroup() %>%
  select(Country, iso2c, urban, rural, `Literacy Rate`, labor_force) %>%
  mutate(
    # Clean the literacy rate column (remove symbols like “%” or spaces)
    `Literacy Rate` = as.numeric(gsub("[^0-9\\.]", "", `Literacy Rate`)),
    urban = as.numeric(urban),
    rural = as.numeric(rural),
    # Determine if the country is more urban or rural
    location_type = case_when(
      is.na(urban) | is.na(rural) ~ NA_character_,
      urban > rural ~ "Urban",
      rural > urban ~ "Rural",
      TRUE ~ "Equal"
    )
  )

# --------------------------------------------------------------
# Merge literacy data with country information
# --------------------------------------------------------------
countries <- countries %>%
  left_join(
    Literacy_Rate_latest %>%
      select(Country, `Literacy Rate`, location_type, urban, rural, labor_force),
    by = c("country" = "Country")
  )

# --------------------------------------------------------------
# Merge gender distribution data
# --------------------------------------------------------------
countries <- countries %>%
  filter(continent != "Antarctica") %>%
  left_join(
    gender_summary %>% select(code, gender),
    by = c("code_pays" = "code")
  )

# --------------------------------------------------------------
# Merge macroeconomic indicators (GDP, unemployment, exchange rate)
# --------------------------------------------------------------
countries <- countries %>%
  left_join(
    Macro1 %>% select(iso2c, GDP, GDP_per_capita_PPP, Unemployment, Exchange_rate),
    by = c("code_pays" = "iso2c")
  )

# --------------------------------------------------------------
# Merge capital coordinates and places (for map display)
# --------------------------------------------------------------
countries <- countries %>%
  left_join(
    Capital %>% select(iso2c, latitude, longitude, Places),
    by = c("code_pays" = "iso2c")
  )

# =====================================================================
# FUNCTION: get_country_table() to build reactable
# ---------------------------------------------------------------------
# Arguments:
#   - continent_name : Name of the continent to filter countries on.
#   - highlighted_countries : Optional character vector of countries to highlight.
# ---------------------------------------------------------------------
# Output:
#   - A reactable HTML table with multiple column groups and rich content.
# =====================================================================

get_country_table <- function(continent_name, highlighted_countries = character(0)) {
  
  # -------------------------------------------------------------------
  # Prepare sparkline data for GDP growth & Inflation over time
  # -------------------------------------------------------------------
  spark_data <- growth_inflation %>%
    group_by(iso2c) %>%
    summarise(
      GDP_GR_sparkline = list(GDP_GR),
      Inflation_sparkline = list(Inflation),
      .groups = "drop"
    )
  
  # -------------------------------------------------------------------
  # Main dataset preparation
  # -------------------------------------------------------------------
  df <- countries |> 
    filter(continent != "Antarctica") %>%
    left_join(population_code, by = c("code_pays" = "code")) %>%
    left_join(spark_data, by = c("code_pays" = "iso2c")) %>%
    filter(continent == continent_name) %>%
    filter(!is.na(total_population)) %>%
    
    # Optional filtering on highlighted countries
    {
      if (length(highlighted_countries) > 0) {
        filter(., country %in% highlighted_countries)
      } else {
        .
      }
    } %>%
    
  # -----------------------------------------------------------------
  # Add flags, location and country display elements
  # -----------------------------------------------------------------
  mutate(
    flag_url = paste0("https://flagcdn.com/24x18/", tolower(code_pays), ".png"),
    phone = phone_codes[code_pays],
    country_display = paste0(
      country, " (", code_pays, ", ", phone, ") ",
      sprintf(
        "<i class='fa fa-location-dot location-icon' 
               style='color: green; cursor: pointer;' 
               id='loc_%s' 
               title='Map capital city'></i>",
        code_pays
      )
    ),
    pyramid_icon = sprintf(
      "<img src='pictures/pyramid_img.png' class='pyramid-icon' 
              id='icon_%s' title='Show age pyramid'>", code_pays
    )
  ) %>%
    
  # -----------------------------------------------------------------
  # Add population display with age pyramid icon
  # -----------------------------------------------------------------
  mutate(
    population_with_icon = sprintf(
      "<div style='display: flex; align-items: center; justify-content: flex-end; gap: 1px;'>
           <span>%s</span>
           <img src='pictures/pyramid_img.png' class='pyramid-icon' id='icon_%s' title='Show age pyramid'>
         </div>", 
      formatC(total_population, format = "d", big.mark = ","), code_pays
    )
  ) %>%
    
  # -----------------------------------------------------------------
  # Add urban/rural location icons
  # -----------------------------------------------------------------
  mutate(
    location_icon = case_when(
      location_type == "Urban" ~ sprintf(
        "<i class='fa-solid fa-city' style='color:green; font-size:13px;' title='Urban population'></i> 
           <span style='font-style: italic; font-size:13px;'>%.0f%%</span>", urban
      ),
      location_type == "Rural" ~ sprintf(
        "<i class='fa-solid fa-tractor' style='color:#66cc66; font-size:13px;' title='Rural population'></i> 
           <span style='font-style: italic; font-size:13px;'>%.0f%%</span>", rural
      ),
      location_type == "Equal" ~ sprintf(
        "<i class='fa-solid fa-balance-scale' style='color:#999;' title='Equal urban/rural population'></i> 
           <span style='font-style: italic;'>%.0f%%</span>", urban
      ),
      TRUE ~ "<i class='fa-solid fa-question-circle' style='color:gray;' title='Unknown urban/rural ratio'></i>"
    )
  ) %>%
    
  # -----------------------------------------------------------------
  #  Add multimedia and AI icons
  # -----------------------------------------------------------------
  mutate(
    video_icon = ifelse(!is.na(Places),
                        sprintf(
                          "<i class='fas fa-map-marked-alt' style='color:#66cc66; font-size: 16px; cursor: pointer;' 
                                  id='video_%s' title='Best places Video'></i>",
                          code_pays
                        ),
                        "<i class='fas fa-map-marked-alt' style='color: grey; font-size: 16px;' title='No video available'></i>"
    ),
    info_icon = sprintf(
      "<i class='fas fa-brain info-icon' id='info_%s' title='Ask it to AI' 
            style='color: #66cc66; font-size: 16px; cursor: pointer;'></i>",
      code_pays
    )
  )
  
  # -------------------------------------------------------------------
  # Build the Reactable table
  # -------------------------------------------------------------------
  reactable(
    df %>% select(
      flag_url, country_display, population_with_icon, gender, `Literacy Rate`,
      labor_force, location_icon, GDP, GDP_per_capita_PPP, GDP_GR_sparkline, 
      Unemployment, Exchange_rate, Inflation_sparkline, video_icon, info_icon
    ),
    
    # ---------------------------------------------------------------
    # Column formatting
    # ---------------------------------------------------------------
    columns = list(
      flag_url = colDef(
        name = "Flag",
        width = 44,
        align = "center",
        cell = function(value, index) {
          country_code <- df$code_pays[index]
          htmltools::HTML(sprintf("
            <div class='flag-container' style='display: flex; align-items: center; gap: 0px; justify-content: left;'>
              <img src='%s' alt='flag' class='waving-flag' style='width:18px; height:14px;' />
              <button class='anthem-icon' id='anthem_%s' title='Play Anthem' style='background:none; border:none; cursor:pointer; font-size:9px; color:green;'>🎵</button>
            </div>
          ", value, tolower(country_code)))
        },
        html = TRUE
      ),
      country_display = colDef(
        name = "Country",
        align = "left",
        width = 260,
        html = TRUE,
        cell = function(value) {
          if (value %in% highlighted_countries) {   
            div(style = "background-color: lightgreen;", value)
          } else {
            value
          }
        }
      ),
      population_with_icon = colDef(
        name = "Population",
        width = 120,
        html = TRUE,
        align = "center",
        cell = function(value) htmltools::HTML(
          sprintf("<span style='font-size:13px;'><i>%s</i></span>", value)
        )
      ),
      
      gender = colDef(
        name = "Gender",
        width = 100,
        html = TRUE,
        align = "center",
        cell = function(value) htmltools::HTML(sprintf("<i>%s</i>", value))
      ),
      
      `Literacy Rate` = colDef(
        name = "Literacy",
        width = 77,  
        align = "center",
        html = TRUE,
        cell = function(value) {
          if (is.na(value)) value <- 0
          if (value <= 1) value <- round(value * 100, 0)
          bar_width <- paste0(value, "%")
          
          html <- paste0(
            "<div style='display: flex; align-items: center; gap: 1px;'>",
            # label
            "<div style='min-width: 10px; font-style: italic; color: black; font-size: 13px;'>",
            sprintf("%.0f%%", value),
            "</div>",
            # bar container with fixed height
            "<div style='flex-grow: 1; background-color: #a9a9a9; border-radius: 5px; height: 7px; position: relative;'>",
            # bar itself
            "<div style='background-color: green; width: ", bar_width, "; height: 100%; border-radius: 5px;'>",
            "</div>",
            "</div>",
            "</div>"
          )
          htmltools::HTML(html)
        }
      ),
      labor_force = colDef(
        name = "Labor",
        width = 65,
        align = "center",
        html = TRUE,
        cell = function(value) {
          if (is.na(value)) return("<span style='color:gray; font-size: 13px;'>N/A</span>")
          
          # Ensure value is numeric before comparing or formatting
          numeric_value <- suppressWarnings(as.numeric(value))
          
          if (is.na(numeric_value)) {
            # If value cannot be converted to numeric, display as is (e.g., "N/A" or any string)
            display_value <- as.character(value)
            color <- "#cccccc"  # neutral gray background for unknown/non-numeric values
            text_color <- "black"
          } else {
            # Assign color based on numeric value
            color <- if (numeric_value < 40) {
              "#b7efb7"  # light green
            } else if (numeric_value < 60) {
              "#66cc66"  # medium green
            } else {
              "#238823"  # dark green
            }
            
            # Determine text color based on background
            text_color <- if (color == "#b7efb7") "black" else "white"
            
            # Format numeric value as integer percentage
            display_value <- sprintf("%.0f%%", numeric_value)
          }
          
          htmltools::HTML(sprintf(
            "<div style='background-color:%s; color:%s; border-radius: 4px; padding: 2px 6px; font-size: 13px; font-style: italic;'>%s</div>",
            color, text_color, display_value
          ))
        }
      ),
      location_icon = colDef( name = "Location", 
                              width = 85, html = TRUE, 
                              align = "center", 
                              cell = function(value) htmltools::HTML(value) 
      ),
      GDP = colDef(
        name = "GDP",
        align = "center",
        width = 140,
        html = TRUE,
        cell = function(value) {
          if (is.na(value)) {
            return(htmltools::HTML(
              "<span style='font-style: italic; font-size: 13px; color: gray;'>N/A</span>"
            ))
          }
          formatted_value <- formatC(as.numeric(value), format = "f", big.mark = ",", digits = 0)
          htmltools::HTML(sprintf(
            "<span style='font-style: italic; font-size: 13px;'>%s</span>",
            formatted_value
          ))
        }
      ),
      
      GDP_per_capita_PPP = colDef(
        name = "GDP/cap",
        align = "center",
        width = 85,
        html = TRUE,
        cell = function(value) {
          if (is.na(value)) return("<span style='font-style: italic; font-size: 13px; color: gray;'>N/A</span>")
          htmltools::HTML(sprintf("<span style='font-style: italic; font-size: 13px;'>%s</span>",
                                  formatC(value, format = "f", digits = 0, big.mark = ",")))
        }
      ),
      
      Unemployment = colDef(
        name = "Um",
        align = "center",
        width = 60,
        html = TRUE,
        cell = function(value) {
          if (is.na(value)) return("<span style='font-style: italic; font-size: 13px; color: gray;'>N/A</span>")
          htmltools::HTML(sprintf("<span style='font-style: italic; font-size: 13px;'>%.2f%%</span>", value))
        }
      ),
      
      Exchange_rate = colDef(
        name = "Ex Rate($)",
        align = "center",
        width = 95,
        html = TRUE,
        cell = function(value) {
          if (is.na(value)) return("<span style='font-style: italic; font-size: 13px; color: gray;'>N/A</span>")
          htmltools::HTML(sprintf("<span style='font-style: italic; font-size: 13px;'>%.1f</span>", value))
        }
      ),
      
      GDP_GR_sparkline = colDef(
        name = "GDP Growth",
        align = "center",
        html = TRUE,
        width = 107,
        cell = function(value) {
          if (is.null(value)) return("")
          
          vals <- round(unlist(value), 2)
          last_val <- tail(vals, 1)
          
          # Sparkline with negatives bars in light green
          spk <- sparkline(
            values = vals,
            type = "bar",
            barColor = "darkgreen",
            negBarColor = "#66cc66",
            tooltipFormat = '{{value}}%',
            tooltipValueLookups = NULL
          )
          
          last_val_color <- if (!is.na(last_val) && last_val > 0) "green" else "red"
          
          htmltools::tags$div(
            style = "display: flex; align-items: center; justify-content: left;",
            spk,
            htmltools::tags$div(
              style = sprintf("font-style: italic; font-size: 9px; margin-left: 4px; color: %s;", last_val_color),
              sprintf("%.1f%%", last_val)
            )
          )
        }
      ),
      
      
      Inflation_sparkline = colDef(
        name = "Inflation",
        align = "center",
        html = TRUE,
        width = 95,
        cell = function(value) {
          if (is.null(value)) return("")
          vals <- round(unlist(value), 2)
          last_val <- tail(vals, 1)
          
          spk <- sparkline(
            vals,
            type = "line",
            lineColor = "darkgreen",
            fillColor = "rgba(0,100,0,0.3)",
            spotRadius = 1,          # radius of points
            spotColor = "darkgreen", # color of spots
            minSpotColor = "darkgreen",
            maxSpotColor = "darkgreen",
            highlightSpotColor = "red",
            highlightLineColor = "red",
            tooltipFormat = '{{y}}%', # tooltip format showing value with %
            tooltipValueLookups = NULL
          )
          
          # Combine sparkline + label below last point
          htmltools::tags$div(style = "display: flex; align-items: center;", spk, htmltools::tags$div(style = "font-style: italic; font-size: 9px; margin-left: 1px; color: darkgreen;", paste0(round(last_val,1), "%")))
          
        }
      ),
      
      video_icon = colDef(
        name = "Places",
        width = 65,
        align = "center",
        html = TRUE,
        cell = function(value) htmltools::HTML(value)
      ),
      
      info_icon = colDef(
        name = "AI",
        width = 55,
        html = TRUE,
        align = "center",
        cell = function(value, index) {
          country_code <- df$code_pays[index]  
          htmltools::HTML(sprintf(
            "<i class='fa fa-brain info-icon' 
           id='info_%s' 
           title='Ask AI Agent !' 
           style='cursor: pointer; color: darkgreen; font-size: 15px;'></i>",
            country_code
          ))
        }
      )
      
    ),
    # ---------------------------------------------------------------
    # 📌 Column groups
    # ---------------------------------------------------------------
    columnGroups = list(
      colGroup(name = "National symbols", columns = c("flag_url", "country_display")),
      colGroup(name = "Demographics", columns = c("population_with_icon", "gender", "Literacy Rate", "labor_force", "location_icon")),
      colGroup(name = "Macroeconomics", columns = c("GDP", "GDP_per_capita_PPP", "GDP_GR_sparkline", "Unemployment", "Exchange_rate", "Inflation_sparkline")),
      colGroup(name = "Others", columns = c("video_icon", "info_icon"))
    ),
    
    bordered = TRUE,
    highlight = TRUE,
    striped = TRUE,
    defaultPageSize = 5,
    paginationType = "simple",
    compact = TRUE,
    theme = reactableTheme(
      stripedColor = "#f6f8fa",
      highlightColor = "#B0F2B6"
    )
  )
}

# ---------------------------------------------------------------
# Function to add a Font Awesome icon according to the continent
# ---------------------------------------------------------------

get_continent_icon <- function(continent) {
  img_file <- switch(continent,
                     "Africa" = "pictures/Africa.png",
                     "Asia" = "pictures/Asia.png",
                     "Europe" = "pictures/Europe.png",
                     "Americas" = "pictures/Americas.png",
                     "South America" = "pictures/Americas.png",
                     "Oceania" = "pictures/Oceania.png",
                     "Antarctica" = "pictures/Antarctica.png",
                     NULL
  )
  
  if (!is.null(img_file)) {
    htmltools::HTML(sprintf(
      '<div style="display: flex; align-items: center; gap: 10px;">
         <img src="%s" alt="%s" width="25" height="25" style="border-radius: 4px;" />
         <span>%s</span>
       </div>', img_file, continent, continent
    ))
  } else {
    continent
  }
}
# -----------------------------------------------------------------------------
# Function to display a country flag with a button to play the national anthem
# -----------------------------------------------------------------------------
get_country_flag_with_anthem <- function(country_code, country_name) {
  sprintf('
    <span class="flag-container" style="display: flex; align-items: center; gap: 1px;">
      <img src="flags/%s.png" alt="%s" class="pyramid-icon" id="icon_%s" style="width:24px; height:22px; cursor:pointer;" />
      <button class="anthem-icon" id="anthem_%s" style="background:none; border:none; cursor:pointer; font-size:18px; color:green;">🎵</button>
    </span>
  ', tolower(country_code), country_name, tolower(country_code), tolower(country_code))
}

# -----------------------------------------------
# UI with  navbarPage with a lot of css theming
# -----------------------------------------------

ui <- navbarPage(
  div(
    class = "title-panel",
    style = "
    display: flex;
    color: #ffffff;
    justify-content: center;
    align-items: center;
    gap: 10px;
    font-size: 24px;
    font-weight: bold;
    padding: 0;
    margin: 0;
    line-height: 1; /* important pour réduire l’espace vertical du texte */
    height: 20px; /* ou une hauteur fixe, ajuste selon besoin */
  ",
    "Migration Recommender",
    tags$div(
      class = "globe-animation",
      style = "display: flex; margin-left: 10px;",
      tags$img(src = "pictures/globe11.png", class = "globe-frame active", style = "width:35px; height:35px;"),
      tags$img(src = "pictures/globe21.png", class = "globe-frame", style = "width:35px; height:35px;"),
      tags$img(src = "pictures/globe31.png", class = "globe-frame", style = "width:35px; height:35px;"),
      tags$img(src = "pictures/globe41.png", class = "globe-frame", style = "width:35px; height:35px;")
    )
  ),
  windowTitle = h3("Explore the World", style = "color: #fab005; font-weight: bold; font-size: 24px;"),
  
  # Main page 
  tabPanel(" ",
           fluidPage(
             tags$head(
               # layout design of the application
               tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"),
               tags$style(HTML("
  
  .waving-flag {
    width: 100%;
    height: 100%;
    object-fit: cover;
    animation: wave 5s ease-in-out infinite;
  }

  .globe-animation {
    position: relative;
    width: 35px;
    height: 35px;
  }

  .globe-frame {
    position: absolute;
    top: 0;
    left: 0;
    width: 50px;
    height: 50px;
    opacity: 0;
    transition: opacity 0.2s linear;
    pointer-events: none;
  }

  .globe-frame.active {
    opacity: 1;
  }
")),
               
               tags$style(HTML("
  .navbar {
    position: fixed;
    top: 0;
    width: 100%;
    z-index: 9999;
  }

  body {
    padding-top: 70px; /* éviter que le contenu passe sous la barre */
  }
")),
               
               tags$script(HTML("
  document.addEventListener('DOMContentLoaded', () => {
    const frames = document.querySelectorAll('.globe-frame');
    let currentFrame = 0;
    const totalFrames = frames.length;

    function showNextFrame() {
      frames.forEach((frame, index) => {
        frame.classList.remove('active');
      });
      frames[currentFrame].classList.add('active');
      currentFrame = (currentFrame + 1) % totalFrames;
    }

    setInterval(showNextFrame, 1000);
  });
  
  document.addEventListener('click', function(event) {
  const target = event.target;
  if (target.classList.contains('anthem-icon')) {
    const id = target.id;
    const match = id.match(/^anthem_(.+)$/);
    if (match && window.Shiny) {
      Shiny.setInputValue('play_anthem_country', match[1], { priority: 'event' });
    }
  }
});

")),
               tags$style(HTML("
  .navbar {
    background: 
      repeat center center url('https://img.fortawesome.com/1ce05b4b/tablecloth-pattern3.png'), 
      linear-gradient(135deg, #009B3A, #006622);
    overflow: hidden;
  }
  
  .navbar-default .navbar-nav > li > a,
.navbar-default .navbar-nav > li > a:focus,
.navbar-default .navbar-nav > li > a:hover,
.navbar-default .navbar-nav > .active > a,
.navbar-default .navbar-nav > .active > a:focus,
.navbar-default .navbar-nav > .active > a:hover {
    color: #ffffff;
    background-color: transparent;
}

")),
               tags$style(HTML("
  .pyramid-icon {
    width: 22px;
    height: 16px;
    cursor: pointer;
    transition: transform 0.2s ease;
  }

  .pyramid-icon:hover {
    transform: scale(1.3);
  }
")),
               
               tags$script(HTML("
  document.addEventListener('click', function(event) {
    const target = event.target;
    if (target.classList.contains('pyramid-icon')) {
      const id = target.id;
      const match = id.match(/^icon_(.+)$/);
      if (match && window.Shiny) {
        Shiny.setInputValue('show_pyramid_country', match[1], { priority: 'event' });
      }
    }
  });
")),
               tags$script(HTML("
  document.addEventListener('click', function(event) {
    const target = event.target;
    if (target.classList.contains('location-icon')) {
      const id = target.id;  // par ex. 'loc_AF'
      const match = id.match(/^loc_(.+)$/);
      if (match && window.Shiny) {
        Shiny.setInputValue('show_capital', match[1], { priority: 'event' });
      }
    }
  });
")),
               tags$script(HTML("
  document.addEventListener('click', function(event) {
    const target = event.target;
    if (target.classList.contains('fa-map-marked-alt')) {
      const id = target.id;
      const match = id.match(/^video_(.+)$/);
      if (match && window.Shiny) {
        Shiny.setInputValue('show_video_country', match[1], { priority: 'event' });
      }
    }
  });
")),
               
               tags$script(HTML("
  document.addEventListener('click', function(event) {
    const target = event.target;
    if (target.classList.contains('info-icon')) {
      const id = target.id;
      const match = id.match(/^info_(.+)$/);
      if (match && window.Shiny) {
        Shiny.setInputValue('show_info_modal', match[1], { priority: 'event' });
      }
    }
  });
")),
               tags$style(HTML("
  .irs--shiny .irs-bar {
    top: 25px;
    height: 8px;
    border-top: 1px solid #238823;
    border-bottom: 1px solid #238823;
    background: #238823;
    cursor: s-resize;
    z-index: 2;
  }
")),
               tags$style(HTML("
  .btn-success {
    background-color: #238823 !important;
    border-color: #238823 !important;
  }

  .btn-success:hover {
    background-color: #238823 !important;
    border-color: #238823 !important;
  }
")),
               tags$style(HTML("
  .irs--shiny .irs-from,
  .irs--shiny .irs-to,
  .irs--shiny .irs-single {
    color: #fff;
    text-shadow: none;
    padding: 1px 3px;
    background-color: #238823;
    border-radius: 3px;
    font-size: 11px;
    line-height: 1.333;
  }
")),
               tags$style(HTML("
    /* Pour laisser de la place au footer et éviter qu’il cache le contenu */
    body {
      padding-bottom: 60px;
    }
  ")),
               tags$style(HTML("
  /* Style pour le footer */
  footer.navbar-fixed-bottom {
    background: 
      repeat center center url('https://img.fortawesome.com/1ce05b4b/tablecloth-pattern3.png'), 
      linear-gradient(135deg, #009B3A, #006622);
    overflow: hidden;
    color: #ffffff;
    padding: 10px 20px;
    text-align: center;
    border-top: none;
    font-weight: bold;
  }

  /* Si tu veux gérer les liens dans le footer (au cas où) */
  footer.navbar-fixed-bottom a,
  footer.navbar-fixed-bottom a:focus,
  footer.navbar-fixed-bottom a:hover {
    color: #ffffff;
    background-color: transparent;
    text-decoration: underline;
  }
")),
               tags$style(HTML("
.radio-buttons-custom input[type=checkbox],
.radio-buttons-custom input[type=radio] {
  appearance: none;
  -webkit-appearance: none;
  background-color: white;
  border: 2px solid #238823;
  width: 18px;
  height: 18px;
  cursor: pointer;
  position: relative;
  margin-right: 8px;
  border-radius: 4px; /* Rounded corners for checkbox */
}

.radio-buttons-custom input[type=checkbox]:checked,
.radio-buttons-custom input[type=radio]:checked {
  background-color: #238823;
  border-color: #238823;
}

.radio-buttons-custom input[type=checkbox]:checked::after,
.radio-buttons-custom input[type=radio]:checked::after {
  content: \"\\2713\"; /* Checkmark */
  color: #238823;
  position: absolute;
  top: 0px;
  left: 3px;
  font-size: 12px;
}

.radio-buttons-custom input[type=radio] {
  border-radius: 50%; /* Make radios circular */
}

.radio-buttons-custom input[type=radio]:checked::after {
  content: \"\u2022\"; /* Dot for radio */
  top: -1px;
  left: 5px;
  font-size: 18px;
}
")),
               tags$style(HTML("
  /* Effet au survol de l'étiquette sélectionnée */
  .selectize-input.items.has-options.full.has-items .item:hover {
    background-color: #1e6f1d;
    box-shadow: 0 0 1px #238823;
  }

  /* Option active dans la liste déroulante (survolée par la souris) */
  .selectize-dropdown .option.active {
    background-color: #238823;
    color: #fff;
    box-shadow: 0 0 1px #238823;
  }

  /* Option déjà sélectionnée dans la liste déroulante */
  .selectize-dropdown .option.selected {
    background-color: #1e6f1d;
    color: #fff;
    box-shadow: 0 0 1px #238823;
  }

  /* === Contour vert quand le widget est actif (focus) === */
  .selectize-input.focus {
    border-color: #238823 !important;
    box-shadow: 0 0 0 1px #238823 !important;
  }
")),
  
               # Import Three.js and OrbitControls from CDN
               tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/three.js/r128/three.min.js"),
               tags$script(src = "https://cdn.jsdelivr.net/npm/three@0.128.0/examples/js/controls/OrbitControls.min.js"),
               tags$script(src = "globe.js"),  
               
               tags$style(HTML("
      #globe-container {
    width: 100%;
    height: 450px;
    margin-bottom: 20px;
    position: relative;
    display: flex;
    align-items: center;
    justify-content: center;
    border: none;
    background-color: transparent;
  }

  canvas {
    display: block;
    max-width: 100%;
    max-height: 100%;
    background-color: transparent;
  }

    ")),
               tags$style(HTML("
      .modal {
        display: flex !important;
        align-items: center !important;
        justify-content: center !important;
        min-height: 100vh !important;
      }
    ")),
               tags$script(HTML("
  Shiny.addCustomMessageHandler('reloadPage', function(message) {
    location.reload();
  });
"))
               
             ),
             
# ------------------------------------------------------------------------
# Main Page of UI
# ------------------------------------------------------------------------
               
               
             fluidRow(
               # The form
               column(
                 6,
                
                 p("Welcome to our migration recommender system, please fill the form with your informations, click", 
                   strong("'Search Destination'", style="color: darkgreen"), 
                   "button then go to the line highlighted in the table to explore your future destination.", style="text-align: justify"
                 ),
                 br(),
                 wellPanel(
                   fluidRow(
                     column(
                       6,
                       selectInput("continent", "Continent", choices = unique(result$continent)),
                       selectInput("reasons", "Migration Reason", choices = c("scholarship", "residence", "asylum", "refugee")),
                       div(class = "radio-buttons-custom",
                           radioButtons("gender", "Gender", choices = c("Male" = "m", "Female" = "f"), inline = TRUE)
                       )
                     ),
                     column(
                       6,
                       selectInput("country", "Country", choices = unique(result$country)),
                       sliderInput("age", "Age", min = 1, max = 100, value = 25, step = 1),
                       actionButton("run_globe", "Search Destination", icon = icon("globe"), class = "btn btn-success"),
                       actionButton("reset", "Reset", icon = icon("refresh"), class = "btn btn-success")
                     )
                   ),
                   h5("Recommendations per continent:", style = "text-decoration: underline; font-weight: bold;"),
                   uiOutput("recommended_by_continent"),
                   
                   h5("Country recommended globaly:", style = "text-decoration: underline; font-weight: bold;"),
                  uiOutput("recommended_global"),
                 )
               ),
               
               # The globe
               
               column(
                 6,
                 div(id = "globe-container")
               )
             ),
             fluidRow(    
             column(12, reactableOutput("continent_table"))
           )
           )),
# ----------------------------------------------------------
# Footer 
# ----------------------------------------------------------

  tabPanel(
  tags$footer(
    class = "navbar-fixed-bottom",
    style = "position: fixed; bottom: 0; left: 0; width: 100%; z-index: 1000;",
    "© 2025 Migration Recommander App - Conrad MESSINA - Submission to Posit Table Contest 2025"
  ))
)

# --------------------------------------------------------
# Server
# --------------------------------------------------------

server <- function(input, output, session) {
  
  # ✅ This is the correct way to declare reactive values
  country_info <- reactiveValues(data = NULL)
  
  # Render the main continent summary table using reactable
  output$continent_table <- renderReactable({
    
    reactable::reactable(
      continents_summary,  # Data frame with continent summaries
      columns = list(
        continent = colDef(
          name = "Continent",
          # Custom cell rendering: insert continent icon using a helper function
          cell = function(value) {
            get_continent_icon(value)
          },
          html = TRUE  # Allow HTML rendering inside cells
        ),
        nb_pays = colDef(name = "# Countries")  # Number of countries column
      ),
      # Details panel shows countries inside the selected continent
      details = function(index) {
        continent_selected <- continents_summary$continent[index]
        tagList(
          get_country_table(continent_selected, highlighted_countries())
        )
      },
      bordered = TRUE,
      highlight = TRUE,
      striped = TRUE,
      defaultPageSize = 5,
      compact = TRUE,
      defaultExpanded = TRUE  # Expand all rows by default
    )
  })
  
  
  # Show modal with age pyramid plot for selected country
  observeEvent(input$show_pyramid_country, {
    country_code <- input$show_pyramid_country
    
    showModal(modalDialog(
      title = paste("Age Pyramid -", countrycode(country_code, "iso2c", "country.name")),
      renderPlot({
        plot_pyramide_par_pays(pyramide_plot, country_code = country_code)
      }),
      easyClose = TRUE,
      size = "l",
      centered = TRUE 
    ))
  })
  
  # Show modal with national anthem audio for selected country
  observeEvent(input$play_anthem_country, {
    country_code <- input$play_anthem_country
    mp3_path <- paste0("anthem/", tolower(country_code), ".mp3")  # Path in www/anthem folder
    
    showModal(modalDialog(
      title = paste("National Anthem -", countrycode(country_code, "iso2c", "country.name")),
      tags$audio(
        src = mp3_path,
        type = "audio/mp3",
        controls = TRUE,
        autoplay = NA,
        style = "width: 100%;"
      ),
      easyClose = TRUE,
      size = "m",
      centered = TRUE 
    ))
  })
  
  # Show modal with capital city info and a Leaflet map
  observeEvent(input$show_capital, {
    code <- input$show_capital
    cap <- Capital %>% filter(iso2c == code)
    
    if (nrow(cap) == 1) {
      flag_url <- paste0("https://flagcdn.com/w160/", tolower(code), ".png")  # Official flag CDN
      
      showModal(modalDialog(
        title = tags$div(
          style = "display: flex; align-items: center; gap: 10px;",
          tags$span(paste("Capital of", cap$country)),
          tags$img(
            src = flag_url,
            alt = paste("Flag of", cap$country),
            style = "width: 35px; height: 20px;"
          )
        ),
        easyClose = TRUE,
        size = "l",
        centered = TRUE,
        footer = modalButton("Dismiss"),
        
        # Leaflet map below the title
        leafletOutput("capital_map", height = "400px")
      ))
      
      
      output$capital_map <- renderLeaflet({
        leaflet() %>%
          addTiles() %>%
          setView(lng = cap$longitude, lat = cap$latitude, zoom = 8) %>%
          addMarkers(lng = cap$longitude, lat = cap$latitude, label = cap$country)
      })
    }
  })
  
  # Show modal with embedded YouTube video tour of the capital city
  observeEvent(input$show_video_country, {
    code <- input$show_video_country
    video_link <- Capital %>% filter(iso2c == code) %>% pull(Places)
    
    if (!is.null(video_link) && nzchar(video_link)) {
      
      # Extract YouTube video ID from URL
      video_id <- sub(".*v=([a-zA-Z0-9_-]+).*", "\\1", video_link)
      
      embed_link <- sprintf("https://www.youtube.com/embed/%s?autoplay=1", video_id)  # Autoplay enabled
      
      showModal(modalDialog(
        title = paste("Wonders of :", countrycode(code, "iso2c", "country.name")),
        tags$iframe(
          width = "100%",
          height = "400px",
          src = embed_link,
          frameborder = "0",
          allow = "accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; autoplay",
          allowfullscreen = NA
        ),
        easyClose = TRUE,
        size = "l",
        centered = TRUE 
      ))
    }
  })
  
  # Show modal with AI assistant chat for additional country info
  observeEvent(input$show_info_modal, {
    code <- input$show_info_modal
    country_name <- countrycode(code, "iso2c", "country.name")
    
    # Find the selected country's data in the main dataset
    df_data <- countries
    row_data <- countries[countries$code_pays == code, ]
    
    # Save values temporarily in reactiveValues for later use
    country_info$data <- list(
      Country = country_name,
      Population = row_data$population_with_icon,
      Gender = row_data$gender,
      Literacy = row_data$`Literacy Rate`,
      Labor = row_data$labor_force,
      Location = row_data$location_icon,
      GDP = row_data$GDP,
      GDP_Cap = row_data$GDP_per_capita_PPP,
      GDP_Growth = tail(unlist(row_data$GDP_GR_sparkline), 1),
      Unemp = row_data$Unemployment,
      Exchange_Rate = row_data$Exchange_rate,
      Inflation = tail(unlist(row_data$Inflation_sparkline), 1)
    )
    
    # Helper function to safely round numbers or return "N/A"
    round_safe <- function(x, digits = 1) {
      if (length(x) == 0 || is.null(x)) {
        return("N/A")
      } else if (is.numeric(x)) {
        return(round(x, digits))
      } else if (suppressWarnings(!is.na(as.numeric(x)))) {
        return(round(as.numeric(x), digits))
      } else {
        return("N/A")
      }
    }
    
    # Build a summary sentence with known data about the country
    known_info <- paste0(
      "Here is what I already know about this country", ":\n",
      "flag, anthem, places to visit, population pyramid, gender proportion", "\n",
      "- Population: ", country_info$data$Population, "\n",
      "- Literacy rate: ", round_safe(country_info$data$Literacy*100, 1), "%\n",
      "- Labor force participation: ", round_safe(country_info$data$Labor, 1), "%\n",
      "- GDP: ", country_info$data$GDP, "\n",
      "- GDP per capita (PPP): ", country_info$data$GDP_Cap, "\n",
      "- GDP growth (latest): ", round_safe(country_info$data$GDP_Growth, 1), "%\n",
      "- Unemployment rate: ", round_safe(country_info$data$Unemp, 2), "%\n",
      "- Exchange rate (USD - local): ", round_safe(country_info$data$Exchange_Rate, 2), "\n",
      "- Inflation (latest): ", round_safe(country_info$data$Inflation, 1), "%\n"
    )
    
    # Initial prompt for the AI assistant when the modal opens
    initial_prompt <- paste0(
      "Hello! I'm exploring ", country_name, ".\n\n",
      known_info,
      "\nAs I'm searching for additional information, could you please answer some questions?"
    )
    
    showModal(modalDialog(
      title = paste("AI Assistant –", country_name),
      size = "l",
      easyClose = FALSE,
      footer = modalButton("Close"),
      
      tagList(
        uiOutput("chatbot_response"),  # Placeholder for AI response
        textAreaInput(
          inputId = "chatbot_prompt",
          label = NULL,
          value = initial_prompt,  
          rows = 10,
          width = "100%"
        ),
        actionButton("send_prompt", "Send")
      )
    ))
  })
  
  # Handle sending prompts to AI assistant and showing responses
  observeEvent(input$send_prompt, {
    req(input$chatbot_prompt)  # Ensure prompt is not empty
    
    code <- isolate(input$show_info_modal)  # Get current country code
    country_name <- countrycode(code, "iso2c", "country.name")
    
    user_prompt <- paste0(
      "User needs more information on ", country_name, 
      " (code: ", code, ") : ", input$chatbot_prompt
    )
    
    # When the globe is ready (custom event), start rotation (likely front-end JS)
    observeEvent(input$globe_ready, {
      session$sendCustomMessage("startGlobeRotation", list())
    })
    
    # Render AI response output based on prompt
    output$chatbot_response <- renderUI({
      response <- gemini(user_prompt)  # Call to AI (custom function)
      html_content <- commonmark::markdown_html(response)  # Convert markdown to HTML
      HTML(html_content)
    })
    
    # Clear the input text area after sending
    updateTextAreaInput(session, "chatbot_prompt", value = "")
    
  })
  
  # Print selected country code in the UI (for debug or display)
  output$country_info <- renderPrint({
    req(input$country_clicked)
    paste("Selected country:", input$country_clicked)
  })
  
  # Reactive expression to compute country recommendations based on user inputs
  results <- eventReactive(input$run_globe, {
    res <- suppressWarnings({
      recommander_pays(
        continent_filter = NULL,
        country_filter = input$country,
        age = input$age,
        sexe = input$gender,
        raison = input$reasons
      )
    })
  })
  
  # Show recommendations by continent in UI, formatted as HTML
  output$recommended_by_continent <- renderUI({
    req(results())
    
    df <- results()$top_par_continent
    
    # Filter out rows with missing continent or destination
    df <- df[!is.na(df$continent) & !is.na(df$destination), ]
    
    if(nrow(df) == 0) {
      tags$p("No country recommended by continent")
    } else {
      # Format each recommendation as bold continent and italic destination
      html_parts <- lapply(seq_len(nrow(df)), function(i) {
        paste0(
          "<b>", df$continent[i], "</b>: ",
          "<i>", df$destination[i], "</i>"
        )
      })
      
      # Join all parts with a semicolon separator
      texte_concatene <- paste(html_parts, collapse = " ; ")
      
      # Return as HTML for display
      HTML(texte_concatene)
    }
  })
  
  # Show the top recommended country globally
  output$recommended_global <- renderUI({
    req(results())
    
    top_global <- results()$top_par_continent$destination[1]
    
    if(length(top_global) == 0) {
      tags$p("No country recommended globally")
    } else {
      HTML(paste0("<p><i>", top_global, "</i></p>"))
    }
  })
  
  # Reactive values to track expanded continents and highlighted countries in UI
  expanded_continents <- reactiveVal(character(0))
  highlighted_countries <- reactiveVal(character(0))
  
  # Stop globe rotation and highlight the top recommended country on globe and map
  observeEvent(results(), {
    res <- results()
    req(res)
    
    if (!is.null(res$top_global$destination) && nzchar(res$top_global$destination)) {
      session$sendCustomMessage("stopGlobeRotation", list())  # Stop globe rotation (front-end)
      
      # Focus globe on recommended country with green highlight
      session$sendCustomMessage("focusGlobeOnCountry", list(
        destination = res$top_global$destination, 
        color = "#238823"
      ))
      
      # Fill country polygon with green on globe or map
      session$sendCustomMessage("fillCountryPolygon", list(
        destination = res$top_global$destination, 
        fillColor = "#238823"
      ))
    }
    
    if (!is.null(res$top_par_continent$destination)) {
      highlighted_countries(res$top_par_continent$destination)
    }
    if (!is.null(res$top_par_continent$continent)) {
      expanded_continents(res$top_par_continent$continent)
    }
  })
  
  # Update the list of countries in selectInput when continent is selected
  observeEvent(input$continent, {
    countries_filtered <- result$country[result$continent == input$continent]
    updateSelectInput(session, "country", choices = countries_filtered)
  })
  
  # Reset the app
  observeEvent(input$reset, {
    session$sendCustomMessage("reloadPage", list())
  })
  
}

# Run app
shinyApp(ui, server)
