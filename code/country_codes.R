# USCIS country-of-birth codes -> full names.

# Codes the 2001 list from AILA does not list, plus the handful whose 2001 name is
# wrong or is not the spelling these current data use. Entered by hand; each name
# below is one the ELIS records themselves use, except where noted.
uscis_country_codes_manual <- tribble(
  ~code   , ~country                           ,
  # -- absent from the 2001 list ------------------------------------------
  "SOSUD" , "South Sudan"                      , # independent 2011
  "ETMOR" , "East Timor"                       , # independent 2002; absent from these data
  "MONTE" , "Montenegro"                       , # independent 2006
  "KV"    , "Kosovo"                           , # independent 2008
  "SRBIA" , "Serbia"                           ,
  "SERBI" , "Serbia and Montenegro"            ,
  "BELAR" , "Belarus"                          ,
  "COTED" , "Côte d'Ivoire"                    ,
  "ANGUI" , "Anguilla"                         , # the list has ANQUI = Anguilla
  "FRGUI" , "French Guiana"                    , # the list has FGUIA = French Guiana
  "SAMOA" , "Samoa"                            , # the list has WSAMO = Western Samoa
  "STMAF" , "Saint Martin"                     ,
  "MAYOT" , "Mayotte"                          ,
  # -- 2001 name is wrong ---------------------------------------------------
  "YEMEN" , "Yemen"                            , # list says "Yemen-Sanaa"
  "PALES" , "Palestine, State of"              , # list says "Jordan"
  # -- 2001 name is an obsolete form the ELIS records have moved on from ----
  # each would otherwise strand records on a label ELIS barely uses:
  # "Kampuchea" has 1 ELIS record against 211 for Cambodia, "Byelarus" 2
  # against 581 for Belarus, and "Ivory Coast" 5 against 514 for Côte d'Ivoire
  "CAMBO" , "Cambodia"                         , # list says "Kampuchea"
  "BYELA" , "Belarus"                          , # list says "Byelarus"
  "IVORY" , "Côte d'Ivoire"                    , # list says "Ivory Coast"
  # -- 2001 name is right but not how these data spell it -------------------
  "CHINA" , "China"                            , # list says "China, People's Republic of"
  "TIBET" , "Tibet"                            , # list says "China, People's Republic of"
  "SKORE" , "South Korea"                      , # list says "Korea, South"
  "NKORE" , "North Korea"                      , # list says "Korea, North"
  "US"    , "United States"                    , # list says "USA"
  "USSR"  , "Soviet Union"                     , # list says "USSR"
  "DECON" , "Democratic Republic of the Congo" , # list says "Democratic Republic of Congo"
  "CONGO" , "Congo-Brazzaville"                , # list says "Congo"; DECON is the DRC
  "STVIN" , "St. Vincent-Grenadines"           , # list says "St. Vincent/Grenadines"
  "BRUNE" , "Brunei Darussalam"                , # list says "Brunei"
  "WGERM" , "West Germany"                     , # list says "Germany, West"
  "FSM"   , "Micronesia, Federated States of"  , # list says "Federated States of Micronesia"
  "TURKE" , "Turkiye"                          , # list says "Turkey"
  # the list calls all five of these "French Polynesia"; ELIS says "Polynesia"
  "POLYN" , "Polynesia"                        ,
  "FPOLY" , "Polynesia"                        ,
  "SOCIE" , "Polynesia"                        , # Society Islands
  "TUBAU" , "Polynesia"                        , # Tubuai Islands
  "AUSTR" , "Polynesia" # Austral Islands
)

# Codes in neither the list nor the table above, each an unmistakable
# data-entry variant of a code that is in them. They inherit whatever name
# the canonical code resolves to.
uscis_country_code_aliases <- tribble(
  ~code   , ~canonical ,
  # ELSAL = El Salvador
  "ESAL"  , "ELSAL"    ,
  "ELSA"  , "ELSAL"    ,
  "EL"    , "ELSAL"    ,
  "EL SA" , "ELSAL"    ,
  "ELAL"  , "ELSAL"    ,
  "ELASL" , "ELSAL"    ,
  "ELSA;" , "ELSAL"    ,
  "ELSAV" , "ELSAL"    ,
  "ELSLA" , "ELSAL"    ,
  "ESSAL" , "ELSAL"    ,
  "UELSA" , "ELSAL"    ,
  # HONDU = Honduras
  "HODNU" , "HONDU"    ,
  "HOUDU" , "HONDU"    ,
  "HONOD" , "HONDU"    ,
  "HONUD" , "HONDU"    ,
  "HONU"  , "HONDU"    ,
  "HOUND" , "HONDU"    ,
  "HOND"  , "HONDU"    ,
  "HODU"  , "HONDU"    ,
  "HNDU"  , "HONDU"    ,
  "HONDI" , "HONDU"    ,
  "2HOND" , "HONDU"    ,
  "H0NDU" , "HONDU"    ,
  "UHOND" , "HONDU"    ,
  "YORO"  , "HONDU"    , # a department of Honduras, not a country
  # NICAR = Nicaragua
  "NICA"  , "NICAR"    ,
  # PANAM = Panama
  "BANAM" , "PANAM"    ,
  # SOMAL = Somalia
  "SOMOL" , "SOMAL"    ,
  # US = United States
  "USA"   , "US"       ,
  "U.S"   , "US"       ,
  "U.S."  , "US"       ,
  # UNKNO = Unknown
  "UNKN"  , "UNKNO"    ,
  # AFRIC = Africa; WAFRI is presumed the same, at less precision
  "WAFRI" , "AFRIC"
)

# the 2001 list, with the manual layer applied over it
uscis_country_codes <-
  read_csv(
    "inputs/reference/ins_claims_country_codes_2001.csv",
    show_col_types = FALSE
  ) |>
  select(code, country = name) |>
  rows_upsert(uscis_country_codes_manual, by = "code")

# then the misspellings, each taking the name its canonical code resolved to
uscis_country_codes <-
  uscis_country_code_aliases |>
  left_join(uscis_country_codes, join_by(canonical == code)) |>
  select(code, country) |>
  bind_rows(uscis_country_codes) |>
  arrange(code)
