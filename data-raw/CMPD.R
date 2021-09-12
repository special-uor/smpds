## code to prepare `CMPD` dataset goes here
# The Chinese Modern Pollen Data by Ni Jian
cmpd_counts <- readxl::read_xlsx("inst/extdata/cmpd_counts.xlsx",
                                 sheet = 2,
                                 skip = 1) %>%
  dplyr::rename(entity_name = `Site name`,
                longitude = `Longitude (°E)`,
                latitude = `Latitude(°N)`,
                elevation = `Elevation (m)`) %>%
  dplyr::mutate(entity_name = entity_name %>%
                  stringr::str_squish()) %>%
  dplyr::slice(-c(1:2))
cmpd_metadata <- readxl::read_xlsx("inst/extdata/cmpd_metadata.xlsx",
                                   sheet = 2,
                                   skip = 1)  %>%
  dplyr::rename(site_name = `Site`,
                entity_name = `Site name`,
                longitude = `Longitude (°E)`,
                latitude = `Latitude(°N)`,
                elevation = `Elevation (m)`,
                entity_type = `Sample type`,
                data_source = `Data source`,
                data_type = `Data type`,
                publication = 21) %>% # 参考文献 (References)
  dplyr::mutate(ID_CMPD = seq_along(entity_name),
                entity_name = entity_name %>%
                  stringr::str_squish(),
                site_name = list(site_name, entity_name) %>%
                  purrr::pmap_chr(function(site_name, entity_name) {
                    site <- iconv(site_name, "latin1", "ASCII", sub = "") %>%
                      stringr::str_remove_all('\\*') %>%
                      stringr::str_remove_all("\\)") %>%
                      stringr::str_squish()
                    if (site != "")
                      entity_name <- entity_name %>%
                        stringr::str_remove_all(pattern = site)
                    entity_name %>%
                      stringr::str_squish() %>%
                      # stringr::str_remove_all("[-]*$") %>%
                      stringr::str_remove_all("HLJ-") %>%
                      stringr::str_remove_all("[0-9-\\*]*$") %>%
                      stringr::str_remove_all("25.*$") %>%
                      stringr::str_squish()
                  }),
                basin_size = NA,
                age_BP = NA,
                DOI = NA,
                source = "CMPD") %>%
  dplyr::mutate(ID_BIOME = tibble::tibble(latitude, longitude) %>%
                  smpds::parallel_extract_biome(buffer = 12000, cpus = 4) %>%
                  .$ID_BIOME) %>%
  progressr::with_progress()

cmpd_metadata2 <- cmpd_metadata %>%
  dplyr::mutate(ID_BIOME =
                  ifelse(site_name %>% stringr::str_detect("Qinghai Lake") &
                           is.na(ID_BIOME),
                         22,
                         ID_BIOME)) %>% # Steppe (ID_BIOME = 22)
  dplyr::mutate(ID_BIOME =
                  ifelse(stringr::str_detect(entity_type,
                                             "Marine|marine|Coastal|coastal") &
                           is.na(ID_BIOME),
                         -888888,
                         ID_BIOME))
cmpd_metadata2 %>%
  dplyr::filter(site_name %>% stringr::str_detect("Qinghai Lake"),
                is.na(ID_BIOME)) %>%
  dplyr::select(1:6, 8, 27) # Steppe (ID_BIOME = 22)

cmpd_metadata2 %>%
  dplyr::filter(is.na(ID_BIOME))

# cmpd_metadata2 %>% # Entities from marine/coastal sites with ID_BIOME
#   dplyr::filter(stringr::str_detect(entity_type,
#                                     "Marine|marine|Coastal|coastal")) %>%
#   dplyr::select(1:6, 8, 27) %>%
#   dplyr::filter(!is.na(ID_BIOME)) %>%
#   readr::write_excel_csv("~/Downloads/SMPDSv2/CMPD-marine-sites-with-biomes.csv", na = "")

# ------------------------------------------------------------------------------
# |                                 Clean data                                 |
# ------------------------------------------------------------------------------
## Load table with taxons
cmpd_taxons <- readr::read_csv("inst/extdata/cmpd_taxa.csv")
### Clean taxon names
cmpd_clean_taxon_names <- cmpd_taxons %>%
  dplyr::filter(action == "update")

### Nonsense. categories, to check numbers before deciding whether to delete
cmpd_clean_taxon_names_nonsense <- cmpd_taxons %>%
  dplyr::filter(action == "inspect")

### Irrelevant taxon (to be deleted)
cmpd_clean_taxon_names_delete <- cmpd_taxons %>%
  dplyr::filter(action == "delete")

## Create long version of the count data
cmpd_counts_long <- cmpd_counts %>%
  tidyr::pivot_longer(cols = -c(1:6), names_to = "taxon_name") %>%
  dplyr::filter(!is.na(value))

## Find entities with >1% of the total assemblage in the
## `cmpd_clean_taxon_names_nonsense` list
aux <- cmpd_counts_long %>%
  dplyr::filter(value > 0) %>% # Filter taxon with count = 0
  dplyr::group_by(entity_name) %>% # Group by entity_name
  dplyr::mutate(nonsense = ifelse(taxon_name %in%
                                    cmpd_clean_taxon_names_nonsense$taxon_name,
                                  value, 0) %>%
                  as.double() %>%
                  sum(na.rm = TRUE), # Find % of nonsense taxon
                total = sum(as.double(value), na.rm = TRUE)) %>%
  dplyr::filter(nonsense > 0) %>% # Filter entities without nonsense taxon
  dplyr::distinct(entity_name, .keep_all = TRUE) %>%
  dplyr::select(-taxon_name, -value)

entities_with_high_perc_of_nonsense_taxon <- aux %>%
  dplyr::filter(nonsense > 1)

entities_with_low_perc_of_nonsense_taxon <- aux %>%
  dplyr::filter(nonsense <= 1)

cmpd_counts_long2 <- cmpd_counts_long %>%
  # Filter entities with >1% of the total assemblage of nonsense taxon
  dplyr::filter(!(entity_name %in%
                    entities_with_high_perc_of_nonsense_taxon$entity_name)) %>%
  # Filter counts with <1% of the total assemblage of nonsense taxon
  dplyr::filter(
    !(entity_name %in%
        entities_with_low_perc_of_nonsense_taxon$entity_name) &
    !(taxon_name %in%
        cmpd_clean_taxon_names_nonsense$taxon_name)) %>%
  dplyr::filter(!(taxon_name %in% # Filter irrelevant taxo
                    cmpd_clean_taxon_names_delete$taxon_name)) %>%
  dplyr::left_join(smpds::clean_taxa() %>% # Update taxon names
                     dplyr::bind_rows(cmpd_clean_taxon_names) %>%
                     dplyr::distinct(),
                   by = "taxon_name") %>%
  dplyr::filter(action != "delete") %>%
  dplyr::rename(taxon_name_original = taxon_name,
                taxon_name = clean_name) %>%
  # Aggregate multiple records of entity_name - taxon_name pairs
  dplyr::group_by(entity_name, taxon_name) %>%
  dplyr::mutate(value = sum(as.double(value), na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(taxon_name = ifelse(is.na(taxon_name),
                                    taxon_name_original,
                                    taxon_name)) %>%
  dplyr::distinct(entity_name, taxon_name, .keep_all = TRUE)


cmpd_counts_wide <- cmpd_counts_long2 %>%
  dplyr::select(-taxon_name_original) %>%
  tidyr::pivot_wider(1:6, names_from = "taxon_name") %>%
  dplyr::select(1:6, order(colnames(.)[-c(1:6)]) + 6) # Sort the taxon_names alphabetically

CMPD_all <- cmpd_metadata2 %>%
  dplyr::select(ID_CMPD,
                source,
                site_name,
                entity_name,
                latitude,
                longitude,
                elevation,
                basin_size,
                entity_type,
                age_BP,
                ID_BIOME,
                publication,
                DOI
  ) %>%
  dplyr::right_join(cmpd_counts_wide %>%
                      dplyr::select(-c(1:2, 4:6)),
                    by = "entity_name")

# ------------------------------------------------------------------------------
# |                           Extract other subsets                            |
# ------------------------------------------------------------------------------
CMPD_ENTITIES <-
  c("Alashan-00-02", "Alashan-00-04", "Alashan-00-06", "Alashan-00-07",
    "Alashan-00-09", "Alashan-00-15", "Alashan-00-16", "Alashan-00-17",
    "Alashan-00-18", "Alashan-00-19", "Alashan-00-22", "Alashan-00-23",
    "Alashan-00-24", "Alashan-00-24.132", "Alashan-00-25", "Alashan-00-26",
    "Alashan-00-27", "Alashan-00-28", "Alashan-00-31", "Alashan-00-32",
    "Alashan-00-33", "Alashan-01-03.97", "Alashan-01-05.99", "Alashan-01-06",
    "Alashan-01-07", "Alashan-01-11", "Alashan-01-12", "Alashan-01-13",
    "Alashan-01-15", "Alashan-01-16", "Alashan-01-18", "Alashan-01-23",
    "Alashan-01-24.131", "Alashan-01-25", "Alashan-01-27", "Ali 1",
    "Ali 2", "Ali 3", "Ali 4", "Ali 5", "Angren", "Aqikekule", "Baihe",
    "Baihua Mt 01", "Baihua Mt 02", "Baihua Mt 03", "Baihua Mt 04",
    "Baihua Mt 05", "Baihua Mt 06", "Baihua Mt 07", "Baihua Mt 08",
    "Baihua Mt 09", "Baihua Mt 10", "Baiyangdian 1", "Baiyangdian 2",
    "Baiyangdian 3", "Baiyangdian 4", "Baiyangdian 5", "Baiyangdian 6",
    "Bakeyao", "Balikun Lake", "Bao Qing 3", "Bao Qing 4", "Bao Qing 5",
    "Bao Qing 6", "Bao Qing 7", "Baxi 1", "Baxi 2", "Baxi 3", "Baxi 4",
    "Baxi 5", "Baxi 6", "Baxi 7", "Beikan", "Beilikekule", "Beisu Lake",
    "Bodongquan 1", "Bodongquan 2", "Budala Palace", "Bunan Lake",
    "Caerhan", "Caidamo 1", "Caidamo 2", "Caidamo 3", "Cangumiao",
    "Caotan Lake", "Cengcuo", "Chagannuoer", "Chaiwopu", "Changbai Mt 1",
    "Changbai Mt 2", "Changbai Mt 3", "Changbai Mt 4", "Changbai Mt 5",
    "Changbai Mt 6", "Changbai_Mt 1", "Changbai_Mt 2", "Changbai_Mt 3",
    "Changbai_Mt 4", "Changxingdao", "Chanling 1", "Chanling 2",
    "Chaoli", "Chaozhou", "Chasuqi", "Chebaling 1", "Chebaling 2",
    "Chebaling 3", "Chebaling 4", "Chebaling 5", "China 001", "China 002",
    "China 003", "China 004", "China 005", "China 006", "China 007",
    "China 008", "China 009", "China 010", "China 011", "China 012",
    "China 013", "China 014", "China 015", "China 016", "China 017",
    "China 018", "China 019", "China 020", "China 021", "China 022",
    "China 023", "China 024", "China 025", "China 026", "China 027",
    "China 028", "China 029", "China 030", "China 031", "China 032",
    "China 033", "China 034", "China 035", "China 036", "China 038",
    "China 040", "China 041", "China 042", "China 043", "China 044",
    "China 045", "China 046", "China 047", "China 048", "China 049",
    "China 050", "China 051", "China 052", "China 053", "China 054",
    "China 055", "China 056", "China 057", "China 058", "China 059",
    "China 060", "China 061", "China 062", "China 063", "China 064",
    "China 065", "China 066", "China 067", "China 068", "China 069",
    "China 070", "China 071", "China 073", "China 074", "China 075",
    "China 076", "China 077", "China 078", "China 079", "China 080",
    "China 081", "China 082", "China 083", "China 084", "China 085",
    "China 086", "China 087", "China 088", "China 089", "China 090",
    "China 092", "China 093", "China 094", "China 095", "China 096",
    "China 097", "China 098", "China 099", "China 100", "China 101",
    "China 102", "China 103", "China 104", "China 105", "China 107",
    "China 108", "China 110", "China 111", "China 112", "China 113",
    "China 114", "China 115", "China 117", "China 118", "China 119",
    "China 120", "China 121", "China 122", "China 123", "China 124",
    "China 125", "China 126", "China 127", "China 128", "China 129",
    "China 130", "China 132", "China 133", "China 134", "China 135",
    "China 136", "China 137", "China 138", "China 139", "China 140",
    "China 141", "China 142", "China 143", "China 144", "China 145",
    "China 146", "China 147", "China North-HL03", "China North-JA01",
    "China North-PH05", "China North-PH07", "China North-PO03", "China North-QU02",
    "China North-QU03", "Chitsai Lake", "Chuanlong", "Cuiluan", "Cuoer 1",
    "Cuoer 2", "Cuomorong", "Cuona 1", "Cuona 2", "Cuona 3", "Cuona 4",
    "Dadianzi", "Dafeng Mt 1", "Dafeng Mt 2", "Dahaizi 1", "Dahaizi 2",
    "Daihai", "Daihai 99a", "Dajiuhu", "Dajiuhu 1", "Dalijia 1",
    "Dalijia 2", "Dalijia 3", "Daluoba", "Daming Mt 1", "Daming Mt 2",
    "Daming Mt 3", "Daming Mt 4", "Daxigou 01", "Daxigou 02", "Daxigou 03",
    "Daxigou 04", "Daxigou 05", "Daxigou 06", "Daxigou 07", "Daxigou 08",
    "Daxigou 09", "Daxigou 10", "Daxigou 11", "Daxigou 12", "Daxigou 13",
    "Daxigou 14", "Daxinganling Mt A1", "Daxinganling Mt A2", "Daxinganling Mt A3",
    "Daxinganling Mt CY1", "Daxinganling Mt CY2", "Daxinganling Mt CY3",
    "Daxinganling Mt CY4", "Daxinganling Mt DY1", "Daxinganling Mt GH1",
    "Daxinganling Mt GH2", "Daxinganling Mt GH3", "Daxinganling Mt GH4",
    "Daxinganling Mt GH5", "Daxinganling Mt H1", "Daxinganling Mt H2",
    "Daxinganling Mt H3", "Daxinganling Mt H4", "Daxinganling Mt H5",
    "Daxinganling Mt H6", "Daxinganling Mt HL1", "Daxinganling Mt HL2",
    "Daxinganling Mt KLC01", "Daxinganling Mt KLC02", "Daxinganling Mt KLC03",
    "Daxinganling Mt KLC04", "Daxinganling Mt KLC05", "Daxinganling Mt KLC06",
    "Daxinganling Mt KLC07", "Daxinganling Mt KLC08", "Daxinganling Mt KLC09",
    "Daxinganling Mt KLC10", "Daxinganling Mt KLC11", "Daxinganling Mt KLC12",
    "Daxinganling Mt KLC13", "Daxinganling Mt L1", "Daxinganling Mt L2",
    "Daxinganling Mt L3", "Daxinganling Mt L4", "Daxinganling Mt L6",
    "Daxinganling Mt L7", "Daxinganling Mt ML2", "Daxinganling Mt ML3",
    "Daxinganling Mt MT3", "Daxinganling Mt MT4", "Daxinganling Mt MT5",
    "Daxinganling Mt MT6", "Daxinganling Mt MY1", "Daxinganling Mt MY2",
    "Daxinganling Mt MY3", "Daxinganling Mt NC1", "Daxinganling Mt NC2",
    "Daxinganling Mt NC3", "Daxinganling Mt Q1", "Daxinganling Mt Q2",
    "Daxinganling Mt Q3", "Daxinganling Mt Q4", "Daxinganling Mt SL02",
    "Daxinganling Mt SL03", "Daxinganling Mt SL04", "Daxinganling Mt SL05",
    "Daxinganling Mt SL06", "Daxinganling Mt SL07", "Daxinganling Mt SL08",
    "Daxinganling Mt SL09", "Daxinganling Mt SL10", "Daxinganling Mt SL11",
    "Daxinganling Mt SL12", "Daxinganling Mt SL13", "Daxinganling Mt SL14",
    "Daxinganling Mt SS1", "Daxinganling Mt SS2", "Daxinganling Mt WLQ1",
    "Daxinganling Mt WLQ2", "Daxinganling Mt WRC01", "Daxinganling Mt WRC02",
    "Daxinganling Mt WRC03", "Daxinganling Mt WRC04", "Daxinganling Mt WRC05",
    "Daxinganling Mt WRC07", "Daxinganling Mt WRC08", "Daxinganling Mt WRC09",
    "Daxinganling Mt WRC10", "Daxinganling Mt WRC11", "Daxinganling Mt WRC12",
    "Daxinganling Mt WS1", "Daxinganling Mt WS2", "Daxinganling Mt XA",
    "Daxinganling Mt YTL1", "Daxinganling Mt YTL2", "Dayandong 1",
    "Dayaocun", "Dayawan 01", "Dayawan 02", "Dayawan 03", "Dayawan 04",
    "Dayawan 05", "Dayawan 06", "Dayawan 07", "Dayawan 08", "Dayawan 09",
    "Dayawan 10", "Dayawan 11", "Dayawan 12", "Dayawan 13", "Dayawan 14",
    "Dayawan 15", "Dayawan 16", "Dayawan 17", "Dayawan 18", "Dayawan 19",
    "Dayawan 20", "Dayawan 21", "Dayawan 22", "Dayawan 23", "Dayawan 24",
    "Dayawan 25", "Dayawan 26", "Dayawan 27", "Dayawan 28", "Dayawan 29",
    "Dayawan 30", "Dayawan 31", "Dayawan 32", "Dayawan 33", "Dayawan 34",
    "Dayawan 35", "Dayawan 36", "Dayawan 37", "Dayawan 38", "Dayawan 39",
    "Dayawan 40", "Dayawan 41", "Dayawan 42", "Dayawan 43", "Dayawan 44",
    "Dayawan 45", "Dayawan 46", "Dayawan 47", "Dayawan 48", "Dayawan 49",
    "Dayawan 50", "Dayawan 51", "Dianchi", "Dinghu 1", "Dinghu 2",
    "Dingnan", "Dingri 1", "Dongfuyechi", "Donghai 1", "Donghai 2",
    "Donghai 3", "Dongling Mt 01", "Dongling Mt 02", "Dongling Mt 03",
    "Dongling Mt 04", "Dongling Mt 05", "Dongling Mt 06", "Dongling Mt 07",
    "Dongling Mt 08", "Dongling Mt 09", "Dongling Mt 10", "Donglingshan",
    "Doupeng Mt 1", "Doupeng Mt 2", "Dulan 1", "Dulan 2", "Dunde",
    "Ejinaqi", "Erhai ES", "Erlucuo", "Fenghuang Mt 1", "Fenghuang Mt 2",
    "Fenghuang Mt 3", "Fenghuang Mt 4", "Fengning", "Fujian 1", "Fuzhou",
    "Gahai 1", "Gahai 2", "Gahai 3", "Gangou", "Gaoximage", "Geermo 1",
    "Geermo 2", "Gerencuo 1", "Gerencuo 2", "Gerencuo 3", "Glacier Boue",
    "Glacier Mousse", "Gonggouyan", "Gu hu 28", "Guangdongs 1", "Guangdongs 3",
    "Guangxi 01", "Guangxi 02", "Guangxi 03", "Guangxi 04", "Guangxi 05",
    "Guangxi 06", "Guangxi 07", "Guangxi 08", "Guangxi 09", "Guangxi 10",
    "Guangzhou", "Guanzhi Mt 1", "Guanzhi Mt 2", "Guiyang", "Guizhou",
    "Guizhou 1", "Guizhou 2", "Guizhou 3", "Guizhou 4", "Guizhou 5",
    "Guizhou s1", "Guizhou s3", "Guizhou s4", "Guizhou s5", "Guizhou s6",
    "Haerbaling", "Haideng Lake", "Hainandao 2", "Hainandao 3", "Haizhou Bay 01",
    "Haizhou Bay 02", "Haizhou Bay 03", "Haizhou Bay 04", "Haizhou Bay 05",
    "Haizhou Bay 06", "Haizhou Bay 07", "Haizhou Bay 08", "Haizhou Bay 09",
    "Haizhou Bay 10", "Haizhou Bay 11", "Haizhou Bay 12", "Haizhou Bay 13",
    "Haizhou Bay 14", "Haizhou Bay 15", "Haizhou Bay 16", "Haizhou Bay 17",
    "Haizhou Bay 18", "Hangcuo", "Hanhai Lake", "Hani_New", "Hanjiang 1",
    "Hanjiang 3", "Haoluku Lake", "Hdgutian 1", "Hdgutian 2", "Hdgutian 3",
    "Hdgutian 4", "Hdgutian 5", "Hdgutian 6", "Heishawan 1", "Heishiding 1",
    "Heishiding 2", "Hengkungaosu", "Heqiao", "Heqing", "Heyuan",
    "Hezuo 1", "Hezuo 2", "Hezuo 3", "Hezuo 4", "Hongkong 1", "Hongkong 2",
    "Hongkong 3", "Hongkong 4", "Hongkong 5", "Hongkong 6", "Hongkong 7",
    "Hongkong z02", "Hongkong z03", "Hongkong z04", "Hongkong z05",
    "Hongkong z06", "Hongkong z07", "Hongkong z08", "Hongkong z09",
    "Hongkong z10", "Hongkong z11", "Hongkong z12", "Hongkong z14",
    "Hongkong z15", "Hongkong z16", "Hongkong z17", "Hongkong z18",
    "Hongkong z20", "Hongkong z21", "Hongkong z22", "Hongkong z23",
    "Hongshen", "Hongyuan", "Hongyuan 01", "Hongyuan 02", "Hongyuan 03",
    "Hongyuan 04", "Hongyuan 05", "Hongyuan 06", "Hongyuan 07", "Hongyuan 08",
    "Hongyuan 09", "Hongyuan 10", "Huangjiapu", "Huangpu", "Huangshui",
    "Huaping 01", "Huaping 02", "Huaping 03", "Huaping 04", "Huaping 05",
    "Huaping 06", "Huaping 07", "Huaping 08", "Huaping 09", "Huaping 10",
    "Huaping 12", "Huaping 13", "Huaping 14", "Huaping 15", "Huaping 16",
    "Huaping 17", "Huaping 18", "Huaping 19", "Huaping 20", "Huaping 21",
    "Huaping 22", "Huaping 23", "Huidong 1", "Huidong 2", "Huitong",
    "Huma", "Hunan", "Hungshui 1", "Hungshui 2", "Hungshui 3", "Hungshui 4",
    "Hungshui 5", "Hungshui 6", "Hungshui 7", "Hutuo River 1", "Hutuo River 2",
    "Hutuo River 3", "Inner Mongolia 01", "Inner Mongolia 02", "Inner Mongolia 03",
    "Inner Mongolia 04", "Inner Mongolia 05", "Inner Mongolia 06",
    "Inner Mongolia 07", "Inner Mongolia 08", "Inner Mongolia 09",
    "Inner Mongolia 10", "Inner Mongolia 11", "Inner Mongolia 12",
    "Inner Mongolia 13", "Inner Mongolia 14", "Inner Mongolia 15",
    "Inner Mongolia 16", "Inner Mongolia 17", "Inner Mongolia 18",
    "Inner Mongolia C01", "Inner Mongolia C02", "Inner Mongolia C03",
    "Inner Mongolia C04", "Inner Mongolia C05", "Inner Mongolia C06",
    "Inner Mongolia C07", "Inner Mongolia C08", "Inner Mongolia C09",
    "Inner Mongolia C10", "Inner Mongolia C11", "Inner Mongolia C12",
    "Inner Mongolia C13", "Inner Mongolia C14", "Inner Mongolia C15",
    "Inner Mongolia DS01", "Inner Mongolia DS02", "Inner Mongolia DS03",
    "Inner Mongolia DS04", "Inner Mongolia DS05", "Inner Mongolia DS06",
    "Inner Mongolia DS07", "Inner Mongolia DS08", "Inner Mongolia DS09",
    "Inner Mongolia DS10", "Inner Mongolia DS11", "Inner Mongolia DS12",
    "Inner Mongolia DS13", "Inner Mongolia DS14", "Inner Mongolia DS15",
    "Inner Mongolia DS16", "Inner Mongolia DS17", "Inner Mongolia DS18",
    "Inner Mongolia MS01", "Inner Mongolia MS02", "Inner Mongolia MS03",
    "Inner Mongolia MS04", "Inner Mongolia MS05", "Inner Mongolia MS06",
    "Inner Mongolia MS07", "Inner Mongolia MS08", "Inner Mongolia MS09",
    "Inner Mongolia MS10", "Inner Mongolia MS11", "Inner Mongolia MS12",
    "Inner Mongolia MS13", "Inner Mongolia MS15", "Inner Mongolia MS16",
    "Inner Mongolia MS17", "Inner Mongolia MS18", "Inner Mongolia MS19",
    "Inner Mongolia MS20", "Inner Mongolia TS01", "Inner Mongolia TS02",
    "Inner Mongolia TS03", "Inner Mongolia TS04", "Inner Mongolia TS05",
    "Inner Mongolia TS06", "Inner Mongolia TS07", "Inner Mongolia TS08",
    "Inner Mongolia TS09", "Inner Mongolia TS10", "Inner Mongolia TS11",
    "Inner Mongolia TS12", "Inner Mongolia TS13", "Inner Mongolia TS14",
    "Inner Mongolia TS15", "Inner Mongolia TS16", "Inner Mongolia TS17",
    "Inner Mongolia TS18", "Inner Mongolia TS19", "Inner Mongolia TS20",
    "Inner Mongolia TS21", "Inner Mongolia TS22", "Inner Mongolia TS23",
    "Inner Mongolia TS24", "Inner Mongolia TS25", "Inner Mongolia TS26",
    "Inner Mongolia TS27", "Inner Mongolia TS28", "Inner Mongolia TS29",
    "Inner Mongolia TS30", "Inner Mongolia TS32", "Inner Mongolia TS33",
    "Inner Mongolia TS34", "Inner Mongolia TS35", "Inner Mongolia TS36",
    "Inner Mongolia TS37", "Inner Mongolia TS38", "Inner Mongolia TS39",
    "Inner Mongolia TS40", "Inner Mongolia TS41", "Inner Mongolia TS42",
    "Inner Mongolia TS43", "Inner Mongolia TS44", "Inner Mongolia TS45",
    "Inner Mongolia TS46", "Inner Mongolia TS50", "Inner Mongolia TS51",
    "Inner Mongolia TS52", "Inner Mongolia TS53", "Inner Mongolia TS54",
    "Inner Mongolia TS55", "Inner Mongolia TS56", "Inner Mongolia TS57",
    "Inner Mongolia TS58", "Inner Mongolia TS59", "Inner Mongolia TS60",
    "Inner Mongolia TS62", "Inner Mongolia TS63", "Inner Mongolia TS64",
    "Inner Mongolia TS65", "Inner Mongolia TS66", "Inner Mongolia TS68",
    "Inner Mongolia TS69", "Inner Mongolia TS70", "Inner Mongolia TS71",
    "Inner Mongolia TS72", "Inner Mongolia TS73", "Inner Mongolia TS74",
    "Inner Mongolia TS75", "Inner Mongolia TS76", "Inner Mongolia TS77",
    "Inner Mongolia TS78", "Inner Mongolia TS79", "Inner Mongolia TS80",
    "Inner Mongolia TS81", "Inner Mongolia TS82", "Inner Mongolia TS83",
    "Inner Mongolia TS84", "Inner Mongolia TS85", "Inner Mongolia TS86",
    "Inner Mongolia TS87", "Inner Mongolia TS88", "Inner Mongolia TS89",
    "Inner Mongolia TS90", "Inner Mongolia TS91", "Inner Mongolia TS92",
    "Inner Mongolia TS93", "Inner Mongolia TS94", "Inner Mongolia TS95",
    "Inner Mongolia TS96", "Jiajihe", "Jiangcun", "Jianggao", "Jiangjunpaozi",
    "Jiangsu", "Jinggang Mt 1", "Jinggang Mt 2", "Jinggang Mt 3",
    "Jinggang Mt 4", "Jinggang Mt 5", "Jinggang Mt 6", "Jinxiu",
    "Jiuxian Mt", "Junshan", "Kaili 1", "Kaili 2", "Kaitong", "Kekexili 1",
    "Kekexili 2", "Kekexili 3", "Kekexili 4", "Kekexili 5", "Kekexili 6",
    "Kekexili 7", "Keligao", "Kenli 1", "Kenli 2", "Kenli 3", "Kunlun 1",
    "Kunlun 2", "Kunlun Mt 01", "Kunlun Mt 02", "Kunlun Mt 03", "Kunlun Mt 04",
    "Kunlun Mt 05", "Kunlun Mt 06", "Kunlun Mt 07", "Kunlun Mt 08",
    "Kunlun Mt 09", "Kunlun Mt 10", "Lanzhou 1", "Lanzhou 2", "Lanzhou 3",
    "Lechang", "Liangtian", "Liangwang Mt 2", "Liangwang Mt z1",
    "Liangwang Mt z2", "Liangwang Mt z3", "Liangwang Mt z4", "Liangwang Mt z5",
    "Liangwang Mt z6", "Lianyungang", "Lingao", "Lingdingyang", "Liuchanghe",
    "Liupanshui", "Liushuwan", "Loess Plateau 02", "Loess Plateau 03",
    "Loess Plateau 08", "Loess Plateau 09", "Loess Plateau 11", "Loess Plateau 13",
    "Loess Plateau 15", "Loess Plateau 15-1", "Loess Plateau 16",
    "Loess Plateau 18", "Loess Plateau 18-1", "Loess Plateau 22",
    "Loess Plateau 29", "Loess Plateau 30", "Loess Plateau 31", "Loess Plateau 34",
    "Loess Plateau 36", "Loess Plateau 37", "Loess Plateau 38", "Loess Plateau 41",
    "Loess Plateau 44", "Loess Plateau 45", "Loess Plateau 46", "Loess Plateau 47",
    "Loess Plateau 52", "Loess Plateau 54", "Loess Plateau 56", "Loess Plateau 57",
    "Loess Plateau 58", "Loess Plateau 60", "Longgan Lake", "Longnan",
    "Longquan Lake", "Lopei Mt 0540", "Lopei Mt 0560", "Lopei Mt 0580",
    "Lopei Mt 0600", "Lopei Mt 0615", "Lopei Mt 0700", "Lopei Mt 0800",
    "Lopei Mt 0850", "Lopei Mt 0870", "Lopei Mt 0872", "Lopei Mt 0874",
    "Lopei Mt 0876", "Lopei Mt 0878", "Lopei Mt 0880", "Lopei Mt 1090",
    "Lopei Mt 1095", "Lopei Mt 1170", "Lopei Mt 1175", "Lopei Mt 1195",
    "Lopei Mt 1200", "Lopei Mt 1220", "Lopei Mt 1240", "Lopei Mt 1260",
    "Lopei Mt 1280", "Lopei Mt 1285", "Lopei Mt 1290", "Lopei Mt 1295",
    "Lopei Mt 1300", "Lopei Mt 1320", "Lopei Mt 1340", "Lopei Mt 1360",
    "Lopei Mt 1400", "Lopei Mt 1420", "Lopei Mt 1460", "Lopei Mt 1480",
    "Luofu Mt 1", "Luofu Mt 2", "Luofu Mt 3", "Luofu Mt 4", "Luofu Mt 5",
    "Luoqui 1", "Luoqui 2", "Lushan Mt 1", "Lushan Mt 2", "Lushan Mt 3",
    "Lushan Mt 4", "Lushan Mt 5", "Lushan Mt 6", "Luxun Lake", "Maer Lake SC-1",
    "Maili 1", "Maili 2", "Maili 3", "Manasi Lake", "Maofeng Mt 1",
    "Maofeng Mt 2", "Maohebei", "Maolan Mt", "Menghai 1", "Menghai 2",
    "Menghai 3", "Minshan", "Muhuaheke", "Namocuo 1", "Namocuo 2",
    "Namocuo 3", "Namocuo 4", "Nanbanjiang", "Nangong", "Nanhai",
    "Nanhuasi", "Nanjing 1", "Nanjing 2", "Nanjing a", "Nanjing b",
    "Nanjing c", "Nanjing d", "Nanjing e", "Nanjing f", "Nanjing g",
    "Nanping 1", "Nanping 2", "Nanping 3", "Nanshan", "Napahai 34",
    "Naqu 1", "Naqu 2", "Naqu 3", "Naqu 4", "Naripingcuo", "NECT 01",
    "NECT 02", "NECT 03", "NECT 04", "NECT 05", "NECT 06", "NECT 07",
    "NECT 08", "NECT 09", "NECT 10", "NECT 11", "NECT 12", "NECT 13",
    "NECT 14", "NECT 15", "NECT 16", "NECT 17", "NECT 18", "NECT 19",
    "NECT 20", "NECT 21", "NECT 22", "NECT 23", "NECT 24", "NECT 25",
    "NECT 26", "NECT 27", "NECT 28", "NECT 29", "NECT 30", "NECT 31",
    "NECT 32", "NECT 33", "NECT 34", "Niangziguan", "Nianqing 1",
    "Nianqing 2", "Nianqing 3", "Nianqing 4", "Nianqing 5", "Nianqing 6",
    "Nianqing 7", "Ningbo 1", "Ningbo 2", "Ningbo 3", "Ningbo 4",
    "Paipu", "Panzhihua 1", "Panzhihua 2", "Panzhihua 3", "Payango",
    "Pengcuo 1", "Pengcuo 2", "Pingbian", "Pingshi", "Pojianghaizi",
    "Poyang Lake 1", "Poyang Lake 2", "Punandian", "Qidong", "Qinghai 01",
    "Qinghai 02", "Qinghai 03", "Qinghai 04", "Qinghai 05", "Qinghai 06",
    "Qinghai 07", "Qinghai 08", "Qinghai 09", "Qinghai 10", "Qinghai 11",
    "Qinghai 12", "Qinghai 13", "Qinghai 14", "Qinghai 15", "Qinghai 16",
    "Qinghai 17", "Qinghai 18", "Qinghai 19", "Qinghai 20", "Qinghai 21",
    "Qinghai 22", "Qinghai 23", "Qinghai 24", "Qinghai 25", "Qinghai 26",
    "Qinghai 27", "Qinghai 28", "Qinghai 29", "Qinghai 30", "Qinghai 31",
    "Qinghai 32", "Qinghai 33", "Qinghai 34", "Qinghai 35", "Qinghai 36",
    "Qinghai 37", "Qinghai 38", "Qinghai 39", "Qinghai 40", "Qinghai 41",
    "Qinghai 42", "Qinghai 43", "Qinghai 44", "Qinghai 45", "Qinghai 46",
    "Qinghai 47", "Qinghai 48", "Qinghai 49", "Qinghai 50", "Qinghai 51",
    "Qinghai 52", "Qinghai 53", "Qinghai 54", "Qinghai 55", "Qinghai 56",
    "Qinghai 57", "Qinghai 58", "Qinghai 59", "Qinghai 60", "Qinghai 61",
    "Qinghai 62", "Qinghai 63", "Qinghai 64", "Qinghai 65", "Qinghai 66",
    "Qinghai 67", "Qinghai 68", "Qinghai 69", "Qinghai 70", "Qinghai 71",
    "Qinghai 72", "Qinghai 73", "Qinghai 74", "Qinghai 75", "Qinghai 76",
    "Qinghai 77", "Qinghai 78", "Qinghai 79", "Qinghai 80", "Qinghai 81",
    "Qinghai 82", "Qinghai 83", "Qinghai 84", "Qinghai 85", "Qinghai 86",
    "Qinghai 87", "Qinghai Lake 1", "Qinghai Lake 2", "Qinghai Lake 3",
    "Qinghai Lake 4", "Qinghai Lake 5", "Qinghai Lake 6", "Qinghai_1",
    "Qinghai_2", "Qinghai_3", "Qinghai-Tibet 38", "Qinghai-Tibet 42",
    "Qinghai-Tibet 61", "Qinglan", "Qingshuigou 05", "Qingshuigou 22",
    "Qujiang", "Reshuitang", "Reyueshan 1", "Reyueshan 2", "Rongchi 1",
    "Rongchi 2", "Ruoergai", "Ruoergai RM", "Ruoguo Glacier 01",
    "Ruoguo Glacier 02", "Ruoguo Glacier 04", "Ruoguo Glacier 05",
    "Ruoguo Glacier 06", "Ruoguo Glacier 07", "Ruoguo Glacier 08",
    "Ruoguo Glacier 09", "Ruoguo Glacier 10", "RuoguoG Hugong 1",
    "RuoguoG Hugong 2", "RuoguoG Hugong 3", "RuoguoG Hugong 4", "RuoguoG Hugong 5",
    "RuoguoG Hugong 6", "RuoguoG Hugong 7", "Ruomohong 1", "Ruomohong 2",
    "Ruyuan", "Ruyuan 2", "San Jiang 01", "San Jiang 02", "San Jiang 03",
    "San Jiang 04", "San Jiang 07", "San Jiang 11", "Sandaolaoyefu 01",
    "Sandaolaoyefu 02", "Sandaolaoyefu 03", "Sandaolaoyefu 04", "Sandaolaoyefu 05",
    "Sandaolaoyefu 06", "Sandaolaoyefu 07", "Sandaolaoyefu 08", "Sandaolaoyefu 09",
    "Sandaolaoyefu 10", "Sandu", "Sanming 1", "Sanming 2", "Sanming 3",
    "Sanya 1", "Sanya 2", "Sanya 4", "Seling Co", "Shanghang 1",
    "Shanghang 3", "Shanghang 4", "Shanghang 5", "Shennongjia", "Shiwandashan 1",
    "Shiwandashan 2", "Shiwandashan 3", "Shiwandashan 4", "Shiwandashan 5",
    "Shiwandashan 6", "Shixing 1", "Shixing 2", "Shixing 3", "Shiyang River 01",
    "Shiyang River 02", "Shiyang River 03", "Shiyang River 04", "Shiyang River 05",
    "Shiyang River 06", "Shiyang River 07", "Shiyang River 08", "Shiyang River 09",
    "Shiyang River 10", "Shiyang River 11", "Shiyang River 12", "Shiyang River 13",
    "Shiyang River 14", "Shiyang River 15", "Shiyang River 16", "Shiyang River 17",
    "Shiyang River 18", "Shiyang River 19", "Shiyang River 20", "Shiyang River 21",
    "Shiyang River 22", "Shiyang River 23", "Shiyang River 24", "Shiyang River 25",
    "Shiyang River 26", "Shiyang River 27", "Shiyang River 28", "Shiyang River 29",
    "Shiyang River 30", "Shiyang River 31", "Shiyang River 32", "Shiyang River 33",
    "Shiyang River 34", "Shiyang River 35", "Shiyang River 36", "Shiyang River 37",
    "Shiyang River 38", "Shiyang River 39", "Shiyang River 40", "Shiyang River 41",
    "Shiyang River-02-01", "Shiyang River-02-02", "Shiyang River-02-03",
    "Shiyang River-02-04", "Shiyang River-02-05", "Shiyang River-02-06",
    "Shiyang River-02-07", "Shiyang River-02-08", "Shiyang River-02-09",
    "Shiyang River-02-10", "Shiyang River-02-11", "Shiyang River-02-12",
    "Shiyang River-02-13", "Shiyang River-02-14", "Shiyang River-02-15",
    "Shiyang River-02-16", "Shiyang River-02-17", "Shiyang River-02-18",
    "Shiyang River-02-19", "Shiyang River-02-20", "Shiyang River-02-21",
    "Shiyang River-02-22", "Shiyang River-02-23", "Shiyang River-02-24",
    "Shiyang River-02-25", "Shiyang River-02-26", "Shiyang River-02-27",
    "Shiyang River-02-29", "Shiyang River-02-30", "Shiyang River-02-31",
    "Shiyang River-02-32", "Shiyang River-02-33", "Shiyang River-02-34",
    "Shiyang River-02-35", "Shuidong 1", "Shuidong 2", "Suichuan",
    "Sun-moon Lake", "T114", "Taibai Mt 1", "Taibai Mt 2", "Taibai Mt 3",
    "Taibai Mt 4", "Taibei", "Taidong", "Tainan", "Taixi", "Tanggula 1",
    "Tanggula 2", "Tanggula 3", "Tanggula 4", "Tanghongling", "Tianbaoyan 1",
    "Tianbaoyan 2", "Tianbaoyan 3", "Tianbaoyan 4", "Tianmu Mt 1",
    "Tianmu Mt 2", "Tianmu Mt 3", "Tianmu Mt 4", "Tianmu Mt 5", "Tianshan Mt 1",
    "Tianshan Mt 2", "Tianshan Mt 3", "Tianshan Mt 4", "Tianshan Mt 5",
    "Tianshan Mt 6", "Tianshan Mt 7", "Tianshan North 01", "Tianshan North 02",
    "Tianshan North 03", "Tianshan North 04", "Tianshan North 05",
    "Tianshan North 06", "Tianshan North 07", "Tianshan North 08",
    "Tianshan North 09", "Tianshan North 10", "Tianshan North 11",
    "Tianshan North 12", "Tianshan North 13", "Tianshan North 14",
    "Tianshan North 15", "Tianshan North 16", "Tianshan North 17",
    "Tianshan North 18", "Tianshan North 19", "Tianshan North 20",
    "Tianshan North 21", "Tianshan North 22", "Tianshan North 23",
    "Tianshan North 24", "Tianshan North 25", "Tianshan North 26",
    "Tianshan North 27", "Tianshan North 28", "Tianshan North 29",
    "Tianshan North 30", "Tianshan North 31", "Tianshan North 32",
    "Tianshan North 33", "Tianshan North 34", "Tianshan North 35",
    "Tianshan North 36", "Tianshan North 37", "Tianshan North 38",
    "Tianshan North 39", "Tianshan North 40", "Tianshan North 41",
    "Tianshan North 42", "Tianshan North 43", "Tianshan North 44",
    "Tianshan North 45", "Tianshan North 46", "Tianshan North 47",
    "Tianshan North 48", "Tianshan North 49", "Tianshan North 50",
    "Tianshan North 51", "Tianshan North 52", "Tianshan North 53",
    "Tianshan North 54", "Tianshan North 55", "Tianshan North 56",
    "Tianshan North 57", "Tianshan North 58", "Tianshan North 59",
    "Tianshan North 60", "Tianshan North 61", "Tianshan North 62",
    "Tianshan North 63", "Tianshan North 64", "Tianshan North 65",
    "Tianshan North 66", "Tianshan North 67", "Tianshan North 68",
    "Tianshan North 69", "Tianshan North 70", "Tianshan North 71",
    "Tianshan North 72", "Tianshan North 73", "Tianshan North 74",
    "Tianshan North 75", "Tianshan North 76", "Tianshan North 77",
    "Tianshan North 78", "Tianshan North 79", "Tianshan North 80",
    "Tianshan North 81", "Tianshan North 82", "Tianshan South 01",
    "Tianshan South 02", "Tianshan South 03", "Tianshan South 04",
    "Tianshan South 05", "Tianshan South 06", "Tianshan South 07",
    "Tianshan South 08", "Tianshan South 09", "Tianshan South 10",
    "Tianshan South 11", "Tianshan South 12", "Tianshi Lac", "Tibet 01",
    "Tibet 02-1", "Tibet 03", "Tibet 04", "Tibet 05", "Tibet 06-1",
    "Tibet 07", "Tibet 08", "Tibet 09", "Tibet 10-1", "Tibet 11",
    "Tibet 12", "Tibet 13", "Tibet 14", "Tibet 15", "Tibet 16", "Tibet 17",
    "Tibet 18", "Tibet 19-1", "Tibet 20", "Tibet 21", "Tibet 22",
    "Tibet 23", "Tibet 24", "Tibet 25", "Tibet 26", "Tibet 27", "Tibet 28",
    "Tibet 29", "Tibet 30", "Tibet 31", "Tibet 32", "Tibetan Plateau 02-154",
    "Tibetan Plateau CTP-02", "Tibetan Plateau CTP-03", "Tibetan Plateau CTP-04",
    "Tibetan Plateau CTP-05", "Tibetan Plateau CTP-06", "Tibetan Plateau CTP-07",
    "Tibetan Plateau CTP-08", "Tibetan Plateau CTP-09", "Tibetan Plateau CTP-11",
    "Tibetan Plateau CTP-12", "Tibetan Plateau CTP-13", "Tibetan Plateau CTP-14",
    "Tibetan Plateau CTP-15", "Tibetan Plateau CTP-16", "Tibetan Plateau CTP-17",
    "Tibetan Plateau CTP-19", "Tibetan Plateau CTP-20", "Tibetan Plateau CTP-21",
    "Tibetan Plateau CTP-25", "Tibetan Plateau CTP-26", "Tibetan Plateau CTP-27",
    "Tibetan Plateau CTP-28", "Tibetan Plateau CTP-29", "Tibetan Plateau CTP-30",
    "Tibetan Plateau CTP-31", "Tibetan Plateau CTP-32", "Tibetan Plateau CTP-33",
    "Tibetan Plateau CTP-34", "Tibetan Plateau CTP-35", "Tibetan Plateau CTP-36",
    "Tibetan Plateau CTP-37", "Tibetan Plateau CTP-38", "Tibetan Plateau CTP-39",
    "Tibetan Plateau CTP-40", "Tibetan Plateau DC-U-6", "Tibetan Plateau ER-U-1",
    "Tibetan Plateau GA", "Tibetan Plateau KU-U-5", "Tibetan Plateau MD-U-1",
    "Tibetan Plateau MIHA-2", "Tibetan Plateau MIHA-4", "Tibetan Plateau MIHA-8",
    "Tibetan Plateau MIY-01", "Tibetan Plateau MIY-02", "Tibetan Plateau MIY-03",
    "Tibetan Plateau MIY-09", "Tibetan Plateau MIY-10", "Tibetan Plateau MIY-11",
    "Tibetan Plateau MIY-12", "Tibetan Plateau MIY-13", "Tibetan Plateau MIY-14",
    "Tibetan Plateau MIY-15", "Tibetan Plateau MIY-16", "Tibetan Plateau MIY-17",
    "Tibetan Plateau MIY-18", "Tibetan Plateau MIY-19", "Tibetan Plateau MIY-20",
    "Tibetan Plateau MIY-21", "Tibetan Plateau MIY-22", "Tibetan Plateau MIY-23",
    "Tibetan Plateau MIY-25", "Tibetan Plateau MIY-27", "Tibetan Plateau MIY-29",
    "Tibetan Plateau MIY-30", "Tibetan Plateau MIY-31", "Tibetan Plateau MIY-35",
    "Tibetan Plateau MIY-36", "Tibetan Plateau MIY-37", "Tibetan Plateau MIY-38",
    "Tibetan Plateau MIY-39", "Tibetan Plateau MIY-40", "Tibetan Plateau MIY-41",
    "Tibetan Plateau MIY-42", "Tibetan Plateau MIY-43", "Tibetan Plateau MIY-44",
    "Tibetan Plateau MIY-46", "Tibetan Plateau MIY-48", "Tibetan Plateau MIY-49",
    "Tibetan Plateau MIY-50", "Tibetan Plateau MIY-51", "Tibetan Plateau MIY-52",
    "Tibetan Plateau MIY-53", "Tibetan Plateau MIY-54", "Tibetan Plateau NB",
    "Tibetan Plateau NE1", "Tibetan Plateau NE2", "Tibetan Plateau NE3",
    "Tibetan Plateau NE4", "Tibetan Plateau NE5", "Tibetan Plateau NE6",
    "Tibetan Plateau S-01", "Tibetan Plateau S-02", "Tibetan Plateau S-03",
    "Tibetan Plateau S-04", "Tibetan Plateau S-05", "Tibetan Plateau S-06",
    "Tibetan Plateau S-07", "Tibetan Plateau S-08", "Tibetan Plateau S-09",
    "Tibetan Plateau S-10", "Tibetan Plateau S-11", "Tibetan Plateau S-12",
    "Tibetan Plateau S-13", "Tibetan Plateau S-14", "Tibetan Plateau S-15",
    "Tibetan Plateau S-16", "Tibetan Plateau S-17", "Tibetan Plateau S-19",
    "Tibetan Plateau S-20", "Tibetan Plateau S-21", "Tibetan Plateau S-22",
    "Tibetan Plateau S-24", "Tibetan Plateau S-25", "Tibetan Plateau S-26",
    "Tibetan Plateau TS01-01", "Tibetan Plateau TS01-02", "Tibetan Plateau TS01-03",
    "Tibetan Plateau TS01-04", "Tibetan Plateau TS01-05", "Tibetan Plateau TS01-06",
    "Tibetan Plateau TS01-07", "Tibetan Plateau TS93-01", "Tibetan Plateau TS93-02",
    "Tibetan Plateau TS93-03", "Tibetan Plateau TS93-04", "Tibetan Plateau TS93-06",
    "Tibetan Plateau TS93-07", "Tibetan Plateau TS93-08", "Tibetan Plateau TS93-09",
    "Tibetan Plateau TS93-10", "Tibetan Plateau TS93-11", "Tibetan Plateau TS93-12",
    "Tibetan Plateau TS93-13", "Tibetan Plateau TS93-14", "Tibetan Plateau TS93-16",
    "Tibetan Plateau TS93-17", "Tibetan Plateau TS93-18", "Tibetan Plateau TS93-19",
    "Tibetan Plateau TS93-21", "Tibetan Plateau TS93-22", "Tibetan Plateau TS93-23",
    "Tibetan Plateau TS93-24", "Tibetan Plateau TS93-25", "Tibetan Plateau TS93-26",
    "Tibetan Plateau TS93-27", "Tibetan Plateau TS93-28", "Tibetan Plateau TS93-29",
    "Tibetan Plateau TS93-30", "Tibetan Plateau TS93-31", "Tibetan Plateau TS93-32",
    "Tibetan Plateau TS93-33", "Tibetan Plateau TS93-34", "Tibetan Plateau TS93-35",
    "Tibetan Plateau TS93-36", "Tibetan Plateau TS93-37", "Tibetan Plateau TS93-38",
    "Tibetan Plateau TS93-39", "Tibetan Plateau TS93-40", "Tibetan Plateau TS93-41",
    "Tibetan Plateau TS93-42", "Tibetan Plateau TS93-43", "Tibetan Plateau TS93-44",
    "Tibetan Plateau TS93-45", "Tibetan Plateau TS93-46", "Tibetan Plateau TS93-47",
    "Tibetan Plateau TS93-48", "Tibetan Plateau TS93-49", "Tibetan Plateau TS93-50",
    "Tibetan Plateau TS93-51", "Tibetan Plateau TS93-52", "Tibetan Plateau TS93-53",
    "Tibetan Plateau TS93-54", "Tibetan Plateau TS93-55", "Tibetan Plateau TS93-57",
    "Tibetan Plateau TS93-58", "Tibetan Plateau TS93-59", "Tibetan Plateau TS93-61",
    "Tibetan Plateau TS93-62", "Tibetan Plateau TS93-64", "Tibetan Plateau TS93-65",
    "Tibetan Plateau TS93-66", "Tibetan Plateau TS93-67", "Tibetan Plateau TS93-68",
    "Tibetan Plateau TS93-69", "Tibetan Plateau TS93-70", "Tibetan Plateau TS93-71",
    "Tibetan Plateau TS93-72", "Tibetan Plateau TS93-74", "Tibetan Plateau TS93-75",
    "Tibetan Plateau TS93-76", "Tibetan Plateau TS93-78", "Tibetan Plateau TS93-79",
    "Tibetan Plateau TS93-80", "Tibetan Plateau TS93-81", "Tibetan Plateau TS93-82",
    "Tibetan Plateau TS93-83", "Tibetan Plateau TS93-84", "Tibetan Plateau TS93-85",
    "Tibetan Plateau TS93-86", "Tibetan Plateau TS93-87", "Tibetan Plateau TS94-01",
    "Tibetan Plateau TS94-02", "Tibetan Plateau TS94-03", "Tibetan Plateau TS94-04",
    "Tibetan Plateau TS94-05", "Tibetan Plateau TS94-06", "Tibetan Plateau TS94-07",
    "Tibetan Plateau TS94-08", "Tibetan Plateau TS94-09", "Tibetan Plateau TS94-11",
    "Tibetan Plateau TS94-12", "Tibetan Plateau TS94-13", "Tibetan Plateau TS94-14",
    "Tibetan Plateau TS94-15", "Tibetan Plateau TS94-16", "Tibetan Plateau TS94-17",
    "Tibetan Plateau TS94-18", "Tibetan Plateau TS94-19", "Tibetan Plateau TS94-20",
    "Tibetan Plateau TS94-21", "Tibetan Plateau TS94-23", "Tibetan Plateau TS94-24",
    "Tibetan Plateau TS94-25", "Tibetan Plateau TS94-26", "Tibetan Plateau TS94-27",
    "Tibetan Plateau TS94-28", "Tibetan Plateau TS94-29", "Tibetan Plateau TS94-30",
    "Tibetan Plateau TS94-31", "Tibetan Plateau TS94-32", "Tibetan Plateau TS94-33",
    "Tibetan Plateau TS94-34", "Tibetan Plateau TS94-35", "Tibetan Plateau TS94-36",
    "Tibetan Plateau TS94-37", "Tibetan Plateau TS94-38", "Tibetan Plateau TS94-39",
    "Tibetan Plateau TS94-40", "Tibetan Plateau TS94-41", "Tibetan Plateau TS94-42",
    "Tibetan Plateau TS94-43", "Tibetan Plateau TS94-44", "Tibetan Plateau TS94-45",
    "Tibetan Plateau TS94-46", "Tibetan Plateau TS94-47", "Tibetan Plateau TS94-48",
    "Tibetan Plateau TS94-49", "Tibetan Plateau TS94-50", "Tibetan Plateau TS94-51",
    "Tibetan Plateau TS94-52", "Tibetan Plateau TS94-53", "Tibetan Plateau TS94-54",
    "Tibetan Plateau TS95-01", "Tibetan Plateau TS95-02", "Tibetan Plateau TS95-03",
    "Tibetan Plateau TS95-04", "Tibetan Plateau TS95-05", "Tibetan Plateau TS95-06",
    "Tibetan Plateau TS95-07", "Tibetan Plateau TS95-08", "Tibetan Plateau TS95-09",
    "Tibetan Plateau TS95-10", "Tibetan Plateau TS95-11", "Tibetan Plateau TS95-12",
    "Tibetan Plateau TS95-13", "Tibetan Plateau TS95-14", "Tibetan Plateau TS95-15",
    "Tibetan Plateau TS95-16", "Tibetan Plateau TS95-17", "Tibetan Plateau TS95-18",
    "Tibetan Plateau TS95-19", "Tibetan Plateau TS95-20", "Tibetan Plateau TS95-21",
    "Tibetan Plateau TS95-22", "Tibetan Plateau TS95-23", "Tibetan Plateau TS95-24",
    "Tibetan Plateau TS95-25", "Tibetan Plateau TS95-26", "Tibetan Plateau TS95-27",
    "Tibetan Plateau TS95-28", "Tibetan Plateau TS95-29", "Tibetan Plateau TS95-30",
    "Tibetan Plateau TS95-31", "Tibetan Plateau TS95-32", "Tibetan Plateau TS95-33",
    "Tibetan Plateau TS95-34", "Tibetan Plateau TS95-35", "Tibetan Plateau TS95-36",
    "Tibetan Plateau TS95-37", "Tibetan Plateau TS95-38", "Tibetan Plateau TS95-39",
    "Tibetan Plateau TS95-40", "Tibetan Plateau TS95-41", "Tibetan Plateau TS95-42",
    "Tibetan Plateau TS95-43", "Tibetan Plateau TS95-44", "Tibetan Plateau TS95-45",
    "Tibetan Plateau TS95-46", "Tibetan Plateau TS95-48", "Tibetan Plateau TS95-49",
    "Tibetan Plateau TS95-50", "Tibetan Plateau TS95-51", "Tibetan Plateau TS95-52",
    "Tibetan Plateau TS95-53", "Tibetan Plateau TS95-54", "Tibetan Plateau TS95-55",
    "Tibetan Plateau TS95-56", "Tibetan Plateau TS95-57", "Tibetan Plateau TS95-58",
    "Tibetan Plateau TS95-59", "Tibetan Plateau TS95-60", "Tibetan Plateau TS95-61",
    "Tibetan Plateau TS95-62", "Tibetan Plateau TS95-63", "Tibetan Plateau TS99-01",
    "Tibetan Plateau TS99-02", "Tibetan Plateau TS99-03", "Tibetan Plateau TS99-04",
    "Tibetan Plateau TS99-05", "Tibetan Plateau TS99-06", "Tibetan Plateau TS99-07",
    "Tibetan Plateau TS99-09", "Tibetan Plateau TS99-10", "Tibetan Plateau TS99-11",
    "Tibetan Plateau TS99-12", "Tibetan Plateau TS99-13", "Tibetan Plateau TS99-14",
    "Tibetan Plateau TS99-15", "Tibetan Plateau TS99-16", "Tibetan Plateau TS99-17",
    "Tibetan Plateau TS99-18", "Tibetan Plateau TS99-19", "Tibetan Plateau TS99-20",
    "Tibetan Plateau TS99-21", "Tibetan Plateau TS99-22", "Tibetan Plateau TS99-23",
    "Tibetan Plateau TS99-24", "Tibetan Plateau TS99-25", "Tibetan Plateau TS99-8a",
    "Tibetan Plateau TS99-8b", "Tibetan Plateau WHA-25", "Tibetan Plateau XX-U-1",
    "Tibetan Plateau ZL-U-1", "Tongguan Mt 6", "Tongguan Mt 7", "Tongguanshan",
    "Tongzha", "Ulanul Lake", "Urumqui T-19", "Urumqui T-21", "Urumqui T-23",
    "Urumqui T-27", "Urumqui T-29", "Urumqui T-32", "Urumqui T-33",
    "Urumqui T-34", "Urumqui T-35", "Urumqui T-36", "Urumqui T-38",
    "Weichang", "Weining", "Wenchang", "Wudaoliang 1", "Wudaoliang 2",
    "Wulungu Lake", "Wumaqu", "Wuqia", "Wutong Mt 1", "Wutong Mt 2",
    "Wutong Mt 3", "Wuxi", "Wuzhi Mt 01", "Wuzhi Mt 02", "Wuzhi Mt 03",
    "Wuzhi Mt 04", "Wuzhi Mt 05", "Wuzhi Mt 06", "Wuzhi Mt 07", "Wuzhi Mt 08",
    "Wuzhi Mt 09", "Wuzhi Mt 10", "Wuzhi Mt 11", "Wuzhi Mt 12", "Wuzhi Mt 13",
    "Wuzhi Mt 14", "Wuzhi Mt 15", "Wuzhi Mt 16", "Wuzhi Mt 17", "Wuzhi Mt 18",
    "Wuzhi Mt 19", "Wuzhi Mt 20", "Wuzhi Mt 21", "Wuzhi Mt 22", "Wuzhi Mt 23",
    "Wuzhi Mt 24", "Wuzhi Mt 25", "Wuzhi Mt 26", "Wuzhi Mt 27", "Wuzhi Mt 28",
    "Wuzhi Mt 29", "Wuzhi Mt 30", "Wuzhi Mt 31", "Wuzhi Mt 32", "Wuzhi Mt 33",
    "Wuzhi Mt 34", "Xiangpishan", "Xiangtou Mt 1", "Xiangtou Mt 2",
    "Xiangtou Mt 3", "Xiangtou Mt 4", "Xiangtou Mt 5", "Xiangtou Mt 6",
    "Xiangtou Mt 7", "Xiaoheigou 01", "Xiaoheigou 02", "Xiaoheigou 03",
    "Xiaoheigou 04", "Xiaoheigou 05", "Xiaoheigou 06", "Xiaoheigou 07",
    "Xiaoheigou 08", "Xiaoheigou 09", "Xiaoheigou 10", "Xiaonan",
    "Xiaoshazi", "Xinfeng 1", "Xinfeng 2", "Xinfeng 3", "Xinfeng 4",
    "Xinjingbridge", "Xiongoucuo", "Xishan 1", "Xishan 2", "Xishan 3",
    "Xishan 4", "Xishan 5", "Xiyaohu", "Xugecuo", "Yalongwan", "Yangbajing",
    "Yangchi", "Yangerzhuang", "Yangpu 1", "Yangpu 2", "Yangpu 3",
    "Yangpu 4", "Yangpu 5", "Yanshan Mt 01", "Yanshan Mt 02", "Yanshan Mt 03",
    "Yanshan Mt 04", "Yanshan Mt 05", "Yanshan Mt 06", "Yanshan Mt 07",
    "Yanshan Mt 08", "Yanshan Mt 09", "Yanshan Mt 10", "Yanshan Mt 17",
    "Yanshan Mt 18", "Yanshan Mt 19", "Yanshengang", "Yidun Lake",
    "Yingde 1", "Yingde 2", "Yingde qt", "Yixing 1", "Yixing 2",
    "Yulin", "Yunan 1", "Yunan 2", "Yuntaishan", "Zengpiyan 1", "Zengpiyan 2",
    "Zengpiyan 3", "Zengpiyan 4", "Zengpiyan 5", "Zhangzhou", "Zhuhai 1",
    "Zhuhai 2", "Zigetangcuo 1", "Zigetangcuo 2", "Zigetangcuo 3",
    "Zigetangcuo 4", "Zigetangcuo 5", "Zigetangcuo 6", "Zigetangcuo 7",
    "Zijinshan", "Zoige 1", "Zoige 2", "Zoige 3")

CMPD_excluded <- CMPD_all %>%
  dplyr::filter(entity_name %in% CMPD_ENTITIES)
CMPD <- CMPD_all %>%
  dplyr::filter(!(entity_name %in% CMPD_ENTITIES))

usethis::use_data(CMPD, overwrite = TRUE, compress = "xz")

# ------------------------------------------------------------------------------
# |                         Find matches in the EMPDv2                         |
# ------------------------------------------------------------------------------
compare_latlon(EMPDv2, CMPD, digits = 2)

# ------------------------------------------------------------------------------
# |                                  Sandbox                                   |
# ------------------------------------------------------------------------------
CMPD %>%
  dplyr::rowwise() %>%
  dplyr::mutate(total_count = dplyr::c_across(Abelia:Zygophyllum) %>%
                  sum(na.rm = TRUE), .before = Abelia) %>%
  dplyr::arrange(total_count) %>%
  dplyr::filter(total_count < 99)

# Find duplicated records
idx <- duplicated(cmpd_counts$entity_name)
cmpd_counts_dup <- cmpd_counts %>%
  dplyr::filter(entity_name %in% cmpd_counts$entity_name[idx])

# ------------------------------------------------------------------------------
# |                   Export nonsense records for inspection                   |
# ------------------------------------------------------------------------------
cmpd_counts_nonsense <- cmpd_counts %>%
  dplyr::select(1:6, !!cmpd_clean_taxon_names_nonsense$taxon_name) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(total_count = sum(dplyr::c_across(`Abies+Picea`:`Tilia+Ulmus`) %>%
                                    as.numeric(), na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(total_count > 0)

cmpd_counts_nonsense2 <- cmpd_metadata2 %>%
  dplyr::select(ID_CMPD,
                source,
                site_name,
                entity_name,
                latitude,
                longitude,
                elevation
  ) %>%
  dplyr::right_join(cmpd_counts_nonsense %>%
                     dplyr::select(-c(1:2, 4:6)))
cmpd_counts_nonsense2 %>%
  readr::write_excel_csv("~/Downloads/SMPDSv2/CMPD-taxons-for-inspection_2021-08-09.csv", na = "")


# ------------------------------------------------------------------------------
# |          Export duplicated record: entity_name - taxon_name pairs          |
# ------------------------------------------------------------------------------
# Find duplicate entity_name - taxon_name pairs
tmp20 <- cmpd_counts_long2 %>%
  dplyr::group_by(entity_name, taxon_name) %>%
  dplyr::mutate(n = length(taxon_name),
                unique_count = length(unique(value))) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(entity_name, taxon_name) %>%
  dplyr::filter(n != 1)
tmp21 <- tmp20 %>%
  dplyr::filter(unique_count > 1)
tmp21 %>%
  dplyr::select(1:7, 9, 8) %>%
  readr::write_excel_csv("~/Downloads/SMPDSv2/CMPD-multiple-records-same-taxon-entity.csv", na = "")


cmpd_counts_long <- cmpd_counts %>%
  tidyr::pivot_longer(cols = -c(1:6), names_to = "taxon_name") %>%
  dplyr::filter(!is.na(value))  %>%
  dplyr::filter(!(taxon_name %in% cmpd_clean_taxon_names_nonsense$taxon_name),
                !(taxon_name %in% cmpd_clean_taxon_names_delete$taxon_name)) %>%
  dplyr::left_join(cmpd_clean_taxon_names,
                   by = "taxon_name") %>%
  dplyr::rename(taxon_name_original = taxon_name,
                taxon_name = clean_name)

cmpd_counts2 <- cmpd_counts %>%
  dplyr::select(-!!cmpd_clean_taxon_names_nonsense$taxon_name,
                -!!cmpd_clean_taxon_names_delete$taxon_name) %>%
  magrittr::set_names(c(colnames(.)[c(1:6)],
                        tibble::tibble(taxon_name = colnames(.)[-c(1:6)]) %>%
                          dplyr::left_join(cmpd_clean_taxon_names,
                                           by = "taxon_name") %>%
                          .$clean_name))
