#' \code{AMSS} dataset
#'
#' The African Modern Surface Samples dataset. Part of the [smpds::SMPDSv2].
#'
#' @format A data frame (\code{tibble} object) with `r nrow(smpds::AMSS)` rows and `r ncol(smpds::AMSS)` variables:
#' \describe{
#'   \item{source}{\code{character} Source of the dataset}
#'   \item{site_name}{\code{character} Site name}
#'   \item{entity_name}{\code{character} Entity name}
#'   \item{latitude}{\code{numeric} Latitude}
#'   \item{longitude}{\code{numeric} Longitude}
#'   \item{elevation}{\code{numeric} Elevation}
#'   \item{basin_size}{\code{character} Basin size}
#'   \item{site_type}{\code{character} Site type}
#'   \item{entity_type}{\code{character} Entity type}
#'   \item{age_BP}{\code{character} Age BP}
#'   \item{publication}{\code{character} Publication}
#'   \item{doi}{\code{character} DOI}
#'   \item{ID_BIOME}{\code{numeric} Biome identifier}
#'   \item{ID_SAMPLE}{\code{numeric} Sample identifier}
#'   \item{mi}{\code{numeric} Moisture index}
#'   \item{gdd0}{\code{numeric} Growing degree days with 0 deg Celsius}
#'   \item{mat}{\code{numeric} Mean annual temperature}
#'   \item{mtco}{\code{numeric} Mean temperature of the coldest month}
#'   \item{mtwa}{\code{numeric} Mean temperature of the warmest month}
#'   \item{map}{\code{numeric} Mean annual precipitation}
#'   \item{clean}{\code{data.frame} Clean pollen counts}
#'   \item{intermediate}{\code{data.frame} Intermediate (grouped) pollen counts}
#'   \item{amalgamated}{\code{data.frame} Amalgamated pollen counts}
#' }
#'
#' @references
#' \insertRef{jolly1996representation}{smpds}
#'
#' \insertRef{lebamba2009modern}{smpds}
#'
#' \insertRef{julier2018modern}{smpds}
#'
#' \insertRef{julier2019variability}{smpds}
"AMSS"

#' \code{APD} dataset
#'
#' The African Pollen dataset.
#'
#' @format A data frame (\code{tibble} object) with `r nrow(smpds::APD)` rows and `r ncol(smpds::APD)` variables:
#' \describe{
#'   \item{...}{int ...}
#' }
#'
#' @references
# `r Rdpack::insert_ref("vincens2007african", "smpds")`
#' \insertRef{vincens2007african}{smpds}
#'
#' @source
#' http://fpd.sedoo.fr/fpd/bibli.do
"APD"

#' \code{australia_pollen} dataset
#'
#' The Australian Pollen dataset.
#'
#' @format A data frame (\code{tibble} object) with `r nrow(smpds::australia_pollen)` rows and `r ncol(smpds::australia_pollen)` variables:
#' \describe{
#'   \item{...}{int ...}
#' }
#'
"australia_pollen"

#' \code{CMPD} dataset
#'
#' The Chinese Modern Pollen Dataset by Ni Jian.
#'
#' @format A data frame (\code{tibble} object) with `r nrow(smpds::CMPD)` rows and `r ncol(smpds::CMPD)` variables:
#' \describe{
#'   \item{...}{int ...}
#' }
"CMPD"

#' \code{CRU_coords} dataset
#'
#' The CRU TS 2.1 dataset coordinates at 0.5 degrees resolution.
#'
#' @format A data frame (\code{tibble} object) with `r nrow(smpds::CRU_coords)` rows and `r ncol(smpds::CRU_coords)` variables:
#' \describe{
#'   \item{latitude}{Numeric values for latitude in decimal degrees.}
#'   \item{longitude}{Numeric values for longitude in decimal degrees.}
#'   \item{elevation}{Numeric values for elevation in metres above sea level.}
#' }
#'
#' @references
#' \insertRef{mitchell2005improved}{smpds}
#'
#' @source
#' https://crudata.uea.ac.uk/~timm/grid/CRU_TS_2_1.html
"CRU_coords"

#' \code{EMPDv2} dataset
#'
#' The Eurasian Modern Pollen Database (EMPD), version 2.
#'
#' @format A data frame (\code{tibble} object) with `r nrow(smpds::EMPDv2)` rows and `r ncol(smpds::EMPDv2)` variables:
#' \describe{
#'   \item{...}{int ...}
#' }
#'
#' @references
#' \insertRef{davis2020eurasian}{smpds}
#'
#' @source
#' https://doi.pangaea.de/10.1594/PANGAEA.909130
"EMPDv2"

#' \code{EMBSeCBIO} dataset
#'
#' The Eastern Mediterranean-Black Sea-Caspian-Corridor region and biome
#' reconstructions database.
#'
#' @format A data frame (\code{tibble} object) with `r nrow(smpds::EMBSeCBIO)` rows and `r ncol(smpds::EMBSeCBIO)` variables:
#' \describe{
#'   \item{...}{int ...}
#' }
#'
#' @references
#' \insertRef{harrison2017embsecbio}{smpds}
#'
#' \insertRef{harrison2021embsecbio}{smpds}
#'
#' @source
#' 10.17864/1947.309
"EMBSeCBIO"

#' \code{Herzschuh} dataset
#'
#' Dataset provided by \insertCite{herzschuh2019position;textual}{smpds}.
#'
#' @format A data frame (\code{tibble} object) with `r nrow(smpds::Herzschuh)` rows and `r ncol(smpds::Herzschuh)` variables:
#' \describe{
#'   \item{...}{int ...}
#' }
#'
#' @references
#' \insertAllCited{}
#'
#' @source
#' https://static-content.springer.com/esm/art%3A10.1038%2Fs41467-019-09866-8/MediaObjects/41467_2019_9866_MOESM3_ESM.zip
"Herzschuh"

#' \code{IbMPD} dataset
#'
#' The Iberian Modern Pollen Dataset (IbMPD).
#'
#' @format A data frame (\code{tibble} object) with `r nrow(smpds::IbMPD)` rows and `r ncol(smpds::IbMPD)` variables:
#' \describe{
#'   \item{...}{int ...}
#' }
#' @references
#' \insertRef{rdgdr294}{smpds}
#'
#' @source
#' https://researchdata.reading.ac.uk/294
"IbMPD"

#' \code{japanese_pollen} dataset
#'
#' The Japanese Pollen dataset.
#'
#' @format A data frame (\code{tibble} object) with `r nrow(smpds::japanese_pollen)` rows and `r ncol(smpds::japanese_pollen)` variables:
#' \describe{
#'   \item{...}{int ...}
#' }
#'
"japanese_pollen"

#' \code{NEOTOMA} dataset
#'
#' The Neotoma Paleoecology Database subset (NEOTOMA).
#'
#' @format A data frame (\code{tibble} object) with `r nrow(smpds::NEOTOMA)` rows and `r ncol(smpds::NEOTOMA)` variables:
#' \describe{
#'   \item{...}{int ...}
#' }
#'
#' @references
#' \insertRef{williams2018neotoma}{smpds}
#'
#' @source
#' https://www.neotomadb.org
"NEOTOMA"

#' \code{Phelps} dataset
#'
#' Dataset provided by \insertCite{phelps2020asymmetric;textual}{smpds}.
#'
#' @format A data frame (\code{tibble} object) with `r nrow(smpds::Phelps)` rows and `r ncol(smpds::Phelps)` variables:
#' \describe{
#'   \item{...}{int ...}
#' }
#'
#' @references
#' \insertAllCited{}
#'
#' @source
#' https://doi.pangaea.de/10.1594/PANGAEA.905309
"Phelps"

#' \code{PNV_1km} dataset
#'
#' Global Maps of Potential Natural Vegetation at 1 km resolution, version 4.
#'
#' @format A data frame (\code{RasterBrick} object) with `r dim(smpds::PNV_1km)[1]` rows and `r dim(smpds::PNV_1km)[2]` columns.
#'
#' @references
#' \insertRef{hengl2018global}{smpds}
#'
#' @source
#' \insertRef{hengl2018global1kmres}{smpds}
"PNV_1km"

#' \code{SMPDSv1} dataset
#'
#' SPECIAL Modern Pollen Data for climate reconstructionS, version 1.
#'
#' @format A data frame (\code{tibble} object) with `r nrow(smpds::SMPDSv1)` rows and `r ncol(smpds::SMPDSv1)` variables:
#' \describe{
#'   \item{...}{int ...}
#' }
#'
#' @source
#' \insertRef{rdgdr194}{smpds}
"SMPDSv1"

#' \code{SMPDSv2} dataset
#'
#' SPECIAL Modern Pollen Data for climate reconstructionS, version 2.
#'
#' @format A data frame (\code{tibble} object) with `r nrow(smpds::SMPDSv2)` rows and `r ncol(smpds::SMPDSv2)` variables:
#' \describe{
#'   \item{...}{int ...}
#' }
#'
#' @references
#' \insertRef{davis2020eurasian}{smpds}
#'
#' \insertRef{finsinger2007modern}{smpds}
#'
#' \insertRef{rdgdr194}{smpds}
#'
#' \insertRef{rdgdr294}{smpds}
#'
#' \insertRef{herzschuh2019position}{smpds}
#'
#' \insertRef{phelps2020asymmetric}{smpds}
#'
#' \insertRef{vincens2007african}{smpds}
#'
#' \insertRef{williams2018neotoma}{smpds}
"SMPDSv2"

#' \code{climate}
"climate"

#' \code{entity}
"entity"

#' \code{pollen_count}
"pollen_count"

#' \code{taxon_name}
"taxon_name"

#' \code{additional_european_pollen} dataset
#'
#' Additional European pollen samples. Part of the [smpds::SMPDSv2].
"additional_european_pollen"

#' \code{dugerdil_pollen} dataset
#'
#' Pollen samples provided by L. Dugerdil. Part of the [smpds::SMPDSv2].
#'
#' @references
#' \insertRef{dugerdil2021climate}{smpds}
"dugerdil_pollen"

#' \code{gaillard_pollen} dataset
#'
#' Pollen samples provided by M.J. Gaillard. Part of the [smpds::SMPDSv2].
#'
#' @references
#' \insertRef{gaillard1992modern}{smpds}
"gaillard_pollen"

#' \code{moroccan_pollen} dataset
#'
#' Moroccan pollen samples. Part of the [smpds::SMPDSv2].
#'
#' @references
#' \insertRef{hamouri1991changements}{smpds}
#'
#' \insertRef{lamb1995vegetational}{smpds}
#'
#' \insertRef{cheddadi1998holocene}{smpds}
#'
#' \insertRef{el201418}{smpds}
#'
#' \insertRef{cheddadi2015history}{smpds}
#'
#' \insertRef{campbell2017environmental}{smpds}
#'
#' \insertRef{cheddadi2017microrefugia}{smpds}
#'
#' \insertRef{fletcher2017ams}{smpds}
#'
#' \insertRef{zielhofer2017atlantic}{smpds}
"moroccan_pollen"

#' \code{neotropics_pollen} dataset
#'
#' Neotropics pollen samples. Part of the [smpds::SMPDSv2].
#'
#' @references
#' \insertRef{bush2021modern}{smpds}
"neotropics_pollen"

#' \code{north_america_pollen} dataset
#'
#' North America  pollen samples. Part of the [smpds::SMPDSv2].
"north_america_pollen"

#' \code{south_america_pollen} dataset
#'
#' South America  pollen samples. Part of the [smpds::SMPDSv2].
"south_america_pollen"

#' \code{southern_hemisphere_pollen} dataset
#'
#' Southern hemisphere  pollen samples. Part of the [smpds::SMPDSv2].
"southern_hemisphere_pollen"

#' \code{tatiana_pollen} dataset
#'
#' Pollen samples provided by T. Blyakharchuk. Part of the [smpds::SMPDSv2].
"tatiana_pollen"
