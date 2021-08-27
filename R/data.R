#' \code{APD} dataset
#'
#' The African Pollen Dataset.
#'
#' @format A data frame (\code{tibble} object) with `r nrow(smpds::APD)` rows and `r ncol(smpds::APD)` variables:
#' \describe{
#'   \item{...}{int ...}
#' }
#'
#' @references
#' \insertRef{vincens2007african}{smpds}
#'
#' @source
#' http://fpd.sedoo.fr/fpd/bibli.do
"APD"

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

#' \code{ItMPD} dataset
#'
#' The Italian Modern Pollen Dataset (ItMPD).
#'
#' @format A data frame (\code{tibble} object) with `r nrow(smpds::ItMPD)` rows and `r ncol(smpds::ItMPD)` variables:
#' \describe{
#'   \item{...}{int ...}
#' }
#'
#' @references
#' \insertRef{finsinger2007modern}{smpds}
"ItMPD"

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

#' \code{PNV} dataset
#'
#' Global Maps of Potential Natural Vegetation at 1 km resolution, version 4.
#'
#' @format A data frame (\code{RasterBrick} object) with `r dim(smpds::PNV)[1]` rows and `r dim(smpds::PNV)[2]` columns.
#'
#' @references
#' \insertRef{hengl2018global}{smpds}
#'
#' @source
#' \insertRef{hengl2018global1kmres}{smpds}
"PNV"

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
#'
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
