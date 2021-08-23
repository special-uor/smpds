#' \code{APD} dataset
#'
#' The African Pollen Dataset.
#'
#' @format A data frame (\code{tibble} object) with `r nrow(smpds::APD)` rows and `r ncol(smpds::APD)` variables:
#' \describe{
#'   \item{...}{int ...}
#' }
#' @source
#' Davis, B.A., Chevalier, M., Sommer, P., Carter, V.A., Finsinger, W., Mauri, A., Phelps, L.N., Zanon, M., Abegglen, R., Åkesson, C.M. and Alba-Sánchez, F., 2020. The Eurasian Modern Pollen Database (EMPD), version 2. Earth system science data, 12(4), pp.2423-2445. https://doi.org/10.5194/essd-12-2423-2020
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
#' @source
#' https://crudata.uea.ac.uk/~timm/grid/CRU_TS_2_1.html
#' @references
#' Mitchell, T.D. and Jones, P.D., 2005. An improved method of constructing a database of monthly climate observations and associated high‐resolution grids. International Journal of Climatology: A Journal of the Royal Meteorological Society, 25(6), pp.693-712. https://doi.org/10.1002/joc.1181
#'
"CRU_coords"

#' \code{EMPDv2} dataset
#'
#' The Eurasian Modern Pollen Database (EMPD), version 2.
#'
#' @format A data frame (\code{tibble} object) with `r nrow(smpds::EMPDv2)` rows and `r ncol(smpds::EMPDv2)` variables:
#' \describe{
#'   \item{...}{int ...}
#' }
#' @source
#' Davis, B.A., Chevalier, M., Sommer, P., Carter, V.A., Finsinger, W., Mauri, A., Phelps, L.N., Zanon, M., Abegglen, R., Åkesson, C.M. and Alba-Sánchez, F., 2020. The Eurasian Modern Pollen Database (EMPD), version 2. Earth system science data, 12(4), pp.2423-2445. https://doi.org/10.5194/essd-12-2423-2020
"EMPDv2"

#' \code{Herzschuh} dataset
#'
#' @format A data frame (\code{tibble} object) with `r nrow(smpds::Herzschuh)` rows and `r ncol(smpds::Herzschuh)` variables:
#' \describe{
#'   \item{...}{int ...}
#' }
#' @source
#' Herzschuh, U., Cao, X., Laepple, T., Dallmeyer, A., Telford, R.J., Ni, J., Chen, F., Kong, Z., Liu, G., Liu, K.B. and Liu, X., 2019. Position and orientation of the westerly jet determined Holocene rainfall patterns in China. Nature communications, 10(1), pp.1-8. https://doi.org/10.1038/s41467-019-09866-8
"Herzschuh"

#' \code{IbMPD} dataset
#'
#' The Iberian Modern Pollen Dataset (IbMPD).
#'
#' @format A data frame (\code{tibble} object) with `r nrow(smpds::IbMPD)` rows and `r ncol(smpds::IbMPD)` variables:
#' \describe{
#'   \item{...}{int ...}
#' }
#' @source
#' Harrison, S.P., Shen, Y. and Sweeney, L., 2021. Pollen data and charcoal data of the Iberian Peninsula. University of Reading. Dataset. http://doi.org/10.17864/1947.294
"IbMPD"

#' \code{ItMPD} dataset
#'
#' The Italian Modern Pollen Dataset (ItMPD).
#'
#' @format A data frame (\code{tibble} object) with `r nrow(smpds::ItMPD)` rows and `r ncol(smpds::ItMPD)` variables:
#' \describe{
#'   \item{...}{int ...}
#' }
#' @source
#' Finsinger, W., Heiri, O., Valsecchi, V., Tinner, W. and Lotter, A.F., 2007. Modern pollen assemblages as climate indicators in southern Europe. Global Ecology and Biogeography, 16(5), pp.567-582. https://doi.org/10.1111/j.1466-8238.2007.00313.x
"ItMPD"

#' \code{PNV} dataset
#'
#' Global Maps of Potential Natural Vegetation at 1 km resolution, version 4.
#'
#' @format A data frame (\code{RasterBrick} object) with `r dim(smpds::PNV)[1]` rows and `r dim(smpds::PNV)[2]` columns.
#'
#' @source
#' Hengl, Tomislav (2018): Global Maps of Potential Natural Vegetation at 1 km resolution, version 4. Harvard Dataverse. Dataset. https://doi.org/10.7910/DVN/QQHCIK
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
#' Harrison, Sandy  (2019): Modern pollen data for climate reconstructions, version 1 (SMPDS). University of Reading. Dataset. http://dx.doi.org/10.17864/1947.194
"SMPDSv1"
