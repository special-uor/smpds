#' \code{AMSS} dataset
#'
#' The African Modern Surface Samples dataset. Part of the [smpds::SMPDSv2].
#'
#' @section Variables:
#' \describe{
#'   \item{source}{\code{character} Source from which the data was repatriated}
#'   \item{site_name}{\code{character} Site name as given by the original
#'   authors or as defined by us where there was not an unique name given to
#'   the site}
#'   \item{entity_name}{\code{character} Entity name. An entity may be a
#'   separate core from the same site or an alternative measurement type of the
#'   same core}
#'   \item{latitude}{\code{numeric} Latitude of the sampling site, given in
#'   decimal degrees, where N is positive and S is negative}
#'   \item{longitude}{\code{numeric} Longitude of the sampling site in decimal
#'   degrees, where E is positive and W is negative}
#'   \item{elevation}{\code{numeric} Elevation of the sampling site in metres
#'   above (+) or below (-) sea level}
#'   \item{basin_size}{\code{character} Size of sampled site (e.g. lake, bog,
#'   etc.) in \[km^2\] or given as a categorical estimated when precise
#'   information was not recorded or was not available}
#'   \item{site_type}{\code{character} Information about type of site
#'   (e.g. cave, lake, glacial, terrestrial, etc.)}
#'   \item{entity_type}{\code{character} Information about the type of entity
#'   (e.g. composite, core top, surface sample, etc.)}
#'   \item{age_BP}{\code{character} Sample age in years before present (BP) or
#'   categorical estimate where not numeric values were available}
#'   \item{publication}{\code{character} Citation for the publication(s) where
#'   the data was originally published}
#'   \item{doi}{\code{character} Digital Object Identifier (DOI) for the (each)
#'   publication - if available}
#'   \item{ID_BIOME}{\code{numeric} Unique identifier for each potential
#'   natural vegetation (PNV)}
#'   \item{PNV (only [smpds::SMPDSv2])}{\code{character} Potential Natural
#'   Vegetation based on the work by
#'   \insertCite{hengl2018global}{smpds}}
#'   \item{ID_SAMPLE}{\code{numeric} Unique identifier for each sample}
#'   \item{sample_name (only [smpds::australia_pollen])}{\code{character}
#'   Sample name}
#'   \item{mi}{\code{numeric} Reconstructed Moisture Index \[unitless\] using
#'   the CRU TS 4.04 \insertCite{harris2020}{smpds} dataset}
#'   \item{gdd0}{\code{numeric} Reconstructed Growing Degree Days above 0°C
#'   \[°C day\] using the CRU TS 4.04 \insertCite{harris2020}{smpds} dataset}
#'   \item{mat}{\code{numeric} Reconstructed Mean Annual Temperature \[°C\]
#'   using the CRU TS 4.04 \insertCite{harris2020}{smpds} dataset}
#'   \item{mtco}{\code{numeric} Reconstructed Mean Temperature of the Coldest
#'   Month \[°C\] using the CRU TS 4.04 \insertCite{harris2020}{smpds} dataset}
#'   \item{mtwa}{\code{numeric} Reconstructed Mean Temperature of the Warmest
#'   Month \[°C\] using the CRU TS 4.04 \insertCite{harris2020}{smpds} dataset}
#'   \item{map}{\code{numeric} Reconstructed Mean Annual Precipitation
#'   \[mm/year\] using the CRU TS 4.04 \insertCite{harris2020}{smpds} dataset}
#'   \item{clean}{\code{data.frame} Clean pollen counts. Each column represents
#'   a different pollen taxon}
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
#'
#' \insertAllCited{}
"AMSS"

#' \code{APD} dataset
#'
#' The African Pollen dataset.
#'
#' @inheritSection AMSS Variables
#'
#' @references
# `r Rdpack::insert_ref("vincens2007african", "smpds")`
#' \insertRef{vincens2007african}{smpds}
#'
#' \insertAllCited{}
#'
#' @source
#' http://fpd.sedoo.fr/fpd/bibli.do
"APD"

#' \code{australia_pollen} dataset
#'
#' The Australian Pollen dataset.
#'
#' @inheritSection AMSS Variables
#'
#' @references
#' \insertRef{adeleye2021environmental}{smpds}
#'
#' \insertRef{adeleye2021holocene}{smpds}
#'
#' \insertRef{beck2017early}{smpds}
#'
#' \insertRef{Field2018}{smpds}
#'
#' \insertRef{Fletcher2014}{smpds}
#'
#' \insertRef{Herbert2016}{smpds}
#'
#' \insertRef{Luly1993}{smpds}
#'
#' \insertRef{Luly1986}{smpds}
#'
#' \insertRef{Mariani2017}{smpds}
#'
#' \insertRef{McWethy2010}{smpds}
#'
#' \insertRef{McWethy2014}{smpds}
#'
#' \insertRef{Pickett2004}{smpds}
#'
#' \insertRef{Prebble2019}{smpds}
#'
#' \insertAllCited{}
#'
"australia_pollen"

#' \code{CMPD} dataset
#'
#' The Chinese Modern Pollen Dataset by Ni Jian.
#'
#' @inheritSection AMSS Variables
#'
#' @references
#' \insertRef{chen2021modern}{smpds}
#'
#' \insertAllCited{}
#'
"CMPD"

#' \code{CRU_coords} dataset
#'
#' The CRU TS 2.1 dataset coordinates at 0.5 degrees resolution.
#'
#' @section Variables:
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
#' @inheritSection AMSS Variables
#'
#' @references
#' \insertRef{davis2020eurasian}{smpds}
#'
#' \insertAllCited{}
#'
#' @source
#' https://doi.pangaea.de/10.1594/PANGAEA.909130
"EMPDv2"

#' \code{EMBSeCBIO} dataset
#'
#' The Eastern Mediterranean-Black Sea-Caspian-Corridor region and biome
#' reconstructions database.
#'
#' @inheritSection AMSS Variables
#'
#' @references
#' \insertRef{harrison2017embsecbio}{smpds}
#'
#' \insertRef{harrison2021embsecbio}{smpds}
#'
#' \insertAllCited{}
#'
#' @source
#' 10.17864/1947.309
"EMBSeCBIO"

#' \code{Herzschuh} dataset
#'
#' Dataset provided by \insertCite{herzschuh2019position;textual}{smpds}.
#'
#' @inheritSection AMSS Variables
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
#' @inheritSection AMSS Variables
#'
#' @references
#' \insertRef{rdgdr294}{smpds}
#'
#' \insertRef{harrison2022}{smpds}
#'
#' \insertAllCited{}
#'
#' @source
#' https://researchdata.reading.ac.uk/294
"IbMPD"

#' \code{japanese_pollen} dataset
#'
#' The Japanese Pollen dataset.
#'
#' @inheritSection AMSS Variables
#'
#' @references
#' \insertRef{Takahara2000}{smpds}
#'
#' \insertAllCited{}
"japanese_pollen"

#' \code{NEOTOMA} dataset
#'
#' The Neotoma Paleoecology Database subset (NEOTOMA).
#'
#' @inheritSection AMSS Variables
#'
#' @references
#' \insertRef{williams2018neotoma}{smpds}
#'
#' \insertAllCited{}
#'
#' @source
#' https://www.neotomadb.org
"NEOTOMA"

#' \code{Phelps} dataset
#'
#' Dataset provided by \insertCite{phelps2020asymmetric;textual}{smpds}.
#'
#' @inheritSection AMSS Variables
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
#' @inheritSection AMSS Variables
#'
#' @references
#' \insertRef{harrison2019}{smpds}
#'
#' \insertAllCited{}
#'
#' @source
#' \insertRef{rdgdr194}{smpds}
"SMPDSv1"

#' \code{SMPDSv2} dataset
#'
#' SPECIAL Modern Pollen Data for climate reconstructionS, version 2.
#'
#' @inheritSection AMSS Variables
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
#'
#' \insertAllCited{}
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
#'
#' @inheritSection AMSS Variables
#'
#' @references
#' \insertAllCited{}
"additional_european_pollen"

#' \code{dugerdil_pollen} dataset
#'
#' Pollen samples provided by L. Dugerdil. Part of the [smpds::SMPDSv2].
#'
#' @inheritSection AMSS Variables
#'
#' @references
#' \insertRef{dugerdil2021climate}{smpds}
#'
#' \insertAllCited{}
"dugerdil_pollen"

#' \code{gaillard_pollen} dataset
#'
#' Pollen samples provided by M.J. Gaillard. Part of the [smpds::SMPDSv2].
#'
#' @inheritSection AMSS Variables
#'
#' @references
#' \insertRef{gaillard1992modern}{smpds}
#'
#' \insertAllCited{}
"gaillard_pollen"

#' \code{moroccan_pollen} dataset
#'
#' Moroccan pollen samples. Part of the [smpds::SMPDSv2].
#'
#' @inheritSection AMSS Variables
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
#'
#' \insertAllCited{}
"moroccan_pollen"

#' \code{neotropics_pollen} dataset
#'
#' Neotropics pollen samples. Part of the [smpds::SMPDSv2].
#'
#' @inheritSection AMSS Variables
#'
#' @references
#' \insertRef{bush2021modern}{smpds}
#'
#' \insertAllCited{}
"neotropics_pollen"

#' \code{north_america_pollen} dataset
#'
#' North America  pollen samples. Part of the [smpds::SMPDSv2].
#'
#' @inheritSection AMSS Variables
#'
#' @references
#' \insertAllCited{}
"north_america_pollen"

#' \code{south_america_pollen} dataset
#'
#' South America  pollen samples. Part of the [smpds::SMPDSv2].
#'
#' @inheritSection AMSS Variables
#'
#' @references
#' \insertAllCited{}
"south_america_pollen"

#' \code{southern_hemisphere_pollen} dataset
#'
#' Southern hemisphere  pollen samples. Part of the [smpds::SMPDSv2].
#'
#' @inheritSection AMSS Variables
#'
#' @references
#' \insertRef{Black2007}{smpds}
#'
#' \insertRef{Dodson1978}{smpds}
#'
#' \insertRef{Dodson1999}{smpds}
#'
#' \insertRef{Haberle1996}{smpds}
#'
#' \insertRef{haberle1993late}{smpds}
#'
#' \insertRef{Hope2009}{smpds}
#'
#' \insertRef{Hope1988}{smpds}
#'
#' \insertRef{hope1999holocene}{smpds}
#'
#' \insertRef{Macphail1980}{smpds}
#'
#' \insertRef{Macphail1979}{smpds}
#'
#' \insertRef{Macphail1975}{smpds}
#'
#' \insertRef{macphail1983value}{smpds}
#'
#' \insertRef{MacphailMidenhall1980}{smpds}
#'
#' \insertRef{Norton1986}{smpds}
#'
#' \insertRef{Prebble2019}{smpds}
#'
#' \insertRef{Shulmeister2003}{smpds}
#'
#' \insertAllCited{}
"southern_hemisphere_pollen"

#' \code{tatiana_pollen} dataset
#'
#' Pollen samples provided by T. Blyakharchuk. Part of the [smpds::SMPDSv2].
#'
#' @inheritSection AMSS Variables
#'
#' @references
#' \insertAllCited{}
"tatiana_pollen"
