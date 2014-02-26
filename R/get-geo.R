#' @rdname getGeo
#' @export
#'
#' @title 
#' Returns geographic information about a location string
#'
#' @description
#' \code{getGeo} connects to the Data Science Tool Kit and converts the
#' location string into a pair of coordinates, and then into information
#' (city, state, country, congressional district...) for those coordinates
#'
#' @author
#' Pablo Barbera \email{pablo.barbera@@nyu.edu}
#'
#' @param location location string for which information is desired
#'
#' @param verbose If TRUE, provides additional information on console.
#'
#' @examples \dontrun{
#' ## Download geographic information for "New York"
#'  getGeo("New York")
#' }
#'

getGeo <- function(location, verbose=FALSE, rdstk="http://www.datasciencetoolkit.org"){
    require(RCurl); require(rjson)
    # empty list for results
    result <- list()
    ## check if location is set of coordinates, then scrape
    coord <- grepl("-?[0-9]+\\.[0-9]+.*-?[0-9]+\\.[0-9]+", location)
    if (coord){
            coordinates.clean <- sub(x=location, 
                pattern='.* (-?[0-9]{1,3}\\.[0-9]+,-?[0-9]{1,3}\\.[0-9]+).*', 
                replacement="\\1")
            result[['lat']] <-  sub(x=coordinates.clean, 
                pattern='.*(^-?[0-9]+\\.[0-9]+).*', 
                replacement="\\1")
            result[['lng']] <- sub(x=coordinates.clean, 
                pattern='.*,[[:blank:]]?(-?[0-9]{1,3}\\.[0-9]+).*', 
                replacement="\\1")
            Encoding(result[['lng']]) <- "UTF-8"; Encoding(result[['lat']]) <- "UTF-8"
            geo.info <- coordinates2politics(result[['lat']], result[['lng']], rdstk=rdstk)
            for (p in 1:length(geo.info[[1]]$politics)){
                result[[(geo.info[[1]]$politics[[p]][['friendly_type']])]] <-
                    geo.info[[1]]$politics[[p]][['name']]
            }
    }
    # if not, try to extract coordinates from location
    if (!coord){
        geo <- getCoordinates(location, rdstk=rdstk)    
        if (length(geo$interpretations)>0){
            result[['lat']] <- geo$interpretations[[1]]$feature$geometry$center[[1]]
            result[['lng']] <- geo$interpretations[[1]]$feature$geometry$center[[2]]
            geo.info <- coordinates2politics(result[['lat']], result[['lng']], rdstk=rdstk)
            for (p in 1:length(geo.info[[1]]$politics)){
                result[[(geo.info[[1]]$politics[[p]][['friendly_type']])]] <-
                    geo.info[[1]]$politics[[p]][['name']]
            }
        }
    }
    if (verbose){
        print(location)
        print(unlist(result))
    }
    return(result)
}

getCoordinates <- function (address, session = getCurlHandle(), rdstk) 
{
    api <- paste(rdstk, "/twofishes?query=", 
        sep = "")
    get.addy <- getURL(paste(api, URLencode(address), sep = ""), 
        curl = session)
    result <- fromJSON(get.addy)
    return(result)
}

coordinates2politics <- function (latitude, longitude, session = getCurlHandle(), 
    rdstk="http://www.datasciencetoolkit.org") 
{
    api <- paste(rdstk, "/coordinates2politics/", 
        sep = "")
    result <- getURL(paste(api, latitude, "%2c", longitude, sep = ""), 
        curl = session)
    return(fromJSON(result))
}




