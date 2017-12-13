################################################################################
#' Download and store hash value from downloaded file
#'
#' To avoid redownloading existing files, verify if the file exists locally.
#' If the file exists, compare the checksum value to that logged by original download.
#'
#' @param inputs A character string. Represents the url of file to be downloaded.
#'
#' @param cacheRepo A repository used for storing cached objects.
#'
#' @param mask A character string. A shapefile used to crop downloaded rasters or shapefiles to a specific region
#'
#' @param quick Logical. If \code{TRUE}, checksum is compiled using the combination
#'              of the filename and its size.
#'              If \code{FALSE} (default), cheksum is compiled using the object.
#'
#' @param cascade Logical. If \code{FALSE}, file is untar and/or unzip. Default is \code{FALSE}.
#'
#' @param quiet Logical. If \code{TRUE}, suppress status messages (if any), and the progress bar.
#'
#' @return Invoked for its side-effect of downloading files to the \code{destfile/} directory.
#'
#' @author Melina Houle
#' @author Jean Marchal
#' @docType methods
#' @export
#' @importFrom data.table data.table
#' @importFrom methods is
#' @importFrom raster raster crop mask writeRaster
#' @importFrom reproducible Cache
#' @importFrom sf read_sf st_intersection st_write
#' @importFrom tools file_ext
#' @importFrom utils download.file
#' @rdname downloadInputs
#'
#' @examples
#' downloadInputs("NFDB_PT")
#'
downloadInputs <- function(inputs, cacheRepo = NULL, mask = NULL, quick = FALSE, cascade = FALSE, quiet = TRUE)
{
  downloadInputsImpl <- function(files, repo, mask, quick = FALSE, cascade = FALSE, quiet = TRUE)
  {
    for (file in files)
      download.file(file, destfile = file.path(repo, basename(file)), method = "auto", mode = "wb", quiet = quiet)

    # Cascade = TRUE will untar / unzip all files
    if (cascade)
    {
      c(
        lapply (
          files,
          function(file)
          {
            if (file_ext(file) == "zip")
            {
              fun <- unzip
            }
            else if (file_ext(file) == "tar")
            {
              fun <- untar
            }
            fun(file, exdir = repo)
          }
        ),
        files) -> files
    }

    if (!is.null(mask))
    {
      mask <- read_sf(mask)
      for (file in files)
      {
        if (tolower(file_ext(file)) == "shp")
        {
          from <- read_sf(file)
          to <- st_intersection(from, mask)
          st_write(to, file)
        }
        else if (tolower(file_ext(file)) %in% c("tif", "tiff"))
        {
          from <- raster(file)
          to <- crop(mask(from, mask), mask)
          writeRaster(paste0(file_path_sans_ext(file), ".tif"), overwrite = TRUE)
        }
      }
    }
  }

  paths <- unlist(urls[dataset %in% inputs, lapply(files, function(file) file.path(url, file))])

  if (length(paths) == 0)
    stop("This dataset is not present in webDatabases::urls yet. Please submit a pull request to the PredictiveEcology/webDatabases repo to add it.")

  if (is.null(cacheRepo))
  {
    cacheRepo <- reproducible::.checkCacheRepo(cacheRepo, create = TRUE)
  }
  invisible(Cache(downloadInputsImpl, files = paths, repo = cacheRepo, mask = mask, quick = quick, cascade = cascade, quiet = quiet, cacheRepo = cacheRepo))
}

