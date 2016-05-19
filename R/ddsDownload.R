#' DDS RESTful File Upload.
#'
#' @param local_folder The location (UNC path) of a local folder where files will be saved
#' @param project The DDS project to download.
#' @param chunk_size_bytes optionally, files can be downloaded per chunck, specify download chunk size here.
#' @return Standard Output indicating a successful or failed download.
#' @examples
#' ddsUpload(local_folder="/Users/nn/Desktop/upload_testers",project="UploadTester")
ddsDownload <- function(project=NULL,
                        local_folder=NULL,
                        chunk_size_bytes = 1293892) {
  if (is.null(local_folder) | is.null(project))
    stop(sprintf('ddsDownload requires valid input for local_folder and/or project'))
  transaction                = new("transaction")
  transaction@project        = project
  transaction@local_object   = local_folder
  transaction@isfolder       = file.info(local_folder)$isdir
  if (!transaction@isfolder) {stop("ddsDownload only accepts folders as input to local_folder")}
  ################################################
  #do some file checking
  ################################################
  if (!file.exists(transaction@local_object))
    dir.create(transaction@local_object, showWarnings = TRUE, recursive = FALSE)
    stop(sprintf('File/Folder not found: %s',transaction@local_object))
  ################################################
  #Before we process files locally, let's reach out
  #to DDS and see if/what is already there
  ################################################
  dds_info = .getDDSProjectId(transaction)
  transaction@projectid = dds_info$projId
  transaction@dds$dirs = dds_info$dds$dirs
  transaction@dds$dir_ids = dds_info$dds$dir_ids
  #before we continue, I don't know what multiple filehashes per file would would like, so we'll stop if that happens
  #rewrite on 5/10/16, dds only providing current version of file
  #make sure to rewrite this if condition above ever becomes true
  filehashes = unlist(dds_info$dds$filehashes)
  names(dds_info$dds$filenames) = filehashes
  transaction@dds$filenames = dds_info$dds$filenames
  transaction@dds$fileids = dds_info$dds$fileids



}
