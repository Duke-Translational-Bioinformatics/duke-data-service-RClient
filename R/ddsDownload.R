#' DDS RESTful File Download.
#'
#' @param local_folder The location (UNC path) of a local folder where files will be saved
#' @param project The DDS project to download.
#' @param stop_on_mismatch=TRUE A logical indicating if ddsDownload should abort due to mistached DDS hash and current download
#' @param chunk_size_bytes optionally, files can be downloaded per chunck, specify download chunk size here.
#' @return Standard Output indicating a successful or failed download.
#' @examples
#' ddsDownload(local_folder="/Users/nn31/Desktop/upload_testers",project="UploadTester")
ddsDownload <- function(project=NULL,
                        local_folder=NULL,
                        stop_on_mismatch=TRUE,
                        chunk_size_bytes = 1293892) {
  if (is.null(local_folder) | is.null(project))
    stop(sprintf('ddsDownload requires valid input for local_folder and/or project'))
  transaction                = new("transaction")
  transaction@project        = project
  transaction@local_object   = local_folder
  transaction@isfolder       = file.info(local_folder)$isdir
  if (is.na(transaction@isfolder)) {
    dir.create(local_folder)
    transaction@isfolder = TRUE
  }
  if (!transaction@isfolder) {stop("ddsDownload only accepts folders as input to local_folder")}
  ################################################
  #do some file checking
  ################################################
  if (!file.exists(transaction@local_object))
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
  ################################################
  #We have valid input and the file/folder exists on the client
  #Let's obtain a list of files that need to be uploaded
  ################################################
  if (transaction@isfolder) {
    #we will use relative paths from here on, so stick with that
    user_dir <- getwd()
    setwd(transaction@local_object)
    on.exit(setwd(user_dir))
    transaction@local = .getFileNamesIntoList(transaction)
    transaction@local$filenames = unlist(transaction@local$filenames)
    transaction@local$dirs = unlist(transaction@local$dirs)
  }
  ################################################
  #Now we know what is local and dds, we can do
  #the real heavy lifting, let's first create any
  #folders that are on local, but not dds
  ################################################
  folders_to_add = setdiff(transaction@dds$dirs,transaction@local$dirs)
  folders_to_add = folders_to_add[folders_to_add != ""]
  #creating folders on DDS and storing metadata in transaction@dds
  if (length(folders_to_add)>0) {
    null = lapply(folders_to_add, function(x) dir.create(x))
    transaction@local$dirs = c(transaction@local$dirs,folders_to_add)
    transaction@local$dirs = transaction@local$dirs[transaction@local$dirs != ""]
  }
  ################################################
  #Now all local folders are on DDS, let's put
  #all local files on DDS if not already there,
  #or their hash doesn't match ddsRClient version
  ################################################
  files_to_add = setdiff(transaction@dds$filenames,transaction@local$filenames)
  if (length(files_to_add)>0) {
    for (i in 1:length(files_to_add)) {
      # if (dirname(files_to_add[i])==".") {folderName=NULL}
      # else {
      #   if (transaction@dds$dirs[dirname(files_to_add[i])==transaction@dds$dirs]==dirname(files_to_add[i])) {
      #     folderName = dirname(files_to_add[i])
      #   }
      # }
      .downloadFile(filepath = files_to_add[i],
                    fileid = transaction@dds$fileids[files_to_add[i]==transaction@dds$filenames],
                    file_hash = names(transaction@dds$filenames[transaction@dds$filenames==files_to_add[i]]),
                    stopom=stop_on_mismatch,
                    chunksizeBytes=chunk_size_bytes)
    }
  }

}

.downloadFile <- function(filepath,
                          fileid,
                          file_hash,
                          stopom=stop_on_mismatch,
                          chunksizeBytes){
  message(sprintf('Downloading file %s:',filepath))
  #Let's get the swift URL of the information
  #At some point, let's rewrite
  #http://stackoverflow.com/questions/17306695/httr-get-function-running-out-of-space-when-downloading-a-large-file
  r = ddsRequest(customrequest="GET",
                 endpoint=paste0('/files/',fileid,'/url'))
  file_url = paste0(r$body$host,r$body$url)
  host = r$body$host
  endpt = r$body$url
  #let's plan to update this at some point to be more robust - see above url for tip
  download.file(url = file_url,
                  destfile = filepath,
                  mode='wb')
  #see if hash matches dds, if not, throw a warning providing each hash
  if (!digest(filepath,algo='md5',file=TRUE)==file_hash) {
    if (stopom) {stop(sprintf("Downloaded File (%s) from DDS does not match the DDS reported Hash. If DDS doesn't have a hash, rerun ddsDownload with the stop_on_mismatch=FALSE."))}
    message(sprintf("NOTE: File %s, has a mismatched hash on disk when compared to DDS, the stop_on_mismatch flag is TRUE, so continuing to download to local disk."))
  }
  #throw a warning before overwriting a file, instruct user to change the paramerter to fileOverwrite=True
}
