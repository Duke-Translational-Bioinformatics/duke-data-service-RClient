#' DDS RESTful File Upload.
#'
#' @param file_folder The location (UNC path) of a file or folder to be uploaded
#' @param project The DDS project to upload the payload.
#' @param chunk_size_bytes If determined that files need to be uploaded, specify upload chunk size here.
#' @return The URL of the resource available on DDS.
#' @examples
#' ddsUpload(file_folder="/Users/nn31/Desktop/UploadTester",project="UploadTester")

ddsUpload<-function(
  file_folder=NULL,
  project=NULL,
  chunk_size_bytes = 1293892
  ) {
  if (is.null(file_folder) | is.null(project))
    stop(sprintf('ddsUpload requires valid input for file_folder and project'))
  transaction                = new("transaction")
  transaction@project        = project
  transaction@local_object  = file_folder
  transaction@isfolder       = file.info(file_folder)$isdir
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
  message(sprintf('Collecting Information available on local disk for file/folder: %s.',transaction@local_object))
  message("=====================================================================")
  if (transaction@isfolder) {
    #we will use relative paths from here on, so stick with that
    user_dir <- getwd()
    setwd(transaction@local_object)
    on.exit(setwd(user_dir))
    transaction@local = .getFileNamesIntoList(transaction)
  } else {
    transaction@local = .getFileNamesIntoList(transaction)
  }
  ################################################
  #Now we know what is local and dds, we can do
  #the real heavy lifting, let's first create any
  #folders that are on local, but not dds
  ################################################
  folders_to_add = setdiff(transaction@local$dirs,transaction@dds$dirs)
  folders_to_add = folders_to_add[folders_to_add != ""]
  #creating folders on DDS and storing metadata in transaction@dds
  if (length(folders_to_add)>0) {transaction@dds = .createDDSFolders(transaction,folders_to_add)}
  ################################################
  #Now all local folders are on DDS, let's put
  #all local files on DDS if not already there,
  #or their hash doesn't match ddsRClient version
  ################################################
  files_to_add = setdiff(transaction@local$filenames,transaction@dds$filenames)
  if (length(files_to_add)>0) {
  for (i in 1:length(files_to_add)) {
    if (dirname(files_to_add[i])==".") {folderId=NULL}
    else {
      if (transaction@dds$dirs[dirname(files_to_add[i])==transaction@dds$dirs]==dirname(files_to_add[i])) {
        folderId = transaction@dds$dir_ids[dirname(files_to_add[i])==transaction@dds$dirs]
        folderId = basename(folderId)
      }
    }
    .uploadFile(filepath = files_to_add[i],
                project_id = transaction@projectid,
                file_hash = names(transaction@local$filenames[transaction@local$filenames==files_to_add[i]]),
                folder_id=folderId,
                chunksizeBytes=chunk_size_bytes)
  }
 }
  files_to_version = intersect(transaction@local$filenames,transaction@dds$filenames)
  if (length(files_to_version)>0) {
    for (i in 1:length(files_to_version)) {
      #get the index
      local_hash = names(transaction@local$filenames[transaction@local$filenames==files_to_version[i]])
      dds_hash   = names(transaction@dds$filenames[transaction@dds$filenames==files_to_version[i]])
      if (!(local_hash==dds_hash)) {
        if (dirname(files_to_version[i])==".") {folderId=NULL}
        else {
          if (transaction@dds$dirs[dirname(files_to_version[i])==transaction@dds$dirs]==dirname(files_to_version[i])) {
            folderId = transaction@dds$dir_ids[dirname(files_to_version[i])==transaction@dds$dirs]
            folderId = basename(folderId)
          }
        }
        .uploadFile(filepath = files_to_version[i],
                    project_id = transaction@projectid,
                    file_hash = names(transaction@local$filenames[transaction@local$filenames==files_to_version[i]]),
                    folder_id=folderId,
                    chunksizeBytes=chunk_size_bytes,
                    file_id=transaction@dds$fileids[transaction@dds$filenames==files_to_version[i]]
        )
      }
    }
  }
  message("=====================================================================")
  message(paste0("ddsUpload complete, project updates can now be viewed."))
}

.uploadFile <- function(filepath,
                        project_id,
                        file_hash,
                        folder_id=NULL,
                        chunksizeBytes=chunk_size_bytes,
                        file_id=NULL) {
      ################################################
      #first order of business (per apiary blueprint) is to create an upload object within dds
      ################################################
      body = list('name'=basename(filepath),
                  'content_type'='application/json',
                  'size' = file.info(filepath)$size,
                  'hash' = list('value' = file_hash,
                                'algorithm' = 'md5'))
      r = ddsRequest(customrequest="POST",
                 endpoint=paste0('/projects/',project_id,'/uploads'),
                 body_list=body)
      if (r$status!=201)
        stop(sprintf('in .uploadFile(): endpoint /project/%s/uploads failed with %s',project_id,r$status))
      local_object_id = r$body$id
      chunkNumber <- 1 # service requires that chunk number be >0
      connection<-file(filepath, open="rb")
      tryCatch(
        {
          fileSizeBytes<-file.info(filepath)$size
          totalUploadedBytes<-0
          repeat {
            chunk <- readBin(con=connection, what="raw", n=chunksizeBytes)
            if (length(chunk)==0) break
            ################################################
            ## get a pre-signed URL to upload the chunk. This also ensures that the project container exists in the storage_provider
            ################################################
            body = list('number'=chunkNumber,
                        'size'=length(chunk), #this is the bytes of the chunk, we do this because the last is usually different than specified
                        'hash' = list('value' = digest(chunk,algo='md5',serialize = FALSE),
                                      'algorithm' = 'md5'))
            r = ddsRequest(customrequest="PUT",
                           endpoint=paste0('/uploads/',local_object_id,'/chunks'),
                           body_list=body)
            ################################################
            #Using the response from that call, upload the chunk to the service
            ################################################
            r2 = ddsRequest(
              url=r$body$host, # omitting the endpoint
              endpoint=r$body$url,
              body_list = chunk, # the request body
              customrequest=r$body$http_verb, # the request method
              httpheader=""
            )
            totalUploadedBytes <- totalUploadedBytes + length(chunk)
            percentUploaded <- totalUploadedBytes*100/fileSizeBytes
            # print progress, but only if there's more than one chunk
            if (chunkNumber>1 | percentUploaded<100) {
              cat(sprintf("Uploaded %.1f%%\n", percentUploaded))
            }
            chunkNumber <- chunkNumber + 1
          }
        #Inform DDS that the file is now available in Swift
        r = ddsRequest(customrequest="PUT",
                       endpoint=paste0('/uploads/',local_object_id,'/complete'))
        ################################################
        #Complete the upload process by creating a file
        ################################################
        if (is.null(file_id)) {
          if (is.null(folder_id)) {
            body = list('parent' = list("kind"="dds-project",
                                        "id"=project_id),
                        'upload' = list("id"=local_object_id))

            r = ddsRequest(customrequest="POST",
                           endpoint=paste0('/files'),
                           body_list=body)
            message(sprintf('Added file %s(%s):',filepath,r$body$id))
          } else {
            body = list('parent' = list("kind"="dds-folder",
                                        "id"=folder_id),
                        'upload' = list("id"=local_object_id))
            r = ddsRequest(customrequest="POST",
                           endpoint=paste0('/files'),
                           body_list=body)
            message(sprintf('Added file %s(%s):',filepath,r$body$id))
          }
        } else {
          body = list('upload' = list('id' = local_object_id),
                      'label' = 'File Version via ddsRClient')
          r = ddsRequest(customrequest="PUT",
                         endpoint=paste0('/files/',file_id),
                         body_list=body)
          message(sprintf('Versioned file %s(%s):',filepath,file_id))

          }
        },
        finally=close(connection)
      )
}
