setClass(
  # Set the name for the class
  "FileUpload",
  # Define the slots
  slots = c(
    isfolder         = "logical",
    upload_object    = "character",
    local            = "list",
    dds              = "list",
    project          = "character",
    projectid        = "character"
  ),
  # Set the default values for the slots. (optional)
  prototype=list(
    isfolder         = FALSE,
    upload_object    = "",
    local            = list("filenames"="",
                            "hash_algorithm"="",
                            "hash_value"=""),
    dds            = list("filenames"="",
                            "hash_algorithm"="",
                            "hash_value"=""),
    project          = "",
    projectid        = ""
  )
)

#' DDS RESTful File Upload.
#'
#' @param file_folder The location (UNC path) of a file or folder to be uploaded
#' @param project The DDS project to upload the payload.
#' @return The URL of the resource available on DDS.
#' @examples
#' ddsUpoad(file_folder="stinkbug.png",project="Entomology Library")

ddsUpload<-function(
  file_folder=NULL,
  project=NULL,
  chunk_size_bytes = 1024
  ) {
  if (is.null(file_folder) | is.null(project))
    stop(sprintf('ddsUpload requires valid input for file_folder and project'))
  fileupload                = new("FileUpload")
  fileupload@project        = project
  fileupload@upload_object  = file_folder
  fileupload@isfolder       = file.info(file_folder)$isdir
  fileupload@chunk_size_bytes = chunk_size_bytes
  ################################################
  #do some file checking
  ################################################
  if (!file.exists(fileupload@upload_object))
    stop(sprintf('File/Folder not found: %s',filepath))
  ################################################
  #We have valid input and the file/folder exists on the client
  #Let's obtain a list of files that need to be uploaded
  ################################################
  if (fileupload@isfolder) {
    fileupload@local$filenames = .getFileNamesIntoList(fileupload)
  } else {
    fileupload@local$filenames = fileupload@upload_object
  }
  ################################################
  #If the project is already on DDS, get the id,
  #if not, then POST and get id
  ################################################
  fileupload@projectid = .getProjectId(fileupload)
  ################################################
  #If user submitted a folder, then we need to
  #check DDS to see if the folder structure meets
  #local folder structure. While doing so we will
  #cache folder_ids to make uploading easier
  ################################################
  if (fileupload@isfolder) {

  }
}

.uploadFile <- function(filepath,
                        project_id,
                        chunksizeBytes=chunk_size_bytes) {
      file.info(filepath)$size
      ################################################
      #first order of business (per apiary blueprint) is to create an upload object within dds
      ################################################
      body = list('name'=basename(filepath),
                  'content_type'='application/json',
                  'size' = file.info(filepath)$size,
                  'hash.value' = md5sum(filepath)[[1]],
                  'hash.algorithm' = 'md5')
      r = ddsRequest(customrequest="POST",
                 endpoint=paste0('/projects/',project_id,'/uploads'),
                 body_list=body)
      if (r$status!=201)
        stop(sprintf('in .uploadFile(): endpoint /project/%s/uploads failed with %s',project_id,r$status))
      upload_object_id = r$body$id
      ################################################
      ################################################
      ################################################
      ################################################
      ################################################
      ################################################
      ################################################
      ################################################
      ################################################
      ################################################
      chunkNumber <- 1 # service requires that chunk number be >0
      connection<-file(filepath, open="rb")
      tryCatch(
        {
          fileSizeBytes<-file.info(filepath)$size
          totalUploadedBytes<-0
          repeat {
            chunk <- readBin(con=connection, what="raw", n=chunksizeBytes)
            if (length(chunk)==0) break

            ## get the signed S3 URL
            chunkRequest <- list(chunkNumber=chunkNumber, chunkedFileToken=token)

            curlHandle<-getCurlHandle()

            result<-webRequestWithRetries(
              fcn=function(curlHandle) {
                chunkUploadUrl <- createChunkedFileUploadChunkURL(chunkRequest)
                if (debug) message(sprintf('url= %s\n', chunkUploadUrl))

                httpResponse<-.getURLIntern(chunkUploadUrl,
                                            postfields=chunk, # the request body
                                            customrequest="PUT", # the request method
                                            httpheader=headers, # the headers
                                            curl=curlHandle,
                                            debugfunction=NULL,
                                            .opts=.getCache("curlOpts")
                )
                # return the http response
                httpResponse$body
              },
              curlHandle,
              extraRetryStatusCodes=400 #SYNR-967
            )
            .checkCurlResponse(object=curlHandle, response=result$body, logErrorToSynapse=TRUE)

            totalUploadedBytes <- totalUploadedBytes + length(chunk)
            percentUploaded <- totalUploadedBytes*100/fileSizeBytes
            # print progress, but only if there's more than one chunk
            if (chunkNumber>1 | percentUploaded<100) {
              cat(sprintf("Uploaded %.1f%%\n", percentUploaded))
            }

            chunkResults[[length(chunkResults)+1]]<-chunkNumber
            chunkNumber <- chunkNumber + 1
          }
        },
        finally=close(connection)
      )
}


setMethod(f=".getFileNamesIntoList",
          signature="FileUpload",
          definition=function(Object){
          # A method to extract filenames from an upload object
          dirs <- list.dirs(Object@upload_object)
          file_list = character()
          for (i in 1:length(dirs)) {
            temp <- list.files(dirs[i])
            for (j in 1:length(temp)) {
              if (!file.info(file.path(dirs[i],temp[j]))$isdir) {
                file_list <- c(file_list,file.path(dirs[i],temp[j]))
              }
            }
          }
          return(file_list)
})
setMethod(f=".getProjectId",
          signature="FileUpload",
          definition=function(Object){
          r = ddsRequest(customrequest="GET",
                         endpoint='/projects')
          #Notice could have multiple matches, but we only pull the first - is this the right thing to do?
          #May want to make submitting project id optional
          matchProj = sapply(r$body$results, function(x) if (x$name==Object@project) {return(TRUE)} else {return(FALSE)})
          if (sum(matchProj) > 0) {
            projId = r$body$results[matchProj][[1]]$id
          } else {
            body = list(name=Object@project,
                        description=paste0(Object@project," via R Client .getProjectId()"))
            r = ddsRequest(customrequest="POST",
                           endpoint='/projects',
                           body_list=body)
            if (r$status!=201) {
              stop(sprintf('.getProjectId() failed to create project "%s" on resource %s with status %s',
                           Object@project,
                           .getCache('url'),
                           r$status))
            } else {
              projId = r$body$id
            }
          }
          return(projId)
})



























