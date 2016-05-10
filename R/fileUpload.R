setClass(
  # Set the name for the classes
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
                            "dirs"=""),
    dds              = list("filenames"="",
                            "dirs"="",
                            "dir_ids"=""),
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
#' ddsUpoad(file_folder="/Users/nn31/Desktop/test_pics",project="Entomology Library")

ddsUpload<-function(
  file_folder=NULL,
  project=NULL,
  chunk_size_bytes = 1293892
  ) {
  if (is.null(file_folder) | is.null(project))
    stop(sprintf('ddsUpload requires valid input for file_folder and project'))
  fileupload                = new("FileUpload")
  fileupload@project        = project
  fileupload@upload_object  = file_folder
  fileupload@isfolder       = file.info(file_folder)$isdir
  ################################################
  #do some file checking
  ################################################
  if (!file.exists(fileupload@upload_object))
    stop(sprintf('File/Folder not found: %s',filepath))
  ################################################
  #Before we process files locally, let's reach out
  #to DDS and see if/what is already there
  ################################################
  dds_info = .getDDSProjectId(fileupload)
  fileupload@projectid = dds_info$projId
  fileupload@dds$dirs = dds_info$dds$dirs
  fileupload@dds$dir_ids = dds_info$dds$dir_ids
  #before we continue, I don't know what multiple filehashes per file would would like, so we'll stop if that happens
  inddx = sapply(dds_info$dds$filehashes, function(x) (length(x)>1))
  if (sum(inddx)>0) {
    stop(sprintf("At least one file within DDS contains multiple hashes, ddsRclient currently cannot handle this situation."))
  }
  #make sure to rewrite this if condition above ever becomes true
  filehashes = unlist(dds_info$dds$filehashes)
  names(filehashes) = dds_info$dds$filenames
  fileupload@dds$filehashes = filehashes

  ################################################
  #We have valid input and the file/folder exists on the client
  #Let's obtain a list of files that need to be uploaded
  ################################################
  if (fileupload@isfolder) {
    fileupload@local = .getFileNamesIntoList(fileupload)
  } else {
    fileupload@local$filenames = fileupload@upload_object
  }
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
                        file_hash,
                        folder_id=NULL,
                        chunksizeBytes=chunk_size_bytes) {
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
      upload_object_id = r$body$id
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
                           endpoint=paste0('/uploads/',upload_object_id,'/chunks'),
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
                       endpoint=paste0('/uploads/',upload_object_id,'/complete'))
        ################################################
        #Complete the upload process by creating a file
        ################################################
        if (is.null(folder_id)) {
          body = list('parent' = list("kind"="dds-project",
                                      "id"=project_id),
                      'upload' = list("id"=upload_object_id))

          r = ddsRequest(customrequest="POST",
                         endpoint=paste0('/files'),
                         body_list=body)
        } else {
          body = list('parent' = list("kind"="dds-folder",
                                      "id"=folder_id),
                      'upload' = list("id"=upload_object_id))
          r = ddsRequest(customrequest="POST",
                         endpoint=paste0('/files'),
                         body_list=body)
        }
        },
        finally=close(connection)
      )
}
setMethod(f=".getFileNamesIntoList",
          signature="FileUpload",
          definition=function(Object){
          user_dir <- getwd()
          setwd(Object@upload_object)
          # A method to fill in local metadata about file/directory structure
          filenames         = list.files(recursive=TRUE,full.names=TRUE)
          filenames         = gsub("^.|^./","",filenames)
          filenames_full    = list.files(Object@upload_object,recursive=TRUE,full.names=TRUE)
          filehashes        = sapply(filenames_full, function(x) digest(x,algo='md5',file=TRUE))
          names(filenames) = filehashes
          dirs              = list.dirs(recursive=TRUE,full.names=TRUE)
          dirs              = gsub("^.|^./","",dirs)
          dirs              = dirs[dirs != ""]
          on.exit(setwd(user_dir))
          return( list("filenames" =filenames,
                       "dirs"       =dirs)
                  )
})

parseChildren <- function(children, #Children is a response body from one of the following DDS endpoints: /projects/{id}/children or /folders/{id}/children
                          wtl = list(
                            dirs = character(),
                            dir_ids = character(),
                            filenames = character(),
                            filehashes = character())
                          ){
  for (i in 1:length(children)) {
    temp = children[[i]]
    if (temp$kind=="dds-folder") {
      if (temp$parent$kind=="dds-folder") {
      #find out the index of where the parent lives in the directory base
      ind     = which(temp$parent$id==basename(wtl$dir_ids))
      wtl$dirs    = c(wtl$dirs,paste0(wtl$dirs[ind],'/',temp$name))
      wtl$dir_ids  = c(wtl$dir_ids,paste0(wtl$dir_ids[ind],'/',temp$id))
      #paste0('/',basename(fileupload@upload_object),'/',paste(names,collapse='/'))
      } else {
        wtl$dirs    = c(wtl$dirs,temp$name)
        wtl$dir_ids = c(wtl$dir_ids,temp$id)
      }
      #After gathering information about that folder, let's see what that folder's children looks like
      r = ddsRequest(customrequest="GET",
                         endpoint=paste0('/folders/',temp$id,'/children'))
      if (length(r$body$results)>0) {
          wtl = Recall(r$body$results,wtl)
      }
    }
    else if (temp$kind=="dds-file") {
      if (temp$parent$kind=="dds-folder") {
        #get the upload hash id
        ################################################
        # CHANGE THIS POSSIBLY, depending on issue #9
        ################################################
        file_info = ddsRequest(customrequest="GET",endpoint=paste0('/files/',temp$id))
        #find out the index of where the parent lives in the directory base
        ind     = which(temp$parent$id==basename(wtl$dir_ids))
        wtl$filenames = c(wtl$filenames,paste0(wtl$dirs[ind],'/',temp$name))
        wtl$filehashes = c(wtl$filehashes,ifelse(is.null(file_info$body$upload$hash),"",file_info$body$upload$hash))
        #paste0('/',basename(fileupload@upload_object),'/',paste(names,collapse='/'))
      }
      else {
        file_info = ddsRequest(customrequest="GET",endpoint=paste0('/files/',temp$id))
        wtl$filenames = c(wtl$filenames,temp$name)
        wtl$filehashes = c(wtl$filehashes,ifelse(is.null(file_info$body$upload$hash),"",file_info$body$upload$hash))
      }
    }
  }
  return(wtl)
}

setMethod(f=".getDDSProjectId",
          signature="FileUpload",
          definition=function(Object){
          dds = list()
          r = ddsRequest(customrequest="GET",
                         endpoint='/projects')
          #Notice could have multiple matches, but we only pull the first - is this the right thing to do?
          #May want to make submitting project id optional
          matchProj = sapply(r$body$results, function(x) if (x$name==Object@project) {return(TRUE)} else {return(FALSE)})
          if (sum(matchProj) > 0) {
            projId = r$body$results[matchProj][[1]]$id
            #Now let's get all of the informaiton associated with this project
            r = ddsRequest(customrequest="GET",
                           endpoint=paste0('/projects/',projId,'/children'))
            if (length(r$body$results)>0) {
              dds = parseChildren(r$body$results)
            }
            #We need to create a metadata configuration for file/structure and file/information

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
          out = list("projId"=projId,
                     "dds"=dds)
          return(out)
})



























