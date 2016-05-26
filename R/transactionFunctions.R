setMethod(f=".createDDSFolders",
          signature="transaction",
          definition=function(Object,add_folder_list){

            created_dirs = list('dirs'    = '',
                                'dir_ids' = '')
            for (i in 1:length(add_folder_list)) {
              #find out if any of the parent folders match folders already on dds
              indx = ( dirname(add_folder_list[i])==Object@dds$dirs )
              if (sum(indx)>1) {stop(".createDDSFolders() found multiple base directories matching a folder to be added, intervention needed.")}
              else if (sum(indx)==1) {
                base_dir_ids = Object@dds$dir_ids[indx]
                body = list("parent"=list("kind"="dds-folder",
                                          "id"=basename(base_dir_ids)),
                            "name"=basename(add_folder_list[i]))
                r = ddsRequest(customrequest="POST",
                               endpoint=paste0('/folders'),
                               body_list=body)
                ids = paste0(base_dir_ids,'/',r$body$id)
                Object@dds$dirs = c(Object@dds$dirs,add_folder_list[i])
                Object@dds$dir_ids = c(Object@dds$dir_ids, ids)
                message(sprintf('Adding folder %s(%s) to DDS within project %s(%s).',add_folder_list[i],r$body$id,Object@project,Object@projectid))
              } else {
                body = list("parent"=list("kind"="dds-project",
                                          "id"=Object@projectid),
                            "name"=basename(add_folder_list[i]))
                r = ddsRequest(customrequest="POST",
                               endpoint=paste0('/folders'),
                               body_list=body)
                Object@dds$dirs = c(Object@dds$dirs,add_folder_list[i])
                Object@dds$dir_ids = c(Object@dds$dir_ids, r$body$id)
              }
            }
            return(Object@dds)
})

setMethod(f=".getFileNamesIntoList",
          signature="transaction",
          definition=function(Object){
            message(sprintf('Collecting Information available on local disk for file/folder: %s.',Object@local_object))
            message("=====================================================================")
            # A method to fill in local metadata about file/directory structure
            if (Object@isfolder) {
              filenames         = list.files(recursive=TRUE,full.names=TRUE)
              filenames         = gsub("^.|^./","",filenames)
              filenames_full    = list.files(Object@local_object,recursive=TRUE,full.names=TRUE)
              filehashes        = sapply(filenames_full, function(x) digest(x,algo='md5',file=TRUE))
              names(filenames) = filehashes
              dirs              = list.dirs(recursive=TRUE,full.names=TRUE)
              dirs              = gsub("^.|^./","",dirs)
              dirs              = dirs[dirs != ""]
            } else {
              filenames         = basename(Object@local_object)
              names(filenames)  = digest(Object@local_object,algo='md5',file=TRUE)
              dirs              = Object@local$dirs
            }
            return( list("filenames" = ifelse(length(filenames)==0,"",list(filenames)),
                         "dirs"      = ifelse(length(dirs)==0,"",list(dirs)))
            )
})

parseChildren <- function(children, #Children is a response body from one of the following DDS endpoints: /projects/{id}/children or /folders/{id}/children
                          wtl = list(
                            dirs       = character(),
                            dir_ids    = character(),
                            filenames  = character(),
                            fileids    = character(),
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
        file_info       = ddsRequest(customrequest="GET",endpoint=paste0('/files/',temp$id))
        #find out the index of where the parent lives in the directory base
        ind             = which(temp$parent$id==basename(wtl$dir_ids))
        wtl$filenames   = c(wtl$filenames,paste0(wtl$dirs[ind],'/',temp$name))
        wtl$fileids     = c(wtl$fileids,file_info$body$id)
        wtl$filehashes  = c(wtl$filehashes,ifelse(is.null(file_info$body$current_version$upload$hash$value),"",file_info$body$current_version$upload$hash$value))
      }
      else {
        file_info        = ddsRequest(customrequest="GET",endpoint=paste0('/files/',temp$id))
        wtl$filenames    = c(wtl$filenames,temp$name)
        wtl$fileids      = c(wtl$fileids,file_info$body$id)
        wtl$filehashes   = c(wtl$filehashes,ifelse(is.null(file_info$body$current_version$upload$hash$value),"",file_info$body$current_version$upload$hash$value))
        algorithm        = ifelse(is.null(file_info$body$current_version$upload$hash$algorithm),"",file_info$body$current_version$upload$hash$algorithm)
        if (!(algorithm %in% c("","md5"))) {stop("DDS File contains hash that is not supported in ddsRClient.")}
        rm(algorithm)
      }
    }
  }
  return(wtl)
}

setMethod(f=".getDDSProjectId",
          signature="transaction",
          definition=function(Object){
            message(sprintf('Querying DDS for projects on DDS that match %s',Object@project))
            r = ddsRequest(customrequest="GET",
                           endpoint='/projects')
            dds = Object@dds
            #Notice could have multiple matches, but we only pull the first - is this the right thing to do?
            #May want to make submitting project id optional
            matchProj = sapply(r$body$results, function(x) if (x$name==Object@project) {return(TRUE)} else {return(FALSE)})
            if (sum(matchProj) > 1) {stop("More than 1 project exists by that name, ddsRClient currently doesn't support these types of projects")}
            if (sum(matchProj) > 0) {
              projId = r$body$results[matchProj][[1]]$id
              #Now let's get all of the informaiton associated with this project
              r = ddsRequest(customrequest="GET",
                             endpoint=paste0('/projects/',projId,'/children'))
              message(sprintf('Stepping through DDS project %s(%s).',Object@project,projId))
              if (length(r$body$results)>0) {
                dds = parseChildren(r$body$results)
              }
            } else {
              body = list(name=Object@project,
                          description=paste0(Object@project," via R Client .getProjectId()"))
              r = ddsRequest(customrequest="POST",
                             endpoint='/projects',
                             body_list=body)
                projId = r$body$id
                message(sprintf('Creating DDS project %s(%s):',Object@project,projId))
            }
            if (sum(duplicated(dds$dirs))>0) {stop(sprintf("'%s' contains duplicate folder structures, this is supported within DDS portal, but not the R Client (at this time).", Object@project))}
            if (sum(duplicated(dds$filenames))>0) {stop(sprintf("'%s' contains duplicate file structures, this is supported within DDS portal, but not the R Client (at this time).", Object@project))}
            out = list("projId"=projId,
                       "dds"=dds)
            return(out)
})
