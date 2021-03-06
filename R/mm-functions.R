#!/usr/bin/env Rscript
# MAP-MATCHING FUNCIONS
#
# Collection of functions to perform a map-matching of GPS locations using the
# OSRM match service.
#
# Author: Angel J. Lopez @ UGent
# Date: Nov 5th, 2016

library(RCurl)
library(jsonlite)

# Global variables, OSRM services.
options(osrm.server="http://beetle:5000/", osrm.mode="bicycle", osrm.log="log")
#ifelse(!dir.exists(file.path(getwd(), getOption("osrm.log"))), dir.create(file.path(getwd(), getOption("osrm.log"))),FALSE)


#' osrmMatchBuildQuery function
#'
#' Build the request string for the match service in OSRM API v5.
#' @param x Longitude coordinantes (degress).
#' @param y Latitude coordinantes (degress).
#' @param timestamps Array of timestamps (POSIXct) monotonically increasing.
#' @param radiuses Standard deviation of GPS precision used for map matching. If applicable use GPS accuracy.
#' @return a url request string
#' @keywords map-matching
#' @export
#' @examples
#' osrmMatchBuildQuery()
osrmMatchBuildQuery <- function(x, y, timestamps, radiuses=NULL) {
  # coords and timestamaps are collapsed into a single row
  points     <- paste(x,y,sep = ",",collapse = ";")
  timestamps <- paste(as.integer(timestamps), collapse = ";")
  str.radiuses <- ""
  if(!is.null(radiuses)){
    str.radiuses <- paste(radiuses, collapse = ";")
    str.radiuses <- paste("&radiuses=",str.radiuses, sep = "")
  }
  # build the query
  request    <- paste(getOption("osrm.server"),
                      "match/v1/", getOption("osrm.mode"), "/",
                      points,
                      "?timestamps=", timestamps,
                      str.radiuses,
                      "&geometries=geojson&annotations=true",
                      sep="")
  return (request)
}

#' osrmMatchExtractMatchings function
#'
#' Parse the matching output into a dataframe.
#' @param matchings dataframe with the road geometry to be parsed.
#' @return dataframe with the road geometry
#' @keywords map-matching
#' @export
#' @examples
#' osrmMatchExtractMatchings()
osrmMatchExtractMatchings <- function(matchings) {
  coordinates <- data.frame()
  geom.id=1

  # for each continue segment
  for (coord in matchings$geometry$coordinates){
    ds <- as.data.frame(coord)
    names(ds) = c("x_geom","y_geom")
    ds$geom_idx <- geom.id-1
    ds$geom_seq <- seq(length(ds)) # sequence
    ds$geom_confidence   <- matchings$confidence[geom.id]
    ds$geom_duration<- matchings$duration[geom.id]
    ds$geom_distance<- matchings$distance[geom.id]
    coordinates <- rbind(coordinates, ds)
    geom.id = geom.id + 1
  }

  return(coordinates)
}


osrmMatchRequest <- function(request, loc.id, trip.id, seg.id, log=T){
  # Perform a call to the match service
  #
  # Args:
  #   request: Request string
  #   loc.id:  GPS location ids (reference to the orginal points).
  #   trip.id: Trip identifier.
  #   seg.id:  Segment identifier.
  #   log:     Boolean to generate trace files.
  #
  # Returns:
  #   A list with two dataframes:
  #      - matched points, locations that were snapped to the network.
  #      - matching pathway, augmented locations including curves and corners.
  out <- tryCatch(
    {
      # Send the query
      ua <- "'osrm' R package"
      resRaw <- RCurl::getURL(utils::URLencode(request), useragent = ua)
      # Parse the results
      res <- jsonlite::fromJSON(resRaw)

      # Error trigger: api output
      e <- simpleError(res$message)
      if(res$code != "Ok"){ stop(e) }

      # Warning trigger: no records
      w <- simpleWarning("No records found")
      if(nrow(res$tracepoints)==0){ warning(w) }

      # Write log
      if(log){
        logfile=paste(format(Sys.Date(), "%Y%m%d"), trip.id, seg.id, sep=".")
        logfile=paste(getOption("osrm.log"),logfile, sep = "/")
        write(request, file=paste(logfile, "in", "txt", sep="."))
        write(resRaw, file=paste(logfile, "out", "json", sep="."))
      }

      # Tracepoints: Matched locations
      res$tracepoints$location[sapply(res$tracepoints$location, is.null)] <- list(c(NA, NA))
      waypoints <- as.data.frame(matrix(unlist(res$tracepoints$location),byrow=T, ncol=2 ))
      names(waypoints) <- c("x_mm", "y_mm")
      waypoints <- cbind(waypoints, res$tracepoints[,c("waypoint_index","matchings_index","name")])

      # Matchings: Network geometry
      matchings <- osrmMatchExtractMatchings(res$matchings)

      # Add reference
      waypoints$loc_ref <- loc.id; waypoints$trip_ref <- trip.id; waypoints$seg_ref <- seg.id;
      matchings$trip_ref <- trip.id; matchings$seg_ref <- seg.id;

      # Augmented trajectory
      augmented <- osrmMatchAugmentedTrajectory(waypoints, matchings)

      #result
      out = list(waypoints=waypoints,matchings=matchings)
      return(out)
    },
    error=function(cond){
      message(cond)
      return(NULL)
    },
    warning=function(cond){
      message(cond)
      return(NULL)
    }
  )
  return(out)
}


osrmMatchLocations <- function(x, y, timestamps, loc.id, trip.id, seg.id){
  # Control function for performing the map-matching
  #
  # Args:
  #   x: Longitude coordinante (degress).
  #   y: Latitude coordinante (degress).
  #   timestamps: List of timestamps (POSIXct), monotonic increasing.
  #   loc.id:  GPS location ids (reference to the orginal points).
  #   trip.id: Trip identifier.
  #   seg.id:  Segment identifier.
  #
  # Returns:
  #   A list with two dataframes:
  #      - matched points, locations that were snapped to the network.
  #      - matching pathway, augmented locations including curves and corners.

  # add a randon error
  radiuses <- as.integer(runif(length(x),10,20))

  request <- osrmMatchBuildQuery(x,y,timestamps,radiuses)
  out <- osrmMatchRequest(request, loc.id, trip.id, seg.id)
  return(out)
}


osrmMatch <- function(x, y, timestamps, loc.id, trip.id){
  # Perform a map-matching at trip level
  #
  # Args:
  #   x: List of longitude coordinantes (degress).
  #   y: List of latitude coordinantes (degress).
  #   timestamps: List of timestamps (POSIXct), monotonic increasing.
  #   loc.id:  GPS location ids (reference to the orginal points).
  #   trip.id: Trip identifier (single value).
  #
  # Returns:
  #   A list with two dataframes:
  #      - matched points, locations that were snapped to the network.
  #      - matching pathway, augmented locations including curves and corners.
  n<-length(x)
  LIMIT=90
  idx.segment <- 1

  waypoints <- data.frame()
  matchings <- data.frame()

  # more than 100 points (limit on OSRM)
  idx.split <- seq(1,n,by = LIMIT)[-1]
  idx.split <- c(idx.split, n)

  idx.start=1
  for (idx in idx.split){
    idx.end=idx
    if(idx.start>idx.end) {break()}
    # match locations
    idxs=idx.start:idx.end
    out.match <- osrmMatchLocations(x[idxs],y[idxs],timestamps[idxs],loc.id[idxs],trip.id, idx.segment)

    if (!is.null(out.match)){
      out.waypoints <- out.match[["waypoints"]]
      out.matchings <- out.match[["matchings"]]

      out.waypoints$idx <- idxs
      if (nrow(waypoints)==0){
        waypoints <- out.waypoints
        matchings <- out.matchings
      }else {
        waypoints <- rbind(waypoints, out.waypoints)
        matchings <- rbind(matchings, out.matchings)
      }
    }else {
      # create a null entrance as a output
      out.waypoints <- data.frame(x_mm=NA,y_mm=NA,waypoint_index=NA,matchings_index=NA,name=NA,
                              loc_ref=loc.id[idxs], trip_ref=trip.id, seg_ref=idx.segment, idx=idxs)
      waypoints = rbind(waypoints, out.waypoints)
    }
    # Next segment
    idx.segment <- idx.segment + 1
    idx.start   <- idx+1
  }
  if (nrow(waypoints)==0) { return(NULL) }

  out = list(waypoints=waypoints,matchings=matchings)
  return(out)
}

#' osrmMatchBatch function
#'
#' It performs the map-matching in batch.
#' @param x Longitude coordinantes (degress).
#' @param y Latitude coordinantes (degress).
#' @param timestamps Array of timestamps (POSIXct) monotonically increasing.
#' @param loc.id GPS location identifiers (reference to the orginal points).
#' @param trip.id Trip identifiers.
#' @return A list with two dataframes:
#'     (1) waypoints, location points that were snapped to the network.
#'     (2) matchings, network path including curves and corners.
#' @keywords map-matching
#' @export
#' @examples
#' # create a log directory in your working directory
#' ifelse(!dir.exists(file.path(getwd(), getOption("osrm.log"))),
#'         dir.create(file.path(getwd(), getOption("osrm.log"))),FALSE)
#' # run the batch process
#' out.mm <- osrmMatchBatch(ds3$longitude, ds3$latitude, ds3$timestamp,ds3$id, ds3$tripid)
osrmMatchBatch <- function(x, y, timestamps, loc.id, trip.id){
  n<-length(x)
  trips <- unique(trip.id)
  waypoints <- data.frame()
  matchings <- data.frame()
  for (t.id in trips){
    idxs <- which(trip.id == t.id)
    out.match <- osrmMatch(x[idxs],y[idxs],timestamps[idxs],loc.id[idxs], t.id)
    if (!is.null(out.match)){
      out.waypoints <- out.match[["waypoints"]]
      out.matchings <- out.match[["matchings"]]

      if (nrow(waypoints)==0){
        waypoints <- out.waypoints
        matchings <- out.matchings
      }else {
        waypoints <- rbind(waypoints, out.waypoints)
        matchings <- rbind(matchings, out.matchings)
      }
    }
  }
  if (nrow(waypoints)==0) { return(NULL) }

  out = list(waypoints=waypoints,matchings=matchings)
  return(out)
}

#' segmentPartition function
#'
#' It performs a segmentation using the dwell time as parameter.
#' @param dwell Time in seconds for segmenting the timestamps.
#' @param timestamps Array of timestamps (POSIXct) monotonically increasing.
#' @param group Array of the grouping variable (device) with same length as timestamps.
#' @return A dataframe with the ids (tripid)
#' @keywords dwell segmentation
#' @export
#' @examples
#' segmentPartition()
segmentPartition <- function(dwell, timestamp, group){
  #logfile
  logfile <- file("segmentPartition.log",open="a")
  # Timediff
  n = length(timestamp)
  timediff <- rep(NA, n)
  timediff[2:n] = timestamp[2:n]-timestamp[1:(n-1)]

  # group partition: identification of the device
  partition.group=rep(F,n)
  partition.group[2:n] = group[2:n]!=group[1:(n-1)]

  # Set a big number for the timediff between different devices
  BIG=100000
  timediff[partition.group] = BIG

  # Set a segment partition based on a dwell = 60 seconds
  partition.segment=timediff>dwell
  partition.segment[1]=F

  # set a identifier for each partition segment
  idx.segments = which(partition.segment)
  idx.start = 1
  id.seq = 1
  id = rep(-1, n)
  loc_seq = rep(NA,n)
  for (idx in idx.segments){

    idx.end = idx-1

    # set the id
    idxs=idx.start:idx.end
    id[idxs]= id.seq
    # set a sequencial number within segment
    loc_seq[idxs]=(idxs)-(idx.start-1)

    # next seq
    id.seq=id.seq+1
    idx.start= idx

    # message
    msg1 = paste("tripid:", id.seq, "obs:",length(idxs))
    cat(msg1, file = logfile, sep="\n")
  }
  # last segment
  idxs=idx.start:n
  id[idxs]= id.seq
  loc_seq[idxs]=(idxs)-(idx.start-1)
  # message
  msg1 = paste("tripid:", id.seq, "obs:",length(idxs))
  cat(msg1, file = logfile, sep="\n")
  close(logfile)

  # result
  timediff[partition.group] = NA
  segments <- data.frame(tripid=id, loc_seq=loc_seq, trip_head=partition.segment,difftime=timediff)
  segments$trip_head[1]=T
  return(segments)
}
osrmMatchAugmentedTrajectory <- function(df.waypoints,df.matchings){

  matching.ids = unique(df.waypoints$matchings_index[!is.na(df.waypoints$matchings_index)])

  out <- data.frame()
  # select matchings segments
  for(id in matching.ids){
    # create subset
    waypoints <- df.waypoints[which(df.waypoints$matchings_index == id), ]
    matchings <- df.matchings[which(df.matchings$geom_idx == id), ]

    # end index
    end.w <- nrow(waypoints)
    end.m <- nrow(matchings)

    # comparision field
    waypoints$latlon <- with(waypoints, x_mm + y_mm)
    matchings$latlon <- with(matchings, x_geom + y_geom)

    # Align vectors
    # Elements that does exist in both waypoints and matchings
    idx.m.in <- which((matchings$latlon %in% waypoints$latlon))
    idx.w.in <- which((waypoints$latlon %in% matchings$latlon))

    # all elements exist in waypoint (do nothing)
    if(end.m==length(idx.w.in)){
      out.loc <- data.frame(x=waypoints$x_mm,
                            y=waypoints$y_mm,
                            loc_ref=waypoints$loc_ref,
                            geom_idx=id, geom_seq=seq(end.w))
      out <- rbind(out, out.loc)
      # loop exit
      break
    }


    out.loc <- data.frame()
    # it is the first element in matchings
    if(idx.m.in[1]==1){
      # copy from waypoints up to first match
      idxs <- seq(idx.w.in[1])
      out.loc <- data.frame(x=waypoints$x_mm[idxs],
                            y=waypoints$y_mm[idxs],
                            loc_ref=waypoints$loc_ref[idxs],
                            geom_idx=id, geom_seq=NA)

    }else {
      # copy from waypoint up to first match - 1
      idxs <- seq(idx.w.in[1]-1)
      out.loc1 <- data.frame(x=waypoints$x_mm[idxs],
                             y=waypoints$y_mm[idxs],
                             loc_ref=waypoints$loc_ref[idxs], geom_idx=id, geom_seq=NA)
      # copy from matchings up to first match
      idxs <- seq(idx.m.in[1])
      out.loc2 <- data.frame(x=matchings$x_geom[idxs],
                             y=matchings$y_geom[idxs], loc_ref=NA, geom_idx=id, geom_seq=NA)

      out.loc=rbind(out.loc1,out.loc2)

    }

    # update output
    out <- rbind(out, out.loc)
    # update indexes
    idx.w <- idx.w.in[1]+1
    idx.m <- idx.m.in[1]+1

    # check for extra points
    while(T){
      if(waypoints$x_mm[idx.w]==matchings$x_geom[idx.m] & waypoints$y_mm[idx.w]==matchings$y_geom[idx.m]){
        # add waypoint
        out.loc <- data.frame(x=waypoints$x_mm[idx.w], y=waypoints$y_mm[idx.w],
                              loc_ref=waypoints$loc_ref[idx.w], geom_idx=id, geom_seq=NA)
        idx.w <- idx.w+1
        idx.m <- idx.m+1
      } else{
        # add extra point
        out.loc <- data.frame(x=matchings$x_geom[idx.m], y=matchings$y_geom[idx.m],
                              loc_ref=NA, geom_idx=id, geom_seq=NA)
        idx.m <- idx.m+1
      }
      out <- rbind(out, out.loc)

      # Check for idx out of the range
      #if(idx.w > end.w & idx.m < end.m){ # matchings left
      #  out.loc <- data.frame(x=matchings$x_geom[idx.m:end.m],
      #                        y=matchings$y_geom[idx.m:end.m], loc_ref=NA)
      #}
      if(idx.w <= end.w & idx.m > end.m){ # waypoints left
        out.loc <- data.frame(x=waypoints$x_mm[idx.w:end.w],
                              y=waypoints$y_mm[idx.w:end.w],
                              loc_ref=waypoints$loc_ref[idx.w:end.w], geom_idx=id, geom_seq=NA)
      }
      if(idx.w > end.w | idx.m > end.m){ # exit
        out <- rbind(out, out.loc)
        break()
      }
    }

    # add sequence
    out$geom_seq <- seq(nrow(out))
  }

  # add reference
  out$seg_ref <- unique(df.waypoints$seg_ref)
  out$trip_ref <- unique(df.waypoints$trip_ref)

  return(out)
}

osrmMatchAugmentedTrajectoryBatch <- function(df.waypoints,df.matchings){
  trip.ids <- unique(df.matchings$trip_ref)
  out <- data.frame()
  for (t in trip.ids){
    seg.ids <- unique(df.matchings$seg_ref[which(df.matchings$trip_ref %in% t)])
    for(s in seg.ids){
      idx.w <- which(with(df.waypoints, trip_ref %in% t & seg_ref %in% s))
      idx.m <- which(with(df.matchings, trip_ref %in% t & seg_ref %in% s))
      out.aug <- osrmMatchAugmentedTrajectory(df.waypoints[idx.w, ],df.matchings[idx.m, ])
      out <- rbind(out, out.aug)
    }
    message(t)
  }
  return (out)
}


#out.aug <- osrmMatchAugmentedTrajectory2(df.waypoints,df.matchings)
#out.aug2 <- osrmMatchAugmentedTrajectoryBatch(df.waypoints,df.matchings)


