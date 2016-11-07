#!/usr/bin/env Rscript
# MAP-MATCHING FUNCIONS
#
# Collection of functions to perform a map-matching of GPS locations using the
# OSRM match service.
#
# Author: Angel J. Lopez @ UGent
# Date: Nov 5th, 2016

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
