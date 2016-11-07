

## Install 

```r 
install_github('osrmMatch','alopezag')
```

### Global variable 

Set the OSRM server and create a log folder in the current working directory.

```r 
options(osrm.server="http://beetle:5000/", 
        osrm.mode="bicycle", 
        osrm.log="log")

ifelse(!dir.exists(file.path(getwd(), getOption("osrm.log"))), 
        dir.create(file.path(getwd(), getOption("osrm.log"))),FALSE)
```