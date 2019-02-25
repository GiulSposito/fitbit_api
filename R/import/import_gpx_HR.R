library(XML)
library(lubridate)
library(tidyverse)

# read gpx file
gpx_file <- htmlTreeParse("./data/Visconde_de_Sotello_e_Moenda.gpx", useInternalNodes = T)

# trackpoint structure
# <trkpt lat="-22.7036870" lon="-46.7560630">
#   <ele>675.1</ele>
#   <time>2019-02-24T11:13:36Z</time>
#   <extensions>
#     <gpxtpx:TrackPointExtension>
#       <gpxtpx:hr>105</gpxtpx:hr>
#     </gpxtpx:TrackPointExtension>
#   </extensions>
# </trkpt>


# extract (by xpath) times
dtime <- xpathSApply(gpx_file, path = "//trkpt/time", xmlValue) 
hr    <- xpathSApply(gpx_file, path = "//trkpt/extensions/trackpointextension/hr", xmlValue) 

# compose a tiblle
polar_hr <- tibble(
  timestamp = ymd_hms(dtime),
  hr        = as.integer(hr)
)

glimpse(polar_hr)

# plot data
polar_hr %>% 
  ggplot() +
  geom_line(aes(x=timestamp, y=hr, color=hr)) +
  scale_color_gradient(name="heart rate (bpm)",low="green", high="red") + 
  theme_minimal()
