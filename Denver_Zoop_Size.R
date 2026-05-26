#read in the zoop data
zoop <- read.csv("Data/Input/ZoopDB_data_20251016.csv")

#convert to microns 
zoop$mean_length_um <- zoop$mean_length*1000


hist(zoop$mean_length_um, breaks = 50)


hist(zoop$mean_length_um, breaks = 500, xlim = range(0,500))
