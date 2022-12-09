rm(list=ls())
gc()

##
in.folder = "C:/Users/cormi/Documents/ImageProcessing/Reference_May/LC08_L1TP_014042_20160508_20200907_02_T1"
out.folder = "C:/Users/cormi/Documents/ImageProcessing/Reference_May"
#change the date to match today's date
date<-paste(format(Sys.time(), "acolite_settings_%Y%m%d_%I%p."),"txt", sep = "")
out.settings = date
##

#Read in settings file
set.file = read.delim("C:/Users/cormi/Documents/acolite_py_win/config/landsat.txt",header=F)#Acolite default
#define input file
set.file[1,1] = paste("inputfile=",in.folder,sep="")
#define output file
set.file[2,1] = paste("output=", out.folder,sep="")
set.file[3,1] = "limit=25.663219,-79.324705,25.801764,-79.198116"
set.file[4,1] = "dsf_aot_estimate=fixed"
#set.file[5,1] = l2w_mask_threshold=0.035
#set.file[6,1] = l2w_mask_high_toa_threshold=0.31
#do a sunglint correction - seems this is all already in the file I have so no need to run code
#set.file[33,1] = "glint_mask_rhos_band=1600"#change mask band
#set.file[34,1] = "glint_mask_rhos_threshold=0.05"#change mask default of 0.05
#set.file[35,1] = "glint_write_rhog_ref=False"
#set.file[37,1] = "glint_write_rhog_all=False"
#write l2w map
#set.file[82,1] =  "map_l2w=True"

#to fix error "ValueError: The requested sample points xi have dimension 6, but this RegularGridInterpolator has dimension 5" based on website help 
#from https://odnature.naturalsciences.be/remsem/acolite-forum/viewtopic.php?t=321
#set.file[98,1]="dsf_interface_reflectance=True"

#write out the settings file
write.table(set.file, paste(out.folder,"/",out.settings,sep=""),row.names=F,col.names=F,quote=F )


#Acolite call, have to provide the full paths for it to work
cmd = paste("C:/Users/cormi/Documents/acolite_py_win/dist/acolite/acolite.exe --cli --settings=",out.folder,"/",out.settings,sep="")
shell(cmd) #run acolite



