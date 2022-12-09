rm(list=ls())
gc()

##
folder = "D:/20122013"
#the list of satellite images that are in your folder (just their L1 product identifier)
sat.data = read.csv("D:/SatelliteImagesDownloaded2012_2013.csv")
#turns the csv file of images and other details into just a list of satellite names
sat.images.list = sat.data$Landsat.Product.Identifier.L1
#this was another optiojn I tried, but I couldn't figure out how to get it to work
#sat.images.list = (list.dirs(folder))
#sat.images.list  = sat.images.list[-1]

rm(sat.data)

#this starts a loop which runs through each satellite in the folder specified above
#based on the list of satellite images from sat.images.list, everything within the {}
# will be run

for (ii in 1:length(sat.images.list))
{
  #this code gives you updates of which image is being processed, the text in quotes can be changed to whatever you like
  print(paste("Running ", ii, " of ", length(sat.images.list), " in satellite images list", sep = ""))

  #this changes the in.folder for ewach lop to match to whichever satellite image you are working on
  in.folder = paste(folder,"/", sat.images.list[ii], sep = "")
  out.folder = "D:/output"
  #change the date to match today's date
  date<-paste(format(Sys.time(), "acolite_settings_%Y%m%d_%I%p."),"txt", sep = "")
  #changes the name of the out.settings to be the name of the satellite image being processed and the date
  out.settings = paste (sat.images.list[ii], date, sep="")
  ##
  
  #Read in settings file (this is blank file which you fill in with the info below)
  set.file = read.delim("C:/Users/cormi/Documents/acolite_py_win/config/landsat.txt",header=F)#Acolite default
  #define input file (sets input file to the image [ii])
  set.file[1,1] = paste("inputfile=",in.folder,sep="")
  #define output file (sets where the output image will be put)
  set.file[2,1] = paste("output=", out.folder,sep="")
  #crops the image to the coordinates that you want, in this case for Bimini it had to be cropped a lot
  #this decreases processing time
  set.file[3,1] = "limit=25.663219,-79.324705,25.801764,-79.198116"
  # a very important fix for the BImini image because it is a very small image and acolite overfits dsf 
  #if it is tiled instead of fixed and near-infrared values become negative
  set.file[4,1] = "dsf_aot_estimate=fixed"
  #the last few lines just remove extra files thta we don't need for the loop and allow the next step of processes in
  #the preprocessing file to run smoothly
  set.file[5,1] = "l1r_delete_netcdf=True"
  set.file[6,1] = "l2r_pans_delete_netcdf=True"
  set.file[7,1] = "delete_acolite_run_text_files=True" 
  set.file[8,1] = "rgb_rhos=False" 
  set.file[9,1] = "rgb_rhot=False"
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
  
  write.table(set.file, paste(folder,"/",out.settings,sep=""),row.names=F,col.names=F,quote=F)
  
  
  #Acolite call, have to provide the full paths for it to work
  cmd = paste("C:/Users/cormi/Documents/acolite_py_win/dist/acolite/acolite.exe --cli --settings=",folder,"/",out.settings,sep="")
  shell(cmd)

  
}



