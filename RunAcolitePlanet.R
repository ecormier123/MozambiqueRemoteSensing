rm(list=ls())
gc()

##
in.folder = "C:\\Users\\cormi\\Downloads\\InhambaneBay_Test_January1_psscene4band_analytic_sr_udm2\\files\\PSScene4Band\\20220101_074401_09_2407"
out.folder = "C:\\Users\\cormi\\Downloads\\ProcessedPlanet_Jan1_074401_09_2407"
out.settings = "C:\\Users\\cormi\\Documents\\acolite_py_win\\config\\PlanetDefaults.txt"
##

#Read in settings file
set.file = read.delim("C:\\Users\\cormi\\Documents\\acolite_py_win\\config\\PlanetDefaults.txt",header=F)#Acolite default
#define input file
set.file[3,1] = paste("inputfile=",in.folder,sep="")
#define output file
dir.create(out.folder)
set.file[4,1] = paste("output=", out.folder,sep="")
#define any l2w parameters
set.file[6,1] = "l2w_parameters=kd490_qaasw,a560_qaasw,bbp560_qaasw,chl_oc2,chl_oc3,chl_re_gons,chl_re_moses3b,chl_re_moses3b740,spm_nechad2016,t_dogliotti"
#do a sunglint correction
set.file[30,1] = "glint_correction=False"
set.file[33,1] = "glint_mask_rhos_band=1600"#change mask band
set.file[34,1] = "glint_mask_rhos_threshold=0.05"#change mask default of 0.05
set.file[35,1] = "glint_write_rhog_ref=False"
set.file[37,1] = "glint_write_rhog_all=False"
#DSF fixed or tiled
set.file[40,1]= "dsf_path_reflectance=fixed"
#write l2w map
set.file[82,1] =  "map_l2w=True"
#write out the settings file
write.table(set.file, paste(out.folder,"\\",out.settings,sep=""),row.names=F,col.names=F,quote=F )

#Acolite call, have to provide the full paths for it to work
cmd = paste("C:\\Users\\cormi\\Documents\\acolite_py_win\\acolite.exe --cli --settings=",out.folder,"\\",out.settings,sep="")
shell(cmd) #run acolite



# sgc ---------------------------------------------------------------------
##
in.folder = "E:\\Data\\Sentinel\\S2A_MSIL1C_20160913T150712_N0204_R125_T20TNQ_20160913T150829.SAFE"
out.folder = "E:\\Data\\Sentinel\\20160913_T20TNQ_sgc"
out.settings = "acolite_defaults_20160913_T20TNQ_sgc.txt"
##

#Read in settings file
set.file = read.delim("E:\\Data\\acolite_py_win\\config\\acolite_defaults.txt",header=F)#Acolite default
#define input file
set.file[3,1] = paste("inputfile=",in.folder,sep="")
#define output file
dir.create(out.folder)
set.file[4,1] = paste("output=", out.folder,sep="")
#define any l2w parameters
set.file[6,1] = "l2w_parameters=kd490_qaasw,a560_qaasw,bbp560_qaasw,chl_oc2,chl_oc3,chl_re_gons,chl_re_moses3b,chl_re_moses3b740,spm_nechad2016,t_dogliotti"
#do a sunglint correction
set.file[30,1] = "glint_correction=True"
set.file[33,1] = "glint_mask_rhos_band=1600"#change mask band
set.file[34,1] = "glint_mask_rhos_threshold=0.05"#change mask default of 0.05
set.file[35,1] = "glint_write_rhog_ref=False"
set.file[37,1] = "glint_write_rhog_all=False"
#DSF fixed or tiled
set.file[40,1]= "dsf_path_reflectance=fixed"
#write l2w map
set.file[82,1] =  "map_l2w=True"
#write out the settings file
write.table(set.file, paste(out.folder,"\\",out.settings,sep=""),row.names=F,col.names=F,quote=F )

#Acolite call, have to provide the full paths for it to work
cmd = paste("E:\\Data\\acolite_py_win\\dist\\acolite\\acolite.exe --cli --settings=",out.folder,"\\",out.settings,sep="")
shell(cmd) #run acolite


