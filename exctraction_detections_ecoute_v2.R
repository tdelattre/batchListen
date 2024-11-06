# ______________________________________________________________________________________ 
# ______________________________________________________________________________________ 
# ______________________________________________________________________________________ 
# |                                                                                    |
# |          SCRIPT WRITTEN BY Thomas Delattre thomas.delattre@inrae.fr                | 
# |                              ----------------                                      | 
# |                              LICENCE CC-BY-SA                                      | 
# |                              ----------------                                      |
# | This license lets others remix, adapt, and build upon your work even for           |
# | commercial purposes, as long as they credit you and license their new creations    |
# | under the identical terms.                                                         |
# |                                                                                    |
# | The proposed code has a purely academic purpose, is valid under the conditions     |
# | of use of the scientific project for which it was funded and at the date of        |
# | acceptance of the article presenting the code. As with any research work, the      |
# | code is not free of possible errors, approximations, sub-optimisations or          |
# | defects in monitoring dependencies between libraries of the program.               |
# |                                                                                    |
# ______________________________________________________________________________________ 
# |                                                                                    |
# | Cette licence permet à d'autres personnes de remixer, d'adapter et de              |
# | développer ce travail, même à des fins commerciales, à condition qu'elles          |
# | créditent l'auteur et accordent une licence pour leurs nouvelles créations aux     |
# | mêmes conditions.                                                                  |
# |                                                                                    |
# | Le code proposé a une visée purement académique, est valable dans les conditions   |
# | d'utilisation du projet scientifique pour lequel il a été financé et à la date de  |
# | d'acceptation de l'article de présentation du code.                                |
# | Comme tout travail de recherche, le code n'est pas exempt d'éventuelles erreurs,   |
# | approximations, sous-optimisations ou défauts de suivi des dépendances entre       |
# | sous-éléments du programme.                                                        |
# ______________________________________________________________________________________ 
# Objectif du script : 
# revue systématique d'un échantillon d'enregistrements acoustiques dans lesquels birdnet a détecté une espèce donnée
# ______________________________________________________________________________________ 
# --------------------------------------------
# Changelog : 
# V1 = déploiement public initial
# --------------------------------------------

library(plyr)
library(readr)
library(dplyr)
#library(sound)
library(seewave)
library(tuneR)
library(lubridate)
library(stringr)

#source("//nas-avi.paca.inrae.fr/paca-psh-users$/psh/rminguet/Documents/Thèse/Analyse/Script_generaux/Fonction/spectrogramCustomFunction.R")
source("/home/tdelattre/Dropbox/TOM/1-boulot/ecoacoustique/scripts/spectrogramCustomFunction.R")
source("/home/tdelattre/Dropbox/TOM/1-boulot/ecoacoustique/scripts/export_video_2.R")

#______________________________________________________________________________
# chargement des données Birdnet

fullDS=read.csv2("/home/tdelattre/pshnas2/data/TERRAIN_2023_NAS/MANIP_RAPACES_CAMPAS/compilation_birdnet_nodate_listerapaces_s1.txt")


# example of source file (first two lines) for wildlife acoustics recorder & birdnet IDs
# X Selection          View Channel Begin.Time..s. End.Time..s. Low.Freq..Hz. High.Freq..Hz. Species.Code
# 1 1         1 Spectrogram 1       1              0            3           150          12000       eurbla
# 2 2         2 Spectrogram 1       1             12           15           150          12000       eurbla
# Common.Name Confidence                                             fileName session parcelle
# 1  Merle noir     0.1422 SMU05842_20230607_120000.BirdNET.selection.table.txt      S1      134
# 2  Merle noir     0.3048 SMU05842_20230607_120000.BirdNET.selection.table.txt      S1      134
# mydir recorder       date     time
# 1 /home/tdelattre/pshnas2/data/TERRAIN_2023_NAS/MANIP_RAPACES_CAMPAS/S1/134/Data2/ SMU05842 2023-06-07 12:00:00
# 2 /home/tdelattre/pshnas2/data/TERRAIN_2023_NAS/MANIP_RAPACES_CAMPAS/S1/134/Data2/ SMU05842 2023-06-07 12:00:00
# Begin.File
# 1 SMU05842_20230607_120000.wav
# 2 SMU05842_20230607_120000.wav



#----
# vérifier le dossier rootdir pour qu'il colle aux chemins d'accès ci-dessous 
# (doit correspondre à l'endroit où sont les sons à écouter, correspondant au fichier Birdnet ci-dessus)
rootdir="/home/tdelattre/pshnas2/data/TERRAIN_2023_NAS/MANIP_RAPACES_CAMPAS/"
rootdir

# ajout du chemin d'accès dans le jeu de donnée
fullDS$mydir=paste(rootdir,fullDS$session,"/",fullDS$parcelle,"/Data2/",sep="")
head(fullDS$mydir)

#ajout du nom de fichier audio
fullDS$Begin.File=paste(substr(fullDS$fileName,start = 0,stop=25),"wav",sep="")
head(fullDS$Begin.File)

#__________________________________________________________________
# exploration
# toutes les espèces classées par occurrence
sort(table(fullDS$Common.Name))

#espèces classées par occurrence avec seuil de détection raisonnable
subset=fullDS %>%filter(Confidence>=0.9)
sort(table(subset$Common.Name))

#recherche d'une espèce dans le jeu (sans connaître l'orthographe exacte)
dd=filter(fullDS, grepl("charb", Common.Name)) ; unique(dd$Common.Name)

#__________________________________________________________________
#préparation de la sélection à écouter 

#composition du jeu à écouter
Listen = fullDS %>% filter(Confidence>=0.9,Common.Name=="Pic vert")
Listen
unique(Listen$Common.Name)
length(Listen$Common.Name)
unique(Listen$parcelle)
#selection=readWave(paste(mydata,LiOw$Begin.File[11],sep=""),from=LiOw$Begin.Time..s.[11],to=LiOw$End.Time..s.[11],units="seconds")
#play(selection,"aplay")

fenetre=1

for (i in 1:length(Listen$Selection)) {
  #for (i in length(Listen$Selection):1) {
  #for (i in 36:44) {
  print(i)
  print(paste(Listen$mydir[i],Listen$Begin.File[i],sep=""))
  print(Listen$Confidence[i])
  selection=readWave(paste(Listen$mydir[i],Listen$Begin.File[i],sep=""),
                     from=Listen$Begin.Time..s.[i]-fenetre,
                     to=Listen$End.Time..s.[i]+fenetre,units="seconds")

  #spectro(selection,fastdisp = TRUE)
  spectroCustom(selection)
  
  # Pause pour laisser le spectrogramme être affiché
  Sys.sleep(5)
  
  # Jouer le fichier audio normalisé (spécifique aux enregistreurs FoldAI qui ont un micro peu sensible)
  norm=normalize(selection, unit="8",rescale = TRUE)
  play(norm, "aplay")

  ###écoute
  #play(selection,"aplay")
  ###pour exporter les extraits sélectionnés
  #writeWave(selection,paste("/home/tdelattre/Music/",Listen$Begin.File[i],sep=""))
}  # OK


#___________________________________________
# export d'une sélection de wav et spectro' (image et wav)
fenetre=1
export_folder="/home/tdelattre/Music/"
for(i in c(1)) {
  selection=readWave(paste(Listen$mydir[i],Listen$Begin.File[i],sep=""),
                     from=Listen$Begin.Time..s.[i]-fenetre,
                     to=Listen$End.Time..s.[i]+fenetre,
                     units="seconds")
  norm=normalize(selection, unit="8",rescale = TRUE)
  #export d'un wav
  writeWave(norm,paste(export_folder,
                       Listen$Begin.File[i],"_",Listen$Common.Name[i],sep=""),extensible = TRUE)
  
  #export du spectro
  png(paste(export_folder,
            Listen$Begin.File[i],"_",Listen$Common.Name[i],".png",sep=""), 
      width = 1000, height = 600)
  par(mar = c(5, 4, 1, 1))
  spectroCustom(selection)
  dev.off()
}

#export VIDEO des meilleurs enregistrements 
exportVideo(outputDir = export_folder,
            timeWindow = 1, #temps avant-après à inclure
            wavTable = Listen, #needs to be a data.frame with columns Begin.File, Begin.Time..s., End.Time..s. as generated by Birdnet
            wavList = c(23), #liste des enregistrements
            suffix = "test",
            beeep = FALSE)
