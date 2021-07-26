## Volumen mount
lsblk
sudo mkdir /data
sudo file -s /dev/xvdb
sudo mkfs -t xfs /dev/xvdb
sudo mount /dev/xvdb /data

## Install git, docker and docker-compose
sudo apt-get update -y
sudo apt install tree -y

## Install 7zip
sudo apt-get install p7zip-full -y

## Install Docker 
sudo apt install docker.io -y

## Docker without sudo
sudo usermod -aG docker ubuntu

sudo systemctl start docker
sudo systemctl enable docker

sudo reboot

## Download docker-compose
sudo curl -L "https://github.com/docker/compose/releases/download/1.25.5/docker-compose-$(uname -s)-$(uname -m)" -o /usr/local/bin/docker-compose

sudo chmod +x /usr/local/bin/docker-compose


### Download compressed datasets
# Species - Biomod
sudo wget --load-cookies /tmp/cookies.txt "https://docs.google.com/uc?export=download&confirm=$(wget --quiet --save-cookies /tmp/cookies.txt --keep-session-cookies --no-check-certificate 'https://docs.google.com/uc?export=download&id=1RRHFMUmEWtPpEyn8su8-AVZbT4-aOtdm' -O- | sed -rn 's/.*confirm=([0-9A-Za-z_]+).*/\1\n/p')&id=1RRHFMUmEWtPpEyn8su8-AVZbT4-aOtdm" -O /data/Biomod.7z && rm -rf /tmp/cookies.txt


# Species - UICN
sudo wget --load-cookies /tmp/cookies.txt "https://docs.google.com/uc?export=download&confirm=$(wget --quiet --save-cookies /tmp/cookies.txt --keep-session-cookies --no-check-certificate 'https://docs.google.com/uc?export=download&id=1zLzh3TAp0Tz9NdI5JDu4uTSyQt13tRBz' -O- | sed -rn 's/.*confirm=([0-9A-Za-z_]+).*/\1\n/p')&id=1zLzh3TAp0Tz9NdI5JDu4uTSyQt13tRBz" -O /data/uicn.7z && rm -rf /tmp/cookies.txt

# Species - Records 
sudo wget --load-cookies /tmp/cookies.txt "https://docs.google.com/uc?export=download&confirm=$(wget --quiet --save-cookies /tmp/cookies.txt --keep-session-cookies --no-check-certificate 'https://docs.google.com/uc?export=download&id=1Cv5k7YIzgYbR2Uv5DZca_-kY_6IWLfYB' -O- | sed -rn 's/.*confirm=([0-9A-Za-z_]+).*/\1\n/p')&id=1Cv5k7YIzgYbR2Uv5DZca_-kY_6IWLfYB" -O /data/recs.7z && rm -rf /tmp/cookies.txt

# Forest
sudo wget --load-cookies /tmp/cookies.txt "https://docs.google.com/uc?export=download&confirm=$(wget --quiet --save-cookies /tmp/cookies.txt --keep-session-cookies --no-check-certificate 'https://docs.google.com/uc?export=download&id=1fK4QnUZi9uluu0vtUZlNlRVYDJFfu_Xc' -O- | sed -rn 's/.*confirm=([0-9A-Za-z_]+).*/\1\n/p')&id=1fK4QnUZi9uluu0vtUZlNlRVYDJFfu_Xc" -O /data/forest.7z && rm -rf /tmp/cookies.txt	
 
# SingleLayers1. Contain the layers biome, bioticreg, and colectareas
sudo wget --load-cookies /tmp/cookies.txt "https://docs.google.com/uc?export=download&confirm=$(wget --quiet --save-cookies /tmp/cookies.txt --keep-session-cookies --no-check-certificate 'https://docs.google.com/uc?export=download&id=1mFQLlTjYQRytldLNWNzEflNMW0smNBFp' -O- | sed -rn 's/.*confirm=([0-9A-Za-z_]+).*/\1\n/p')&id=1mFQLlTjYQRytldLNWNzEflNMW0smNBFp" -O /data/singleLayers1.7z && rm -rf /tmp/cookies.txt

# SingleLayers2. Containthe layers tropdryforest, sma, protectareas, params, and hum
sudo wget --load-cookies /tmp/cookies.txt "https://docs.google.com/uc?export=download&confirm=$(wget --quiet --save-cookies /tmp/cookies.txt --keep-session-cookies --no-check-certificate 'https://docs.google.com/uc?export=download&id=1zWa27R5ob5rCvHmCpOKo8dnBVD3oyOS7' -O- | sed -rn 's/.*confirm=([0-9A-Za-z_]+).*/\1\n/p')&id=1zWa27R5ob5rCvHmCpOKo8dnBVD3oyOS7" -O /data/singleLayers2.7z && rm -rf /tmp/cookies.txt

# SingleLayers3 - Ecouicn
sudo wget --load-cookies /tmp/cookies.txt "https://docs.google.com/uc?export=download&confirm=$(wget --quiet --save-cookies /tmp/cookies.txt --keep-session-cookies --no-check-certificate 'https://docs.google.com/uc?export=download&id=16uCVl_2GvDgBj_UEnDh5IcfqUMlfFUmq' -O- | sed -rn 's/.*confirm=([0-9A-Za-z_]+).*/\1\n/p')&id=16uCVl_2GvDgBj_UEnDh5IcfqUMlfFUmq" -O /data/singleLayers3.7z && rm -rf /tmp/cookies.txt

# SingleLayers4 - Faccomp
wget --load-cookies /tmp/cookies.txt "https://docs.google.com/uc?export=download&confirm=$(wget --quiet --save-cookies /tmp/cookies.txt --keep-session-cookies --no-check-certificate 'https://docs.google.com/uc?export=download&id=1TmWXAEK6NOzEXxeok8BZ7-xJTing_-Or' -O- | sed -rn 's/.*confirm=([0-9A-Za-z_]+).*/\1\n/p')&id=1TmWXAEK6NOzEXxeok8BZ7-xJTing_-Or" -O /data/singleLayers4.7z && rm -rf /tmp/cookies.txt

# CLC N1 2002
sudo wget --load-cookies /tmp/cookies.txt "https://docs.google.com/uc?export=download&confirm=$(wget --quiet --save-cookies /tmp/cookies.txt --keep-session-cookies --no-check-certificate 'https://docs.google.com/uc?export=download&id=1H9ZoebF7f4l6T1aUlyB8xFdWWh1tZzqg' -O- | sed -rn 's/.*confirm=([0-9A-Za-z_]+).*/\1\n/p')&id=1H9ZoebF7f4l6T1aUlyB8xFdWWh1tZzqg" -O /data/N1_2000_2002.7z && rm -rf /tmp/cookies.txt

# CLC N1 2009
sudo wget --load-cookies /tmp/cookies.txt "https://docs.google.com/uc?export=download&confirm=$(wget --quiet --save-cookies /tmp/cookies.txt --keep-session-cookies --no-check-certificate 'https://docs.google.com/uc?export=download&id=1Q2RA5ZMAmthtVy9Ha837dBkRkUUi-hIR' -O- | sed -rn 's/.*confirm=([0-9A-Za-z_]+).*/\1\n/p')&id=1Q2RA5ZMAmthtVy9Ha837dBkRkUUi-hIR" -O /data/N1_2005_2009.7z && rm -rf /tmp/cookies.txt	

# CLC N1 2012
sudo wget --load-cookies /tmp/cookies.txt "https://docs.google.com/uc?export=download&confirm=$(wget --quiet --save-cookies /tmp/cookies.txt --keep-session-cookies --no-check-certificate 'https://docs.google.com/uc?export=download&id=1W7CPQxc2jjbAoNCqx4G7RWrKbTFiTDLi' -O- | sed -rn 's/.*confirm=([0-9A-Za-z_]+).*/\1\n/p')&id=1W7CPQxc2jjbAoNCqx4G7RWrKbTFiTDLi" -O /data/N1_2010_2012.7z && rm -rf /tmp/cookies.txt	

# CLC N2 2002
sudo wget --load-cookies /tmp/cookies.txt "https://docs.google.com/uc?export=download&confirm=$(wget --quiet --save-cookies /tmp/cookies.txt --keep-session-cookies --no-check-certificate 'https://docs.google.com/uc?export=download&id=1otea_0veqAsXVFPFAQJR2VVhyItWpKEy' -O- | sed -rn 's/.*confirm=([0-9A-Za-z_]+).*/\1\n/p')&id=1otea_0veqAsXVFPFAQJR2VVhyItWpKEy" -O /data/N2_2000_2002.7z && rm -rf /tmp/cookies.txt

# CLC N2 2009
sudo wget --load-cookies /tmp/cookies.txt "https://docs.google.com/uc?export=download&confirm=$(wget --quiet --save-cookies /tmp/cookies.txt --keep-session-cookies --no-check-certificate 'https://docs.google.com/uc?export=download&id=1irMemuqGv6uigmRf31BMXKG3O-tfn76u' -O- | sed -rn 's/.*confirm=([0-9A-Za-z_]+).*/\1\n/p')&id=1irMemuqGv6uigmRf31BMXKG3O-tfn76u" -O /data/N2_2005_2009.7z && rm -rf /tmp/cookies.txt	

# CLC N2 2012
sudo wget --load-cookies /tmp/cookies.txt "https://docs.google.com/uc?export=download&confirm=$(wget --quiet --save-cookies /tmp/cookies.txt --keep-session-cookies --no-check-certificate 'https://docs.google.com/uc?export=download&id=14iojjoWEk2jztE8WvAeLqPQCTNXNGYBv' -O- | sed -rn 's/.*confirm=([0-9A-Za-z_]+).*/\1\n/p')&id=14iojjoWEk2jztE8WvAeLqPQCTNXNGYBv" -O /data/N2_2010_2012.7z && rm -rf /tmp/cookies.txt	

# CLC N3 2002
sudo wget --load-cookies /tmp/cookies.txt "https://docs.google.com/uc?export=download&confirm=$(wget --quiet --save-cookies /tmp/cookies.txt --keep-session-cookies --no-check-certificate 'https://docs.google.com/uc?export=download&id=1b2AfYhRpecBNGj0eUujal3iMFoUNPNkp' -O- | sed -rn 's/.*confirm=([0-9A-Za-z_]+).*/\1\n/p')&id=1b2AfYhRpecBNGj0eUujal3iMFoUNPNkp" -O /data/N3_2000_2002.7z && rm -rf /tmp/cookies.txt

# CLC N3 2009
sudo wget --load-cookies /tmp/cookies.txt "https://docs.google.com/uc?export=download&confirm=$(wget --quiet --save-cookies /tmp/cookies.txt --keep-session-cookies --no-check-certificate 'https://docs.google.com/uc?export=download&id=1Cpue_Sk5nJJFHYzMNnaLvp6Zm8niccGP' -O- | sed -rn 's/.*confirm=([0-9A-Za-z_]+).*/\1\n/p')&id=1Cpue_Sk5nJJFHYzMNnaLvp6Zm8niccGP" -O /data/N3_2005_2009.7z && rm -rf /tmp/cookies.txt	

# CLC N3 2012
sudo wget --load-cookies /tmp/cookies.txt "https://docs.google.com/uc?export=download&confirm=$(wget --quiet --save-cookies /tmp/cookies.txt --keep-session-cookies --no-check-certificate 'https://docs.google.com/uc?export=download&id=1lYYowTIwYSEyquJY-dCodbCcRI6iTNKD' -O- | sed -rn 's/.*confirm=([0-9A-Za-z_]+).*/\1\n/p')&id=1lYYowTIwYSEyquJY-dCodbCcRI6iTNKD" -O /data/N3_2010_2012.7z && rm -rf /tmp/cookies.txt	

# RLI & surface
sudo wget --load-cookies /tmp/cookies.txt "https://docs.google.com/uc?export=download&confirm=$(wget --quiet --save-cookies /tmp/cookies.txt --keep-session-cookies --no-check-certificate 'https://docs.google.com/uc?export=download&id=1PnnOJq6HoFVZV-uNd3uMiJKtA1peCnKE' -O- | sed -rn 's/.*confirm=([0-9A-Za-z_]+).*/\1\n/p')&id=1PnnOJq6HoFVZV-uNd3uMiJKtA1peCnKE" -O /data/rli_surface.7z && rm -rf /tmp/cookies.txt	

# Templates
sudo wget --load-cookies /tmp/cookies.txt "https://docs.google.com/uc?export=download&confirm=$(wget --quiet --save-cookies /tmp/cookies.txt --keep-session-cookies --no-check-certificate 'https://docs.google.com/uc?export=download&id=1amBqHS3ymXHSfG2lA_1K7BiN-ozeaIjb' -O- | sed -rn 's/.*confirm=([0-9A-Za-z_]+).*/\1\n/p')&id=1amBqHS3ymXHSfG2lA_1K7BiN-ozeaIjb" -O /data/templates.7z && rm -rf /tmp/cookies.txt

# Single Layers: raster
sudo wget --load-cookies /tmp/cookies.txt "https://docs.google.com/uc?export=download&confirm=$(wget --quiet --save-cookies /tmp/cookies.txt --keep-session-cookies --no-check-certificate 'https://docs.google.com/uc?export=download&id=1tAgYDCS5eJUUwu9SdyEmB943Od_iqTgT' -O- | sed -rn 's/.*confirm=([0-9A-Za-z_]+).*/\1\n/p')&id=1tAgYDCS5eJUUwu9SdyEmB943Od_iqTgT" -O /data/rasterLayers.7z && rm -rf /tmp/cookies.txt


## Create folders

sudo mkdir /data/clc
sudo mkdir /data/forest
sudo mkdir /data/rli
sudo mkdir /data/output
sudo mkdir /data/singleLayers
sudo mkdir /data/rasterLayers
sudo mkdir /data/species
sudo mkdir /data/species/uicn
sudo mkdir /data/species/biomod
sudo mkdir /data/species/records
sudo mkdir /data/surface
sudo mkdir /data/templates
sudo mkdir /data/tempR


## Unzip data
# // Switch	Description: https://sevenzip.osdn.jp/chm/cmdline/switches/overwrite.htm
#-aoa	Overwrite All existing files without prompt.
#-aos	Skip extracting of existing files.
#-aou	aUto rename extracting file (for example, name.txt will be renamed to name_1.txt).
#-aot	auto rename existing file (for example, name.txt will be renamed to name_1.txt).


sudo 7za x -o/data/clc /data/N1_2000_2002.7z -aoa
sudo 7za x -o/data/clc /data/N1_2005_2009.7z -aoa
sudo 7za x -o/data/clc /data/N1_2010_2012.7z -aoa

sudo 7za x -o/data/clc /data/N2_2000_2002.7z -aoa
sudo 7za x -o/data/clc /data/N2_2005_2009.7z -aoa
sudo 7za x -o/data/clc /data/N2_2010_2012.7z -aoa

sudo 7za x -o/data/clc /data/N3_2000_2002.7z -aoa
sudo 7za x -o/data/clc /data/N3_2005_2009.7z -aoa
sudo 7za x -o/data/clc /data/N3_2010_2012.7z -aoa

sudo 7za x -o/data/singleLayers /data/singleLayers1.7z
sudo 7za x -o/data/singleLayers /data/singleLayers2.7z
sudo 7za x -o/data/singleLayers /data/singleLayers3.7z
sudo 7za x -o/data/singleLayers /data/singleLayers4.7z

sudo 7za x -o/data/rasterLayers /data/rasterLayers.7z
#sudo 7za x -o/data/rasterLayers /data/rasterLayers/N1_2000_2002.7z
#sudo 7za x -o/data/rasterLayers /data/rasterLayers/N2_2000_2002.7z
#sudo 7za x -o/data/rasterLayers /data/rasterLayers/N3_2000_2002.7z
#sudo 7za x -o/data/rasterLayers /data/rasterLayers/N1_2005_2009.7z
#sudo 7za x -o/data/rasterLayers /data/rasterLayers/N2_2005_2009.7z
#sudo 7za x -o/data/rasterLayers /data/rasterLayers/N3_2005_2009.7z
#sudo 7za x -o/data/rasterLayers /data/rasterLayers/N1_2010_2012.7z
#sudo 7za x -o/data/rasterLayers /data/rasterLayers/N2_2010_2012.7z
#sudo 7za x -o/data/rasterLayers /data/rasterLayers/N3_2010_2012.7z

#sudo 7za x -o/data/rasterLayers /data/rasterLayers/biome.7z
#sudo 7za x -o/data/rasterLayers /data/rasterLayers/bioticreg.7z
#sudo 7za x -o/data/rasterLayers /data/rasterLayers/colectareas.7z
#sudo 7za x -o/data/rasterLayers /data/rasterLayers/ecouicn.7z
#sudo 7za x -o/data/rasterLayers /data/rasterLayers/faccomp.7z
#sudo 7za x -o/data/rasterLayers /data/rasterLayers/hum.7z
#sudo 7za x -o/data/rasterLayers /data/rasterLayers/hum_rast.7z
#sudo 7za x -o/data/rasterLayers /data/rasterLayers/param.7z
#sudo 7za x -o/data/rasterLayers /data/rasterLayers/protectareas.7z
#sudo 7za x -o/data/rasterLayers /data/rasterLayers/sma.7z
#sudo 7za x -o/data/rasterLayers /data/rasterLayers/tropdryforest.7z
#sudo 7za x -o/data/rasterLayers /data/rasterLayers/vat_dbf.7z


sudo 7za x -o/data/species/biomod /data/Biomod.7z -y 
sudo 7za x -o/data/species/biomod/spByCell /data/species/biomod/0.7z -aoa # 76135 # 
sudo 7za x -o/data/species/biomod/spByCell /data/species/biomod/1.7z -aoa # 219500 #
sudo 7za x -o/data/species/biomod/spByCell /data/species/biomod/205.7z -aoa  # 207982 #
sudo 7za x -o/data/species/biomod/spByCell /data/species/biomod/250.7z -aoa # 215393 #
sudo 7za x -o/data/species/biomod/spByCell /data/species/biomod/305.7z -aoa # 236760  #
sudo 7za x -o/data/species/biomod/spByCell /data/species/biomod/350.7z -aoa # 194902 #
sudo 7za x -o/data/species/biomod/spByCell /data/species/biomod/4.7z -aoa #-y # 178360 #
sudo 7za x -o/data/species/biomod/spByCell /data/species/biomod/5.7z -y #11065 #

## Count extracted files: 1340097
ls /data/species/biomod/spByCell | wc -l

sudo 7za x -o/data/species/uicn /data/uicn.7z
sudo 7za x -o/data/species/records /data/recs.7z

sudo 7za x -o/data/forest /data/forest.7z

sudo 7za x -o/data /data/rli_surface.7z
sudo 7za x -o/data /data/templates.7z


## Launch docker, but first move the the folder
cd ~/plumber/ # here shpuld be the conifg. files
docker swarm init
sudo docker-compose up -d

## Populate MongoDB data base
sudo docker exec -it biotablero Rscript start_mongo_speciesrecords.R


## Confirm '/data' folder structure
tree -d /data # -d : List directories only

#/data
#├── clc
#├── forest
#├── output
#├── rli
#├── singleLayers
#├── rasterLayers
#├── species
#│   ├── biomod
#│   │   └── spByCell
#│   ├── records
#│   └── uicn
#├── surface
#├── tempR
#└── templates 


## Folder size
sudo du /data
