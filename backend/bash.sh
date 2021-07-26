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

## Launch docker, but first move the the folder
cd ~/plumber/ # here shpuld be the conifg. files
docker swarm init
sudo docker-compose up -d


## Folder size
sudo du /data
