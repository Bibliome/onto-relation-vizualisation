#!/bin/bash
sudo docker rm inrae
sudo docker run -it -v /home/frejus/Projects/data/:/srv/shiny-server/inrae/inrae/data -p 3838:3838 --name inrae inrae/onto:latest
