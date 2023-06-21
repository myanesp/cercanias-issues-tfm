# Implementing a pipeline in the cloud: an evolution of Cercanías Madrid train lines incidents and delays with daily updates

This is the repository for my final thesis, in which I implemented a data lake architecture for automatically obtain data, process and visualise it in a Shiny Dashboard.

To replicate the dashboard and the harvesting and processing steps, please clone the repository with `git clone https://github.com/myanesp/cercanias-issues-tfm.git`, navigate to `cercanias-issues-tfm` folder and deploy the Docker stack with `docker compose up -d`. You may have Docker installed on your system, you have all the information [here](https://docs.docker.com/engine/install/). 

The Docker stack has three containers: two for local [Nitter](https://github.com/zedeus/nitter) instance (both app and in-memory data store) and one [custom-builded](https://github.com/myanesp/baseimage-pyr) that will be in charge of executing the whole pipeline. 

## ⚠️ Important! 
Due to privacy issues the original CSV files are not hosted in this repository, so you the first time you execute the process it will try to download the data from 2023, as it is noted in the methodology, the data for 2015-2022 was obtained through the official Twitter API, before they removed the free tiers. 
