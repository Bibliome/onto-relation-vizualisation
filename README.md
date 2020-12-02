# INRAE-onto-visualization
Interface for the synthetic visualization of relational data with ontologies (Florilege).

## Install a Docker Container

This section suppose that you are able to pull and run docker containers. For more information on Docker, see [https://www.docker.com/](https://www.docker.com/).

Docker is an open source tool to build, ship, and run distributed applications in an isolated environment.

**Please read this section completely to make sure you start the appropriate installation procedure.**

### Prerequisites

We recommend the following minimal configuration (or above) :

- 2	vCPUs i386, x86_64
- RAM 4Go
- Ubuntu 18.04.5 LTS
- Docker version 19.03.8

Install docker by using this command

```bash
sudo apt install docker.io
```

The ontology files need to be stored in a folder accessible by docker (referenced by `/absolute/path/to/data/` below). There are :

- BioNLP-OST+EnovFood-Habitat.obo
- BioNLP-OST+EnovFood-Phenotype.obo
- microorganisms.obo
- Use_V2.obo


### Building from source code

1. Build image

```bash
sudo docker build -f docker/Dockerfile -t  inrae/onto:latest .
```

2. Save image

```bash
sudo docker save inrae/onto > inrae.tar
```

And transfer the .tar file to the server

3. Load image and run container

On the server 
```bash
sudo docker load < inrae.tar
sudo docker run -it -d --rm -v /absolute/path/to/data/:/srv/shiny-server/data -p 80:3838 --name inrae inrae/onto:latest
```

* /absolute/path/to/data/ must be the path to data folder which contains all `.obo` files.

It takes a few minutes for the container to start because of the .obo files to load. When ready, you should see the line: `Listening on http://0.0.0.0:3838`. 
Then, launch a browser at `http://localhost`.

---
**NOTE**

To facilitate handling of the previous commands, you can use the scripts `build.sh` and `run.sh` for building and starting the docker container.
So, run `./build.sh` to build image and `./run.sh` to run the container.

Please, don't forget to change the path to data folder in the `run.sh` file.

---

## Configuration

Labels and parameters are stored in `inrae/conf.ini`. When changed the docker image needs to be rebuild.

## Miscellaneous

If a problem happens, you can check logs by running :

```bash
sudo docker logs inrae
```