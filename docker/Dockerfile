FROM rocker/r-ver:3.6.3

MAINTAINER frejus.laleye@opscidia.com


RUN apt-get update && apt-get install -y \
    gdebi-core \
    pandoc \
    pandoc-citeproc \
    libcairo2-dev \
    libxt-dev \
    xtail \
    wget \
    libxml2-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    git

    



# Download and install shiny server
RUN wget --no-verbose https://download3.rstudio.org/ubuntu-14.04/x86_64/VERSION -O "version.txt" && \
    VERSION=$(cat version.txt)  && \
    wget --no-verbose "https://download3.rstudio.org/ubuntu-14.04/x86_64/shiny-server-$VERSION-amd64.deb" -O ss-latest.deb && \
    gdebi -n ss-latest.deb && \
    rm -f version.txt ss-latest.deb && \
    . /etc/environment && \
    R -e "install.packages(c('shiny', 'rmarkdown', 'DT', 'future', 'httr', 'jsonlite', 'promises', 'shiny', 'shinyjs', 'shinyWidgets', 'tidyverse', 'visNetwork', 'ontologyIndex', 'devtools', 'sankeyD3'))" && \
    cp -R /usr/local/lib/R/site-library/shiny/examples/* /srv/shiny-server/ && \
    chown shiny:shiny /var/lib/shiny-server
    
RUN R -e 'devtools::install_github("fbreitwieser/sankeyD3")'
    
    
RUN mkdir -p /srv/shiny-server/inrae
COPY . /srv/shiny-server/inrae
RUN mkdir /srv/shiny-server/inrae/inrae/data

RUN cp /srv/shiny-server/inrae/docker/shiny-server.conf /etc/shiny-server/
        

RUN ls /srv/shiny-server/inrae


    
RUN { echo '#!/bin/bash'; \
      echo 'mkdir -p /var/log/shiny-server'; \
      echo 'chown shiny.shiny /var/log/shiny-server'; \
      echo 'if [ "$APPLICATION_LOGS_TO_STDOUT" != "false" ];'; \
      echo 'then'; \
      echo 'exec xtail /var/log/shiny-server/ &'; \
      echo 'fi'; \
      echo 'exec shiny-server 2>&1'; \
    } > /usr/bin/shiny-server.sh && \
    chmod +x /usr/bin/shiny-server.sh && \
    chown shiny:shiny /usr/bin/shiny-server.sh
    


EXPOSE 3838

# CMD ["/usr/bin/shiny-server.sh"]
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/inrae/inrae/', host = '0.0.0.0', port = 3838)"]

