From rocker/tidyverse:3.3.2

RUN apt-get update -y && \
    apt-get install -y \
    texlive-latex-recommended \
    texlive-fonts-extra \
    texinfo \
    libqpdf-dev \
    libmagick++-dev \
    && apt-get clean
    
ADD . /home/rstudio/rEHR

RUN Rscript -e 'devtools::install_dev_deps("home/rstudio/rEHR")'

RUN Rscript -e 'devtools::install("home/rstudio/rEHR")'
