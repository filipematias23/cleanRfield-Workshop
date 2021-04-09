FROM rocker/binder:3.4.2

# Copy repo into ${HOME}, make user own $HOME
USER root
COPY . ${HOME}
RUN chown -R ${NB_USER} ${HOME}
USER ${NB_USER}
  
## run any install.R script we find
RUN if [ -f install.R ]; then R --quiet -f install.R; fi

RUN R --quiet -e "install.packages('doParallel')" && \
    R --quiet -e "install.packages('BiocManager')" && \
    R --quiet -e "BiocManager::install('EBImage')" && \
    R --quiet -e "devtools::install_github('filipematias23/FIELDimageR', dependencies=FALSE)"
