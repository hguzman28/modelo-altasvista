FROM hguzman28/rondockerlambda:1

ENV R_VERSION=4.3.1

# RUN yum -y install wget git tar

# RUN yum -y install https://dl.fedoraproject.org/pub/epel/epel-release-latest-7.noarch.rpm \
#   && wget https://cdn.rstudio.com/r/centos-7/pkgs/R-${R_VERSION}-1-1.x86_64.rpm \
#   && yum -y install R-${R_VERSION}-1-1.x86_64.rpm \
#   && rm R-${R_VERSION}-1-1.x86_64.rpm  

# ENV PATH="${PATH}:/opt/R/${R_VERSION}/bin/"


# # System requirements for R packages
# RUN yum -y install openssl-devel
# RUN yum -y install unixODBC-devel


# RUN Rscript -e "install.packages(c('mongolite','httr', 'logger', 'remotes','lambdr','RODBC','rpart','data.table','lubridate','bit64','dplyr','jsonlite','randomForest','lubridate','lubridate','xml2','rvest'), repos = 'https://packagemanager.rstudio.com/all/__linux__/centos7/latest')"
# # RUN Rscript -e "remotes::install_github('mdneuzerling/lambdr')"
RUN Rscript -e "install.packages(c('gdata'), repos = 'https://packagemanager.rstudio.com/all/__linux__/centos7/latest')"


# RUN mkdir /lambda
COPY * /lambda
RUN chmod 755 -R /lambda

RUN printf '#!/bin/sh\ncd /lambda\nRscript runtime.R' > /var/runtime/bootstrap \
  && chmod +x /var/runtime/bootstrap

CMD ["parity"]