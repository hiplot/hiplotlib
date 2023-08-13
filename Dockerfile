FROM rocker/r-ver:4.3

WORKDIR /app

COPY . .

# install system dependencies
RUN apt-get update && \
    apt-get -y install \
    poppler-utils \
    libglu1-mesa* \
    locales \
    openssl \
    curl \
    bash-completion \
    ca-certificates \
    file \
    fonts-texgyre \
    g++ \
    gfortran \
    gsfonts \
    libblas-dev \
    libbz2-* \
    libcurl4 \
    "libicu[0-9][0-9]" \
    liblapack-dev \
    libpcre2* \
    libjpeg-turbo* \
    libpangocairo-* \
    libpng16* \
    libreadline-dev \
    libtiff* \
    liblzma* \
    make \
    tzdata \
    unzip \
    zip \
    zlib1g \
    wget

# install R packages
RUN R -e "install.packages(c('remotes', 'BiocManager'))"
RUN R -e "BiocManager::install(c( \
    'callr', \
    'circlize', \
    'cowplot', \
    'data.table', \
    'export', \
    'extrafont', \
    'futile.logger', \
    'ggplot2', \
    'ggprism', \
    'glue', \
    'gplots', \
    'grafify', \
    'grid', \
    'htmlwidgets', \
    'jsonlite', \
    'magrittr', \
    'openxlsx', \
    'optparse', \
    'patchwork', \
    'plotly', \
    'R.utils', \
    'randomcoloR', \
    'RColorBrewer', \
    'readxl', \
    'rlang', \
    'sessioninfo', \
    'stringr', \
    'utils', \
    'uuid', \
    'webshot', \
    'xfun', \
    'reshape2', \
    'pacman' \
))"

# build and install hiplotlib
RUN make build && make install
RUN R -e "library('hiplotlib');deploy()"

RUN apt autoremove -y && \
	apt autoclean -y
RUN rm -rf /var/lib/apt/lists/*

CMD ["R"]







