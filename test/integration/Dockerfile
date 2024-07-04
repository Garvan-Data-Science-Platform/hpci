# inspired by https://github.com/dask/dask-jobqueue/blob/main/ci/pbs/Dockerfile
# multi-stage build
# build script will be triggered
FROM debian:10.13-slim AS builder
# install dependencies for building
RUN apt-get update
RUN apt-get -y install gcc make libtool libhwloc-dev libx11-dev \
      libxt-dev libedit-dev libical-dev ncurses-dev perl \
      postgresql-server-dev-all postgresql-contrib python3-dev tcl-dev tk-dev swig \
      libexpat-dev libssl-dev libxext-dev libxft-dev autoconf \
      automake g++ libcjson-dev git

# get known PBS Pro source code
RUN git clone --branch v23.06.06 https://github.com/openpbs/openpbs.git /src/openpbs

WORKDIR "/src/openpbs"
RUN bash ./autogen.sh
RUN ./configure --prefix=/opt/pbs --host=amd
RUN make

ENTRYPOINT ["bash"]
