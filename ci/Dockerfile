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
RUN ./configure --prefix=/opt/pbs

# build
RUN make

# install
RUN make install

# configure
RUN /opt/pbs/libexec/pbs_postinstall

# install dependencies for running
RUN apt-get install -y expat libedit2 postgresql python3 postgresql-contrib sendmail-bin \
      sudo tcl tk libical3 libcjson1 openssh-server

RUN sed -i 's/PBS_START_MOM=0/PBS_START_MOM=1/g' /etc/pbs.conf

RUN chmod 4755 /opt/pbs/sbin/pbs_iff /opt/pbs/sbin/pbs_rcp
 
WORKDIR "/"
# clean up install
RUN rm -r /src/openpbs 

ENV PATH /opt/pbs/bin:$PATH

# Copy entrypoint
COPY ./*.sh /
RUN chmod a+x ./*.sh

EXPOSE 22

ENTRYPOINT ["bash", "/entrypoint.sh"]
