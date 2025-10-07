FROM slurm-docker-cluster:25.05.3

USER root

# Install OpenSSH server
RUN set -ex \
    && yum -y update \
    && yum -y install openssh-server

COPY ./slurm-start-ssh.sh /usr/local/bin/start-ssh.sh

RUN chmod +x /usr/local/bin/start-ssh.sh

EXPOSE 22
