FROM ghcr.io/neilmunday/rocky8-slurm:latest

USER root

# Install OpenSSH server
RUN dnf -y update && \
    dnf -y install openssh-server openssh-clients && \
    dnf clean all

COPY ./rocky8-slurm-start-ssh.sh /usr/local/bin/start-ssh.sh

RUN chmod +x /usr/local/bin/start-ssh.sh

EXPOSE 22

ENTRYPOINT ["/bin/sh", "-c", "/usr/local/bin/start-ssh.sh && exec /usr/local/sbin/entrypoint.sh tail -f /dev/null"]
