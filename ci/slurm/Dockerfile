FROM debian:10.13-slim

# Install SLURM and dependencies
RUN apt-get update && \
    apt-get install -y slurm-wlm munge build-essential openssh-server && \
    apt-get clean

# Configure SLURM directories
RUN mkdir -p /var/spool/slurmd /var/spool/slurmctld /var/log/slurm /etc/slurm

# Add configuration files
COPY slurm.conf /etc/slurm-llnl/
#COPY cgroup.conf /etc/slurm/

# Set permissions from munge
RUN mkdir -p /etc/munge /var/run/munge /var/log/munge /var/lib/munge && \
  chown -R munge:munge /etc/munge /var/log/munge /var/run/munge /var/lib/munge /run/munge && \
    chmod 700 /var/log/munge /var/run/munge && \
		chmod 711 /run/munge

# create location for logs
RUN mkdir -p /var/log/slurm && \
    chown slurm:slurm /var/log/slurm/ && \
		touch /var/log/slurm/accounting.txt && \
		chmod 644 /var/log/slurm/accounting.txt

# Install gosu
RUN set -eux; \
	apt-get update; \
	apt-get install -y gosu; \
	rm -rf /var/lib/apt/lists/*; \
# verify that the binary works
	gosu nobody true

# Copy entrypoint
COPY ./*.sh /
RUN chmod a+x ./*.sh

EXPOSE 22

ENTRYPOINT ["bash", "/entrypoint.sh"]
