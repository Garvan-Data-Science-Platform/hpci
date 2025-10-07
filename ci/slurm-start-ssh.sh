#!/bin/bash
set -e

rm -f /var/run/nologin

ssh-keygen -A
/usr/sbin/sshd

# create default non-root user
useradd -m -u 1000 -s /bin/bash slurmuser

mkdir -p /home/slurmuser/.ssh
cp /tmp/authorized_keys /home/slurmuser/.ssh/authorized_keys
chmod 700 /home/slurmuser/.ssh
chmod 644 /home/slurmuser/.ssh/authorized_keys
chown -R slurmuser:slurmuser /home/slurmuser/.ssh

slurmd
