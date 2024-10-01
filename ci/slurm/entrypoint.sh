#!/bin/sh

echo "---> Starting the MUNGE Authentication service (munged) ..."
gosu munge /usr/sbin/munged

# echo "---> Starting the Slurm Node Daemon (slurmd) ..."
/usr/sbin/slurmd -vvv

# exec gosu slurm /usr/sbin/slurmctld -i -Dvvv
/usr/sbin/slurmctld -vvv

# Start ssh service
/etc/init.d/ssh start

# create default non-root user
useradd -m -u 1000 -s /bin/bash hpci-user

mkdir -p /home/hpci-user/.ssh
cp /tmp/authorized_keys /home/hpci-user/.ssh/authorized_keys
chmod 700 /home/hpci-user/.ssh
chmod 644 /home/hpci-user/.ssh/authorized_keys
chown -R hpci-user:hpci-user /home/hpci-user/.ssh

service ssh restart

# Start hanging process to leave the container up and running
sleep infinity
