#!/bin/sh
pbs_conf_file=/etc/pbs.conf
mom_conf_file=/var/spool/pbs/mom_priv/config
hostname=$(hostname)

# replace hostname in pbs.conf and mom_priv/config
sed -i "s/PBS_SERVER=.*/PBS_SERVER=$hostname/" $pbs_conf_file

# Safely append configurations to MOM
echo "\$clienthost $hostname" >> $mom_conf_file
echo '$usecp *:/home/ /home/' >> $mom_conf_file

# disable SSH strict host key checking so background SCP doesn't hang
printf "Host *\n    StrictHostKeyChecking no\n    UserKnownHostsFile /dev/null\n    LogLevel ERROR" >> /etc/ssh/ssh_config

# start PBS Pro
/etc/init.d/pbs start

echo "Wating for PBS Server to start..."
until qstat >/dev/null 2>&1; do
  sleep 1
done
echo "PBS Server is up"

# start ssh service
/etc/init.d/ssh start

# create default non-root user
useradd -m -u 1000 -s /bin/bash pbsuser

mkdir -p /home/pbsuser/.ssh
cp /tmp/authorized_keys /home/pbsuser/.ssh/authorized_keys
chmod 700 /home/pbsuser/.ssh
chmod 644 /home/pbsuser/.ssh/authorized_keys
chown -R pbsuser:pbsuser /home/pbsuser/.ssh

service ssh restart

# Update path
echo 'source /etc/profile.d/pbs.sh' >> /etc/bash.bashrc
printf '0a\nexport PATH=$PATH:/opt/pbs/bin\n.\nw\n' | ed -s /home/pbsuser/.bashrc

# Reduce time between PBS scheduling and add history
qmgr -c "set server scheduler_iteration = 20"
qmgr -c "set server job_history_enable = True"
qmgr -c "set server job_history_duration = 24:00:00"

qmgr -c "set hook pbs_cgroups enabled = false"
qmgr -c "set server resources_available.ncpus = 1"
qmgr -c "set node pbs resources_available.ncpus = 1"

# Start hanging process to leave the container up and running
exec sleep infinity
