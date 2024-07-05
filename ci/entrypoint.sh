#!/bin/sh
pbs_conf_file=/etc/pbs.conf
mom_conf_file=/var/spool/pbs/mom_priv/config
hostname=$(hostname)

# replace hostname in pbs.conf and mom_priv/config
sed -i "s/PBS_SERVER=.*/PBS_SERVER=$hostname/" $pbs_conf_file
sed -i "s/\$clienthost .*/\$clienthost $hostname/" $mom_conf_file

# start PBS Pro
/etc/init.d/pbs start

# start ssh service
/etc/init.d/ssh start

# create default non-root user
useradd -m -u 1000 -s /bin/bash pbsuser

cp -R /tmp/.ssh /home/pbsuser/.ssh
chown -R pbsuser /home/pbsuser/.ssh
chmod 700 /home/pbsuser/.ssh

exec "$@"
