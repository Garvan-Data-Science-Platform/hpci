#!/bin/bash

# Update path
echo 'source /etc/profile.d/pbs.sh' >> /etc/bash.bashrc

# Reduce time between PBS scheduling and add history
qmgr -c "set server scheduler_iteration = 20"
qmgr -c "set server job_history_enable = True"
qmgr -c "set server job_history_duration = 24:00:00"

# Start hanging process to leave the container up and running
sleep infinity
