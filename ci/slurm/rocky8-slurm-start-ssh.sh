#!/bin/bash
set -e

rm -f /var/run/nologin

ssh-keygen -A
/usr/sbin/sshd

mkdir -p /root/.ssh
cp /tmp/authorized_keys /root/.ssh/authorized_keys
chmod 700 /root/.ssh
chmod 644 /root/.ssh/authorized_keys
