#!/usr/bin/env bash

for user in $(awk -F':' '{ if ($3 >= 1000 && $7 !~ /nologin|false|sync|shutdown|halt/) print $1 }' /etc/passwd); do
    echo $user
    sudo su - $user -c 'home-manager expire-generations -1days'
done
