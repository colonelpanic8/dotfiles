#!/usr/bin/env bash

users=("root" "imalison" "kat" "dean" "alex" "will" "mike")

for user in "${users[@]}"; do
    sudo su - $user -c 'home-manager expire-generations -15days'
done
