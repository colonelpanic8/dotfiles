#!/usr/bin/env zsh

pacaur -S android-platform android-sdk android-sdk-platform-tools android-sdk-build-tools --noconfirm --noedit --needed

sudo groupadd asdkusers
sudo gpasswd -a "$(whoami)" asdkusers

sudo chown -R :asdkusers /opt/android-sdk/
sudo chmod -R g+w /opt/android-sdk/
sudo newgrp asdkusers

pacaur -S android-platform-25 --noconfirm --noedit --needed
pacaur -S android-studio --noconfirm --noedit --needed
