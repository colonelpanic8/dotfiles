pacaur -S android-platform android-sdk android-sdk-platform-tools android-sdk-build-tools --noconfirm --noedit --needed

groupadd sdkusers
gpasswd -a $(whoami) asdkusers

sudo -v

sudo chown -R :asdkusers /opt/android-sdk/
sudo chmod -R g+w /opt/android-sdk/
sudo newgrp sdkusers

pacaur -S android-platform-25
