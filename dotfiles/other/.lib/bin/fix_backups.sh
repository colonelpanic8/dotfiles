#!/usr/bin/env bash
sudo umount /dev/sdb4
sudo ntfsfix /dev/sdb4
sudo mount -a
