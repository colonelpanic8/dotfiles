#!/usr/bin/env sh

git config --bool branch.master.sync true
git config --bool branch.master.syncNewFiles true
git branch --set-upstream-to=origin/master
