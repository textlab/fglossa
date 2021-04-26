#!/bin/sh

set -v

git stash
git pull
git stash pop
exit 0
