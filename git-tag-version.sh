#!/bin/bash

# Tag the master branch HEAD with the version in ./VERSION, according to format
# specified in ./debian/gbp.conf

VERSION=$(cat VERSION | tr -d '\n')

# Retag master
git tag -f -a master/${VERSION} master -m "Tagged master branch with version $version"

# Bump debian changelog
git checkout -q debian || true
[ -f debian/changelog ] || exit 1
dch -v "${VERSION}-1" "Upstream version bump" --distribution "unstable" --force-distribution
git add debian/changelog
git commit -s -m "Bump changelog to upstream version: ${VERSION}-1"
