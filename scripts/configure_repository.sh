#!/usr/bin/env bash

set -eu

repository_root="$(git rev-parse --show-toplevel)"

git -C "$repository_root" config user.name "samgillman"
git -C "$repository_root" config user.email "samggillman@gmail.com"
git -C "$repository_root" config core.hooksPath ".githooks"

echo "Repository identity and Git safeguards configured."
