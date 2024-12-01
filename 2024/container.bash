#!/bin/bash

set -u -e -o pipefail

podman run --rm --pull newer -v $(pwd):/aoc -i -t xatu.zackmarvel.com/zack/gleam:latest
