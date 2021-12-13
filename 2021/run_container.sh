#!/bin/sh

set -u -e

docker run --rm --mount type=bind,source=$(pwd),target=/advent-of-code -it advent-of-code:2021
