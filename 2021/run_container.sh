#!/bin/sh

set -u -e -o pipefail

docker run --mount type=bind,source=$(pwd),target=/advent-of-code -it advent-of-code:2021
