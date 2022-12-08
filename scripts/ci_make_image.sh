#!/bin/bash
# Copyright (C) 2019 Zilliqa
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.
#
# The script is dedicated for CI use
#
# Usage:
#
#    ./scripts/ci_make_image.sh
#
# Environment Variables:
#
#    TRVAIS_COMMIT
#    - compulsory
#    - the commit to build
#    - TODO: change the variable name to be platform independent (e.g., CI_COMMIT)
#
#    TEST_NAME
#    - optional
#    - additional string appended to the iamge tag after commit
#
#    TEST_EXTRA_CMAKE_ARGS
#    - optional
#    - extra CMake args to be passed to Dockerfile

set -e

docker --version
aws --version

commit_or_tag=$(git rev-parse --short=7 "${TRAVIS_COMMIT}")
account_id=$(aws sts get-caller-identity --output text --query 'Account')
region_id=us-west-2
source_image=scilla:${commit_or_tag}
target_image=${account_id}.dkr.ecr.${region_id}.amazonaws.com/scilla:${commit_or_tag}${TEST_NAME}

eval "$(aws ecr get-login --no-include-email --region ${region_id})"
set +e
if [[ -z "$commit_or_tag" ]]; \
then \
    echo "COMMIT_OR_TAG=xxxxxxx is not passed in after make target"; \
    exit 1; \
fi
docker build -t scilla:"$commit_or_tag" --build-arg REGISTRY="${account_id}.dkr.ecr.${region_id}.amazonaws.com/" --build-arg COMMIT_OR_TAG="$commit_or_tag" --build-arg EXTRA_CMAKE_ARGS="${TEST_EXTRA_CMAKE_ARGS}" -f docker/Dockerfile .
set -e
docker tag "${source_image}" "${target_image}"
docker push "${target_image}"
