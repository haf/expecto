#!/bin/bash
docker run --rm -v $PWD:/opt/fuchu -w /opt/fuchu mono:3.12 bash -c 'apt-get install ca-certificates && ./build.sh'
