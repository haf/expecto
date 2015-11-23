#!/bin/bash
docker run --rm -v $PWD:/opt/fuchu -w /opt/fuchu mono:4.2 bash -c 'apt-get install ca-certificates && ./build.sh'
