#!/bin/bash
docker run --rm -v $PWD:/opt/expecto -w /opt/expecto fsharp:latest bash -c './build.sh'
