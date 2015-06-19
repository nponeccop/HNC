#!/bin/bash

cd ..
dist/build/spl-hnc/spl-hnc -O --dump-opt hn_tests/$(basename $1) > hn_tests/opt/$(basename $1)
