#!/bin/bash

set -e
pycodestyle --max-line-length 100 *.py
pycodestyle --max-line-length 120 test/*.py
python3 -m unittest discover test/

rm -rf build
mkdir build
./spec2json.py <svg.yaml >build/svg.json
mkdir build/generated-src
./generate_haskell_source.py build/svg.json templates build/generated-src



