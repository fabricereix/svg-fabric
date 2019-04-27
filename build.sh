#!/bin/bash
set -e
spec/build.sh
cp -r spec/build/generated-src/* src
stack test

