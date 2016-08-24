#!/bin/bash

rm -rf build
cabal build
./hcp
