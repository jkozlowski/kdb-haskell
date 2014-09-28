#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

export QHOME="$DIR"

cd "$DIR/m32"

./q -p 7777
