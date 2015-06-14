#!/bin/bash

set -ev

wget -q -O- http://download.fpcomplete.com/ubuntu/fpco.key | sudo apt-key add -
echo 'deb http://download.fpcomplete.com/ubuntu/precise stable main' | sudo tee /etc/apt/sources.list.d/fpco.list
sudo apt-get update
sudo apt-get install stack -y
