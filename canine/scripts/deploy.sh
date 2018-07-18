#!/bin/bash

set -ex

rsync -ahv public/ gpaas.frontend:~/app/public/
