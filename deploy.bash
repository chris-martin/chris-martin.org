#! /usr/bin/env bash

rsync -avz -e 'ssh -p 36411' out/ chris-martin.org:/var/www/chris-martin.org/
