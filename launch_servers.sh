#!/bin/bash

cat config/base_ports.config | parallel --line-buffer ./dist/build/Main/Main {}
