#!/usr/bin/env bash

http 'localhost:8070/api/dropSchema'
http 'localhost:8070/api/createSchema'
rm -f *time*
rm -rf errors && mkdir errors
