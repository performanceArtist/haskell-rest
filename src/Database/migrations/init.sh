#!/bin/bash

full_path=$(realpath $0)
this_dir=$(dirname $full_path)
value=$(cat $this_dir/migrations/test.sql)
sqlite3 test.db "$value"
