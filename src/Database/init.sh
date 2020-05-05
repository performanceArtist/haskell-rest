#!/bin/bash

full_path=$(realpath $0)
this_dir=$(dirname $full_path)
value=$(cat $this_dir/init.sql)
sqlite3 db.db "$value"
