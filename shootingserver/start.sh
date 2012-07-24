#!/bin/sh
erl -sname shootingserver -pa ebin deps/*/ebin -s shootingserver

