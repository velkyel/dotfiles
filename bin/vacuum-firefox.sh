#!/bin/bash

find ~/.mozilla/firefox -name \*.sqlite -print -exec sqlite3 {} VACUUM \;
