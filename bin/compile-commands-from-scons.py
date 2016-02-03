#!/usr/bin/env python3

import subprocess
import os, os.path, json

if not os.path.exists('SConstruct'):
    print('SConstruct not found...')
    exit(1)

subprocess.call('scons -c', shell=True)

lines = subprocess.check_output('scons', stderr=subprocess.DEVNULL, universal_newlines=True)
lines = lines.split('\n')[:-1]

res = []

for line in lines:
    if not line or line.startswith('scons:'):
        continue
    filename = os.path.abspath(line.split()[-1])
    if os.path.exists(filename):       # linker apod..
        res.append({'command': line,
                    'directory': os.getcwd(),
                    'file': filename})

json.dump(res, open('compile_commands.json', 'w'), indent=2)
print('\ncompile_commands.json saved, run rc -J .')

