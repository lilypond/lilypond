#!/usr/bin/python
import os
import sys

for i in sys.argv[1:]:
  print os.path.realpath(i)
