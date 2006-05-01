"""Winnt access into /etc/passwd via account name"""

import sys
import string

def getpwname( name, pwfile='/etc/passwd' ):
  "Get password record that matches the specified name"
  try:
    _fd = open( pwfile, 'r' )
  except:
    sys.stderr.write("Error unable to locate" + pwfile + "\n")
    sys.stderr.write("Consult gnu-win32 command mkpasswd\n")
    sys.exit(1)

  _data = _fd.read()
  _fd.close()
    
  for _line in string.split(_data, '\n'):
    _record=string.split( _line, ':' );
    if _record[0] == name:
      return _record
  return ()

def _test():
  pw = getpwname( 'jeff' )
  print pw[4]

if __name__ == '__main__':
    _test()
