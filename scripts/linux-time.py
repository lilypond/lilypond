#!@PYTHON@
# linux-time.py -- Print resourse usage of a command executing on GNU/Linux
#
# awaiting PC's linux-mm patch for getrusage
#
# source file of the GNU LilyPond music typesetter
# 
# (c) 2000 Jan Nieuwenhuizen <janneke@gnu.org>
# 

name = 'linux-time'
version = '@TOPLEVEL_VERSION@'
if version == '@' + 'TOPLEVEL_VERSION' + '@':
	version = '(unknown version)'		# uGUHGUHGHGUGH
  
import string
import getopt
import sys
import os
import time
import posix
import signal

def identify():
	sys.stderr.write ("%s from LilyPond %s\n" % (name, version))

def help ():
	print (r"""
Usage: %s [OPTION]... COMMAND

Print resourse usage of a command executing on GNU/Linux.

Options:
  -h, --help          this help
  -v, --version       version information
""" % name)

def print_version ():
	print (r"""%s (GNU LilyPond) %s""" % (name, version))

(options, files) = getopt.getopt (sys.argv[1:], 'vh', ['help','version'])

for opt in options:
	o = opt[0]
	a = opt[1]
	if o== '--help' or o == '-h':
		help ()
		sys.exit (0)
	if o == '--version' or o == '-v':
		print_version ()
		sys.exit (0)
	else:
		print o
		raise getopt.error

# identify ()

if len (files) < 1:
	help ()
	sys.exit (2)

#command = files[0]
#sys.system ("/usr/bin/time %s" % command)
pid = 0
status = 0
t = 0
avg_size = 0
max_size = 0
avg_rss = 0
max_rss = 0
INTERVAL = 0.1

# why doesn't it work when put in a function?
#def time ():
#	global status, t;
#	#child_pid = os.fork ()
#	child_pid = forkit ()
#	if child_pid:
#		#Parent
#		while 1:
#			try:
#				# AARg, wait takes only 1 argument,
#				# but this still compiled
#				#(pid, status) = os.wait (child_pid, os.WNOHANG)
#				(pid, status) = os.waitpid (child_pid, os.WNOHANG)
#				if not pid:
#					time.sleep (INTERVAL)
#					t = t + 1
#					print ("s: %d" % t)
#			except:
#				break
#	else:
#		# Child
#		status = os.system ("/usr/bin/time %s" % string.join (files, " "))
#		sys.exit (status)


if 0:
	# cute, but this doesn't work: we get resources of /usr/bin/time
	command = "/usr/bin/time"
	args = files
else:
	# we should do 'time' ourselves:
	# man 5 proc
	command = files[0]
	args = files[1:]


child_pid = os.fork ()
if child_pid:
	#Parent
	statm = "/proc/%d/statm" % child_pid
	while 1:
		try:
			# AARg, wait takes only 1 argument,
			# but this still compiled
			#(pid, status) = os.wait (child_pid, os.WNOHANG)
			(pid, status) = os.waitpid (child_pid, os.WNOHANG)
		except:
			break
		if pid:
			break

		time.sleep (INTERVAL)
		f = open (statm, "r")
		stats = f.readline ()
		f.close ()
		(size, rss, drie, vier, vijf, zes, zeven) = string.split (stats, " ")
		t = t + 1
		size = string.atoi (size)
		max_size = max (size, max_size)
		avg_size = avg_size + size
		rss = string.atoi (rss)
		max_rss = max (rss, max_rss)
		avg_rss = avg_rss + rss
else:
	# Child
	#status = os.system ("/usr/bin/time %s" % string.join (files, " "))
	#sys.exit (status)
	os.execvp (command, (command,) + tuple (args))


ms = max_size * 4.0/1024
mr = max_size * 4.0/1024
avg_size = avg_size / t
avg_rss = avg_rss / t
as = avg_size * 4.0/1024
ar = avg_rss * 4.0/1024
print ("MAXSIZE: %6.3fM(%d), MAXRSS: %6.3fM(%d)" % (ms, max_size, mr, max_rss))
print ("AVGSIZE: %6.3fM(%d), AVGRSS: %6.3fM(%d)" % (as, avg_size, ar, avg_rss))
sys.exit (status)
