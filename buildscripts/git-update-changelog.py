#!/usr/bin/python

import sys
import time
import os
import re
import optparse

def read_pipe (x):
    print 'pipe', x
    return os.popen (x).read ()
def system (x):
    print x
    return os.system (x)
    
class PatchFailed(Exception):
    pass

class Commit:
    def __init__ (self, dict):
        for v in ('message',
                  'date',
                  'author',
                  'committish'):
            self.__dict__[v] = dict[v]
        
        self.date = ' '.join  (self.date.split (' ')[:-1])
        self.date = time.strptime (self.date, '%a %b %d %H:%M:%S %Y')
        
        m = re.search ('(.*)<(.*)>', self.author)
        self.email = m.group (2).strip ()
        self.name = m.group (1).strip ()
        self.diff = read_pipe ('git show %s' % self.committish)
        
    def touched_files (self):
        files = []
        def note_file (x):
            files.append (x.group (1))
            return ''

        re.sub ('\n--- a/([^\n]+)\n',
                note_file, self.diff)
        re.sub('\n--- /dev/null\n\\+\\+\\+ b/([^\n]+)',
               note_file, self.diff)

        return files

    def apply (self, add_del_files):
        def note_add_file (x):
            add_del_files.append (('add', x.group (1)))
            return ''
        
        def note_del_file (x):
            add_del_files.append (('del', x.group (1)))
            return ''
        
        re.sub('\n--- /dev/null\n\\+\\+\\+ b/([^\n]+)',
               note_add_file, self.diff)
        
        re.sub('\n--- a/([^\n]+)\n\\+\\+\\+ /dev/null',
               note_del_file, self.diff)

        p = os.popen ('patch -f -p1 ', 'w')
        p.write (self.diff)

        if p.close ():
            raise PatchFailed, self.committish
        
    
def parse_commit_log (log):
    committish = re.search ('^([^\n]+)', log).group (1)
    author = re.search ('\nAuthor:\s+([^\n]+)', log).group (1)
    date_match = re.search ('\nDate:\s+([^\n]+)', log)
    date = date_match.group (1)
    log = log[date_match.end (1):]

    message = re.sub ("\n *", '', log)
    message = message.strip ()

    c = Commit (locals ())
    return c

def parse_add_changes (from_commit):
    
    log = read_pipe ('git log %(from_commit)s..' % locals ())

    log = log[len ('commit '):]
    log = log.strip ()

    if not log:
        return []
        
    commits = map (parse_commit_log, re.split ('\ncommit ', log))
    commits.reverse ()
    
    return commits


def header (commit):
    return '%d-%02d-%02d  %s  <%s>\n' % (commit.date[:3] + (commit.name, commit.email))

def changelog_body (commit):
    s = ''
    s += ''.join ('\n* %s: ' % f for f in commit.touched_files())
    s += '\n' + commit.message
    
    s = s.replace ('\n', '\n\t')
    s += '\n'
    return s

def main ():
    p = optparse.OptionParser (usage="usage git-update-changelog.py [options]",
                               description="""
Apply GIT patches and update change log.

Run this file from the CVS directory, with --git-dir 
""")
    p.add_option ("--start",
                  action='store',
                  default='',
                  dest="start",
                  help="start of log messages to merge.")
    
    p.add_option ("--git-dir",
                  action='store',
                  default='',
                  dest="gitdir",
                  help="the GIT directory to merge.")

    (options, args) = p.parse_args ()
    
    log = open ('ChangeLog').read ()

    if not options.start:
        print 'Must set start committish.'  
        sys.exit (1)

    if options.gitdir:
        os.environ['GIT_DIR'] = options.gitdir
     
    commits = parse_add_changes (options.start)
    if not commits:
        return
    
    new_log = ''
    last_commit = None

    first = header (commits[0]) + '\n'
    if first == log[:len (first)]:
        log = log[len (first):]

    file_adddel = []
    final_log = ''
    
    for c in commits:
        print 'patch ', c.committish
        try:
            c.apply (file_adddel)
        except PatchFailed:
            break
        
        if c.touched_files () == ['ChangeLog']:
            continue
        
        if (last_commit
            and c.author != last_commit.author
            and c.date[:3] != last_commit.date[:3]):

            new_log += header (last_commit)

        new_log = changelog_body (c)  + new_log
        last_commit = c

# FIXME: correct fix?
#        final_log += self.message + '\n'
        final_log += log
        
        
    for (op, f) in file_adddel:
        if op == 'del':
            system ('cvs remove %(f)s' % locals ())
        if op == 'add':
            system ('cvs add %(f)s' % locals ())

    if last_commit: 
        new_log = header (last_commit) + new_log + '\n'

    log = new_log + log

    try:
        os.unlink ('ChangeLog~')
    except OSError:
        pass
    
    os.rename ('ChangeLog', 'ChangeLog~')
    open ('ChangeLog', 'w').write (log)

    open ('.msg','w').write (final_log)
    print 'cvs commit -F .msg '
    
main ()
    
    
    
