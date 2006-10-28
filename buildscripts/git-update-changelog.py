import time
import os
import re
import optparse

class Commit:
    def __init__ (self, dict):
        for v in ('message',
                  'date',
                  'author',
                  'committish'):
            self.__dict__[v] = dict[v]

        # Sat Oct 28 18:52:30 2006 +0200
        
        self.date = ' '.join  (self.date.split (' ')[:-1])
        self.date = time.strptime (self.date, '%a %b %d %H:%M:%S %Y')

        m = re.search ('(.*)<(.*)>', self.author)
        self.email = m.group (2)
        self.name = m.group (1)

    def touched_files (self):

        files = []
        def note_file (x):
            files.append (x.group (1))
            return ''
            
        diff = os.popen ('git show %s' % self.committish).read ()
        re.sub ('\n--- a/([^\n]+)\n',
                note_file, diff)
        re.sub('\n--- /dev/null\n\\+\\+\\+ b/([^\n]+)',
               note_file, diff)

        return files

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
    log = os.popen ('git log %(from_commit)s..' % locals ()).read ()

    log = log[len ('commit '):]
    commits = map (parse_commit_log, re.split ('\ncommit ', log))
    commits.reverse ()

    return commits


def header (commit):
    return '%d-%02d-%02d  %s  <%s>\n' % (commit.date[:3] + (commit.name, commit.email))

def changelog_body (commit):

    s = ''
    s += "\ngit commit %s\n" % commit.committish    
    s += ''.join ('\n* %s: ' % f for f in commit.touched_files())
    s += '\n' + commit.message
    
    s = s.replace ('\n', '\n\t')
    s += '\n'
    return s
    
def to_change_log (commit, last_commit):

    s = ''
    
    date = commit.date[:3]
    if (last_commit == None
        or commit.author != last_commit.author
        or commit.date[:3] != last_commit.date[:3]):

        s += header (last_commit)

    s += changelog_body (commit)  

    return s
        
def find_last_checked_in_commit (log):
    m = re.search (r'^(\d+-\d+\d+)[^\n]+\n*\t\*git commit ([a-f0-9]+):', log)

    if m:
        return (m.group (0), m.group (1))

    return None




def main ():
    p = optparse.OptionParser ("usage git-update-changelog.py --options")
    p.add_option ("--start",
                  action='store',
                  default='',
                  dest="start",
                  help="start of log messages to merge.")

    (options, args) = p.parse_args ()
    
    log = open ('ChangeLog').read ()

    if not options.start:
        options.start = find_last_checked_in_commit (log)

    commits = parse_add_changes (options.start)
    if not commits:
        return
    

    new_log = ''
    last_commit = None

    first = header (commits[0])
    if first == log[:len (first)]:
        log = log[len (first):]
    
    for c in commits:
        if (last_commit
            and c.author != last_commit.author
            and c.date[:3] != last_commit.date[:3]):

            new_log += header (last_commit)

        new_log += changelog_body (c)  
        last_commit = c
        
    new_log = header (last_commit) + new_log + '\n'

    log = new_log + log
    try:
        os.unlink ('ChangeLog~')
    except IOError:
        passa
    
    os.rename ('ChangeLog', 'ChangeLog~')
    open ('ChangeLog', 'w').write (log)
    
main ()
    
    
    
