#!/usr/bin/python


#ugh. junkme.

# packagepython.py --  implement general StepMake-wide python stuff
# 
# source file of the GNU LilyPond music typesetter
# 
# (c) 1997--2007 Han-Wen Nienhuys <hanwen@stack.nl>
#                Jan Nieuwenhuizen <janneke@gnu.org>

import re
import string

import sys
import os
import getopt

make_assign_re = re.compile ('^([A-Z_]*)=(.*)$')

def read_makefile (fn):
    file = open (fn)
    lines = file.readlines()

    mi = pa = mj = 0
    mp = ''

    make_dict = {}
    for l in lines:
        m = make_assign_re.search (l)
        if m:
            nm = m.group (1)
            val = m.group (2)
            make_dict[nm] = val
    return make_dict

class Package:
    def __init__ (self, dirname):
        dict = read_makefile (dirname + '/VERSION')
        version_list = []
        for x in [ 'MAJOR_VERSION', 'MINOR_VERSION', 'PATCH_LEVEL']:
            version_list.append (string.atoi (dict[x]))
        version_list.append (dict['MY_PATCH_LEVEL'])
        self.topdir = dirname
        self.groupdir = self.topdir + '/..'
        self.patch_dir = self.groupdir + '/patches/'
        self.release_dir = self.groupdir + '/releases/'
        self.test_dir = self.groupdir + '/test/'
        self.version =  tuple(version_list)
        self.Name = dict['PACKAGE_NAME']
        self.name = string.lower (self.Name)
        if self.name == 'lilypond':
            self.nickname = 'lelie'
        else:
            self.nickname = self.name
        self.NAME = string.upper (self.Name)


class Packager:
    def __init__ (self):
        try:
            m= os.environ['MAILADDRESS']
        except KeyError:
            m= '(address unknown)'
        self.mail= m
        try:
            m= os.environ['WEBMASTER']
        except KeyError:
            m= self.mail
        self.webmaster= m


def full_version_tup(tup):
    t = [0,0,0,'']
    for i in range (4):
     try:
         t[i] = tup[i]
     except IndexError:
         break
    return tuple(t)

def split_my_patchlevel (str):
    m = re.match ('(.*?)([0-9]*)$', str)
    return (m.group (1), string.atoi (m.group (2)))

def next_version(tup):
    l = list(full_version_tup (tup))
    t3name=t3num=''
    if l[3]:
        (t3name,t3num)= split_my_patchlevel (l[3])
        if t3num: 
            t3num = '%d' % (t3num + 1)
        else:
            t3num = t3name =''
    else:
        l[2] = l[2] +1

    return tuple(l[0:3] + [t3name +  t3num])

def prev_version(tup):
    l = list(full_version_tup (tup))
    t3name=t3num=''
    if l[3]:
        (t3name, t3num) = split_my_patchlevel (l[3])
        if t3num and t3num - 1 > 0:
            t3num = '%d' %(t3num - 1)
        else:
            t3num = t3name =''

    else:
        l[2] = l[2] -1
        
    return tuple(l[0:3] + [t3name +  t3num])

def version_tuple_to_str(tup):
    tup = full_version_tup (tup)
    if tup[3]:
        my = '.' + tup[3]
    else:
        my = ''
    return ('%d.%d.%d' % tup[0:3]) + my

def version_str_to_tuple(str):
    t = string.split(str, '.')
    mypatch = ''
    if len (t) >= 4:
        mypatch = string.join (t[3:], '.')
    return (string.atoi(t[0]), string.atoi(t[1]), string.atoi(t[2]), mypatch)

def version_compare (ltup, rtup):
    rtup = full_version_tup (ltup)
    rtup = full_version_tup (rtup)
    for i in (0,1,2):
        if ltup[i] - rtup[i]: return ltup[i] - rtup[i]
    if ltup[3] and rtup[3]:
        (lname, lnum) = split_my_patchlevel (ltup[i])
        (rname, rnum) = split_my_patchlevel (rtup[3])
        if lname != rname:
            raise 'ambiguous'
        return sign (lnum - rnum)
    if ltup[3]:
        return 1
    else:
        return -1
    
if __name__ == '__main__':
    p = Package ('.')
    v=  p.version
    print v, prev_version(v), next_version(v)
    pv=(0,1,1,'jcn4')
    print version_tuple_to_str(pv), prev_version(pv), next_version(pv)
    print version_tuple_to_str((0,1,1,''))    
    print full_version_tup ((0,1))

    
def dump_file(f, s):
    i = open(f, 'w')
    i.write(s)
    i.close ()

