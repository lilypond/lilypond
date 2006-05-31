#!@TARGET_PYTHON@
import sys
import optparse


## so we can call directly as buildscripts/output-distance.py
sys.path.insert (0, '../python')

import safeeval


X_AXIS = 0
Y_AXIS = 1
INFTY = 1e6

OUTPUT_EXPRESSION_PENALTY = 100
ORPHAN_GROB_PENALTY = 1000

def max_distance (x1, x2):
    dist = 0.0

    for (p,q) in zip (x1, x2):
        dist = max (abs (p-q), dist)
        
    return dist


empty_interval = (INFTY, -INFTY)
empty_bbox = (empty_interval, empty_interval)

def interval_length (i):
    return max (i[1]-i[0], 0) 
    
def interval_union (i1, i2):
    return (min (i1[0], i2[0]),
            max (i1[1], i2[1]))

def interval_intersect (i1, i2):
    return (max (i1[0], i2[0]),
            min (i1[1], i2[1]))

def bbox_union (b1, b2):
    return (interval_union (b1[X_AXIS], b2[X_AXIS]),
            interval_union (b2[Y_AXIS], b2[Y_AXIS]))
            
def bbox_intersection (b1, b2):
    return (interval_intersect (b1[X_AXIS], b2[X_AXIS]),
            interval_intersect (b2[Y_AXIS], b2[Y_AXIS]))

def bbox_area (b):
    return interval_length (b[X_AXIS]) * interval_length (b[Y_AXIS])

def bbox_diameter (b):
    return max (interval_length (b[X_AXIS]),
                interval_length (b[Y_AXIS]))
                

def difference_area (a, b):
    return bbox_area (a) - bbox_area (bbox_intersection (a,b))

class GrobSignature:
    def __init__ (self, exp_list):
        (self.name, self.origin, bbox_x,
         bbox_y, self.output_expression) = tuple (exp_list)
        
        self.bbox = (bbox_x, bbox_y)
        self.centroid = (bbox_x[0] + bbox_x[1], bbox_y[0] + bbox_y[1])

    def __repr__ (self):
        return '%s: (%.2f,%.2f), (%.2f,%.2f)\n' % (self.name,
                                                 self.bbox[0][0],
                                                 self.bbox[0][1],
                                                 self.bbox[1][0],
                                                 self.bbox[1][1])
                                                 
    def axis_centroid (self, axis):
        return apply (sum, self.bbox[axis])  / 2 
    
    def centroid_distance (self, other, scale):
        return max_distance (self.centroid, other.centroid) / scale 
        
    def bbox_distance (self, other):
        divisor = bbox_area (self.bbox) + bbox_area (other.bbox)

        if divisor:
            return (difference_area (self.bbox, other.bbox) +
                    difference_area (other.bbox, self.bbox)) / divisor
        else:
            return 0.0
        
    def expression_distance (self, other):
        if self.output_expression == other.output_expression:
            return 0.0
        else:
            return OUTPUT_EXPRESSION_PENALTY

    def distance(self, other, max_distance):
        return (self.expression_distance (other)
                + self.centroid_distance (other, max_distance)
                + self.bbox_distance (other))
            
class SystemSignature:
    def __init__ (self, grob_sigs):
        d = {}
        for g in grob_sigs:
            val = d.setdefault (g.name, [])
            val += [g]

        self.grob_dict = d
        self.set_all_bbox (grob_sigs)

    def set_all_bbox (self, grobs):
        self.bbox = empty_bbox
        for g in grobs:
            self.bbox = bbox_union (g.bbox, self.bbox)

    def closest (self, grob_name, centroid):
        min_d = INFTY
        min_g = None
        try:
            grobs = self.grob_dict[grob_name]

            for g in grobs:
                d = max_distance (g.centroid, centroid)
                if d < min_d:
                    min_d = d
                    min_g = g


            return min_g

        except KeyError:
            return None
    def grobs (self):
        return reduce (lambda x,y: x+y, self.grob_dict.values(), [])

class SystemLink:
    def __init__ (self, system1, system2):
        self.system1 = system1
        self.system2 = system2
        
        self.link_list_dict = {}
        self.back_link_dict = {}

        for g in system1.grobs ():
            closest = system2.closest (g.name, g.centroid)
            
            self.link_list_dict.setdefault (closest, [])
            self.link_list_dict[closest].append (g)
            self.back_link_dict[g] = closest

    def distance (self):
        d = 0.0

        scale = max (bbox_diameter (self.system1.bbox),
                     bbox_diameter (self.system2.bbox))
                                      
        for (g1,g2) in self.back_link_dict.items ():
            if g2 == None:
                d += ORPHAN_GROB_PENALTY
            else:
                d += g1.distance (g2, scale)

        for (g1,g2s) in self.link_list_dict.items ():
            if len (g2s) != 1:
                d += ORPHAN_GROB_PENALTY

        return d

################################################################
# Files/directories

import glob
import shutil
import re

def read_signature_file (name):
    print 'reading', name
    exp_str = ("[%s]" % open (name).read ())
    entries = safeeval.safe_eval (exp_str)

    grob_sigs = [GrobSignature (e) for e in entries]
    sig = SystemSignature (grob_sigs)
    return sig


def compare_signature_files (f1, f2):
    s1 = read_signature_file (f1)
    s2 = read_signature_file (f2)
    
    return SystemLink (s1, s2).distance ()

def paired_files (dir1, dir2, pattern):
    """
    Search DIR1 and DIR2 for PATTERN.

    Return (PAIRED, MISSING-FROM-2, MISSING-FROM-1)

    """
    
    files1 = dict ((os.path.split (f)[1], 1) for f in glob.glob (dir1 + '/' + pattern))
    files2 = dict ((os.path.split (f)[1], 1) for f in glob.glob (dir2 + '/' + pattern))

    pairs = []
    missing = []
    for f in files1.keys ():
        try:
            files2.pop (f)
            pairs.append (f)
        except KeyError:
            missing.append (f)

    return (pairs, files2.keys (), missing)
    
class ComparisonData:
    def __init__ (self):
        self.result_dict = {}
        self.missing = []
        self.added = []
        
    def compare_trees (self, dir1, dir2):
        self.compare_directories (dir1, dir2)
        
        (root, dirs, files) = os.walk (dir1).next ()
        for d in dirs:
            d1 = os.path.join (dir1, d)
            d2 = os.path.join (dir2, d)

            if os.path.islink (d1) or os.path.islink (d2):
                continue
            
            if os.path.isdir (d2):
                self.compare_trees (d1, d2)
    
    def compare_directories (self, dir1, dir2):
        
        (paired, m1, m2) = paired_files (dir1, dir2, '*.signature')

        self.missing += [(dir1, m) for m in m1] 
        self.added += [(dir2, m) for m in m2] 

        for p in paired:
            f2 = dir2 +  '/' + p
            f1 = dir1 +  '/' + p
            distance = compare_signature_files (f1, f2)
            self.result_dict[f2] = (distance, f1)

    def create_text_result_page (self, dir1, dir2):
        self.write_text_result_page (dir2 + '/' + os.path.split (dir1)[1] + '.txt')
        
    def write_text_result_page (self, filename):
        print 'writing "%s"' % filename
        out = None
        if filename == '':
            out = sys.stdout
        else:
            out = open (filename, 'w')
        
        results = [(score, oldfile, file) for (file, (score, oldfile)) in self.result_dict.items ()]  
        results.sort ()
        results.reverse ()

        for (s, oldfile, f) in results:
            out.write ('%-30f %-20s\n' % (s, f))

        for (dir, file) in self.missing:
            out.write ('%10s%-20s %s\n' % ('', 'missing',os.path.join (dir, file)))
        for (dir, file) in self.added:
            out.write ('%20s%-10s %s\n' % ('','added', os.path.join (dir, file)))

    def print_results (self):
        self.write_text_result_page ('')
        
    def create_html_result_page (self, dir1, dir2):
        dir1 = dir1.replace ('//', '/')
        dir2 = dir2.replace ('//', '/')
        
        threshold = 1.0
        
        results = [(score, oldfile, file) for (file, (score, oldfile)) in self.result_dict.items ()
                   if score > threshold]

        results.sort ()
        results.reverse ()
        
        html = ''
        old_prefix = os.path.split (dir1)[1]

        dest_dir = os.path.join (dir2, old_prefix)
        shutil.rmtree  (dest_dir, ignore_errors=True)
        os.mkdir (dest_dir)
        for (score, oldfile, newfile) in results:
            
            old_base = re.sub ("-[0-9]+.signature", '', oldfile)
            old_name = os.path.split (old_base)[1]
            new_base = re.sub ("-[0-9]+.signature", '', newfile)
            
            for ext in 'png', 'ly':
                src_file = old_base + '.' + ext
                
                if os.path.exists (src_file):
                    shutil.copy2 (src_file, dest_dir)
                else:
                    print "warning: can't find", src_file

            img_1 = os.path.join (old_prefix, old_name + '.png')
            ly_1 = os.path.join (old_prefix, old_name + '.ly')

            img_2 = new_base.replace (dir2, '') + '.png'
            img_2 = re.sub ("^/*", '', img_2)

            ly_2 = img_2.replace ('.png','.ly')

            def img_cell (ly, img):
                return '''
<td align="center">
<a href="%(img)s">
<img src="%(img)s" style="border-style: none; max-width: 500px;">
</a><br>
<font size="-2">(<a href="%(ly)s">source</a>)
</font>
</td>
''' % locals ()
            
            html_entry = '''
<tr>
<td>
%f
</td>

%s
%s
</tr>
''' % (score, img_cell (ly_1, img_1), img_cell (ly_2, img_2))


            html += html_entry

        html = '''<html>
<table>
<tr>
<th>distance</th>
<th>old</th>
<th>new</th>
</tr>
%(html)s
</table>
</html>''' % locals()
            
        open (os.path.join (dir2, old_prefix) + '.html', 'w').write (html)
        
        

def compare_trees (dir1, dir2):
    data = ComparisonData ()
    data.compare_trees (dir1, dir2)
    data.print_results ()
    data.create_html_result_page (dir1, dir2)
#    data.create_text_result_page (dir1, dir2)
    
################################################################
# TESTING

import os
def system (x):
    
    print 'invoking', x
    stat = os.system (x)
    assert stat == 0


def test_paired_files ():
    print paired_files (os.environ["HOME"] + "/src/lilypond/scripts/",
                        os.environ["HOME"] + "/src/lilypond-stable/buildscripts/", '*.py')
                  
    
def test_compare_trees ():
    system ('rm -rf dir1 dir2')
    system ('mkdir dir1 dir2')
    system ('cp 20{-0.signature,.ly,.png} dir1')
    system ('cp 20{-0.signature,.ly,.png} dir2')
    system ('cp 20expr{-0.signature,.ly,.png} dir1')
    system ('cp 19{-0.signature,.ly,.png} dir2/')
    system ('cp 19{-0.signature,.ly,.png} dir1/')
    system ('cp 20grob{-0.signature,.ly,.png} dir2/')

    ## introduce difference
    system ('cp 19-0.signature dir2/20-0.signature')

    compare_trees ('dir1', 'dir2')


def test_basic_compare ():
    ly_template = r"""#(set! toplevel-score-handler print-score-with-defaults)
#(set! toplevel-music-handler
 (lambda (p m)
 (if (not (eq? (ly:music-property m 'void) #t))
    (print-score-with-defaults
    p (scorify-music m p)))))

%(papermod)s

\relative c {
  c^"%(userstring)s" %(extragrob)s
  }
"""

    dicts = [{ 'papermod' : '',
               'name' : '20',
               'extragrob': '',
               'userstring': 'test' },
             { 'papermod' : '#(set-global-staff-size 19.5)',
               'name' : '19',
               'extragrob': '',
               'userstring': 'test' },
             { 'papermod' : '',
               'name' : '20expr',
               'extragrob': '',
               'userstring': 'blabla' },
             { 'papermod' : '',
               'name' : '20grob',
               'extragrob': 'c4',
               'userstring': 'test' }]

    for d in dicts:
        open (d['name'] + '.ly','w').write (ly_template % d)
        
    names = [d['name'] for d in dicts]
    
    system ('lilypond -ddump-signatures --png -b eps ' + ' '.join (names))
    
    sigs = dict ((n, read_signature_file ('%s-0.signature' % n)) for n in names)
    combinations = {}
    for (n1, s1) in sigs.items():
        for (n2, s2) in sigs.items():
            combinations['%s-%s' % (n1, n2)] = SystemLink (s1,s2).distance ()

    results = combinations.items ()
    results.sort ()
    for k,v in results:
        print '%-20s' % k, v

    assert combinations['20-20'] == 0.0
    assert combinations['20-20expr'] > 50.0
    assert combinations['20-19'] < 10.0


def test_sigs (a,b):
    sa = read_signature_file (a)
    sb = read_signature_file (b)
    link = SystemLink (sa, sb)
    print link.distance()


def run_tests ():
    do_clean = 1
    dir = 'output-distance-test'

    print 'test results in ', dir
    if do_clean:
        system ('rm -rf ' + dir)
        system ('mkdir ' + dir)
        
    os.chdir (dir)

    test_basic_compare ()
    test_compare_trees ()
    
################################################################
#

def main ():
    p = optparse.OptionParser ("output-distance - compare LilyPond formatting runs")
    p.usage = 'output-distance.py [options] tree1 tree2'
    
    p.add_option ('', '--test',
                  dest="run_test",
                  action="store_true",
                  help='run test method')

    (o,a) = p.parse_args ()

    if o.run_test:
        run_tests ()
        sys.exit (0)

    if len (a) != 2:
        p.print_usage()
        sys.exit (2)

    compare_trees (a[0], a[1])

if __name__ == '__main__':
    main()

