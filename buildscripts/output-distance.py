#!@TARGET_PYTHON@
import sys
import optparse
import os

## so we can call directly as buildscripts/output-distance.py
me_path = os.path.abspath (os.path.split (sys.argv[0])[0])
sys.path.insert (0, me_path + '/../python/')


import safeeval


X_AXIS = 0
Y_AXIS = 1
INFTY = 1e6

OUTPUT_EXPRESSION_PENALTY = 1
ORPHAN_GROB_PENALTY = 1
THRESHOLD = 1.0

def max_distance (x1, x2):
    dist = 0.0

    for (p,q) in zip (x1, x2):
        dist = max (abs (p-q), dist)
        
    return dist


empty_interval = (INFTY, -INFTY)
empty_bbox = (empty_interval, empty_interval)

def interval_is_empty (i):
    return i[0] > i[1]

def interval_length (i):
    return max (i[1]-i[0], 0) 
    
def interval_union (i1, i2):
    return (min (i1[0], i2[0]),
            max (i1[1], i2[1]))

def interval_intersect (i1, i2):
    return (max (i1[0], i2[0]),
            min (i1[1], i2[1]))

def bbox_is_empty (b):
    return (interval_is_empty (b[0])
            or interval_is_empty (b[1]))

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

            ## skip empty bboxes.
            if bbox_is_empty (g.bbox):
                continue
            
            closest = system2.closest (g.name, g.centroid)
            
            self.link_list_dict.setdefault (closest, [])
            self.link_list_dict[closest].append (g)
            self.back_link_dict[g] = closest

    def geometric_distance (self):
        d = 0.0
        for (g1,g2) in self.back_link_dict.items ():
            if g2:
                # , scale
                d += g1.bbox_distance (g2)

        return d
    
    def orphan_distance (self):
        d = 0
        for (g1,g2) in self.back_link_dict.items ():
            if g2 == None:
                d += ORPHAN_GROB_PENALTY
        return d
    
    def output_exp_distance (self):
        d = 0
        for (g1,g2) in self.back_link_dict.items ():
            if g2:
                d += g1.expression_distance (g2)

        return d

    def distance (self):
        return (self.output_exp_distance (),
                self.orphan_distance (),
                self.geometric_distance ())
    
def read_signature_file (name):
    print 'reading', name
    exp_str = ("[%s]" % open (name).read ())
    entries = safeeval.safe_eval (exp_str)

    grob_sigs = [GrobSignature (e) for e in entries]
    sig = SystemSignature (grob_sigs)
    return sig


class FileLink:
    def __init__ (self):
        self.original_name = ''
        self.base_names = ('','')
        self.system_links = {}
        self._distance = None
        
    def add_system_link (self, link, number):
        self.system_links[number] = link

    def calc_distance (self):
        d = 0.0
        for l in self.system_links.values ():
            d = max (d, l.geometric_distance ())
        return d

    def distance (self):
        if type (self._distance) != type (0.0):
            return self.calc_distance ()
        
        return self._distance

    def text_record_string (self):
        return '%-30f %-20s\n' % (self.distance (),
                             self.original_name)

    def source_file (self):
        for ext in ('.ly', '.ly.txt'):
            if os.path.exists (self.base_names[1] + ext):
                return self.base_names[1] + ext
        return ''
    
    def add_file_compare (self, f1, f2):
        system_index = [] 

        def note_system_index (m):
            system_index.append (int (m.group (1)))
            return ''
        
        base1 = re.sub ("-([0-9]+).signature", note_system_index, f1)
        base2 = re.sub ("-([0-9]+).signature", note_system_index, f2)

        self.base_names = (os.path.normpath (base1),
                           os.path.normpath (base2))

        def note_original (match):
            self.original_name = match.group (1)
            return ''
        
        if not self.original_name:
            self.original_name = os.path.split (base1)[1]

            ## ugh: drop the .ly.txt
            for ext in ('.ly', '.ly.txt'):
                try:
                    re.sub (r'\\sourcefilename "([^"]+)"',
                            note_original, open (base1 + ext).read ())
                except IOError:
                    pass
                
        s1 = read_signature_file (f1)
        s2 = read_signature_file (f2)

        link = SystemLink (s1, s2)

        self.add_system_link (link, system_index[0])

    def link_files_for_html (self, old_dir, new_dir, dest_dir):
        for ext in ('.png', '.ly'):
            for oldnew in (0,1):
                link_file (self.base_names[oldnew] + ext, 
                           dest_dir + '/' + self.base_names[oldnew] + ext)

    def html_record_string (self,  old_dir, new_dir):
        def img_cell (ly, img, name):
            if not name:
                name = 'source'
            else:
                name = '<tt>%s</tt>' % name
                
            return '''
<td align="center">
<a href="%(img)s">
<img src="%(img)s" style="border-style: none; max-width: 500px;">
</a><br>
<font size="-2">(<a href="%(ly)s">%(name)s</a>)
</font>
</td>
''' % locals ()
        

        img_1  = self.base_names[0] + '.png'
        ly_1  = self.base_names[0] + '.ly'
        img_2  = self.base_names[1] + '.png'
        ly_2  = self.base_names[1] + '.ly'
        html_2  = self.base_names[1] + '.html'
        name = self.original_name
        
        html_entry = '''
<tr>
<td>
%f<br>
(<a href="%s">details</a>)
</td>

%s
%s
</tr>
''' % (self.distance (), html_2,
       img_cell (ly_1, img_1, name), img_cell (ly_2, img_2, name))


        return html_entry


    def html_system_details_string (self):
        systems = self.system_links.items ()
        systems.sort ()

        html = ""
        for (c, link) in systems:
            e = '<td>%d</td>' % c
            for d in link.distance ():
                e += '<td>%f</td>' % d
            
            e = '<tr>%s</tr>' % e
            html += e

        original = self.original_name
        html = '''<html>
<head>
<title>comparison details for %(original)s</title>
</head>
<body>
<table border=1>
<tr>
<th>system</th>
<th>output</th>
<th>orphan</th>
<th>geo</th>
</tr>

%(html)s
</table>

</body>
</html>
''' % locals ()
        return html

    def write_html_system_details (self, dir1, dir2, dest_dir):
        dest_file =  os.path.join (dest_dir, self.base_names[1] + '.html')

        details = open_write_file (dest_file)
        details.write (self.html_system_details_string ())

################################################################
# Files/directories

import glob
import re



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
        self.file_links = {}
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
            if len (self.file_links) > 10:
                continue
            
            f2 = dir2 +  '/' + p
            f1 = dir1 +  '/' + p
            self.compare_files (f1, f2)

    def compare_files (self, f1, f2):
        name = os.path.split (f1)[1]
        name = re.sub ('-[0-9]+.signature', '', name)
        
        file_link = None
        try:
            file_link = self.file_links[name]
        except KeyError:
            file_link = FileLink ()
            self.file_links[name] = file_link

        file_link.add_file_compare (f1,f2)

    def write_text_result_page (self, filename):
        print 'writing "%s"' % filename
        out = None
        if filename == '':
            out = sys.stdout
        else:
            out = open_write_file (filename)

        ## todo: support more scores.
        results = [(link.distance(), link)
                   for link in self.file_links.values ()]
        results.sort ()
        results.reverse ()

        
        for (score, link) in results:
            if score > THRESHOLD:
                out.write (link.text_record_string ())

        out.write ('\n\n')
        out.write ('%d below threshold\n' % len ([1 for s,l  in results
                                                    if THRESHOLD >=  s > 0.0]))
        out.write ('%d unchanged\n' % len ([1 for (s,l) in results if s == 0.0]))
        
    def create_text_result_page (self, dir1, dir2, dest_dir):
        self.write_text_result_page (dest_dir + '/index.txt')
        
    def create_html_result_page (self, dir1, dir2, dest_dir):
        dir1 = dir1.replace ('//', '/')
        dir2 = dir2.replace ('//', '/')
        
        results = [(link.distance(), link)
                   for link in self.file_links.values ()]
        results.sort ()
        results.reverse ()

        html = ''
        old_prefix = os.path.split (dir1)[1]
        for (score, link) in results:
            if score <= THRESHOLD:
                continue

            link.write_html_system_details (dir1, dir2, dest_dir)
            link.link_files_for_html (dir1, dir2, dest_dir) 
            html += link.html_record_string (dir1, dir2)


        html = '''<html>
<table rules="rows" border bordercolor="blue">
<tr>
<th>distance</th>
<th>old</th>
<th>new</th>
</tr>
%(html)s
</table>
</html>''' % locals()

        html += ('<p>')
        below_count  =len ([1 for s,l  in results
                         if THRESHOLD >=  s > 0.0])

        if below_count:
            html += ('<p>%d below threshold</p>' % below_count)

        html += ('<p>%d unchanged</p>'
                 % len ([1 for (s,l) in results if s == 0.0]))


        dest_file = dest_dir + '/index.html'
        open_write_file (dest_file).write (html)
        
    def print_results (self):
        self.write_text_result_page ('')
        

def compare_trees (dir1, dir2, dest_dir):
    data = ComparisonData ()
    data.compare_trees (dir1, dir2)
    data.print_results ()

    if os.path.isdir (dest_dir):
        system ('rm -rf %s '% dest_dir)

    data.create_html_result_page (dir1, dir2, dest_dir)
    data.create_text_result_page (dir1, dir2, dest_dir)
    
################################################################
# TESTING

def mkdir (x):
    if not os.path.isdir (x):
        print 'mkdir', x
        os.makedirs (x)

def link_file (x, y):
    mkdir (os.path.split (y)[0])
    os.link (x, y)
    
def open_write_file (x):
    d = os.path.split (x)[0]
    mkdir (d)
    return open (x, 'w')


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
    system ('cp 20{-*.signature,.ly,.png} dir1')
    system ('cp 20{-*.signature,.ly,.png} dir2')
    system ('cp 20expr{-*.signature,.ly,.png} dir1')
    system ('cp 19{-*.signature,.ly,.png} dir2/')
    system ('cp 19{-*.signature,.ly,.png} dir1/')
    system ('cp 19-1.signature 19-sub-1.signature')
    system ('cp 19.ly 19-sub.ly')
    system ('cp 19.png 19-sub.png')
    
    system ('mkdir -p dir1/subdir/ dir2/subdir/')
    system ('cp 19-sub{-*.signature,.ly,.png} dir1/subdir/')
    system ('cp 19-sub{-*.signature,.ly,.png} dir2/subdir/')
    system ('cp 20grob{-*.signature,.ly,.png} dir2/')
    system ('cp 20grob{-*.signature,.ly,.png} dir1/')

    ## introduce differences
    system ('cp 19-1.signature dir2/20-1.signature')

    ## radical diffs.
    system ('cp 19-1.signature dir2/20grob-1.signature')
    system ('cp 19-1.signature dir2/20grob-2.signature')

    compare_trees ('dir1', 'dir2', 'compare-dir1dir2')


def test_basic_compare ():
    ly_template = r"""#(set! toplevel-score-handler print-score-with-defaults)
#(set! toplevel-music-handler
 (lambda (p m)
 (if (not (eq? (ly:music-property m 'void) #t))
    (print-score-with-defaults
    p (scorify-music m p)))))

\sourcefilename "my-source.ly"

%(papermod)s
<<
\new Staff \relative c {
  c4^"%(userstring)s" %(extragrob)s
  }
\new Staff \relative c {
  c4^"%(userstring)s" %(extragrob)s
  }
>>
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
               'extragrob': 'r2. \\break c1',
               'userstring': 'test' }

             ]

    for d in dicts:
        open (d['name'] + '.ly','w').write (ly_template % d)
        
    names = [d['name'] for d in dicts]
    
    system ('lilypond -ddump-signatures --png -b eps ' + ' '.join (names))
    
    sigs = dict ((n, read_signature_file ('%s-1.signature' % n)) for n in names)
    combinations = {}
    for (n1, s1) in sigs.items():
        for (n2, s2) in sigs.items():
            combinations['%s-%s' % (n1, n2)] = SystemLink (s1,s2).distance ()

    results = combinations.items ()
    results.sort ()
    for k,v in results:
        print '%-20s' % k, v

    assert combinations['20-20'] == (0.0,0.0,0.0)
    assert combinations['20-20expr'][0] > 0.0
    assert combinations['20-19'][2] < 10.0
    assert combinations['20-19'][2] > 0.0


def run_tests ():
    do_clean = 0
    dir = 'output-distance-test'

    print 'test results in ', dir
    if do_clean:
        system ('rm -rf ' + dir)
        system ('mkdir ' + dir)
        
    os.chdir (dir)
    if do_clean:
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

