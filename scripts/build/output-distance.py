#!@PYTHON@
import sys
import optparse
import os
import math

## so we can call directly as scripts/build/output-distance.py
me_path = os.path.abspath (os.path.split (sys.argv[0])[0])
sys.path.insert (0, me_path + '/../python/')
sys.path.insert (0, me_path + '/../python/out/')


X_AXIS = 0
Y_AXIS = 1
INFTY = 1e6

OUTPUT_EXPRESSION_PENALTY = 1
ORPHAN_GROB_PENALTY = 1
options = None

################################################################
# system interface.
temp_dir = None
class TempDirectory:
    def __init__ (self):
        import tempfile
        self.dir = tempfile.mkdtemp ()
        print 'dir is', self.dir
    def __del__ (self):
        print 'rm -rf %s' % self.dir 
        os.system ('rm -rf %s' % self.dir)
    def __call__ (self):
        return self.dir


def get_temp_dir  ():
    global temp_dir
    if not temp_dir:
        temp_dir = TempDirectory ()
    return temp_dir ()

def read_pipe (c):
    print 'pipe' , c
    return os.popen (c).read ()

def system (c):
    print 'system' , c
    s = os.system (c)
    if s :
        raise Exception ("failed")
    return

def shorten_string (s):
    threshold = 15 
    if len (s) > 2*threshold:
        s = s[:threshold] + '..' + s[-threshold:]
    return s

def max_distance (x1, x2):
    dist = 0.0

    for (p,q) in zip (x1, x2):
        dist = max (abs (p-q), dist)
        
    return dist


def compare_png_images (old, new, dest_dir):
    def png_dims (f):
        m = re.search ('([0-9]+) x ([0-9]+)', read_pipe ('file %s' % f))
        
        return tuple (map (int, m.groups ()))

    dest = os.path.join (dest_dir, new.replace ('.png', '.compare.jpeg'))
    try:
        dims1 = png_dims (old)
        dims2 = png_dims (new)
    except AttributeError:
        ## hmmm. what to do?
        system ('touch %(dest)s' % locals ())
        return
    
    dims = (min (dims1[0], dims2[0]),
            min (dims1[1], dims2[1]))

    dir = get_temp_dir ()
    system ('convert -depth 8 -crop %dx%d+0+0 %s %s/crop1.png' % (dims + (old, dir)))
    system ('convert -depth 8 -crop %dx%d+0+0 %s %s/crop2.png' % (dims + (new, dir)))

    system ('compare -depth 8 %(dir)s/crop1.png %(dir)s/crop2.png %(dir)s/diff.png' % locals ())

    system ("convert  -depth 8 %(dir)s/diff.png -blur 0x3 -negate -channel alpha,blue -type TrueColorMatte -fx 'intensity'    %(dir)s/matte.png" % locals ())

    system ("composite -compose atop -quality 65 %(dir)s/matte.png %(new)s %(dest)s" % locals ())


################################################################
# interval/bbox arithmetic.

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
            return 0
        else:
            return 1

################################################################
# single System.

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

################################################################
## comparison of systems.

class SystemLink:
    def __init__ (self, system1, system2):
        self.system1 = system1
        self.system2 = system2
        
        self.link_list_dict = {}
        self.back_link_dict = {}


        ## pairs
        self.orphans = []

        ## pair -> distance
        self.geo_distances = {}

        ## pairs
        self.expression_changed = []

        self._geometric_distance = None
        self._expression_change_count = None
        self._orphan_count = None
        
        for g in system1.grobs ():

            ## skip empty bboxes.
            if bbox_is_empty (g.bbox):
                continue
            
            closest = system2.closest (g.name, g.centroid)
            
            self.link_list_dict.setdefault (closest, [])
            self.link_list_dict[closest].append (g)
            self.back_link_dict[g] = closest


    def calc_geometric_distance (self):
        total = 0.0
        for (g1,g2) in self.back_link_dict.items ():
            if g2:
                d = g1.bbox_distance (g2)
                if d:
                    self.geo_distances[(g1,g2)] = d

                total += d

        self._geometric_distance = total
    
    def calc_orphan_count (self):
        count = 0
        for (g1, g2) in self.back_link_dict.items ():
            if g2 == None:
                self.orphans.append ((g1, None))
                
                count += 1

        self._orphan_count = count
    
    def calc_output_exp_distance (self):
        d = 0
        for (g1,g2) in self.back_link_dict.items ():
            if g2:
                d += g1.expression_distance (g2)

        self._expression_change_count = d

    def output_expression_details_string (self):
        return ', '.join ([g1.name for g1 in self.expression_changed])
    
    def geo_details_string (self):
        results = [(d, g1,g2) for ((g1, g2), d) in self.geo_distances.items()]
        results.sort ()
        results.reverse ()
        
        return ', '.join (['%s: %f' % (g1.name, d) for (d, g1, g2) in results])

    def orphan_details_string (self):
        return ', '.join (['%s-None' % g1.name for (g1,g2) in self.orphans if g2==None])

    def geometric_distance (self):
        if self._geometric_distance == None:
            self.calc_geometric_distance ()
        return self._geometric_distance
    
    def orphan_count (self):
        if self._orphan_count == None:
            self.calc_orphan_count ()
            
        return self._orphan_count
    
    def output_expression_change_count (self):
        if self._expression_change_count == None:
            self.calc_output_exp_distance ()
        return self._expression_change_count
        
    def distance (self):
        return (self.output_expression_change_count (),
                self.orphan_count (),
                self.geometric_distance ())
    
def read_signature_file (name):
    print 'reading', name
    
    entries = open (name).read ().split ('\n')
    def string_to_tup (s):
        return tuple (map (float, s.split (' '))) 

    def string_to_entry (s):
        fields = s.split('@')
        fields[2] = string_to_tup (fields[2])
        fields[3] = string_to_tup (fields[3])

        return tuple (fields)
    
    entries = [string_to_entry (e) for e in entries
               if e and not e.startswith ('#')]

    grob_sigs = [GrobSignature (e) for e in entries]
    sig = SystemSignature (grob_sigs)
    return sig


################################################################
# different systems of a .ly file.

hash_to_original_name = {}

class FileLink:
    def __init__ (self, f1, f2):
        self._distance = None
        self.file_names = (f1, f2)
        
    def text_record_string (self):
        return '%-30f %-20s\n' % (self.distance (),
                                  self.name ()
                                  + os.path.splitext (self.file_names[1])[1]
                                  )
    
    def calc_distance (self):
        return 0.0

    def distance (self):
        if self._distance == None:
           self._distance = self.calc_distance ()

        return self._distance
    
    def source_file (self):
        for ext in ('.ly', '.ly.txt'):
            base = os.path.splitext (self.file_names[1])[0]
            f = base + ext
            if os.path.exists (f):
                return f
            
        return ''
        
    def name (self):
        base = os.path.basename (self.file_names[1])
        base = os.path.splitext (base)[0]
        base = hash_to_original_name.get (base, base)
        base = os.path.splitext (base)[0]
        return base
    
    def extension (self):
        return os.path.splitext (self.file_names[1])[1]

    def link_files_for_html (self, dest_dir):
        for f in self.file_names:
            link_file (f, os.path.join (dest_dir, f))

    def get_distance_details (self):
        return ''

    def get_cell (self, oldnew):
        return ''
    
    def get_file (self, oldnew):
        return self.file_names[oldnew]
    
    def html_record_string (self, dest_dir):
        dist = self.distance()
        
        details = self.get_distance_details ()
        if details:
            details_base = os.path.splitext (self.file_names[1])[0]
            details_base += '.details.html'
            fn = dest_dir + '/'  + details_base
            open_write_file (fn).write (details)

            details = '<br>(<a href="%(details_base)s">details</a>)' % locals ()

        cell1 = self.get_cell (0)
        cell2 = self.get_cell (1)

        name = self.name () + self.extension ()
        file1 = self.get_file (0)
        file2 = self.get_file (1)
        
        return '''<tr>
<td>
%(dist)f
%(details)s
</td>
<td>%(cell1)s<br><font size=-2><a href="%(file1)s"><tt>%(name)s</tt></font></td>
<td>%(cell2)s<br><font size=-2><a href="%(file2)s"><tt>%(name)s</tt></font></td>
</tr>''' % locals ()


class FileCompareLink (FileLink):
    def __init__ (self, f1, f2):
        FileLink.__init__ (self, f1, f2)
        self.contents = (self.get_content (self.file_names[0]),
                         self.get_content (self.file_names[1]))
        

    def calc_distance (self):
        ## todo: could use import MIDI to pinpoint
        ## what & where changed.

        if self.contents[0] == self.contents[1]:
            return 0.0
        else:
            return 100.0;
        
    def get_content (self, f):
        print 'reading', f
        s = open (f).read ()
        return s


class GitFileCompareLink (FileCompareLink):
    def get_cell (self, oldnew):
        str = self.contents[oldnew]

        # truncate long lines
        str = '\n'.join ([l[:80] for l in str.split ('\n')])

        
        str = '<font size="-2"><pre>%s</pre></font>' % str
        return str
    
    def calc_distance (self):
        if self.contents[0] == self.contents[1]:
            d = 0.0
        else:
            d = 1.0001 *options.threshold

        return d

        
class TextFileCompareLink (FileCompareLink):
    def calc_distance (self):
        import difflib
        diff = difflib.unified_diff (self.contents[0].strip().split ('\n'),
                                     self.contents[1].strip().split ('\n'),
                                     fromfiledate = self.file_names[0],
                                     tofiledate = self.file_names[1]
                                     )
        
        self.diff_lines =  [l for l in diff]
        self.diff_lines = self.diff_lines[2:]
        
        return math.sqrt (float (len ([l for l in self.diff_lines if l[0] in '-+'])))
        
    def get_cell (self, oldnew):
        str = ''
        if oldnew == 1:
            str = '\n'.join ([d.replace ('\n','') for d in self.diff_lines])
        str = '<font size="-2"><pre>%s</pre></font>' % str
        return str

class LogFileCompareLink (TextFileCompareLink):
  def get_content (self, f):
      c = TextFileCompareLink.get_content (self, f)
      c = re.sub ("\nProcessing `[^\n]+'\n", '', c)
      return c
        
class ProfileFileLink (FileCompareLink):
    def __init__ (self, f1, f2):
        FileCompareLink.__init__ (self, f1, f2)
        self.results = [{}, {}]
    
    def get_cell (self, oldnew):
        str = ''
        for k in ('time', 'cells'):
            if oldnew==0:
                str += '%-8s: %d\n' %  (k, int (self.results[oldnew][k]))
            else:
                str += '%-8s: %8d (%5.3f)\n' % (k, int (self.results[oldnew][k]),
                                                self.get_ratio (k))

        return '<pre>%s</pre>' % str
            
    def get_ratio (self, key):
        (v1,v2) = (self.results[0].get (key, -1),
                   self.results[1].get (key, -1))

        if v1 <= 0 or v2 <= 0:
            return 0.0

        return (v1 - v2) / float (v1+v2)
    
    def calc_distance (self):
        for oldnew in (0,1):
            def note_info (m):
                self.results[oldnew][m.group(1)] = float (m.group (2))
            
            re.sub ('([a-z]+): ([-0-9.]+)\n',
                    note_info, self.contents[oldnew])

        dist = 0.0
        factor = {
            'time': 0.1,
            'cells': 5.0,
            }
        
        for k in ('time', 'cells'):
            real_val = math.tan (self.get_ratio (k) * 0.5 * math.pi)
            dist += math.exp (math.fabs (real_val) * factor[k])  - 1

        dist = min (dist, 100)
        return dist

    
class MidiFileLink (TextFileCompareLink):
    def get_content (self, oldnew):
        import midi
        
        data = FileCompareLink.get_content (self, oldnew)
        midi = midi.parse (data)
        tracks = midi[1]

        str = ''
        j = 0
        for t in tracks:
            str += 'track %d' % j
            j += 1

            for e in t:
                ev_str = repr (e)
                if re.search ('LilyPond [0-9.]+', ev_str):
                    continue
                
                str += '  ev %s\n' % `e`
        return str
    


class SignatureFileLink (FileLink):
    def __init__ (self, f1, f2 ):
        FileLink.__init__ (self, f1, f2)
        self.system_links = {}

    def add_system_link (self, link, number):
        self.system_links[number] = link

    def calc_distance (self):
        d = 0.0

        orphan_distance = 0.0
        for l in self.system_links.values ():
            d = max (d, l.geometric_distance ())
            orphan_distance += l.orphan_count ()
            
        return d + orphan_distance

    def add_file_compare (self, f1, f2):
        system_index = [] 

        def note_system_index (m):
            system_index.append (int (m.group (1)))
            return ''
        
        base1 = re.sub ("-([0-9]+).signature", note_system_index, f1)
        base2 = re.sub ("-([0-9]+).signature", note_system_index, f2)

        self.base_names = (os.path.normpath (base1),
                           os.path.normpath (base2))

        s1 = read_signature_file (f1)
        s2 = read_signature_file (f2)

        link = SystemLink (s1, s2)

        self.add_system_link (link, system_index[0])

    
    def create_images (self, dest_dir):

        files_created = [[], []]
        for oldnew in (0, 1):
            pat = self.base_names[oldnew] + '.eps'

            for f in glob.glob (pat):
                infile = f
                outfile = (dest_dir + '/' + f).replace ('.eps', '.png')
                data_option = ''
                if options.local_data_dir:
                    data_option = ('-slilypond-datadir=%s/../share/lilypond/current '
                                   % os.path.dirname(infile))
                
                mkdir (os.path.split (outfile)[0])
                cmd = ('gs -sDEVICE=png16m -dGraphicsAlphaBits=4 -dTextAlphaBits=4 '
                       ' %(data_option)s '
                       ' -r101 '
                       ' -sOutputFile=%(outfile)s -dNOSAFER -dEPSCrop -q -dNOPAUSE '
                       ' %(infile)s  -c quit ') % locals ()

                files_created[oldnew].append (outfile)
                system (cmd)

        return files_created
    
    def link_files_for_html (self, dest_dir):
        FileLink.link_files_for_html (self, dest_dir)
        to_compare = [[], []]

        exts = []
        if options.create_images:
            to_compare = self.create_images (dest_dir)
        else:
            exts += ['.png', '-page*png']
        
        for ext in exts:            
            for oldnew in (0,1):
                for f in glob.glob (self.base_names[oldnew] + ext):
                    dst = dest_dir + '/' + f
                    link_file (f, dst)

                    if f.endswith ('.png'):
                        to_compare[oldnew].append (f)
                        
        if options.compare_images:                
            for (old, new) in zip (to_compare[0], to_compare[1]):
                compare_png_images (old, new, dest_dir)


    def get_cell (self, oldnew):
        def img_cell (ly, img, name):
            if not name:
                name = 'source'
            else:
                name = '<tt>%s</tt>' % name
                
            return '''
<a href="%(img)s">
<img src="%(img)s" style="border-style: none; max-width: 500px;">
</a><br>
''' % locals ()
        def multi_img_cell (ly, imgs, name):
            if not name:
                name = 'source'
            else:
                name = '<tt>%s</tt>' % name

            imgs_str = '\n'.join (['''<a href="%s">
<img src="%s" style="border-style: none; max-width: 500px;">
</a><br>''' % (img, img) 
                                  for img in imgs])


            return '''
%(imgs_str)s
''' % locals ()



        def cell (base, name):
            pat = base + '-page*.png'
            pages = glob.glob (pat)

            if pages:
                return multi_img_cell (base + '.ly', sorted (pages), name)
            else:
                return img_cell (base + '.ly', base + '.png', name)



        str = cell (os.path.splitext (self.file_names[oldnew])[0], self.name ())  
        if options.compare_images and oldnew == 1:
            str = str.replace ('.png', '.compare.jpeg')
            
        return str


    def get_distance_details (self):
        systems = self.system_links.items ()
        systems.sort ()

        html = ""
        for (c, link) in systems:
            e = '<td>%d</td>' % c
            for d in link.distance ():
                e += '<td>%f</td>' % d
            
            e = '<tr>%s</tr>' % e

            html += e

            e = '<td>%d</td>' % c
            for s in (link.output_expression_details_string (),
                      link.orphan_details_string (),
                      link.geo_details_string ()):
                e += "<td>%s</td>" % s

            
            e = '<tr>%s</tr>' % e
            html += e
            
        original = self.name ()
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

    files = []
    for d in (dir1,dir2):
        found = [os.path.split (f)[1] for f in glob.glob (d + '/' + pattern)]
        found = dict ((f, 1) for f in found)
        files.append (found)
        
    pairs = []
    missing = []
    for f in files[0]:
        try:
            files[1].pop (f)
            pairs.append (f)
        except KeyError:
            missing.append (f)

    return (pairs, files[1].keys (), missing)
    
class ComparisonData:
    def __init__ (self):
        self.result_dict = {}
        self.missing = []
        self.added = []
        self.file_links = {}

    def read_sources (self):

        ## ugh: drop the .ly.txt
        for (key, val) in self.file_links.items ():
            
            def note_original (match, ln=val):
                key = ln.name ()
                hash_to_original_name[key] = match.group (1)
                return ''

            sf = val.source_file ()
            if sf:
                re.sub (r'\\sourcefilename "([^"]+)"',
                        note_original, open (sf).read ())
            else:
                print 'no source for', val
        
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
        for ext in ['signature',
                    'midi',
                    'log',
                    'profile',
                    'gittxt']:
            (paired, m1, m2) = paired_files (dir1, dir2, '*.' + ext)

            self.missing += [(dir1, m) for m in m1] 
            self.added += [(dir2, m) for m in m2] 

            for p in paired:
                if (options.max_count
                    and len (self.file_links) > options.max_count):
                    continue
                
                f2 = dir2 +  '/' + p
                f1 = dir1 +  '/' + p
                self.compare_files (f1, f2)

    def compare_files (self, f1, f2):
        if f1.endswith ('signature'):
            self.compare_signature_files (f1, f2)
        else:
            ext = os.path.splitext (f1)[1]
            klasses = {
                '.midi': MidiFileLink,
                '.log' : LogFileCompareLink,
                '.profile': ProfileFileLink,
                '.gittxt': GitFileCompareLink, 
                }
            
            if klasses.has_key (ext):
                self.compare_general_files (klasses[ext], f1, f2)

    def compare_general_files (self, klass, f1, f2):
        name = os.path.split (f1)[1]

        file_link = klass (f1, f2)
        self.file_links[name] = file_link
        
    def compare_signature_files (self, f1, f2):
        name = os.path.split (f1)[1]
        name = re.sub ('-[0-9]+.signature', '', name)
        
        file_link = None
        try:
            file_link = self.file_links[name]
        except KeyError:
            generic_f1 = re.sub ('-[0-9]+.signature', '.ly', f1)
            generic_f2 = re.sub ('-[0-9]+.signature', '.ly', f2)
            file_link = SignatureFileLink (generic_f1, generic_f2)
            self.file_links[name] = file_link

        file_link.add_file_compare (f1, f2)

    def write_changed (self, dest_dir, threshold):
        (changed, below, unchanged) = self.thresholded_results (threshold)

        str = '\n'.join ([os.path.splitext (link.file_names[1])[0]
                        for link in changed])
        fn = dest_dir + '/changed.txt'
        
        open_write_file (fn).write (str)
                
    def thresholded_results (self, threshold):
        ## todo: support more scores.
        results = [(link.distance(), link)
                   for link in self.file_links.values ()]
        results.sort ()
        results.reverse ()

        unchanged = [r for (d,r) in results if d == 0.0]
        below = [r for (d,r) in results if threshold >= d > 0.0]
        changed = [r for (d,r) in results if d > threshold]

        return (changed, below, unchanged)
                
    def write_text_result_page (self, filename, threshold):
        out = None
        if filename == '':
            out = sys.stdout
        else:
            print 'writing "%s"' % filename
            out = open_write_file (filename)

        (changed, below, unchanged) = self.thresholded_results (threshold)

        
        for link in changed:
            out.write (link.text_record_string ())

        out.write ('\n\n')
        out.write ('%d below threshold\n' % len (below))
        out.write ('%d unchanged\n' % len (unchanged))
        
    def create_text_result_page (self, dir1, dir2, dest_dir, threshold):
        self.write_text_result_page (dest_dir + '/index.txt', threshold)
        
    def create_html_result_page (self, dir1, dir2, dest_dir, threshold):
        dir1 = dir1.replace ('//', '/')
        dir2 = dir2.replace ('//', '/')

        (changed, below, unchanged) = self.thresholded_results (threshold)


        html = ''
        old_prefix = os.path.split (dir1)[1]
        for link in changed:
            html += link.html_record_string (dest_dir)


        short_dir1 = shorten_string (dir1)
        short_dir2 = shorten_string (dir2)
        html = '''<html>
<table rules="rows" border bordercolor="blue">
<tr>
<th>distance</th>
<th>%(short_dir1)s</th>
<th>%(short_dir2)s</th>
</tr>
%(html)s
</table>
</html>''' % locals()

        html += ('<p>')
        below_count = len (below)

        if below_count:
            html += ('<p>%d below threshold</p>' % below_count)
            
        html += ('<p>%d unchanged</p>' % len (unchanged))

        dest_file = dest_dir + '/index.html'
        open_write_file (dest_file).write (html)


        for link in changed:
            link.link_files_for_html (dest_dir)
        

    def print_results (self, threshold):
        self.write_text_result_page ('', threshold)

def compare_trees (dir1, dir2, dest_dir, threshold):
    data = ComparisonData ()
    data.compare_trees (dir1, dir2)
    data.read_sources ()

    
    data.print_results (threshold)

    if os.path.isdir (dest_dir):
        system ('rm -rf %s '% dest_dir)

    data.write_changed (dest_dir, threshold)
    data.create_html_result_page (dir1, dir2, dest_dir, threshold)
    data.create_text_result_page (dir1, dir2, dest_dir, threshold)
    
################################################################
# TESTING

def mkdir (x):
    if not os.path.isdir (x):
        print 'mkdir', x
        os.makedirs (x)

def link_file (x, y):
    mkdir (os.path.split (y)[0])
    try:
        print x, '->', y
        os.link (x, y)
    except OSError, z:
        print 'OSError', x, y, z
        raise OSError
    
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
                        os.environ["HOME"] + "/src/lilypond-stable/scripts/build/", '*.py')
                  
    
def test_compare_trees ():
    system ('rm -rf dir1 dir2')
    system ('mkdir dir1 dir2')
    system ('cp 20{-*.signature,.ly,.png,.eps,.log,.profile} dir1')
    system ('cp 20{-*.signature,.ly,.png,.eps,.log,.profile} dir2')
    system ('cp 20expr{-*.signature,.ly,.png,.eps,.log,.profile} dir1')
    system ('cp 19{-*.signature,.ly,.png,.eps,.log,.profile} dir2/')
    system ('cp 19{-*.signature,.ly,.png,.eps,.log,.profile} dir1/')
    system ('cp 19-1.signature 19.sub-1.signature')
    system ('cp 19.ly 19.sub.ly')
    system ('cp 19.profile 19.sub.profile')
    system ('cp 19.log 19.sub.log')
    system ('cp 19.png 19.sub.png')
    system ('cp 19.eps 19.sub.eps')

    system ('cp 20multipage* dir1')
    system ('cp 20multipage* dir2')
    system ('cp 19multipage-1.signature dir2/20multipage-1.signature')

    
    system ('mkdir -p dir1/subdir/ dir2/subdir/')
    system ('cp 19.sub{-*.signature,.ly,.png,.eps,.log,.profile} dir1/subdir/')
    system ('cp 19.sub{-*.signature,.ly,.png,.eps,.log,.profile} dir2/subdir/')
    system ('cp 20grob{-*.signature,.ly,.png,.eps,.log,.profile} dir2/')
    system ('cp 20grob{-*.signature,.ly,.png,.eps,.log,.profile} dir1/')
    system ('echo HEAD is 1 > dir1/tree.gittxt')
    system ('echo HEAD is 2 > dir2/tree.gittxt')

    ## introduce differences
    system ('cp 19-1.signature dir2/20-1.signature')
    system ('cp 19.profile dir2/20.profile')
    system ('cp 19.png dir2/20.png')
    system ('cp 19multipage-page1.png dir2/20multipage-page1.png')
    system ('cp 20-1.signature dir2/subdir/19.sub-1.signature')
    system ('cp 20.png dir2/subdir/19.sub.png')
    system ("sed 's/: /: 1/g'  20.profile > dir2/subdir/19.sub.profile")

    ## radical diffs.
    system ('cp 19-1.signature dir2/20grob-1.signature')
    system ('cp 19-1.signature dir2/20grob-2.signature')
    system ('cp 19multipage.midi dir1/midi-differ.midi')
    system ('cp 20multipage.midi dir2/midi-differ.midi')
    system ('cp 19multipage.log dir1/log-differ.log')
    system ('cp 19multipage.log dir2/log-differ.log &&  echo different >> dir2/log-differ.log &&  echo different >> dir2/log-differ.log')

    compare_trees ('dir1', 'dir2', 'compare-dir1dir2', options.threshold)


def test_basic_compare ():
    ly_template = r"""

\version "2.10.0"
#(define default-toplevel-book-handler
  print-book-with-defaults-as-systems )

#(ly:set-option (quote no-point-and-click))

\sourcefilename "my-source.ly"
 
%(papermod)s
\header { tagline = ##f }
\score {
<<
\new Staff \relative c {
  c4^"%(userstring)s" %(extragrob)s
  }
\new Staff \relative c {
  c4^"%(userstring)s" %(extragrob)s
  }
>>
\layout{}
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
               'extragrob': 'r2. \\break c1',
               'userstring': 'test' },
             ]

    for d in dicts:
        open (d['name'] + '.ly','w').write (ly_template % d)
        
    names = [d['name'] for d in dicts]

    system ('lilypond -ddump-profile -dseparate-log-files -ddump-signatures --png -dbackend=eps ' + ' '.join (names))
    

    multipage_str = r'''
    #(set-default-paper-size "a6")
    \score {
      \relative {c1 \pageBreak c1 }
      \layout {}
      \midi {}
    }
    '''

    open ('20multipage.ly', 'w').write (multipage_str.replace ('c1', 'd1'))
    open ('19multipage.ly', 'w').write ('#(set-global-staff-size 19.5)\n' + multipage_str)
    system ('lilypond -dseparate-log-files -ddump-signatures --png 19multipage 20multipage ')
 
    test_compare_signatures (names)
    
def test_compare_signatures (names, timing=False):
    import time

    times = 1
    if timing:
        times = 100

    t0 = time.clock ()

    count = 0
    for t in range (0, times):
        sigs = dict ((n, read_signature_file ('%s-1.signature' % n)) for n in names)
        count += 1

    if timing:
        print 'elapsed', (time.clock() - t0)/count


    t0 = time.clock ()
    count = 0
    combinations = {}
    for (n1, s1) in sigs.items():
        for (n2, s2) in sigs.items():
            combinations['%s-%s' % (n1, n2)] = SystemLink (s1,s2).distance ()
            count += 1

    if timing:
        print 'elapsed', (time.clock() - t0)/count

    results = combinations.items ()
    results.sort ()
    for k,v in results:
        print '%-20s' % k, v

    assert combinations['20-20'] == (0.0,0.0,0.0)
    assert combinations['20-20expr'][0] > 0.0
    assert combinations['20-19'][2] < 10.0
    assert combinations['20-19'][2] > 0.0


def run_tests ():
    dir = 'test-output-distance'

    do_clean = not os.path.exists (dir)

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
    
    p.add_option ('', '--test-self',
                  dest="run_test",
                  action="store_true",
                  help='run test method')
    
    p.add_option ('--max-count',
                  dest="max_count",
                  metavar="COUNT",
                  type="int",
                  default=0, 
                  action="store",
                  help='only analyze COUNT signature pairs')

    p.add_option ('', '--threshold',
                  dest="threshold",
                  default=0.3,
                  action="store",
                  type="float",
                  help='threshold for geometric distance')

    p.add_option ('--no-compare-images',
                  dest="compare_images",
                  default=True,
                  action="store_false",
                  help="Don't run graphical comparisons")

    p.add_option ('--create-images',
                  dest="create_images",
                  default=False,
                  action="store_true",
                  help="Create PNGs from EPSes")


    p.add_option ('--local-datadir',
                  dest="local_data_dir",
                  default=False,
                  action="store_true",
                  help='whether to use the share/lilypond/ directory in the test directory')

    p.add_option ('-o', '--output-dir',
                  dest="output_dir",
                  default=None,
                  action="store",
                  type="string",
                  help='where to put the test results [tree2/compare-tree1tree2]')

    global options
    (options, args) = p.parse_args ()

    if options.run_test:
        run_tests ()
        sys.exit (0)

    if len (args) != 2:
        p.print_usage()
        sys.exit (2)

    name = options.output_dir
    if not name:
        name = args[0].replace ('/', '')
        name = os.path.join (args[1], 'compare-' + shorten_string (name))
    
    compare_trees (args[0], args[1], name, options.threshold)

if __name__ == '__main__':
    main()

