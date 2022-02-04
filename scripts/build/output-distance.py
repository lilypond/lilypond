# output-distance.py
# -*- coding: utf-8 -*-
#
# This file is part of LilyPond, the GNU music typesetter.
#
# Copyright (C) 2006--2022  Han-Wen Nienhuys <hanwen@xs4all.nl>
#
# LilyPond is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# LilyPond is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.


import difflib
import errno
import functools
import glob
import html
import math
import optparse
import os
import re
import shutil
import subprocess
import sys
import tempfile
import time
from typing import Dict, List, Tuple, Optional

Vec = Tuple[float, float]
Interval = Tuple[float, float]
Bbox = Tuple[Interval, Interval]

# so we can call directly as scripts/build/output-distance.py
me_path = os.path.abspath(os.path.split(sys.argv[0])[0])
sys.path.insert(0, me_path + '/../../python/')

# Keep our includes after adapting sys.path above.
import midi # type: ignore

X_AXIS = 0
Y_AXIS = 1
INFTY = 1e6

ORPHAN_GROB_PENALTY = 1
options: optparse.Values


def log_terse(s: str):
    if not options.verbose:
        print(s)


def log_verbose(s: str):
    if options.verbose:
        print(s)


################################################################
# system interface.
temp_dir = ''


def get_temp_dir() -> str:
    global temp_dir
    if not temp_dir:
        temp_dir = tempfile.mkdtemp()
    return temp_dir


def system(c: str, cwd=None):
    if not cwd:
        cwd = os.getcwd()
    log_verbose('system %s (cwd=%s)' % (c, cwd))
    # explicitly use bash, so we don't get dash on Ubuntu.
    subprocess.run(["/bin/bash", "-c", c.encode('utf-8')], check=True, cwd=cwd)


def shorten_string(s: str, threshold: int = 15) -> str:
    if len(s) > 2*threshold:
        s = s[:threshold] + '..' + s[-threshold:]
    return s


def max_distance(x1: Vec, x2: Vec) -> float:
    dist = 0.0

    for (p, q) in zip(x1, x2):
        dist = max(abs(p-q), dist)

    return dist


def png_dims(fn: str) -> Tuple[int, int]:
    """Reads width/height from PNG file."""

    # avoid subprocessing, as subprocessing is relatively expensive,
    # and we run this for each image we handle
    with open(fn, 'rb') as f:
        header = f.read(32)
        w = int.from_bytes(header[16:20], 'big', signed=False)
        h = int.from_bytes(header[20:24], 'big', signed=False)
        return (w, h)


def compare_png_images(old: str, new: str, dest_dir: str) -> float:
    file_dims = {}
    for fn in [old, new]:
        if os.path.exists(fn):
            file_dims[fn] = png_dims(fn)
    
    maxdims: Tuple = tuple(map(max, zip(*file_dims.values())))
    temp_dir = get_temp_dir()

    for (new_name, input_name) in [('crop1', old), ('crop2', new)]:
        dest_file = os.path.join(temp_dir, new_name + '.png')
        if maxdims == file_dims[input_name]:
            # Avoid spawning subprocess if we can
            if options.verbose:
                print('cp %s %s' % (input_name, dest_file))
            shutil.copy(input_name, dest_file)
            continue
        
        # Uses PNG32:filename to generate cropped image in color (most
        # input images are grayscale, but we don't want a grayscale
        # cropX.png). We need -flatten to extend the size beyond its
        # current size.
        args = ['convert', '-crop', '%dx%d+0+0!' % maxdims,
                '-background', 'white', '-flatten', input_name, 'PNG32:%s' % dest_file]
        if options.verbose:
            print('running %s' % ' '.join(args))
        subprocess.run(args, check=True)
 
    diff_dest = os.path.join(temp_dir, 'diff.png')

    # MAE = Mean Absolute Error, a metric where smaller means more
    # similar.  We avoid per-pixel metrics (eg. AE), as they make
    # larger/higher-res images have a bigger weight.
    args = ['compare', '-verbose', '-metric', 'mae', '-depth', '8',
            '-dissimilarity-threshold', '1', os.path.join(temp_dir, 'crop1.png'),
            os.path.join(temp_dir, 'crop2.png'), diff_dest]
    if options.verbose:
        print('running %s' % ' '.join(args))
    proc = subprocess.Popen(args,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE)
    out, err = proc.communicate()
    err_str = err.decode('utf-8')
    if not os.path.exists(diff_dest):
        out_str = out.decode('utf-8')
        print('missing path %s' % diff_dest)
        print(err_str)
        print(out_str)
        raise SystemExit

    dist: float = -1.0
    for line in err_str.split('\n'):
        m = re.search(r'all: [0-9.e-]+ \(([0-9.e-]+)\)', line)
        if m:
            dist = float(m.group(1)) * 100.0
            break
    else:
        print('missing distance marker: ')
        print(err_str)
        raise SystemExit
        
    system('convert -depth 8 %(temp_dir)s/diff.png -blur 0x3 -negate -channel alpha,blue -type TrueColorMatte -fx intensity %(temp_dir)s/matte.png' % locals())

    dest = os.path.join(dest_dir, new.replace('.png', '.compare.jpeg'))
    system('composite -compose atop -quality 65 %(temp_dir)s/matte.png %(temp_dir)s/crop2.png %(dest)s' % locals())
        
    return dist

################################################################
# interval/bbox arithmetic.

empty_interval = (INFTY, -INFTY)
empty_bbox = (empty_interval, empty_interval)


def interval_is_empty(i: Interval):
    return i[0] > i[1]


def interval_length(i: Interval):
    return max(i[1]-i[0], 0)


def interval_union(i1: Interval, i2: Interval) -> Interval:
    return (min(i1[0], i2[0]),
            max(i1[1], i2[1]))


def interval_intersect(i1: Interval, i2: Interval) -> Interval:
    return (max(i1[0], i2[0]),
            min(i1[1], i2[1]))


def bbox_is_empty(b) -> bool:
    return (interval_is_empty(b[0])
            or interval_is_empty(b[1]))


def bbox_union(b1: Bbox, b2: Bbox) -> Bbox:
    return (interval_union(b1[X_AXIS], b2[X_AXIS]),
            interval_union(b1[Y_AXIS], b2[Y_AXIS]))


def bbox_intersection(b1: Bbox, b2: Bbox) -> Bbox:
    return (interval_intersect(b1[X_AXIS], b2[X_AXIS]),
            interval_intersect(b1[Y_AXIS], b2[Y_AXIS]))


def bbox_area(b: Bbox) -> float:
    return interval_length(b[X_AXIS]) * interval_length(b[Y_AXIS])


def bbox_diameter(b: Bbox) -> float:
    return max(interval_length(b[X_AXIS]),
               interval_length(b[Y_AXIS]))


def difference_area(a: Bbox, b: Bbox) -> float:
    return bbox_area(a) - bbox_area(bbox_intersection(a, b))


class GrobSignature:
    """A (grob-name, bbox) tuple"""

    def __init__(self, exp_list):
        (self.name, bbox_x, bbox_y) = tuple(exp_list)

        self.bbox = (bbox_x, bbox_y)
        self.centroid = ((bbox_x[0] + bbox_x[1])/2.0,
                         (bbox_y[0] + bbox_y[1])/2.0)

    def __repr__(self):
        return '%s: (%.2f,%.2f), (%.2f,%.2f)\n' % (self.name,
                                                   self.bbox[0][0],
                                                   self.bbox[0][1],
                                                   self.bbox[1][0],
                                                   self.bbox[1][1])

    def centroid_distance(self, other, scale: float) -> float:
        return max_distance(self.centroid, other.centroid) / scale

    def bbox_distance(self, other) -> float:
        divisor = bbox_area(self.bbox) + bbox_area(other.bbox)

        if divisor:
            return (difference_area(self.bbox, other.bbox) +
                    difference_area(other.bbox, self.bbox)) / divisor
        else:
            return 0.0


class SystemSignature:
    """Signature for a single System.

    Abstracts away from the precise appearance to a list of grob-type => list<GrobSignature>.
    """

    def __init__(self, grob_sigs: List[GrobSignature]):
        d: Dict[str, List[GrobSignature]] = {}
        for g in grob_sigs:
            val = d.setdefault(g.name, [])
            val += [g]

        self.grob_dict = d
        self.bbox = empty_bbox
        for g in grob_sigs:
            self.bbox = bbox_union(g.bbox, self.bbox)

    def closest(self, grob_name: str, centroid: Vec) -> Optional[GrobSignature]:
        min_d = INFTY
        min_g = None
        try:
            grobs = self.grob_dict[grob_name]

            for g in grobs:
                d = max_distance(g.centroid, centroid)
                if d < min_d:
                    min_d = d
                    min_g = g

            return min_g
        except KeyError:
            return None

    def grobs(self):
        return functools.reduce(lambda x, y: x+y, list(self.grob_dict.values()), [])


class SystemLink:
    """Compares two Systems through their SystemSignatures."""

    def __init__(self, system1: SystemSignature, system2: SystemSignature):
        """Init from two SystemSignature instances"""

        self.system1 = system1
        self.system2 = system2

        self.link_list_dict: Dict[Optional[GrobSignature],
                                  List[GrobSignature]] = {}

        # pairs
        self.orphans = []

        # pair -> distance
        self.geo_distances: Dict[Tuple[GrobSignature,
                                       GrobSignature], float] = {}

        self._geometric_distance: Optional[float] = None
        self._expression_change_count = None

        # maps GrobSignature in system1 to its hopefully existing twin in system2.
        self.back_link_dict = {}
        if self.system1 and self.system2:
            # This is quadratic, because closest() has no geometric
            # structure to speed up searching. For small snippets,
            # this is acceptable.
            for g in system1.grobs():

                # skip empty bboxes.
                if bbox_is_empty(g.bbox):
                    continue

                closest = system2.closest(g.name, g.centroid)

                self.link_list_dict.setdefault(closest, [])
                self.link_list_dict[closest].append(g)
                if closest is not None:
                    self.back_link_dict[g] = closest
                else:
                    self.orphans.append((g, None))

            # find grobs in system2 but not in system1
            for g in system2.grobs():

                if bbox_is_empty(g.bbox):
                    continue

                if g not in self.link_list_dict:
                    closest = system1.closest(g.name, g.centroid)
                    if closest is not None:
                        self.link_list_dict[g] = [closest]
                        self.back_link_dict[closest] = g
                    else:
                        self.orphans.append((None, g))

    def calc_geometric_distance(self):
        if self.system1 and self.system2:
            total = 0.0
        else:
            total = 100.0 * (self.system1 != self.system2)

        for (g1, g2) in list(self.back_link_dict.items()):
            d = g1.bbox_distance(g2)
            if d:
                self.geo_distances[(g1, g2)] = d

            total += d

        self._geometric_distance = total

    def geo_details_string(self) -> str:
        results = [(d, g1, g2)
                   for ((g1, g2), d) in list(self.geo_distances.items())]
        # Only compare distances.
        results.sort(key=lambda x: -x[0])

        return ', '.join(['%s: %f' % (g1.name, d) for (d, g1, g2) in results])

    def orphan_details_string(self) -> str:
        return ', '.join(['%s' % g1.name for (g1, g2) in self.orphans if g2 is None])

    def geometric_distance(self) -> float:
        if self._geometric_distance is None:
            self.calc_geometric_distance()
        assert self._geometric_distance is not None
        return self._geometric_distance

    def orphan_count(self) -> int:
        return len(self.orphans)

    def distance_tuple(self) -> Tuple[int, float]:
        return (self.orphan_count(),
                self.geometric_distance())


def scheme_float(s: str) -> float:
    if ('nan' in s) or ('inf' in s):
        s = s.split('.')[0]
    return float(s)


def read_signature_file(name: str):
    """Returns SystemSignature or None if file doesn't exist."""
    log_verbose('reading %s' % name)

    try:
        f = open(name, encoding='utf-8')
    except IOError as e:
        if e.errno == errno.ENOENT:
            return None
        else:
            raise

    entries = f.read().split('\n')

    def string_to_tup(s):
        return tuple(map(scheme_float, s.split(' ')))

    def string_to_entry(s):
        fields = s.split('@')

        # Backward compatibility; remove this once we stop comparing
        # againsts older versions
        if len(fields) == 5:
            fields = (fields[0], string_to_tup(
                fields[2]), string_to_tup(fields[3]))
        else:
            fields[1] = string_to_tup(fields[1])
            fields[2] = string_to_tup(fields[2])

        return tuple(fields)

    entries = [string_to_entry(e) for e in entries
               if e and not e.startswith('#')]

    grob_sigs = [GrobSignature(e) for e in entries]
    sig = SystemSignature(grob_sigs)
    return sig


################################################################
# different systems of a .ly file.

hash_to_original_name: Dict[str, str] = {}


class FileLink:
    """Base class of files that should be compared."""

    def __init__(self, f1: str, f2: str):
        self._distance = None
        self.file_names = (f1, f2)

    def text_record_string(self) -> str:
        return '%-30f %-20s\n' % (self.distance(),
                                  self.name()
                                  + os.path.splitext(self.file_names[1])[1]
                                  )

    def calc_distance(self):
        return 0.0

    def distance(self) -> float:
        if self._distance is None:
            self._distance = self.calc_distance()

        assert self._distance is not None
        return self._distance

    def source_file(self) -> str:
        """Returns the corresponding .ly file."""
        for ext in ('.ly', '.ly.txt'):
            base = os.path.splitext(self.file_names[1])[0]
            f = base + ext
            if os.path.exists(f):
                return f

        return ''

    def directories(self) -> List[str]:
        """Directories of the two files"""
        return [os.path.dirname(f) for f in self.file_names]

    def name(self) -> str:
        """Returns the \\sourcefilename for this test file"""
        base = os.path.basename(self.file_names[1])
        base = os.path.splitext(base)[0]
        base = hash_to_original_name.get(base, base)
        base = os.path.splitext(base)[0]
        return os.path.join(self.prefix(), base)

    def prefix(self) -> str:
        return os.path.commonpath(self.file_names)

    def extension(self) -> str:
        return os.path.splitext(self.file_names[1])[1]

    def link_files_for_html(self, dest_dir):
        for f in self.file_names:
            link_file(f, os.path.join(dest_dir, f))

    def get_distance_details(self, dest_file) -> str:
        return ''

    def get_cell(self, oldnew: int) -> Tuple[str, str]:
        return '', ''

    def get_file(self, oldnew: int) -> str:
        return self.file_names[oldnew]

    def html_record_string(self, dest_dir: str) -> str:
        dist = self.distance()

        details = self.get_distance_details(self.file_names[1])
        if details:
            details_base = os.path.splitext(self.file_names[1])[0]
            details_base += '.details.html'
            fn = dest_dir + '/' + details_base
            open_write_file(fn).write(details)

            details = '<br>(<a href="%(details_base)s">details</a>)' % locals()

        name = self.name() + self.extension()

        cells = ['', '']
        for oldnew in (0, 1):
            file = self.get_file(oldnew)
            class_attr, cell = self.get_cell(oldnew)
            if class_attr:
                class_attr = ' class="%s"' % class_attr
            if cell or os.path.exists(file):
                cells[oldnew] = '''<figure%(class_attr)s>
%(cell)s
<figcaption><a href="%(file)s">%(name)s</a></figcaption>
</figure>''' % locals()

        cell1 = cells[0]
        cell2 = cells[1]

        return '''<tr>
<td>
%(dist)f
%(details)s
</td>
<td>%(cell1)s</td>
<td>%(cell2)s</td>
</tr>''' % locals()


class FileCompareLink (FileLink):
    def __init__(self, f1: str, f2: str):
        FileLink.__init__(self, f1, f2)
        self.contents = (self.get_content(self.file_names[0]),
                         self.get_content(self.file_names[1]))

    def calc_distance(self):
        if self.contents[0] == self.contents[1]:
            return 0.0
        else:
            return 100.0

    def get_content(self, name: str) -> Optional[str]:
        log_verbose('reading %s' % name)
        try:
            return open(name, 'r', encoding='utf-8').read()
        except IOError as e:
            if e.errno == errno.ENOENT:
                return None
            else:
                raise


class GitFileCompareLink (FileCompareLink):
    def get_cell(self, oldnew: int):
        str = self.contents[oldnew]

        # truncate long lines
        if str:
            str = '\n'.join([l[:80] for l in str.split('\n')])

        if str:
            str = '<pre>%s</pre>' % html.escape(str)

        if not str:
            str = ''
        return '', str

    def calc_distance(self):
        if self.contents[0] == self.contents[1]:
            d = 0.0
        else:
            d = 1.0001 * options.threshold

        return d


class TextFileCompareLink (FileCompareLink):
    snippet_fn_re = re.compile(r"`\./([0-9a-f]{2}/lily-[0-9a-f]{8}).eps'")

    def calc_distance(self):
        if self.contents[0] == self.contents[1]:
            return 0

        if (self.contents[0] is None) != (self.contents[1] is None):
            # Just one side available.  Don't show a diff.  If the user
            # wants to see the content, they can click through the link.
            self.diff_lines = []
            return 100

        # Extract the old and the new hashed snippet names from the log file
        # and replace the old by the new, so file name changes don't show
        # up as log differences...
        cont0 = self.contents[0].strip()
        cont1 = self.contents[1].strip()
        m0 = re.search(TextFileCompareLink.snippet_fn_re, cont0)
        m1 = re.search(TextFileCompareLink.snippet_fn_re, cont1)
        if (m0 and m1 and (m0.group(1) != m1.group(1))):
            cont0 = cont0.replace(m0.group(1), m1.group(1))

        diff = difflib.unified_diff(cont0.split('\n'),
                                    cont1.split('\n'),
                                    fromfiledate=self.file_names[0],
                                    tofiledate=self.file_names[1]
                                    )

        self.diff_lines = [l for l in diff]
        self.diff_lines = self.diff_lines[2:]

        return math.sqrt(float(len([l for l in self.diff_lines if l[0] in '-+'])))

    def get_cell(self, oldnew: int) -> Tuple[str, str]:
        str = ''
        if oldnew == 1:
            str = '\n'.join([d.replace('\n', '') for d in self.diff_lines])
        if str:
            str = '<pre>%s</pre>' % html.escape(str)
        return '', str


class MidiFileLink (TextFileCompareLink):
    def get_content(self, name: str) -> Optional[str]:
        try:
            f = open(name, 'rb')
        except IOError as e:
            if e.errno == errno.ENOENT:
                return None
            else:
                raise

        data = f.read()
        midi_data = midi.parse(data)
        tracks = midi_data[1]

        str = ''
        j = 0
        for track in tracks:
            str += 'track %d' % j
            j += 1

            for ev in track:
                ev_str = repr(ev)
                if re.search('LilyPond [0-9.]+', ev_str):
                    continue

                str += '  ev %s\n' % repr(ev)
        return str


def eps_to_png(fn: str,  dest_dir: str) -> str:
    # EPS files generated for regression tests don't contain fonts
    # to save disk space.  Instead, paths to the fonts are stored in
    # the files that are loaded by Ghostscript's `.loadfont'
    # operator later on.
    #
    # In gub builds, these paths get massaged to be relative to the
    # location of the particular EPS files.  Since gs doesn't
    # provide an option to adjust the font lookup paths for
    # `.loadfont', we enter the directory so that the relative paths
    # are valid.
    (dir, base) = os.path.split(fn)

    out_dir = os.path.abspath(dest_dir + '/' + dir)
    mkdir(out_dir)

    data_option = ''
    if options.local_data_dir:
        data_option = ('-slilypond-datadir=%s/share/lilypond/current'
                       % os.path.abspath(dir))

    driver = open(os.path.join(dir, 'batch.ps'), 'w', encoding='utf-8')
    outfile = os.path.join(out_dir, base).replace('.eps', '.png')
    driver.write('''
        mark /OutputFile (%s)
        /GraphicsAlphaBits 4 /TextAlphaBits 4
        /HWResolution [101 101]
        (png16m) finddevice putdeviceprops setdevice
        (%s) run
        ''' % (outfile, base))
    driver.close()
    cmd = ('gs '
           + data_option
           + ' -dNOSAFER'
           ' -dEPSCrop'
           ' -q'
           ' -dNOPAUSE'
           ' -dNODISPLAY'
           ' -dAutoRotatePages=/None'
           ' -dPrinted=false'
           ' batch.ps'
           ' -c quit')
    system(cmd, cwd=dir)
    return outfile


class SignatureFileLink (FileLink):
    def __init__(self, f1, f2):
        FileLink.__init__(self, f1, f2)
        self.system_links: Dict[int, SystemLink] = {}

    def add_system_link(self, link: SystemLink, number: int):
        self.system_links[number] = link

    def calc_distance(self):
        d = 0.0

        orphan_distance = 0.0
        for l in list(self.system_links.values()):
            d = max(d, l.geometric_distance())
            orphan_distance += l.orphan_count()

        return d + orphan_distance

    def add_file_compare(self, f1: str, f2: str):
        system_index = []

        def note_system_index(m):
            system_index.append(int(m.group(1)))
            return ''

        base1 = re.sub("-([0-9]+).signature", note_system_index, f1)
        base2 = re.sub("-([0-9]+).signature", note_system_index, f2)

        self.base_names = (os.path.normpath(base1),
                           os.path.normpath(base2))

        s1 = read_signature_file(f1)
        s2 = read_signature_file(f2)

        link = SystemLink(s1, s2)

        self.add_system_link(link, system_index[0])

    def create_images(self, dest_dir: str) -> Tuple[Optional[str], Optional[str]]:
        """Returns a (OLD-FILES, NEW-FILES) tuple."""

        outputs = [None, None]
        for oldnew in (0, 1):
            fn = self.base_names[oldnew] + '.eps'
            if os.path.exists(fn):
                outputs[oldnew] = eps_to_png(fn, dest_dir)

        return (outputs[0], outputs[1])

    def link_files_for_html(self, dest_dir: str):
        FileLink.link_files_for_html(self, dest_dir)
        old, new = self.create_images(dest_dir)
        if old and new:
            compare_png_images(old, new, dest_dir)

    def get_cell(self, oldnew):
        def empty_cell():
            return '', ''

        def static_img_cell(img):
            return '', ('''
<div><a href="%(img)s"><img src="%(img)s" alt=""/></a></div>
''' % locals())

        def reactive_img_cell(oldimg, newimg):
            return 'reactive_img', ('''
<div style="background-image: url(\'%(oldimg)s\')"><a href="%(newimg)s"><img src="%(newimg)s" alt=""/></a></div>
''' % locals())

        # If we have systems, we expect that images have been or will
        # be created.
        num_systems = (sum(1 for x in list(self.system_links.values()) if x.system1),
                       sum(1 for x in list(self.system_links.values()) if x.system2))
        expect_compare = num_systems[0] and oldnew

        base = os.path.splitext(self.file_names[oldnew])[0]

        if expect_compare:
            ext = '.compare.jpeg'
        else:
            ext = '.png'

        img = base + ext
        if expect_compare:
            oldimg = os.path.splitext(self.file_names[0])[0] + '.png'
            return reactive_img_cell(oldimg, img)
        elif num_systems[oldnew]:
            return static_img_cell(img)
        else:
            return empty_cell()

    def get_distance_details(self, dest_file: str) -> str:
        systems = sorted(self.system_links.items())

        rel_top = os.path.relpath(os.path.curdir, os.path.dirname(dest_file))
        style_href = os.path.join(rel_top, 'style.css')

        html = ""
        for (c, link) in systems:
            e = '<td>%d</td>' % c
            for d in link.distance_tuple():
                e += '<td>%f</td>' % d

            e = '<tr>%s</tr>' % e

            html += e

            e = '<td>%d</td>' % c
            for s in (link.orphan_details_string(),
                      link.geo_details_string()):
                e += "<td>%s</td>" % s

            e = '<tr>%s</tr>' % e
            html += e

        original = self.name()
        html = '''<!DOCTYPE html>
<html lang="en">
<head>
<title>comparison details for %(original)s</title>
<link rel="stylesheet" type="text/css" href="%(style_href)s"/>
<meta charset="UTF-8">
</head>
<body>
<table>
<tr>
<th>system</th>
<th>orphan</th>
<th>geo</th>
</tr>

%(html)s
</table>

</body>
</html>
''' % locals()
        return html


################################################################
# Files/directories

def compare_signature_files(f1: str, f2: str) -> Tuple[int, float]:
    s1 = read_signature_file(f1)
    s2 = read_signature_file(f2)

    return SystemLink(s1, s2).distance_tuple()


def paired_files(dir1: str, dir2: str, pattern: str) -> Tuple[List[str], List[str], List[str]]:
    """
    Search DIR1 and DIR2 for PATTERN.

    Return (PAIRED, MISSING-FROM-DIR1, MISSING-FROM-DIR2)
    """

    files = []
    for d in (dir1, dir2):
        foundlist = [os.path.split(f)[1] for f in glob.glob(d + '/' + pattern)]
        found = dict((f, 1) for f in foundlist)
        files.append(found)

    pairs = []
    missing = []
    for f in files[0]:
        try:
            files[1].pop(f)
            pairs.append(f)
        except KeyError:
            missing.append(f)

    return (pairs, list(files[1].keys()), missing)


class ComparisonData:
    """All the comparison data; may span several directories"""

    def __init__(self):
        self.result_dict = {}
        self.missing = []
        self.added = []
        self.file_links = {}

    def read_sources(self):

        # ugh: drop the .ly.txt
        for (key, val) in list(self.file_links.items()):

            def note_original(match, ln=val):
                key = ln.name()
                hash_to_original_name[key] = match.group(1)
                return ''

            sf = val.source_file()
            if sf:
                re.sub(r'\\sourcefilename "([^"]+)"',
                       note_original, open(sf, 'r', encoding='utf-8').read())
            else:
                print('no source for', val.file_names[1])

    def compare_trees(self, dir1: str, dir2: str):
        self.compare_directories(dir1, dir2)

        dir1_ok = False
        try:
            (root1, dirs1, files1) = next(os.walk(dir1))
            dir1_ok = True
        except StopIteration:
            dirs1 = []
            pass

        dir2_ok = False
        try:
            (root2, dirs2, files2) = next(os.walk(dir2))
            dir2_ok = True
        except StopIteration:
            dirs2 = []
            pass

        if not (dir1_ok or dir2_ok):
            sys.stderr.write(
                'Failed to walk through %s and %s; please check that they exist.\n' % (dir1, dir2))
            sys.exit(1)

        for d in sorted(set(dirs1 + dirs2)):
            # don't walk the share folders
            if d.startswith("share"):
                continue

            d1 = os.path.join(dir1, d)
            d2 = os.path.join(dir2, d)

            if os.path.islink(d1) or os.path.islink(d2):
                continue

            self.compare_trees(d1, d2)

    def compare_directories(self, dir1: str, dir2: str):
        log_terse('comparing %s' % dir1)
        log_terse('       to %s' % dir2)

        total_compared = 0
        for ext in ['signature',
                    'midi',
                    'log',
                    'gittxt']:
            (paired, missing1, missing2) = paired_files(dir1, dir2, '*.' + ext)

            self.missing += [(dir2, m) for m in missing2]
            self.added += [(dir2, m) for m in missing1]

            # we sort the file names for easier debugging
            to_compare = sorted(paired + missing1 + missing2)
            if to_compare:
                total_compared += len(to_compare)
                log_terse('%6d %s' % (len(to_compare), ext))

            for p in to_compare:
                if (options.max_count
                        and len(self.file_links) > options.max_count):
                    continue

                f2 = dir2 + '/' + p
                f1 = dir1 + '/' + p
                self.compare_files(f1, f2)

        log_terse('%6d total' % total_compared)

    def compare_files(self, f1: str, f2: str):
        if f1.endswith('signature'):
            self.compare_signature_files(f1, f2)
        else:
            ext = os.path.splitext(f1)[1]
            klasses = {
                '.midi': MidiFileLink,
                '.log': TextFileCompareLink,
                '.gittxt': GitFileCompareLink,
            }

            if ext in klasses:
                self.compare_general_files(klasses[ext], f1, f2)

    def compare_general_files(self, klass, f1: str, f2: str):
        prefix = os.path.commonprefix([f1, f2])
        name = os.path.split(f1)[1]
        name = os.path.join(prefix, name)

        file_link = klass(f1, f2)
        self.file_links[name] = file_link

    def compare_signature_files(self, f1: str, f2: str):
        prefix = os.path.commonprefix([f1, f2])
        name = os.path.split(f1)[1]
        name = re.sub('-[0-9]+.signature', '', name)
        name = os.path.join(prefix, name)

        file_link = None
        try:
            file_link = self.file_links[name]
        except KeyError:
            generic_f1 = re.sub('-[0-9]+.signature', '.ly', f1)
            generic_f2 = re.sub('-[0-9]+.signature', '.ly', f2)
            file_link = SignatureFileLink(generic_f1, generic_f2)
            self.file_links[name] = file_link

        file_link.add_file_compare(f1, f2)

    def write_changed(self, dest_dir: str, threshold: float):
        (changed, below, unchanged) = self.thresholded_results(threshold)

        non_ext = [os.path.splitext(link.file_names[1])[0] for link in changed]
        str = '\n'.join(sorted(set(non_ext)))
        if str:
            str += '\n'
        fn = dest_dir + '/changed.txt'

        open_write_file(fn).write(str)

    def thresholded_results(self, threshold: float):
        # todo: support more scores.
        results = [(link.distance(), link)
                   for link in list(self.file_links.values())]
        # Only compare distances.
        results.sort(key=lambda x: -x[0])

        unchanged = [r for (d, r) in results if d == 0.0]
        below = [r for (d, r) in results if threshold >= d > 0.0]
        changed = [r for (d, r) in results if d > threshold]
        assert len(results) == len(unchanged) + len(below) + len(changed)

        return (changed, below, unchanged)

    def write_text_result_page(self, filename: str, threshold: float):
        verbose = True
        out = sys.stdout
        if filename == '':
            verbose = options.verbose
        else:
            out = open_write_file(filename)

        (changed, below, unchanged) = self.thresholded_results(threshold)

        if verbose:
            for link in changed:
                out.write(link.text_record_string())
            for m in self.missing:
                out.write('in baseline only: %s\n' % m[1])
            out.write('\n\n')
        else:
            out.write('output-distance summary:\n')
            out.write('%6d changed\n' % len(changed))

        out.write('%6d in baseline only\n' % len(self.missing))
        out.write('%6d below threshold\n' % len(below))
        out.write('%6d unchanged\n' % len(unchanged))

    def create_text_result_page(self, dest_dir: str, threshold: float):
        self.write_text_result_page(dest_dir + '/index.txt', threshold)

    def create_html_result_page(self, dest_dir: str, threshold: float):
        (changed, below, unchanged) = self.thresholded_results(threshold)

        table_rows = '''
<tr>
<th>distance</th>
<th>before</th>
<th>after</th>
</tr>
'''

        for link in changed:
            table_rows += link.html_record_string(dest_dir)

        def make_row(label, value):
            return '<tr><td>%d</td><td>%s</td></tr>' % (value, label)

        def make_nz_row(label, value):
            if value:
                return make_row(label, value)
            else:
                return ''

        summary = '<table id="summary">'
        summary += make_nz_row('in baseline only', len(self.missing))
        summary += make_nz_row('newly added', len(self.added))
        summary += make_nz_row('below threshold', len(below))
        summary += make_row('unchanged', len(unchanged))
        summary += '</table>'

        me = sys.argv[0]

        open_write_file(dest_dir + '/style.css').write('''
:root {
    background-color: white;
    color: black;

    --line-color: blue;
    --link-color: blue;
}

hr, table, tr, th, td {
    border: 1px solid var(--line-color);
}

a {
    color: var(--link-color);
}

figcaption {
    margin-top: 0.5rem;
}

figcaption button {
    float: right;
}

figure {
    display: inline-block;
    margin: 0rem;
    padding: 0rem;
}

figure > div:first-child {
    background-color: white;
    background-repeat: no-repeat;
    border: 0.5rem solid white;
    border-radius: 0.5rem;
    color: black;
}

figure.reactive_img.active > div:first-child img {
    opacity: 0;
}

figure img {
    border: none;
}

table {
    border-collapse: collapse;
    margin: 1rem 0.25rem;
}

#summary td:first-child {
    text-align: right;
}

td, th {
    padding: 0.5rem;
}

td {
    vertical-align: top;
}

table.ruled_rows td,
table.ruled_rows th {
    border-style: solid hidden;
}

td:empty {
    background-image: repeating-linear-gradient(-45deg, rgba(127,127,0,.1), rgba(127,127,0,.1) 3rem, rgba(255,255,0,.2) 3rem, rgba(255,255,0,.2) 6rem);
}

@media (prefers-color-scheme: dark) {
    :root {
        background-color: #1c1c1c;
        color: #ffffff;

        --line-color: #838383;
        --link-color: #59a0e0;
    }
}
''')

        html = '''<!DOCTYPE html>
<html lang="en">
<head>
<title>LilyPond regression test results</title>
<link rel="stylesheet" type="text/css" href="style.css"/>
<meta charset="UTF-8">
<meta name="author" content="This file was autogenerated by %(me)s"/>
<script>
// <![CDATA[
    function showOnlyMatchingRows(substring) {
        var table = document.getElementById("test_cases");
        for (row of table.getElementsByTagName("tr")) {
            html = row.innerHTML;
            row.style.display =
                ((html.indexOf('>distance<') != -1) ||
                 (html.indexOf(substring + '">') != -1)) ? "" : "none";
        }
    }

    function addControls() {
        function makeMomentaryButton(label, object) {
            function activate() { object.classList.add("active"); }
            function revert() { object.classList.remove("active"); }

            var button = document.createElement("button");
            button.appendChild(document.createTextNode(label));
            button.addEventListener("mousedown", activate);
            button.addEventListener("mouseup", revert);
            button.addEventListener("mouseout", revert);
            return button;
        }

        for (fig of document.getElementsByClassName("reactive_img")) {
            var caps = fig.getElementsByTagName("figcaption");
            if (caps.length > 0) {
                caps[0].appendChild(makeMomentaryButton("Flip", fig));
            }
        }
    }
// ]]>
</script>
</head>
<body onload="addControls()">
<p>
  click to filter rows by type:
  <a href="#" onClick="showOnlyMatchingRows('.ly')">ly</a> /
  <a href="#" onClick="showOnlyMatchingRows('.signature')">signature</a> /
  <a href="#" onClick="showOnlyMatchingRows('.midi')">midi</a> /
  <a href="#" onClick="showOnlyMatchingRows('.log')">log</a> /
  <a href="#" onClick="showOnlyMatchingRows('.gittxt')">gittxt</a> /
  <a href="#" onClick="showOnlyMatchingRows('')">reset to all</a>
</p>

<hr />

%(summary)s

<table id="test_cases" class="ruled_rows">
%(table_rows)s
</table>
</body>
</html>''' % locals()

        dest_file = dest_dir + '/index.html'
        open_write_file(dest_file).write(html)

        for link in changed:
            link.link_files_for_html(dest_dir)

    def print_results(self, threshold):
        self.write_text_result_page('', threshold)


def compare_tree_pairs(tree_pairs, dest_dir: str, threshold: float):
    """Compare a list of directories."""
    shutil.rmtree(dest_dir, ignore_errors=True)

    data = ComparisonData()
    for dir1, dir2 in tree_pairs:
        data.compare_trees(dir1, dir2)

    data.read_sources()

    data.write_changed(dest_dir, threshold)
    data.create_html_result_page(dest_dir, threshold)
    data.create_text_result_page(dest_dir, threshold)
    data.print_results(threshold)


################################################################
# TESTING

def mkdir(x):
    if not os.path.isdir(x):
        log_verbose('mkdir %s' % x)
        os.makedirs(x)


def link_file(x, y):
    mkdir(os.path.split(y)[0])
    try:
        log_verbose('%s -> %s' % (x, y))
        os.link(x, y)
    except OSError as z:
        if z.errno == errno.ENOENT:
            pass
        else:
            print('OSError', x, y, z)
            raise


def open_write_file(x):
    log_verbose('writing %s' % x)
    d = os.path.split(x)[0]
    mkdir(d)
    return open(x, 'w', encoding='utf-8')


def test_paired_files():
    print(paired_files(os.environ["HOME"] + "/src/lilypond/scripts/",
                       os.environ["HOME"] + "/src/lilypond-stable/scripts/build/", '*.py'))


def test_compare_tree_pairs():
    system('rm -rf dir1 dir2')
    system('mkdir dir1 dir2')
    system('cp 19-1.signature 19.sub-1.signature')
    system('cp 19.ly 19.sub.ly')
    system('cp 19.log 19.sub.log')
    system('cp 19-1.eps 19.sub-1.eps')

    system('cp 20multipage* dir1')
    system('cp 20multipage* dir2')

    system('cp removed* dir1')
    system('cp added* dir2')

    system('mkdir -p dir1/subdir/ dir2/subdir/')
    system('cp 19.sub{-*.signature,.ly,-1.eps,.log} dir1/subdir/')
    system('cp 19.sub{-*.signature,.ly,-1.eps,.log} dir2/subdir/')

    # Make sure we have unicode text in the HTML
    system(u'echo HEAD is 人人的乐谱软件 > dir1/tree.gittxt')
    system('echo HEAD is 2 > dir2/tree.gittxt')

    # introduce differences
    system('cp 20-1.signature dir2/subdir/19.sub-1.signature')

    # radical diffs.
    system('cp 20grob{-*.signature,.ly,.eps,-?.eps,.log} dir1/')
    system('cp 19-1.signature dir2/20grob-1.signature')
    system('cp 19-1.signature dir2/20grob-2.signature')
    system('cp 19-1.eps dir2/20grob-1.eps')
    system('cp 19-1.eps dir2/20grob-2.eps')
    system('cp 19.ly dir2/20grob.ly')
    system('cp 19.eps dir2/20grob.eps')
    system('cp 19.log dir2/20grob.log')
    system('cp 20{.ly,.log} dir2/')
    system('cp 19multipage.midi dir1/midi-differ.midi')
    system('cp 20multipage.midi dir2/midi-differ.midi')
    system('cp 19multipage.log dir1/log-differ.log')
    system('cp 19multipage.log dir2/log-differ.log &&  echo different >> dir2/log-differ.log &&  echo different >> dir2/log-differ.log')

    compare_tree_pairs([('dir1', 'dir2')],
                       'compare-dir1dir2', options.threshold)

    for f in [
            "index.html",
            "index.txt",
            "changed.txt",
            "dir2/20grob.compare.jpeg",
            "dir2/20grob.png",
            "dir1/20grob.png",
            "style.css",
    ]:
        fn = os.path.join("compare-dir1dir2", f)
        assert os.path.exists(fn), fn
    html = open("compare-dir1dir2/index.html", encoding='utf-8').read()
    assert "removed.log" in html
    assert "added.log" in html


def test_compare_png_images():
    # Compare 2 images looking like "xx." and "x.". The second image
    # should be scaled up to match the first so we compare "xx." and
    # "x..", 
    open('p1.xpm', 'wb').write(rb'''/* XPM */
static char * XFACE[] = {
"3 1 2 1",
"a c #ffffff",
"b c #000000",
"bba",
};
''')
    open('p2.xpm', 'wb').write(rb'''/* XPM */
static char * XFACE[] = {
"2 1 2 1",
"a c #ffffff",
"b c #000000",
"ba",
};
''')
    system('convert p1.xpm p1.png')
    system('convert p2.xpm p2.png')
    dist = compare_png_images('p1.png', 'p2.png', './')

    # 1 pixel out of 3 differing = 33% error
    assert math.fabs(dist - 100.0/3) < 1e-4


def test_basic_compare():
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
\new Staff \relative c' {
  c4^"%(userstring)s" %(extragrob)s
  }
\new Staff \relative c' {
  c4^"%(userstring)s" %(extragrob)s
  }
>>
\layout{}
}

"""

    dicts = [{'papermod': '',
              'name': '20',
              'extragrob': '',
              'userstring': '20'},
             {'papermod': '#(set-global-staff-size 19.5)',
              'name': '19',
              'extragrob': '',
              'userstring': '191919'},
             {'papermod': '',
              'name': '20grob',
              'extragrob': 'r2. \\break c1',
              'userstring': 'test'},
             ]

    for d in dicts:
        open(d['name'] + '.ly', 'w', encoding='utf-8').write(ly_template % d)

    simple_names = [d['name'] for d in dicts]

    multipage_str = r'''
    #(set-default-paper-size "a6")
    \book {
\score {
      \relative c' { c1 \pageBreak c1 }
      \layout {}
      \midi {}
    }
    \paper {}
    }
    '''

    open('20multipage.ly', 'w',
         encoding='utf-8').write(multipage_str.replace('c1', 'd1'))
    open('19multipage.ly', 'w', encoding='utf-8').write(
        '#(set-global-staff-size 19.5)\n' + multipage_str)

    open('added.ly', 'w', encoding='utf-8').write(
        r"""
#(define default-toplevel-book-handler
  print-book-with-defaults-as-systems )

#(ly:set-option (quote no-point-and-click))

\sourcefilename "added.ly"
{ a'4 }
""")
    open('removed.ly', 'w', encoding='utf-8').write(
        r"""
#(define default-toplevel-book-handler
  print-book-with-defaults-as-systems )

#(ly:set-option (quote no-point-and-click))

\sourcefilename "removed.ly"
{ e'4 }
""")

    names = simple_names + ["20multipage",
                            "19multipage"] + ['added', 'removed']
    binary = os.environ.get("LILYPOND_BINARY", "lilypond")
    system('%s -dseparate-page-formats=ps -daux-files -dtall-page-formats=ps --formats=ps -dseparate-log-files -dinclude-eps-fonts -dgs-load-fonts --header=texidoc -dcheck-internal-types -ddump-signatures -danti-alias-factor=1 %s' % (binary, ' '.join(names)))
    test_compare_signatures(simple_names)


def test_compare_signatures(names, timing=False):
    times = 1
    if timing:
        times = 100

    t0 = time.time()

    count = 0
    for t in range(0, times):
        sigs = dict((n, read_signature_file('%s-1.signature' % n))
                    for n in names)
        count += 1

    if timing:
        print('elapsed', (time.clock() - t0)/count)

    t0 = time.time()
    count = 0
    combinations = {}
    links = {}
    for (n1, s1) in list(sigs.items()):
        for (n2, s2) in list(sigs.items()):
            key = '%s-%s' % (n1, n2)
            link = SystemLink(s1, s2)
            links[key] = link
            combinations[key] = link.distance_tuple()
            count += 1

    if timing:
        print('elapsed', (time.clock() - t0)/count)

    results = sorted(combinations.items())

    if options.verbose:
        for k, v in results:
            print('%-20s' % k, v)

    oc_forward = links["20grob-20"].orphan_count()
    oc_reverse = links["20-20grob"].orphan_count()
    assert oc_forward == oc_reverse
    assert oc_forward > 0

    assert combinations['20-20'] == (0.0, 0.0)
    assert combinations['20-19'][1] < 10.0
    assert combinations['20-19'][1] > 0.0
    assert combinations['20grob-20'][0] > 0


def run_tests():
    testdir = os.path.join(options.output_dir, 'test-output-distance')
    print('test results in ', testdir)

    shutil.rmtree(testdir, ignore_errors=True)
    system('mkdir ' + testdir)
    os.chdir(testdir)

    test_compare_png_images()
    test_basic_compare()
    test_compare_tree_pairs()
    shutil.rmtree(testdir, ignore_errors=True)


def main():
    p = optparse.OptionParser(
        "output-distance - compare LilyPond formatting runs")
    p.usage = 'output-distance.py [options] tree1 tree2 [tree3 tree4]...'

    p.add_option('--max-count',
                 dest="max_count",
                 metavar="COUNT",
                 type="int",
                 default=0,
                 action="store",
                 help='only analyze COUNT signature pairs')

    p.add_option('--local-datadir',
                 dest="local_data_dir",
                 default=False,
                 action="store_true",
                 help='whether to use the share/lilypond/ directory in the test directory')

    p.add_option('-o', '--output-dir',
                 dest="output_dir",
                 default=None,
                 action="store",
                 type="string",
                 help='where to put the test results [tree2/compare-tree1tree2]')

    p.add_option('', '--test-self',
                 dest="run_test",
                 action="store_true",
                 help='run test method')

    p.add_option('', '--threshold',
                 dest="threshold",
                 default=0.3,
                 action="store",
                 type="float",
                 help='threshold for geometric distance')

    p.add_option('-v', '--verbose',
                 dest="verbose",
                 default=False,
                 action="store_true",
                 help='log progress verbosely')

    global options
    (options, args) = p.parse_args()

    if options.run_test:
        run_tests()
        return

    if len(args) % 2 == 1:
        p.print_usage()
        sys.exit(2)

    out = options.output_dir
    if not out:
        out = args[0].replace('/', '')
        out = os.path.join(args[1], 'compare-' + shorten_string(out))

    compare_tree_pairs(
        list(zip(args[0::2], args[1::2])), out, options.threshold)


if __name__ == '__main__':
    main()
    if temp_dir:
        shutil.rmtree(temp_dir)
