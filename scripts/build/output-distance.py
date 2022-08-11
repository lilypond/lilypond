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


import argparse
import difflib
import errno
import functools
import glob
import html
import math
import os
import re
import shutil
import subprocess
import sys
import tempfile
import time
from typing import Dict, List, Tuple, Optional, Any

Vec = Tuple[float, float]
Interval = Tuple[float, float]
Bbox = Tuple[Interval, Interval]

# so we can call directly as scripts/build/output-distance.py
me_path = os.path.abspath(os.path.split(sys.argv[0])[0])
sys.path.insert(0, me_path + '/../../python/')

# Keep our includes after adapting sys.path above.
import midi # type: ignore

def log_terse(s: str):
    if not options.verbose:
        print(s)

def log_verbose(s: str):
    if options.verbose:
        print(s)


################################################################
# system interface.


def system(c: str, cwd=None):
    if not cwd:
        cwd = os.getcwd()
    log_verbose('system %s (cwd=%s)' % (c, cwd))
    # explicitly use bash, so we don't get dash on Ubuntu.
    subprocess.run(["/bin/bash", "-c", c.encode('utf-8')], check=True, cwd=cwd)

def png_dims(fn: str) -> Tuple[int, int]:
    """Reads width/height from PNG file."""

    # avoid subprocessing, as subprocessing is relatively expensive,
    # and we run this for each image we handle
    header = read_file(fn, 32)
    w = int.from_bytes(header[16:20], 'big', signed=False)
    h = int.from_bytes(header[20:24], 'big', signed=False)
    return (w, h)


def read_file(fn: str, n: int =-1) -> bytes:
    with open(fn, 'rb') as f:
        return f.read(n)


def compare_png_images(old: str, new: str) -> float:
    file_dims = {}
    for fn in [old, new]:
        if os.path.exists(fn):
            file_dims[fn] = png_dims(fn)

    maxdims: Tuple = tuple(map(max, zip(*file_dims.values())))
    for input_name in [old, new]:
        if maxdims == file_dims[input_name]:
            continue

        # Uses PNG32:filename to generate cropped image in color (most
        # input images are grayscale, but we don't want a grayscale
        # cropped image). We need -flatten to extend the size beyond its
        # current size.
        resize_fn = input_name + '.resize.png'
        args = ['convert', '-crop', '%dx%d+0+0!' % maxdims,
                '-background', 'white', '-flatten', input_name, 'PNG32:%s' % resize_fn]
        if options.verbose:
            print('running %s' % ' '.join(args))
        subprocess.run(args, check=True)
        os.rename(resize_fn, input_name)
    diff_dest = new.replace('.png', '.diff.png')

    # MAE = Mean Absolute Error, a metric where smaller means more
    # similar.  We avoid per-pixel metrics (eg. AE), as they make
    # larger/higher-res images have a bigger weight.
    args = ['compare', '-verbose', '-metric', 'mae', '-depth', '8',
            '-dissimilarity-threshold', '1', old, new, diff_dest]
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
            dist = float(m.group(1)) * MAX_DISTANCE
            break
    else:
        print('missing distance marker: ')
        print(err_str)
        raise SystemExit

    if options.verbose:
        print('distance: %f' % dist)
    return dist

################################################################


MAX_DISTANCE = 100

hash_to_original_name: Dict[str, str] = {}


class FileLink:
    """Base class of files that should be compared."""

    def __init__(self, f1: str, f2: str):
        self._distance: Optional[float] = None
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

    def get_cell(self, oldnew: int) -> str:
        return ''

    def get_file(self, oldnew: int) -> str:
        return self.file_names[oldnew]

    def html_record_string(self) -> str:
        dist = self.distance()
        name = self.name() + self.extension()

        cells = ['', '']
        for oldnew in [0, 1]:
            file = self.get_file(oldnew)
            cell = self.get_cell(oldnew)
            if cell or os.path.exists(file):
                cells[oldnew] = '''<figure>
%(cell)s
<figcaption><a href="%(file)s">%(name)s</a></figcaption>
</figure>''' % locals()

        cell1 = cells[0]
        cell2 = cells[1]

        return '''<tr>
<td>
%(dist)f
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
            return MAX_DISTANCE

    def get_content(self, name: str) -> Optional[str]:
        log_verbose('reading %s' % name)
        try:
            return read_file(name).decode()
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
        return str

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
            return MAX_DISTANCE

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

    def get_cell(self, oldnew: int) -> str:
        str = ''
        if oldnew == 1:
            str = '\n'.join([d.replace('\n', '') for d in self.diff_lines])
        if str:
            str = '<pre>%s</pre>' % html.escape(str)
        return str


class ImageLink (FileLink):
    """Comparison of a pair of images (either PNG or EPS)"""

    def __init__(self, dest_dir: str, f1: str, f2: str):
        FileLink.__init__(self, f1, f2)
        self.dest_dir = dest_dir
        self.image_exists = tuple(os.path.exists(f) for f in self.file_names)
        self._file_data_equal = False

        if self.image_exists[0] and self.image_exists[1]:
            self._file_data_equal = read_file(self.file_names[0]) == read_file(self.file_names[1])

    def images_to_convert(self, only_changed=True) -> Tuple[List[str],List[str]]:
        images: Tuple[List[str], List[str]] = ([], [])
        for oldnew in [0, 1]:
            file_name  = self.file_names[oldnew]
            if not file_name.endswith('.eps'):
                continue

            if (self.image_exists[oldnew] and (not self._file_data_equal or
                                              # for unchanged images, we only render the new file.
                                            (oldnew==1 and not only_changed))):
                images[oldnew].append(file_name)
        return images

    def update_images(self, pngs: Dict[str, str]):
        updated = []
        for fn in self.file_names:
            updated.append(pngs.get(fn, fn))

        self.file_names = (updated[0], updated[1])

    def calc_distance(self) -> float:
        if self._file_data_equal:
            return 0
        if not self.image_exists[0] or not self.image_exists[1]:
            return MAX_DISTANCE

        dist = compare_png_images(self.file_names[0],
                                  self.file_names[1])
        return dist

    def get_cell_html(self, name: str, oldnew: int) -> str:
        base = os.path.splitext(self.file_names[oldnew])[0]

        if self.image_exists[0] and self.image_exists[1] and oldnew == 1 and self.distance() > 0:
            newimg = os.path.relpath(self.file_names[oldnew], self.dest_dir)
            oldimg = os.path.relpath(self.file_names[0], self.dest_dir)
            diffimg = os.path.splitext(newimg)[0] + '.diff.png'

            return '''
<figure class="reactive_img">
  <div>
    <div class="newimg"><img alt="new image" src="%(newimg)s" /></div>
    <div class="diffimg"><img alt="diff image" src="%(diffimg)s" /></div>
    <div class="oldimg"><img alt="old image" src="%(oldimg)s" /></div>
    <!-- add it invisibly without absolute position so the parent takes up space. -->
    <div style="opacity: 0.0"><img alt="diff image" src="%(diffimg)s" /></div>
  </div>
  <figcaption><span>&nbsp;</span></figcaption>
</figure>
''' % locals()
        else:
            if self.distance() == 0:
                # if the images are the same, we only have to render
                # one of the pair, in this case the new one.
                oldnew = 1

            if self.image_exists[oldnew]:
                img = os.path.relpath(self.file_names[oldnew], self.dest_dir)
                return ('''
<div><a href="%s"><img alt="image of music" src="%s" /></a></div>
''' % (img, img))

        return ''


class ImagesLink (FileLink):
    """Comparison of a sequence of images"""

    def __init__(self, f1: str, f2: str):
        FileLink.__init__(self, f1, f2)
        self.image_links: Dict[int, ImageLink] = {}

    def add_image_pair(self, dest_dir: str, f1: str, f2: str):
        system_index = []

        def note_system_index(m):
            system_index.append(int(m.group(1)))
            return ''

        self.base_names = [os.path.normpath(
            re.sub('-([0-9]+).eps', note_system_index, f)) for f in [f1, f2]]

        assert system_index[0] == system_index[1]
        self.image_links[system_index[0]] = ImageLink(dest_dir, f1, f2)

    def get_images_to_convert(self, only_changed=True) -> Tuple[List[str], List[str]]:
        result: Tuple[List[str], List[str]] = ([], [])
        for link in self.image_links.values():
            if only_changed or link.distance() == 0.0:
                images = link.images_to_convert(only_changed=only_changed)
                for oldnew in [0, 1]:
                    result[oldnew].extend(images[oldnew])
        return result

    def calc_distance(self) -> float:
        return max(l.distance() for l in self.image_links.values())

    def update_images(self, pngs: Dict[str, str]):
        for links in self.image_links.values():
            links.update_images(pngs)

    def get_cell(self, oldnew: int) -> str:
        htmls = []

        for (key, link) in sorted(self.image_links.items()):
            html = link.get_cell_html('image %d' % key, oldnew)
            if html:
                htmls.append(html)

        return '<br>'.join(htmls)


def eps_bbox_empty(fn: str) -> bool:
    header = read_file(fn, 1024)
    header_line = b'\n%%BoundingBox: '
    index = header.index(header_line)
    assert index > 0, fn

    header = header[index + len(header_line):]
    header = header[:header.index(b'\n')]
    llx, lly, urx, ury = tuple(map(int, header.split(b' ')))
    return llx >= urx or lly >= ury


class MidiFileLink (TextFileCompareLink):
    def get_content(self, name: str) -> Optional[str]:
        try:
            data = read_file(name)
        except IOError as e:
            if e.errno == errno.ENOENT:
                return None
            else:
                raise

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


def eps_to_png(files: Dict[str, str]):
    """convert EPS in `files` (dict keys) to PNG files (dict values)"""
    if not files:
        return

    start = time.time()
    print('converting %d EPS files' % len(files))

    # EPS files generated for regression tests don't contain fonts
    # to save disk space.  Instead, paths to the fonts are stored in
    # the files that are loaded by Ghostscript's `.loadfont'
    # operator later on.
    data_option = []
    if options.local_datadir:
        for basedir in set(os.path.dirname(f) for f in files):
            if os.path.isdir(os.path.join(basedir, 'share')):
                data_option = ['-slilypond-datadir=%s/share/lilypond/current'
                               % os.path.abspath(basedir)]
                break

    # Ghostscript doesn't like rendering empty pages.
    empty_eps = tempfile.NamedTemporaryFile(
        mode="w", suffix="empty.ps", encoding="utf-8", delete=False)
    empty_eps.write(r'''%!PS-Adobe-3.0 EPSF-3.0
%%BoundingBox: 0 0 1 1
%%EndComments
''')
    empty_eps.close()

    for destdir in set(os.path.dirname(f) for f in files.values()):
        os.makedirs(destdir, exist_ok=True)

    job_count = min(options.job_count, len(files))
    batches : List[List[Tuple[str, str]]] = [[] for j in range(0, job_count)]
    j = 0
    for item in sorted(files.items()):
        batches[j].append(item)
        j = (j+1) % job_count

    drivers = []
    procs = []
    for batch in batches:
        driver = tempfile.NamedTemporaryFile(
            mode="w", suffix="batch.ps", encoding="utf-8", delete=False)
        for (input_fn, outfile) in batch:
            verbose_print = ''
            if options.verbose:
                verbose_print = ' (processing %s\n) print ' % input_fn
            driver.write('''
                %s
                mark /OutputFile (%s)
                /GraphicsAlphaBits 4 /TextAlphaBits 4
                /HWResolution [101 101]
                (png16m) finddevice putdeviceprops setdevice
                (%s) run
                ''' % (verbose_print, outfile, input_fn))

        driver.close()
        drivers.append(driver)
        args =['gs',
               '-dNOSAFER',
               '-dEPSCrop',
               '-q',
               '-dNOPAUSE',
               '-dNODISPLAY',
               '-dAutoRotatePages=/None',
               '-dPrinted=false'] + data_option + [
                   driver.name,
                   '-c',
                   'quit'
                   ]
        if options.verbose:
            print('running %s' % args)
        proc = subprocess.Popen(args)
        procs.append(proc)

    for proc in procs:
      rc = proc.wait()
      if rc:
          raise SystemExit('Ghostscript failed')

    dt = time.time() - start
    if options.verbose:
        print('converted %d EPS files in %f s' % (len(files), dt))

    for driver in drivers:
        os.unlink(driver.name)
    os.unlink(empty_eps.name)


################################################################
# Files/directories


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

    def __init__(self, dest_dir: str):
        self.dest_dir = dest_dir
        self.missing: List[Tuple[str, str]] = []
        self.added: List[Tuple[str, str]] = []
        self.file_links: Dict[str, FileLink] = {}

    def read_sources(self):
        for val in self.file_links.values():
            def note_original(match, ln=val):
                key = ln.name()
                hash_to_original_name[key] = match.group(1)
                return ''

            sf = val.source_file()
            if sf:
                re.sub(r'\\sourcefilename "([^"]+)"',
                       note_original, read_file(sf).decode())
            else:
                print('no source for', val.file_names[1])
        self._convert_images_changed()

    def _convert_images_changed(self):
        images_to_convert = ([], [])

        for val in self.file_links.values():
            if isinstance(val, ImagesLink):
                as_imagelink: Any = val
                oldnew_images = as_imagelink.get_images_to_convert(only_changed=True)
                for oldnew in [0, 1]:
                    images_to_convert[oldnew].extend(oldnew_images[oldnew])

        png_images = {}
        for oldnew in [0,1]:
            png_map = dict((k, os.path.join(self.dest_dir, k.replace('.eps', '.png')))
                           for k in images_to_convert[oldnew])
            eps_to_png(png_map)
            for (key, val) in list(self.file_links.items()):
                if isinstance(val, ImagesLink):
                    val.update_images(png_map)

    def compare_directories(self, dir1: str, dir2: str):
        log_terse('comparing %s' % dir1)
        log_terse('       to %s' % dir2)

        total_compared = 0
        for ext in ['eps',
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
        if f1.endswith('.eps'):
            self.compare_image_files(f1, f2)
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

    def compare_image_files(self, f1: str, f2: str):
        prefix = os.path.commonprefix([f1, f2])
        name = os.path.split(f1)[1]

        stripped = re.sub('-[0-9]+.eps', '', name)
        if name == stripped:
            return
        name = os.path.join(prefix, stripped)

        file_link: Any = None
        try:
            file_link = self.file_links[name]
        except KeyError:
            generic_f1 = re.sub('-[0-9]+.eps', '.ly', f1)
            generic_f2 = re.sub('-[0-9]+.eps', '.ly', f2)
            file_link = ImagesLink(generic_f1, generic_f2)
            self.file_links[name] = file_link

        file_link.add_image_pair(self.dest_dir, f1, f2)

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

        todo = []
        for c in changed:
            if isinstance(c, ImagesLink):
                todo.append(c)

        self._render_unchanged(todo)
        return (changed, below, unchanged)

    def _render_unchanged(self, images_links):
        todo = ([], [])

        # We only render the right side for unchanged images.
        oldnew = 1
        for link in images_links:
            oldnew_images = link.get_images_to_convert(only_changed=False)
            todo[oldnew].extend(oldnew_images[oldnew])

        pngs = dict((k, os.path.join(self.dest_dir, k).replace('.eps','.png'))
                                    for k in todo[oldnew])
        eps_to_png(pngs)
        for link in images_links:
            link.update_images(pngs)

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
            table_rows += link.html_record_string()

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
    float: left;
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

figure.reactive_img.active > div > div.newimg {
    opacity: 0.0;
}
figure.reactive_img.active > div > div.diffimg {
    opacity: 0.0;
}
figure.reactive_img.active > div > div.oldimg {
    opacity: 1.0;
}

figure.reactive_img > div > div.newimg {
    position: absolute;
    opacity: 1.0;
}
figure.reactive_img > div > div.diffimg {
    position: absolute;
    opacity: 0.3;
    filter: blur(2px);
}
figure.reactive_img > div > div.oldimg {
    position: absolute;
    opacity: 0.0;
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


def compare_tree_pairs(tree_pairs, dest_dir: str, threshold: float) -> ComparisonData:
    """Compare a list of directories.

    Returns ComparisonData
    """
    shutil.rmtree(dest_dir, ignore_errors=True)
    data = ComparisonData(dest_dir)
    for dir1, dir2 in tree_pairs:
        data.compare_directories(dir1, dir2)

    data.read_sources()

    data.write_changed(dest_dir, threshold)
    data.create_html_result_page(dest_dir, threshold)
    data.create_text_result_page(dest_dir, threshold)
    data.print_results(threshold)
    return data


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


def test_eps_bbox_empty():
    non_empty = b'''%!PS-Adobe-3.0 EPSF-3.0
%%BoundingBox: 0 0 1 1
%%EndComments
'''
    empty = b'''%!PS-Adobe-3.0 EPSF-3.0
%%BoundingBox: 0 30 0 35
%%EndComments
'''

    open('non_empty.eps', 'wb').write(non_empty)
    open('empty.eps', 'wb').write(empty)

    assert not eps_bbox_empty ('non_empty.eps')
    assert eps_bbox_empty ('empty.eps')


def test_compare_tree_pairs():
    system('rm -rf dir1 dir2')
    system('mkdir dir1 dir2')
    system('cp 19.ly 19.sub.ly')
    system('cp 19.log 19.sub.log')
    system('cp 19-1.eps 19.sub-1.eps')

    system('cp 20multipage* dir1')
    system('cp 20multipage* dir2')

    system('cp removed* dir1')
    system('cp added* dir2')

    # Make sure we have unicode text in the HTML
    system(u'echo HEAD is 人人的乐谱软件 > dir1/tree.gittxt')
    system('echo HEAD is 2 > dir2/tree.gittxt')

    # radical diffs.
    system('cp 20grob{.ly,.eps,-?.eps,.log} dir1/')
    system('cp 19-1.eps dir2/20grob-1.eps')
    system('cp 19-1.eps dir2/20grob-2.eps')

    # Test context diffs.
    system('cp 20grob-1.eps dir1/context-1.eps')
    system('cp 20grob-2.eps dir1/context-2.eps')
    system('cp 20grob-1.eps dir2/context-1.eps')
    system('cp 20grob-1.eps dir2/context-2.eps')

    system('cp 19.ly dir2/20grob.ly')
    system('cp 19.eps dir2/20grob.eps')
    system('cp 19.log dir2/20grob.log')
    system('cp 20{.ly,.log} dir2/')
    system('cp 19multipage.midi dir1/midi-differ.midi')
    system('cp 20multipage.midi dir2/midi-differ.midi')
    system('cp 19multipage.log dir1/log-differ.log')
    system('cp 19multipage.log dir2/log-differ.log &&  echo different >> dir2/log-differ.log &&  echo different >> dir2/log-differ.log')

    data = compare_tree_pairs([('dir1', 'dir2')],
                              'compare-dir1dir2', options.threshold)

    for f in [
            "index.html",
            "index.txt",
            "changed.txt",
            "dir1/20grob-1.png",
            "dir2/20grob-1.png",
            "dir1/20grob-2.png",
            "dir2/20grob-2.png",
            "dir2/added-1.png",
            "dir1/removed-1.png",
            "style.css",
    ]:
        fn = os.path.join("compare-dir1dir2", f)
        assert os.path.exists(fn), fn
    html_fn = "compare-dir1dir2/index.html"
    html = read_file(html_fn).decode()
    assert "removed.log" in html
    assert "added.log" in html
    assert "dir2/removed.compare.jpeg" not in html

    assert html.index("dir2/added-multipage-2.png") <  html.index("dir2/added-multipage-10.png")

    tidy_bin = shutil.which('tidy')
    if tidy_bin:
        subprocess.run([tidy_bin, '-o', '/dev/null', '-q', html_fn], check=True)

    images_link = data.file_links['dir/20grob']
    assert images_link.distance() > 0
    assert len(images_link.image_links) == 2
    first_link = images_link.image_links[1]
    assert first_link.image_exists == (True, True)
    assert first_link.distance() > 0

    second_link = images_link.image_links[2]
    assert second_link.image_exists == (True, True)
    assert second_link.distance() > 0



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
    dist = compare_png_images('p1.png', 'p2.png')

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


    open('added-multipage.ly', 'w', encoding='utf-8').write(
        r"""
#(define default-toplevel-book-handler
  print-book-with-defaults-as-systems )

#(ly:set-option (quote no-point-and-click))

\sourcefilename "added-multipage.ly"
\repeat unfold 12 { a'1\break }
""")

    
    names = simple_names + ["20multipage",
                            "19multipage"] + ['added', 'removed', 'added-multipage.ly']
    binary = os.environ.get("LILYPOND_BINARY", "lilypond")
    args = [binary, '-dseparate-page-formats=ps', '-daux-files', '-dtall-page-formats=ps',
           '--formats=ps', '-dseparate-log-files', '-dinclude-eps-fonts', '-dgs-load-fonts',
           '--header=texidoc', '-dcheck-internal-types', '-danti-alias-factor=1'] + names
    system(' '.join(args))


def run_tests():
    testdir = os.path.join(options.output_dir, 'test-output-distance')
    print('test results in ', testdir)

    shutil.rmtree(testdir, ignore_errors=True)
    system('mkdir ' + testdir)
    os.chdir(testdir)

    test_eps_bbox_empty()
    test_compare_png_images()
    test_basic_compare()
    test_compare_tree_pairs()
    shutil.rmtree(testdir, ignore_errors=True)

options : argparse.Namespace

def main():
    p = argparse.ArgumentParser(
        description="output-distance - compare LilyPond formatting runs",
        usage = 'output-distance.py [options] tree1 tree2 [tree3 tree4]...',
    )

    p.add_argument('--max-count',
                   metavar="COUNT",
                   type=int,
                   default=0,
                   help='only analyze COUNT signature pairs')

    p.add_argument('--job-count',
                   metavar='COUNT',
                   type=int,
                   default=1,
                   help='parallelism for PS to PNG conversion')

    p.add_argument('--local-datadir',
                   action="store_true",
                   help='whether to use the share/lilypond/ directory in the test directory')

    p.add_argument('-o', '--output-dir',
                   type=str,
                   required=True,
                   help='where to put the test results [tree2/compare-tree1tree2]')

    p.add_argument('--test-self',
                   action='store_true',
                   help='run test method')

    p.add_argument('--threshold',
                   dest="threshold",
                   default=0.3,
                   type=float,
                   help='threshold for distance')

    p.add_argument('-v', '--verbose',
                   action="store_true",
                   help='log progress verbosely')

    p.add_argument('dirs',
                   nargs='*',
                   help='directories to compare')
    global options
    options = p.parse_args()

    if options.test_self:
        run_tests()
        return

    if len(options.dirs) % 2 == 1:
        p.print_usage()
        sys.exit(2)

    compare_tree_pairs(
        list(zip(options.dirs[0::2], options.dirs[1::2])), options.output_dir, options.threshold)


if __name__ == '__main__':
    main()
