#!@TARGET_PYTHON@
import sys

sys.path.insert (0, 'python')
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
                print g1, g2s 
                d += ORPHAN_GROB_PENALTY

        return d
        
def read_signature_file (name):
    exp_str = ("[%s]" % open (name).read ())
    entries = safeeval.safe_eval (exp_str)

    grob_sigs = [GrobSignature (e) for e in entries]
    sig = SystemSignature (grob_sigs)
    return sig




def compare_directories (dir1, dir2):

    pass


################################################################
# TESTING

def test ():
    def system (x):
        print 'invoking', x
        stat = os.system (x)
        assert stat == 0
        
    import os
    dir = 'output-distance-test'

    print 'test results in dir'
    system ('rm -rf ' + dir)
    os.mkdir (dir)
    os.chdir (dir)
    ly_template = r"""#(set! toplevel-score-handler print-score-with-defaults)
#(set! toplevel-music-handler
 (lambda (p m)
 (if (not (eq? (ly:music-property m 'void) #t))
    (print-score-with-defaults
    p (scorify-music m p)))))

#(ly:set-option 'point-and-click)



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
    
    system ('lilypond -ddump-signatures -b eps ' + ' '.join (names))
    
    sigs = dict ((n, read_signature_file ('%s-0.signature' % n)) for n in names)

    combinations = {}
    for (n1, s1) in sigs.items():
        for (n2, s2) in sigs.items():
            combinations['%s-%s' % (n1, n2)] = SystemLink (s1,s2).distance ()
            

    results =   combinations.items ()
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

if __name__ == '__main__':
    if sys.argv[1:]:
        test_sigs (sys.argv[1],
                    sys.argv[2])
    else:
        test ()


