import math

def gcd (a,b):
	if b == 0:
		return a
	c = a
	while c: 
		c = a % b
		a = b
		b = c
	return a

	    
class Rational:
    def __init__ (self, n, d = 1):
	    self.num = n
	    self.den = d
	    self.simplify ()
	    
    def simplify (self):
	    (n,d) = (self.num, self.den)
	    if d < 0:
		    d = -d
		    n = -n
	    if n == 0:
		    return (0,1)
	    else:
		    g = gcd (n, d)
		    (n,d) =  (n/g, d/g)
		    
            (self.num, self.den) = (n,d)

    def __mul__ (a,b):
	    (x,y) = (a.num, a.den)
	    (p,q) = (b.num, b.den)
	    return Rational (x*p, y*q)

    def __div__ (a,b):
	    (x,y) = (a.num, a.den)
	    (q,p) = (b.num, b.den)
	    return Rational (x*p, y*q)

    def __add__ (a, b):
	    (x,y) = (a.num, a.den)
	    (p,q) = (b.num, b.den)

	    return Rational (x*q + p*y, y*q)

    def __neg__ (a):
	    (p,q) = (a.num, a.den)
	    return Rational (-p,q)
    
    def __str__ (a):
	    return '%d/%d' % (a.num, a.den) 

    def __sub__ (a, b):
	    return a + (- b)

    def __cmp__ (a, b):
	    num = (a - b).num
	    return num.__cmp__ (0)
	    
    def float (self):
	    r = 1.0 * self.num
	    r /= self.den
	    return r
    
    def floor (self):
	    return Rational(int (math.floor (self.float())), 1)

    def ceil (self):
	    
	    return Rational(int (math.ceil (self.float())), 1)
	    
if __name__ == '__main__':
	print '1/2', Rational (1,2)
	print '1/2', Rational (2,4)
	r1 = Rational (1,2)
	r2 = Rational (1,3)

	print '5/6', r1 + r2
	print '1/6', r1 - r2
	print '1/6', r1 * r2

	print '1/2 < 2/3 ', Rational (1,2) < Rational (2, 3)
	print '1/2 == 1/2 ', Rational (1,2) == Rational (1, 2)
	print 'floor (5/6) ', Rational (5,6).floor()
	print 'floor (3/3) ', Rational (3,3).floor()
	print '(1/2) / (2/4)', Rational (1,2) / Rational (2,4)
