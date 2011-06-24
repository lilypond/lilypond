"""Implementation of rational arithmetic."""

from __future__ import division

import math as _math

def _gcf(a, b):
    """Returns the greatest common factor of a and b."""
    a = abs(a)
    b = abs(b)
    while b:
        a, b = b, a % b
    return a

class Rational(object):
    """
    This class provides an exact representation of rational numbers.
 
    All of the standard arithmetic operators are provided.  In mixed-type
    expressions, an int or a long can be converted to a Rational without
    loss of precision, and will be done as such.

    Rationals can be implicity (using binary operators) or explicity
    (using float(x) or x.decimal()) converted to floats or Decimals;
    this may cause a loss of precision.  The reverse conversions can be
    done without loss of precision, and are performed with the
    from_exact_float and from_exact_decimal static methods.  However,
    because of rounding error in the original values, this tends to
    produce "ugly" fractions.  "Nicer" conversions to Rational can be made
    with approx_smallest_denominator or approx_smallest_error.
    """

    def __init__(self, numerator, denominator=1):
       """Contructs the Rational object for numerator/denominator."""
       if not isinstance(numerator, (int, long)):
           raise TypeError('numerator must have integer type')
       if not isinstance(denominator, (int, long)):
           raise TypeError('denominator must have integer type')
       if not denominator:
           raise ZeroDivisionError('rational construction')
       self._d = denominator
       self._n = numerator
       self.normalize_self()
    # Cancel the fraction to reduced form
    def normalize_self(self):
       factor = _gcf(self._n, self._d)
       self._n = self._n // factor
       self._d = self._d // factor
       if self._d < 0:
           self._n = -self._n
           self._d = -self._d

    def numerator(self):
        return self._n

    def denominator(self):
        return self._d

    def __repr__(self):
        if self._d == 1:
            return "Rational(%d)" % self._n
        else:
            return "Rational(%d, %d)" % (self._n, self._d)
    def __str__(self):
        if self._d == 1:
            return str(self._n)
        else:
            return "%d/%d" % (self._n, self._d)
    def __hash__(self):
        try:
            return hash(float(self))
        except OverflowError:
            return hash(long(self))
    def __float__(self):
        return self._n / self._d
    def __int__(self):
        if self._n < 0:
            return -int(-self._n // self._d)
        else:
            return int(self._n // self._d)
    def __long__(self):
        return long(int(self))
    def __nonzero__(self):
        return bool(self._n)
    def __pos__(self):
        return self
    def __neg__(self):
        return Rational(-self._n, self._d)
    def __abs__(self):
        if self._n < 0:
            return -self
        else:
            return self
    def __add__(self, other):
        if isinstance(other, Rational):
            return Rational(self._n * other._d + self._d * other._n,
                            self._d * other._d)
        elif isinstance(other, (int, long)):
            return Rational(self._n + self._d * other, self._d)
        elif isinstance(other, (float, complex)):
            return float(self) + other
        else:
            return NotImplemented
    __radd__ = __add__
    def __sub__(self, other):
        if isinstance(other, Rational):
            return Rational(self._n * other._d - self._d * other._n,
                            self._d * other._d)
        elif isinstance(other, (int, long)):
            return Rational(self._n - self._d * other, self._d)
        elif isinstance(other, (float, complex)):
            return float(self) - other
        else:
            return NotImplemented
    def __rsub__(self, other):
        if isinstance(other, (int, long)):
            return Rational(other * self._d - self._n, self._d)
        elif isinstance(other, (float, complex)):
            return other - float(self)
        else:
            return NotImplemented
    def __mul__(self, other):
        if isinstance(other, Rational):
            return Rational(self._n * other._n, self._d * other._d)
        elif isinstance(other, (int, long)):
            return Rational(self._n * other, self._d)
        elif isinstance(other, (float, complex)):
            return float(self) * other
        else:
            return NotImplemented
    __rmul__ = __mul__
    def __truediv__(self, other):
        if isinstance(other, Rational):
            return Rational(self._n * other._d, self._d * other._n)
        elif isinstance(other, (int, long)):
            return Rational(self._n, self._d * other)
        elif isinstance(other, (float, complex)):
            return float(self) / other
        else:
            return NotImplemented
    __div__ = __truediv__
    def __rtruediv__(self, other):
        if isinstance(other, (int, long)):
            return Rational(other * self._d, self._n)
        elif isinstance(other, (float, complex)):
            return other / float(self)
        else:
            return NotImplemented
    __rdiv__ = __rtruediv__
    def __floordiv__(self, other):
        truediv = self / other
        if isinstance(truediv, Rational):
            return truediv._n // truediv._d
        else:
            return truediv // 1
    def __rfloordiv__(self, other):
        return (other / self) // 1
    def __mod__(self, other):
        return self - self // other * other
    def __rmod__(self, other):
        return other - other // self * self
    def __divmod__(self, other):
        return self // other, self % other
    def __cmp__(self, other):
        if other == 0:
            return cmp(self._n, 0)
        else:
            return cmp(self - other, 0)
    def __pow__(self, other):
        if isinstance(other, (int, long)):
            if other < 0:
                return Rational(self._d ** -other, self._n ** -other)
            else:
                return Rational(self._n ** other, self._d ** other)
        else:
                return float(self) ** other
    def __rpow__(self, other):
        return other ** float(self)
    def round(self, denominator):
        """Return self rounded to nearest multiple of 1/denominator."""
        int_part, frac_part = divmod(self * denominator, 1)
        round_direction = cmp(frac_part * 2, 1)
        if round_direction == 0:
           numerator = int_part + (int_part & 1) # round to even
        elif round_direction < 0:
           numerator = int_part
        else:
           numerator = int_part + 1
        return Rational(numerator, denominator)



def rational_from_exact_float(x):
    """Returns the exact Rational equivalent of x."""
    mantissa, exponent = _math.frexp(x)
    mantissa = int(mantissa * 2 ** 53)
    exponent -= 53
    if exponent < 0:
        return Rational(mantissa, 2 ** (-exponent))
    else:
        return Rational(mantissa * 2 ** exponent)



def rational_approx_smallest_denominator(x, tolerance):
    """
    Returns a Rational approximation of x.
    Minimizes the denominator given a constraint on the error.

    x = the float or Decimal value to convert
    tolerance = maximum absolute error allowed,
                must be of the same type as x
    """
    tolerance = abs(tolerance)
    n = 1
    while True:
        m = int(round(x * n))
        result = Rational(m, n)
        if abs(result - x) < tolerance:
            return result
        n += 1


def rational_approx_smallest_error(x, maxDenominator):
    """
    Returns a Rational approximation of x.
    Minimizes the error given a constraint on the denominator.

    x = the float or Decimal value to convert
    maxDenominator = maximum denominator allowed
    """
    result = None
    minError = x
    for n in xrange(1, maxDenominator + 1):
        m = int(round(x * n))
        r = Rational(m, n)
        error = abs(r - x)
        if error == 0:
            return r
        elif error < minError:
            result = r
            minError = error
    return result

def divide(x, y):
    """Same as x/y, but returns a Rational if both are ints."""
    if isinstance(x, (int, long)) and isinstance(y, (int, long)):
        return Rational(x, y)
    else:
        return x / y

