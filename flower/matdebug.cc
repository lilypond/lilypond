#include "dstream.hh"
#include "matrix.hh"

static Dstream *dout = 0;

/**
  Set the debugging output. Will not delete/swallow argument.
 */
void set_matrix_debug(Dstream&ds)
{
    dout = &ds;
}

Matrix::operator String() const
{
    String s;
#ifndef NPRINT
    s="matrix {\n";
    for (int i=0; i< rows(); i++){
	for (int j = 0; j < cols(); j++) {
	    s+= String(dat->elem(i,j), "%6f ");
	}
	s+="\n";
    }
    s+="}\n";
#endif
    return s;
}


void
Matrix::print() const
{
#ifndef NPRINT
    if (!dout)
	return;
    *dout << *this;
#endif
}

Vector::operator String() const
{
    String s;
#ifndef NPRINT
    s="vector [";
    for (int i=0; i < dim(); i++) {
	s += String(dat[i], "%6f") + String(' ');
    }
    s+="]";
#endif
    return s;
}


void
Vector::print() const
{
#ifndef NDEBUG
    if (!dout)
	return;
    *dout << *this<<'\n';
#endif
}
