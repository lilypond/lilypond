#include "dstream.hh"
#include "matrix.hh"

static Dstream *dout = new Dstream(0,0);

void set_matrix_debug(Dstream&ds)
{
    dout = &ds;
}

Matrix::operator String() const
{
    String s("matrix {\n");
#ifndef NPRINT
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
    *dout << *this;
#endif
}

Vector::operator String() const
{
    String s("vector [");
#ifndef NPRINT
    for (int i=0; i < dim(); i++) {
	s += String(dat[i], "%6f") + String(' ');
    }
#endif
    s+="]";
    return s;
}


void
Vector::print() const
{
#ifndef NDEBUG
    *dout << *this<<'\n';
#endif
}
