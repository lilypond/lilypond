#include "smat.hh"

void
Full_storage::operator=(Full_storage const &fs)
{
    resize(fs.h, fs.w);
    OK();
    fs.OK();
    for (int i=0; i<h; i++)
	for (int j=0; j<w; j++)
	    els[i][j]= fs.els[i][j];
}

void
Full_storage::OK() const
{
#ifndef NDEBUG
    //    static Real dummy;		
    assert(maxh >= h && maxw >= w);
    assert(h >= 0 && w >= 0);
    assert(els||!maxh);
    if (maxh>0) {		// access outer elts.
	Real *r = els[maxh -1];
	#if 0
	if (maxw>0) {
	    assert(r);
	    Real s = r[maxw -1]; // accessing unitialised memory.
	    s = sin(s);
	}
	#endif
    }
#endif
}
void
Full_storage::resize_cols(int newh)
{
    if (newh <= maxh) {
	h=newh;
	return;
    }
   
    Real ** newa=new Real*[newh];
    int j=0;
    for (; j < h; j++)
	newa[j] = els[j];
    for (; j < newh; j++)
	newa[j] = new Real[maxw];
    delete[] els;
    els=newa;

    h = maxh = newh;
}

void
Full_storage::resize_rows(int neww)
{
    if (neww <= maxw) {
	w=neww;
	return;
    }
    for (int i=0; i < maxh ; i++) {
	Real* newa=new Real[neww];
	for (int k=0; k < w; k++)
	    newa[k] = els[i][k];

	delete[] els[i];
	els[i] = newa;
    }
    w = maxw = neww;	
}

Full_storage::~Full_storage() {
    for (int i=0; i < maxh; i++)
	delete [] els[i];
    delete[] els;
}

void
Full_storage::resize(int rows, int cols)
{
    OK();
    resize_cols(rows);
    resize_rows(cols);

}


bool
Full_storage::mult_ok(int i, int j) const
{
    return valid(i,j);
}

bool
Full_storage::trans_ok(int i, int j) const
{
       return valid(i,j);
} 


void
Full_storage::trans_next(int &i, int &j) const
{
    assert(trans_ok(i,j));
    i++;
    if (i >= h) {
	i=0;
	j ++;
    }
}

void
Full_storage::mult_next(int &i, int &j) const
{
    assert(mult_ok(i,j));
    j++;
    if (j >= w) {
	j=0;
	i++;
    }
}

void
Full_storage::delete_column(int k)
{
    assert(0 <= k &&k<w);    
    for (int i=0; i< h ; i++)
	for (int j=k+1; j <w; j++)
	    els[i][j-1]=els[i][j];
    w--;
}
void
Full_storage::delete_row(int k)
{
    assert(0 <= k &&k<h);
    for (int i=k+1; i < h ; i++)
	for (int j=0; j < w; j++)
	    els[i-1][j]=els[i][j];
    h--;
}


void
Full_storage::insert_row(int k)
{
    assert(0 <= k&& k <=h);
    resize_cols(h+1);
    for (int i=h-1; i > k ; i--)
	for (int j=0; j <w; j++)
	    els[i][j]=els[i-1][j];

}


Array<Real>
Full_storage::row(int n) const
{
    Array<Real> r;
    for (int j = 0; j < w; j++)
	r.push(els[n][j]);
    return r;
}

Array<Real>
Full_storage::column(int n) const
{
    
    Array<Real> r;
    for (int i = 0; i<h; i++)
	r.push(els[i][n]);
    return r;
}


Full_storage::Full_storage(Full_storage&s)
{
    init();
    (*this) = s;
}
virtual_smat*
Full_storage::clone()
{
    return new Full_storage(*this);
}
/****************************************************************/

virtual_smat *
virtual_smat::get_full(int n, int m)
{
    return new Full_storage(n,m);
}
