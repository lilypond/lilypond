/*
  diagonal-storage.cc -- implement Diagonal_storage 

  source file of the Flower Library

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#include "diagonal-storage.hh"


#ifdef INLINE
#undef INLINE
#endif

//#define INLINE inline

// #include "full-storage.icc"

int
Diagonal_storage::dim() const
{
  return band_.rows();
}

Diagonal_storage::Diagonal_storage()
{
}

int
Diagonal_storage::rows() const
{
  return band_.rows();
}

int
Diagonal_storage::cols() const
{
  return band_.rows();
}

int
Diagonal_storage::band_size_i() const
{
  return (band_.cols()-1)/2;
}

void
Diagonal_storage::set_band_size (int s)
{
  assert (s>=0);
  Full_storage f (dim(), 2*s+1);
  for (int i=0; i < dim(); i++) 
    {
      int k=-s;
      for (;  k < -band_size_i(); k++)
	f.elem (i,k + s) = 0.0;
      for (; k <=  band_size_i()&& k<=s ; k++)
	f.elem (i, k + s) = band_.elem (i,k+ band_size_i());
      for (; k <= s; k++)
	f.elem (i, k + s) =0.0;
    }

  band_ = f;
}



/*
  any takers?
  */
void
Diagonal_storage::insert_row (int)
{
  assert (false);
}

void
Diagonal_storage::delete_row (int)
{
  assert (false);
}

void
Diagonal_storage::resize (int,int)
{
}

void
Diagonal_storage::resize (int)
{
}

void
Diagonal_storage::delete_column (int)
{
  assert (false);
}
  
Diagonal_storage::~Diagonal_storage()
{
}


bool
Diagonal_storage::band_elt_b (int i,int j) const
{
  return abs (i-j) <= band_size_i();
}

void
Diagonal_storage::assert_valid (int i,int j) const
{
  assert (band_elt_b (i,j));
  assert (i >=0 && j >=0 && i < dim() && j < dim ());
}


void
Diagonal_storage::resize_dim (int d)
{
  Full_storage f (d, 2*band_size_i()+1);
  for (int i=0; i < d && i < dim(); i++) 
    {
      for (int k=0;  k < 2*band_size_i(); k++)
	f.elem (i,k) = elem (i,k);
    }

  band_ = f;
}



bool
Diagonal_storage::mult_ok (int i,int) const
{
  return i < dim();
}

void
Diagonal_storage::mult_next (int &i, int &j) const
{
  j++;
  if (j < i - band_size_i()) 
    j = i- band_size_i();
  if (j > i + band_size_i() || j >= dim ()) 
    {
      i++;
      j = 0 >? i - band_size_i(); 
    }
}

bool
Diagonal_storage::trans_ok (int ,int j) const
{
  return j < dim();
}

void
Diagonal_storage::trans_next (int &i, int& j) const
{
  i++;
  if (i < j - band_size_i())
    i = j-band_size_i();
  
  if (i >= dim() || i > j + band_size_i ()) 
    {
      j++;
      i = 0 >? j - band_size_i(); 
    }
}

static Real nul_entry=0.0;

Real 
Diagonal_storage::elem (int i, int j) const
{
  if (abs (i-j) > band_size_i())
    return 0;
  else
    return band_.elem (i, j - i +band_size_i());
}

Real &
Diagonal_storage::elem (int i, int j)
{
  /*
    if this fails, the previous call fucked up
    */
  assert (!nul_entry);

  if (abs (i-j) > band_size_i())  
    return nul_entry;
  else
    return band_.elem (i, j - i + band_size_i());
}

/*
  Hairy routine to try to save some fp-ops
 */

bool
Diagonal_storage::try_right_multiply (Matrix_storage*dest,
				      const Matrix_storage*right) const
{
  if (right->name() != Diagonal_storage::static_name ()) 
    return false;
  
  const Diagonal_storage*  right_diag = (Diagonal_storage const*)right;
  int band2 = right_diag->band_size_i();
  int n = dim();
  /*
    should check if dest is a Diagonal_storage of sufficient size too.
    */
  for (int i=0;  i < n; i++) 
    {
      for (int j = 0; j < n; j++) 
	{
	  int startk = i - band_size_i() >? 0 >? j - band2;
	  int stopk = i + band_size_i() <? n-1 <? j  + band2;
	  int relk =  startk + band_size_i() -i;
	  Real sum =0.0;
	  for (int k = startk; k <= stopk; k++)
	    sum += band_.elem (i, relk++) * right_diag->elem (k, j);
	  dest->elem (i, j) = sum;
	    
	}
    }
  return true;
}

IMPLEMENT_IS_TYPE_B1(Diagonal_storage, Matrix_storage);


Diagonal_storage::Diagonal_storage (Matrix_storage*stor_l, int band_i)
{
  set_band_size (band_i);
  resize_dim (stor_l->dim());

  for (int i=0,j=0; mult_ok (i,j); mult_next (i,j))
    band_.elem (i, j + band_i -i) = stor_l->elem (i,j);
}

void
Diagonal_storage::OK() const
{
  band_.OK();
}

IMPLEMENT_VIRTUAL_COPY_CONS(Diagonal_storage, Matrix_storage);
