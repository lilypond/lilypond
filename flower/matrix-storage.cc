/*
  matrix-storage.cc -- implement Matrix_storage

  source file of the Flower Library

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "full-storage.hh"
#include "diagonal-storage.hh"

void
Matrix_storage::set_addition_result (Matrix_storage *&dat, Matrix_storage *right)
{
  if (dat && dat->name() == Diagonal_storage::static_name () 
	 && right->name() == Diagonal_storage::static_name ()) 
	   {
	Diagonal_storage *L = (Diagonal_storage*)dat;
	Diagonal_storage* R = (Diagonal_storage*) right;

	if (R->band_size_i() > L->band_size_i ()) 
	  {
	    L->set_band_size (R->band_size_i());
	  }
	    return ;
    }
  if  (!dat || !dat->is_type_b (Full_storage::static_name())) 
    {

	Matrix_storage *new_stor = (dat)? new Full_storage (dat) : 
	    new Full_storage (right->rows(), right->cols ());
	delete dat;
	dat = new_stor;
    }
}

Matrix_storage*
Matrix_storage::get_product_result (Matrix_storage*left, Matrix_storage*right)
{
  Matrix_storage* dest =0;
  set_product_result (dest, left,right);
  return dest;
}
  

/*
  hairy
 */
void
Matrix_storage::set_product_result (Matrix_storage*&dest, 
			   Matrix_storage*left, Matrix_storage*right)
{
  if (left->name() == Diagonal_storage::static_name () 
	 && right->name() == Diagonal_storage::static_name ()) 
	   {
	Diagonal_storage *L = (Diagonal_storage*)left;
	Diagonal_storage* R = (Diagonal_storage*) right;

	if  (L->band_size_i() + R->band_size_i () < L->dim ()/2) 
	  {
	    if (dest ->name() != Diagonal_storage::static_name ())
	      {
		delete dest;
		dest = new Diagonal_storage;
	      }
	 
	    dest->set_size (L->dim());
	    return;
	  }
    }

  if (dest && dest->name() == Full_storage::static_name ()) 
    {
	dest->set_size (left->rows(), right->cols ());
    }
  else 
    {
	delete dest;
	dest = new Full_storage (left->rows(), right->cols ());
    }
}

IMPLEMENT_IS_TYPE_B(Matrix_storage);

Matrix_storage *
Matrix_storage::get_full (int n, int m)
{
  return new Full_storage (n,m);
}



 bool
Matrix_storage::try_right_multiply (Matrix_storage *, 
				   const Matrix_storage *) const
{
  return false;
}

Array<Real>
Matrix_storage::row (int n) const
{
  Array<Real> r;
  for (int j = 0; j < cols(); j++)
	r.push (elem (n,j));
  return r;
}

Array<Real>
Matrix_storage::column (int n) const
{
  Array<Real> r;
  for (int i = 0; i < rows(); i++)
	r.push (elem (i,n));
  return r;
}

void
Matrix_storage::set_size (int rows, int cols)
{
      
  resize (rows,cols);
}
	
void
Matrix_storage::set_size (int rows)
{
      
	resize (rows);
}
	

void
Matrix_storage::set_band (Matrix_storage *&mat, int b)
{
  Matrix_storage* ns = new Diagonal_storage (mat, b);
  delete mat;
  mat=ns;
}
	

void
Matrix_storage::set_full (Matrix_storage *&mat)
{
  Matrix_storage* ns = new Full_storage (mat);
  delete mat;
  mat=ns;
}
