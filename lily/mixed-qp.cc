/*
  qlp.cc -- implement Mixed_qp

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "debug.hh"
#include "qlp.hh"


void
Mixed_qp::add_equality_cons (Vector , double)
{
  assert (false);
}

void
Mixed_qp::add_fixed_var (int i, Real r)
{
  eq_cons.push (i);
  eq_consrhs.push (r);
}


/**
  eliminate appropriate variables, until we have a Ineq_constrained_qp
  then solve that.

  PRE
  cons should be ascending
  */
Vector
Mixed_qp::solve (Vector start) const 
{
  if (!dim())
    return Vector (0);
  
  print();
  Ineq_constrained_qp pure (*this);
  
  for  (int i= eq_cons.size()-1; i>=0; i--) 
    {
      pure.eliminate_var (eq_cons[i], eq_consrhs[i]);
      start.del (eq_cons[i]);
    }
  Vector sol = pure.solve (start);
  for (int i= 0; i < eq_cons.size(); i++) 
    {
      sol.insert (eq_consrhs[i],eq_cons[i]);
    }
  return sol;
}


Mixed_qp::Mixed_qp (int n)
  : Ineq_constrained_qp (n)
{
}

void
Mixed_qp::OK() const
{
#ifndef NDEBUG
  Ineq_constrained_qp::OK();
  assert (eq_consrhs.size() == eq_cons.size ());
#endif    
}

void
Mixed_qp::print() const
{
#ifndef NPRINT
  Ineq_constrained_qp::print();
  for (int i=0; i < eq_cons.size(); i++) 
    {
      DOUT << "eq cons "<<i<<": x["<<eq_cons[i]<<"] == " << eq_consrhs[i]<< '\n';
    }
#endif
}

