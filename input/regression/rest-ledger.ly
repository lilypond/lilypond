\version "1.3.146"
\header {
texidoc = "whole and half rests moving outside the staff should get
ledger lines"
}


\score {
\notes  {
   \property Voice.Rest \set #'staff-position  = #4
  r1 \property Voice.Rest \set #'staff-position  = #5
  r1 \property Voice.Rest \set #'staff-position  = #6
  
  r1 \property Voice.Rest \set #'staff-position  = #-6
  r1 \property Voice.Rest \set #'staff-position  = #-7
  r1 \property Voice.Rest \set #'staff-position  = #-8
  r1

   \property Voice.Rest \set #'staff-position  = #6
  r2 \property Voice.Rest \set #'staff-position  = #7
  r2 \property Voice.Rest \set #'staff-position  = #8
  
  r2 \property Voice.Rest \set #'staff-position  = #-4
  r2 \property Voice.Rest \set #'staff-position  = #-5
  r2 \property Voice.Rest \set #'staff-position  = #-6
  r2

  

  
}}
