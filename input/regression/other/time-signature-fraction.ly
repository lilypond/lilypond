\version "2.25.28"

\header {
  texidoc = "The deprecated context property @code{timeSignatureFraction}
accesses the value of @code{timeSignature}."
}

#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning
  (ly:translate-cpp-warning-scheme
   "the property '%s' is deprecated; use '%s'")
  "timeSignatureFraction" "timeSignature")

%% It is unnecessary to test every available access method: there are other
%% tests covering them.  It is enough to cover this property's old->new and
%% new->old conversions somehow.
{
  %% setting timeSignatureFraction sets timeSignature
  \set Score.timeSignatureFraction = ##f
  \contextPropertyCheck Score.timeSignature ##f

  \set Score.timeSignatureFraction = 22/7
  \contextPropertyCheck Score.timeSignature 22/7

  %% getting timeSignatureFraction gets timeSignature or a fraction derived from
  %% it
  \set Score.timeSignature = ##f
  \applyContext % TODO: Use \contextPropertyCheck once issue 6835 is fixed.
  #(lambda (ctx)
    (or
     (equal? (ly:context-property ctx 'timeSignatureFraction) #f)
     (ly:error "fail")))

  \set Score.timeSignature = 37/73
  \applyContext
  #(lambda (ctx)
    (or
     (equal? (ly:context-property ctx 'timeSignatureFraction) '(37 . 73))
     (ly:error "fail")))

  \set Score.timeSignature = #'((2 3) . 8)
  \applyContext
  #(lambda (ctx)
    (or
     (equal? (ly:context-property ctx 'timeSignatureFraction) '(5 . 8))
     (ly:error "fail")))

  \set Score.timeSignature = #'((3 . 6) (2 . 2))
  \applyContext
  #(lambda (ctx)
    (or
     (equal? (ly:context-property ctx 'timeSignatureFraction) '(9 . 6))
     (ly:error "fail")))

  s
}
