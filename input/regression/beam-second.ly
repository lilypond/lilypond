\version "1.7.18"
\header{
  texidoc="
Seconds are tricky.  We used to have problems with seconds being too
steep, or getting too long stems.  In a file like this, showing
seconds, you'll spot something fishy very quickly.
 
" }
\score{
    \notes\relative c''{
	\stemUp
	 b8-[ c]
	 b16-[ c]
	 a'-[ b]
    }
    \paper{
	raggedright = ##t
    }
}%% new-chords-done %%
