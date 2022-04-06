\header {

  texidoc = "External fonts may be used without being installed on the
operating system, by loading either a specific font file or a
directory that contains font files.  In this example two logos ('GPL'
and 'GFDL') should be printed, rather than letter glyphs."

  tagline = ##f
}

\version "2.18.0"

%% Create dummy font files in tmp dir (and subdir).

%% Temporarily disable font-export.
%% TODO: This option already seems to be #f at this point when the
%% regression test suite is run, so further explanation would help.
#(ly:set-option 'font-export-dir #f)

%% We want the logo font embedded into output, so we the test result
%% is self-contained.
#(ly:set-option 'gs-load-fonts #f)

gplLogoFont = "
T1RUTwAKAIAAAwAgQ0ZGIE5UIR0AAAUAAAAD2kZGVE2PvFSrAAAI3AAAABxPUy8yV+hiwAAAARAA
AABgY21hcAANAugAAAOcAAABQmhlYWQaNWE/AAAArAAAADZoaGVhCBUFxAAAAOQAAAAkaG10eAm0
AGMAAAj4AAAACG1heHAAAlAAAAABCAAAAAZuYW1lH80HAQAAAXAAAAIrcG9zdP+4ADIAAATgAAAA
IAABAAAAAQAAnX06cV8PPPUACwPoAAAAANqmcOcAAAAA2qetQwAxAAAFmAIwAAAACAACAAAAAAAA
AAEAAAIh//UAWgXMAAAAAAWYAAEAAAAAAAAAAAAAAAAAAAACAABQAAACAAAABAXMAZAABQAAAooC
vAAAAIwCigK8AAAB4AAxAQIAAAIABQkAAAAAAAAAAAABAAAAAAAAAAAAAAAAUGZFZACAAEEAQQMg
/zgAWgIhAAsAAAABAAAAAAAAAiEAIAAgAAEAAAAOAK4AAQAAAAAAAAA8AHoAAQAAAAAAAQAIAMkA
AQAAAAAAAgAHAOIAAQAAAAAAAwAcASQAAQAAAAAABAAAAUMAAQAAAAAABQAJAVgAAQAAAAAABgAI
AXQAAwABBAkAAAB4AAAAAwABBAkAAQAQALcAAwABBAkAAgAOANIAAwABBAkAAwA4AOoAAwABBAkA
BAAAAUEAAwABBAkABQASAUQAAwABBAkABgAQAWIAUAB1AGIAbABpAGMAIABkAG8AbQBhAGkAbgAs
ACAAYQBmAHQAZQByACAAdABoAGUAIABGAHIAZQBlACAAUwBvAGYAdAB3AGEAcgBlACAARgBvAHUA
bgBkAGEAdABpAG8AbgAgACgAZwBuAHUALgBvAHIAZwApAC4AAFB1YmxpYyBkb21haW4sIGFmdGVy
IHRoZSBGcmVlIFNvZnR3YXJlIEZvdW5kYXRpb24gKGdudS5vcmcpLgAARAB1AG0AbQB5AEcAUABM
AABEdW1teUdQTAAAUgBlAGcAdQBsAGEAcgAAUmVndWxhcgAARgBvAG4AdABGAG8AcgBnAGUAIAAy
AC4AMAAgADoAIAAgADoAIAAzADAALQAzAC0AMgAwADIAMAAARm9udEZvcmdlIDIuMCA6ICA6IDMw
LTMtMjAyMAAAAAAAVgBlAHIAcwBpAG8AbgAgACAAAFZlcnNpb24gIAAARAB1AG0AbQB5AEcAUABM
AABEdW1teUdQTAAAAAAAAwAAAAMAAAAcAAEAAAAAADwAAwABAAAAHAAEACAAAAAEAAQAAQAAAEH/
/wAAAEH////AAAEAAAAAAAABBgAAAQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAABAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAMAAAAA
AAD/tQAyAAAAAAAAAAAAAAAAAAAAAAAAAAABAAQEAAEBAQlEdW1teUdQTAABAgABACv4GwD4HAH4
HQL4HgP4HwS8ixwFmPjEBRwAlw8cAAAQHACaERwAIRwDuRIABQIAAQABAD0APQBFAEVQdWJsaWMg
ZG9tYWluLCBhZnRlciB0aGUgRnJlZSBTb2Z0d2FyZSBGb3VuZGF0aW9uIChnbnUub3JnKS5EdW1t
eUdQTAAAAAAiAAICAAEAFgMXvRb6GPip/hgGvfx3FfhF+bT8RQcO9vlR9xMViweFi4WHiYaNkJCP
kYsI/Kj4LxVj+0Fk+0Bj+0EI+dYGdY12knmYdZyCo4umi6CRopOgm7KirqWst8G/usSzy7jPsNyg
CER0SmVPX1lmXWBmWHp0fXKDb4mCioOLgwhesnm5HpkGrI6rlKqWvp68preqm5eblpebj5COkYyS
CIyMBZZ9jIAeiAZ5inmHeoZwg3CAcX+kpKifqZ6ooaejoamSlZKWjJgIjAeVgo6AHoCLfYiEiVp9
YG9kcazMyLrMqqaXp5WojgiFi4GLBYeL/AqL+/qL+y+LIIsF+F38LRV3zoDVi9OL0ZbPosSLhIqE
i4SLYpFjk2OUXJhdm11xZnVjhVkI/Lz3uhWgi8eLBZ6LnYqei5OKk4iQhIyJjImLiIuHioiKhwiH
d4Z4h3cIXosFms37BIsFeTl5N3k5CPcDBpKtk6ySrQhXBo2TjJKNkwjsBoJhgl+BYYN2boxyiwiB
i1SLBXqLe4t6jIGMf5CLl4uNi46MjZ3end6e3pCZnJGciwj3WRaliwXVBqKLoYqii5OKk4iQhIyJ
jImLiIuHioeKiIJggmGBYIN2bIxyiwhxi1OLgYsFgmSDZIJkCF8Goe2g7qHtCPdwFrYGdzF3MHcx
CPMGjIONg4yDCPsxBqHtoe6h7Qj7SXQVgWCCXoFgCPcAi6j3FwX3GPulFYsHj4+Ihx+Li4uMBY+H
jYce6lkVl2axd7iLCI6LkYsFm4uZjJqOuZO4m7WfwaS9rLqvurC9t6bDkJWPmIuWi5aGloCPCIOO
fIuLlIuMjIqLjI2WkYyMi6ikqaacr5CVjZaLlgiSB4eidJRxiwiHBmWKYX1ufYqKiYuKi4aLiJCL
j4uOjY2OjQirmrWatIwIkQaYi5qJl4WfgpN4i3aLf4l9hX+Ac3t2eXmHh3R2i4kIiweehJN6i3mL
fYZ8hX58a3VwcnJNTj5VOWRgd115W4N7iHuKe4sIhYuHiwV0i3KQdpd3ln2fhKCLjIqLi4yLkJCO
j4uOi4+KjIcIDvp8FBwFYRV3n/i1iwaLDAqLjJuQp5PElPAMDJsLm5cMDQAAAAAAAQAAAADabjaA
AAAAANqmcOcAAAAA2qetQwPoADIFzAAx"

gfdlLogoFont = "
T1RUTwAKAIAAAwAgQ0ZGIH4FALoAAAVEAAAEUEZGVE2PvFQcAAAJlAAAABxPUy8yWBlixAAAARAA
AABgY21hcAANAugAAAPgAAABQmhlYWQaBGDXAAAArAAAADZoaGVhCBsFlwAAAOQAAAAkaG10eAsu
AHcAAAmwAAAACG1heHAAAlAAAAABCAAAAAZuYW1li6hnNgAAAXAAAAJwcG9zdP+4ADMAAAUkAAAA
IAABAAAAAQAAY2S57F8PPPUACwPoAAAAANqmcOcAAAAA2qestAAy//wFZQJbAAAACAACAAAAAAAA
AAEAAAJa//0AWgWXAAAAAAVlAAEAAAAAAAAAAAAAAAAAAAACAABQAAACAAAABAWXAZAABQAAAooC
vAAAAIwCigK8AAAB4AAxAQIAAAIABQkAAAAAAAAAAAABAAAAAAAAAAAAAAAAUGZFZACAAEEAQQMg
/zgAWgJaAAMAAAABAAAAAAAAAloAIAAgAAEAAAAOAK4AAQAAAAAAAABRAKQAAQAAAAAAAQAJAQoA
AQAAAAAAAgAHASQAAQAAAAAAAwAcAWYAAQAAAAAABAAAAYUAAQAAAAAABQAJAZoAAQAAAAAABgAJ
AbgAAwABBAkAAACiAAAAAwABBAkAAQASAPYAAwABBAkAAgAOARQAAwABBAkAAwA4ASwAAwABBAkA
BAAAAYMAAwABBAkABQASAYYAAwABBAkABgASAaQAKABjACkAIAAyADAAMAA3ACAARgByAGUAZQAg
AFMAbwBmAHQAdwBhAHIAZQAgAEYAbwB1AG4AZABhAHQAaQBvAG4AIAAoAGcAbgB1AC4AbwByAGcA
KQAuAAoARwBOAFUAIABGAHIAZQBlACAARABvAGMAdQBtAGUAbgB0AGEAdABpAG8AbgAgAEwAaQBj
AGUAbgBzAGUAIAB2ADEALgAzAC4AAChjKSAyMDA3IEZyZWUgU29mdHdhcmUgRm91bmRhdGlvbiAo
Z251Lm9yZykuCkdOVSBGcmVlIERvY3VtZW50YXRpb24gTGljZW5zZSB2MS4zLgAARAB1AG0AbQB5
AEcARgBEAEwAAER1bW15R0ZETAAAUgBlAGcAdQBsAGEAcgAAUmVndWxhcgAARgBvAG4AdABGAG8A
cgBnAGUAIAAyAC4AMAAgADoAIAAgADoAIAAzADAALQAzAC0AMgAwADIAMAAARm9udEZvcmdlIDIu
MCA6ICA6IDMwLTMtMjAyMAAAAAAAVgBlAHIAcwBpAG8AbgAgACAAAFZlcnNpb24gIAAARAB1AG0A
bQB5AEcARgBEAEwAAER1bW15R0ZETAAAAAADAAAAAwAAABwAAQAAAAAAPAADAAEAAAAcAAQAIAAA
AAQABAABAAAAQf//AAAAQf///8AAAQAAAAAAAAEGAAABAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAEAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAwAAAAAAAP+1ADIAAAABAAAAAAAAAAAAAAAAAAAAAAEABAQAAQEBCkR1bW15R0ZETAAB
AgABAC74GwD4HAH4HQL4HgP4HwSMDAG9hxwFZfjvBRwAsQ8cAAAQHAC0ERwALBwEJBIABQIAAQAB
AFIAUgBbAFsoYykgMjAwNyBGcmVlIFNvZnR3YXJlIEZvdW5kYXRpb24gKGdudS5vcmcpLgpHTlUg
RnJlZSBEb2N1bWVudGF0aW9uIExpY2Vuc2UgdjEuMy5EdW1teUdGREwAAAAAIgACAgABABkDaL0W
HAUz+Kkc+s0Gvfx3FfhFHATP/EUHDvpqthX+JYup96sFgeiB6YHoCPrpg/7gBpUwlDGVMIEwgjKB
MAj6HAb9GPeEFXOKgYqDi4OLg4xzjAiLdpqKBZeKj4eLfghIB32BgYh/iwhebczr5Ka/uR+li55+
l3YIjV+WiwWOu4uVjp4IiZEFcZx3kXCLCENfSfsB+wO3Q80fp4ugk62iCN0Hi5iMjJWPCNj3bxWD
i4uLW40Ii3iXiQWaiYuJi14I+2EHi2yKhImHiYiJiYOKCH+Ji3cFpoyXjJaLlouXiqmKCIuffI0F
g4yHjYqPiZCKk4unCOkHk4ySi5KLCJwGmIuPh4x9CI1ymIuKwQWLkYuSjLIIfouJcAWKgoiJfYsI
egZ/i4eMhYwI9w4HoI2QipaLCKKZiIUfjmWXi4/TiY4Ff42Fi36LhItTiX6LCPd9++sVq4upoZ+w
na2Wt4vBCOB40D8eZooFcIuLiomLh4uLi2ONCIt4lYkFl4mMiYteCPt1B4t5ioWHhgiCggV/B6OM
loyTiwiWi56Jl4sIfacVhIuGjISNCPeyB5GNkIuSi56LoIWWgaZykmWLTIs5fkhEiwj3lG8VnYuU
i52NCIyPBZDefosFg1gFi4uKih6BhYKKeYt+i32Le4wI94gHi6mMk42PjY6OjZOMCJeMi6AFZImM
i3+LgIuKi2WNCIt2l4oFmomLi4tdCPt1B4t6ioSGhgiAgot/BaGMlYyTi6CLrYmdiwj4aTkVYC9u
IyMwqLYe+GCT/GAHa9tr9vbcq6sei46Kj4mOi/czjvcfi/csCJMGjgRgL24jIzCotrbmqPPz525g
HoMWqzqrICA7a2tr22v29tyrqx6SSRWH/AUFioqKix56dkh5PotSi06VWqSKjIqMi4wIjIyLix7i
9xo390mLjAWNjIyNHowGtH7BgcCL14vVn6LDjIyLjIyLCI2NiokfhH4VbVlGeUOLWItXlGKXCN37
RIuKi4k0+xgFunTGgsGL1YvOm5ueCJL4BBVzUUF3PotVi1SVYpgI4PtKMvsbBbtyyIHDi9eLz52b
nwj7NfhVFX6LfYx7iwhYUoiCH4sHfuCExh69ypqWH4sHnEOxQh6RB9PaZ3Ifiwd2Q4BcVSuNox6L
B5u/jrseoYugi5qKCA4cBZcUixV3n/jviwaNDAqTCo6OjZWNjI+NjPe5DAy4C5GNjYyMj6GMjI0M
DQAAAAEAAAAA2m42gAAAAADapnDnAAAAANqnrLQFlwAyBZcARQ==
"

% TODO: this should be rewritten (e.g. using rnrs bytevectors, see base64
% implementations in guile-gnome and guile-gcrypt) as soon as
% compatibility with Guile v1.8 is no longer needed. -vv

#(define (base64-decode out-port input)
  (let ((base64-decode-table (make-hash-table 64))
        (alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")
        (in-port (if (string? input)
                  (open-input-string input)
                  input))
        (current-state 1)
        (current-char '())
        (byte #f)
        (top-bits '())
        (bottom-bits '()))
    (map (lambda (item)
       (hash-set! base64-decode-table item
        (string-index alphabet item)))
     (string->list alphabet))
    ;; inspired by https://sourceware.org/legacy-ml/guile/2000-01/msg00622.html
    ;; looks clunky but gets the job done.
    (while
     (not (eof-object? current-char))
     (set! current-char (read-char in-port))
     (case current-state
       ((1) (begin
             (set! byte
              (hashv-ref base64-decode-table current-char))
             (if byte
              (begin
               (set! top-bits
                (* (logand byte #b00111111) 4))))))
       ((2) (if (eof-object? current-char)
             (begin
              (set! bottom-bits #b00000000)
              (write-char (integer->char
                (logior top-bits bottom-bits))
               out-port))
             (begin
              (set! byte
               (hashv-ref base64-decode-table current-char))
              (if byte
                  (begin
                   (set! bottom-bits
                    (/ (logand byte #b00110000) 16))
                   (write-char (integer->char
                     (logior top-bits bottom-bits))
                    out-port)
                   (set! top-bits
                    (* (logand byte #b00001111) 16)))))))
       ((3) (if (eof-object? current-char)
             (begin
              (set! bottom-bits #b00000000)
              (write-char (integer->char
                (logior top-bits bottom-bits))
               out-port))
             (begin
              (set! byte
               (hashv-ref base64-decode-table current-char))
              (if byte
               (begin
                (set! bottom-bits
                 (/ (logand byte #b00111100) 4))
                (write-char (integer->char
                  (logior top-bits bottom-bits))
                 out-port)
                (set! top-bits
                 (* (logand byte #b00000011) 64)))))))
       ((4) (if (eof-object? current-char)
             (begin
              (set! bottom-bits #b00000000)
              (write-char (integer->char
                (logior top-bits bottom-bits))
               out-port))
             (begin
              (set! byte
               (hashv-ref base64-decode-table current-char))
              (if byte
               (begin
                (set! bottom-bits
                 (logand byte #b00111111))
                (write-char (integer->char
                  (logior top-bits bottom-bits))
                 out-port)))))))
     (if byte
      (begin
       (if (eqv? current-state 4)
           (set! current-state 1)
           (set! current-state (1+ current-state)))
       (set! byte #f))))
    #t))

#(define (dump-base64 name content)
  (let* ((port (open-output-file name)))
   (cond-expand
    (guile-2 (set-port-encoding! port "ISO-8859-1"))
    (else))
   (base64-decode port content)
   (close port)))

#(define (my-tmpnam)
  "like tmpnam but not deprecated"
  (format #f "~a/font-name-add-file-~a" (or (getenv "TMPDIR") "/tmp") (random 1000000)))

#(define temp-dir (my-tmpnam))
#(define subdir (string-append temp-dir "/subdir"))

#(let*
  ((fontfile (string-append temp-dir "/gpl-logo-font.otf" )))
  
  (mkdir temp-dir) ;; will error out if it already exists.
  (mkdir subdir)
  
  (dump-base64 fontfile gplLogoFont)
  (ly:font-config-add-font fontfile)

  (dump-base64 (string-append subdir "/gfdl-logo.otf") gfdlLogoFont)
  (ly:font-config-add-directory subdir))

\book {
  \markup {
    GPL logo: \fontsize #20 \override #'(font-name . "DummyGPL") "A"
  }
  \markup {
    GFDL logo: \fontsize #20 \override #'(font-name . "DummyGFDL") "A"
  }
}

#(define (clear-dir dirname)
  (let*
   ((dir (opendir dirname)))
   (do ((f (readdir dir) (readdir dir)))
    ((eof-object? f))
    (or (equal? "." f) (equal? ".." f)
     (delete-file (string-append dirname "/" f))))
   (closedir dir)))

#(begin
  (clear-dir subdir)
  (rmdir subdir)
  (clear-dir temp-dir)
  (rmdir temp-dir))
