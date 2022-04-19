;;; Directory Local Variables
;;; See Info node `(emacs) Directory Variables' for more information.

((c++-mode
  (c-file-style . "gnu")
  (indent-tabs-mode . nil))
 (scheme-mode
  (indent-tabs-mode . nil)
  (eval .
        ;; Teach Emacs about a few Scheme forms that it would
        ;; otherwise reformat badly.  This can be removed when
        ;; the patch
        ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=55033
        ;; occurs in a released version that becomes reasonably
        ;; current. --JeanAS
        (progn
          "This is code run by LilyPond's .dir-locals.el to fix indentation \
according to our standards.  If Emacs is warning you that this 'may not be \
safe', just accept running it."
          (put 'match 'scheme-indent-function 1)
          (put 'match-lambda 'scheme-indent-function 0)
          (put 'match-lambda* 'scheme-indent-function 0)
          (put 'match-let 'scheme-indent-function 'scheme-let-indent)
          (put 'match-let* 'scheme-indent-function 1)
          (put 'match-letrec 'scheme-indent-function 1)
          (put 'and-let* 'scheme-indent-function 1)
          (put 'with-syntax 'scheme-indent-function 1)
          (put 'eval-when 'scheme-indent-function 1))))
 (shell-script-mode
  (indent-tabs-mode . nil))
 (texinfo-mode
  (fill-column . 66)
  (indent-tabs-mode . nil)))
