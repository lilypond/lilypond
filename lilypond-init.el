;;; lilypond-init.el --- Startup code for LilyPond mode
;;;
;;; Add this to your ~/.emacs or ~/.emacs.el, or
;;; install this file into Emacs' site-start.d

(autoload 'LilyPond-mode "lilypond-mode")
(setq auto-mode-alist
      (cons '("\\.ly$" . LilyPond-mode) auto-mode-alist))

(add-hook 'LilyPond-mode-hook (lambda () (turn-on-font-lock)))

