;;; lilypond-init.el --- Startup code for LilyPond mode
;;
;; Instructions, extracted from Documentation/topdocs/INSTALL.texi: 

;; Emacs mode for entering music and running LilyPond is contained in
;; the source archive as `lilypond-mode.el', `lilypond-indent.el',
;; `lilypond-font-lock.el' and `lilypond-words.el'. You should install 
;; these files to a directory included in your `load-path'. 
;; File `lilypond-init.el' should be placed to `load-path/site-start.d/' 
;; or appended to your `~/.emacs' or `~/.emacs.el'. 

;; As a user, you may want add your source path or, e.g., `~/site-lisp/' to
;; your `load-path'. Append the following line (modified) to your `~/.emacs':

;(setq load-path (append (list (expand-file-name "~/site-lisp")) load-path))

(autoload 'LilyPond-mode "lilypond-mode" "LilyPond Editing Mode" t)
(add-to-list 'auto-mode-alist '("\\.ly\\'" . LilyPond-mode))
(add-to-list 'auto-mode-alist '("\\.ily\\'" . LilyPond-mode))
(add-hook 'LilyPond-mode-hook (lambda () (turn-on-font-lock)))

