;; This is the Aquamacs Preferences file.
;; Add Emacs-Lisp code here that should be executed whenever
;; you start Aquamacs Emacs. If errors occur, Aquamacs will stop
;; evaluating this file and print errors in the *Messags* buffer.
;; Use this file in place of ~/.emacs (which is loaded as well.)

;;CEDET needs to be at the top to over-write the built in version
(add-to-list 'load-path "~/.emacs.d/elisp/")
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/")

(when (featurep 'aquamacs)
  (load-file "~/dev/elisp/cedet-1.1/common/cedet.el"))

(require 'ido)
(ido-mode t)

(global-ede-mode 1)
(semantic-load-enable-gaudy-code-helpers)
(global-srecode-minor-mode 1)

(require 'semantic/ia)
(add-to-list 'load-path "~/.emacs.d/")

(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)

(defun my-semantic-hook ()
  (imenu-add-to-menubar "TAGS"))
(add-hook 'semantic-init-hooks 'my-semantic-hook)

(defun my-c-mode-cedet-hook ()
  (add-to-list 'ac-sources 'ac-source-gtags)
  (add-to-list 'ac-sources 'ac-source-semantic))
(add-hook 'c-mode-common-hook 'my-c-mode-cedet-hook)

(require 'git)

(global-set-key [f3] 'ido-switch-buffer)
(global-set-key [f4] 'bury-buffer)
(global-set-key [f7] 'next-error)
(global-set-key [(S-f7)] 'previous-error)
(global-set-key (kbd "C-c o") 'occur)

(setq special-display-regexps (remove "[ ]?\\*[hH]elp.*" special-display-regexps))

(setq ns-function-modifier 'hyper)


(when (fboundp 'winner-mode)
  (winner-mode 1))

;;Aquamacs specific
(when (featurep 'aquamacs)
  (global-unset-key (kbd "M-`"))
  (global-set-key (kbd "M-`") 'raise-next-frame))

(add-hook 'ediff-load-hook
	  (lambda ()
	    
	    (add-hook 'ediff-before-setup-hook
		      (lambda ()
			(setq ediff-saved-window-configuration (current-window-configuration))))
	    
	    (let ((restore-window-configuration
		   (lambda ()
		     (set-window-configuration ediff-saved-window-configuration))))
	      (add-hook 'ediff-quit-hook restore-window-configuration 'append)
	      (add-hook 'ediff-suspend-hook restore-window-configuration 'append))))

(load-file "~/.emacs.d/elisp/sourcepair.el")
(define-key global-map "\M-s" 'sourcepair-load)

(require 'xcscope)

(define-key global-map [(control f3)]  'cscope-set-initial-directory)
(define-key global-map [(control f4)]  'cscope-unset-initial-directory)
(define-key global-map [(control f5)]  'cscope-find-this-symbol)
(define-key global-map [(control f6)]  'cscope-find-global-definition)
(define-key global-map [(control f7)] 'cscope-find-global-definition-no-prompting)
(define-key global-map [(control f8)]  'cscope-pop-mark)
(define-key global-map [(control f9)]  'cscope-next-symbol)
(define-key global-map [(control f10)] 'cscope-next-file)
(define-key global-map [(control f11)] 'cscope-prev-symbol)
(define-key global-map [(control f12)] 'cscope-prev-file)
(define-key global-map [(meta f9)]  'cscope-display-buffer)
(defin-ekey global-map [(meta f10)] 'cscope-display-buffer-toggle)