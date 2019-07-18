;;; Set and load platform specific custom-file
(cond ((featurep 'aquamacs) (setq custom-file "~/.emacs.d/custom_aquamacs.el"))
      ((eq system-type 'darwin) (setq custom-file "~/.emacs.d/custom_osx.el"))
      ((eq system-type 'windows-nt) (setq custom-file "~/.emacs.d/custom_windows.el"))
      ( t  (setq custom-file "~/.emacs.d/custom.el")))

(load custom-file)

;;; Git setup for Windows
;; PS1 setup under Git for Windows relies on ~/.config/git/git-prompt.sh"
;; If that file doesn't exist the prompt will be a mess.
;; .dcm. TODO Check for existence of file and show a warning if it doesn't exist.
(if (equal system-type 'windows-nt)
    (progn (prefer-coding-system 'utf-8)
	   (setq explicit-shell-file-name "C:/Program Files/Git/bin/bash.exe")
	   (setq explicit-bash.exe-args '("--noediting" "--login" "-i"))
	   (setq shell-file-name explicit-shell-file-name)
	   (add-to-list 'exec-path "C:/Program Files/Git/bin")
	   (add-to-list 'exec-path "C:/Program Files/Git/usr/bin")))

;;; Built-in package setup
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 50)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/")
(add-to-list 'load-path "~/.emacs.d/lisp")

(require 'ido)
(ido-mode t)

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;;; Bootstrap straight.el for other packages
;;all use-package invocations will default to having ":straight t"
;;specfied unless overridden by ":straight nil"
(setq straight-use-package-by-default t)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
       'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; install use-package which will be used for package management from now on
(straight-use-package 'use-package)

;;; Install/Register Packages

(use-package auto-complete
  :config 
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
  (ac-config-default))

(use-package magit)
(use-package helpful
  :bind (("C-h v" . helpful-variable)
	 ;; Note that the built-in `describe-function' includes both
	 ;; functions and macros. `helpful-function' is functions
	 ;; only, so we use helpful-callable' to handle both.("C-h f" . helpful-callable)
	 ("C-h k" . helpful-key)
	 ;; Lookup the current symbol at point. C-c C-d is a common
	 ;; keybinding for this in lisp modes.
	 ("C-c C-d" . helpful-at-point)
	 ;; Look up *F*unctions (excludes macros). Shadows
	 ;; `Info-goto-emacs-command-node'. However, helpful already
	 ;; links to the manual if a function is referenced.
	 ("C-h F" . helpful-function)
	 ;; Look up *C*ommands.
	 ;;
	 ;; Only looks at interactive functions. Shadows
	 ;; `describe-coding-system', which feels like an acceptable
	 ;; loss.
	 ("C-h C" . helpful-command)))

;;; Custom keybinds
(global-set-key [f2] 'other-window)
(global-set-key [(M-f2)] '2C-command) 
(global-set-key [f3] 'ido-switch-buffer)
(global-set-key [(C-f3)] 'ido-find-file)
(global-set-key [f4] 'bury-buffer)
(global-set-key [(C-f4)] 'ido-kill-buffer)
(global-set-key [f7] 'next-error)
(global-set-key [(M-f7)] 'previous-error)
(global-set-key (kbd "C-c o") 'occur)
(global-set-key(kbd "C-;") 'comment-region)
(global-set-key(kbd "M-;") 'uncomment-region)

(setq special-display-regexps (remove "[ ]?\\*[hH]elp.*" special-display-regexps))

(when (fboundp 'winner-mode)
  (winner-mode 1))

;;Aquamacs specific
(when (featurep 'aquamacs)
  (global-unset-key (kbd "M-`"))
  (global-set-key (kbd "M-`") 'raise-next-frame))

;; Restore 
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

;;; Custom functions
(require 'tramp)
(defun sudo-edit (&optional arg)
  "Edit currently visited file as root.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun iwb ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(load-file "~/.emacs.d/lisp/sourcepair.el")
(define-key global-map "\M-s" 'sourcepair-load)

;;;c++ mode setup
(c-add-style "resip-style" 
	     '("ellemtel"
	       (indent-tabs-mode . nil)
	       (c-offsets-alist . ((innamespace . [0])))))

(defun my-c++-mode-hook ()
  (c-set-style "resip-style")        ; use my-style defined above
  (auto-fill-mode)         
  (c-toggle-auto-hungry-state 1)
  (set-fill-column 90))

(add-hook 'c++-mode-hook
          'my-c++-mode-hook)

(add-hook 'c-mode-common-hook
  (lambda()
    (local-set-key [(control tab)] 'hs-toggle-hiding)
    ;; (local-set-key (kbd "C-c <left>")  'hs-hide-block)
    ;; (local-set-key (kbd "C-c <up>")    'hs-hide-all)
    ;; (local-set-key (kbd "C-c <down>")  'hs-show-all)
    (hs-minor-mode t)))

;;;org-mode stuff
(defun my-org-mode-hook ()
  (turn-on-auto-fill)
  (set-fill-column 110)
  (setq org-file-apps
	(append '(
		  ("\\.doc\\'" . default)
		  ("\\.docx\\'" . default)
		  ) org-file-apps )))

(require 'org-inlinetask)

(add-hook 'org-mode-hook
          'my-org-mode-hook)

(put 'narrow-to-region 'disabled nil)

