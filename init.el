(cond ((featurep 'aquamacs) (setq custom-file "~/.emacs.d/custom_aquamacs.el"))
      ((eq system-type 'darwin) (setq custom-file "~/.emacs.d/custom_osx.el"))
      ((eq system-type 'windows-nt) (setq custom-file "~/.emacs.d/custom_windows.el"))
      ( t  (setq custom-file "~/.emacs.d/custom.el")))

(load custom-file)

(if (equal system-type 'windows-nt)
    (progn (prefer-coding-system 'utf-8)
	   (setq explicit-shell-file-name "C:/Program Files/Git/bin/bash.exe")
	   (setq explicit-bash.exe-args '("--noediting" "--login" "-i"))
	   (setq shell-file-name explicit-shell-file-name)
	   (add-to-list 'exec-path "C:/Program Files/Git/bin")
	   (add-to-list 'exec-path "C:/Program Files/Git/usr/bin")))

(require 'package)

(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(defgroup persistent-scratch nil
  "Save scratch buffer between sessions"
  :group 'initialization)

(defcustom persistent-scratch-file-name "~/.emacs-persistent-scratch"
  "Location of *scratch* file contents for persistent-scratch."
  :type 'directory
  :group 'persistent-scratch)

(defun save-persistent-scratch ()
  "Write the contents of *scratch* to the file name
`persistent-scratch-file-name'."
  (with-current-buffer (get-buffer-create "*scratch*")
    (write-region (point-min) (point-max) persistent-scratch-file-name)))

(defun load-persistent-scratch ()
  "Load the contents of `persistent-scratch-file-name' into the
scratch buffer, clearing its contents first."
  (if (file-exists-p persistent-scratch-file-name)
      (with-current-buffer (get-buffer "*scratch*")
        (delete-region (point-min) (point-max))
        (insert-file-contents persistent-scratch-file-name))))

;;?dcm? -- WTF does # do in this context?
(push #'load-persistent-scratch after-init-hook)
(push #'save-persistent-scratch kill-emacs-hook)

(run-with-idle-timer 60 t 'save-persistent-scratch)
;;!dcm! -- clean this up
(provide 'persistent-scratch)
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 50)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)




;;(add-to-list 'load-path "~/.emacs.d/elisp/")
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/")
(add-to-list 'load-path "~/.emacs.d/lisp")

;;(load "~/.emacs.d/ottconfig/rc/emacs-rc-cedet.el")

(require 'ido)
(ido-mode t)

(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)

;; (defun my-semantic-hook ()
;;   (imenu-add-to-menubar "TAGS"))
;; (add-hook 'semantic-init-hooks 'my-semantic-hook)

;; (defun my-c-mode-cedet-hook ()
;;   (add-to-list 'ac-sources 'ac-source-gtags)
;;   (add-to-list 'ac-sources 'ac-source-semantic))
;; (add-hook 'c-mode-common-hook 'my-c-mode-cedet-hook)

(require 'git)

(global-set-key [f2] 'other-window)
(global-set-key [(M-f2)] '2C-command) 
(global-set-key [f3] 'ido-switch-buffer)
(global-set-key [f4] 'bury-buffer)
(global-set-key [(C-f4)] 'ido-kill-buffer)
(global-set-key [f7] 'next-error)
(global-set-key [(M-f7)] 'previous-error)
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

(load-file "~/.emacs.d/lisp/sourcepair.el")
(define-key global-map "\M-s" 'sourcepair-load)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-delay 0.05)
 '(c-offsets-alist (quote ((innamespace . 0))))
 '(menu-bar-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(buffer-menu-buffer ((t (:weight bold)))))

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

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)


;;c++ mode setup
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

(defun iwb ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))


(add-hook 'c-mode-common-hook
  (lambda()
    (local-set-key [(control tab)] 'hs-toggle-hiding)
    ;; (local-set-key (kbd "C-c <left>")  'hs-hide-block)
    ;; (local-set-key (kbd "C-c <up>")    'hs-hide-all)
    ;; (local-set-key (kbd "C-c <down>")  'hs-show-all)
    (hs-minor-mode t)))

;;org-mode stuff
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

;; predictive install location
;;(add-to-list 'load-path "~/.emacs.d/predictive/")
;; dictionary locations
;;(add-to-list 'load-path "~/.emacs.d/predictive/latex/")
;;(add-to-list 'load-path "~/.emacs.d/predictive/texinfo/")
;;(add-to-list 'load-path "~/.emacs.d/predictive/html/")
;; load predictive package
;;(require 'predictive)
(put 'narrow-to-region 'disabled nil)

