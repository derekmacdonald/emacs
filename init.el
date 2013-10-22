;;CEDET needs to be at the top to over-write the built in version
;;(load "~/.emacs.d/ottconfig/rc/emacs-rc-cedet.el")

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

(push #'load-persistent-scratch after-init-hook)
(push #'save-persistent-scratch kill-emacs-hook)

(run-with-idle-timer 60 t 'save-persistent-scratch)
;;!dcm! -- clean this up
(provide 'persistent-scratch)
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 50)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)




(add-to-list 'load-path "~/.emacs.d/elisp/")
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/")
(add-to-list 'load-path "~/.emacs.d/")

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
(define-key global-map [(meta f10)] 'cscope-display-buffer-toggle)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(menu-bar-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Buffer-menu-buffer ((t (:weight bold))) t))

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
