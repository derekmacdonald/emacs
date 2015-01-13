(cond ((featurep 'aquamacs) (setq custom-file "~/.emacs.d/custom_aquamacs.el"))
      ((eq system-type 'darwin) (setq custom-file "~/.emacs.d/custom_osx.el"))
      ( t  (setq custom-file "~/.emacs.d/custom.el")))

(load custom-file)

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)

;;CEDET needs to be at the top to over-write the built in version
;;; emacs-rc-cedet.el ---
;; (require 'eieio) 
;; (load-file "~/projects/cedet/common/cedet.el")
;; (semantic-load-enable-excessive-code-helpers)
;;(load-file "~/projects/cedet/cedet-devel-load.el")
;;(load-file "~/projects/cedet/contrib/cedet-contrib-load.el")
;;(add-to-list 'load-path "~/projects/cedet/contrib/")
;;!dcm!(add-to-list  'Info-directory-list "~/projects/cedet-bzr/doc/info")

(add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-mru-bookmark-mode)
(add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
;(add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
;;(add-to-list 'semantic-default-submodes 'global-cedet-m3-minor-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-highlight-func-mode)
;;(add-to-list 'semantic-default-submodes 'global-semantic-show-unmatched-syntax-mode)
;;(add-to-list 'semantic-default-submodes 'global-semantic-highlight-edits-mode)
;;(add-to-list 'semantic-default-submodes 'global-semantic-show-parser-state-mode)

;; Activate semantic
(semantic-mode 1)

;;(require 'semantic/bovine/c)
;;(require 'semantic/bovine/clang)

;;(require 'cedet-files)

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

(load-file "~/.emacs.d/lisp/sourcepair.el")
(define-key global-map "\M-s" 'sourcepair-load)

(require 'xcscope)

(define-key cscope:map [(control f3)]  'cscope-find-functions-calling-this-function)
(define-key cscope:map [(control f4)]  'cscope-find-called-functions)
(define-key global-map [(control f3)]  'cscope-set-initial-directory)
(define-key global-map [(control f4)]  'cscope-unset-initial-directory)
(define-key global-map [(control f5)]  'cscope-find-this-symbol)
(define-key global-map [(control f6)]  'cscope-find-global-definition)
(define-key global-map [(control f7)]  'cscope-find-global-definition-no-prompting)
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

(defun my-markdown-mode-hook ()
  (define-key markdown-mode-map (kbd "A-j") 'markdown-jump)
  (define-key markdown-mode-map (kbd "A-k") 'markdown-kill-thing-at-point)
  (define-key markdown-mode-map (kbd "A-n") 'outline-next-visible-heading)
  (define-key markdown-mode-map (kbd "A-o") 'markdown-follow-thing-at-point)
  (define-key markdown-mode-map (kbd "A-p") 'outline-previous-visible-heading)
;;  (define-key markdown-mode-map (kbd "A-s") 'Prefix Command)
;;  (define-key markdown-mode-map (kbd "A-t") 'Prefix Command)
  (define-key markdown-mode-map (kbd "A-u") 'outline-up-heading)
;;  (define-key markdown-mode-map (kbd "A-x") 'Prefix Command)
  (define-key markdown-mode-map (kbd "A-]") 'markdown-complete)
  (define-key markdown-mode-map (kbd "A-_") 'markdown-insert-hr)
  (define-key markdown-mode-map (kbd "A-<") 'markdown-exdent-region)
  (define-key markdown-mode-map (kbd "A->") 'markdown-indent-region)
  (define-key markdown-mode-map (kbd "A--") 'markdown-promote)
  (define-key markdown-mode-map (kbd "A-=") 'markdown-demote)
  (define-key markdown-mode-map (kbd "M-n") 'markdown-next-link)
  (define-key markdown-mode-map (kbd "M-p") 'markdown-previous-link)
  (define-key markdown-mode-map (kbd "M-{") 'markdown-backward-paragraph)
  (define-key markdown-mode-map (kbd "M-}") 'markdown-forward-paragraph)
  (define-key markdown-mode-map (kbd "C-M-i") 'ispell-complete-word)
  (define-key markdown-mode-map (kbd "A-x d") 'markdown-move-down)
  (define-key markdown-mode-map (kbd "A-x l") 'markdown-promote)
  (define-key markdown-mode-map (kbd "A-x m") 'markdown-insert-list-item)
  (define-key markdown-mode-map (kbd "A-x r") 'markdown-demote)
  (define-key markdown-mode-map (kbd "A-x u") 'markdown-move-up)
  (define-key markdown-mode-map (kbd "A-c ]") 'markdown-complete-buffer)
  (define-key markdown-mode-map (kbd "A-c c") 'markdown-check-refs)
  (define-key markdown-mode-map (kbd "A-c e") 'markdown-export)
  (define-key markdown-mode-map (kbd "A-c m") 'markdown-other-window)
  (define-key markdown-mode-map (kbd "A-c n") 'markdown-cleanup-list-numbers)
  (define-key markdown-mode-map (kbd "A-c o") 'markdown-open)
  (define-key markdown-mode-map (kbd "A-c p") 'markdown-preview)
  (define-key markdown-mode-map (kbd "A-c v") 'markdown-export-and-preview)
  (define-key markdown-mode-map (kbd "A-c w") 'markdown-kill-ring-save)
  (define-key markdown-mode-map (kbd "A-s C-b") 'markdown-blockquote-region)
  (define-key markdown-mode-map (kbd "A-s C-p") 'markdown-pre-region)
  (define-key markdown-mode-map (kbd "A-s b") 'markdown-insert-blockquote)
  (define-key markdown-mode-map (kbd "A-s c") 'markdown-insert-code)
  (define-key markdown-mode-map (kbd "A-s e") 'markdown-insert-italic)
  (define-key markdown-mode-map (kbd "A-s p") 'markdown-insert-pre)
  (define-key markdown-mode-map (kbd "A-s s") 'markdown-insert-bold)
  (define-key markdown-mode-map (kbd "A-t !") 'markdown-insert-header-setext-1)
  (define-key markdown-mode-map (kbd "A-t 1") 'markdown-insert-header-atx-1)
  (define-key markdown-mode-map (kbd "A-t 2") 'markdown-insert-header-atx-2)
  (define-key markdown-mode-map (kbd "A-t 3") 'markdown-insert-header-atx-3)
  (define-key markdown-mode-map (kbd "A-t 4") 'markdown-insert-header-atx-4)
  (define-key markdown-mode-map (kbd "A-t 5") 'markdown-insert-header-atx-5)
  (define-key markdown-mode-map (kbd "A-t 6") 'markdown-insert-header-atx-6)
  (define-key markdown-mode-map (kbd "A-t @") 'markdown-insert-header-setext-2)
  (define-key markdown-mode-map (kbd "A-t H") 'markdown-insert-header-setext-dwim)
  (define-key markdown-mode-map (kbd "A-t h") 'markdown-insert-header-dwim)
  (define-key markdown-mode-map (kbd "A-t s") 'markdown-insert-header-setext-2)
  (define-key markdown-mode-map (kbd "A-t t") 'markdown-insert-header-setext-1)
  (define-key markdown-mode-map (kbd "A-TAB I") 'markdown-insert-reference-image)
  (define-key markdown-mode-map (kbd "A-TAB i") 'markdown-insert-image)
  (define-key markdown-mode-map (kbd "A-a l") 'markdown-insert-reference-link-dwim)
  (define-key markdown-mode-map (kbd "A-a f") 'markdown-insert-footnote)
  (define-key markdown-mode-map (kbd "A-a l") 'markdown-insert-link)
  (define-key markdown-mode-map (kbd "A-a r") 'markdown-insert-reference-link-dwim)
  (define-key markdown-mode-map (kbd "A-a u") 'markdown-insert-uri)
  (define-key markdown-mode-map (kbd "A-a w") 'markdown-insert-wiki-link))

(add-hook 'markdown-mode-hook
          'my-markdown-mode-hook)
