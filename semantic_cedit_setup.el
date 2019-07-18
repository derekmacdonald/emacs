;;!dcm! -- this really doesn't work that well. If there isn't an easy to use option by now using an IDE with emacs bindings is probably the best approach
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
;;(add-to-list 'semantic-default-submodes'global-semantic-show-parser-state-mode)

;; Activate semantic
(semantic-mode 1)

;;(require 'semantic/bovine/c)
;;(require 'semantic/bovine/clang)

;;(require 'cedet-files)
