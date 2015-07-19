;; Load package manager
(require 'package)
(dolist (source '(("marmalade" . "http://marmalade-repo.org/packages")
				  ("elpa" . "http://tromey.com/elpa/")
				  ("melpa" . "http://melpa.milkbox.net/packages/")
				  ))
  (add-to-list 'package-archives source t))
(package-initialize)

;; Required packages
(when (not package-archive-contents)
  (package-refresh-contents))
(defvar tmtxt/packages
  '(ssh multi-term solarized-theme xcscope))
(dolist (p tmtxt/packages)
  (when (not (package-installed-p p))
	(package-install p)))

;; Set up xcscope
(require 'xcscope)
(cscope-setup)

;; Set up c mode
(require 'cc-mode)

;; Set up C indents
(setq-default c-basic-offset 4 c-default-style "linux")
(setq-default tab-width 4 indent-tabs-mode t)
(define-key c-mode-base-map (kbd "RET") 'newline-and-indent)

;; Auto pair brackets
(electric-pair-mode +1)

;; Set solarised dark theme
(load-theme 'solarized-dark t)

;; Set up company
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))

;; (optional) adds CC special commands to `company-begin-commands' in order to
;; trigger completion at interesting places, such as after scope operator
;;     std::|
(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)

;; set up irony
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

;; replace the `completion-at-point' and `complete-symbol' bindings in
;; irony-mode's buffers by irony-mode's function
(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))
(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)


;; set up multi-term
(setq multi-term-program "/bin/bash")
(add-hook 'term-mode-hook
		  (lambda ()
			(setq show-trailing-whitespace nil)
			))


(multi-term) ;; open multi-term 1
(multi-term) ;; open multi-term 2
(multi-term) ;; open multi-term 3
(multi-term) ;; open multi-term 4
(global-set-key (kbd "M-1")
				(lambda ()
				  (interactive)
				  (switch-to-buffer "*terminal<1>*")))
(global-set-key (kbd "M-2")
				(lambda ()
				  (interactive)
				  (switch-to-buffer "*terminal<2>*")))
(global-set-key (kbd "M-3")
				(lambda ()
				  (interactive)
				  (switch-to-buffer "*terminal<3>*")))
(global-set-key (kbd "M-4")
				(lambda ()
				  (interactive)
				  (switch-to-buffer "*terminal<4>*")))
