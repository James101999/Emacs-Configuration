(defvar my-term-shell "/bin/bash")
(defadvice ansi-term (before force-bash)
  (interactive (list my-term-shell)))
(ad-activate 'ansi-term)

(defun config-visit ()
  (interactive)
  (find-file "~/.emacs.d/config.org"))
(global-set-key (kbd "C-c e") 'config-visit)

(defun config-reload ()
  (interactive)
  (org-babel-load-file (expand-file-name "~/.emacs.d/config.org")))
(global-set-key (kbd "C-c r") 'config-reload)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(use-package sudo-edit
  :ensure t
  :bind ("s-e" . 'sudo-edit))

;; The following lines are always needed.Choose your own keys.
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-switchb)

;; Switches code to current buffer when typing code
(setq org-src-window-setup 'current-window)

;; Snippets
;; Emacs-Lisp
(add-to-list 'org-structure-template-alist
	     '("el" . "src emacs-lisp"))

(add-hook 'org-mode-hook 'org-indent-mode)

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode))))

(setq ido-enable-files-matching nil)
(setq ido-create-new-buffer 'always)
(setq ido-everywhere t)
(ido-mode 1)

(use-package ido-vertical-mode
  :ensure t
  :init
  (ido-vertical-mode 1))
(setq ido-vertical-define-keys 'C-n-and-C-p-only)

(use-package smex
  :ensure t
  :init  (smex-initialize)
  :bind  ( "M-x" .  'smex))

(global-set-key (kbd "C-x C-b")  'ido-switch-buffer)

;; Centaur Tabs
(use-package centaur-tabs
  :ensure t
  :init
  (centaur-tabs-mode))

;; which-key
(use-package which-key
  :ensure t
  :init
  (which-key-mode))

(use-package beacon
  :ensure t
  :init
  (beacon-mode 1))

(use-package rainbow-mode
  :ensure t
  :init (add-hook 'prog-mode-hook 'rainbow-mode))

(use-package rainbow-delimiters
  :ensure t
  :init (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package switch-window
  :ensure t
  :config
  (setq switch-window-input-style 'minibuffer)
  (setq switch-window-increase 4)
  (setq switch-window-threshold 2)
  (setq switch-window-shortcut-style 'qwerty)
  (setq switch-window-qwerty-shortcuts
	'("a" "s" "d" "f" "h" "j" "k" "l"))
  :bind
  ([remap other-window] . 'switch-window))

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((recents . 10)))
  (setq dashboard-banner-logo-title "Hello!"))

(use-package symon
  :ensure t
  :bind
  ("s-j" . 'symon-mode))

(use-package spaceline
  :ensure t
  :config
  (require 'spaceline-config)
  (setq powerline-default-separator (quote arrow))
  (spaceline-spacemacs-theme))

(use-package dmenu
  :ensure t
  :bind
  ("s-f" . ' dmenu))

(setq display-time-24hr-format t)
(display-time-mode 1)

(line-number-mode 1)
(column-number-mode 1)

;; Annoying toolbars
(menu-bar-mode -1)
(tool-bar-mode -1)

(defun split-and-follow-horizontally ()
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)

(defun split-and-follow-vertically ()
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 3") 'split-and-follow-vertically)

;; Disable annoying startup
(setq inhibit-startup-message t)

;; Yes or No
(defalias 'yes-or-no-p 'y-or-n-p)

;; Scrolling
(setq scroll-conservatively 100)

;; Disable annoying bell
(setq ring-bell-function 'ignore)

;; Highlight Cursor
;; Set it to work on GUI Emacs only
(when window-system (global-hl-line-mode t))

;; Prettify symbol
(when window-system (global-prettify-symbols-mode t))

(global-set-key (kbd "C-x b")  'ibuffer)

(use-package avy
  :ensure t    
  :init
  :bind ( "M-s" .  'avy-goto-char))

(use-package hungry-delete
  :ensure t
  :config
  (global-hungry-delete-mode))

(defun kill-whole-word ()
  (interactive)
  (backward-word)
  (kill-word 1))
(global-set-key (kbd "C-c w w")  'kill-whole-word)

(defun copy-whole-line ()
  (interactive)
  (save-excursion
    (kill-new
     (buffer-substring
      (point-at-bol)
       (point-at-eol)))))
(global-set-key (kbd "C-c w l") 'copy-whole-line)

(global-subword-mode 1)

(setq electric-pair-pairs '(
			    (?\(  .  ?\))
			    (?\[ . ?\])
			    (?\{ . ?\})))
(electric-pair-mode t)

(use-package popup-kill-ring
  :ensure t
  :bind ("M-y" . 'popup-kill-ring))

(use-package multiple-cursors
  :ensure t
  :bind ("C-S-c C-S-c" . 'mc/edit-lines)
	     ("C->" . 'mc/mark-next-like-this)
	     ("C-<" . 'mc/mark-previous-like-this)
	     ("C-c q" . 'mc/mark-all-like-this))

(use-package expand-region
  :ensure t
  :bind ("C-q" . 'er/expand-region))

(load
   "~/.opam/4.08.1/share/emacs/site-lisp/tuareg-site-file.el")
(load
 "~/.opam/4.08.1/share/emacs/site-lisp/ocp-indent.el")
(let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var"
						     "share")))))
  (when (and opam-share (file-directory-p opam-share))
    ;; Register Merlin
    (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
    (autoload 'merlin-mode "merlin" nil t nil)
    ;; Automatically start it in OCaml buffers
    (add-hook 'tuareg-mode-hook 'merlin-mode t)
    (add-hook 'caml-mode-hook 'merlin-mode t)
    (push "/home/james/.opam/4.08.1/share/emacs/site-lisp" load-path)
    ;; Use opam switch to lookup ocamlmerlin binary
    (setq merlin-command "/home/james/.opam/4.08.1/bin/ocamlmerlin")))
;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line

(load (expand-file-name "~/quicklisp/slime-helper.el"))
;; Default Common Lisp
(setq inferior-lisp-program "/usr/bin/sbcl")
(setq slime-contribs '(slime-fancy)) ; almost everything

(load-file (let ((coding-system-for-read 'utf-8))
		(shell-command-to-string "agda-mode locate")))

;; Web Mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . web-mode))

(setq org-latex-create-formula-image-program 'dvipng)

;; pdf-tools
(use-package pdf-tools
  :ensure t
  :init
  (pdf-tools-install))

;; saveplace-pdf-tools
(require 'saveplace-pdf-view)
(save-place-mode 1)

;; Hook LaTex with pdf-tools
;; (server-start)
(setq TeX-view-program-selection '((output-pdf "PDF Tools")))

;; Syncs the pdf
(setq TeX-source-correlate-mode t)

;; to have the buffer refresh after compilation
 (add-hook 'TeX-after-compilation-finished-functions
	#'TeX-revert-document-buffer)

;; Enable elpy
(use-package elpy
  :ensure t
  :init
  (elpy-enable))
;; For elpy
;;(setq elpy-rpc-python-command "python3")
;; For interactive shell
(setq python-shell-interpreter "python"
 python-shell-interpreter-args "-i")

;; Company Mode
(use-package company
  :ensure t
  :config
  (setq company-idle-delay nil)
  ;; (setq company-minimum-prefix-length 3)
  (setq company-begin-commands '(self-insert-command))
  (setq company-require-match nil)
  (add-hook 'after-init-hook 'global-company-mode))
  (global-set-key (kbd "<C-tab>")  #'company-complete)
(with-eval-after-load 'company
  (define-key company-active-map (kbd "M-n")  nil)
  (define-key company-active-map (kbd "M-p")  nil)
  (define-key company-active-map (kbd "SPC") nil)
  (define-key company-active-map (kbd "C-n")  #'company-select-next)
  (define-key company-active-map (kbd "C-p")  #'company-select-previous)
  (define-key company-active-map (kbd "SPC") #'company-abort))

(use-package company-irony
  :ensure t
  :config
  (require 'company)
  (add-to-list 'company-backends 'company-irony))

(use-package company-c-headers
  :ensure t
  :config
  (require 'company)
  (add-to-list 'company-backends 'company-c-headers))

(use-package irony
  :ensure t
  :config
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(with-eval-after-load 'company
  (add-hook 'c++-mode-hook 'company-mode)
  (add-hook 'c-mode-hook 'company-mode))

;; Open .v files with Proof-General's coq-mode
(require 'proof-site "~/.emacs.d/lisp/PG/generic/proof-site")

;; Load company-coq when opening Coq files
(add-hook 'coq-mode-hook #'company-coq-mode)

(use-package company-coq
  :ensure t
  :config
  (require 'company)
  (add-hook 'coq-mode-hook #'company-coq-mode))

(use-package company-math
  :ensure t
  :config
  (require 'company)
  ;(add-to-list 'company-backends 'company-math-symbols-unicode)
  (add-to-list 'company-backends 
  (append
  '(company-math-symbols-latex 
  company-auctex-environments
  company-auctex-labels
  company-auctex-macros
  company-auctex-bibs))))
;(use-package company-auctex
 ; :ensure t
;  :config
 ; (company-auctex-init))

(eval-after-load 'flycheck
      '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

(use-package yasnippet
  :ensure t
  :config (use-package yasnippet-snippets
	    :ensure t)
  (require 'yasnippet)
  (yas-reload-all)
  (yas-global-mode 1))

(use-package swiper
  :ensure t
  :bind ("C-s" . 'swiper))
