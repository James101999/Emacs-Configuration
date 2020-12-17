;; this is intended for manually installed libraries
(add-to-list 'load-path "~/.opam/4.08.1/share/emacs/site-lisp/site-lisp/")

;; load the package system and add some repositories
(require 'package)

;; disable automatic loading of packages after init.el is done
(setq package-enable-at-startup nil)
;; and force it to happen now
(package-initialize)

;; NOW you can (require) your ELPA packages and configure them as normal
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and
  ;; MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa"
				       (concat proto "://melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu"
					 (concat proto "://elpa.gnu.org/packages/")))))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(unless (package-installed-p 'spacemacs-theme)
  (package-refresh-contents)
  (package-install 'spacemacs-theme))

(unless (package-installed-p 'pdf-tools)
  (package-refresh-contents)
  (package-install 'pdf-tools))

;; Do here basic initialization, (require) non-ELPA packages, etc.

;; ====================================

;; Development Setup

;; ====================================

;;; This is the actual config file. It is omitted if it doesn't exist so emacs won't
;;; refuse to launch.
(when (file-readable-p "~/.emacs.d/config.org")
  (org-babel-load-file (expand-file-name "~/.emacs.d/config.org")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-source-correlate-method (quote synctex))
 '(agda2-backend "")
 '(agda2-program-args nil)
 '(agda2-program-name "agda")
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(c-default-style
   (quote
    ((c-mode . "k&r")
     (java-mode . "java")
     (awk-mode . "awk"))))
 '(company-auto-commit t)
 '(company-auto-commit-chars (quote (32 95 40 41 119 46 34 36 39 60 62 47 124 33)))
 '(company-backends
   (quote
    ((company-math-symbols-latex company-auctex-environments company-auctex-labels company-auctex-macros company-auctex-bibs)
     company-c-headers company-irony merlin-company-backend company-bbdb company-semantic company-cmake company-capf company-clang company-files
     (company-dabbrev-code company-gtags company-etags company-keywords)
     company-oddmuse company-dabbrev)))
 '(company-idle-delay nil)
 '(company-tooltip-align-annotations t)
 '(coq-compile-before-require t)
 '(custom-enabled-themes (quote (spacemacs-dark)))
 '(custom-safe-themes
   (quote
    ("fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default)))
 '(dynamic-completion-mode t)
 '(ein:polymode t)
 '(elpy-project-root t)
 '(elpy-rpc-python-command "python3")
 '(elpy-shell-display-buffer-after-send t)
 '(elpy-shell-starting-directory (quote current-directory))
 '(elpy-shell-use-project-root t)
 '(global-auto-complete-mode nil)
 '(global-display-line-numbers-mode t)
 '(global-semantic-idle-completions-mode t nil (semantic/idle))
 '(line-number-mode t)
 '(merlin-ac-setup t)
 '(merlin-company-everywhere t)
 '(org-babel-load-languages (quote ((emacs-lisp . t) (ocaml . t) (python . t))))
 '(org-babel-python-command "python3")
 '(org-babel-python-mode (quote python))
 '(org-format-latex-options
   (quote
    (:foreground default :background default :scale 2.0 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
		 ("begin" "$1" "$" "$$" "\\(" "\\["))))
 '(org-structure-template-alist
   (quote
    (("el" . "src emacs-lisp")
     ("a" . "export ascii")
     ("c" . "center")
     ("C" . "comment")
     ("e" . "example")
     ("E" . "export")
     ("h" . "export html")
     ("l" . "export latex")
     ("q" . "quote")
     ("s" . "src")
     ("v" . "verse"))))
 '(package-archives
   (quote
    (("melpa" . "https://melpa.org/packages/")
     ("gnu" . "https://elpa.gnu.org/packages/"))))
 '(package-selected-packages
   (quote
    (yasnippet-snippets multiple-cursors expand-region swiper popup-kill-ring symon dmenu spaceline dashboard rainbow-delimiters sudo-edit hungry-delete switch-window rainbow-mode avy smex ido-vertical-mode org-bullets beacon saveplace-pdf-view spacemacs-theme which-key centaur-tabs pdf-tools company-coq company-bibtex cmake-ide haskell-mode pos-tip racket-mode sicp flycheck-rtags company-rtags flycheck-irony cpputils-cmake jedi sage-shell-mode magic-latex-buffer markdown-mode markdown-mode+ ein exec-path-from-shell merlin-eldoc jupyter proof-general company-reftex auctex ob-sml slime-company slime auto-complete company-php company-lsp company-irony company-c-headers company-web org irony utop tuareg merlin gnu-elpa-keyring-update sml-mode)))
 '(preview-transparent-color t)
 '(python-shell-interpreter "python3")
 '(scroll-bar-mode nil)
 '(show-paren-mode t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#292b2e" :foreground "#b2b2b2" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :foundry "DAMA" :family "Ubuntu"))))
 '(org-level-1 ((t (:inherit bold :foreground "#4f97d7" :weight bold :height 1.2))))
 '(org-level-2 ((t (:inherit bold :foreground "#2d9574" :weight bold :height 1.1))))
 '(org-level-3 ((t (:foreground "#67b11d" :weight normal :height 1.0)))))
