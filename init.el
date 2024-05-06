;; ~/.emacs.d/init.el
;;
(require 'package)

;; add source
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
;; org-mode
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
;; init package
(package-initialize)

;;Config theme
(setq default-directory "~/")
(display-time-mode 1) ;;
(setq display-time-24hr-format t) ;;
(setq display-time-day-and-date t) ;;display time
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(setq inhibit-startup-message t)
(global-hl-line-mode 1)
(global-display-line-numbers-mode t)
;;Install user-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package zenburn-theme
     :config
     (load-theme 'zenburn t))
(use-package telephone-line
   :ensure t
   :config
    (telephone-line-mode 1)
    (setq telephone-line-primary-left-separator 'telephone-line-cubed-left
      telephone-line-secondary-left-separator 'telephone-line-cubed-hollow-left
      telephone-line-primary-right-separator 'telephone-line-cubed-right
      telephone-line-secondary-right-separator 'telephone-line-cubed-hollow-right)
(setq telephone-line-height 24
      telephone-line-evil-use-short-tag t))

;;;;
;;;Emacs Application Framworks
(use-package eaf
  :load-path "~/.emacs.d/site-lisp/emacs-application-framework/"
  :custom
  ; See https://github.com/emacs-eaf/emacs-application-framework/wiki/Customization
  (setq browse-url-browser-function 'eaf-open-browser)
  :init
  (evil-set-initial-state 'eaf-mode 'emacs) ; Evil mode doesn't work well with eaf keybindings.
  :config
  (defalias 'browse-web #'eaf-open-browser)
  ) ;; unbind, see more in the Wiki
;(require 'eaf-browser)
;(require 'eaf-pdf-viewer)
;(require 'eaf-org-previewer)
(use-package eaf-browser
  :load-path "~/.emacs.d/site-lisp/emacs-application-framework/app/browser"
  :custom
  (eaf-browser-continue-where-left-off t)
  (eaf-browser-enable-adblocker t)
  (eaf-bind-key nil "M-q" eaf-browser-keybinding)
  )
(use-package eaf-pdf-viewer
  :load-path "~/.emacs.d/site-lisp/emacs-application-framework/app/pdf-viewer"
  :custom
  (eaf-bind-key scroll_p "C-n" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)

  )
(use-package eaf-org-previewer
  :load-path "~/.emacs.d/site-lisp/emacs-application-framework/app/org-viewer"
  )
;;;evil
(use-package evil
  :ensure t
  :config
  (evil-mode)
  (evil-set-undo-system 'undo-tree))

;;;undo-tree
(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode 1))
;;;auto-save
;(use-package auto-save
;  :load-path "~/.emacs.d/site-lisp/auto-save"
;  :config
;  (auto-save-enable)

;  (setq auto-save-silent t)   ; quietly save
;  (setq auto-save-delete-trailing-whitespace t)  ; automatically delete spaces at the end of the line when saving

;;; custom predicates if you don't want auto save.
;;; disable auto save mode when current filetype is an gpg file.
;  (setq auto-save-disable-predicates
;      '((lambda ()
;      (string-suffix-p
;      "gpg"
;      (file-name-extension (buffer-name)) t))))
;)

;;icons
(use-package all-the-icons
  :if (display-graphic-p))
;;neotree
(use-package neotree
	    :ensure t
	    :config
	    (global-set-key [f8] 'neotree-toggle)
	    (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
	    (setq-default neo-autorefresh t)
	    (add-hook 'neotree-mode-hook
              (lambda ()
                (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
                (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-quick-look)
                (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
                (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)
                (define-key evil-normal-state-local-map (kbd "g") 'neotree-refresh)
                (define-key evil-normal-state-local-map (kbd "n") 'neotree-next-line)
                (define-key evil-normal-state-local-map (kbd "p") 'neotree-previous-line)
                (define-key evil-normal-state-local-map (kbd "A") 'neotree-stretch-toggle)
                (define-key evil-normal-state-local-map (kbd "H") 'neotree-hidden-file-toggle)))
	    (setq neo-window-fixed-size nil)
;; Set the neo-window-width to the current width of the
  ;; neotree window, to trick neotree into resetting the
  ;; width back to the actual window width.
	    (eval-after-load "neotree"
		'(add-to-list 'window-size-change-functions
                  (lambda (frame)
                    (let ((neo-window (neo-global--get-window)))
                      (unless (null neo-window)
                        (setq neo-window-width (window-width neo-window)))))))
	    )
	      ;;;window management
;(use-package edwina
;  :ensure t
;  :config
;  (setq display-buffer-base-action '(display-buffer-below-selected))
;  (edwina-setup-dwm-keys)
;  (edwina-mode 1))
;  (add-to-list 'display-buffer-alist
;                    `(,(rx bos "*helm" (* not-newline) "*" eos)
;                         (display-buffer-in-side-window)
;                         (inhibit-same-window . t)
;                        (window-height . 0.2)))
(use-package winum
  :ensure t
  :config
  (winum-mode))

;;;projectile
(use-package projectile
  :config
  ;; move cache to ~/.emacs.d/.cache/
  (setq projectile-cache-file (expand-file-name ".cache/projectile.cache" user-emacs-directory))
  ;;enable minor mode
  (projectile-mode 1)
  ;;leader key
  (define-key projectile-mode-map (kbd "C-c C-p") 'projectile-command-map)
  (setq projectile-project-search-path '("D:/Home/Works/" )))

(use-package helm-projectile
  :if (functionp 'helm) ;;
  :config
  (helm-projectile-on))

;;;git
(use-package magit)

;;;Latex
(use-package auctex
  :ensure t
  :defer t
  :config
   (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
       TeX-source-correlate-start-server t)
   (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
   )
;;;pdf-tools
(use-package pdf-tools
  :ensure t
  :pin manual
  :config
  (pdf-tools-install)  ; Standard activation command
  (pdf-loader-install) ; On demand loading, leads to faster startup time
   (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
       TeX-source-correlate-start-server t)
   (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
    (add-hook 'pdf-view-mode-hook (lambda () (display-line-numbers-mode 0)))
    )
;;;org
;; Outline-based notes management and organizer
(use-package org
  :defer t
  :config
  (setq org-adapt-indentation t
        org-hide-leading-stars t
        org-hide-emphasis-markers t
        org-pretty-entities t
        org-src-fontify-natively t
        org-startup-folded t
        org-edit-src-content-indentation 0)
  (org-babel-do-load-languages
        'org-babel-load-languages
        '((python . t)))
  )
;;;org bullet
(use-package org-superstar
  :config
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))
  (with-eval-after-load 'org-superstar
    (setq org-superstar-item-bullet-alist
        '((?* . ?•)
          (?+ . ?➤)
          (?- . ?•)))
  (setq org-superstar-headline-bullets-list '(?\d))
  (setq org-superstar-special-todo-items t)
  (setq org-superstar-remove-leading-stars t)
  (setq org-hide-leading-stars t)
  ;; Enable custom bullets for TODO items
  (setq org-superstar-todo-bullet-alist
        '(("TODO" . ?☐)
          ("NEXT" . ?✒)
          ("HOLD" . ?✰)
          ("WAITING" . ?☕)
          ("CANCELLED" . ?✘)
          ("DONE" . ?✔)))
  (org-superstar-restart))
  (setq org-ellipsis " ▼ ")
  )

;;
(use-package org-preview 
  :load-path "~/.emacs.d/pkg")
;;;preview org
(use-package org-preview-html
  :ensure t
  :config
  (setq org-preview-html-refresh-configuration 'save))
;;;pyvenv
(use-package conda
  :ensure t
  :init
  (setq conda-anaconda-home (expand-file-name "C:/Python/anaconda3"))
  (setq conda-env-home-directory (expand-file-name "C:/Python/anaconda3")))
;(use-package conda
;  :config (progn
;            (conda-env-initialize-interactive-shells)
;            (conda-env-initialize-eshell)
            ;(conda-env-autoactivate-mode t)
            ;(setq conda-env-home-directory (expand-file-name "~/.conda/"))
;            (custom-set-variables '(conda-anaconda-home "C:/Python/anaconda3/"))))

;;;keep on bottom
;;auto completion for emacs
(use-package helm
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files))
  :config
  ;; Helm minor mode
  (helm-mode 1)
;  (setq helm-display-function 'helm-display-buffer-in-own-frame
;        helm-display-buffer-reuse-frame t
					;      helm-use-undecorated-frame-option t)
  (setq helm-always-two-windows 0)
  (defvar spacemacs-helm-display-help-buffer-regexp '("\\*.*Helm.*Help.*\\*"))
  (defvar spacemacs-helm-display-buffer-regexp `("\\*.*helm.*\\*"
                                               (display-buffer-in-side-window)
                                               (inhibit-same-window . nil)
                                               (side . bottom)
                                               (window-width . 0.6)
                                               (window-height . 0.4)))

  (defun display-helm-at-bottom (buffer &optional _resume)
  (let ((display-buffer-alist (list spacemacs-helm-display-help-buffer-regexp
                                    spacemacs-helm-display-buffer-regexp)))
    (display-buffer buffer)))
  (setq helm-display-function 'display-helm-at-bottom)
  )


;;; move customize-set-variable out of init.el
(setq custom-file "~/.emacs.d/custom.el")
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(load custom-file)
