;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
(setq +latex-viewers '(pdf-tools))

;;org gtd
(setq org-agenda-files '("/mnt/d/Home/Works/Git/git_cloud/Alaric_GTD_org/"))

(setq-default org-agenda-dir "/mnt/d/Home/Works/Git/git_cloud/Alaric_GTD_org/")

(add-hook! 'org-capture-mode-hook
  (defun +org--restart-mode-maybe-h ()
    (when (memq #'+org--restart-mode-h doom-switch-buffer-hook)
      (+org--restart-mode-h))))

(after! org
  (advice-remove #'make-indirect-buffer #'+org--restart-mode-before-indirect-buffer-a)
  (defadvice! +org--restart-mode-before-indirect-buffer-a (&optional buffer _)
    :before #'org-capture-get-indirect-buffer
    (with-current-buffer (or buffer (current-buffer))
      (when (memq #'+org--restart-mode-h doom-switch-buffer-hook)
        (+org--restart-mode-h)))))

(after! org
  (defadvice! dan/org-capture-prevent-restart (fn &rest args)
    :around #'+org--restart-mode-h
    (unless (bound-and-true-p org-capture-mode)
      (apply fn args)))
  (add-hook! 'org-capture-after-finalize-hook
             (let ((buffer (org-capture-get :buffer)))
               (when (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (+org--restart-mode-h)))))
  (setq org-todo-keywords
    '((sequence "BUG(b!)" "|" "FIXED(f!)")
      (sequence "TODO(t!)" "SOMEDAY(s)" "|" "DONE(d!)" "CANCELED(c @/!)")
     ))
;; 这边就是为路径赋值
  (defvar org-agenda-dir "" "gtd org files location")
  (setq org-agenda-file-inbox (expand-file-name "inbox.org" org-agenda-dir))
  (setq org-agenda-file-someday (expand-file-name "someday.org" org-agenda-dir))
  (setq org-agenda-file-note (expand-file-name "notes.org" org-agenda-dir))
  (setq org-agenda-file-task (expand-file-name "task.org" org-agenda-dir))
  (setq org-agenda-file-calendar (expand-file-name "calendar.org" org-agenda-dir))
  (setq org-agenda-file-finished (expand-file-name "finished.org" org-agenda-dir))
  (setq org-agenda-file-canceled (expand-file-name "canceled.org" org-agenda-dir))


;; 添加每次打开时可添加的任务类型
  (setq org-capture-templates
        '(
          ("t" "Todo [inbox]" entry (file+headline org-agenda-file-inbox "Inbox")
           "* TODO  %i%?"
           :empty-lines 1)
          ;;("x" "Todo" entry (file+headline org-agenda-file-task "Task")
          ;; "* TODO [#B] %?\n  %i\n"
          ;; :empty-lines 1)
          ("l" "Tolearn" entry (file+headline org-agenda-file-someday "Learning")
           "* TODO  %?\n  %i\n"
           :empty-lines 1)
          ("h" "Toplay" entry (file+headline org-agenda-file-someday "Hobbies")
           "* TODO  %?\n  %i\n"
           :empty-lines 1)
          ("o" "Todo_others" entry (file+headline org-agenda-file-task "Others")
           "* TODO  %?\n  %i\n"
           :empty-lines 1)
          ("n" "notes" entry (file+headline org-agenda-file-note "Quick notes")
           "* %?\n  %i\n %U"
           :empty-lines 1)
          ("i" "ideas" entry (file+headline org-agenda-file-note "Quick ideas")
           "*  %i\n "
           :empty-lines 1)
          )
        )

;; 绑定键位
  (define-key global-map "\C-cr" 'org-refile)
;; 添加finished和canceled两个文件路径，并且只转移到一级标题
  (setq org-refile-targets  '((org-agenda-file-finished :maxlevel . 1)
                             (org-agenda-file-canceled :maxlevel . 1)
                             (org-agenda-file-task :maxlevel . 3)
                             (org-agenda-file-someday :maxlevel . 3)
                             (org-agenda-file-calendar :maxlevel . 3)
                             ))
)
