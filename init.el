;; Package initialization
(require 'package)

(add-to-list 'package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
	("marmalade" . "http://marmalade-repo.org/packages/")
	("melpa-milkbox" . "http://melpa.milkbox.net/packages/")
    ("melpa" . "https://melpa.org/packages/")
    ("melpa-stable" . "https://stable.melpa.org/packages/")
    )
      t)

(add-to-list 'package-archives
    '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(package-initialize)

(when (not package-archive-contents)
    (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)
(require 'iso-transl)
(require 'windmove) ; For S-<arrow keys> window moving
; Configure S-<arrow keys> for real terminal mode
;(windmove-default-keybindings 'control)
;(define-key input-decode-map "?\x1B[[D" [S-left])
;(define-key input-decode-map "?\x1B[[C" [S-right])
;(define-key input-decode-map "?\e[[A" [S-up])
;(define-key input-decode-map "?\e[[B" [S-down])


(add-to-list 'load-path "~/.emacs.d/custom")
(let ((default-directory  "~/.emacs.d/custom/"))
  (normal-top-level-add-subdirs-to-load-path))

;; Changes behaviour of C-v and M-v and alike
(setq next-screen-context-lines 20)
;; Remove the scrollbar, menu and tool bar
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)

(defun push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region
   Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))

;; Echo keys almost instantly
(setq echo-keystrokes 1.0E-50)

(global-set-key (kbd "C-h C-SPC") 'push-mark-no-activate)

;; Better way to clear eshell
(defun eshell-erase-buffer ()
  "Clear `eshell' buffer, comint-style."
  (interactive)
  (let ((input (eshell-get-old-input)))
    (eshell/clear-scrollback)
    (eshell-emit-prompt)
    (insert input)))

(use-package helm)
(use-package company)
(add-hook 'after-init-hook 'company-mode)
(require 'setup-general)
(require 'setup-helm)
(require 'setup-editing)


(add-hook 'c-mode-hook
          (lambda ()
            (if (version< emacs-version "24.4")
                (require 'setup-helm-gtags))
            ;; (require 'setup-ivy-counsel)
            ;; (require 'setup-ggtags)
            (require 'setup-cedet)))

;; For line numbering
(add-hook 'after-init-hook 'global-linum-mode)
;; Version with a pipe
;; (setq linum-format "%4d \u2502")
;; Version without a pipe
(setq linum-format "%4d  ")

;; Set apropos order in a more convenient order
(setq apropos-sort-by-scores t)

;; Disables autosaving AND backup files
(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files
(setq create-lockfiles nil) ; stop creating # files

;; Set up magit hotkeys
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

;; Setup Auto Yasnippets - https://github.com/abo-abo/auto-yasnippet
(global-set-key (kbd "C-\"") #'aya-create)
(global-set-key (kbd "C-'") #'aya-expand)
(global-set-key (kbd "C-M-'") #'aya-open-line)

;; Sets the 80-column header
(require 'fill-column-indicator)
(set-fill-column 80)

;; ZONE it!
(require 'zone)

;; Limit available zone transitions
(setq zone-programs
      [zone-pgm-putz-with-case
       zone-pgm-whack-chars
       zone-pgm-five-oclock-swan-dive
       zone-pgm-rotate-LR-lockstep
       zone-pgm-paragraph-spaz])

;; Set zone timer
(zone-when-idle 1200)

;; Sets Qt's home
(defvar qt-home-dir
  (if (eq system-type 'windows-nt) "C:/Qt/" "~/Qt/"))

;; Sets current Qt version
(defvar qt-version "5.12.0/")

;; Sets current Qt toolchain home
(defvar qt-version-dir
  (concat qt-home-dir
          qt-version
          (if (eq system-type 'windows-nt) ; Add compiler toolchain
              "msvc/"
            "gcc_64/")))

;; Setup .pro files to be in mode qt-pro-mode
(add-to-list 'auto-mode-alist '("\\.pro\\'" . qt-pro-mode))


(add-hook 'qml-mode-hook
          (lambda ()
            ;; Setup QML autocompletion
            (require 'company-qml)
            (add-to-list 'company-backends 'company-qml)

            ;; Add newer Qt versions QML types
            (mapc (lambda (x)
                    (add-to-list 'company-qml-extra-qmltypes-files x))
                  (directory-files-recursively qt-version-dir "\.qmltypes$" ))
            (company-mode)))


;; Choose the Java indenting style for C-like modes
(setq c-default-style "java")

;; For C++ better usage
(add-hook 'c-mode-hook
	  (lambda ()
	    (setq company-backends (delete 'company-semantic company-backends))
	    (define-key c-mode-map  [(tab)] 'company-complete)
	    (define-key c++-mode-map  [(tab)] 'company-complete)

	    (add-to-list 'company-backends 'company-c-headers)
        (company-mode)))


;; Change C/C++ tabs to 4 spaces
(setq c-basic-offset 4)

;; Hotkey to indent command
(global-set-key (kbd "C-c TAB") 'c-indent-command)

;; `Semantic` autocompletion-by-context
(use-package cc-mode)
(use-package semantic)

(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)

(semantic-mode 1)


;; Gtags for ... tags?
(use-package ggtags)
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
              (ggtags-mode 1))))

(define-key ggtags-mode-map (kbd "C-c g s") 'ggtags-find-other-symbol)
(define-key ggtags-mode-map (kbd "C-c g h") 'ggtags-view-tag-history)
(define-key ggtags-mode-map (kbd "C-c g r") 'ggtags-find-reference)
(define-key ggtags-mode-map (kbd "C-c g f") 'ggtags-find-file)
(define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
(define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags)

(define-key ggtags-mode-map (kbd "M-,") 'pop-tag-mark)


;; Imenu with gtags
(setq-local imenu-create-index-function #'ggtags-build-imenu-index)


;; Helm + helm-gtags
(setq
 helm-gtags-ignore-case t
 helm-gtags-auto-update t
 helm-gtags-use-input-at-cursor t
 helm-gtags-pulse-at-cursor t
 helm-gtags-prefix-key "\C-cg"
 helm-gtags-suggested-key-mapping t
 )

(use-package helm-gtags)
;; Enable helm-gtags-mode
(add-hook 'dired-mode-hook 'helm-gtags-mode)
(add-hook 'eshell-mode-hook 'helm-gtags-mode)
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)

(define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
(define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
(define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
(define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
(define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
(define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)

;; Always activate undo-tree-mode because of redo (M-_)
(add-hook 'find-file-hook #'undo-tree-mode)

(defun set-limit-column-ruler (&optional column)
  "Creates a ruler to the right of the screen at column COLUMN.
It's main purpouse is to be a visual cue so you will know when your
lines have gone too far."
  (unless column (setq column 80))
  (set-fill-column column)
  (turn-on-fci-mode))

(defun set-lisp-limit-column-ruler ()
  "By Google's Lisp Style convention, ruler is at column 100."
  (set-limit-column-ruler 100))

(add-hook 'prog-mode-hook #'set-limit-column-ruler)
(add-hook 'lisp-mode-hook #'set-lisp-limit-column-ruler)
(add-hook 'lisp-interaction-mode-hook #'set-lisp-limit-column-ruler)
(add-hook 'emacs-lisp-mode-hook #'set-lisp-limit-column-ruler)
(add-hook 'scheme-mode-hook #'set-lisp-limit-column-ruler)
(add-hook 'clojure-mode-hook #'set-lisp-limit-column-ruler)

;; Use neotree as file explorer
(use-package neotree)
(global-set-key [f8] 'neotree-toggle)

;; Activate all-the-icons to use with neotree
(require 'all-the-icons)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

;; Use spaceline from SpaceEmacs (powered by powerline)
;; (require 'powerline)
;; (powerline-nano-theme)
;; (require 'spaceline-config)
;; (spaceline-spacemacs-theme)
;; (use-package spaceline-all-the-icons
;;   :after spaceline
;;   :config (spaceline-all-the-icons-theme))
;; (spaceline-all-the-icons--setup-anzu)            ;; Enable anzu searching
;; (spaceline-all-the-icons--setup-package-updates) ;; Enable package update indicator
;; (spaceline-all-the-icons--setup-git-ahead)       ;; Enable # of commits ahead of upstream in git
;; (spaceline-all-the-icons--setup-neotree)         ;; Enable Neotree mode line

;; Use doom-modeline
(use-package doom-modeline)
(doom-modeline-init)

;; DO WARN ABOUT SUPERUSER/ROOT EDITING
(defface find-file-root-header-face
  '((t (:foreground "white" :background "red3")))
  "*Face use to display header-lines for files opened as root.")

(defun find-file-root-header-warning ()
  "*Display a warning in header line of the current buffer.
This function is suitable to add to `find-file-hook'."
  (when (string-equal
         (file-remote-p (or buffer-file-name default-directory) 'user)
         "root")
    (let* ((warning "WARNING: EDITING FILE AS ROOT!")
           (space (+ 6 (- (window-width) (length warning))))
           (bracket (make-string (/ space 2) ?-))
           (warning (concat bracket warning bracket)))
      (setq header-line-format
            (propertize  warning 'face 'find-file-root-header-face)))))

(add-hook 'find-file-hook 'find-file-root-header-warning)
(add-hook 'dired-mode-hook 'find-file-root-header-warning)

;; Use ledger mode
(use-package ledger-mode)
(autoload 'ledger-mode "ledger-mode" "A major mode for Ledger" t)
(add-to-list 'load-path
             (expand-file-name "/path/to/ledger/source/lisp/"))
(add-to-list 'auto-mode-alist '("\\.ledger$" . ledger-mode))

;; Use rainbow-delimiters and rainbow-identifiers
(use-package rainbow-identifiers)
(add-hook 'prog-mode-hook 'rainbow-identifiers-mode)
(use-package rainbow-delimiters)
(add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'lisp-interaction-mode-hook 'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'scheme-mode-hook 'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)


(use-package paredit)


(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       'paredit-mode)
;;(add-hook 'eval-expression-minibuffer-setup-hook 'paredit-mode)
;;(add-hook 'ielm-mode-hook             'paredit-mode)
(add-hook 'lisp-mode-hook             'paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'paredit-mode)
(add-hook 'scheme-mode-hook           'paredit-mode)
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook
 'paredit-mode-hook
 (lambda ()
   (define-key paredit-mode-map (kbd "C-c <left>") 'paredit-backward-slurp-sexp)
   (define-key paredit-mode-map (kbd "C-c <right>") 'paredit-backward-barf-sexp)))

;; Unbind slime's keybind's hijacking
(defun override-slime ()
  (define-key slime-repl-mode-map
    (read-kbd-macro paredit-backward-delete-key) nil)
  (define-key slime-repl-mode-map (kbd "M-_") nil)
  (define-key slime-mode-indirect-map (kbd "M-_") nil)
  (define-key undo-tree-map (kbd "M-_") 'undo-tree-redo))

(eval-after-load "slime-mode"
  '(define-key slime-mode-indirect-map (kbd "M-_") nil))

(add-hook 'slime-repl-mode-hook 'override-slime)

;; Properly configure slime contribs
(require 'slime-autoloads)
(setq inferior-lisp-program "/usr/bin/sbcl")
(add-to-list 'slime-contribs 'slime-indentation)

(put 'erase-buffer 'disabled nil)

(setq org-startup-indented t
      org-bullets-bullet-list '(" ") ;; no bullets, needs org-bullets package
      org-ellipsis "  " ;; folding symbol
      org-pretty-entities t
      org-hide-emphasis-markers t
      ;; show actually italicized text instead of /italicized text/
      org-agenda-block-separator ""
      org-fontify-whole-heading-line t
      org-fontify-done-headline t
      org-fontify-quote-and-verse-blocks t)

