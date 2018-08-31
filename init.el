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

(add-to-list 'load-path "~/.emacs.d/custom")

(require 'setup-general)
(if (version< emacs-version "24.4")
    (require 'setup-ivy-counsel)
  (require 'setup-helm)
  (require 'setup-helm-gtags))
;; (require 'setup-ggtags)
(require 'setup-cedet)
(require 'setup-editing)

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

;; Set up magit hotkeys
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

;; Setup Auto Yasnippets - https://github.com/abo-abo/auto-yasnippet
(global-set-key (kbd "C-\"") #'aya-create)
(global-set-key (kbd "C-'") #'aya-expand)
(global-set-key (kbd "C-M-'") #'aya-open-line)

;; ZONE it!
(require 'zone)

;; load the packaged named zone-select.
(load "zone-select") ;; best not to include the ending “.el” or “.elc”

;; Limit available zone transitions
(setq zone-programs [zone-pgm-putz-with-case zone-pgm-whack-chars zone-pgm-five-oclock-swan-dive zone-pgm-rotate-LR-lockstep zone-pgm-paragraph-spaz zone-pgm-random-life])

;; Set zone timer
(zone-when-idle 1200)

;; Setup .pro files to be in mode qt-pro-mode
(add-to-list 'auto-mode-alist '("\\.pro\\'" . qt-pro-mode))

;; Choose the Java indenting style for C-like modes
(setq c-default-style "java")

;; For C++ better usage
(require 'company)
(add-hook 'after-init-hook 'company-mode)

(setq company-backends (delete 'company-semantic company-backends))
(define-key c-mode-map  [(tab)] 'company-complete)
(define-key c++-mode-map  [(tab)] 'company-complete)

(add-to-list 'company-backends 'company-c-headers)


;; Change C/C++ tabs to 4 spaces
(setq c-basic-offset 4)

;; Hotkey to indent command
(global-set-key (kbd "C-c TAB") 'c-indent-command)

;; `Semantic` autocompletion-by-context
(require 'cc-mode)
(require 'semantic)

(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)

(semantic-mode 1)


;; Gtags for ... tags?
(require 'ggtags)
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

(require 'helm-gtags)
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


;; Use neotree as file explorer
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

;; Activate all-the-icons to use with neotree
(require 'all-the-icons)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

;; Use spaceline from SpaceEmacs (powered by powerline)
(require 'powerline)
(powerline-nano-theme)
(require 'spaceline-config)
(spaceline-spacemacs-theme)
(use-package spaceline-all-the-icons
  :after spaceline
  :config (spaceline-all-the-icons-theme))
(spaceline-all-the-icons--setup-anzu)            ;; Enable anzu searching
(spaceline-all-the-icons--setup-package-updates) ;; Enable package update indicator
(spaceline-all-the-icons--setup-git-ahead)       ;; Enable # of commits ahead of upstream in git
(spaceline-all-the-icons--setup-neotree)         ;; Enable Neotree mode line

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
(require 'ledger-mode)
(autoload 'ledger-mode "ledger-mode" "A major mode for Ledger" t)
(add-to-list 'load-path
             (expand-file-name "/path/to/ledger/source/lisp/"))
(add-to-list 'auto-mode-alist '("\\.ledger$" . ledger-mode))

(put 'erase-buffer 'disabled nil)
