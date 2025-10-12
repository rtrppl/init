;; This setup uses the straight package manager. It was written to maximize
;; speed and ease of reproduction.  
;;
;; Version 0.1
;;
;; The setup expects the following software to be installed:
;; - ripgrep 
;; - mpv
;; - fd
;; - tree
;;
;; The following part is setting up package management for Emacs

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(straight-use-package '(org :type built-in))

;; These are common-sense standards for Emacs - change them at your peril :). 

(setq inhibit-splash-screen t)    
(setq gc-cons-threshold 20000000)  
(setq backup-directory-alist
       `((".*" . ,temporary-file-directory)))     
(setq auto-save-file-name-transforms                         
      `((".*" ,temporary-file-directory t))) 

(global-auto-revert-mode t)
(display-time-mode t)
(setq ring-bell-function 'ignore)
(prefer-coding-system 'utf-8)
(delete-selection-mode t) ;; replace marked text
(tool-bar-mode -1)        ;; no gui toolbar
(set-fringe-mode 10)
(setq large-file-warning-threshold nil) ;; no warnings for large files
(pixel-scroll-precision-mode)
(setq scroll-conservatively 100)


;; loading packages

;; CNFonts do not seem to work on Ubuntu machines. Disabled for now.

;(use-package cnfonts)
;; 让 cnfonts 随着 Emacs 自动生效。
;(cnfonts-enable)
;; 让 spacemacs mode-line 中的 Unicode 图标正确显示。
;; (cnfonts-set-spacemacs-fallback-fonts)
;(setq cnfonts-use-face-font-rescale t)
;(cnfonts--next-fontsize 2)

;; Cleandesk is an upgrade to Dired, the file manager on Emacs

(use-package consult
  :bind (;; A recursive grep
         ("C-c o R" . consult-ripgrep)
         ;; Search for files names recursively
         ("C-c o F" . consult-find)
         ;; Search through the outline (headings) of the file
         ("C-c o o" . consult-outline)
         ;; Search the current buffer
         ("C-c o L" . consult-line)
	       ;; Search in notes
	       ("C-c o N" . consult-notes-search-in-all-notes)
         ;; switch to another buffer, or bookmarked file, or recently
         ;; opened file.
         ("C-c o b" . consult-buffer)
	 ("C-+" . consult-narrow-key)))

(use-package dired
  :straight nil
  :config
  (when (and (eq system-type 'darwin) (executable-find "gls"))
    (setq insert-directory-program "gls"))
  (add-hook 'dired-mode-hook
      (lambda ()
        (dired-hide-details-mode)))
  (put 'dired-find-alternate-file 'disabled nil)
  (setq delete-by-moving-to-trash t))

(use-package discover
  :config
  (global-discover-mode 1))

(use-package embark
  :bind (("C-." . embark-act)
         :map minibuffer-local-map
         ("C-c C-c" . embark-collect)
         ("C-c C-e" . embark-export)))

(use-package embark-consult)

(use-package marginalia
  :config
  (marginalia-mode 1))

(use-package markdown-mode)

;; A package to search on several different search engines
;; at the same time. 

(use-package metasearch
 :straight (:host github :repo "rtrppl/metasearch"
		   :branch "main")
  :config
  (defun metasearch-search ()
    (interactive)
    (metasearch-search-set "Search"))
  :bind
  (:map global-map
	      ("C-c d m" . metasearch-search)
	      ("C-c d M" . metasearch-search-set)))

;; For reading ebooks.

(use-package nov
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

(use-package olivetti
  :config
  (add-hook 'olivetti-mode-on-hook (lambda () (olivetti-set-width 65) (set-fringe-mode 0))))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package org
  :straight nil)

(use-package org-web-tools)


(use-package ox-pandoc)

(use-package powerthesaurus
  :bind
  (:map global-map)
  ("C-c d P" . powerthesaurus-lookup-word-at-point)) 

(use-package pyim)
(use-package pyim-basedict)

(pyim-basedict-enable)
(setq default-input-method "pyim")
(setq pyim-page-length 6)
(pyim-default-scheme 'quanpin)
(pyim-isearch-mode 1)

(use-package vertico
  :config
  (setq vertico-cycle t)
  (setq vertico-resize t)
;  (setq vertico-sort-function nil)
;  (setq vertico-respect-minibuffer-completion-styles t)
  (vertico-mode 1))

(use-package which-key
  :config
  (which-key-mode))

(use-package zenburn-theme)

;; The following section is about how Emacs looks.


;; start up window & font & design > go max!
(add-to-list 'default-frame-alist '(fullscreen . maximized))
;; org-mode emphasis
(add-to-list 'org-emphasis-alist '("_" nil))
;(setq org-emphasis-alist
;      '(("/" (italic :foreground "DarkOrange1"))
;	("_" nil) ;; disable underline
;	("*" bold)
;	("=" org-verbatim verbatim)
;	("~" org-code verbatim)
;	("+" (:strike-through t))))
;; Underline line at descent position, not baseline position
(setq x-underline-at-descent-line t)
(load-theme 'zenburn t)
(fringe-mode 10)
(set-face-attribute 'fringe nil :background (face-background 'default))
(menu-bar-mode -1) ;; no menu bar
(transient-mark-mode 1) ;; enable transient mark mode
(setq-default line-spacing 0.15)
(global-visual-line-mode 1)

(when (eq system-type 'gnu/linux)
  ;; Set the default face attributes for Ubuntu/Linux
  (set-face-attribute 'default nil
                      :inherit nil
                      :extend nil
                      :stipple nil
                      :inverse-video nil
                      :box nil
                      :strike-through nil
                      :overline nil
                      :underline nil
                      :slant 'normal
                      :weight 'normal
                      :height 140  ;; 140 means 14pt, adjust if desired
                      :width 'normal
                      :foundry "nil"
                      :family "DejaVu Sans Mono"))  ;; or "Ubuntu Mono"
(when (eq system-type 'darwin)
;; Set the default face attributes for macOS
  (set-face-attribute 'default nil
                      :inherit nil
                      :extend nil
                      :stipple nil
                      :inverse-video nil
                      :box nil
                      :strike-through nil
                      :overline nil
                      :underline nil
                      :slant 'normal
                      :weight 'normal
                      :height 160
                      :width 'normal
                      :foundry "nil"
                      :family "Menlo"))

(when (eq system-type 'windows-nt)
;; Set the default face attributes for macOS
  (set-face-attribute 'default nil
                      :inherit nil
                      :extend nil
                      :stipple nil
                      :inverse-video nil
                      :box nil
                      :strike-through nil
                      :overline nil
                      :underline nil
                      :slant 'normal
                      :weight 'normal
                      :height 140
                      :width 'normal
                      :foundry "nil"
                      :family "Consolas"))

;; Set the bold face attributes
(set-face-attribute 'bold nil
                    :foreground "plum1"
                    :weight 'bold)

(electric-indent-mode -1) ;; prevents indents in org-babbel

;; This fixes the Elfeed display.
(setq shr-use-fonts nil)

;; Settings for Chinese fonts for different OS
(when (eq system-type 'darwin)
  (add-to-list 'face-font-rescale-alist
               '("PingFang SC" . 1.4))
  (dolist (script '(kana han cjk-misc symbol))
    (set-fontset-font t script
                      (font-spec :family "PingFang SC"))))
(when (eq system-type 'gnu/linux)
  (add-to-list 'face-font-rescale-alist
               '("Noto Sans Mono CJK SC" . 1.4))
  (dolist (script '(kana han cjk-misc symbol))
    (set-fontset-font t script
                      (font-spec :family "Noto Sans Mono CJK SC"))))

;; TODO Add setting for Windows



;; org

(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))
(setq org-agenda-include-diary nil)
(setq org-startup-indented t)
(setq org-log-done 'note) 
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "WAITING(w)" "RECREATION(r)" "PROJECT(o)" "|" "DONE(d@)")))

(setq org-archive-location "%s_archive::datetree/")
(setq org-goto-interface 'outline-path-completion)

(setq org-refile-targets '((org-agenda-files :maxlevel . 5)))
(setq org-refile-use-outline-path t)
(setq org-refile-allow-creating-parent-nodes 'confirm)

(setq org-refile-targets '((nil :maxlevel . 5)
(org-agenda-files :maxlevel . 5)))
(setq org-outline-path-complete-in-steps nil)

(add-hook 'calendar-load-hook
	  (lambda ()
	    (calendar-set-date-style 'european)))
(setq calendar-week-start-day 1)
(setq org-deadline-warning-days 7)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-search-view-always-boolean t)
(setq org-sparse-tree-open-archived-trees t)
(require 'org-habit)
(setq org-agenda-show-future-repeats nil)
(setq org-default-priority ?C)
(setq org-agenda-prefix-format '(
  (agenda  . " %i %?-12t ") 
  (timeline  . "  % s")
  (todo  . " %?-12t% s")
  (tags  . " %i %-12:c")
  (search . " %i %-12:c")))

(setq org-goto-interface 'outline-path-completion)
(setq org-use-speed-commands t)
(setq org-startup-folded nil)

(setq org-footnote-auto-adjust nil)
(require 'org-inlinetask)

(setq org-edit-src-content-indentation 0)
(setq org-src-tab-acts-natively t)
(setq org-src-preserve-indentation t)
(setq org-confirm-babel-evaluate nil)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)))

;; keybindings

(global-set-key (kbd "M-_") 'rotate-windows)
(define-key org-mode-map (kbd "C-f") nil)
(define-key org-mode-map (kbd "C-f <right>") 'forward-fn)
(define-key org-mode-map (kbd "C-f <left>") 'backward-fn)
(global-set-key (kbd "M-<right>") 'right-word)
(global-set-key (kbd "M-<left>") 'left-word)
(global-set-key (kbd "M-<up>") 'backward-paragraph)
(global-set-key (kbd "M-<down>") 'forward-paragraph)
(global-set-key (kbd "C-<up>") 'org-metaup)
(global-set-key (kbd "C-<down>") 'org-metadown)
(global-set-key (kbd "C-<right>") 'org-metaright)
(global-set-key (kbd "C-<left>") 'org-metaleft)
(global-set-key (kbd "C-)") 'forward-paragraph)
(global-set-key (kbd "C-(") 'backward-paragraph)
(global-set-key (kbd "s-n") 'make-frame)
(global-set-key (kbd "s-z") 'undo)
(global-set-key (kbd "s-<right>") 'end-of-visual-line)
(global-set-key (kbd "s-<left>") 'beginning-of-visual-line)
(global-set-key (kbd "s-<up>") 'beginning-of-buffer)
(global-set-key (kbd "s-<down>") 'end-of-buffer)
(global-set-key (kbd "s-<return>") 'org-meta-return)
(global-set-key (kbd "M-s-<left>") 'org-previous-link)
(global-set-key (kbd "M-s-<down>") 'org-open-at-point)
(global-set-key (kbd "C-/") 'toggle-input-method)

;; modififications

(defun xah-save-all-unsaved ()
  "Save all unsaved files. no ask.
Version 2019-11-05"
  (interactive)
  (save-some-buffers t))
(if (version< emacs-version "27")
    (add-hook 'focus-out-hook 'xah-save-all-unsaved)
  (setq after-focus-change-function 'xah-save-all-unsaved))

(defun rotate-windows (arg)
  "Rotate your windows; use the prefix argument to rotate the other direction"
  (interactive "P")
  (if (not (> (count-windows) 1))
  (message "You can't rotate a single window!")
(let* ((rotate-times (prefix-numeric-value arg))
   (direction (if (or (< rotate-times 0) (equal arg '(4)))
  'reverse 'identity)))
  (dotimes (_ (abs rotate-times))
(dotimes (i (- (count-windows) 1))
  (let* ((w1 (elt (funcall direction (window-list)) i))
	 (w2 (elt (funcall direction (window-list)) (+ i 1)))
	 (b1 (window-buffer w1))
	 (b2 (window-buffer w2))
	 (s1 (window-start w1))
	 (s2 (window-start w2))
	 (p1 (window-point w1))
	 (p2 (window-point w2)))
    (set-window-buffer-start-and-point w1 b2 s2 p2)
    (set-window-buffer-start-and-point w2 b1 s1 p1)))))))

(defun copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(defun forward-fn ()
  "Move forward to next footnote."
  (interactive)
  (re-search-forward "fn:" nil t))

(defun backward-fn ()
  "Move backward to previous footnote."
  (interactive)
  (re-search-backward "fn:" nil t))

;; spellchecking

(when (not (eq system-type 'windows-nt))
  (with-eval-after-load "ispell"
  (setenv "LANG" "en_US")
  (setq ispell-program-name "hunspell")
  ;; Configure German, Swiss German, and two variants of English.
  (setq ispell-dictionary "de_DE,en_US")
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "de_DE,en_US")
  (setq ispell-personal-dictionary "~/.hunspell_personal"))
  (add-hook 'org-mode-hook 'turn-on-flyspell))
  
;; load pp

 (when (file-exists-p "~/setup/.pp.el")
  (load-file (expand-file-name "~/setup/.pp.el")))
