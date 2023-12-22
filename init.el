;; -*- lexical-binding: t -*-

;; ==============================================================================
;; Package manager
;; ==============================================================================

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq package-enable-at-startup nil)

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(setq use-package-always-ensure t)

;; ==============================================================================
;; General settings
;; ==============================================================================

;; Disable autosave/backup/lockfiles
(setq-default custom-file
	      (expand-file-name ".custom.el" user-emacs-directory))

(when
    (file-exists-p custom-file)
  (load custom-file))
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq create-lockfiles nil)
(setq auto-save-default nil)

(defalias 'yes-or-no-p 'y-or-n-p)

;; Set font
(defun font-is-installed (font-name)
  "check if font is installed on a system"
  (let ((f (find-font (font-spec :name font-name))))
    (if (null f)
	nil
      t)))

(if (font-is-installed "Inconsolata")
    (set-face-attribute 'default nil
			:family "Inconsolata"
			:height 160
			:weight 'normal
			:width 'normal))

(setq inhibit-startup-screen t)
(menu-bar-mode -1)
(blink-cursor-mode -1)
(setq-default cursor-type 'bar)
(global-display-line-numbers-mode 1)
(set-default 'truncate-lines t)

;; Disable GUI controls
(when (window-system)
  (progn
    (setq frame-resize-pixelwise t)
    (toggle-scroll-bar -1)
    (tool-bar-mode -1)
    (global-hl-line-mode 1)))

;; Hide startup message
(defun no-startup-echo-area-message ()
  (message ""))
(defalias 'startup-echo-area-message 'no-startup-echo-area-message)

;; Automatically reload files from disk
(setq global-auto-revert-mode t)

(use-package github-theme
  :init
  (load-theme 'github t))

;; ==============================================================================
;; C/C++
;; ==============================================================================

(setq c-default-style "linux"
      c-basic-offset 4)

;; ==============================================================================
;; Lisp
;; ==============================================================================

(add-hook 'lisp-mode-hook 'electric-pair-mode)

;; ==============================================================================
;; Go
;; ==============================================================================

(use-package go-mode)

