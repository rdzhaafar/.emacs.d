;; -*- lexical-binding: t -*-

;; ==============================================================================
;; Random tweaks
;; ==============================================================================

;; Disable autosave files and all kinds of miscellaneous
;; garbage that litters in source directories
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

;; Alias "yes or no" prompt to "y or n" because I can't
;; be bothered to type 2 extra letters
(defalias 'yes-or-no-p 'y-or-n-p)

;; ==============================================================================
;; Visual tweaks
;; ==============================================================================

(defun font-is-installed (font-name)
  "check if font is installed on a system"
  (let ((f (find-font (font-spec :name font-name))))
    (if (null f)
	nil
      t)))

;; Change default font to Inconsolata if installed
(if (font-is-installed "Inconsolata")
    (set-face-attribute 'default nil
			:family "Inconsolata"
			:height 160
			:weight 'normal
			:width 'normal))

;; Disable menu bar and blinking cursor
(menu-bar-mode -1)
(blink-cursor-mode -1)

;; Show line numbers everywhere
(global-linum-mode 1)

;; Change cursor style to '|'
(setq-default cursor-type 'bar)

;; Visual settings when running in GUI mode
(when (window-system)
  (progn
    (setq frame-resize-pixelwise t)
    (toggle-scroll-bar -1)
    (tool-bar-mode -1)
    (global-hl-line-mode 1)
    (add-to-list 'initial-frame-alist '(fullscreen . maximized))))

;; Hide startup screen and inhibit the welcome message
(setq inhibit-startup-screen t)

(defun no-startup-echo-area-message ()
  (message ""))
(defalias 'startup-echo-area-message 'no-startup-echo-area-message)

;; ==============================================================================
;; Editor settings
;; ==============================================================================

;; Reload buffers automatically when they're changed on disk
(setq global-auto-revert-mode t)

;; Fix default C/C++ formatting
(setq c-default-style "linux"
      c-basic-offset 4)

;; ==============================================================================
;; Package manager config
;; ==============================================================================

;; Install straight.el
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

;; Disable package.el
(setq package-enable-at-startup nil)

;; Install use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; Always automatically install packages
(setq use-package-always-ensure t)

;; ==============================================================================
;; Packages
;; ==============================================================================

(use-package go-mode)
(use-package doom-modeline
  :init
  (doom-modeline-mode 1))
(use-package github-theme
  :init
  (load-theme 'github t))

;; ==============================================================================
;; Command aliases
;; ==============================================================================

(defalias 'end 'end-of-buffer)
(defalias 'start 'beginning-of-buffer)

