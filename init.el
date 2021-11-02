;; -*- lexical-binding: t -*-

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

;; Alias "yes or no" prompt to "y or n" because I can't
;; be bothered to type 2 extra letters
(defalias 'yes-or-no-p 'y-or-n-p)

;; Set default font to SF Mono if installed
(defun font-is-installed (font-name)
  "check if font is installed on a system"
  (let ((f (find-font (font-spec :name font-name))))
    (if (null f)
	nil
      t)))

(if (font-is-installed "SF Mono")
    (set-face-attribute 'default nil
			:family "SF Mono"
			:height 130
			:weight 'normal
			:width 'normal))
			
;; Visual tweaks
(menu-bar-mode -1)
(blink-cursor-mode -1)
(global-linum-mode 1)
(when (window-system)
  (progn
    (setq frame-resize-pixelwise t)
    (toggle-scroll-bar -1)
    (tool-bar-mode -1)
    (global-hl-line-mode 1)
    (set-frame-size (selected-frame) 110 70)))

;; Disable startup screen and turn off the GNU
;; welcome message
(defun no-startup-echo-area-message ()
  (message ""))
(defalias 'startup-echo-area-message 'no-startup-echo-area-message)
(setq inhibit-startup-screen t)

;; Fix default C/C++ formatting
(setq c-default-style "linux"
      c-basic-offset 4)
