;; -*- lexical-binding: t -*-

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

(defalias 'yes-or-no-p 'y-or-n-p)

(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
 (package-install 'use-package))

(require 'use-package-ensure)
(setq use-package-always-ensure t)
(use-package auto-compile
  :config
  (auto-compile-on-load-mode))
(setq load-prefer-newer t)


(defun font-installed-p (font)
  "check if a font is installed"
  (let ((f (find-font (font-spec :name font))))
    (if (null f)
	nil
      t)))

(set-face-attribute 'default nil
                    :family
		    (if (font-installed-p "JetBrains Mono NL")
			"JetBrains Mono NL"
		      "Source Code Pro")
                    :height 110
                    :weight 'normal
                    :width 'normal)

(menu-bar-mode -1)
(when (window-system)
  (progn
    (setq frame-resize-pixelwise t)
    (toggle-scroll-bar -1)
    (tool-bar-mode -1)))

(add-hook 'prog-mode-hook 'linum-mode)

(blink-cursor-mode -1)

(setq inhibit-startup-screen t)

(defun rd-startup-echo-area-message ()
  (message ""))
(defalias 'startup-echo-area-message 'rd-startup-echo-area-message)


(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t)
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config))

(use-package ivy
  :diminish
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-use-selectable-prompt t)
  :config
  (ivy-mode))

(use-package counsel
  :after
  (ivy)
  :config
  (counsel-mode))

(use-package swiper
  :after
  (ivy)
  :bind
  (("C-s" . swiper)
   ("C-r" . swiper)))

(use-package rust-mode
  :config
  (setq rust-format-on-save t))

; NOTE: GCMH should always be loaded last
(use-package gcmh
	     :init
	     (gcmh-mode 1))
