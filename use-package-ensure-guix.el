;;; use-package-ensure-guix.el --- Ensure used packages with Guix

;; Copyright (C) 2023  Mitchell Schmeisser <mitchellschmeisser@librem.one>

;; Author: Mitchell Schmeisser <mitchellschmeisser@librem.one>
;; Maintainer: Mitchell Schmeisser <mitchellschmeisser@librem.one>
;; URL: https://github.com/paperclip4465/use-package-ensure-guix
;; Keywords: dotemacs config package guix

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package allows you to make use-package.el ensure that Emacs-packages
;; are installed by using GNU Guix. It will install respective packages into
;; an Emacs-specific guix-profile.

;;; Code:

(require 'guix)
(require 'guix-misc)
(require 'guix-profiles)
(require 'guix-read)
(require 'guix-ui-package)
(require 'guix-ui-profile)
(require 'guix-utils)

(defgroup use-package-ensure-guix nil
  "use-package support for guix"
  :group 'use-package-ensure)

(defcustom use-package-ensure-guix-profile (concat (getenv "HOME") "/.emacs.d/guix-profile")
  "Location of use-package guix profile"
  :type 'string
  :group 'use-package-ensure-guix)

(defun use-package-ensure-guix-show-packages ()
  (interactive)
  (guix-profile-info-show-packages use-package-ensure-guix-profile))

(defun use-package-ensure-guix-installed-p (package)
  (bui-assoc-value package 'installed))

(defun use-package-ensure-guix--get-guix-package-name (package-name)
  "Make sure package name has \"emacs-\" prefix"
  (if (string-match "^emacs-.+" package-name)
      package-name
    (concat "emacs-" package-name)))

(defun use-package-ensure-guix--get-guix-package (package)
  "Return guix package from package name"
  (car (guix-output-list-get-entries use-package-ensure-guix-profile 'name
		 (use-package-ensure-guix--get-guix-package-name package))))

;;;###autoload
(defun use-package-ensure-guix-update-load-path ()
  "Ensures all ensured packages are added to the load-path if not
already there."
  (mapc (lambda (x)
	  (unless (member x load-path)
	    (add-to-list 'load-path x)))
	(directory-files (concat use-package-ensure-guix-profile "/share/emacs/site-lisp") t)))


;;;###autoload
(defun use-package-ensure-guix--install (package)
  "Install PACKAGE, a guix package as returned by `use-package-ensure-guix--get-guix-package`,
into `use-package-ensure-guix-profile`"
  (if (use-package-ensure-guix-installed-p package)
      t
    (guix-process-package-actions
     use-package-ensure-guix-profile
     `((install (,(string-to-number (car (split-string (bui-entry-id package) ":"))) "out")))
     (current-buffer))))


;;;###autoload
(defun use-package-ensure-guix (name args _state &optional _no-refresh)
  (dolist (ensure args)
    (let ((package
	   (or (and (eq ensure t)
		    (use-package-as-symbol name))
	       ensure)))
      (when package
	(when (consp package)
	  (setq package (car package)))

	(let ((package (use-package-ensure-guix--get-guix-package (use-package-as-string package))))
	  (unless (use-package-ensure-guix-installed-p package)
	    (use-package-ensure-guix--install package))
	  (use-package-ensure-guix-update-load-path))))))

;;;###autoload
(defun use-package-ensure-guix-profile-info ()
  "Display interactive information for
`guix-use-package-ensure-guix-profile'."
  (interactive)
  (bui-get-display-entries 'guix-profile 'info
			   (list 'profile use-package-ensure-guix-profile)))

(provide 'use-package-ensure-guix)

;;; use-package-ensure-guix.el ends here
