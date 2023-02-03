;;; Copyright © 2022-2023 Mitchell Schmeisser <mitchellschmeisser@librem.one>

(require 'guix)
(require 'guix-misc)
(require 'guix-profiles)
(require 'guix-read)
(require 'guix-ui-package)
(require 'guix-utils)

(defgroup use-package-ensure-guix nil
  "use-package support for guix"
  :group 'use-package-ensure)

(defcustom use-package-ensure-guix-profile (concat (getenv "HOME") "/.emacs.d/guix-profile")
  "Location of use-package guix profile"
  :type 'string
  :group 'use-package-ensure-guix)

(defun use-package-guix-show-ensured ()
  (interactive)
  (guix-profile-info-show-packages use-package-ensure-guix-profile))


(defun use-package-guix-package-installed-p (package)
  (bui-assoc-value package 'installed))

(defun use-package-guix-canonicalize-name (package-name)
  "Make sure package name has \"emacs-\" prefix"
  (if (string-match "^emacs-.+" package-name)
      package-name
    (concat "emacs-" package-name)))

(defun emacs-package->guix-package (package)
  "Return guix package from package name"
  (car (guix-output-list-get-entries use-package-ensure-guix-profile 'name
				     (use-package-guix-canonicalize-name package))))

;;;###autoload
(defun use-package-guix-update-load-path ()
  "Ensures all ensured packages are added to the load-path if not
already there."
  (mapc (lambda (x)
	  (unless (member x load-path)
	    (add-to-list 'load-path x)))
	(directory-files (concat use-package-ensure-guix-profile "/share/emacs/site-lisp") t)))


;;;###autoload
(defun use-package-guix-install-package (package)
  "Install PACKAGE, a guix package as returned by `emacs-package->guix-package`,
into `use-package-ensure-guix-profile`"
  (if (use-package-guix-package-installed-p package)
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

	(let ((package (emacs-package->guix-package (use-package-as-string package))))
	  (unless (use-package-guix-package-installed-p package)
	    (use-package-guix-install-package package))
	  (use-package-guix-update-load-path))))))

;;;###autoload
(defun use-package-guix-profile ()
  "Display interface for `guix-use-package-ensure-guix-profile'."
  (interactive)
  (bui-get-display-entries 'guix-profile 'info
			   (list 'profile use-package-ensure-guix-profile)))

(provide 'use-package-ensure-guix)
