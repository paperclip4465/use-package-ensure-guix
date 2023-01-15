(require 'guix)
(require 'guix-profiles)
(require 'guix-read)
(require 'guix-ui-package)
(require 'guix-utils)
(require 'guix-misc)

(defgroup use-package-guix nil
  "use-package support for guix"
  :group 'use-package-ensure)

(defcustom use-package-profile (concat (getenv "HOME") "/.emacs.d/guix-profile")
  "Location of use-package guix profile"
  :type 'string
  :group 'use-package-guix)

(defvar use-package-ensured-guix-packages '()
  "List of guix packages which use-package ensures.")

(defun use-package-guix-show-ensured ()
  (interactive)
  (guix-profile-info-show-packages use-package-profile))


(defun use-package-guix-package-installed-p (package)
  (bui-assoc-value package 'installed))

(defun use-package-guix-canonicalize-name (package-name)
  "Make sure package name has \"emacs-\" prefix"
  (if (string-match "^emacs-.+" package-name)
      package-name
    (concat "emacs-" package-name)))

(defun emacs-package->guix-package (package)
  "Return guix package from package name"
  (car (guix-output-list-get-entries use-package-profile 'name
				     (canonicalize-name package))))

(defun use-package-guix-update-load-path ()
  "Ensures all ensured packages are added to the load-path if not
already there."
  (mapc (lambda (x)
	  (unless (member x load-path)
	    (add-to-list 'load-path x)))
	(directory-files (concat use-package-profile "/share/emacs/site-lisp") t)))


(defun use-package-guix-install-package (package)
  "Install PACKAGE, a guix package as returned by `emacs-package->guix-package`,
into `use-package-profile`"
  (if (guix-package-installed-p package)
      t
    (guix-process-package-actions
     use-package-profile
     `((install (,(string-to-number (car (split-string (bui-entry-id package) ":"))) "out")))
     (current-buffer))))


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
	  (unless (guix-package-installed-p package)
	    (use-package-guix-install-package package)
	    (use-package-guix-update-load-path)))))))

;;;###autoload
(defun guix-use-package-profile ()
  "Display interface for `guix-use-package-profile'."
  (interactive)
  (bui-get-display-entries 'guix-profile 'info
			   (list 'profile use-package-profile)))

(provide 'use-package-guix)