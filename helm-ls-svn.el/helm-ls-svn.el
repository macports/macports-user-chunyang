;;; helm-ls-svn.el --- list svn files                -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Chunyang Xu

;; Author: Chunyang Xu <chunyang@macports.org>
;; Created: Wed Jun 10 20:58:26 CST 2015
;; Version: 0.1
;; URL: https://trac.macports.org/browser/users/chunyang/helm-ls-svn.el
;; Package-Requires: ((emacs "24.1") (helm "1.7.0"))
;; Keywords: helm svn

;; This file is not part of Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; `helm-ls-svn.el' is a helm extension for listing files in svn project.
;;
;;
;; TODO
;; ====
;;
;; - Improve svn status source.
;; - Helm-find-files integration.
;; - Submit to melpa.
;; - Find out a suitable way to search in svn project.
;; - Improve performance.

;;; Code:

(require 'helm-files)

;; Define the sources.
(defvar helm-source-ls-svn-status nil)
(defvar helm-source-ls-svn nil)
(defvar helm-source-ls-svn-buffers nil)


(defgroup helm-ls-svn nil
  "Helm completion for svn repos."
  :group 'helm)

(defcustom helm-ls-svn-status-command 'vc-dir
  "Favorite svn-status command for emacs."
  :group 'helm-ls-svn
  :type 'symbol)

(defvar helm-ls-svn-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-generic-files-map)
    map))

(defun helm-ls-svn-root-dir (&optional directory)
  (locate-dominating-file (or directory default-directory) ".svn"))

(defun helm-ls-svn-not-inside-svn-repo ()
  (not (helm-ls-svn-root-dir)))

(defun helm-ls-svn-branch ()
  (shell-command-to-string
   "svn info | grep '^URL:' | egrep -o '(tags|branches)/[^/]+|trunk' | egrep -o '[^/]+$' | tr -d '\n'"))

(defun helm-ls-svn-header-name (name)
  (let ((branch (helm-ls-svn-branch)))
    (format "%s (%s)"
            name (if (string-empty-p branch)
                     (helm-ls-svn-root-dir) branch))))

(defun helm-ls-svn-init ()
  (let ((root (helm-ls-svn-root-dir)))
    (with-current-buffer (helm-candidate-buffer 'global)
      (call-process-shell-command
       (format "find %s -type f -not -iwholename '*.svn/*'"
               root)
       nil t ))))

(defun helm-ls-svn-status ()
  (helm-aif (helm-ls-svn-root-dir)
      (with-helm-default-directory it
          (with-output-to-string
            (with-current-buffer standard-output
              (apply #'process-file
                     "svn" nil t nil
                     (list "status")))))))

(defun helm-ls-svn-status-transformer (candidates _source)
  (let ((root (helm-ls-svn-root-dir)))
    (mapcar (lambda (candidate)
              (cons candidate
                    (expand-file-name (cadr (split-string candidate)) root)))
            candidates)))

(defclass helm-ls-svn-source (helm-source-in-buffer)
  ((header-name :initform 'helm-ls-svn-header-name)
   (init :initform 'helm-ls-svn-init)
   (keymap :initform helm-ls-svn-map)
   (help-message :initform helm-generic-file-help-message)
   (candidate-number-limit :initform 9999)
   (action :initform (helm-actions-from-type-file))))

(defclass helm-ls-svn-status-source (helm-source-in-buffer)
  ((header-name :initform 'helm-ls-svn-header-name)
   (init :initform
         (lambda ()
           (helm-init-candidates-in-buffer 'global
             (helm-ls-svn-status))))
   (keymap :initform helm-ls-svn-map)
   (filtered-candidate-transformer :initform 'helm-ls-svn-status-transformer)
   (action :initform
           (helm-make-actions
            "Find file" 'helm-find-many-files
            "svn status" (lambda (_candidate)
                           (funcall helm-ls-svn-status-command
                                    (helm-default-directory)))))))

;;;###autoload
(defun helm-ls-svn-ls ()
  (interactive)
  (when (helm-ls-svn-not-inside-svn-repo)
    (user-error "Not under a svn repository"))
  (unless helm-source-ls-svn-buffers
    (setq helm-source-ls-svn-buffers
          (helm-make-source "Buffers in project" 'helm-source-buffers
            :header-name #'helm-ls-svn-header-name
            :buffer-list (lambda () (helm-browse-project-get-buffers
                                     (helm-ls-svn-root-dir))))))
  (unless helm-source-ls-svn
    (setq helm-source-ls-svn
          (helm-make-source "svn files" 'helm-ls-svn-source)))
  (unless helm-source-ls-svn-status
    (setq helm-source-ls-svn-status
          (helm-make-source "svn status" 'helm-ls-svn-status-source)))
  (helm :sources '(helm-source-ls-svn-status
                   helm-source-ls-svn-buffers
                   helm-source-ls-svn)
        :buffer "*helm ls svn*"))

(provide 'helm-ls-svn)
;;; helm-ls-svn.el ends here
