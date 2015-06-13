;;; helm-ls-svn.el --- list svn files                -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Chunyang Xu

;; Author: Chunyang Xu <chunyang@macports.org>
;; Created: Wed Jun 10 20:58:26 CST 2015
;; Version: 0.1
;; URL: https://trac.macports.org/browser/users/chunyang/helm-ls-svn.el
;; Package-Requires: ((emacs "24.1") (helm "1.7.0") (cl-lib "0.5"))
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
;; Installation
;; ============
;;
;; To install, make sure this file is saved in a directory in your `load-path',
;; and add the line:
;;
;;   (require 'helm-ls-svn)
;;
;; to your Emacs initialization file.
;;
;;
;; Usage
;; =====
;;
;; By calling helm-ls-svn-ls in any buffer that is a part of a svn repo, you
;; will be presented with a corresponding helm buffer containing a list of all
;; the buffers/files currently in that same repository,
;;
;;
;; Reporting Bugs
;; ==============
;;
;; Bug report, suggestion and patch are welcome. Please email me (see above to
;; get my email address).
;;
;;
;; TODO
;; ====
;;
;; - Implement persistent action (diff) for status source.
;; - Helm-find-files integration.
;; - Submit to melpa.
;; - Find out a suitable way to search in svn project.
;; - Improve performance.

;;; Code:

(require 'cl-lib)
(require 'vc)
(require 'vc-svn)
(require 'helm-files)

;; Define the sources.
(defvar helm-source-ls-svn-status nil)
(defvar helm-source-ls-svn nil)
(defvar helm-source-ls-svn-buffers nil)


(defgroup helm-ls-svn nil
  "Helm completion for svn repos."
  :group 'helm
  :link '(emacs-commentary-link :tag "commentary" "helm-ls-svn.el")
  :link '(emacs-library-link :tag "lisp file" "helm-ls-svn.el")
  :link '(url-link :tag "web page" "https://trac.macports.org/browser/users/chunyang/helm-ls-svn.el"))

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

(defun helm-ls-svn-collect-data()
  (let ((root (helm-ls-svn-root-dir)))
    (with-current-buffer (helm-candidate-buffer 'global)
      (let ((default-directory root))
        (cl-remove-if
         (lambda (item) (or (null item)
                            (file-directory-p item)))
         (mapcar (lambda (item) (car (last (split-string item))))
                 (split-string
                  (shell-command-to-string
                   "svn status -non-interactive --quiet --verbose")
                  "\n")))))))

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

(defun helm-ls-svn-diff (candidate)
  (find-file candidate)
  (call-interactively #'vc-diff))

(defun helm-ls-svn-revert (_candidate)
  (let ((marked (helm-marked-candidates)))
    (cl-loop for f in marked do
             (progn
               (vc-svn-revert f)
               (helm-aif (get-file-buffer f)
                   (with-current-buffer it
                     (revert-buffer t t)))))))

(defun helm-ls-svn-status-action-transformer (actions _candidate)
  (let ((disp (helm-get-selection nil t)))
    (cond ((string-match "^?" disp)
           (append actions
                   (helm-make-actions
                    "Add files(s)"
                    (lambda (candidate)
                      (let ((default-directory
                              (file-name-directory candidate))
                            (marked (helm-marked-candidates)))
                        (vc-call-backend 'SVN 'register marked)))
                    "Delete file(s)"
                    #'helm-delete-marked-files)))
          ((string-match "^M" disp)
           (append actions
                   (helm-make-actions
                    "Diff file" #'helm-ls-svn-diff
                    "Commit file(s)"
                    (lambda (_candidate)
                      (let* ((marked (helm-marked-candidates))
                             (default-directory
                               (file-name-directory (car marked))))
                        (vc-checkin marked 'SVN)))
                    "Revert file(s)" #'helm-ls-svn-revert
                    "Copy file(s) `C-u to follow'" #'helm-find-files-copy
                    "Rename file(s) `C-u to follow'" #'helm-find-files-rename)))
          ((string-match "^A" disp)
           (append actions
                   (helm-make-actions
                    "svn delete" #'vc-svn-delete-file
                    "Revert file(s)" #'helm-ls-svn-revert)))
          (t actions))))

(defclass helm-ls-svn-source (helm-source-in-buffer)
  ((header-name :initform 'helm-ls-svn-header-name)
   (data :initform 'helm-ls-svn-collect-data)
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
   ;; TODO: Implement persistent action
   ;; (persistent-action :initform 'helm-ls-svn-diff)
   ;; (persistent-help :initform "Diff")
   (action-transformer :initform 'helm-ls-svn-status-action-transformer)
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
