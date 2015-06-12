;;; helm-ls-svn.el --- list svn files                -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Chunyang Xu

;; Author: Chunyang Xu <xuchunyang56@gmail.com>
;; Created: Wed Jun 10 20:58:26 CST 2015
;; Version: 0.1
;; URL: https://github.com/xuchunyang/helm-ls-svn
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

;;; Code:

(require 'helm-files)

(defgroup helm-ls-svn nil
  "Helm completion for svn repos."
  :group 'helm)

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

;;;###autoload
(defun helm-ls-svn-ls ()
  (interactive)
  (when (helm-ls-svn-not-inside-svn-repo)
    (user-error "Not under a svn repository"))
  (helm :sources
        (helm-build-in-buffer-source "SVN files"
          :header-name (lambda (name) (format "%s (%s)" name (helm-ls-svn-branch)))
          :init
          (lambda ()
            (let ((root (helm-ls-svn-root-dir)))
              (with-current-buffer (helm-candidate-buffer 'global)
                (call-process-shell-command
                 (format "find %s -type f -not -iwholename '*.svn/*'"
                         root)
                 nil t ))))
          :help-message helm-generic-file-help-message
          :keymap helm-ls-svn-map
          :candidate-number-limit 9999
          :action (helm-actions-from-type-file))
        :buffer "*helm ls svn*"))

(provide 'helm-ls-svn)
;;; helm-ls-svn.el ends here
