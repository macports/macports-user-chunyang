;;; port.el --- a Emacs interface for MacPorts port(1)  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Chunyang Xu

;; Author: Chunyang Xu <chunyang@macports.org>
;; Version: 0.01
;; Package-Requires: ((emacs "24.1"))
;; Keywords: MacPorts

;; Simplified BSD License:
;;
;; Redistribution and use in source and binary forms, with or
;; without modification, are permitted provided that the following
;; conditions are met:
;;
;;    1. Redistributions of source code must retain the above
;;       copyright notice, this list of conditions and the following
;;       disclaimer.
;;
;;    2. Redistributions in binary form must reproduce the above
;;       copyright notice, this list of conditions and the following
;;       disclaimer in the documentation and/or other materials
;;       provided with the distribution.
;;
;; This software is provided by Chunyang Xu "AS IS" and any express
;; or implied warranties, including, but not limited to, the implied
;; warranties of merchantability and fitness for a particular
;; purpose are disclaimed.  In no event shall Chunyang Xu or
;; contributors be liable for any direct, indirect, incidental,
;; special, exemplary, or consequential damages (including, but not
;; limited to, procurement of substitute goods or services; loss of
;; use, data, or profits; or business interruption) however caused
;; and on any theory of liability, whether in contract, strict
;; liability, or tort (including negligence or otherwise) arising in
;; any way out of the use of this software, even if advised of the
;; possibility of such damage.
;;
;; The views and conclusions contained in the software and
;; documentation are those of the authors and should not be
;; interpreted as representing official policies, either expressed
;; or implied, of Chunyang Xu.

;;; Commentary:

;; Latest version can be found at:
;; https://svn.macports.org/repository/macports/users/chunyang/port.el/

;; NOTE: Use the command line program `port' is too slow.

;;; Code:

(require 'tabulated-list)

(define-derived-mode port-menu-mode tabulated-list-mode "MacPorts Port Menu"
  "Major mode browsing a list of ports.
Letters do not insert themselves; instead, they are commands. "
  (setq tabulated-list-format `[("Package" 18 nil)
                                ("Version" 28 nil)
                                ("Status"  11 nil)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Status" nil))
  (tabulated-list-init-header))

;;;###autoload
(defun port-list-ports ()
  (interactive)
  (let ((buf (get-buffer-create "*Ports*")))
    (with-current-buffer buf
      (port-menu-mode)
      (setq tabulated-list-entries
            (mapcar
             (lambda (item)
               (let ((port (split-string (string-trim item))))
                 (list nil (vector (nth 0 port)
                                   (nth 1 port)
                                   (let ((status (nth 2 port)))
                                     (if status (substring status 1 -1)
                                       "deactivated"))))))
             (butlast
              (split-string
               (shell-command-to-string "port -q installed requested")
               "\n"))))
      (tabulated-list-print t)
      (switch-to-buffer buf))))

;;;###autoload
(defun helm-port ()
  (interactive)
  (and
   (require 'helm nil t)
   (require 'subr-x nil t)
   (helm :sources
         (helm-build-sync-source "MacPorts"
           :candidates (lambda ()
                         (mapcar
                          #'string-trim
                          (butlast
                           (split-string
                            (shell-command-to-string "port -q installed requested")
                            "\n"))))
           :action '(("Go to home page" .
                      (lambda (candidate)
                        (when-let ((string candidate) (regexp " (active)")
                                   (index (string-match regexp string)))
                          (setq candidate (substring string 0 index)))
                        (shell-command (concat "port gohome " candidate)))))))))

(provide 'port)
;;; port.el ends here
