;;; portfile.el --- Major mode for editing MacPorts Portfiles

;; Copyright (C) 2015  Chunyang Xu

;; Author: Chunyang Xu <chunyang@macports.org>
;; URL: https://svn.macports.org/repository/macports/users/chunyang/portfile.el/
;; Version: 0.1
;; Created: Tue Jun  9 03:14:33 CST 2015
;; Keywords: MacPorts Portfile

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
;;
;; `portfile.el' aims to provide a major mode for editing MacPorts
;; Portfiles. It's work in process and I consider it to be usable when the
;; following functions are implemented:
;;
;; - syntax highlight
;; - indent
;; - run lint
;;
;; A good reference is mpvim (http://svn.macports.org/repository/macports/contrib/mpvim/).


;;; Code:

(defvar portfile-required-list
  '("PortSystem" "name" "version" "maintainers"
    "homepage" "platforms"
    "master_sites" "master_sites-append" "master_sites-delete"
    "categories" "categories-append" "categories-delete"
    "description" "description-append"
    "long_description" "long_description-append"))

(defvar portfile-optional-list
  '("PortGroup" "epoch" "revision" "patch_sites"
    "license" "conflicts" "license_noconflict"
    "replaced_by" "supported_archs"
    "distfiles" "distfiles-append" "distfiles-delete"
    "distname" "dist_subdir" "worksrcdir"
    "installs_libs" "nextgroup=PortfileYesNo" "skipwhite"
    "depends_skip_archcheck" "depends_skip_archcheck-append" "depends_skip_archcheck-delete"
    "checksums" "checksums-append" "checksums-delete"))

(defvar portfile-mode-font-lock-keywords
  (list (cons (concat "\\_<"
                      (regexp-opt (append portfile-required-list
                                          portfile-optional-list) t) "\\_>")
              'font-lock-keyword-face)))

(define-derived-mode portfile-mode tcl-mode "portfile-mode"
  "Portfile mode"
  (font-lock-add-keywords nil portfile-mode-font-lock-keywords)
  (set (make-local-variable 'compile-command) "port lint --nitpick"))

;; Test:
;; (add-hook 'tcl-mode-hook (lambda ()
;;                            (when (string-equal (buffer-name) "Portfile")
;;                              (portfile-mode))))

(provide 'portfile)
;;; portfile.el ends here
