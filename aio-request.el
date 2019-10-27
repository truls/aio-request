;;; aio-request.el --- Wrap request.el in aio -*- lexical-binding: t -*-

;; Copyright (C) 2019 Truls Asheim

;; Author: Truls Asheim <truls@asheim.dk>
;; Maintainer: Truls Asheim <truls@asheim.dk>
;; Created: 21 Aug 2019
;; Keywords: lisp comm
;; Homepage: https://github.com/truls/aio-request
;; Package-Version: 0.1
;; Package-Requires: ((aio "1.0") (request "0.3.1") (emacs "24.3"))

;;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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
;; Wraps the request library such that it can be used to build
;; asynchronous code using the aio library.

;;; Code:

(require 'cl-lib)
(require 'aio)
(require 'request)

;;;###autoload
(cl-defun aio-request (url &rest settings
                           &key
                           (type "GET")
                           (params nil)
                           (data nil)
                           (files nil)
                           (parser nil)
                           (headers nil)
                           (encoding 'utf-8)
                           (timeout request-timeout)
                           (status-code nil)
                           (sync nil)
                           (response (make-request-response))
                           (unix-socket nil))
  "An aio wrapper for the request package.

URL is the url to be fetched and SETTINGS is a property list with
settings.  Refer to the documentation for the request package for
information about available settings.  Note that the properties
for specifying callbacks (:complete, :success and :error) are
unavailable as they are reserved internally for promise
resolution.  Further, they do not serve any purpose when the
`request' function is called through this wrapper.

This function returns a promise which resolves to either an error
or the response object.  Refer to the `request' package
documentation for details about the response object.

See the documentation for the `aio' package for information about
how to use the returned promise object.

Example:
  (let ((response (aio-wait-for (aio-request \"google.com\"))))
     ...)"
  (let* ((promise (aio-promise))
         (settings
          (plist-put
           settings
           :error
           (cl-function
            (lambda (&key
                symbol-status
                error-thrown
                &allow-other-keys)
              (aio-resolve promise
                           (lambda () (signal symbol-status error-thrown)))))))
         (settings
          (plist-put
           settings
           :success
           (cl-function
            (lambda (&key response &allow-other-keys)
              (aio-resolve promise (lambda () response)))))))
    (prog1
        promise
      (apply #'request url settings))))

(provide 'aio-request)

;;; aio-request.el ends here
