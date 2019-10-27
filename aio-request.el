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
(defun aio-request (url &rest settings)
  "Provides an aio wrapper from the request library.

Works like the request library except that a promise is
returned. This promise resolves to either an error or the
response object. The response object is described by the
request.el documentation.

Like in request URL is the url to be fetched and SETTINGS is a
property list with settings. See the documentation for request
for details. Note that the properties containing
callbacks (:complete, :success and :error) cannot be used as they
are reserved for resolving the promise and are pointless under
async/await semantics.

See the documentation for the aio.el library for information on
how to use the returned promise object.

Example:
  (let ((response (aio-wait-for (aio-request \"google.com\"))))
     ...)
"
  (when (or (plist-get settings :complete)
            (plist-get settings :success)
            (plist-get settings :error))
    (user-error "The :complete, :success, and :error callbacks doesn't make sense with request-aio"))
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
