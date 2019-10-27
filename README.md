# `aio` wrapper for `request.el`

This package provides a wrapper around the `request.el` package
allowing it to be used with the
[aio](https://github.com/skeeto/emacs-aio) library.

This package provides a single function `aio-request` which is used as
follows:

    (defun url settings)
URL is the url to be fetched and SETTINGS is a property list with
settings.  Refer to the documentation for the request package for
information about available settings.  Note that the properties
for specifying callbacks (`:complete`, `:success` and `:error`) are
unavailable as they are reserved internally for promise
resolution.  Further, they do not serve any purpose when the
`request` function is called through this wrapper.

This function returns a promise which resolves to either an error
or the response object.  Refer to the `request,el` package
documentation for details about the response object.

See the documentation for the `aio` package for information about
how to use the returned promise object.

Example:

    (let ((response (aio-wait-for (aio-request \"google.com\"))))
       ...)"
