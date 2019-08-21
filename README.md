# `aio` wrapper for `requests`

This package provide the `aio-requests` which is a wrapper around
`requests` returning a promise. This promise resolves to either an
    error or the response object. The response object is describe by the
request documentation.

Like in request URL is the url to be fetched and SETTINGS is a
property list with settings. See the documentation for request
for details. Note that the properties containing
callbacks (:complete, :success and :error) cannot be used as they
are reserved for resolving the promise and are pointless under
async/await semantics.

See the documentation for the aio library for information on how
to use the returned promise object.

Example:
```
    (let ((response (aio-wait-for (aio-request \"google.com\"))))
     ...)
```
