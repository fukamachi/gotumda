# Gotumda - Put all your tasks into one bucket.

Gotumda is a web application to manage your tasks.

Though this application is tested on Clozure CL v1.6, this might work on other Lisp such as SBCL.

As this application's frontend is using Google Closure Library, Python and Java are needed to start the server.

## Usage

Gotumda is written in Common Lisp.

    (ql:quickload :gotumda)
    (gotumda:start)

## Installation

I'm afraid, but, I bet you cannot run Gotumda on your machine. Gotumda only works on the latest version of [Caveman](https://github.com/fukamachi/caveman). If you really would like to run this app, you should checkout this and Caveman first.

## Screenshot

<img src="https://github.com/fukamachi/gotumda/raw/master/screenshot.png" alt="Screenshot" title="Screenshot" />

## Dependency

* Clozure CL (or other CL implementation)
* Python v2.7
* Java v1.6
* the latest [Caveman](https://github.com/fukamachi/caveman)

## Restriction

* No user authorization
* Editing tasks isn't ready

## Author

* Eitarow Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2011 Eitarow Fukamachi (e.arrows@gmail.com)

# License

Licensed under the Apache License 2.0.
