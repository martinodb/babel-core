# nlp-tools

This package presents an interface to the `Penelope' NLP components that were developed in the context of the Odycceus project. These components are basically the different [Spacy](https://spacy.io) functionalities wrapped in a Restful API. 

The interface functions are located in the file `penelope-interface.lisp`.

## For using the package with Lispworks on Windows

OpenSSL is not installed by default on Windows systems, but is needed by LispWorks to interact with web servers using HTTPS. You will need to install it manually, and point LispWorks to the adequate library files. This will solve the ‘libeay32.dll file not found’ error that you are getting. Follow these steps:

1. Download [OpenSSL](http://slproweb.com/download/Win64OpenSSL-1_1_0L.exe)  for Windows (64 bit) and run the installer. You might have to reboot your machine.
2. Add the following line to your .lispworks file. If the package comm is not found, move this line down in the file. Also, you might need to adapt the names of the dlls in your OpenSSL folder.
3. `(comm:set-ssl-library-path '("libssl-1_1-x64.dll" "libcrypto-1_1-x64.dll"))`

Now, restart Lispworks and try:

```
(ql:quickload :nlp-tools)
(in-package :nlp-tools)
(get-penelope-tokens "Paul kicked the ball.")
```
This should return:

```
("Paul" "kicked" "the" "ball" ".")
```
