# Lisp

My WIP Lisp implementation, used to try out ideas and better understand the design decisions involved in creating languages.
The primary design goal is a simple, minimal and readable core.
Implementation is explained on my [blog](http://reinvanderwoerd.nl/blog/2017/03/18/writing-a-lisp/).

### Currently supported features:
* Haskeline-based REPL
* Error messages
* Enviroment inspection: `(env)`
* Require-ing other files
* Macro's
* Lamda shorthand: `#{* 2 %}`
* Variable arguments


### Planned features:
* Comments
* Even smaller core


### Try it

    $ git clone https://github.com/reinvdwoerd/lisp
    $ cd ./lisp
    $ stack install
    $ stack exec lisp

    lisp=> (require lisp/core)
