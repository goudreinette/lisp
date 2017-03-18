# Lisp

My WIP Lisp implementation, used to try out ideas and better understand the design decisions involved in creating languages.
The primary design goal is a simple, minimal and readable core.

### Currently supported features:
* Haskeline-based REPL
* Error messages
* Enviroment inspection: `(env)`
* Require-ing other files
* Macro's
* Lamda shorthand: `#{* 2 %}`

### Planned features:
* Variable arguments
* Comments
* Even smaller core


### Try it

    $ git clone https://github.com/reinvdwoerd/lisp
    $ cd ./lisp
    $ stack install
    $ stack exec lisp

    lisp=> (require lisp/core)