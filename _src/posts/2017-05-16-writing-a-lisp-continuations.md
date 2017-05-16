    Title: Writing a Lisp: Continuations
    Date: 2017-05-16T14:07:54
    Tags: DRAFT

_Replace this with your post text. Add one or more comma-separated
Tags above. The special tag `DRAFT` will prevent the post from being
published._

<!-- more -->

```
(+ 1 (+ 2 (+ 3 (+ (call/cc (lambda (here) 
                             (here 20) 4))
                  5))))
```