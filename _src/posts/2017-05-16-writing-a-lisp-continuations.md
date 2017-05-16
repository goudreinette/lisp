    Title: Writing a Lisp: Continuations
    Date: 2017-05-16T14:07:54
    Tags: DRAFT

_Replace this with your post text. Add one or more comma-separated
Tags above. The special tag `DRAFT` will prevent the post from being
published._

<!-- more -->

```scheme
(+ 1 (+ 2 (+ 3 (+ (call/cc (lambda (here) 
                             (here 20) 4))
                  5))))
```



```scheme
(define (return-test)
  (call/cc 
    (lambda (return)
      1 (return 2) 3)))
```


```haskell
callCC env [l] = do
  callback <- eval env l
  cont <- makeCont
  eval env (List [callback, cont])
  where makeCont = do
          contFnBody <- topFrame >>= walk replaceContForm
          return $ makeFn False Anonymous [Symbol "x"] [List [shortCircuit', contFnBody]] env

        extractCallframe (Callframe val) =
          val

        topFrame = do
          stack <- State.get
          return $ fromJust $ find containsCallCCForm $ map extractCallframe $ reverse stack

        containsCallCCForm val =
                  case val of
                    List [Symbol "call/cc", _] -> True
                    List xs                    -> any containsCallCCForm xs
                    _                          -> False

        replaceContForm val =
          return $ case val of
            List [Symbol "call/cc", _] -> Symbol "x"
            _                          -> val
```