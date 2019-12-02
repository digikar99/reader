# Examples

```lisp
(named-readtables:in-readtables reader:reader)
;;  OR 
(reader:enable-reader-syntax &rest reader-macro-identifiers) ; check (describe)
;; another way, until SLIME allows changing readtables more than twice
;; (disable-reader-syntax) ; is complementary
```




```lisp
CL-USER> (mapcar λ(write-to-string -) '(2 3 4)) ; lambdas
("2" "3" "4")
CL-USER> #[λ(+ - --) '(2 3 4) '(4 5 6)]         ; generic-cl:map (lists or vectors)
(6 8 10)
CL-USER> (gethash 'a {:eq 'a 1 'b 2})           ; hash-tables
1
T
CL-USER> (hash-set:hs-memberp #{"a" "b" "c"} "c") ; hash-set
T
CL-USER> [#2A((1 2 3) (4 5 6)) 1 0]             ; accessors
4
CL-USER> (let ((arr (numcl:asarray #2A((1 2 3) (4 5 6)))))
           [arr t 0])
#(1 4)                  ; simple-arrays use aref and arrays use numcl:aref
CL-USER> [{"a" 1 "b" 2} "a"]                    ; accessors
1
T
CL-USER> [(cl-json:decode-json-from-string "{\"a\":1, \"b\":2}") :b]
2                                               ; works with alists
CL-USER> [{"a" "apple" "b" "ball"} "b" 1]       ; accessors can be chained
#\a
```

See [reader-test.lisp](reader-test.lisp) for more examples.

## Notes

- `{"a" 1 "b" 2}` - hash tables use `'equal` as the default test. The intended test function (one of `(:eq :eql :equalp :equal)` can be specified as the first element of the literal syntax; conversely, these cannot be used as the first keys of the hash table. Besides providing with a choice, this also gets the indentation correct (see [Modification for emacs](#modifications-for-emacs).
- `(setf [] ...)` does not work with alists and plists. Modifying alists and plists destructively would likely require compiler macros.
- `λ` can take up to 3 arguments, and the remaining are captured by `&rest args`. Further, an optional integer (from 0 to 3 both inclusive) can be put in front of `λ` to indicate the number of arguments before `&rest args`: `(λ2(identity args) 1 2 3 4) => (3 4)` vs `(λ(identity args) 1 2 3 4) => (1 2 3 4)`

## Testing

```lisp
(ql:quickload :reader-test)
(prove:run-test-package :reader-test)
```

# Modifications for emacs

```lisp
(modify-syntax-entry ?\[ "(]" lisp-mode-syntax-table)
(modify-syntax-entry ?\] ")[" lisp-mode-syntax-table)
(modify-syntax-entry ?\{ "(}" lisp-mode-syntax-table)
(modify-syntax-entry ?\} "){" lisp-mode-syntax-table)
(define-key paredit-mode-map (kbd "{") 'paredit-open-curly)
(define-key paredit-mode-map (kbd "}") 'paredit-close-curly)
(global-set-key (kbd "C-S-l") (lambda () ; for inserting lambda
                                (interactive)
                                (insert-char (aref (symbol-name 'λ) 0))))
(setq paredit-space-for-delimiter-predicates
      (list (lambda (endp delimiter)
              (not (and (eql ?\( delimiter)
                        (member (char-before (point))
                                '(?\@ ?λ))))))))
(set-language-environment "UTF-8")
```

Motivation: [paredit curly brace matching in swank-clojure repl - Stack OverFlow](https://stackoverflow.com/questions/8598116/paredit-curly-brace-matching-in-swank-clojure-repl)

## Known Issues

Known issues include aligning hash-tables without first-element-as-key:

```lisp
{"hello" 1
         "world" 2}
```

A work-around is to specify the test as the first element, and let that default exist for one-liners:

```lisp
{:equal "hello" 1
        "world" 2}
{"hello" 1 "world" 2}
```

# Comments

I'm aware of the [lambda syntax](https://github.com/vseloved/rutils/blob/master/docs/tutorial.md) 
in rutils and clojure. However, emacs seems to treat `^` specially and something like
`(member ?\^ '(?\^))` gives an "End of file during parsing" error. This is an issue with
the modification marked `; for inserting lambda`. Regarding `-` instead of `%1`: the former
is easier to type than the latter. The `λ()` can take up to 3 (optional) arguments `- -- ---`.
