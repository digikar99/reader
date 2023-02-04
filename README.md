# Installation

Download from the [Releases section](https://github.com/digikar99/reader/releases).

> I'll promote v0.10.0 to v1.0.0 if there are no significant changes over the next year.

# Getting Started

```lisp
;; Enable reader macros using the following:
(reader:enable-reader-syntax &rest reader-macro-identifiers)
;; OR
(reader+swank:enable-package-local-reader-syntax &rest reader-macro-identifiers)

;; For instance, to enable reader macros for hash table and set:
(reader:enable-reader-syntax 'hash-table 'set)

;; The following gives you a list of identifiers:
(describe 'reader:enable-reader-syntax)

;; Complementory macros
;; (reader:disable-reader-syntax) ; is complementary
;; OR
;; (reader+swank-disable-package-local-reader-syntax)
```

# Examples

```lisp
CL-USER> #[[1 2 3
            4 5 6]
           [3 2 1
            7 8 9]]
#3A(((1 2 3) (4 5 6)) ((3 2 1) (7 8 9)))        ; cleaner syntax for arrays
CL-USER> (gethash 'a {eq 'a 1 'b 2})            ; hash-tables
1
T
CL-USER> (let ((reader:*set-function* 'hash-set:list-to-hs))
           (hash-set:hs-memberp (eval (read-from-string "#{\"a\" \"b\" \"c\"}"))
                                "c"))
T
CL-USER> #{'(1) '(1)} ; default is CL:REMOVE-DUPLICATES
((1) (1))
CL-USER> #{'(1) '(1) :test #'equal}
((1))
CL-USER> [#2A((1 2 3) (4 5 6)) 1 0]             ; accessors
4
CL-USER> (setf reader:*get-val-array-function* 'select:select)
SLCT:SELECT
CL-USER> (let ((arr #2A((1 2 3) (4 5 6))))
           [arr t 0])
#(1 4)
CL-USER> [{"a" 1 "b" 2} "a"]
1
T
CL-USER> [(cl-json:decode-json-from-string "{\"a\":1, \"b\":2}") :b] ; works with alists
2
CL-USER> (-> {"a" "apple" "b" "ball"} ["b"] [1]); accessors can be chained using ->, an arrow
                                                ; see https://github.com/hipeta/arrow-macros/
#\a
CL-USER> #!echo -n hello world
hello world                                     ; Captures until the end of line
NIL                                             ; Should be possible to capture into a string
NIL                                             ; but not ideal for speed
0
CL-USER> (let ((a t)) !a)                       ; not
NIL
CL-USER> (let ((a 5.0d0)) $a)                   ; write-to-string
"5.0d0"                   ; should this be "ensure-string"? raise an issue!
CL-USER> ?reader:enable-reader-syntax           ; variable use not intended
READER:ENABLE-READER-SYNTAX
  [symbol]
ENABLE-READER-SYNTAX names a macro:
  Lambda-list: (&REST READER::READER-MACRO-IDENTIFIERS)
  Documentation:
    READER-MACRO-IDENTIFIERS are any of the following symbols:
      GET-VAL, HASH-TABLE, NOT, STRING, DESCRIBE, ARRAY, SET, RUN-PROGRAM
  Source file: /home/user/quicklisp/local-projects/reader/reader.lisp
```
## Notes

The functions used for constructing arrays, hash-tables, sets, accessing array elements or accessors in general can be specified by

- `*array-function*`
- `*hash-table-function*`
- `*set-function*`
- `*get-val-array-function*`
- `*get-val-function*`

This should allow users to use [fset](https://github.com/slburson/fset) or [access](https://github.com/AccelerationNet/access) or other libraries for the respective functions.

By default, alists and plists are treated distinct from lists whenever possible (see the `get-val` method specialized on lists). To disable this behavior, set each of these to `T`:

- `*alists-are-lists*`
- `*plists-are-lists*`


### Hash Tables

- `{"a" 1 "b" 2}` - hash tables use `'equalp` as the default test. The intended test function  can be specified as the first element of the literal syntax. Besides providing with a choice, this also gets the indentation correct (see [Modification for emacs](#modifications-for-emacs).

### Setting alists and plists

- `(setf [] ...)` does not work with empty alists and plists. This seems to require setf-expanders with `get-val` generic-function.

### array vs get-val

- `[...]` (`get-val`) does not work inside `#[...]` (`array`) syntax. I do not have a plan or haven't figured out how to combine the two; and I find that okay since the `array` syntax is meant for cleaner representation in works involving matrices.

## Notes for existing users upgrading to v0.10.0 from v0.9.1

### Lambda

- `lambda` with the following usage has been removed.

```lisp
CL-USER> (mapcar λ(write-to-string -) '(2 3 4)) ; lambdas
("2" "3" "4")
```

This begins to feel like line-noise after a while and is not ideal for readability; a better equivalent is the simple macro

```lisp
CL-USER> (defmacro lm (&rest var-body)
           `(lambda ,(butlast var-body)
              ,@(last var-body)))
LM
CL-USER> (mapcar (lm o (write-to-string o)) '(2 3 4))
("2" "3" "4")
```

### Hash-table

Since the function used for constructing hash-tables is now configurable (via `reader:*hash-table-function*`), the first input to the hash-table may be interpreted as the equality function. Thus, hash-table syntax can take an even (with default test `cl:equalp`) or odd (with the first symbol being interpreted as the equality function) number of arguments.


### Setting alists and plists

Now works for non-empty lists.

## Testing

```lisp
(ql:quickload "reader")
(5am:run :reader)
;; OR
(asdf:test-system "reader")
```

# Emacs Indentation Support

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
{equal "hello" 1
       "world" 2}
{"hello" 1 "world" 2}
```

# Comments

- [`cl-interpol`](http://edicl.github.io/cl-interpol/) is a prominent library providing perl / shell-like string interpolation facilities.
- [`cl-json`](https://common-lisp.net/project/cl-json/cl-json.html) can be used for parsing JSON.
