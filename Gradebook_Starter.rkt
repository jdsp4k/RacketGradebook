#lang racket
(require racket/port) 
; standard Racket. We're going to be doing some I/O other than reading from 
; keyboard or printing to screen. 


; Given: A text file containing an unknown number of student names (first, last pairs)
; with 3 exam scores for each, print a list sorted by last names of
; first   last    average

; convert each line (a string) to list of strings, breaking on whitespace
(define (line-to-list stringlist)
   ; Given a list and a string, split the string into a list of substrings. 
   ; append this list onto the list that's passed in, returning the updated 
   ; list  
   (define (one-string string outlist)
    (define tmp (list (string-split string)))
    (append outlist tmp))
	; iterate through the list of strings, 1 at a time. Tail recursive, of course. 
  (define (iter strings outlist)
    (if (empty? strings) outlist
        (iter (rest strings) (one-string (first strings) outlist))))
	; main function passes all of our data and an empty list to start the process 
  (iter stringlist '()))
  
; sort by last name (2nd item), breaking ties on first name (first item),
; bringing everything else along for the ride

; for convenience, functions to pull out first name, and last name, and 
; sorts-as name, consisting of "lastname firstname"
(define (lastname stringlist)
    (first (rest stringlist)))
(define (firstname stringlist)
    (first stringlist))
(define (sortname stringlist)
  (string-append (lastname stringlist) " " (firstname stringlist)))

; which of 2 strings is smaller? Allow (require) user to pass in the 
; comparison function to be used. So "smaller" means "comes earlier based
; on whatever the passed-in function does." 
(define (min-str str1 str2 lt-func)
  (if (lt-func str1 str2) str1 str2))

; given a list of names, and a function to pull the sort key from the data, 
; find the smallest (earliest) name in the list. string-ci<? is a library 
; function--string comparison, case insensitive, less-than comparison. 
(define (min-name lst sel)
  (define (iter lst m)
    (cond
      [(empty? lst) m]
      [else  (iter (rest lst) (min-str m (sel (first lst)) string-ci<?))]))
  (iter lst "zzzzzzzzzzzzzzzzz"))


; selection sort. (Yes, just as inefficient as any other selection sort, but 
; this is a small data set. If list is empty or 1 item, return it. Otherwise 
; find earliest name; filter out all records with that name (in the general case, 
; there may be duplicates), append it to the sorted list of everything with a 
; name larger than the minimum. Note that this is NOT tail-recursive and should 
; not be used for large lists. 
(define (sort-name lst)
  (if (< (length lst) 2)
      lst
      (let
          ([m (min-name lst sortname)])
          (append (filter (lambda (x) (string-ci=? (sortname x) m)) lst)
                  (sort-name (filter (lambda (x) (string-ci<? m (sortname x))) lst))))))

; Globalize average function.
; Take a list of ints and return the average of that list
(define (avg-scores lst)
  (let ([nums (map string->number lst)])
    (exact->inexact (/ (apply + nums) (length lst)))))

; Takes a list and recursively finds the average of each sublist
(define (r-avg lst [ret empty])
  (if (empty? lst)
      ret
      (r-avg (rest lst) (cons (avg-scores (first lst)) ret))))

; Weighted average function.
; Take a list of averages, a list of weights, and a list of value, returns a weighted average.
(define (weighted-average scores weight value)
  (apply + (map * (map / scores value) weight)))

; Slices a list from n to m.
(define (slice n m lst [p 0] [ret empty])
  (if (< p n)
      (slice n m (rest lst) (+ p 1) ret)
      (if (< p m)
          (slice n m (rest lst) (+ p 1) (cons (first lst) ret))
          ret)))

; Takes a list, adds the first two items, consolodates the result back into the list
(define (collapse lst)
  (cons (+ (first lst) (second lst)) (rest (rest lst))))

; Takes a list of bounds and a list, and returns the list split at those bounds
(define (r-slice bounds-list lst [ret-list empty])
  (if (< (length bounds-list) 2)
      ret-list
      (r-slice (collapse bounds-list) lst (append ret-list (list (slice (first bounds-list) (first (collapse bounds-list)) lst))))))

; Takes a list of strings representing numbers and returns the weighted average. 
(define (compute-avgs lst [cat-counts (list 2 5 3)] [weights (list 0.35 0.65)] [values (list 20 100)])
  (if (empty? lst)
      empty
      (let
          ([m (first lst)]
          [n (first (rest lst))])
          (list n m (weighted-average (reverse (r-avg (r-slice cat-counts lst))) weights values)))))

; Takes a grade (in percentage form) and returns the associated letter grade.
; Optionally takes lists of limits and letter grades. If the percentage grade
; is less than the first limit, returns the first item in grade, otherwise
; remove the first item of both and recurse.
(define (letter-grade per-grade [limits '(90 80 70 60)] [grade '("A" "B" "C" "D" "F")])
  (if (empty? limits)
      (first grade)
      (if (< dec-grade (first limits))
          (letter-grade dec-grade (rest limits) (rest grade))
          (first grade))))

; output preparation. Tail recursive. Take the first list-of-strings, 
; build the first name, last name, convert the average to a string, finish 
; with newline. Add the 'line' string to our growing output string, iterate to 
; next line. After last line, our output is in one big string. 
(define (prep-for-file lst)
  (define (iter lst so-far)
    (if (empty? lst)
        so-far
        (letrec (
                 [line (first lst)]
                 [outline (string-append (car line) ", " (cadr line) ": " (real->decimal-string (* 100 (caddr line))) "% " (per-grade (* 100 (caddr line))) "\n")])
          (iter (rest lst) (string-append so-far outline)))))
  (iter lst ""))


;  MAIN PROGRAM

; this (probably over-complicated) line opens the file in text mode, reading 
; the input as one big string; then splits the string on newlines; then converts 
; our list-of-strings to a list of lists, each sublist being a list of strings: 
; ("first" "last" "score1" "score2" "score3")
(define workinglist (line-to-list (string-split (first (port->lines  
   (open-input-file "input1.txt"
                       #:mode 'text) #:line-mode 'return)) "\n")))
; compute averages, sort by name, save that list. 
(define printoutlist (sort-name (map compute-avgs workinglist)))
; open output file, text mode, replacing old file if one already exists. 
(define out (open-output-file "output1.txt" #:mode 'text #:exists 'replace))
; send to output
(display (prep-for-file printoutlist) out)
; close file. 
(close-output-port out)