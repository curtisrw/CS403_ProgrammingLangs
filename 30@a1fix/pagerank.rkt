#lang racket

;; Assignment 1: Implementing PageRank
;;
;; PageRank is a popular graph algorithm used for information
;; retrieval and was first popularized as an algorithm powering
;; the Google search engine. Details of the PageRank algorithm will be
;; discussed in class. Here, you will implement several functions that
;; implement the PageRank algorithm in Racket.
;;
;; Hints: 
;; 
;; - For this assignment, you may assume that no graph will include
;; any "self-links" (pages that link to themselves) and that each page
;; will link to at least one other page.
;;
;; - you can use the code in `testing-facilities.rkt` to help generate
;; test input graphs for the project. The test suite was generated
;; using those functions.
;;
;; - You may want to define "helper functions" to break up complicated
;; function definitions.

(provide graph?
         pagerank?
         num-pages
         num-links
         get-backlinks
         mk-initial-pagerank
         step-pagerank
         iterate-pagerank-until
         rank-pages)

;; This program accepts graphs as input. Graphs are represented as a
;; list of links, where each link is a list `(,src ,dst) that signals
;; page src links to page dst.
;; (-> any? boolean?)
(define (graph? glst)
  (and (list? glst)
       (andmap
        (lambda (element)
          (match element
                 [`(,(? symbol? src) ,(? symbol? dst)) #t]
                 [else #f]))
        glst)))

;; Our implementation takes input graphs and turns them into
;; PageRanks. A PageRank is a Racket hash-map that maps pages (each 
;; represented as a Racket symbol) to their corresponding weights,
;; where those weights must sum to 1 (over the whole map).
;; A PageRank encodes a discrete probability distribution over pages.
;;
;; The test graphs for this assignment adhere to several constraints:
;; + There are no "terminal" nodes. All nodes link to at least one
;; other node.
;; + There are no "self-edges," i.e., there will never be an edge `(n0
;; n0).
;; + To maintain consistenty with the last two facts, each graph will
;; have at least two nodes.
;; + There will be no "repeat" edges. I.e., if `(n0 n1) appears once
;; in the graph, it will not appear a second time.
;;
;; (-> any? boolean?)
(define (pagerank? pr)
  (and (hash? pr)
       (andmap symbol? (hash-keys pr))
       (andmap rational? (hash-values pr))
       ;; All the values in the PageRank must sum to 1. I.e., the
       ;; PageRank forms a probability distribution.
       (= 1 (foldl + 0 (hash-values pr)))))

;; Takes some input graph and computes the number of pages in the
;; graph. For example, the graph '((n0 n1) (n1 n2)) has 3 pages, n0,
;; n1, and n2.
;;
;; (-> graph? nonnegative-integer?)

;; ************** FINISHED ***************

(define (num-pages graph)
    ; first check to see if graphs are present
  (if (list? graph)
      ; flatten the graph to remove parens
      ; remove the duplicate pages
      (length (remove-duplicates (flatten graph)))
      #f))



;; Takes some input graph and computes the number of links emanating
;; from page. For example, (num-links '((n0 n1) (n1 n0) (n0 n2)) 'n0)
;; should return 2, as 'n0 links to 'n1 and 'n2.
;;
;; (-> graph? symbol? nonnegative-integer?)

;; ************** FINISHED ***************

(define (num-links graph page)
  (define (loop graph l)
    (match graph
      [`() (set->list l)]
      [`((,p0 ,p1) . ,rst)
       (if (equal? p0 page)
           (loop rst (cons p1 l))
           (loop rst l))]))
 (length (loop graph '())))



;; Calculates a set of pages that link to page within graph. For
;; example, (get-backlinks '((n0 n1) (n1 n2) (n0 n2)) n2) should
;; return (set 'n0 'n1).
;;
;; (-> graph? symbol? (set/c symbol?))

;; ************** FINISHED ***************


(define (get-backlinks graph page)
  (define (loop graph l)
    (match graph
      [`() (set->list l)]
      [`((,p0 ,p1) . ,rst)
       (if (equal? p1 page)
           (loop rst (cons p0 l))
           (loop rst l))]))
  (loop graph '()))



;; Generate an initial pagerank for the input graph g. The returned
;; PageRank must satisfy pagerank?, and each value of the hash must be
;; equal to (/ 1 N), where N is the number of pages in the given
;; graph.
;; (-> graph? pagerank?)

;; ************** FINISHED ***************

(define (mk-initial-pagerank graph)
  ; (mk-initial-pagerank '((n2 n0) (n1 n4) (n4 n0) (n1 n3) (n2 n1) (n0 n1) (n3 n4) (n0 n4) (n4 n1) (n4 n2) (n1 n0)))
  ; Create a list of pages that is unordered and removes duplicates
  (define (append-graph-list)
    (remove-duplicates (flatten graph)))
  
  ; find the length of the list so that you can calculate initial pagerank
  (define (length-list)
    (length (append-graph-list)))

  ; create a recursive call that formulates a hash-set of 
  (define (recursive-pr lst)
    (if (null? lst)
        (hash)
        (hash-set (recursive-pr (rest lst)) (first lst) (/ 1 (length-list)))))
        
  (recursive-pr (append-graph-list)))


;; CODE THAT DIDN'T WORK BELOW

; ***************************************************************************************************************

   ; [pr-list '()])
      ; (append* (list (car (append-graph-list))) (list (/ 1 (length-list))))
      ; (remove first (append-graph-list))))

  ; (add-pr))
;    (cond
;      [(null? append-graph-list) null]
;      [else (add-pr (cons (car (append-graph-list)) (/ 1 (length-list))))]))
;  (add-pr graph))
  ; (add-pr (length (append-graph-list))))

; must find a way to sort list from least to greatest and add in initial pagerank (/ 1 N) after each element
    
;    (cond
;    [(empty? append-graph-list) empty]
;    [else (first (append-graph-list)) (sort (rest (append-graph-list)) <)]))
  
  
; ***************************************************************************************************************



;; Perform one step of PageRank on the specified graph. Return a new
;; PageRank with updated values after running the PageRank
;; calculation. The next iteration's PageRank is calculated as
;;
;; NextPageRank(page-i) = (1 - d) / N + d * S
;;
;; Where:
;;  + d is a specified "dampening factor." in range [0,1]; e.g., 0.85
;;  + N is the number of pages in the graph
;;  + S is the sum of P(page-j) for all page-j.
;;  + P(page-j) is CurrentPageRank(page-j)/NumLinks(page-j)
;;  + NumLinks(page-j) is the number of outbound links of page-j
;;  (i.e., the number of pages to which page-j has links).
;;
;; (-> pagerank? rational? graph? pagerank?)

;; ************** THIS STILL NEEDS WORK ***************

(define (step-pagerank pr d graph)
  'todo)






;; Iterate PageRank until the largest change in any page's rank is
;; smaller than a specified delta.
;;
;; (-> pagerank? rational? graph? rational? pagerank?)

;; ************** THIS STILL NEEDS WORK ***************

(define (iterate-pagerank-until pr d graph delta)
  (for/list ([i (length lst)] #:when (even? i))
      (list-ref lst i)))








;; Given a PageRank, returns the list of pages it contains in ranked
;; order (from least-popular to most-popular) as a list. You may
;; assume that the none of the pages in the pagerank have the same
;; value (i.e., there will be no ambiguity in ranking)
;;
;; (-> pagerank? (listof symbol?))

;; ************** FINISHED ***************

; (rank-pages '#hash((node0 . 339/31250) (node1 . 131103/1000000) (node2 . 144693/500000) (node3 . 15689/125000) (node4 . 131709/500000) (node5 . 179733/1000000)))

(define (rank-pages pr)
  
  (define (hash-list)
    (hash->list pr))

  (define (sort-pagelist)
    (flatten (sort (hash->list pr) < #:key cdr)))
  
  (define (remove-fract lst)
    (for/list ([i (length lst)] #:when (even? i))
      (list-ref lst i)))
  
  (remove-fract (sort-pagelist)))


;; CODE THAT DIDN'T WORK BELOW

; ***************************************************************************************************************

;  (define (sort-pagelist lst)
;    (if (null? lst)
;        (hash) 
;        ; (hash-set (sort-pagelist (rest lst)) (first lst) (car (argmin cdr (hash-list))))))
;        (list (sort-pagelist (rest lst)) (car (argmin cdr (hash-list))))))
;  (sort-pagelist (hash-list)))

;; (hash-set (recursive-pr (rest lst)) (first lst) (/ 1 (length-list)))))

; ***************************************************************************************************************




  
