#lang sicp

; add previous definitions

(define (union-set-tree tree1 tree2)
  (list->tree (union-set (tree->list tree1) (tree->list tree2))))

(define (intersection-set-tree tree1 tree2)
  (list->tree (intersection-set (tree->list tree1) (tree->list tree2))))