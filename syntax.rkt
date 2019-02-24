#lang racket
(require "struct.rkt")

(define-syntax (define-rule a-syntax)
  (raise-syntax-error #f "TODO" a-syntax))