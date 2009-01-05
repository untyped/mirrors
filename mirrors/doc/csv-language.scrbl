#lang scribble/doc

@(require (file "base.ss"))

@(define-eval csv-eval (planet untyped/mirrors/csv/csv))

@title[#:tag "csv-language"]{CSV language}

@defmodule[(planet untyped/mirrors/csv/csv)]

CSV is assembled in @italic{sheets} or @italic{rows} of @italic{cells}. Cells are separated by commas and rows by newlines. String data in cells is appropriately escaped to ensure compatibility with Microsoft Excel.

@defproc[(csv:sheet [rows (U csv:row? (listof csv:row?))] ...) csv:sheet?]{
Creates a sheet of CSV data from the specified @scheme[rows]. Each argument can be a row or a list of rows.

@examples[
  #:eval csv-eval
  (display
   (csv->string
    (csv:sheet (csv:row (csv:cell "Column 1")
                        (csv:cell "Column 2")
                        (csv:cell "Column 3"))
               (for/list ([j (in-range 0 2)])
                 (csv:row (for/list ([i (in-range 0 2)])
                            (csv:cell (format "~a,~a" i j))))))))]}


@defproc[(csv:row [cells (U csv:cell? (listof csv:cell?))] ...) csv:row?]{
Creates a row of CSV data from the specified @scheme[cells]. Each argument can be a cell or a list of cells. See above for examples.}

@defproc[(csv:cell [datum (U boolean? number? string? symbol? bytes? url?)]) csv:cell?]{
Creates a CSV cell containing an appropriately escaped @scheme[datum]. See above for examples.}

@defproc[(csv:sheet? [arg any]) boolean?]{
Predicate that identifies CSV sheet structures.}

@defproc[(csv:row? [arg any]) boolean?]{
Predicate that identifies CSV row structures.}

@defproc[(csv:cell? [arg any]) boolean?]{
Predicate that identifies CSV cell structures.}

@defproc[(csv? [arg any]) boolean?]{
Predicate that identifies CSV sheets, rows or cells.}
