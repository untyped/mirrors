#lang scheme/base
(require (rename-in (except-in srfi/1/list
                               partition
                               last-pair
                               filter-map
                               append-map)
                    [fold s:fold]
                    [fold-right s:fold-right]
                    [first s:first]
                    [second s:second]
                    [third s:third]
                    [fourth s:fourth]
                    [fifth s:fifth]
                    [sixth s:sixth]
                    [seventh s:seventh]
                    [eighth s:eighth]
                    [ninth s:ninth]
                    [tenth s:tenth]
                    [last s:last])
         scheme/list)
(provide (all-from-out srfi/1/list)
         (all-from-out scheme/list))
