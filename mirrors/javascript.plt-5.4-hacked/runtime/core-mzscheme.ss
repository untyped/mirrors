(module core-mzscheme mzscheme
  (provide (all-from-except mzscheme
              primitive?
              number->string
              string->number
              print))
  (provide (rename primitive?     mz:primitive?)
           (rename number->string mz:number->string)
           (rename string->number mz:string->number)
           (rename print          mz:print)))
