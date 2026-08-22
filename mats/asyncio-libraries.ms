;;; Public library boundaries for asynchronous I/O.

(import (chezscheme async io errors))
(import (chezscheme async io stream))
(import (chezscheme async io dns))
(import (chezscheme async io fs))

(mat asyncio-libraries
  (and (procedure? async-io-condition?)
       (procedure? tcp-connect)
       (procedure? dns-lookup)
       (procedure? file-open)))
