python-base-mode

(lambda "lambda" (unless (string= "" arg) " ") (p "" arg) ": " r)
(def "def " (p "function") "(" p ")" (unless (string= "" rtype) " -> ") (p "" rtype) ":" n> (p "..."))
(idef "def " (p "method") "(self" (unless (string= "" args) ", ") (p "" args) ")" (unless (string= "" rtype) " -> ") (p "" rtype) ":" n> (p "..."))
(cdef "@classmethod" n> "def " (p "classmethod") "(cls" (unless (string= "" args) ", ") (p "" args) ")" (unless (string= "" rtype) " -> ") (p "" rtype) ":" n> (p "..."))
(sdef "@staticmethod" n> "def " (p "staticmethod") "(" p ")" (unless (string= "" rtype) " -> ") (p "" rtype) ":" n> (p "..."))
(class "class " (p "Class") ":" n> (p "..."))
(attrs "@attrs.define" n> "class " (p "Class") ":" n> (p "..."))
(field p ": " p " = attrs.field(" q ")")
(for "for " p " in " p ":" n> (p "..."))
(while "while " p ":" n> (p "..."))
(from "from " p " import " q)
