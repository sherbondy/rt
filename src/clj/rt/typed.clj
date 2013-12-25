(ns rt.typed)

(defmacro ann [& args])
(defmacro ann-datatype [& args])
(defmacro ann-form [form ty] form)
(defmacro ann-many [& args])
(defmacro ann-protocol [& args])
(defmacro ann-record [& args])
(defmacro def-alias [& args])
(defmacro defprotocol> [& body]
  `(defprotocol ~@body))
