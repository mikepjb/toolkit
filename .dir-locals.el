;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((clojure-mode
  (cider-clojure-cli-parameters . "-A:cljs-nrepl -m nrepl.cmdline --middleware '%s'")
  (cider-default-cljs-repl . node)))
