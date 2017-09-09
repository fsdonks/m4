;;Shim class for running marathon without
;;aot compilation issues.
;;entrypoint for marathon gui.
(ns marathon.main
  (:gen-class :main true))

import java.awt.EventQueue;
import java.awt.Font;
import java.util.Set;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.UIManager;
import javax.swing.UnsupportedLookAndFeelException;

(defn setDefaultSize [size]  
  (let [keySet (.. UIManager getLookAndFeelDefaults keySet)
        ks   (seq keySet)] ;(keySet.toArray(new Object[keySet.size()]);
    (doseq [k ks]
      (when (and k (.contains (clojure.string/lower-case (str k)) "font"))
        (let [_ (println k)
              ^Font font  (-> (. getDefaults UIManager)
                              (. getFont key))]
          (when font
            (let [font  (.deriveFont font (float size))]
              (.put UIManager k font))))))))


(defn scale-fonts []
  (run  (fn []
          (try (.setLookAndFeel UIManager (.getSystemLookAndFeelClassName UIManager))
               (catch Exception e (.printStrackTrace e)))
          (setDefaultSize 24))))


;;This is the main entry point for marathon.
;;It's a good example of a shim-class, and
;;requires some arcane features to get things
;;working, since we're creating repls on other
;;threads.
(defn -main [& args]
  ;;clojure.set isn't imported by default, causing errors when
  ;;aot-compiling in some places.
  (require 'clojure.set)
  ;;rather than :require it in the ns-decl, we load it
  ;;at runtime.
  (require 'marathon.core)
  ;;if we don't use this, i.e. establish a root binding
  ;;for the *ns* var, we can't use in-ns later....
  ;;which leads to compile-time and run-time errors..
  (binding [*ns* *ns*]
    (in-ns 'marathon.core)
    ;;if we don't use resolve, then we get compile-time aot
    ;;dependency on marathon.core.  This allows us to shim the
    ;;class.
    ((resolve 'marathon.core/hub) :exit? true)))
      
      
