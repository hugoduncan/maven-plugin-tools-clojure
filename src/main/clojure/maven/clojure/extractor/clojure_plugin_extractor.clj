(ns maven.clojure.extractor.clojure-plugin-extractor
  (:require
   [clojure.contrib.find-namespaces :as find-ns])
  (:import
   [maven.clojure.annotations Goal RequiresDependencyResolution]
   [org.apache.maven.plugin.descriptor
    MojoDescriptor
    Parameter
    InvalidPluginDescriptorException]
   [java.lang.annotation
    Retention RetentionPolicy Target Inherited ElementType]))


(defn file-to-ns-sym
  "Convert a file name to the corresponding namespace name"
  [#^String file]
  (symbol (-> file
              (.replaceFirst ".clj$" "")
              (.replaceAll "/" ".")
              (.replaceAll "_" "-"))))

(def *base-path*)
(def *plugin-descriptor*)

(defn test-var [v]
  (println "Testing" v
           (type v)
           (instance? org.apache.maven.plugin.Mojo v)
           (isa? v org.apache.maven.plugin.Mojo))
  v)

(defn to-ns
  [#^java.io.File file]
  (when (and (.isFile file) (.endsWith (.getPath file) ".clj"))
    (let [path (.getPath file)
          path (.substring path (inc (count *base-path*)) (count path))
          path (if (.startsWith path "main/clojure/")
                 (.substring path  (count "main/clojure/") (count path))
                 path)
          try-ns (file-to-ns-sym path)]
      (try
        (load-file (.getPath file))
        (map
         second
         (filter
          (comp
           #(and (not (= org.apache.maven.plugin.Mojo %))
                 (isa? % org.apache.maven.plugin.Mojo)
                 (not (var? %)))
           ;; test-var
           second)
          (ns-map (find-ns try-ns))))))))

(defn add-field
  [#^MojoDescriptor descriptor #^java.lang.reflect.Field field]
  (when-let [#^maven.clojure.annotations.Parameter parameter
             (.getAnnotation
              field
              maven.clojure.annotations.Parameter)]
    (.addParameter
     descriptor
     (doto (Parameter.)
       (.setName (.getName field))
       (.setType (.getName (.getType field)))
       (.setAlias (.alias parameter))
       (.setExpression (.expression parameter))
       (.setDescription (.description parameter))
       (.setEditable (not (.readonly parameter)))
       (.setRequired (.required parameter))
       (.setDefaultValue (.defaultValue parameter))))))

(defn mojo-descriptor
  [#^java.lang.Class v]
  (let [#^Goal goal (.getAnnotation v Goal)
        #^RequiresDependencyResolution resolution (.getAnnotation
                                                   v RequiresDependencyResolution)]
    (when-not goal
      (throw (IllegalArgumentException.
              (str "No Goal annotation for " v))))
    (let [descriptor (doto (MojoDescriptor.)
                       (.setPluginDescriptor *plugin-descriptor*)
                       (.setLanguage "clojure-mojo")
                       (.setImplementation (str v))
                       (.setGoal (.value goal)))]
      (when resolution
        (.setDependencyResolutionRequired descriptor (.value resolution)))
      (doseq [field (.getFields v)]
        (add-field descriptor field))
      descriptor)))

(defn process-source-tree
  [#^String source]
  (let [dir (java.io.File. source)]
    (binding [*base-path* (.getPath dir)]
      (doall                            ; ensure within scope of binding
       (map
        mojo-descriptor
        (filter
         (complement nil?)
         (apply concat (map to-ns (file-seq dir)))))))))

(defn plugin-classes
  [plugin-descriptor sources]
  (binding [*plugin-descriptor* plugin-descriptor]
    (java.util.ArrayList.
     (apply concat (map process-source-tree (distinct sources))))))
