(ns build
  (:require
   [clojure.tools.build.api :as b]
   [deps-deploy.deps-deploy :as deps-deploy]
   [clojure.string :as str]
   [babashka.fs :as fs]
   [clojure.edn :as edn]
   [clojure.test :as test]
   [clojure.java.io :as io]))

(defn re-quote
  (^String [s]
   (java.util.regex.Pattern/quote s)))

(let [deps-edn (edn/read-string (slurp "deps.edn"))]
  (def lib (get-in deps-edn [:io.github.evenmoreirrelevance/libdesc :lib]))
  (def version (get-in deps-edn [:io.github.evenmoreirrelevance/libdesc :mvn/version]))
  (def repo?? (get-in deps-edn [:io.github.evenmoreirrelevance/libdesc :repo])))

(def class-dir "target/classes")
(def jar-file (format "target/%s-%s.jar" (name lib) version))

;; delay to defer side effects (artifact downloads)
(def basis (delay (b/create-basis {:project "deps.edn"})))

(defn- jar-opts [opts]
  (assoc opts
    :lib lib :version version
    :jar-file jar-file
    :scm {:tag (str "v" version)}
    :basis @basis
    :class-dir class-dir
    :target "target"
    :src-dirs ["src"]))

(defn clean [_]
  (b/delete {:path "target"})
  (b/delete {:path "pom.xml"}))

(defn sync-pom [_]
  (b/write-pom
    (conj (jar-opts {})
      {:target "."
       :class-dir nil
       :scm
       {:url repo??
        :tag (str "v" version)}
       :pom-data
       [[:licenses
         [:license
          [:name "Eclipse Public License 2.0"]
          [:url "https://opensource.org/license/epl-2-0/"]]]]})))

(defn jar [_]
  (clean _)
  (sync-pom _)
  (b/copy-dir {:src-dirs ["src" "resources"] :target-dir class-dir}) 
  (b/jar {:class-dir class-dir :jar-file jar-file}))

(defn install [_]
  (b/install (jar-opts {})))

(when-let [creds
           (try (edn/read-string (slurp "../clojars-credentials.edn"))
             (catch java.io.IOException _e nil))]
  (alter-var-root
    (var deps-deploy/default-repo-settings)
    update "clojars" merge (get creds (namespace lib))))

(defn dump-reader
  [src ^java.io.Writer targ]
  (let [buffsrc (java.io.BufferedReader. ^java.io.Reader src)]
    (doseq [^String l (take-while some? (repeatedly #(.readLine buffsrc)))]
      (.append targ (str l "\n")))))

(defn runit
  ([args] (runit {} args))
  ([{:keys [input error]} args]
   (let [^java.util.List args (vec args)
         input (or input *out*)
         error (or error *err*)
         p (-> (ProcessBuilder. args) (.start))
         re (future (dump-reader (java.io.InputStreamReader. (.getErrorStream p)) error))
         ri (future (dump-reader (java.io.InputStreamReader. (.getInputStream p)) input))
         res (try (loop []
                    (or (try (.waitFor p) (catch InterruptedException _ nil))
                      (recur)))
               (finally (.destroy p)))]
     (run! deref [re ri])
     res)))

(defn test-all
  [{:keys [out-file]}]
  (let [test-namespaces (let [root (fs/file "src")]
                          (into #{} (comp
                                      (map #(fs/components (fs/relativize root %)))
                                      (map #(str/join "." %))
                                      (keep #(second (re-matches #"(.+).clj" %)))
                                      (map symbol))
                            (file-seq root)))]
    (with-open [w (io/writer (or (some-> out-file fs/file) *out*))]
      (binding [*out* w]
        (apply require `[~@test-namespaces :reload-all])
        (reduce #(merge-with + %1 %2) nil
          (eduction
            (map #(binding [*ns* (the-ns %)] (test/run-tests %)))
            (map #(select-keys % [:fail :error :pass]))
            test-namespaces))))))

(defn deploy [{:keys [test?] :as _opts}]
  (let [origin (str/trim (with-out-str (runit ["git" "config" "--get" "remote.origin.url"])))]
    (when-not (= repo?? origin)
      (throw (ex-info "origin not the same as repo in libdesc" {:origin origin :repo repo??}))))
  (let [b (str/trim (with-out-str (runit ["git" "rev-parse" "--abbrev-ref" "HEAD"])))]
    (when-not (#{"main" "master"} b)
      (throw (ex-info "must be on main branch" {:branch b}))))
  (when-not (and
              (= 0 (runit ["git" "diff-index" "--quiet" "--cached" "HEAD" "--"]))
              (= 0 (runit ["git" "diff-files" "--quiet"])))
    (throw (ex-info "worktree or index not clean" {})))
  (when test?
    (let [{:keys [fail error]} (test-all nil)]
      (when (or (< 0 fail) (< 0 error))
        (throw (ex-info "tests failed." {:fail fail :error error})))))
  (let [tag (str "v" version)]
    (when (= tag (str/trim (with-out-str (runit ["git" "tag" "--list" tag]))))
      (throw (ex-info "version already tagged" {:version version})))
    (jar _opts)
    (when-not (and
                (= 0 (runit ["git" "commit" "-am" (str "deploy " tag)]))
                (= 0 (runit ["git" "tag" "--force" tag]))
                (= 0 (runit ["git" "push"]))
                (= 0 (runit ["git" "push" "origin" "tag" "--force" tag])))
      (throw (ex-info "errors while pushing to repo" {})))
    (deps-deploy/deploy
      {:artifact jar-file
       :installer :remote
       :sign-releases? false})))

(comment
  (sync-pom nil)
  (clean nil)
  (jar nil)
  (install nil)
  (test-all nil)
  
  (deploy {:test? true})

  *e)
