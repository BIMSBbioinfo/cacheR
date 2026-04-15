;; Guix package definition for cacheR
;; Install with: guix install -f guix.scm
;; Or use in a manifest: (specifications->manifest '("r-cacher"))

(define-module (cacher)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system r)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages cran)
  #:use-module (gnu packages statistics))

(define-public r-cacher
  (package
    (name "r-cacher")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/BIMSBbioinfo/cacheR")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0lgbln1fmmmkfpq9i3dfbsqpi4vpq535c6lb2ifki7z950ab2lg0"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-digest
           r-codetools
           r-filelock))
    (native-inputs
     (list r-testthat r-knitr r-rmarkdown))
    (home-page "https://github.com/BIMSBbioinfo/cacheR")
    (synopsis "Recursive file-based caching with call tree introspection")
    (description
     "cacheR provides tools for caching function results to disk while
tracking recursive parent-child relationships between cached calls.
It automatically detects changes in code, arguments, and input files,
re-running functions only when necessary.  Includes utilities to
inspect, visualise, and export the cache dependency graph.")
    (license license:gpl3+)))

r-cacher
