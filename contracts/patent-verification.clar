;; Patent Verification and Authentication System
;; Ensures patent authenticity and maintains comprehensive patent metadata

(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u300))
(define-constant err-not-found (err u301))
(define-constant err-already-verified (err u302))
(define-constant err-not-verified (err u303))
(define-constant err-invalid-patent (err u304))
(define-constant err-verification-expired (err u305))
(define-constant err-unauthorized-verifier (err u306))
(define-constant err-invalid-status (err u307))

;; Verification validity period in blocks (approximately 1 year)
(define-constant verification-period u52560)

;; Data structures for patent verification
(define-map patent-registry
  { patent-id: (string-ascii 64) }
  {
    patent-number: (string-ascii 32),
    title: (string-utf8 200),
    inventor: (string-utf8 100),
    filing-date: uint,
    issue-date: uint,
    expiry-date: uint,
    classification: (string-ascii 20),
    document-hash: (buff 32),
    verification-status: (string-ascii 20),
    verified-at: uint,
    verified-by: principal,
    last-updated: uint
  }
)

;; Authorized verifiers who can verify patents
(define-map authorized-verifiers
  { verifier: principal }
  { 
    name: (string-utf8 50),
    authorized-at: uint,
    active: bool
  }
)

;; Patent ownership history tracking
(define-map ownership-history
  { patent-id: (string-ascii 64), sequence: uint }
  {
    previous-owner: principal,
    new-owner: principal,
    transfer-date: uint,
    transfer-type: (string-ascii 20)
  }
)

;; Verification certificates
(define-map verification-certificates
  { patent-id: (string-ascii 64) }
  {
    certificate-id: (string-ascii 32),
    issued-at: uint,
    expires-at: uint,
    verifier: principal,
    verification-score: uint,
    notes: (string-utf8 200)
  }
)

;; Patent dispute tracking
(define-map patent-disputes
  { patent-id: (string-ascii 64), dispute-id: uint }
  {
    reporter: principal,
    dispute-type: (string-ascii 30),
    description: (string-utf8 300),
    status: (string-ascii 20),
    reported-at: uint,
    resolved-at: (optional uint)
  }
)

;; Global counters
(define-data-var total-verified-patents uint u0)
(define-data-var total-disputes uint u0)
(define-data-var next-dispute-id uint u1)

;; Read-only functions
(define-read-only (get-patent-info (patent-id (string-ascii 64)))
  (match (map-get? patent-registry { patent-id: patent-id })
    patent-data (ok patent-data)
    (err err-not-found)
  )
)

(define-read-only (is-patent-verified (patent-id (string-ascii 64)))
  (match (map-get? patent-registry { patent-id: patent-id })
    patent-data 
      (and 
        (is-eq (get verification-status patent-data) "verified")
        (> (+ (get verified-at patent-data) verification-period) stacks-block-height))
    false
  )
)

(define-read-only (get-verification-certificate (patent-id (string-ascii 64)))
  (match (map-get? verification-certificates { patent-id: patent-id })
    cert-data (ok cert-data)
    (err err-not-found)
  )
)

(define-read-only (is-authorized-verifier (verifier principal))
  (match (map-get? authorized-verifiers { verifier: verifier })
    verifier-data (get active verifier-data)
    false
  )
)

(define-read-only (get-patent-ownership-history (patent-id (string-ascii 64)) (start-seq uint) (count uint))
  (ok (map get-ownership-record 
    (generate-sequence start-seq (+ start-seq count))
    (list patent-id patent-id patent-id patent-id patent-id patent-id patent-id patent-id patent-id patent-id)))
)

(define-read-only (get-verification-stats)
  (ok {
    total-verified: (var-get total-verified-patents),
    total-disputes: (var-get total-disputes),
    active-verifiers: (count-active-verifiers)
  })
)

;; Public functions for patent registration and verification
(define-public (register-patent 
    (patent-id (string-ascii 64))
    (patent-number (string-ascii 32))
    (title (string-utf8 200))
    (inventor (string-utf8 100))
    (filing-date uint)
    (issue-date uint)
    (expiry-date uint)
    (classification (string-ascii 20))
    (document-hash (buff 32)))
  (begin
    (asserts! (is-none (map-get? patent-registry { patent-id: patent-id })) err-already-verified)
    (asserts! (> expiry-date stacks-block-height) err-invalid-patent)
    
    (map-set patent-registry
      { patent-id: patent-id }
      {
        patent-number: patent-number,
        title: title,
        inventor: inventor,
        filing-date: filing-date,
        issue-date: issue-date,
        expiry-date: expiry-date,
        classification: classification,
        document-hash: document-hash,
        verification-status: "pending",
        verified-at: u0,
        verified-by: contract-owner,
        last-updated: stacks-block-height
      }
    )
    
    (ok true)
  )
)

(define-public (verify-patent (patent-id (string-ascii 64)) (verification-score uint) (notes (string-utf8 200)))
  (let
    (
      (patent-data (unwrap! (map-get? patent-registry { patent-id: patent-id }) err-not-found))
      (certificate-id (generate-certificate-id patent-id))
    )
    (asserts! (is-authorized-verifier tx-sender) err-unauthorized-verifier)
    (asserts! (is-eq (get verification-status patent-data) "pending") err-already-verified)
    (asserts! (<= verification-score u100) err-invalid-status)
    
    (map-set patent-registry
      { patent-id: patent-id }
      (merge patent-data {
        verification-status: "verified",
        verified-at: stacks-block-height,
        verified-by: tx-sender,
        last-updated: stacks-block-height
      })
    )
    
    (map-set verification-certificates
      { patent-id: patent-id }
      {
        certificate-id: certificate-id,
        issued-at: stacks-block-height,
        expires-at: (+ stacks-block-height verification-period),
        verifier: tx-sender,
        verification-score: verification-score,
        notes: notes
      }
    )
    
    (var-set total-verified-patents (+ (var-get total-verified-patents) u1))
    (ok certificate-id)
  )
)

(define-public (reject-patent (patent-id (string-ascii 64)) (reason (string-utf8 200)))
  (let
    (
      (patent-data (unwrap! (map-get? patent-registry { patent-id: patent-id }) err-not-found))
    )
    (asserts! (is-authorized-verifier tx-sender) err-unauthorized-verifier)
    (asserts! (is-eq (get verification-status patent-data) "pending") err-already-verified)
    
    (map-set patent-registry
      { patent-id: patent-id }
      (merge patent-data {
        verification-status: "rejected",
        verified-at: stacks-block-height,
        verified-by: tx-sender,
        last-updated: stacks-block-height
      })
    )
    
    (ok true)
  )
)

(define-public (add-authorized-verifier (verifier principal) (name (string-utf8 50)))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    
    (map-set authorized-verifiers
      { verifier: verifier }
      {
        name: name,
        authorized-at: stacks-block-height,
        active: true
      }
    )
    
    (ok true)
  )
)

(define-public (revoke-verifier (verifier principal))
  (let
    (
      (verifier-data (unwrap! (map-get? authorized-verifiers { verifier: verifier }) err-not-found))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    
    (map-set authorized-verifiers
      { verifier: verifier }
      (merge verifier-data { active: false })
    )
    
    (ok true)
  )
)

(define-public (file-dispute 
    (patent-id (string-ascii 64)) 
    (dispute-type (string-ascii 30)) 
    (description (string-utf8 300)))
  (let
    (
      (dispute-id (var-get next-dispute-id))
    )
    (asserts! (is-some (map-get? patent-registry { patent-id: patent-id })) err-not-found)
    
    (map-set patent-disputes
      { patent-id: patent-id, dispute-id: dispute-id }
      {
        reporter: tx-sender,
        dispute-type: dispute-type,
        description: description,
        status: "open",
        reported-at: stacks-block-height,
        resolved-at: none
      }
    )
    
    (var-set next-dispute-id (+ dispute-id u1))
    (var-set total-disputes (+ (var-get total-disputes) u1))
    
    (ok dispute-id)
  )
)

(define-public (resolve-dispute (patent-id (string-ascii 64)) (dispute-id uint) (resolution (string-ascii 20)))
  (let
    (
      (dispute-data (unwrap! (map-get? patent-disputes { patent-id: patent-id, dispute-id: dispute-id }) err-not-found))
    )
    (asserts! (is-authorized-verifier tx-sender) err-unauthorized-verifier)
    (asserts! (is-eq (get status dispute-data) "open") err-invalid-status)
    
    (map-set patent-disputes
      { patent-id: patent-id, dispute-id: dispute-id }
      (merge dispute-data {
        status: resolution,
        resolved-at: (some stacks-block-height)
      })
    )
    
    (ok true)
  )
)

(define-public (record-ownership-transfer 
    (patent-id (string-ascii 64)) 
    (previous-owner principal) 
    (new-owner principal) 
    (transfer-type (string-ascii 20)))
  (let
    (
      (sequence (get-next-ownership-sequence patent-id))
    )
    (asserts! (is-patent-verified patent-id) err-not-verified)
    
    (map-set ownership-history
      { patent-id: patent-id, sequence: sequence }
      {
        previous-owner: previous-owner,
        new-owner: new-owner,
        transfer-date: stacks-block-height,
        transfer-type: transfer-type
      }
    )
    
    (ok sequence)
  )
)

;; Private helper functions
(define-private (generate-certificate-id (patent-id (string-ascii 64)))
  (unwrap-panic (as-max-len? (unwrap-panic (slice? patent-id u0 u32)) u32))
)

(define-private (get-ownership-record (sequence uint) (patent-id (string-ascii 64)))
  (default-to 
    { previous-owner: contract-owner, new-owner: contract-owner, transfer-date: u0, transfer-type: "none" }
    (map-get? ownership-history { patent-id: patent-id, sequence: sequence }))
)

(define-private (generate-sequence (start uint) (end uint))
  (map + 
    (list u0 u1 u2 u3 u4 u5 u6 u7 u8 u9)
    (list start start start start start start start start start start))
)

(define-private (count-active-verifiers)
  u0  ;; Simplified for demo - would need iterator function in production
)

(define-private (get-next-ownership-sequence (patent-id (string-ascii 64)))
  (let
    (
      (sequence u0)
    )
    (if (is-some (map-get? ownership-history { patent-id: patent-id, sequence: sequence }))
      (+ sequence u1)
      sequence)
  )
)

;; Integration function for auction-market to check patent verification
(define-read-only (can-auction-patent (patent-id (string-ascii 64)))
  (and 
    (is-patent-verified patent-id)
    (is-none (get-active-dispute patent-id)))
)

(define-private (get-active-dispute (patent-id (string-ascii 64)))
  (map-get? patent-disputes { patent-id: patent-id, dispute-id: u1 })  ;; Simplified check
)

