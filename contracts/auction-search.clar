(define-constant err-invalid-range (err u200))
(define-constant err-invalid-page (err u201))

;; Helper function to get the minimum of two uints
(define-read-only (min (a uint) (b uint))
  (if (< a b) a b)
)

(define-data-var total-auctions-created uint u0)

(define-map auction-by-status
  { status: (string-ascii 20), index: uint }
  { auction-id: uint }
)

(define-map status-counts
  { status: (string-ascii 20) }
  { count: uint }
)

(define-read-only (search-auctions-by-status (status (string-ascii 20)) (page uint) (per-page uint))
  (let
    (
      (start-index (* page per-page))
      (end-index (+ start-index per-page))
      (status-count (default-to u0 (get count (map-get? status-counts { status: status }))))
    )
    (asserts! (<= per-page u50) err-invalid-range)
    (asserts! (< start-index status-count) err-invalid-page)
    (ok (get-auctions-by-status-range status start-index (min end-index status-count)))
  )
)

(define-read-only (search-auctions-by-price-range (min-price uint) (max-price uint) (max-results uint))
  (let
    (
      (total-auctions (var-get total-auctions-created))
    )
    (asserts! (<= max-results u100) err-invalid-range)
    (asserts! (<= min-price max-price) err-invalid-range)
    (ok (filter-auctions-by-price-range min-price max-price max-results total-auctions))
  )
)




(define-read-only (get-auction-statistics)
  (let
    (
      (active-count (default-to u0 (get count (map-get? status-counts { status: "active" }))))
      (ended-count (default-to u0 (get count (map-get? status-counts { status: "ended" }))))
      (cancelled-count (default-to u0 (get count (map-get? status-counts { status: "cancelled" }))))
    )
    (ok {
      total: (var-get total-auctions-created),
      active: active-count,
      ended: ended-count,
      cancelled: cancelled-count
    })
  )
)


(define-private (get-auctions-by-status-range (status (string-ascii 20)) (start uint) (end uint))
  (map get-auction-id-by-status-index 
    (generate-range start end)
    (list status status status status status status status status status status
          status status status status status status status status status status
          status status status status status status status status status status
          status status status status status status status status status status
          status status status status status status status status status status))
)

(define-private (get-auction-id-by-status-index (index uint) (status (string-ascii 20)))
  (default-to u0 
    (get auction-id 
      (map-get? auction-by-status { status: status, index: index })))
)

(define-private (filter-auctions-by-price-range (min-price uint) (max-price uint) (max-results uint) (total-auctions uint))
  (fold check-auction-price-range 
    (generate-auction-ids total-auctions max-results)
    { min-price: min-price, max-price: max-price, results: (list), count: u0, max-count: max-results })
)





(define-private (check-auction-price-range 
  (auction-id uint) 
  (acc { min-price: uint, max-price: uint, results: (list 100 uint), count: uint, max-count: uint }))
  (if (>= (get count acc) (get max-count acc))
    acc
    (match (contract-call? .auction-market get-auction auction-id)
      auction-data
        (if (and (>= (get highest-bid auction-data) (get min-price acc))
                 (<= (get highest-bid auction-data) (get max-price acc)))
          (merge acc { 
            results: (unwrap-panic (as-max-len? (append (get results acc) auction-id) u100)),
            count: (+ (get count acc) u1)
          })
          acc)
      err
        acc))
)



(define-private (generate-range (start uint) (end uint))
  (map + 
    (list u0 u1 u2 u3 u4 u5 u6 u7 u8 u9 u10 u11 u12 u13 u14 u15 u16 u17 u18 u19
          u20 u21 u22 u23 u24 u25 u26 u27 u28 u29 u30 u31 u32 u33 u34 u35 u36 u37 u38 u39
          u40 u41 u42 u43 u44 u45 u46 u47 u48 u49)
    (list start start start start start start start start start start start start start start start start start start start start
          start start start start start start start start start start start start start start start start start start start start
          start start start start start start start start start start))
)

(define-private (generate-auction-ids (total uint) (max-count uint))
  (let
    (
      (limit (min total max-count))
    )
    (map + 
      (list u1 u1 u1 u1 u1 u1 u1 u1 u1 u1 u1 u1 u1 u1 u1 u1 u1 u1 u1 u1
            u1 u1 u1 u1 u1 u1 u1 u1 u1 u1 u1 u1 u1 u1 u1 u1 u1 u1 u1 u1
            u1 u1 u1 u1 u1 u1 u1 u1 u1 u1 u1 u1 u1 u1 u1 u1 u1 u1 u1 u1
            u1 u1 u1 u1 u1 u1 u1 u1 u1 u1 u1 u1 u1 u1 u1 u1 u1 u1 u1 u1
            u1 u1 u1 u1 u1 u1 u1 u1 u1 u1 u1 u1 u1 u1 u1 u1 u1 u1 u1 u1)
      (list u0 u1 u2 u3 u4 u5 u6 u7 u8 u9 u10 u11 u12 u13 u14 u15 u16 u17 u18 u19
            u20 u21 u22 u23 u24 u25 u26 u27 u28 u29 u30 u31 u32 u33 u34 u35 u36 u37 u38 u39
            u40 u41 u42 u43 u44 u45 u46 u47 u48 u49 u50 u51 u52 u53 u54 u55 u56 u57 u58 u59
            u60 u61 u62 u63 u64 u65 u66 u67 u68 u69 u70 u71 u72 u73 u74 u75 u76 u77 u78 u79
            u80 u81 u82 u83 u84 u85 u86 u87 u88 u89 u90 u91 u92 u93 u94 u95 u96 u97 u98 u99))
  )
)

(define-public (update-auction-index (auction-id uint) (old-status (string-ascii 20)) (new-status (string-ascii 20)))
  (let
    (
      (old-count (default-to u0 (get count (map-get? status-counts { status: old-status }))))
      (new-count (default-to u0 (get count (map-get? status-counts { status: new-status }))))
    )
    (if (> old-count u0)
      (map-set status-counts { status: old-status } { count: (- old-count u1) })
      true)
    
    (map-set auction-by-status 
      { status: new-status, index: new-count }
      { auction-id: auction-id })
    
    (map-set status-counts 
      { status: new-status } 
      { count: (+ new-count u1) })
    
    (ok true)
  )
)

(define-public (register-new-auction (auction-id uint))
  (let
    (
      (active-count (default-to u0 (get count (map-get? status-counts { status: "active" }))))
    )
    (map-set auction-by-status
      { status: "active", index: active-count }
      { auction-id: auction-id })
    
    (map-set status-counts
      { status: "active" }
      { count: (+ active-count u1) })
    
    (var-set total-auctions-created (+ (var-get total-auctions-created) u1))
    
    (ok true)
  )
)

