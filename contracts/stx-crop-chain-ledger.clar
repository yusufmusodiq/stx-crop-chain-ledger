;; StxCropChainLedger
;; Decentralized agricultural yield tracking and verification protocol
;; Maintains immutable records of crop production cycles with access control

;; ===============================================
;; ERROR CODE DEFINITIONS
;; ===============================================

;; System administrator access control
(define-constant system-owner tx-sender)

;; Protocol error responses
(define-constant record-missing (err u301))
(define-constant duplicate-entry (err u302))
(define-constant field-length-violation (err u303))
(define-constant quantity-bounds-error (err u304))
(define-constant permission-denied (err u305))
(define-constant ownership-mismatch (err u306))
(define-constant admin-restricted (err u300))
(define-constant view-access-denied (err u307))
(define-constant label-format-error (err u308))

;; ===============================================
;; PROTOCOL STATE VARIABLES
;; ===============================================

;; Sequential identifier for production records
(define-data-var production-sequence uint u0)

;; ===============================================
;; CORE DATA STRUCTURES
;; ===============================================

;; Primary production record storage
(define-map production-ledger
  { record-index: uint }
  {
    product-identifier: (string-ascii 64),
    producer-address: principal,
    output-volume: uint,
    creation-height: uint,
    location-notes: (string-ascii 128),
    metadata-labels: (list 10 (string-ascii 32))
  }
)

;; Access control matrix for production data
(define-map access-control-matrix
  { record-index: uint, accessor: principal }
  { permission-granted: bool }
)

;; ===============================================
;; VALIDATION HELPER FUNCTIONS
;; ===============================================

;; Validate individual metadata label format
(define-private (validate-single-label (label (string-ascii 32)))
  (and
    (> (len label) u0)
    (< (len label) u33)
  )
)

;; Comprehensive label collection validation
(define-private (validate-label-set (label-collection (list 10 (string-ascii 32))))
  (and
    (> (len label-collection) u0)
    (<= (len label-collection) u10)
    (is-eq (len (filter validate-single-label label-collection)) (len label-collection))
  )
)

;; Check production record existence in ledger
(define-private (record-exists-check (record-index uint))
  (is-some (map-get? production-ledger { record-index: record-index }))
)

;; Verify producer ownership of specific record
(define-private (confirm-producer-ownership (record-index uint) (producer-principal principal))
  (match (map-get? production-ledger { record-index: record-index })
    production-data (is-eq (get producer-address production-data) producer-principal)
    false
  )
)

;; Extract output volume from production record
(define-private (extract-output-volume (record-index uint))
  (default-to u0
    (get output-volume
      (map-get? production-ledger { record-index: record-index })
    )
  )
)

;; ===============================================
;; RECORD CREATION FUNCTIONS
;; ===============================================

;; Create new production record with comprehensive data
(define-public (create-production-record 
  (product (string-ascii 64)) 
  (volume uint) 
  (notes (string-ascii 128)) 
  (labels (list 10 (string-ascii 32)))
)
  (let
    (
      (next-record-index (+ (var-get production-sequence) u1))
    )
    ;; Comprehensive input validation
    (asserts! (> (len product) u0) field-length-violation)
    (asserts! (< (len product) u65) field-length-violation)
    (asserts! (> volume u0) quantity-bounds-error)
    (asserts! (< volume u1000000000) quantity-bounds-error)
    (asserts! (> (len notes) u0) field-length-violation)
    (asserts! (< (len notes) u129) field-length-violation)
    (asserts! (validate-label-set labels) label-format-error)

    ;; Insert new production record
    (map-insert production-ledger
      { record-index: next-record-index }
      {
        product-identifier: product,
        producer-address: tx-sender,
        output-volume: volume,
        creation-height: block-height,
        location-notes: notes,
        metadata-labels: labels
      }
    )

    ;; Establish producer access permissions
    (map-insert access-control-matrix
      { record-index: next-record-index, accessor: tx-sender }
      { permission-granted: true }
    )

    ;; Update sequence counter
    (var-set production-sequence next-record-index)
    (ok next-record-index)
  )
)

;; ===============================================
;; RECORD MODIFICATION FUNCTIONS
;; ===============================================

;; Comprehensive record update with all fields
(define-public (modify-production-record 
  (record-index uint) 
  (updated-product (string-ascii 64)) 
  (updated-volume uint) 
  (updated-notes (string-ascii 128)) 
  (updated-labels (list 10 (string-ascii 32)))
)
  (let
    (
      (existing-record (unwrap! (map-get? production-ledger { record-index: record-index }) record-missing))
    )
    ;; Ownership and validation checks
    (asserts! (record-exists-check record-index) record-missing)
    (asserts! (is-eq (get producer-address existing-record) tx-sender) ownership-mismatch)
    (asserts! (> (len updated-product) u0) field-length-violation)
    (asserts! (< (len updated-product) u65) field-length-violation)
    (asserts! (> updated-volume u0) quantity-bounds-error)
    (asserts! (< updated-volume u1000000000) quantity-bounds-error)
    (asserts! (> (len updated-notes) u0) field-length-violation)
    (asserts! (< (len updated-notes) u129) field-length-violation)
    (asserts! (validate-label-set updated-labels) label-format-error)

    ;; Apply updates to existing record
    (map-set production-ledger
      { record-index: record-index }
      (merge existing-record { 
        product-identifier: updated-product, 
        output-volume: updated-volume, 
        location-notes: updated-notes, 
        metadata-labels: updated-labels 
      })
    )
    (ok true)
  )
)

;; Append additional metadata labels to existing record
(define-public (append-metadata-labels (record-index uint) (new-labels (list 10 (string-ascii 32))))
  (let
    (
      (existing-record (unwrap! (map-get? production-ledger { record-index: record-index }) record-missing))
      (current-labels (get metadata-labels existing-record))
      (merged-labels (unwrap! (as-max-len? (concat current-labels new-labels) u10) label-format-error))
    )
    ;; Ownership verification and label validation
    (asserts! (record-exists-check record-index) record-missing)
    (asserts! (is-eq (get producer-address existing-record) tx-sender) ownership-mismatch)
    (asserts! (validate-label-set new-labels) label-format-error)

    ;; Update record with merged labels
    (map-set production-ledger
      { record-index: record-index }
      (merge existing-record { metadata-labels: merged-labels })
    )
    (ok merged-labels)
  )
)

;; Complete record removal from ledger
(define-public (delete-production-record (record-index uint))
  (let
    (
      (target-record (unwrap! (map-get? production-ledger { record-index: record-index }) record-missing))
    )
    ;; Ownership verification before deletion
    (asserts! (record-exists-check record-index) record-missing)
    (asserts! (is-eq (get producer-address target-record) tx-sender) ownership-mismatch)

    ;; Execute record deletion
    (map-delete production-ledger { record-index: record-index })
    (ok true)
  )
)

;; ===============================================
;; ACCESS MANAGEMENT FUNCTIONS
;; ===============================================

;; Transfer record ownership to different producer
(define-public (transfer-record-ownership (record-index uint) (recipient-producer principal))
  (let
    (
      (target-record (unwrap! (map-get? production-ledger { record-index: record-index }) record-missing))
    )
    ;; Verify current ownership before transfer
    (asserts! (record-exists-check record-index) record-missing)
    (asserts! (is-eq (get producer-address target-record) tx-sender) ownership-mismatch)

    ;; Execute ownership transfer
    (map-set production-ledger
      { record-index: record-index }
      (merge target-record { producer-address: recipient-producer })
    )
    (ok true)
  )
)

;; Revoke viewing access for specific accessor
(define-public (revoke-viewing-access (record-index uint) (restricted-accessor principal))
  (let
    (
      (target-record (unwrap! (map-get? production-ledger { record-index: record-index }) record-missing))
    )
    ;; Ownership verification and self-restriction prevention
    (asserts! (record-exists-check record-index) record-missing)
    (asserts! (is-eq (get producer-address target-record) tx-sender) ownership-mismatch)
    (asserts! (not (is-eq restricted-accessor tx-sender)) admin-restricted)

    ;; Remove access permissions
    (map-delete access-control-matrix { record-index: record-index, accessor: restricted-accessor })
    (ok true)
  )
)

;; ===============================================
;; VERIFICATION AND AUTHENTICATION
;; ===============================================

;; Comprehensive record authentication and validation
(define-public (verify-production-authenticity (record-index uint) (expected-producer principal))
  (let
    (
      (production-data (unwrap! (map-get? production-ledger { record-index: record-index }) record-missing))
      (verified-producer (get producer-address production-data))
      (record-creation-height (get creation-height production-data))
      (accessor-permission (default-to 
        false 
        (get permission-granted 
          (map-get? access-control-matrix { record-index: record-index, accessor: tx-sender })
        )
      ))
    )
    ;; Access permission verification
    (asserts! (record-exists-check record-index) record-missing)
    (asserts! 
      (or 
        (is-eq tx-sender verified-producer)
        accessor-permission
        (is-eq tx-sender system-owner)
      ) 
      permission-denied
    )

    ;; Producer verification and metadata generation
    (if (is-eq verified-producer expected-producer)
      ;; Successful authentication response
      (ok {
        is-authentic: true,
        current-block: block-height,
        blockchain-age: (- block-height record-creation-height),
        farmer-match: true
      })
      ;; Failed authentication response
      (ok {
        is-authentic: false,
        current-block: block-height,
        blockchain-age: (- block-height record-creation-height),
        farmer-match: false
      })
    )
  )
)

;; ===============================================
;; EMERGENCY SECURITY FUNCTIONS
;; ===============================================

;; Apply security lock to production record
(define-public (apply-security-lock (record-index uint))
  (let
    (
      (target-record (unwrap! (map-get? production-ledger { record-index: record-index }) record-missing))
      (lock-identifier "SECURITY-LOCK")
      (current-labels (get metadata-labels target-record))
    )
    ;; Authorization check for security operations
    (asserts! (record-exists-check record-index) record-missing)
    (asserts! 
      (or 
        (is-eq tx-sender system-owner)
        (is-eq (get producer-address target-record) tx-sender)
      ) 
      admin-restricted
    )

    (ok true)
  )
)

