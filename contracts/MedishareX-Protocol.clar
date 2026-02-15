;; ============================================================
;; MediShareX Protocol
;; Decentralized Medical Equipment Sharing
;; ============================================================

;; =========================
;; CONSTANTS
;; =========================

(define-constant ERR-NOT-FOUND (err u100))
(define-constant ERR-NOT-OWNER (err u101))
(define-constant ERR-NOT-AVAILABLE (err u102))
(define-constant ERR-NOT-BORROWER (err u103))
(define-constant ERR-ALREADY-REQUESTED (err u104))

;; =========================
;; DATA STRUCTURES
;; =========================

(define-map equipment
  uint
  {
    owner: principal,
    name: (string-ascii 50),
    available: bool,
    borrower: (optional principal),
    deposit-required: uint
  }
)

(define-map deposits
  uint
  {
    borrower: principal,
    amount: uint
  }
)

(define-data-var equipment-id-counter uint u0)

;; =========================
;; PRIVATE FUNCTIONS
;; =========================

(define-private (get-next-id)
  (let ((current-id (var-get equipment-id-counter)))
    (begin
      (var-set equipment-id-counter (+ current-id u1))
      (+ current-id u1)
    )
  )
)

;; =========================
;; PUBLIC FUNCTIONS
;; =========================

;; (1) Register Equipment
(define-public (register-equipment 
    (name (string-ascii 50)) 
    (deposit-required uint)
  )
  (let ((new-id (get-next-id))
        (sender tx-sender))
    (begin
      (asserts! (> (len name) u0) ERR-NOT-FOUND)
      (asserts! (>= deposit-required u0) ERR-NOT-FOUND)
      (map-set equipment new-id
        {
          owner: sender,
          name: name,
          available: true,
          borrower: none,
          deposit-required: deposit-required
        }
      )
      (ok new-id)
    )
  )
)

;; (2) Request Equipment (locks deposit)
(define-public (request-equipment (id uint))
  (match (map-get? equipment id)
    equipment-data
      (let ((available (get available equipment-data))
            (deposit (get deposit-required equipment-data))
            (sender tx-sender)
            (owner (get owner equipment-data))
            (name (get name equipment-data)))
        (if available
          (if (>= deposit u0)
            (begin
              ;; Transfer STX deposit to contract
              (try! (stx-transfer? deposit sender (as-contract tx-sender)))

              ;; Record deposit
              (map-set deposits id
                {
                  borrower: sender,
                  amount: deposit
                }
              )

              ;; Update equipment status
              (map-set equipment id
                {
                  owner: owner,
                  name: name,
                  available: false,
                  borrower: (some sender),
                  deposit-required: deposit
                }
              )

              (ok true)
            )
            ERR-NOT-FOUND
          )
          ERR-NOT-AVAILABLE
        )
      )
    ERR-NOT-FOUND
  )
)

;; (3) Owner Approves Borrow (optional control layer)
(define-public (approve-borrow (id uint))
  (match (map-get? equipment id)
    equipment-data
      (begin
        (asserts! (is-eq tx-sender (get owner equipment-data)) ERR-NOT-OWNER)
        (asserts! (not (get available equipment-data)) ERR-NOT-AVAILABLE)
        (ok true)
      )
    ERR-NOT-FOUND
  )
)

;; (4) Return Equipment & Refund Deposit
(define-public (return-equipment (id uint))
  (match (map-get? equipment id)
    equipment-data
      (let ((borrower (get borrower equipment-data))
            (owner (get owner equipment-data))
            (name (get name equipment-data))
            (deposit-req (get deposit-required equipment-data)))
        (if (is-eq (some tx-sender) borrower)
          (match (map-get? deposits id)
            deposit-data
              (let ((dep-amount (get amount deposit-data))
                    (borrower-principal (get borrower deposit-data)))
                (begin
                  ;; Refund deposit
                  (try!
                    (as-contract
                      (stx-transfer?
                        dep-amount
                        tx-sender
                        borrower-principal
                      )
                    )
                  )

                  ;; Remove deposit record
                  (map-delete deposits id)

                  ;; Reset equipment
                  (map-set equipment id
                    {
                      owner: owner,
                      name: name,
                      available: true,
                      borrower: none,
                      deposit-required: deposit-req
                    }
                  )

                  (ok true)
                )
              )
            ERR-NOT-FOUND
          )
          ERR-NOT-BORROWER
        )
      )
    ERR-NOT-FOUND
  )
)

;; =========================
;; READ-ONLY FUNCTIONS
;; =========================

(define-read-only (get-equipment (id uint))
  (map-get? equipment id)
)

(define-read-only (get-deposit (id uint))
  (map-get? deposits id)
)

(define-read-only (get-total-equipment)
  (var-get equipment-id-counter)
)
