;; Carbon Credit Batch Processor
;; Efficient batch operations for carbon credits

(define-constant ERR-NOT-AUTHORIZED (err u200))
(define-constant ERR-INVALID-BATCH-SIZE (err u201))
(define-constant ERR-BATCH-LIMIT-EXCEEDED (err u202))
(define-constant ERR-INVALID-OPERATION (err u203))
(define-constant ERR-BATCH-PROCESSING-FAILED (err u204))

;; Maximum batch size to prevent gas limit issues
(define-constant MAX-BATCH-SIZE u20)

;; Reference to main contract
(define-data-var main-contract principal tx-sender)
(define-data-var batch-counter uint u0)

;; Batch operation tracking
(define-map batch-operations
    uint
    {
        initiator: principal,
        operation-type: (string-ascii 20),
        operations-count: uint,
        successful-operations: uint,
        failed-operations: uint,
        total-amount: uint,
        processed-at: uint,
        completed: bool
    }
)

;; Batch transfer data structure
(define-map batch-transfers
    {batch-id: uint, index: uint}
    {
        project-id: uint,
        amount: uint,
        recipient: principal,
        success: bool
    }
)

;; Batch retirement data structure  
(define-map batch-retirements
    {batch-id: uint, index: uint}
    {
        project-id: uint,
        amount: uint,
        reason: (string-ascii 100),
        success: bool
    }
)

;; User batch statistics
(define-map user-batch-stats
    principal
    {
        total-batches: uint,
        successful-batches: uint,
        total-operations: uint,
        total-amount-processed: uint
    }
)

;; Batch multiple transfers in single transaction
(define-public (batch-transfer-credits 
    (transfers (list 20 {project-id: uint, amount: uint, recipient: principal})))
    (let 
        (
            (batch-id (+ (var-get batch-counter) u1))
            (batch-size (len transfers))
            (user-stats (get-user-batch-stats tx-sender))
        )
        (asserts! (> batch-size u0) ERR-INVALID-BATCH-SIZE)
        (asserts! (<= batch-size MAX-BATCH-SIZE) ERR-BATCH-LIMIT-EXCEEDED)
        
        ;; Initialize batch operation record
        (map-set batch-operations batch-id {
            initiator: tx-sender,
            operation-type: "batch-transfer",
            operations-count: batch-size,
            successful-operations: u0,
            failed-operations: u0,
            total-amount: (fold calculate-total-amount transfers u0),
            processed-at: stacks-block-height,
            completed: false
        })
        
        ;; Process each transfer
        (let 
            (
                (results (process-transfers transfers batch-id))
                (successful-count (get successful results))
                (failed-count (- batch-size successful-count))
            )
            
            ;; Update batch operation with results
            (map-set batch-operations batch-id {
                initiator: tx-sender,
                operation-type: "batch-transfer",
                operations-count: batch-size,
                successful-operations: successful-count,
                failed-operations: failed-count,
                total-amount: (fold calculate-total-amount transfers u0),
                processed-at: stacks-block-height,
                completed: true
            })
            
            ;; Update user statistics
            (update-user-batch-stats tx-sender batch-size successful-count 
                (fold calculate-total-amount transfers u0) (is-eq failed-count u0))
            
            (var-set batch-counter batch-id)
            (ok {batch-id: batch-id, successful: successful-count, failed: failed-count})
        )
    )
)

;; Batch retirement of multiple carbon credits
(define-public (batch-retire-credits 
    (retirements (list 20 {project-id: uint, amount: uint, reason: (string-ascii 100)})))
    (let 
        (
            (batch-id (+ (var-get batch-counter) u1))
            (batch-size (len retirements))
            (user-stats (get-user-batch-stats tx-sender))
        )
        (asserts! (> batch-size u0) ERR-INVALID-BATCH-SIZE)
        (asserts! (<= batch-size MAX-BATCH-SIZE) ERR-BATCH-LIMIT-EXCEEDED)
        
        ;; Initialize batch operation record
        (map-set batch-operations batch-id {
            initiator: tx-sender,
            operation-type: "batch-retire",
            operations-count: batch-size,
            successful-operations: u0,
            failed-operations: u0,
            total-amount: (fold calculate-total-retirement-amount retirements u0),
            processed-at: stacks-block-height,
            completed: false
        })
        
        ;; Process each retirement
        (let 
            (
                (results (process-retirements retirements batch-id))
                (successful-count (get successful results))
                (failed-count (- batch-size successful-count))
            )
            
            ;; Update batch operation with results
            (map-set batch-operations batch-id {
                initiator: tx-sender,
                operation-type: "batch-retire",
                operations-count: batch-size,
                successful-operations: successful-count,
                failed-operations: failed-count,
                total-amount: (fold calculate-total-retirement-amount retirements u0),
                processed-at: stacks-block-height,
                completed: true
            })
            
            ;; Update user statistics
            (update-user-batch-stats tx-sender batch-size successful-count 
                (fold calculate-total-retirement-amount retirements u0) (is-eq failed-count u0))
            
            (var-set batch-counter batch-id)
            (ok {batch-id: batch-id, successful: successful-count, failed: failed-count})
        )
    )
)

;; Process individual transfers within batch
(define-private (process-transfers 
    (transfers (list 20 {project-id: uint, amount: uint, recipient: principal})) 
    (batch-id uint))
    (fold process-single-transfer transfers {batch-id: batch-id, index: u0, successful: u0})
)

;; Process single transfer operation
(define-private (process-single-transfer 
    (transfer {project-id: uint, amount: uint, recipient: principal})
    (context {batch-id: uint, index: uint, successful: uint}))
    (let 
        (
            (success (is-ok (contract-call? .Carbonet transfer-credits 
                (get project-id transfer) (get amount transfer) (get recipient transfer))))
        )
        ;; Store transfer result
        (map-set batch-transfers {batch-id: (get batch-id context), index: (get index context)} {
            project-id: (get project-id transfer),
            amount: (get amount transfer),
            recipient: (get recipient transfer),
            success: success
        })
        
        ;; Update context
        {
            batch-id: (get batch-id context),
            index: (+ (get index context) u1),
            successful: (if success (+ (get successful context) u1) (get successful context))
        }
    )
)

;; Process individual retirements within batch
(define-private (process-retirements 
    (retirements (list 20 {project-id: uint, amount: uint, reason: (string-ascii 100)})) 
    (batch-id uint))
    (fold process-single-retirement retirements {batch-id: batch-id, index: u0, successful: u0})
)

;; Process single retirement operation
(define-private (process-single-retirement 
    (retirement {project-id: uint, amount: uint, reason: (string-ascii 100)})
    (context {batch-id: uint, index: uint, successful: uint}))
    (let 
        (
            (success (is-ok (contract-call? .Carbonet retire-credits 
                (get project-id retirement) (get amount retirement) (get reason retirement))))
        )
        ;; Store retirement result
        (map-set batch-retirements {batch-id: (get batch-id context), index: (get index context)} {
            project-id: (get project-id retirement),
            amount: (get amount retirement),
            reason: (get reason retirement),
            success: success
        })
        
        ;; Update context
        {
            batch-id: (get batch-id context),
            index: (+ (get index context) u1),
            successful: (if success (+ (get successful context) u1) (get successful context))
        }
    )
)

;; Calculate total amount for transfers
(define-private (calculate-total-amount 
    (transfer {project-id: uint, amount: uint, recipient: principal})
    (acc uint))
    (+ acc (get amount transfer))
)

;; Calculate total amount for retirements
(define-private (calculate-total-retirement-amount 
    (retirement {project-id: uint, amount: uint, reason: (string-ascii 100)})
    (acc uint))
    (+ acc (get amount retirement))
)

;; Update user batch statistics
(define-private (update-user-batch-stats 
    (user principal) 
    (operations-count uint) 
    (successful-count uint) 
    (total-amount uint) 
    (batch-success bool))
    (let 
        (
            (current-stats (get-user-batch-stats user))
        )
        (map-set user-batch-stats user {
            total-batches: (+ (get total-batches current-stats) u1),
            successful-batches: (+ (get successful-batches current-stats) 
                                 (if batch-success u1 u0)),
            total-operations: (+ (get total-operations current-stats) operations-count),
            total-amount-processed: (+ (get total-amount-processed current-stats) total-amount)
        })
    )
)

;; Read-only functions
(define-read-only (get-batch-operation (batch-id uint))
    (map-get? batch-operations batch-id)
)

(define-read-only (get-batch-transfer (batch-id uint) (index uint))
    (map-get? batch-transfers {batch-id: batch-id, index: index})
)

(define-read-only (get-batch-retirement (batch-id uint) (index uint))
    (map-get? batch-retirements {batch-id: batch-id, index: index})
)

(define-read-only (get-user-batch-stats (user principal))
    (default-to 
        {
            total-batches: u0,
            successful-batches: u0,
            total-operations: u0,
            total-amount-processed: u0
        }
        (map-get? user-batch-stats user)
    )
)

(define-read-only (get-batch-counter)
    (var-get batch-counter)
)

(define-read-only (get-max-batch-size)
    MAX-BATCH-SIZE
)

;; Administrative functions
(define-public (set-main-contract (contract-address principal))
    (begin
        (asserts! (is-eq tx-sender (var-get main-contract)) ERR-NOT-AUTHORIZED)
        (var-set main-contract contract-address)
        (ok true)
    )
)
