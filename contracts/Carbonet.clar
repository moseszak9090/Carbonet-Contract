(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-AMOUNT (err u101))
(define-constant ERR-INSUFFICIENT-BALANCE (err u102))
(define-constant ERR-PROJECT-NOT-FOUND (err u103))
(define-constant ERR-TOKEN-NOT-FOUND (err u104))
(define-constant ERR-ALREADY-RETIRED (err u105))
(define-constant ERR-INVALID-PROJECT (err u106))
(define-constant ERR-TRANSFER-FAILED (err u107))
(define-constant ERR-UNAUTHORIZED-VERIFIER (err u108))
(define-constant ERR-PROJECT-ALREADY-EXISTS (err u109))

(define-data-var contract-owner principal tx-sender)
(define-data-var next-project-id uint u1)
(define-data-var next-token-id uint u1)
(define-data-var total-supply uint u0)
(define-data-var total-retired uint u0)

(define-map authorized-verifiers principal bool)
(define-map carbon-projects 
    uint 
    {
        project-name: (string-ascii 100),
        location: (string-ascii 100),
        methodology: (string-ascii 50),
        vintage-year: uint,
        developer: principal,
        verifier: principal,
        total-credits: uint,
        issued-credits: uint,
        price-per-credit: uint,
        verified: bool,
        active: bool
    }
)

(define-map carbon-tokens
    uint
    {
        project-id: uint,
        owner: principal,
        amount: uint,
        retired: bool,
        retirement-date: (optional uint),
        retirement-reason: (optional (string-ascii 100))
    }
)

(define-map user-balances {user: principal, project-id: uint} uint)
(define-map user-total-balance principal uint)
(define-map user-retired-balance principal uint)
(define-map project-token-supply uint uint)

(define-read-only (get-contract-owner)
    (var-get contract-owner)
)

(define-read-only (get-total-supply)
    (var-get total-supply)
)

(define-read-only (get-total-retired)
    (var-get total-retired)
)

(define-read-only (get-project-info (project-id uint))
    (map-get? carbon-projects project-id)
)

(define-read-only (get-token-info (token-id uint))
    (map-get? carbon-tokens token-id)
)

(define-read-only (get-user-balance (user principal) (project-id uint))
    (default-to u0 (map-get? user-balances {user: user, project-id: project-id}))
)

(define-read-only (get-user-total-balance (user principal))
    (default-to u0 (map-get? user-total-balance user))
)

(define-read-only (get-user-retired-balance (user principal))
    (default-to u0 (map-get? user-retired-balance user))
)

(define-read-only (is-authorized-verifier (verifier principal))
    (default-to false (map-get? authorized-verifiers verifier))
)

(define-read-only (get-project-token-supply (project-id uint))
    (default-to u0 (map-get? project-token-supply project-id))
)

(define-public (add-authorized-verifier (verifier principal))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (ok (map-set authorized-verifiers verifier true))
    )
)

(define-public (remove-authorized-verifier (verifier principal))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (ok (map-delete authorized-verifiers verifier))
    )
)

(define-public (create-carbon-project 
    (project-name (string-ascii 100))
    (location (string-ascii 100))
    (methodology (string-ascii 50))
    (vintage-year uint)
    (total-credits uint)
    (price-per-credit uint)
    (verifier principal))
    (let 
        (
            (project-id (var-get next-project-id))
        )
        (asserts! (> total-credits u0) ERR-INVALID-AMOUNT)
        (asserts! (> price-per-credit u0) ERR-INVALID-AMOUNT)
        (asserts! (is-none (map-get? carbon-projects project-id)) ERR-PROJECT-ALREADY-EXISTS)
        (map-set carbon-projects project-id {
            project-name: project-name,
            location: location,
            methodology: methodology,
            vintage-year: vintage-year,
            developer: tx-sender,
            verifier: verifier,
            total-credits: total-credits,
            issued-credits: u0,
            price-per-credit: price-per-credit,
            verified: false,
            active: true
        })
        (var-set next-project-id (+ project-id u1))
        (ok project-id)
    )
)

(define-public (verify-project (project-id uint))
    (let 
        (
            (project-data (unwrap! (map-get? carbon-projects project-id) ERR-PROJECT-NOT-FOUND))
        )
        (asserts! (is-eq tx-sender (get verifier project-data)) ERR-UNAUTHORIZED-VERIFIER)
        (asserts! (not (get verified project-data)) ERR-NOT-AUTHORIZED)
        (map-set carbon-projects project-id (merge project-data {verified: true}))
        (ok true)
    )
)

(define-public (mint-carbon-credits (project-id uint) (amount uint) (recipient principal))
    (let 
        (
            (project-data (unwrap! (map-get? carbon-projects project-id) ERR-PROJECT-NOT-FOUND))
            (token-id (var-get next-token-id))
            (current-issued (get issued-credits project-data))
            (new-issued (+ current-issued amount))
            (current-balance (get-user-balance recipient project-id))
            (current-total-balance (get-user-total-balance recipient))
            (current-project-supply (get-project-token-supply project-id))
        )
        (asserts! (is-eq tx-sender (get developer project-data)) ERR-NOT-AUTHORIZED)
        (asserts! (get verified project-data) ERR-INVALID-PROJECT)
        (asserts! (get active project-data) ERR-INVALID-PROJECT)
        (asserts! (> amount u0) ERR-INVALID-AMOUNT)
        (asserts! (<= new-issued (get total-credits project-data)) ERR-INVALID-AMOUNT)
        
        (map-set carbon-tokens token-id {
            project-id: project-id,
            owner: recipient,
            amount: amount,
            retired: false,
            retirement-date: none,
            retirement-reason: none
        })
        
        (map-set carbon-projects project-id (merge project-data {issued-credits: new-issued}))
        (map-set user-balances {user: recipient, project-id: project-id} (+ current-balance amount))
        (map-set user-total-balance recipient (+ current-total-balance amount))
        (map-set project-token-supply project-id (+ current-project-supply amount))
        (var-set next-token-id (+ token-id u1))
        (var-set total-supply (+ (var-get total-supply) amount))
        (ok token-id)
    )
)

(define-public (transfer-credits (project-id uint) (amount uint) (recipient principal))
    (let 
        (
            (sender-balance (get-user-balance tx-sender project-id))
            (recipient-balance (get-user-balance recipient project-id))
            (sender-total-balance (get-user-total-balance tx-sender))
            (recipient-total-balance (get-user-total-balance recipient))
        )
        (asserts! (> amount u0) ERR-INVALID-AMOUNT)
        (asserts! (>= sender-balance amount) ERR-INSUFFICIENT-BALANCE)
        (asserts! (not (is-eq tx-sender recipient)) ERR-TRANSFER-FAILED)
        
        (map-set user-balances {user: tx-sender, project-id: project-id} (- sender-balance amount))
        (map-set user-balances {user: recipient, project-id: project-id} (+ recipient-balance amount))
        (map-set user-total-balance tx-sender (- sender-total-balance amount))
        (map-set user-total-balance recipient (+ recipient-total-balance amount))
        (ok true)
    )
)

(define-public (retire-credits (project-id uint) (amount uint) (reason (string-ascii 100)))
    (let 
        (
            (user-balance (get-user-balance tx-sender project-id))
            (user-total-balance (get-user-total-balance tx-sender))
            (user-retired-balance (get-user-retired-balance tx-sender))
            (token-id (var-get next-token-id))
            (current-block (stacks-block-height))
        )
        (asserts! (> amount u0) ERR-INVALID-AMOUNT)
        (asserts! (>= user-balance amount) ERR-INSUFFICIENT-BALANCE)
        
        (map-set carbon-tokens token-id {
            project-id: project-id,
            owner: tx-sender,
            amount: amount,
            retired: true,
            retirement-date: (some current-block),
            retirement-reason: (some reason)
        })
        
        (map-set user-balances {user: tx-sender, project-id: project-id} (- user-balance amount))
        (map-set user-total-balance tx-sender (- user-total-balance amount))
        (map-set user-retired-balance tx-sender (+ user-retired-balance amount))
        (var-set next-token-id (+ token-id u1))
        (var-set total-supply (- (var-get total-supply) amount))
        (var-set total-retired (+ (var-get total-retired) amount))
        (ok token-id)
    )
)

(define-public (deactivate-project (project-id uint))
    (let 
        (
            (project-data (unwrap! (map-get? carbon-projects project-id) ERR-PROJECT-NOT-FOUND))
        )
        (asserts! (is-eq tx-sender (get developer project-data)) ERR-NOT-AUTHORIZED)
        (map-set carbon-projects project-id (merge project-data {active: false}))
        (ok true)
    )
)

(define-public (update-project-price (project-id uint) (new-price uint))
    (let 
        (
            (project-data (unwrap! (map-get? carbon-projects project-id) ERR-PROJECT-NOT-FOUND))
        )
        (asserts! (is-eq tx-sender (get developer project-data)) ERR-NOT-AUTHORIZED)
        (asserts! (> new-price u0) ERR-INVALID-AMOUNT)
        (map-set carbon-projects project-id (merge project-data {price-per-credit: new-price}))
        (ok true)
    )
)

(define-public (transfer-ownership (new-owner principal))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (var-set contract-owner new-owner)
        (ok true)
    )
)

(define-read-only (get-project-credits-available (project-id uint))
    (let 
        (
            (project-data (unwrap! (map-get? carbon-projects project-id) ERR-PROJECT-NOT-FOUND))
        )
        (ok (- (get total-credits project-data) (get issued-credits project-data)))
    )
)

(define-read-only (calculate-purchase-cost (project-id uint) (amount uint))
    (let 
        (
            (project-data (unwrap! (map-get? carbon-projects project-id) ERR-PROJECT-NOT-FOUND))
        )
        (ok (* amount (get price-per-credit project-data)))
    )
)
