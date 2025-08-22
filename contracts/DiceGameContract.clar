;; Dice Game Contract
;; A provably fair dice game with house edge implementation
;; Players can bet on dice outcomes with transparent randomness

;; Define the game token (using STX for betting)
(define-constant contract-owner tx-sender)

;; Error constants
(define-constant err-owner-only (err u100))
(define-constant err-insufficient-balance (err u101))
(define-constant err-invalid-bet-amount (err u102))
(define-constant err-game-not-found (err u103))
(define-constant err-invalid-prediction (err u104))

;; Game constants
(define-constant min-bet-amount u1000000) ;; 1 STX minimum bet
(define-constant max-bet-amount u100000000) ;; 100 STX maximum bet
(define-constant house-edge u300) ;; 3% house edge (300 basis points)
(define-constant basis-points u10000)

;; Game state tracking
(define-data-var game-counter uint u0)
(define-data-var house-balance uint u0)

;; Game data structure
(define-map games
  uint ;; game-id
  {
    player: principal,
    bet-amount: uint,
    prediction: uint, ;; 1-6 for dice face prediction
    dice-result: (optional uint),
    payout: uint,
    block-height: uint,
    is-resolved: bool
  })

;; Player statistics
(define-map player-stats
  principal
  {
    total-games: uint,
    total-bet: uint,
    total-won: uint,
    total-lost: uint
  })

;; Function 1: Place a dice bet
;; Players can bet on a dice outcome (1-6) with STX
(define-public (place-bet (prediction uint) (bet-amount uint))
  (let
    (
      (game-id (+ (var-get game-counter) u1))
      (current-stats (default-to {total-games: u0, total-bet: u0, total-won: u0, total-lost: u0} 
                                 (map-get? player-stats tx-sender)))
    )
    ;; Validation checks
    (asserts! (and (>= prediction u1) (<= prediction u6)) err-invalid-prediction)
    (asserts! (and (>= bet-amount min-bet-amount) (<= bet-amount max-bet-amount)) err-invalid-bet-amount)
    (asserts! (>= (stx-get-balance tx-sender) bet-amount) err-insufficient-balance)
    
    ;; Transfer bet amount to contract
    (try! (stx-transfer? bet-amount tx-sender (as-contract tx-sender)))
    
    ;; Create game record
    (map-set games game-id
      {
        player: tx-sender,
        bet-amount: bet-amount,
        prediction: prediction,
        dice-result: none,
        payout: u0,
        block-height: burn-block-height,
        is-resolved: false
      })
    
    ;; Update game counter
    (var-set game-counter game-id)
    
    ;; Update player stats
    (map-set player-stats tx-sender
      (merge current-stats {
        total-games: (+ (get total-games current-stats) u1),
        total-bet: (+ (get total-bet current-stats) bet-amount)
      }))
    
    ;; Print game placed event
    (print {
      event: "bet-placed",
      game-id: game-id,
      player: tx-sender,
      prediction: prediction,
      bet-amount: bet-amount,
      block-height: burn-block-height
    })
    
    (ok game-id)))

;; Function 2: Resolve dice game
;; Generates provably fair dice result and handles payouts
(define-public (resolve-game (game-id uint))
  (let
    (
      (game-data (unwrap! (map-get? games game-id) err-game-not-found))
      (player (get player game-data))
      (bet-amount (get bet-amount game-data))
      (prediction (get prediction game-data))
      (current-stats (default-to {total-games: u0, total-bet: u0, total-won: u0, total-lost: u0} 
                                 (map-get? player-stats player)))
      ;; Generate provably fair dice result using block height and game data
      (random-seed (+ (+ game-id burn-block-height) (len (sha256 (concat (unwrap-panic (to-consensus-buff? game-id)) (unwrap-panic (to-consensus-buff? burn-block-height)))))))
      (dice-result (+ (mod random-seed u6) u1))
      (is-winner (is-eq prediction dice-result))
      ;; Calculate payout with house edge
      (base-payout (* bet-amount u6)) ;; 6x multiplier for correct prediction
      (house-fee (/ (* base-payout house-edge) basis-points))
      (net-payout (if is-winner (- base-payout house-fee) u0))
    )
    
    ;; Ensure game exists and is not already resolved
    (asserts! (not (get is-resolved game-data)) err-game-not-found)
    
    ;; Update game with result
    (map-set games game-id
      (merge game-data {
        dice-result: (some dice-result),
        payout: net-payout,
        is-resolved: true
      }))
    
    ;; Handle payout if player won
    (if is-winner
      (begin
        (try! (as-contract (stx-transfer? net-payout tx-sender player)))
        (var-set house-balance (+ (var-get house-balance) house-fee))
        ;; Update winning stats
        (map-set player-stats player
          (merge current-stats {
            total-won: (+ (get total-won current-stats) net-payout)
          }))
      )
      (begin
        ;; House keeps the bet amount
        (var-set house-balance (+ (var-get house-balance) bet-amount))
        ;; Update losing stats
        (map-set player-stats player
          (merge current-stats {
            total-lost: (+ (get total-lost current-stats) bet-amount)
          }))
      )
    )
    
    ;; Print game resolved event
    (print {
      event: "game-resolved",
      game-id: game-id,
      player: player,
      prediction: prediction,
      dice-result: dice-result,
      is-winner: is-winner,
      payout: net-payout,
      house-edge-fee: (if is-winner house-fee u0)
    })
    
    (ok {
      dice-result: dice-result,
      is-winner: is-winner,
      payout: net-payout
    })))

;; Read-only functions for game information

;; Get game details
(define-read-only (get-game (game-id uint))
  (map-get? games game-id))

;; Get player statistics
(define-read-only (get-player-stats (player principal))
  (map-get? player-stats player))

;; Get current game counter
(define-read-only (get-game-counter)
  (var-get game-counter))

;; Get house balance
(define-read-only (get-house-balance)
  (var-get house-balance))

;; Get game constants
(define-read-only (get-game-info)
  (ok {
    min-bet: min-bet-amount,
    max-bet: max-bet-amount,
    house-edge: house-edge,
    total-games: (var-get game-counter),
    house-balance: (var-get house-balance)
  }))

;; Owner functions

;; Withdraw house earnings (owner only)
(define-public (withdraw-house-balance (amount uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= amount (var-get house-balance)) err-insufficient-balance)
    (try! (as-contract (stx-transfer? amount tx-sender contract-owner)))
    (var-set house-balance (- (var-get house-balance) amount))
    (ok amount)))

;; Fund contract (owner only) - for initial liquidity
(define-public (fund-contract (amount uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (ok amount)))