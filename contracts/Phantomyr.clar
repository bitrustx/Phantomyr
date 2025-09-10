;; Phantomyr - Ghostly Guardian Registry Contract
;; Tracks and manages abandoned Stacks ecosystem projects

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u401))
(define-constant ERR_NOT_FOUND (err u404))
(define-constant ERR_ALREADY_EXISTS (err u409))
(define-constant ERR_INVALID_INPUT (err u400))
(define-constant ERR_INVALID_STATUS (err u422))

;; Data Variables
(define-data-var contract-owner principal tx-sender)
(define-data-var total-projects uint u0)
(define-data-var total-revivals uint u0)

;; Project Status Types
(define-constant STATUS_ACTIVE u1)
(define-constant STATUS_ABANDONED u2)
(define-constant STATUS_REVIVING u3)
(define-constant STATUS_REVIVED u4)

;; Data Maps
(define-map projects
  { project-id: uint }
  {
    name: (string-ascii 64),
    repository-url: (string-ascii 256),
    owner: principal,
    status: uint,
    last-activity: uint,
    revival-count: uint,
    created-at: uint,
    updated-at: uint
  }
)

(define-map project-by-url
  { repository-url: (string-ascii 256) }
  { project-id: uint }
)

(define-map revivals
  { revival-id: uint }
  {
    project-id: uint,
    reviver: principal,
    pr-url: (string-ascii 256),
    status: uint,
    created-at: uint,
    completed-at: (optional uint)
  }
)

(define-map user-projects
  { user: principal, project-id: uint }
  { is-owner: bool }
)

;; Read-only functions
(define-read-only (get-contract-owner)
  (var-get contract-owner)
)

(define-read-only (get-total-projects)
  (var-get total-projects)
)

(define-read-only (get-total-revivals)
  (var-get total-revivals)
)

(define-read-only (get-project (project-id uint))
  (map-get? projects { project-id: project-id })
)

(define-read-only (get-project-by-url (repository-url (string-ascii 256)))
  (match (map-get? project-by-url { repository-url: repository-url })
    project-data (get-project (get project-id project-data))
    none
  )
)

(define-read-only (get-revival (revival-id uint))
  (map-get? revivals { revival-id: revival-id })
)

(define-read-only (is-project-owner (project-id uint) (user principal))
  (default-to false 
    (get is-owner (map-get? user-projects { user: user, project-id: project-id }))
  )
)

(define-read-only (get-current-block-height)
  stacks-block-height
)

;; Validation functions
(define-private (is-valid-status (status uint))
  (or 
    (is-eq status STATUS_ACTIVE)
    (or 
      (is-eq status STATUS_ABANDONED)
      (or 
        (is-eq status STATUS_REVIVING)
        (is-eq status STATUS_REVIVED)
      )
    )
  )
)

(define-private (is-valid-string (str (string-ascii 256)))
  (and 
    (> (len str) u0)
    (<= (len str) u256)
  )
)

(define-private (is-valid-name (name (string-ascii 64)))
  (and 
    (> (len name) u0)
    (<= (len name) u64)
  )
)

;; Public functions
(define-public (register-project 
  (name (string-ascii 64))
  (repository-url (string-ascii 256))
)
  (let (
    (project-id (+ (var-get total-projects) u1))
    (current-height (get-current-block-height))
  )
    ;; Validate inputs
    (asserts! (is-valid-name name) ERR_INVALID_INPUT)
    (asserts! (is-valid-string repository-url) ERR_INVALID_INPUT)
    (asserts! (is-none (map-get? project-by-url { repository-url: repository-url })) ERR_ALREADY_EXISTS)
    
    ;; Create project
    (map-set projects
      { project-id: project-id }
      {
        name: name,
        repository-url: repository-url,
        owner: tx-sender,
        status: STATUS_ACTIVE,
        last-activity: current-height,
        revival-count: u0,
        created-at: current-height,
        updated-at: current-height
      }
    )
    
    ;; Create URL mapping
    (map-set project-by-url
      { repository-url: repository-url }
      { project-id: project-id }
    )
    
    ;; Set ownership
    (map-set user-projects
      { user: tx-sender, project-id: project-id }
      { is-owner: true }
    )
    
    ;; Update counter
    (var-set total-projects project-id)
    
    (ok project-id)
  )
)

(define-public (update-project-status 
  (project-id uint)
  (new-status uint)
)
  (let (
    (project (unwrap! (get-project project-id) ERR_NOT_FOUND))
    (current-height (get-current-block-height))
  )
    ;; Validate inputs
    (asserts! (is-valid-status new-status) ERR_INVALID_STATUS)
    (asserts! (or (is-project-owner project-id tx-sender) (is-eq tx-sender (var-get contract-owner))) ERR_UNAUTHORIZED)
    
    ;; Update project
    (map-set projects
      { project-id: project-id }
      (merge project {
        status: new-status,
        updated-at: current-height
      })
    )
    
    (ok true)
  )
)

(define-public (start-revival 
  (project-id uint)
  (pr-url (string-ascii 256))
)
  (let (
    (project (unwrap! (get-project project-id) ERR_NOT_FOUND))
    (revival-id (+ (var-get total-revivals) u1))
    (current-height (get-current-block-height))
  )
    ;; Validate inputs
    (asserts! (is-valid-string pr-url) ERR_INVALID_INPUT)
    (asserts! (is-eq (get status project) STATUS_ABANDONED) ERR_INVALID_STATUS)
    
    ;; Create revival
    (map-set revivals
      { revival-id: revival-id }
      {
        project-id: project-id,
        reviver: tx-sender,
        pr-url: pr-url,
        status: STATUS_REVIVING,
        created-at: current-height,
        completed-at: none
      }
    )
    
    ;; Update project status
    (map-set projects
      { project-id: project-id }
      (merge project {
        status: STATUS_REVIVING,
        updated-at: current-height
      })
    )
    
    ;; Update counter
    (var-set total-revivals revival-id)
    
    (ok revival-id)
  )
)

(define-public (complete-revival (revival-id uint))
  (let (
    (revival-data (unwrap! (get-revival revival-id) ERR_NOT_FOUND))
    (project-id (get project-id revival-data))
    (project-data (unwrap! (get-project project-id) ERR_NOT_FOUND))
    (current-height (get-current-block-height))
    (revival-status (get status revival-data))
    (revival-reviver (get reviver revival-data))
    (project-revival-count (get revival-count project-data))
  )
    ;; Validate authorization
    (asserts! (or (is-eq tx-sender revival-reviver) (is-eq tx-sender (var-get contract-owner))) ERR_UNAUTHORIZED)
    (asserts! (is-eq revival-status STATUS_REVIVING) ERR_INVALID_STATUS)
    
    ;; Update revival
    (map-set revivals
      { revival-id: revival-id }
      (merge revival-data {
        status: STATUS_REVIVED,
        completed-at: (some current-height)
      })
    )
    
    ;; Update project
    (map-set projects
      { project-id: project-id }
      (merge project-data {
        status: STATUS_REVIVED,
        revival-count: (+ project-revival-count u1),
        updated-at: current-height
      })
    )
    
    (ok true)
  )
)

(define-public (update-activity (project-id uint))
  (let (
    (project (unwrap! (get-project project-id) ERR_NOT_FOUND))
    (current-height (get-current-block-height))
  )
    ;; Validate authorization
    (asserts! (or (is-project-owner project-id tx-sender) (is-eq tx-sender (var-get contract-owner))) ERR_UNAUTHORIZED)
    
    ;; Update activity
    (map-set projects
      { project-id: project-id }
      (merge project {
        last-activity: current-height,
        updated-at: current-height
      })
    )
    
    (ok true)
  )
)

;; Admin functions
(define-public (set-contract-owner (new-owner principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_UNAUTHORIZED)
    ;; Validate that new-owner is not the same as current owner
    (asserts! (not (is-eq new-owner (var-get contract-owner))) ERR_INVALID_INPUT)
    ;; Validate that new-owner is not the zero address (standard-principal check)
    (asserts! (is-standard new-owner) ERR_INVALID_INPUT)
    (var-set contract-owner new-owner)
    (ok true)
  )
)