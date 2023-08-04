# teddy-backend

Backend for teddy swap. Consists of two components:

1. *Batcher* Watches the blockchain and builds transactions that fulfil orders.
2. *API* Runs queries for end-users. Backed by db-sync.

## TO DO

- [ ] dev env: Set up CI
- [ ] Batcher: Basic tests for scripts / hashes (0.5d)
- [ ] Batcher: Unit tests for all user flows (3d)
- [ ] Batcher: Chain follower watching on-chain state (10d)
  - CLI
    - start / restore
    - wallet stuff: generate wallet, wallet info
  - State store / restore
  - Build transactions
- [ ] Batcher: Integration tests for some(?) use cases (2d)
- [ ] Batcher: Watch mempool (5d)
- [ ] Batcher: Multiple transactions in one block (3d)
- [ ] Batcher: Monitoring, metrics, etc. (2d)

- [ ] API: Define all routes as servant API (2d)
- [ ] API: Implement routes as SQL queries (4d)
- [ ] API: Integration tests together with batcher (3d)

Maybe:

- Load tests
- Deployment
- Documentation
- Docker image