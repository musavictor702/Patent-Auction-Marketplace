# Patent Auction Marketplace

A decentralized marketplace for auctioning patents on the Stacks blockchain.

## Overview

This smart contract enables companies to bid on tokenized patents in a transparent and secure auction format. Patent owners can create auctions for their intellectual property, and interested parties can place bids to acquire ownership rights.

## Features

- Create patent auctions with customizable parameters
- Place bids on active auctions
- Cancel auctions (if no bids have been placed)
- End auctions after their designated time period
- Claim patents after winning an auction
- Withdraw bids for non-winning participants

## Contract Functions

### Read-Only Functions

- `get-auction`: Retrieve details about a specific auction
- `get-bid`: Get information about a bid placed by a specific address
- `get-patent-owner`: Find the current owner of a patent
- `get-auction-status`: Check the status of an auction
- `is-auction-active`: Determine if an auction is currently active
- `get-next-auction-id`: Get the ID that will be assigned to the next created auction

### Public Functions

- `create-auction`: Create a new patent auction
- `place-bid`: Place a bid on an active auction
- `cancel-auction`: Cancel an auction (only if no bids have been placed)
- `end-auction`: End an auction after its designated end block
- `claim-patent`: Claim a patent after winning an auction
- `withdraw-bid`: Withdraw a bid (only for non-winning bids or cancelled auctions)

## Usage Examples

### Creating an Auction

```clarity
(contract-call? .auction-market create-auction "PATENT-123" "Revolutionary AI Algorithm" u1000 u5000)
```

### Placing a Bid

```clarity
(contract-call? .auction-market place-bid u1 u6000)
```

### Claiming a Patent

```clarity
(contract-call? .auction-market claim-patent u1)
```

## Error Codes

- `u100`: Not the contract owner
- `u101`: Item not found
- `u102`: Auction already exists
- `u103`: Auction not active
- `u104`: Auction already ended
- `u105`: Bid amount too low
- `u106`: Cannot cancel auction with bids
- `u107`: Not the highest bidder
- `u108`: Auction not ended yet
- `u109`: Patent already claimed

## License

MIT