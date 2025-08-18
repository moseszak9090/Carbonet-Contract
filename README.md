# Carbonet - Carbon Offset Tokenization Smart Contract

A Clarity smart contract for tokenizing verified carbon offset credits on the Stacks blockchain. Carbonet enables the creation, trading, verification, and retirement of carbon offset tokens with full transparency and immutable tracking.

## Features

- **Project Creation**: Register carbon offset projects with metadata
- **Credit Tokenization**: Mint carbon offset credits as blockchain tokens
- **Verification System**: Authorized verifiers can validate carbon projects
- **Trading & Transfers**: Transfer carbon credits between users
- **Credit Retirement**: Permanently retire credits to claim carbon offset benefits
- **Balance Tracking**: Track active and retired balances per user
- **Price Management**: Set and update credit pricing per project

## Contract Functions

### Administrative Functions

- `add-authorized-verifier` - Add authorized verifier (contract owner only)
- `remove-authorized-verifier` - Remove verifier authorization
- `transfer-ownership` - Transfer contract ownership

### Project Management

- `create-carbon-project` - Register new carbon offset project
- `verify-project` - Verify project legitimacy (authorized verifiers only)
- `deactivate-project` - Deactivate a project
- `update-project-price` - Update price per credit

### Token Operations

- `mint-carbon-credits` - Issue new carbon credits (project developers only)
- `transfer-credits` - Transfer credits between users
- `retire-credits` - Permanently retire credits with reason

### Read-Only Functions

- `get-project-info` - Get project details
- `get-token-info` - Get token information
- `get-user-balance` - Get user balance for specific project
- `get-user-total-balance` - Get user's total active balance
- `get-user-retired-balance` - Get user's total retired balance
- `get-project-credits-available` - Get remaining credits for project
- `calculate-purchase-cost` - Calculate cost for purchasing credits

## Usage Examples

### Deploy and Setup

1. Deploy the contract to Stacks blockchain
2. Add authorized verifiers using `add-authorized-verifier`

### Create Carbon Project

```clarity
(contract-call? .carbonet create-carbon-project 
  "Amazon Rainforest Conservation" 
  "Brazil" 
  "REDD+" 
  u2024 
  u10000 
  u25 
  'SP1VERIFIER123...)
```

### Verify Project

```clarity
(contract-call? .carbonet verify-project u1)
```

### Mint Carbon Credits

```clarity
(contract-call? .carbonet mint-carbon-credits u1 u1000 'SP1BUYER123...)
```

### Transfer Credits

```clarity
(contract-call? .carbonet transfer-credits u1 u500 'SP1RECIPIENT123...)
```

### Retire Credits

```clarity
(contract-call? .carbonet retire-credits u1 u100 "Corporate carbon neutrality")
```

## Data Structures

### Carbon Project
- Project name, location, methodology
- Vintage year and developer information
- Total and issued credits tracking
- Verification status and pricing

### Carbon Token
- Associated project ID and owner
- Credit amount and retirement status
- Retirement date and reason (if retired)

## Error Codes

- `u100` - Not authorized
- `u101` - Invalid amount
- `u102` - Insufficient balance
- `u103` - Project not found
- `u104` - Token not found
- `u105` - Already retired
- `u106` - Invalid project
- `u107` - Transfer failed
- `u108` - Unauthorized verifier
- `u109` - Project already exists

## Testing

Run tests using Clarinet:

```bash
clarinet test
```

## Security Considerations

- Only authorized verifiers can verify projects
- Only project developers can mint credits for their projects
- Credits cannot be double-spent or duplicated
- Retirement is permanent and irreversible
- All transactions are immutably recorded on blockchain

## License

This project is open source and available under the MIT License.
