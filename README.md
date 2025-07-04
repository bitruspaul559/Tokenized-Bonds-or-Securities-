# 🏦 Tokenized Bonds Smart Contract

A Clarity smart contract for issuing and managing tokenized bonds on the Stacks blockchain, enabling fractional investment in traditional securities.

## 🚀 Features

- 📊 **Bond Issuance**: Create tokenized bonds with customizable parameters
- 💰 **Fractional Investment**: Purchase partial bond holdings
- 🔄 **Transfer System**: Transfer bond tokens between holders
- 💸 **Coupon Payments**: Claim periodic interest payments
- 🎯 **Bond Redemption**: Redeem bonds at maturity for face value
- 🔐 **Approval System**: Delegate spending rights to other principals

## 📋 Contract Functions

### Read-Only Functions

- `get-bond-info(bond-id)` - Get bond details
- `get-bond-balance(bond-id, holder)` - Check bond balance
- `get-bond-allowance(bond-id, owner, spender)` - Check spending allowance
- `calculate-coupon-payment(bond-id, holder-balance)` - Calculate coupon amount
- `is-bond-mature(bond-id)` - Check if bond has reached maturity

### Public Functions

- `issue-bond(name, symbol, total-supply, face-value, coupon-rate, maturity-blocks)` - Issue new bond
- `purchase-bond(bond-id, amount)` - Buy bond tokens
- `transfer-bond(bond-id, amount, recipient)` - Transfer bond tokens
- `approve-bond(bond-id, spender, amount)` - Approve spending
- `transfer-from-bond(bond-id, owner, recipient, amount)` - Transfer on behalf
- `claim-coupon(bond-id)` - Claim interest payment
- `redeem-bond(bond-id)` - Redeem mature bonds
- `fund-contract()` - Fund contract for payments (owner only)

## 🛠 Usage Examples

### Issue a Bond

```clarity
(contract-call? .tokenized-bonds issue-bond 
  "Corporate Bond 2024" 
  "CB2024" 
  u1000000 
  u1000 
  u500 
  u52560)
```

### Purchase Bond Tokens

```clarity
(contract-call? .tokenized-bonds purchase-bond u1 u10000)
```

### Transfer Bonds

```clarity
(contract-call? .tokenized-bonds transfer-bond u1 u5000 'SP2J6ZY48GV1EZ5V2V5RB9MP66SW86PYKKNRV9EJ7)
```

### Claim Coupon Payment

```clarity
(contract-call? .tokenized-bonds claim-coupon u1)
```

### Redeem Mature Bonds

```clarity
(contract-call? .tokenized-bonds redeem-bond u1)
```

## 📊 Bond Parameters

- **Face Value**: The redemption value at maturity
- **Coupon Rate**: Annual interest rate (in basis points, e.g., 500 = 5%)
- **Total Supply**: Number of fractional units
- **Maturity Blocks**: Duration until bond matures

## 🔧 Setup Instructions

1. Clone this repository
2. Install Clarinet
3. Deploy the contract to your local testnet
4. Fund the contract using `fund-contract()` function
5. Start issuing and trading bonds!

## ⚠️ Important Notes

- Only contract owner can issue bonds and fund the contract
- Bonds must be purchased before maturity
- Coupon payments require contract to be funded
- Bond redemption only available after maturity

## 🧪 Testing

Use Clarinet console to test contract functions:

```bash
clarinet console
```

## 📈 Investment Flow

1. 🏭 **Issuer** creates bond with specific terms
2. 💳 **Investors** purchase fractional bond tokens
3. 📅 **Periodic** coupon payments claimed by holders
4. 🎯 **Maturity** bond tokens redeemed for face value

## 🔒 Security Features

- Owner-only bond issuance
- Balance validation on all transfers
- Maturity checks for redemption
- Allowance system for delegated transfers
```

```bash
git commit -m "feat: implement tokenized bonds MVP with issuance, trading, and redem
