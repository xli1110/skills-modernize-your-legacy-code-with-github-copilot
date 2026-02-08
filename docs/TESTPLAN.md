# Test Plan - COBOL Account Management System

## Purpose
This test plan validates the business logic and implementation of the legacy COBOL Account Management System. It will serve as the baseline for creating unit and integration tests during the migration to Node.js.

## Scope
- All business logic operations (View Balance, Credit, Debit)
- Data persistence and retrieval
- Business rule validation (overdraft protection, balance calculations)
- User interface interactions
- Error handling and edge cases

---

## Test Cases

| Test Case ID | Test Case Description | Pre-conditions | Test Steps | Expected Result | Actual Result | Status | Comments |
|--------------|----------------------|----------------|------------|-----------------|---------------|---------|----------|
| TC-001 | Verify initial account balance | Application started for the first time | 1. Start the application<br>2. Select option 1 (View Balance) | System displays "Current balance: 1000.00" | | | Initial balance should be $1,000.00 |
| TC-002 | View balance after application start | Application started | 1. Start application<br>2. Select option 1 (View Balance) | Balance displays correctly showing 1000.00 | | | Validates data layer READ operation |
| TC-003 | Credit account with valid amount | Application started, current balance: 1000.00 | 1. Select option 2 (Credit Account)<br>2. Enter amount: 500.00<br>3. Select option 1 to view balance | System displays "Amount credited. New balance: 1500.00"<br>Balance verification shows 1500.00 | | | Tests basic credit functionality |
| TC-004 | Credit account with decimal amount | Application started, current balance: 1000.00 | 1. Select option 2 (Credit Account)<br>2. Enter amount: 123.45<br>3. Select option 1 to view balance | System displays "Amount credited. New balance: 1123.45"<br>Balance shows 1123.45 | | | Validates decimal precision handling |
| TC-005 | Multiple credit transactions | Application started, current balance: 1000.00 | 1. Credit 200.00<br>2. Credit 300.50<br>3. Credit 99.50<br>4. View balance | Each credit shows updated balance<br>Final balance: 1600.00 | | | Tests cumulative credit operations |
| TC-006 | Debit account with sufficient funds | Application started, current balance: 1000.00 | 1. Select option 3 (Debit Account)<br>2. Enter amount: 300.00<br>3. Select option 1 to view balance | System displays "Amount debited. New balance: 700.00"<br>Balance shows 700.00 | | | Tests basic debit functionality |
| TC-007 | Debit account with exact balance amount | Current balance: 1000.00 | 1. Select option 3 (Debit Account)<br>2. Enter amount: 1000.00<br>3. View balance | System displays "Amount debited. New balance: 0.00"<br>Balance shows 0.00 | | | Edge case: debit entire balance |
| TC-008 | Debit account with insufficient funds | Current balance: 1000.00 | 1. Select option 3 (Debit Account)<br>2. Enter amount: 1500.00<br>3. View balance | System displays "Insufficient funds for this debit."<br>Balance remains 1000.00 (unchanged) | | | **Critical**: Overdraft protection validation |
| TC-009 | Debit exactly one cent more than balance | Current balance: 1000.00 | 1. Select option 3 (Debit Account)<br>2. Enter amount: 1000.01<br>3. View balance | System displays "Insufficient funds for this debit."<br>Balance remains 1000.00 | | | Edge case for overdraft protection |
| TC-010 | Multiple debit transactions | Current balance: 1000.00 | 1. Debit 100.00<br>2. Debit 200.00<br>3. Debit 150.50<br>4. View balance | Each debit shows updated balance<br>Final balance: 549.50 | | | Tests cumulative debit operations |
| TC-011 | Mixed credit and debit transactions | Current balance: 1000.00 | 1. Credit 500.00 (balance: 1500.00)<br>2. Debit 200.00 (balance: 1300.00)<br>3. Credit 100.00 (balance: 1400.00)<br>4. Debit 300.00 (balance: 1100.00)<br>5. View balance | Final balance shows 1100.00 correctly | | | Tests transaction sequence integrity |
| TC-012 | Attempt debit after previous insufficient funds | Balance: 1000.00 | 1. Debit 1500.00 (fails)<br>2. Verify balance still 1000.00<br>3. Debit 500.00 (succeeds)<br>4. Verify balance is 500.00 | First debit rejected with error<br>Balance unchanged<br>Second debit succeeds<br>Final balance: 500.00 | | | Ensures failed transactions don't affect subsequent operations |
| TC-013 | Credit after insufficient funds debit | Balance: 500.00 | 1. Debit 600.00 (fails)<br>2. Credit 300.00<br>3. Debit 600.00 (now succeeds)<br>4. View balance | Failed debit shows error<br>Credit succeeds (balance: 800.00)<br>Debit succeeds (balance: 200.00) | | | Tests recovery after failed transaction |
| TC-014 | Maximum transaction amount | Balance: 1000.00 | 1. Credit 9999.99<br>2. View balance<br>3. Debit 5000.00<br>4. View balance | Credit succeeds (balance: 10999.99)<br>Debit succeeds (balance: 5999.99) | | | Tests maximum allowed transaction (PIC 9(6)V99) |
| TC-015 | Maximum balance capacity | Balance: 1000.00 | 1. Credit 998999.99<br>2. View balance | Credit succeeds<br>Balance shows 999999.99 | | | Tests maximum balance limit (PIC 9(6)V99) |
| TC-016 | Minimum non-zero transaction | Balance: 1000.00 | 1. Credit 0.01<br>2. View balance (1000.01)<br>3. Debit 0.01<br>4. View balance | Credit succeeds (1000.01)<br>Debit succeeds (1000.00) | | | Tests minimum transaction amount with decimal precision |
| TC-017 | Zero amount credit | Balance: 1000.00 | 1. Select option 2<br>2. Enter 0.00<br>3. View balance | System accepts input<br>Balance remains 1000.00 or shows error | | | Edge case: zero amount handling |
| TC-018 | Zero amount debit | Balance: 1000.00 | 1. Select option 3<br>2. Enter 0.00<br>3. View balance | System accepts input<br>Balance remains 1000.00 or shows error | | | Edge case: zero amount handling |
| TC-019 | View balance multiple times | Balance: 1000.00 | 1. View balance<br>2. View balance<br>3. View balance | All three views show 1000.00 consistently | | | Validates READ operation idempotency |
| TC-020 | Menu option 1 selection | Application running | 1. Enter 1 at menu prompt | System calls Operations with 'TOTAL'<br>Balance is displayed | | | Validates menu routing |
| TC-021 | Menu option 2 selection | Application running | 1. Enter 2 at menu prompt | System calls Operations with 'CREDIT'<br>Prompts for amount | | | Validates menu routing |
| TC-022 | Menu option 3 selection | Application running | 1. Enter 3 at menu prompt | System calls Operations with 'DEBIT'<br>Prompts for amount | | | Validates menu routing |
| TC-023 | Menu option 4 (Exit) | Application running | 1. Enter 4 at menu prompt | System displays "Exiting the program. Goodbye!"<br>Application terminates | | | Validates clean exit |
| TC-024 | Invalid menu option - zero | Application running | 1. Enter 0 at menu prompt | System displays "Invalid choice, please select 1-4."<br>Menu redisplays | | | Error handling for invalid input |
| TC-025 | Invalid menu option - out of range | Application running | 1. Enter 5 at menu prompt | System displays "Invalid choice, please select 1-4."<br>Menu redisplays | | | Error handling for invalid input |
| TC-026 | Invalid menu option - out of range (high) | Application running | 1. Enter 9 at menu prompt | System displays "Invalid choice, please select 1-4."<br>Menu redisplays | | | Error handling for invalid input |
| TC-027 | Menu loop continuation | Application running | 1. View balance<br>2. Verify menu redisplays<br>3. Credit account<br>4. Verify menu redisplays | After each operation, menu displays again until user selects Exit | | | Validates application loop |
| TC-028 | Balance persistence within session | Application running | 1. Credit 500.00 (balance: 1500.00)<br>2. Exit to menu<br>3. View balance<br>4. Debit 200.00<br>5. View balance | Balance persists across operations within same session<br>Final balance: 1300.00 | | | **Critical**: Validates in-memory persistence |
| TC-029 | Data layer READ operation | Application started | 1. Operations calls DataProgram with 'READ'<br>2. Check returned balance | DataProgram returns current STORAGE-BALANCE value | | | Unit test for data layer |
| TC-030 | Data layer WRITE operation | Current balance: 1000.00 | 1. Operations calls DataProgram with 'WRITE' and new balance 1500.00<br>2. Call READ to verify | STORAGE-BALANCE updated to 1500.00<br>Subsequent READ returns 1500.00 | | | Unit test for data layer |
| TC-031 | Decimal precision preservation | Balance: 1000.00 | 1. Credit 99.99<br>2. Credit 0.01<br>3. View balance<br>4. Debit 50.50<br>5. View balance | Balance shows 1100.00 after credits<br>Balance shows 1049.50 after debit<br>No rounding errors | | | **Critical**: Validates PIC 9(6)V99 precision |
| TC-032 | Negative amount handling - credit | Balance: 1000.00 | 1. Select option 2<br>2. Enter -100.00 | System behavior: accepts or rejects negative input | | | Edge case: negative input validation |
| TC-033 | Negative amount handling - debit | Balance: 1000.00 | 1. Select option 3<br>2. Enter -100.00 | System behavior: accepts or rejects negative input | | | Edge case: negative input validation |
| TC-034 | Non-numeric input - credit | Balance: 1000.00 | 1. Select option 2<br>2. Enter "abc" | System rejects input or handles error gracefully | | | Input validation test |
| TC-035 | Non-numeric input - debit | Balance: 1000.00 | 1. Select option 3<br>2. Enter "xyz" | System rejects input or handles error gracefully | | | Input validation test |
| TC-036 | Non-numeric input - menu | Application running | 1. Enter "a" at menu prompt | System rejects input or handles error gracefully | | | Input validation test |
| TC-037 | Boundary test - debit one cent less than balance | Balance: 1000.00 | 1. Debit 999.99<br>2. View balance | Debit succeeds<br>Balance shows 0.01 | | | Edge case for sufficient funds check |
| TC-038 | Balance display format | Balance: 1000.00 | 1. View balance | Balance displays in format: "Current balance: 1000.00"<br>Includes 2 decimal places | | | UI/Output format validation |
| TC-039 | Credit confirmation message format | Balance: 1000.00 | 1. Credit 500.00 | Message displays: "Amount credited. New balance: 1500.00" | | | UI/Output format validation |
| TC-040 | Debit confirmation message format | Balance: 1000.00 | 1. Debit 300.00 | Message displays: "Amount debited. New balance: 700.00" | | | UI/Output format validation |
| TC-041 | Insufficient funds message format | Balance: 500.00 | 1. Debit 600.00 | Message displays: "Insufficient funds for this debit." | | | UI/Output format validation |
| TC-042 | Sequential operations without viewing balance | Balance: 1000.00 | 1. Credit 500.00<br>2. Credit 200.00<br>3. Debit 300.00<br>4. View balance | Each operation updates internal balance correctly<br>Final balance: 1400.00 | | | Tests internal state management |
| TC-043 | Rapid successive credits | Balance: 1000.00 | 1. Credit 100.00<br>2. Credit 100.00<br>3. Credit 100.00<br>4. Credit 100.00<br>5. Credit 100.00<br>6. View balance | All credits process correctly<br>Final balance: 1500.00 | | | Stress test for data consistency |
| TC-044 | Rapid successive debits | Balance: 1000.00 | 1. Debit 100.00<br>2. Debit 100.00<br>3. Debit 100.00<br>4. Debit 100.00<br>5. Debit 100.00<br>6. View balance | All debits process correctly<br>Final balance: 500.00 | | | Stress test for data consistency |
| TC-045 | Operations program GOBACK | Any operation completed | 1. Complete any operation<br>2. Check control returns to MainProgram | Control returns to MainProgram<br>Menu redisplays | | | Tests proper program flow |
| TC-046 | Large decimal credit | Balance: 1000.00 | 1. Credit 1234.56 | Credit succeeds<br>Balance: 2234.56 | | | Tests decimal handling with larger amounts |
| TC-047 | Large decimal debit | Balance: 5000.00 | 1. Debit 2345.67 | Debit succeeds<br>Balance: 2654.33 | | | Tests decimal handling with larger amounts |
| TC-048 | Exit without performing operations | Application started | 1. Select option 4 immediately | Application exits cleanly<br>Displays goodbye message | | | Tests immediate exit functionality |
| TC-049 | Multiple menu redisplays | Application running | 1. View balance (menu redisplays)<br>2. Enter invalid option (menu redisplays)<br>3. View balance again | Menu displays consistently each time | | | UI consistency test |
| TC-050 | Balance persistence through invalid operations | Balance: 1000.00 | 1. Debit 2000.00 (fails)<br>2. Enter invalid menu option<br>3. View balance | Balance remains 1000.00 throughout<br>No corruption from failed operations | | | **Critical**: Data integrity test |

---

## Test Execution Notes

### Priority Levels
- **Critical Tests**: TC-008, TC-028, TC-031, TC-050 (overdraft protection, persistence, precision, data integrity)
- **High Priority**: TC-001 through TC-016 (core business logic)
- **Medium Priority**: TC-017 through TC-042 (edge cases and validation)
- **Low Priority**: TC-043 through TC-050 (stress tests and UI consistency)

### Test Environment Requirements
- GnuCOBOL compiler installed
- Application compiled successfully
- Test data: Initial balance set to $1,000.00

### Known Limitations to Test
1. **No data persistence**: Balance resets when application restarts
2. **No transaction history**: Previous transactions are not logged
3. **Limited input validation**: System may not handle all invalid inputs gracefully
4. **Single account**: Only one account can be managed at a time

### Automated Test Considerations for Node.js Migration
When creating unit and integration tests for Node.js:
- Mock the data layer for unit testing business logic
- Create integration tests that validate the full flow end-to-end
- Add input validation tests that weren't in the original COBOL
- Implement tests for error handling and edge cases
- Consider adding tests for concurrent operations (not applicable in COBOL)
- Add API endpoint tests if implementing as a REST service

### Business Stakeholder Validation Points
Review with stakeholders:
1. Is the $1,000.00 initial balance correct?
2. Should zero amount transactions be allowed?
3. Should negative amounts be accepted (treated as opposite operation)?
4. What is the maximum transaction amount that should be allowed?
5. What is the maximum balance that should be allowed?
6. Should there be a minimum balance requirement?
7. Are there any missing business rules or validation requirements?

---

## Sign-off

| Role | Name | Signature | Date |
|------|------|-----------|------|
| Business Analyst | | | |
| QA Lead | | | |
| Development Lead | | | |
| Product Owner | | | |

---

## Version History

| Version | Date | Author | Changes |
|---------|------|--------|---------|
| 1.0 | TBD | | Initial test plan creation |
