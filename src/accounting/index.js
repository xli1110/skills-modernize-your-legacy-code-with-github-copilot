/**
 * Account Management System - Node.js Implementation
 * 
 * Converted from legacy COBOL application (main.cob, operations.cob, data.cob)
 * Preserves original business logic, data integrity, and menu options
 * 
 * Original COBOL Architecture:
 * - MainProgram (main.cob): User interface and menu handling
 * - Operations (operations.cob): Business logic layer
 * - DataProgram (data.cob): Data access layer
 */

const readline = require('readline');

// Readline interface will be created when needed
let rl = null;

/**
 * DataProgram Layer - Data Access
 * Equivalent to data.cob
 * Manages persistent storage and retrieval of account balance
 */
class DataProgram {
  constructor() {
    // STORAGE-BALANCE PIC 9(6)V99 VALUE 1000.00
    this.storageBalance = 1000.00;
  }

  /**
   * READ operation - Retrieves current balance
   * @returns {number} Current balance
   */
  read() {
    return this.storageBalance;
  }

  /**
   * WRITE operation - Updates balance
   * @param {number} balance - New balance to store
   */
  write(balance) {
    this.storageBalance = balance;
  }
}

/**
 * Operations Layer - Business Logic
 * Equivalent to operations.cob
 * Implements core business operations: view balance, credit, debit
 */
class Operations {
  constructor(dataProgram) {
    this.dataProgram = dataProgram;
  }

  /**
   * View Balance Operation (TOTAL)
   * Displays current account balance
   */
  viewBalance() {
    const balance = this.dataProgram.read();
    console.log(`Current balance: ${balance.toFixed(2)}`);
  }

  /**
   * Credit Account Operation (CREDIT)
   * Adds amount to account balance
   * @param {number} amount - Amount to credit
   */
  credit(amount) {
    const currentBalance = this.dataProgram.read();
    
    // Round to 2 decimal places to avoid floating point artifacts
    const newBalance = Math.round((currentBalance + amount) * 100) / 100;
    
    // Business Rule: Maximum balance limit is 999,999.99
    if (newBalance > 999999.99) {
      console.log('Credit rejected. Balance would exceed maximum allowed (999,999.99).');
      return false;
    }
    
    this.dataProgram.write(newBalance);
    console.log(`Amount credited. New balance: ${newBalance.toFixed(2)}`);
    return true;
  }

  /**
   * Debit Account Operation (DEBIT)
   * Subtracts amount from account balance with overdraft protection
   * Business Rule: Debit cannot exceed current balance
   * @param {number} amount - Amount to debit
   * @returns {boolean} True if debit successful, false if insufficient funds
   */
  debit(amount) {
    const currentBalance = this.dataProgram.read();
    
    // Business Rule: Overdraft Protection
    if (currentBalance >= amount) {
      // Round to 2 decimal places to avoid floating point artifacts
      const newBalance = Math.round((currentBalance - amount) * 100) / 100;
      this.dataProgram.write(newBalance);
      console.log(`Amount debited. New balance: ${newBalance.toFixed(2)}`);
      return true;
    } else {
      console.log('Insufficient funds for this debit.');
      return false;
    }
  }
}

/**
 * MainProgram - User Interface Layer
 * Equivalent to main.cob
 * Manages menu display, user input, and application flow
 */
class MainProgram {
  constructor() {
    this.dataProgram = new DataProgram();
    this.operations = new Operations(this.dataProgram);
    this.continueFlag = true;
  }

  /**
   * Display menu options
   */
  displayMenu() {
    console.log('--------------------------------');
    console.log('Account Management System');
    console.log('1. View Balance');
    console.log('2. Credit Account');
    console.log('3. Debit Account');
    console.log('4. Exit');
    console.log('--------------------------------');
  }

  /**
   * Prompt for amount input
   * @param {string} promptMessage - Message to display
   * @returns {Promise<number>} Amount entered by user
   */
  async promptForAmount(promptMessage) {
    // Ensure readline interface exists
    if (!rl) {
      rl = readline.createInterface({
        input: process.stdin,
        output: process.stdout
      });
    }
    
    // Use a loop instead of recursion to avoid building up Promise chains
    while (true) {
      const input = await new Promise((resolve) => {
        rl.question(promptMessage, resolve);
      });
      
      const amount = parseFloat(input);
      
      // Validate input is a non-negative number
      if (isNaN(amount) || amount < 0) {
        console.log('Invalid amount. Please enter a valid non-negative number.');
        continue;
      }
      
      // Business Rule: Maximum transaction amount per docs/README.md and TC-014 = 9,999.99
      if (amount > 9999.99) {
        console.log('Amount exceeds maximum allowed (9,999.99).');
        continue;
      }
      
      return amount;
    }
  }

  /**
   * Handle user menu choice
   * @param {string} choice - User's menu selection
   */
  async handleChoice(choice) {
    const userChoice = parseInt(choice, 10);

    switch (userChoice) {
      case 1:
        // View Balance
        this.operations.viewBalance();
        break;

      case 2:
        // Credit Account
        const creditAmount = await this.promptForAmount('Enter credit amount: ');
        this.operations.credit(creditAmount);
        break;

      case 3:
        // Debit Account
        const debitAmount = await this.promptForAmount('Enter debit amount: ');
        this.operations.debit(debitAmount);
        break;

      case 4:
        // Exit
        this.continueFlag = false;
        console.log('Exiting the program. Goodbye!');
        if (rl) {
          rl.close();
        }
        break;

      default:
        // Invalid choice
        console.log('Invalid choice, please select 1-4.');
        break;
    }
  }

  /**
   * Main application loop
   * Continues until user selects Exit option
   */
  async run() {
    // Initialize readline interface
    if (!rl) {
      rl = readline.createInterface({
        input: process.stdin,
        output: process.stdout
      });
    }
    
    while (this.continueFlag) {
      this.displayMenu();
      
      const choice = await new Promise((resolve) => {
        rl.question('Enter your choice (1-4): ', resolve);
      });

      await this.handleChoice(choice);
      
      // Add blank line for readability (unless exiting)
      if (this.continueFlag) {
        console.log('');
      }
    }
  }
}

/**
 * Application Entry Point
 * Starts the Account Management System
 */
async function main() {
  try {
    const app = new MainProgram();
    await app.run();
  } catch (error) {
    console.error('An error occurred:', error.message);
    process.exit(1);
  }
}

// Start the application
if (require.main === module) {
  main();
}

// Export classes for testing
module.exports = {
  DataProgram,
  Operations,
  MainProgram
};
