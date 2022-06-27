# Introduction to Stonks

## TODOs:

1. Interactive menu with cursor and clear screen
2. Internal database
3. ASCII Tables/Charts https://github.com/MitchTalmadge/ASCII-Data
4. GraalVM native binary


## Design

App distributed as a native executable binary, single file.

MacOS
Linux
Windows

When runs first time it checks if the user data file exists.
User data file is encrypted file located in home directory, 
that stores user settings and data.
e.g. /Users/john/.stonks/userdata.db
{
  :username "John Doe"
  :transactions [
    [:buy "AAPL" 145.45 "USD" 1656334796]
    [:sell "AAPL" 150.34 "EUR" 1656334797]
  ]
}

If the user data file doesn't exist, we will ask for username and password
and will create a new user data file.

If the file exists we will ask for password and will try to open userdata file
to validate it.

When user logged in, the app will show a dashboard.

e.g.
Welcome back, username!
Last login: Date time in UTC
---
Total spent: 13456 USD
Current evaluation: 15678 USD
Portfolio performance: 5%
Time on the market: 2 years

Portfolio:
(table)
Ticker  |  Avg. buy price  |  Last price   |   Total   |   Profit

Menu:

1. Buy transaction
2. Sell transaction

User input:

1
Ticker?
AAPL
Price?
134.45

Redraw.
