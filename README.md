# QuantedaApp

## Installation Instructions

### First-time Setup

Perform these steps only once to install the app:

1. Install the `devtools` package for access to remote repositories on GitHub. Type into your console:
   ```R
   install.packages("devtools")
   ```

2. Load the `devtools` package:
  ```R
   library("devtools")
   ```

3. Install the app from GitHub:
  ```R
   install_github("VBuskin/quanteda_app")
   ```

Follow the instructions in the console. When prompted to update packages, do not update. Simply press ENTER to continue.

## Running the App

Whenever you re-open RStudio, follow these steps to run the app:

4. Load the `QuantedaApp`:
  ```R
   library("QuantedaApp")
   ```

5. Launch the app:
  ```R
  run_quanteda_app()
   ```

## Troubleshooting

If you encounter any issues during installation or while running the app, please check the following:

- Ensure you have the latest version of R and RStudio installed.
- Check your internet connection, as the installation requires downloading packages.
- Check for any syntax errors in your query.
