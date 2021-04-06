#!/bin/sh

# Node is installed?
echo "Checking if npm installed..."
npm list > /dev/null 2>&1
commandSucceeded=$?
if [ $commandSucceeded -eq 0 ]; then
    echo "npm installed"
else
    echo "npm NOT Installed; Installing..."
    sudo apt update
    sudo apt install npm
    # Upgrade to latest npm
    sudo npm install npm@latest -g
fi

echo "Yarn installed?"
npmOutput=$(npm list -g | grep yarn)
if [ ! -z "$npmOutput" -a "$npmOutput" != " " ]; then
    echo "Seems like it"
else
    echo "Nope, installing with cmd 'npm install yarn'..."
    sudo npm install -g yarn
fi

echo "Installling mermaid CLI locally..."
yarn init -y
yarn add @mermaid-js/mermaid-cli

echo "Running..."
./node_modules/.bin/mmdc -h

echo "Complete"
