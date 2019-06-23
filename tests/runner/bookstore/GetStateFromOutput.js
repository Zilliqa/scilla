const fs = require('fs');

args = process.argv.slice(2)

if (args.length !== 2) {
    console.log('Usage: node GetStateFromJson.js <output> <newstate>');
    process.exit(1);
}

const oldOutputFilename = `output_${args[0]}.json`;
const newStateFilename = `state_${args[1]}.json`;
data = JSON.parse(fs.readFileSync(oldOutputFilename, 'utf8'));
fs.writeFileSync(newStateFilename, JSON.stringify(data.states, null, 4));
console.log(`[${args[1]}] Obtained state from ${args[0]}`);
