const { createInterface } = require("readline/promises");
(async () => {
  const input = createInterface({
    input: process.stdin,
    output: process.stdout,
  });
  const num = await input.question("Enter a number: ");
  const result = num % 2 == 0 ? "even" : "odd";
  console.log(`${num} is ${result}`);
  input.close();
})();
