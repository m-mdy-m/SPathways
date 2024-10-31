import { createInterface } from "readline/promises";
async function calc(): Promise<void> {
  const input = createInterface({
    input: process.stdin,
    output: process.stdout,
  });
  const first_num = await input.question("Enter first number: ");
  const second_num = await input.question("Enter second number: ");
  const sum = +first_num + +second_num;
  const subtraction = +first_num - +second_num;
  const multiplication = +first_num * +second_num;
  const division = +first_num / +second_num;
  console.log(`${first_num} + ${second_num} = ${sum}`);
  console.log(`${first_num} - ${second_num} = ${subtraction}`);
  console.log(`${first_num} * ${second_num} = ${multiplication}`);
  console.log(`${first_num} / ${second_num} = ${division}`);
  input.close();
}
(async () => {
  await calc();
})();
