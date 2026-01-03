const clicked = S.value(0);
const button = <button on:click={() => clicked(clicked() + 1)}>Click me</button>;
clicked();
button.click();
clicked();
