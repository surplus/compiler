const items = ['Apple', 'Banana', 'Cherry'];
const numbers = [1, 2, 3, 4, 5];

export default (
	<div>
		<ul>
			{items.map(item => <li>{item}</li>)}
		</ul>
		<div class="numbers">
			{numbers.map(n => <span class="num">{n * 2}</span>)}
		</div>
	</div>
);
