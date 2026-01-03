const name = 'World';
const count = 42;

export default (
	<div>
		Hello <strong>{name}</strong>, you have {count} messages.
		<br />
		Your score: {count * 2}
		<span> (doubled)</span>
	</div>
);
