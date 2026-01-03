const Wrapper = ({ children }) => (
	<div class="wrapper">
		<h1>Title</h1>
		{children}
		<footer>Footer</footer>
	</div>
);

const Card = ({ children }) => (
	<div class="card">
		{children}
	</div>
);

export default (
	<Wrapper>
		<Card>
			<p>Paragraph 1</p>
			<p>Paragraph 2</p>
		</Card>
		<div>Outside card</div>
	</Wrapper>
);
