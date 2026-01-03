import S from '@surplus/s';

const Root = () => {
	window.State = {
		show: S.data(true),
		count: S.data(0)
	};

	return (
		<div id="container">
			{window.State.show() ? (
				<button
					id="conditional-btn"
					on:click={() => window.State.count(window.State.count() + 1)}
				>
					Count: {window.State.count()}
				</button>
			) : null}
		</div>
	);
};

export default <Root />;
