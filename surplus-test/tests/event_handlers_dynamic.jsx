import S from '@surplus/s';

const Root = () => {
	window.State = {
		count: S.data(0),
		multiplier: S.data(1)
	};

	return (
		<button
			id="dynamic-btn"
			on:click={() => {
				const mult = window.State.multiplier();
				window.State.count(window.State.count() + mult);
			}}
		>
			Count: {window.State.count()}
		</button>
	);
};

export default <Root />;
