import S from '@surplus/s';

const Root = () => {
	window.State = {
		count: S.data(0),
		disabled: S.data(false)
	};

	return (
		<div>
			<button
				id="counter"
				disabled={State.disabled() || undefined}
				on:click={() => State.count(State.count() + 1)}
			>
				Count: {State.count()}
			</button>
		</div>
	);
};

export default <Root />;
